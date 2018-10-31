(ns hodur-lacinia-datomic-adapter.core
  (:require [camel-snake-kebab.core :refer [->camelCaseKeyword
                                            ->kebab-case-string]]
            [com.walmartlabs.lacinia.resolve :as resolve]
            [datascript.core :as d]
            [datascript.query-v3 :as q]
            [datomic.client.api :as datomic]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private pull-many
  [db selector eids]
  (map #(datomic/pull db selector %)
       eids))

(defn ^:private fetch-page
  ([db selector find where]
   (fetch-page db selector where nil))
  ([db selector find where {:keys [offset limit] :or {offset 0 limit -1}}]
   (let [eids
         (-> (datomic/q {:query (concat `[:find ~find :where] where)
                         :args [db]
                         :limit limit
                         :offset offset})
             flatten)
         total-count
         (or (-> (datomic/q {:query (concat `[:find (~'count ~find) :where] where)
                             :args [db]})
                 flatten
                 first)
             0)
         has-prev (>= (- offset limit) 0)
         has-next (<= (+ offset limit) total-count)]
     {:totalCount total-count
      :pageInfo {:totalPages (int (Math/ceil (/ total-count limit)))
                 :currentPage (int (Math/ceil (/ offset limit)))
                 :pageSize limit
                 :currentOffset offset
                 :hasPrev has-prev
                 :prevOffset (if has-prev (- offset limit) 0)
                 :hasNext has-next
                 :nextOffset (if has-next (+ offset limit) offset)}
      :nodes (pull-many db selector eids)})))

(defn ^:private fetch-one
  [db selector find where]
  (let [eid (-> (datomic/q {:query (concat `[:find ~find :where] where)
                            :args [db]})
                flatten first)]
    (datomic/pull db selector eid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private extract-lacinia-selections
  [context]
  (-> context
      :com.walmartlabs.lacinia.constants/parsed-query
      :selections))

(defn ^:private extract-lacinia-type-name
  [{:keys [field-definition] :as selection}]
  (let [kind (-> field-definition :type :kind)]
    (case kind
      :list     (extract-lacinia-type-name {:field-definition (-> field-definition :type)})
      :non-null (-> field-definition :type :type :type)
      :root     (-> field-definition :type :type)
      :none)))

(defn ^:private find-selection-by-field
  [field-name-path selections]
  (if (not (seqable? field-name-path))
    (find-selection-by-field [field-name-path] selections)
    (loop [accum selections
           field-name (first field-name-path)
           rest-fields (rest field-name-path)]
      (let [selection (->> accum
                           (filter #(= field-name (:field %)))
                           first)]
        (if (empty? rest-fields)
          selection
          (recur (:selections selection)
                 (first rest-fields)
                 (rest rest-fields)))))))

(defn ^:private find-field-on-lacinia-field-name
  [lacinia-field-name fields]
  (->> fields
       (filter #(= lacinia-field-name
                   (:field/camelCaseName %)))
       first))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private query-resolve-one-fields
  [engine-conn]
  (let [selector '[* {:field/parent [*]
                      :param/_parent [*]}]
        eids (-> (q/q '[:find ?f
                        :where
                        [?f :lacinia->datomic.query/type :one]
                        [?f :field/parent ?t]
                        [?t :lacinia/query true]
                        [?t :lacinia/tag true]
                        [?f :lacinia/tag true]
                        [?p :param/parent ?f]
                        [?p :lacinia->datomic.param/lookup-ref]]
                      @engine-conn)
                 vec flatten)]
    (->> eids
         (d/pull-many @engine-conn selector))))

(defn ^:private query-resolve-many-fields
  [engine-conn]
  (let [selector '[* {:field/parent [*]
                      :param/_parent [*]}]
        eids (-> (q/q '[:find ?f
                        :where
                        [?f :lacinia->datomic.query/type :many]
                        [?f :field/parent ?t]
                        [?t :lacinia/query true]
                        [?t :lacinia/tag true]
                        [?f :lacinia/tag true]]
                      @engine-conn)
                 vec flatten)]
    (->> eids
         (d/pull-many @engine-conn selector))))

(defn ^:private query-resolve-lookup-fields
  [engine-conn]
  (let [selector '[:field/camelCaseName
                   :field/kebab-case-name
                   :lacinia->datomic.field/lookup
                   :lacinia->datomic.field/reverse-lookup
                   {:field/parent [:type/PascalCaseName
                                   :type/kebab-case-name]}]
        eids (-> (q/q '[:find ?f
                        :where
                        (or [?f :lacinia->datomic.field/lookup]
                            [?f :lacinia->datomic.field/reverse-lookup])]
                      @engine-conn)
                 vec flatten)]
    (->> eids
         (d/pull-many @engine-conn selector))))

(defn ^:private query-datomic-fields-on-lacinia-type
  [lacinia-type-name engine-conn]
  ;;The reason I had to break into multiple queries is that datascript's `or
  ;;returns a union instead  (it's prob a bug on `q)
  (let [selector '[* {:field/parent [*]
                      :field/type [*]}]

        where-datomic [['?f :field/parent '?tp]
                       ['?tp :type/PascalCaseName lacinia-type-name]
                       ['?f :datomic/tag true]]
        where-depends [['?f :field/parent '?tp]
                       ['?tp :type/PascalCaseName lacinia-type-name]
                       ['?f :lacinia->datomic.field/depends-on]]
        where-dbid    [['?f :field/parent '?tp]
                       ['?tp :type/PascalCaseName lacinia-type-name]
                       ['?f :lacinia->datomic.field/dbid true]]

        query-datomic (concat '[:find ?f :where] where-datomic)
        query-depends (concat '[:find ?f :where] where-depends)
        query-dbid    (concat '[:find ?f :where] where-dbid)

        eids-datomic (-> (q/q query-datomic @engine-conn)
                         vec flatten)
        eids-depends (-> (q/q query-depends @engine-conn)
                         vec flatten)
        eids-dbid    (-> (q/q query-dbid @engine-conn)
                         vec flatten)

        eids         (concat eids-datomic
                             eids-depends
                             eids-dbid)]
    (->> eids
         (d/pull-many @engine-conn selector))))

(defn ^:private query-datomic-fields
  [engine-conn]
  (let [selector '[* {:field/parent [*]
                      :field/type [*]}]
        query '[:find ?f
                :where
                [?f :field/name]
                (or [?f :lacinia->datomic.field/dbid true]
                    [?f :datomic/tag true]
                    [?f :lacinia->datomic.field/reverse-lookup])]
        eids (-> (q/q query @engine-conn)
                 vec flatten)]
    (->> eids
         (d/pull-many @engine-conn selector))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private lacinia->datomic-map
  [type-name fields]
  (reduce (fn [m {:keys [field/camelCaseName
                         field/kebab-case-name
                         lacinia->datomic.field/reverse-lookup
                         lacinia->datomic.field/depends-on
                         lacinia->datomic.field/dbid] :as field}]
            (let [datomic-field-name
                  (cond
                    dbid           [(list :db/id :as camelCaseName)]
                    depends-on     (->> depends-on
                                        (map #(list % :as (-> % name ->camelCaseKeyword)))
                                        vec)
                    :else          [(list (keyword
                                           (->kebab-case-string type-name)
                                           (name kebab-case-name))
                                          :as
                                          camelCaseName)])]
              (assoc m camelCaseName datomic-field-name)))
          {} fields))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private paginated-lookup?
  [lacinia-type-name lacinia-field-name engine-conn]
  (let [{:keys [:lacinia->datomic.field/lookup
                :lacinia->datomic.field/reverse-lookup]}
        (d/q `[:find (~'pull ~'?f [:lacinia->datomic.field/lookup
                                   :lacinia->datomic.field/reverse-lookup]) ~'.
               :where
               [~'?t :lacinia/tag true]
               [~'?f :lacinia/tag true]
               [~'?t :type/PascalCaseName ~lacinia-type-name]
               [~'?f :field/parent ?t]
               [~'?f :field/camelCaseName ~lacinia-field-name]]
             @engine-conn)]
    (if (or lookup reverse-lookup)
      true false)))

(defn ^:private datomic-selector
  [field-selection engine-conn]
  (if-let [selections (:selections field-selection)]
    (let [type-name (extract-lacinia-type-name field-selection)
          lacinia->datomic (->> engine-conn
                                (query-datomic-fields-on-lacinia-type type-name)
                                (lacinia->datomic-map type-name))]
      (->> selections
           (reduce
            (fn [c {:keys [field] :as selection}]
              (if-let [datomic-fields (get lacinia->datomic field)]
                (let [selection-selections (:selections selection)
                      is-paginated?        (paginated-lookup? type-name
                                                              field
                                                              engine-conn)]
                  (if (and selection-selections
                           (not is-paginated?))
                    (conj c (assoc {} (first datomic-fields)
                                   (datomic-selector selection engine-conn)))
                    (apply conj c datomic-fields)))
                c))
            #{})
           vec))
    []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private field->resolve-one-entry
  [{:keys [field/camelCaseName field/parent param/_parent] :as field}]
  {:field-name camelCaseName
   :args (reduce
          (fn [m param]
            (assoc m (:param/camelCaseName param)
                   {:lookup-ref (:lacinia->datomic.param/lookup-ref param)
                    :transform (:lacinia->datomic.param/transform param)}))
          {} _parent)})

(defn ^:private field->resolve-many-entry
  [{:keys [field/camelCaseName param/_parent] :as field}]
  {:field-name camelCaseName
   :args (reduce
          (fn [m param]
            (if-let [where-builder (:lacinia->datomic.param/where-builder param)]
              (let [where-builder-fn (find-var where-builder)]
                (assoc m (:param/camelCaseName param)
                       {:where-builder-fn where-builder-fn}))
              m))
          {} _parent)})

(defn ^:private field->resolve-lookup-entry
  [{:keys [field/camelCaseName field/parent
           :lacinia->datomic.field/lookup
           :lacinia->datomic.field/reverse-lookup] :as field}]
  {:lacinia-type-name (:type/PascalCaseName parent)
   :lacinia-field-name camelCaseName
   :lookup lookup
   :reverse-lookup reverse-lookup})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resolver functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti ^:private execute-fetch
  (fn [db {:keys [resolver-type] :as payload}]
    (clojure.pprint/pprint payload)
    resolver-type))

(defmethod execute-fetch :one-resolver
  [db {:keys [placeholder selector offset limit where] :as payload}]
  (-> (fetch-one db selector placeholder where)
      (resolve/with-context {:where where})))

(defmethod execute-fetch :many-resolver
  [db {:keys [placeholder selector offset limit where] :as payload}] 
  (-> (fetch-page db selector placeholder where
                  {:offset offset
                   :limit limit})
      (resolve/with-context {:where where})))

(defmethod execute-fetch :lookup-resolver
  [db {:keys [placeholder selector offset limit where] :as payload}]
  (-> (fetch-page db selector placeholder where
                  {:limit limit
                   :offset offset})
      (resolve/with-context {:where where})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private context->field-selection
  ([ctx]
   (context->field-selection ctx []))
  ([{:keys [resolver-context field-entry]} path]
   (let [{:keys [field-name]} field-entry
         full-path (concat [field-name] path)]
     (->> resolver-context
          extract-lacinia-selections
          (find-selection-by-field full-path)))))

(defmulti ^:private get-field-selection
  (fn [resolver-type ctx] resolver-type))

(defmethod get-field-selection :one-resolver
  [_ ctx]
  (context->field-selection ctx))

(defmethod get-field-selection :many-resolver
  [_ ctx]
  (context->field-selection ctx [:nodes]))

(defmethod get-field-selection :lookup-resolver
  [_ ctx]
  (context->field-selection ctx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti ^:private get-placeholder
  (fn [resolver-type ctx] resolver-type))

(defmethod get-placeholder :one-resolver
  [_ _]
  '?e)

(defmethod get-placeholder :many-resolver
  [_ _]
  '?e)

(defmethod get-placeholder :lookup-resolver
  [_ {:keys [lookup reverse-lookup]}]
  (cond
    lookup         (->> lookup name (str "?") symbol)
    reverse-lookup (->> reverse-lookup name (str "?") symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti ^:private get-where
  (fn [resolver-type ctx] resolver-type))

(defmethod get-where :one-resolver
  [_ {:keys [args field-entry]}]
  (reduce-kv (fn [a in-arg-name in-arg-val]
               (let [{:keys [lookup-ref transform]} (get-in field-entry [:args in-arg-name])
                     arg-val (if transform
                               ((find-var transform) in-arg-val)
                               in-arg-val)]
                 (reduced [['?e lookup-ref arg-val]])))
             nil args))

(defmethod get-where :many-resolver
  [_ {:keys [args field-entry]}]
  (reduce-kv (fn [c arg-k arg-v]
               (if-let [{:keys [where-builder-fn]}
                        (get (:args field-entry) arg-k)]
                 (where-builder-fn c args)
                 c))
             [] args))

(defmethod get-where :lookup-resolver
  [_ {:keys [lookup reverse-lookup prev-where] :as ctx}]
  (let [this-placeholder (get-placeholder :lookup-resolver ctx)]
    (cond
      lookup         (concat prev-where
                             [['?e lookup this-placeholder]])
      reverse-lookup (concat prev-where
                             [[this-placeholder reverse-lookup '?e]]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private create-resolver
  [resolver-type {:keys [field-name] :as field-entry} engine-conn]
  (fn [{:keys [db where] :as ctx} {:keys [offset limit] :as args} resolved-value]
    (println "=========")
    (println "=> Resolver for field-name" field-name "of type" resolver-type)
    (println "... args:" args)
    (let [context {:args args
                   :resolver-context ctx
                   :field-entry field-entry
                   :prev-where where}
          selector (-> resolver-type
                       (get-field-selection context)
                       (datomic-selector engine-conn))]
      (execute-fetch db {:resolver-type resolver-type
                         :placeholder   (get-placeholder resolver-type context)
                         :selector      selector
                         :where         (get-where resolver-type context)
                         :offset        offset
                         :limit         limit}))))

(defn ^:private attach-one-resolvers
  [lacinia-schema one-fields engine-conn]
  (->> one-fields
       (reduce
        (fn [m {:keys [field-name] :as field-entry}]
          (assoc-in m [:queries field-name :resolve]
                    (create-resolver :one-resolver field-entry engine-conn)))
        lacinia-schema)))

(defn ^:private attach-many-resolvers
  [lacinia-schema many-fields engine-conn]
  (->> many-fields
       (reduce
        (fn [m {:keys [field-name] :as field-entry}]
          (assoc-in m [:queries field-name :resolve]
                    (create-resolver :many-resolver field-entry engine-conn)))
        lacinia-schema)))

(defn ^:private attach-lookup-resolvers
  [lacinia-schema lookup-fields engine-conn]
  (->> lookup-fields
       (reduce
        (fn [m {:keys [lacinia-type-name lacinia-field-name
                       lookup reverse-lookup] :as field-entry}] 
          (assoc-in m [:objects lacinia-type-name :fields lacinia-field-name :resolve]
                    (create-resolver :lookup-resolver field-entry engine-conn)))
        lacinia-schema)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn attach-resolvers
  [lacinia-schema engine-conn]
  (let [one-fields (->> engine-conn
                        query-resolve-one-fields
                        (map field->resolve-one-entry))
        many-fields (->> engine-conn
                         query-resolve-many-fields
                         (map field->resolve-many-entry))
        lookup-fields (->> engine-conn
                           query-resolve-lookup-fields
                           (map field->resolve-lookup-entry))]
    (-> lacinia-schema
        (attach-one-resolvers one-fields engine-conn)
        (attach-many-resolvers many-fields engine-conn)
        (attach-lookup-resolvers lookup-fields engine-conn))))
