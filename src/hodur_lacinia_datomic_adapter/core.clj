(ns hodur-lacinia-datomic-adapter.core
  (:require [camel-snake-kebab.core :refer [->camelCaseKeyword
                                            ->kebab-case-string]]
            [datomic.client.api :as datomic]
            [datascript.core :as d]
            [datascript.query-v3 :as q]
            [com.walmartlabs.lacinia.resolve :as resolve]
            ;;FIXME: these below are probably not needed
            [hodur-engine.core :as engine]
            [hodur-lacinia-schema.core :as hls]
            [hodur-datomic-schema.core :as hds]
            [com.walmartlabs.lacinia.parser :as l-parser]
            [com.walmartlabs.lacinia.util :as l-util]
            [com.walmartlabs.lacinia.schema :as l-schema]
            [com.walmartlabs.lacinia :as lacinia]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;FIXME Datomic does not understand (:db/id :as :id) as a selector, so we need to help it
(defn ^:private find-target-id
  [selector]
  (some->> selector
           (filter seq?)
           (filter #(= :db/id (first %)))
           first
           last))

;;FIXME Datomic does not understand (:db/id :as :id) as a selector, so we need to help it
(defn ^:private fix-db-id
  [m target-id-name]
  (if-let [v (get m :db/id)]
    (-> m
        (dissoc :db/id)
        (assoc target-id-name v))
    m))

;;FIXME Datomic does not understand (:db/id :as :id) as a selector, so we need to help it
(defn ^:private fix-db-ids
  [c target-id-name]
  (map #(fix-db-id % target-id-name) c))

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
         has-next (<= (+ offset limit) total-count)
         target-id-name (find-target-id selector)]
     {:totalCount total-count
      :pageInfo {:totalPages (int (Math/ceil (/ total-count limit)))
                 :currentPage (int (Math/ceil (/ offset limit)))
                 :pageSize limit
                 :currentOffset offset
                 :hasPrev has-prev
                 :prevOffset (if has-prev (- offset limit) 0)
                 :hasNext has-next
                 :nextOffset (if has-next (+ offset limit) offset)}
      :nodes (fix-db-ids (pull-many db selector eids) target-id-name)})))

(defn ^:private fetch-one
  [db selector find where]
  (let [eid (-> (datomic/q {:query (concat `[:find ~find :where] where)
                            :args [db]})
                flatten first)
        target-id-name (find-target-id selector)]
    (fix-db-id (datomic/pull db selector eid) target-id-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private pull-param-type
  [param]
  (cond
    (:lacinia->datomic.param/eid param)    :eid
    (:lacinia->datomic.param/lookup param) :lookup
    :else                                  :unknown))

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

(defn ^:private query-resolve-pull-fields
  [engine-conn]
  (let [selector '[* {:field/parent [*]
                      :param/_parent [*]}]
        eids (-> (q/q '[:find ?f
                        :where
                        [?f :lacinia->datomic.query/type :pull]
                        [?f :field/parent ?t]
                        [?t :lacinia/query true]
                        [?t :lacinia/tag true]
                        [?f :lacinia/tag true]
                        [?p :param/parent ?f]
                        (or [?p :lacinia->datomic.param/lookup]
                            [?p :lacinia->datomic.param/eid])]
                      @engine-conn)
                 vec flatten)]
    (->> eids
         (d/pull-many @engine-conn selector))))

(defn ^:private query-resolve-find-fields
  [engine-conn]
  (let [selector '[* {:field/parent [*]
                      :param/_parent [*]}]
        eids (-> (q/q '[:find ?f
                        :where
                        [?f :lacinia->datomic.query/type :find]
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

(defn ^:private field->resolve-pull-entry
  [{:keys [field/camelCaseName field/parent param/_parent] :as field}]
  {:field-name camelCaseName
   :args (reduce
          (fn [m param]
            (assoc m (:param/camelCaseName param)
                   {:type (pull-param-type param)
                    :ident (case (pull-param-type param)
                             :lookup (:lacinia->datomic.param/lookup param)
                             :none)
                    :transform (:lacinia->datomic.param/transform param)}))
          {} _parent)})

(defn ^:private field->resolve-lookup-entry
  [{:keys [field/camelCaseName field/parent
           :lacinia->datomic.field/lookup
           :lacinia->datomic.field/reverse-lookup] :as field}]
  {:lacinia-type-name (:type/PascalCaseName parent)
   :lacinia-field-name camelCaseName
   :lookup lookup
   :reverse-lookup reverse-lookup})

(defn ^:private field->resolve-find-entry
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resolver functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti ^:private execute-fetch
  (fn [db {:keys [resolver-type] :as payload}]
    (clojure.pprint/pprint payload)
    resolver-type))

(defmethod execute-fetch :list-resolver
  [db {:keys [placeholder selector offset limit where] :as payload}] 
  (-> (fetch-page db selector placeholder where
                  {:offset offset
                   :limit limit})
      (resolve/with-context {:where where})))

(defmethod execute-fetch :one-resolver
  [db {:keys [placeholder selector offset limit where] :as payload}]
  (-> (fetch-one db selector placeholder where)
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

(defmethod get-field-selection :list-resolver
  [_ ctx]
  (context->field-selection ctx [:nodes]))

(defmethod get-field-selection :one-resolver
  [_ ctx]
  (context->field-selection ctx))

(defmethod get-field-selection :lookup-resolver
  [_ ctx]
  (context->field-selection ctx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti ^:private get-placeholder
  (fn [resolver-type ctx] resolver-type))

(defmethod get-placeholder :list-resolver
  [_ _]
  '?e)

(defmethod get-placeholder :one-resolver
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

(defmethod get-where :list-resolver
  [_ {:keys [args field-entry]}]
  (reduce-kv (fn [c arg-k arg-v]
               (if-let [{:keys [where-builder-fn]}
                        (get (:args field-entry) arg-k)]
                 (where-builder-fn c args)
                 c))
             [] args))

(defmethod get-where :one-resolver
  [_ {:keys [args field-entry]}]
  (reduce-kv (fn [a in-arg-name in-arg-val]
               (let [{:keys [type ident transform]} (get-in field-entry [:args in-arg-name])
                     arg-val (if transform
                               ((find-var transform) in-arg-val)
                               in-arg-val)]
                 (if type
                   (reduced (cond
                              (= :eid type) [['?e :db/id (Long/parseLong arg-val)]]
                              (= :lookup type) [['?e ident arg-val]])))))
             nil args))

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

(defn ^:private attach-pull-resolvers
  [lacinia-schema pull-fields engine-conn]
  (->> pull-fields
       (reduce
        (fn [m {:keys [field-name] :as field-entry}]
          (assoc-in m [:queries field-name :resolve]
                    (create-resolver :one-resolver field-entry engine-conn)))
        lacinia-schema)))

(defn ^:private attach-find-resolvers
  [lacinia-schema find-fields engine-conn]
  (->> find-fields
       (reduce
        (fn [m {:keys [field-name] :as field-entry}]
          (assoc-in m [:queries field-name :resolve]
                    (create-resolver :list-resolver field-entry engine-conn)))
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
  (let [pull-fields (->> engine-conn
                         query-resolve-pull-fields
                         (map field->resolve-pull-entry))
        find-fields (->> engine-conn
                         query-resolve-find-fields
                         (map field->resolve-find-entry))
        lookup-fields (->> engine-conn
                           query-resolve-lookup-fields
                           (map field->resolve-lookup-entry))]
    (-> lacinia-schema
        (attach-pull-resolvers pull-fields engine-conn)
        (attach-find-resolvers find-fields engine-conn)
        (attach-lookup-resolvers lookup-fields engine-conn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn transform-email
  [v]
  (clojure.string/lower-case v))

(defn build-employee-name-search-where
  [c {:keys [nameSearch] :as args}]
  (println "where builder called")
  (conj c ['?e :employee/email]))

(def hodur-shared-schema
  '[^{:lacinia/tag true
      :datomic/tag true}
    Employee
    [^{:type ID
       :lacinia/tag true
       :lacinia->datomic.field/dbid true}
     id
     
     ^{:type String
       :lacinia/tag true
       :datomic/tag true
       :datomic/unique :db.unique/identity}
     email

     ^{:type String
       :lacinia/tag true
       :datomic/tag true}
     first-name -> firstName -> :employee/first-name

     ^{:type String
       :lacinia/tag true
       :datomic/tag true}
     last-name

     ^{:type String
       :lacinia/tag true
       :lacinia/resolve :employee/full-name-resolver
       :lacinia->datomic.field/depends-on [:employee/first-name
                                           :employee/last-name]}
     full-name
     
     ^{:type Employee
       :optional true
       :datomic/tag true
       :lacinia/tag true}
     supervisor

     ^{:type EmployeeList
       :lacinia/tag-recursive true
       :lacinia->datomic.field/reverse-lookup :employee/supervisor}
     reportees
     [^{:type Integer
        :optional true
        :default 0 
        :lacinia->datomic.param/offset true}
      offset
      ^{:type Integer
        :optional true
        :default 50
        :lacinia->datomic.param/limit true}
      limit]
     
     ^{:type ProjectList
       :lacinia/tag-recursive true
       :lacinia->datomic.field/lookup :employee/projects}
     projects
     [^{:type Integer
        :optional true
        :default 0 
        :lacinia->datomic.param/offset true}
      offset
      ^{:type Integer
        :optional true
        :default 50
        :lacinia->datomic.param/limit true}
      limit]

     ^{:type Project
       :cardinality [0 n]
       :datomic/tag true}
     projects]

    ^{:datomic/tag true
      :lacinia/tag true}
    Project
    [^{:type ID
       :lacinia/tag true
       :lacinia->datomic.field/dbid true}
     id

     ^{:type String
       :lacinia/tag true
       :datomic/tag true}
     name

     ^{:type String
       :lacinia/tag true
       :datomic/tag true}
     description
     ]

    ^{:datomic/tag-recursive true
      :lacinia/tag-recursive true
      :enum true}
    EmploymentType
    [FULL_TIME PART_TIME]])

(def hodur-lacinia-pagination-schema
  '[^{:lacinia/tag-recursive true}
    ProjectList
    [^Integer
     total-count
     
     ^PageInfo
     page-info

     ^{:type Project
       :optional true
       :cardinality [0 n]}
     nodes]

    ^{:lacinia/tag-recursive true}
    EmployeeList
    [^Integer
     total-count
     
     ^PageInfo
     page-info

     ^{:type Employee
       :cardinality [0 n]}
     nodes]
    
    ^{:lacinia/tag-recursive true}
    PageInfo
    [^{:type Integer}
     total-pages

     ^{:type Integer}
     current-page

     ^{:type Integer}
     page-size
     
     ^{:type Integer}
     current-offset

     ^{:type Boolean}
     has-next
     
     ^{:type Integer}
     next-offset

     ^{:type Boolean}
     has-prev
     
     ^{:type Integer}
     prev-offset]])

(def hodur-lacinia-query-schema
  '[^{:lacinia/tag-recursive true
      :lacinia/query true}
    QueryRoot
    [^{:type Employee
       :lacinia->datomic.query/type :pull}
     employee
     [^{:type String
        :optional true
        :lacinia->datomic.param/lookup :employee/email
        :lacinia->datomic.param/transform hodur-lacinia-datomic-adapter.core/transform-email}
      email
      ^{:type ID
        :optional true
        :lacinia->datomic.param/eid true}
      id]

     ^{:type Project
       :lacinia->datomic.query/type :pull}
     project
     [^{:type ID
        :lacinia->datomic.param/eid true}
      id]

     ^{:type EmployeeList
       :lacinia->datomic.query/type :find
       :lacinia/resolve :project/upsert}
     employees
     [^{:type String
        :optional true
        :lacinia->datomic.param/where-builder hodur-lacinia-datomic-adapter.core/build-employee-name-search-where}
      name-search
      ^{:type Integer
        :optional true
        :default 0 
        :lacinia->datomic.param/offset true}
      offset
      ^{:type Integer
        :optional true
        :default 50
        :lacinia->datomic.param/limit true}
      limit]]])

(def hodur-lacinia-mutation-schema
  '[^{:lacinia/tag-recursive true
      :lacinia/input true
      :lacinia->datomic.input/map-to Employee}
    UpsertEmployeeInput
    [^String email
     ^String first-name
     ^String last-name]

    ^{:lacinia/tag-recursive true
      :lacinia/input true
      :lacinia->datomic.input/map-to Project}
    UpsertProjectInput
    [^String name
     ^String description]

    ^{:lacinia/tag-recursive true
      :lacinia/input true}
    AddProjectToEmployeeInput
    [^{:type ID
       :lacinia->datomic.input/attach-from Project
       :lacinia->datomic.input/dbid true}
     project-id
     ^UpsertEmployeeInput employee]

    ^{:lacinia/tag-recursive true
      :lacinia/input true}
    AddEmployeeToProjectInput
    [^{:type ID
       :lacinia->datomic.input/attach-from Project
       :lacinia->datomic.input/dbid true}
     employee-id
     ^UpsertProjectInput project]
    
    ^{:lacinia/tag-recursive true
      :lacinia/input true}
    AttachProjectToEmployeeInput
    [^{:type ID
       :lacinia->datomic.input/attach-from Project
       :lacinia->datomic.input/dbid true}
     project-id
     ^{:type ID
       :lacinia->datomic.input/attach-from Employee
       :lacinia->datomic.input/dbid true}
     employee-id]

    ^{:lacinia/tag-recursive true
      :lacinia/input true}
    AttachEmployeeToSupervisorInput
    [^{:type ID
       :lacinia->datomic.input/attach-from Employee
       :lacinia->datomic.input/dbid true}
     employee-id
     ^{:type ID
       :lacinia->datomic.input/attach-from Employee
       :lacinia->datomic.input/dbid true}
     supervisor-id]

    ^{:lacinia/tag-recursive true
      :lacinia/input true}
    DeleteProjectInput
    [^{:type ID
       :lacinia->datomic.input/delete-from Project
       :lacinia->datomic.input/dbid true}
     project-id]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ^{:lacinia/tag-recursive true
      :lacinia/mutation true}
    MutationRoot
    [^{:type Employee
       :lacinia->datomic.mutation/type :upsert
       :lacinia/resolve :project/upsert}
     upsert-employee
     [^UpsertEmployeeInput input]

     ^{:type Project
       :lacinia->datomic.mutation/type :upsert
       :lacinia/resolve :project/upsert}
     upsert-project
     [^UpsertProjectInput input]

     ^{:type Employee
       :lacinia->datomic.mutation/type :add-to
       :lacinia/resolve :project/upsert}
     add-project-to-employee
     [^AddProjectToEmployeeInput input]

     ^{:type Project
       :lacinia->datomic.mutation/type :add-to
       :lacinia/resolve :project/upsert}
     add-user-to-project
     [^AddEmployeeToProjectInput input]

     ^{:type Employee
       :lacinia->datomic.mutation/type :attach-to
       :lacinia/resolve :project/upsert}
     attach-project-to-employee
     [^AttachProjectToEmployeeInput input]

     ^{:type Employee
       :lacinia->datomic.mutation/type :attach-to
       :lacinia/resolve :project/upsert}
     attach-employee-to-supervisor
     [^AttachEmployeeToSupervisorInput input]

     ^{:type ID
       :lacinia->datomic.mutation/type :delete
       :lacinia/resolve :project/upsert}
     delete-project
     [^DeleteProjectInput input]]])

(def conn (engine/init-schema hodur-shared-schema
                              hodur-lacinia-query-schema
                              hodur-lacinia-mutation-schema
                              hodur-lacinia-pagination-schema))

#_(d/q '[:find (pull ?f [:field/name
                         :lacinia/tag])
         :where
         [?t :type/name "Employee"]
         [?f :field/parent ?t]
         [?f :field/name "projects"]]
       @conn)

(def lacinia-schema (hls/schema conn))

(def datomic-schema (hds/schema conn))



(defn full-name-resolver
  [context args {:keys [:firstName :lastName] :as resolved-value}]
  (str firstName " " lastName))

(defn ^:private input-type->map-to-type
  [input-type-name engine-conn]
  (if-let [map-to-name (-> @engine-conn
                           (d/pull '[:lacinia->datomic.input/map-to]
                                   [:type/PascalCaseName input-type-name])
                           :lacinia->datomic.input/map-to)]
    (-> @engine-conn
        (d/pull '[* {:field/_parent
                     [{:field/type [*]} *]}]
                [:type/name (str map-to-name)]))))

(defn ^:private map-input-to-datomic
  [input map-to-type]
  (let [fields (:field/_parent map-to-type)]
    (reduce-kv (fn [m k v]
                 (if-let [field (->> fields
                                     (filter #(= k (:field/camelCaseName %)))
                                     first)]
                   (if (= true (:datomic/tag field))
                     (assoc m (keyword (name (:type/kebab-case-name map-to-type))
                                       (name (:field/kebab-case-name field))) v)
                     m)
                   m))
               {} input)))

(defn project-upsert
  [{:keys [db-conn] :as ctx} {:keys [input] :as args} resolved-value]
  ;;FIXME this should attach to all where [?f :lacinia->datomic.mutation/type :upsert]
  ;;FIXME map the :tx-data as per indicated on the input's :lacinia->datomic.input/map-to
  (let [tx-result (datomic/transact db-conn
                                    {:tx-data [{:project/name (:name input)
                                                :project/description (:description input)}]})
        db-after (:db-after tx-result)
        tx-data (:tx-data tx-result)
        ;;FIXME last here works only if something changes in the db, otherwise last will be the inst part of the datom
        datom (last tx-data)
        eid (:e datom)
        ;;FIXME the selector needs to be in sync with the one on the query/pull interfaces
        pulled (datomic/pull db-after '[*] eid)]
    ;;FIXME map to the field's type's lacinia-style (will need to create a selector - above)
    (clojure.pprint/pprint pulled)))

(def cfg {:server-type :ion
          :region "us-east-2"
          :system "datomic-cloud-luchini"
          :query-group "datomic-cloud-luchini"
          :endpoint "http://entry.datomic-cloud-luchini.us-east-2.datomic.net:8182/"
          :proxy-port 8182})

(def client (datomic/client cfg))

(defn ^:private ensure-db [client db-name]
  (-> client
      (datomic/create-database {:db-name db-name}))
  (let [db-conn (datomic/connect client {:db-name db-name})]
    (datomic/transact db-conn {:tx-data datomic-schema})
    db-conn))

(def db-conn (-> client
                 (ensure-db "hodur-test")))

(datomic/transact db-conn {:tx-data [{:employee/email "tl@work.co"
                                      :employee/first-name "Tiago"
                                      :employee/last-name "Luchini"}
                                     {:employee/email "me@work.co"
                                      :employee/first-name "Marcelo"
                                      :employee/last-name "Eduardo"}
                                     {:employee/email "zeh@work.co"
                                      :employee/first-name "Zeh"
                                      :employee/last-name "Fernandes"
                                      :employee/supervisor [:employee/email "tl@work.co"]}
                                     {:employee/email "a@work.co"
                                      :employee/first-name "A"
                                      :employee/last-name "Fernandes"
                                      :employee/supervisor [:employee/email "tl@work.co"]}
                                     {:employee/email "b@work.co"
                                      :employee/first-name "B"
                                      :employee/last-name "Fernandes"
                                      :employee/supervisor [:employee/email "tl@work.co"]}]})


(def prepared-schema (-> lacinia-schema
                         (l-util/attach-resolvers
                          {:employee/full-name-resolver full-name-resolver
                           :project/upsert project-upsert})
                         (attach-resolvers conn)))

(def compiled-schema (-> prepared-schema
                         l-schema/compile))



#_(lacinia/execute compiled-schema
                   "{ A: employeeByEmail (email: \"foo\") { firstName lastName fullName }
                    employeeById (id: 3) { email fullName }
                    B: employeeByEmail (email: \"bla\") { email fullName supervisor { firstName fullName } } }"
                   nil nil)

#_(lacinia/execute compiled-schema
                   "{ B: employeeByEmail (email: \"bla\") { email fullName supervisor { firstName fullName } } }"
                   nil nil)

#_(lacinia/execute compiled-schema
                   "{ employeeByEmail (email: \"foo\") { fullName firstName projects { name } supervisor { fullName } } }"
                   nil nil)



#_ (lacinia/execute
    compiled-schema
    "{ employee (email: \"tl@work.co\") { id fullName supervisor { fullName } projects { totalCount nodes { name } } reportees { totalCount nodes { fullName } } } }"
    nil {:db (datomic/db db-conn)})

#_(lacinia/execute
   compiled-schema
   "{ employee (email: \"tl@work.co\") { id fullName reportees (limit: 20) { totalCount pageInfo { totalPages } nodes { id fullName } } } }" nil {:db (datomic/db db-conn)})

#_(lacinia/execute
   compiled-schema
   "{ project (id: \"11448115068404235\") { id name } }" nil {:db (datomic/db db-conn)})

#_(lacinia/execute
   compiled-schema
   "{ employees (nameSearch: \"bla\") { totalCount nodes { id fullName } }
    # employee (email: \"tl@work.co\") { id firstName }
}" nil {:db (datomic/db db-conn)})

(lacinia/execute
 compiled-schema
 "{ employees (nameSearch: \"bla\") { totalCount nodes { id fullName } }
    employee (email: \"tl@work.co\") { id firstName }
}" nil {:db (datomic/db db-conn)})

#_(lacinia/execute
   compiled-schema
   "mutation { upsertProject (input: { name: \"Project X\" description: \"Mega project!!!!\"}) { id name } }" nil {:db-conn db-conn :db (datomic/db db-conn)})

#_(lacinia/execute
   compiled-schema
   "{ A:employee (id: \"42630264832131144\") { id fullName firstName supervisor { fullName } reportees { id fullName } }
    B:employee (email: \"zeh@work.co\") { id fullName firstName supervisor { fullName } }}"
   nil nil
   )

#_(datomic/q
   '[:find (pull ?p [*])
     :where
     [?p :project/name]]
   (datomic/db db-conn))


#_(-> (datomic/transact db-conn {:tx-data [{:employee/email "tl@worl.co"
                                            :employee/projects [67888245945401859]}]})
      :tx-data)


#_(datomic/pull (datomic/db db-conn)
                '[{:employee/projects [*]}]
                [:employee/email "tl@work.co"])



#_(clojure.pprint/pprint
   (datomic/pull (datomic/db db-conn)
                 '[:db/id
                   :employee/first-name
                   :employee/last-name
                   {(:employee/_supervisor :limit 1 :offset 200) [:employee/first-name :employee/last-name]}]
                 [:employee/email "tl@work.co"]))


#_(datomic/q
   '[:find (pull ?e [:db/id
                     :employee/first-name
                     :employee/last-name
                     {(:employee/_supervisor :limit 1 :offset 200) [:employee/first-name :employee/last-name]}]) (count ?r)
     :where
     [?e :employee/email "tl@work.co"
      ?e :employee/_supervisor ?r]]
   (datomic/db db-conn))

#_(datomic/q
   '[:find ?e (count ?r)
     :where
     [?e :employee/email "tl@work.co"]
     [?r :employee/supervisor ?e]]
   (datomic/db db-conn))


(comment (datomic/q
          '[:find ?e (count ?r)
            :where
            [?e :employee/email "tl@work.co"]
            [?r :employee/supervisor ?e]]
          (datomic/db db-conn))

         (datomic/q
          '[:find ?e
            :where
            [?e :employee/email "tl@work.co"]]
          (datomic/db db-conn))

         (datomic/q
          '[:find ?eid
            :in $ ?eid
            :where
            [?eid]]
          (datomic/db db-conn)
          42630264832131144)

         (datomic/pull (datomic/db db-conn)
                       '[*]
                       42630264832131144)

         
         (datomic/q
          '[:find ?e
            :where
            [?e :employee/email "zeh@work.co"]]
          (datomic/db db-conn))

         (def all-eids
           (flatten
            (datomic/q
             {:query '[:find ?e
                       :where
                       [?e :employee/first-name]]
              :args [(datomic/db db-conn)]
              :limit 2
              :offset 1})))


         ;; find tiago
         (fetch-one (datomic/db db-conn)
                    '[(:employee/first-name :as :firstName)]
                    '?e
                    [['?e :employee/email "tl@work.co"]])

         ;; paginate tiago's reportees
         (fetch-page (datomic/db db-conn)
                     '[:employee/first-name]
                     '?e
                     [['?s :employee/email "tl@work.co"]
                      ['?e :employee/supervisor '?s]]
                     {:limit 2})

         (datomic/q
          '[:find (count ?e)
            :where
            [?e :employee/first-name]]
          (datomic/db db-conn))





         (pull-many (datomic/db db-conn)
                    '[:employee/first-name]
                    [42630264832131144 37418579716472906])

         (pull-many (datomic/db db-conn)
                    '[:employee/first-name]
                    all-eids))
