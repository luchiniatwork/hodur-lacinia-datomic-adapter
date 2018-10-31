(ns user
  (:require [com.walmartlabs.lacinia :as lacinia]
            [com.walmartlabs.lacinia.parser :as l-parser]
            [com.walmartlabs.lacinia.schema :as l-schema]
            [com.walmartlabs.lacinia.util :as l-util]
            [datomic.client.api :as datomic]
            [hodur-datomic-schema.core :as hds]
            [hodur-engine.core :as engine]
            [hodur-lacinia-schema.core :as hls]
            [schemas :as schemas]
            [hodur-lacinia-datomic-adapter.core :as adapter]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Not sure if this should be back on core
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn full-name-resolver
  [context args {:keys [:firstName :lastName] :as resolved-value}]
  (str firstName " " lastName))

#_(defn ^:private input-type->map-to-type
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lots of provided functions to make it "personal"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn transform-email
  [v]
  (clojure.string/lower-case v))

(defn build-employee-name-search-where
  [c {:keys [nameSearch] :as args}]
  (println "where builder called")
  (apply conj c ['[?e :employee/first-name ?first-name]
                 '[?e :employee/last-name ?last-name]
                 (list 'or
                       [(list '.startsWith ^String '?first-name nameSearch)])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Real initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def conn (engine/init-schema schemas/shared-schema
                              schemas/lacinia-query-schema
                              schemas/lacinia-mutation-schema
                              schemas/lacinia-pagination-schema))


(def lacinia-schema (hls/schema conn))

(def datomic-schema (hds/schema conn))

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
                         (adapter/attach-resolvers conn)))

(def compiled-schema (-> prepared-schema
                         l-schema/compile))

(comment )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-query [query]
  (lacinia/execute compiled-schema
                   query
                   nil {:db (datomic/db db-conn)}))

;; getting a list with a search parameters

(run-query
 "
{
  employees (nameSearch: \"Ze\") {
    totalCount
    nodes { email fullName }
  }
}")

;; getting a "one" with an eid
#_(run-query
   "
{
  employee (email: \"tl@work.co\") {
    email
    firstName
  }
}")

(comment )

;; getting one with a lot of attached stuff
#_(run-query
   "
{
  employee (email: \"tl@work.co\") {
    id
    fullName
    supervisor {
      fullName
    }
    projects {
      totalCount
      nodes {
        name
      }
    }
    reportees {
      totalCount
      nodes {
        fullName
      }
    }
  }
}")


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

#_(lacinia/execute
   compiled-schema
   "mutation { upsertProject (input: { name: \"Project X\" description: \"Mega project!!!!\"}) { id name } }" nil {:db-conn db-conn :db (datomic/db db-conn)})

#_(lacinia/execute
   compiled-schema
   "{ A:employee (id: \"42630264832131144\") { id fullName firstName supervisor { fullName } reportees { id fullName } }
    B:employee (email: \"zeh@work.co\") { id fullName firstName supervisor { fullName } }}"
   nil nil
   )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
