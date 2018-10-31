(ns schemas)

(def shared-schema
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

(def lacinia-pagination-schema
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

(def lacinia-query-schema
  '[^{:lacinia/tag-recursive true
      :lacinia/query true}
    QueryRoot
    [^{:type Employee
       :lacinia->datomic.query/type :pull}
     employee
     [^{:type String
        :optional true
        :lacinia->datomic.param/lookup :employee/email
        :lacinia->datomic.param/transform user/transform-email}
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
        :lacinia->datomic.param/where-builder user/build-employee-name-search-where}
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

(def lacinia-mutation-schema
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
