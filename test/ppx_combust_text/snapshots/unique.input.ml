type suggestion =
  { id : int [@primary { auto_increment = true }]
  ; user_id : int [@foreign User.table [ User.f_id ]]
  }
[@@deriving
  combust
    ~name:"suggestions"
    ~constraints:
      [ Schema.table_primary_key
          ~name:"unique_suggestion_and_post"
          ~on_conflict:`REPLACE
          [ "suggestion_id"; "user_id" ]
      ]]
