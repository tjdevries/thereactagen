type t =
  { suggestion_id : int [@foreign Suggestion.table [ Suggestion.f_id ]]
  ; user_id : string [@foreign User.table [ User.f_twitch_user_id ]]
  ; vote : int
  }
[@@deriving
  combust
    ~name:"votes"
    ~constraints:
      [ Schema.table_primary_key
          ~name:"unique_suggestion_and_post"
          ~on_conflict:`REPLACE
          [ "suggestion_id"; "user_id" ]
      ]]
