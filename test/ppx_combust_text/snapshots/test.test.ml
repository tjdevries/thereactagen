type t =
  { id : int [@primary { auto_increment = true }]
  ; username : string [@unique]
  ; display_name : string
  ; password : string
  }
[@@deriving combust ~name:"users"]

include struct
  let _ = fun (_ : t) -> ()

  include struct
    open Base
    open Petrol
    open Petrol.Sqlite3

    let table, fields =
      StaticSchema.declare_table
        schema
        ~name:"users"
        ~constraints:[]
        (let open Schema in
         [ field
             "id"
             ~ty:Type.int
             ~constraints:[ primary_key ~auto_increment:true (); not_null () ]
         ; field
             "username"
             ~ty:Type.text
             ~constraints:[ unique ~name:"username_unique" (); not_null () ]
         ; field "display_name" ~ty:Type.text ~constraints:[ not_null () ]
         ; field "password" ~ty:Type.text ~constraints:[ not_null () ]
         ])
    ;;

    let _ = table
    and _ = fields

    let Expr.[ f_id; f_username; f_display_name; f_password ] = fields

    let _ = f_id
    and _ = f_username
    and _ = f_display_name
    and _ = f_password

    let decode (id, (username, (display_name, (password, ())))) =
      { id; username; display_name; password }
    ;;

    let _ = decode

    let create ~username ~display_name ~password db =
      Query.insert
        ~table
        ~values:
          (let open Expr in
           [ f_username := s username
           ; f_display_name := s display_name
           ; f_password := s password
           ])
      |> Query.returning
           (let open Expr in
            [ f_id ])
      |> Request.make_one
      |> Petrol.find db
      |> Lwt_result.map fst
    ;;

    let _ = create

    let read id db =
      Query.select ~from:table fields
      |> Query.where
           (let open Expr in
            f_id = i id)
      |> Request.make_zero_or_one
      |> Petrol.find_opt db
      |> Lwt_result.map (Option.map ~f:decode)
    ;;

    let _ = read

    let delete id db =
      Query.delete ~from:table
      |> Query.where
           (let open Expr in
            f_id = i id)
      |> Request.make_zero
      |> Petrol.exec db
    ;;

    let _ = delete

    let find_one ?(select = fields) ~where ?(decode = decode) db =
      Query.select select ~from:table
      |> Query.where where
      |> Request.make_zero_or_one
      |> Petrol.find_opt db
      |> Lwt_result.map (Option.map ~f:decode)
    ;;

    let _ = find_one

    let find_many ?(select = fields) ~where ?(decode = decode) db =
      Query.select select ~from:table
      |> Query.where where
      |> Request.make_many
      |> Petrol.collect_list db
      |> Lwt_result.map (List.map ~f:decode)
    ;;

    let _ = find_many
  end
end [@@ocaml.doc "@inline"] [@@merlin.hide]
