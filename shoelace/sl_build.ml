open Base
module File = Bos.OS.File

type value = { type_ : string option [@key "type"] [@default None] }
[@@deriving show { with_path = false }, yojson { strict = false }]

type attribute =
  { name : string
  ; description : string option [@default None]
  ; value : value option [@default None]
  }
[@@deriving show { with_path = false }, yojson { strict = false }]

type element =
  { name : string option [@default None]
  ; attributes : attribute list [@default []]
  }
[@@deriving show { with_path = false }, yojson { strict = false }]

type html = { elements : element list } [@@deriving yojson { strict = false }]
type contributions = { html : html } [@@deriving yojson { strict = false }]

type schema =
  { name : string
  ; contributions : contributions
  }
[@@deriving yojson { strict = false }]

let read_json file =
  match File.read (Fpath.v file) with
  | Ok content -> content
  | _ -> assert false
;;

let process_element (element : element) =
  let name =
    match element.name with
    | Some name -> name
    | _ -> assert false
  in
  let short_name = String.chop_prefix_exn name ~prefix:"sl-" in
  Fmt.str
    {|
let %s children = Tyxml.Html.Unsafe.node "%s" children
(* %s *)
|}
    short_name
    name
    (String.concat ~sep:" "
     @@ List.map element.attributes ~f:(fun elt -> elt.name))
;;

let () =
  let args = Array.to_list Stdlib.Sys.argv in
  let output_file = List.nth_exn args 1 in
  let json_file = List.nth_exn args 2 in
  (* Json *)
  let schema = read_json json_file in
  let safe = Yojson.Safe.from_string schema in
  let schema =
    match schema_of_yojson safe with
    | Ok schema -> schema
    | Error err -> Fmt.failwith "failed to parse: %s" err
  in
  Fmt.pr "%s@." schema.name;
  let sl_input =
    List.find schema.contributions.html.elements ~f:(fun item ->
      match item.name with
      | Some str when String.(str = "sl-carousel") -> true
      | _ -> false)
  in
  let lines = ref "let x = 10" in
  begin
    match sl_input with
    | Some el -> lines := !lines ^ process_element el
    | _ -> ()
  end;
  (* let schema = Yojson.Safe.Util *)
  (* print_endline schema.name; *)
  (* Process the file *)
  Fmt.pr "%s" @@ "Writing to: " ^ output_file;
  let oc = Stdlib.open_out output_file in
  Stdlib.output_string oc !lines;
  Stdlib.close_out oc
;;
