open Base
open Bos.OS

(* let unwrap = function *)
(*   | Ok x -> x *)
(*   | _ -> assert false *)
(* ;; *)

(* Touch the test file if it doesn't exist *)
(* let test_stem = Fpath.to_string stem ^ ".test.ml" in *)
(* let test_fpath = Fpath.(snapshot_dir / test_stem) in *)
(* if not (unwrap @@ File.exists test_fpath) *)
(* then File.write test_fpath "" |> unwrap; *)
(* then Fmt.failwith "Didn't create a test file..."; *)

let print_one_rule name =
  Fmt.pr
    {|(rule
 (targets %s.generated.ml)
 (deps (:pp pp.exe) (:input %s.input.ml))
 (action (progn
          (run ./%%{pp} --impl %%{input} -o %%{targets})
          (run ocamlformat -i %%{targets}))))

(rule
 (alias runtest)
 (deps (file %s.generated.ml))
 (action (diff %s.test.ml %s.generated.ml)))

|}
    name
    name
    name
    name
    name
;;

let () =
  Fmt.pr "; NOTE: This file is auto-generated\n";
  Fmt.pr
    {|
(executable
 (name pp)
 (modules pp)
 (libraries ppx_combust ppxlib))

|};
  let snapshot_dir = Fpath.v "snapshots" in
  let contents = Dir.contents ~rel:true snapshot_dir in
  match contents with
  | Ok contents ->
    List.iter contents ~f:(fun file ->
      let name = Fpath.to_string file in
      let stem = Fpath.rem_ext ~multi:true file in
      Fmt.pr "; Checked %s\n" name;
      (* Write the build instructions *)
      if String.substr_index name ~pattern:".input.ml" |> Option.is_some
      then stem |> Fpath.to_string |> print_one_rule)
  | Error err -> Fmt.failwith "Oh no: %a" Rresult.R.pp_msg err
;;
