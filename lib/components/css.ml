module Classes = struct
  module StringSet = Set.Make (String)

  let merge (classes : string list list) =
    classes
    |> List.flatten
    |> StringSet.of_list
    |> StringSet.to_seq
    |> List.of_seq
  ;;
end
