type info = {
  path: string;
  line: int;
  char: int;
  kind: string;
  code: string;
  msg: string;
}

let filter result =
  let module Filter = Set.Make (String) in
  Filter.of_list result |> Filter.to_list


let parse output =
  try
    begin
      String.split_on_char '(' output
      |> fun str ->
      (match str with
        | [path; rest] -> String.trim path, rest
        | _ -> raise (Invalid_argument output))
      |> fun (path, rest) ->
      (match String.split_on_char ')' rest with
        | [position; rest] ->
            Scanf.sscanf (String.trim position) "%d,%d" (fun line char -> (path, line, char), rest)
        | _ -> raise (Invalid_argument output))
      |> fun ((path, line, char), rest) ->
      (match String.split_on_char ':' rest with
        | [_; kind; rest] ->
            Scanf.sscanf (String.trim kind) "%s %s" (fun kind code ->
              (path, line, char, kind, code), rest)
        | _ -> raise (Invalid_argument output))
      |> fun ((path, line, char, kind, code), rest) ->
      match String.split_on_char '[' rest with
      | [info; _] -> { path; line; char; kind; code; msg = String.trim info }
      | _ -> raise (Invalid_argument output)
    end
  with
  | Scanf.Scan_failure _ -> raise (Invalid_argument output)


let parse result =
  filter result
  |> List.fold_left
       (fun info_list output ->
          try parse output :: info_list with
          | Invalid_argument _ -> info_list)
       []
