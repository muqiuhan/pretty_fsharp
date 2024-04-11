open Pretty_fsharp

let _ =
  let condition = ref true in
  Wait.run condition;
  Sys.argv
  |> Array.to_list
  |> (function
        | []
        | _ :: [] ->
            failwith "No command"
        | _ :: rest -> rest)
  |> String.concat " "
  |> Compile.run
  |> Result.parse
  |> fun info_list ->
  condition := false;
  UI.show info_list
