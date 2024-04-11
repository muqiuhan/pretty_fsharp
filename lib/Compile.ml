open Core

let run command =
  let process_output_to_list2 command =
    let chan = Core_unix.open_process_in command in
    let res = ref ([] : string list) in
    let rec process_otl_aux () =
      In_channel.(input_line_exn chan)
      |> fun e ->
      res := e :: !res;
      process_otl_aux ()
    in
    try process_otl_aux () with
    | End_of_file ->
        let stat = Core_unix.close_process_in chan in
        List.rev !res, stat
  in
  process_output_to_list2 command |> fun (result, _) -> result
