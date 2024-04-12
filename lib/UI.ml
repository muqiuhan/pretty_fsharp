open Info
open Core

let show_kind_and_code info =
  match info.kind with
  | "error" -> Ocolor_format.printf "￫ @{<red>ERROR @{<ul,bold>[%s]@}@}" info.code
  | "warning" -> Ocolor_format.printf "￫ @{<yellow>WARNING @{<ul,bold>[%s]@}@}" info.code
  | _ -> Ocolor_format.printf "￫ @{<white>WARNING @{<ul,bold>[%s]@}@}" info.code


let show_code info =
  let file_value = info.path |> Core.In_channel.read_lines |> Array.of_list in
  let pre, curr, next = info.line - 2, info.line - 1, info.line in
  let pre_line, curr_line, next_line =
    ( (if pre < 0 then "" else file_value.(pre)),
      file_value.(curr),
      if next > Array.length file_value then "" else file_value.(next) )
  in
  let curr_line_pre, curr_line_rest =
    ( String.sub curr_line ~pos:0 ~len:(info.char - 1),
      String.sub curr_line ~pos:(info.char - 1) ~len:(String.length curr_line - info.char) )
  in
  Ocolor_format.printf
    {|

@{<grey>%d | %s@}
@{<cyan>@{<hl,bold>%d@}@} | @{<grey>%s@}@{<white>@{<hl,ul,bold>%s@}@}
%s |%s￮ @{<cyan>@{<ul>%s@}@}
@{<grey>%d | %s@}

|}
    pre
    pre_line
    curr
    curr_line_pre
    curr_line_rest
    (String.make (string_of_int curr |> String.length) ' ')
    (String.make info.char ' ')
    info.msg
    next
    next_line


let show_path info = Ocolor_format.printf {|
ꭍ @{<green>%s(%d,%d)@}
|} info.path info.line info.char

let show info_list =
  Out_channel.newline stdout;
  List.iter
    ~f:(fun info ->
      show_path info;
      show_kind_and_code info;
      show_code info)
    info_list
