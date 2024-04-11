open Info
open Core

let show_kind_and_code info =
  match info.kind with
  | "error" -> Ocolor_format.printf "￫ @{<red>ERROR @{<ul,bold>[%s]@}@}" info.code
  | "warning" -> Ocolor_format.printf "￫ @{<yellow>WARNING @{<ul,bold>[%s]@}@}" info.code
  | _ -> Ocolor_format.printf "￫ @{<white>WARNING @{<ul,bold>[%s]@}@}" info.code


let show_code info =
  let file_value = info.path |> Core.In_channel.read_lines |> Array.of_list in
  let pre, curr, next = info.line, info.line + 1, info.line + 2 in
  let pre_line, curr_line, next_line = file_value.(pre), file_value.(curr), file_value.(next) in
  Ocolor_format.printf
    {|

@{<grey>%d | %s@}
@{<cyan>@{<hl,bold>%d@}@} | @{<white>@{<hl>%s@}@}
%s |%s￮ @{<cyan>@{<ul>%s@}@}
@{<grey>%d | %s@}

|}
    pre
    pre_line
    curr
    curr_line
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
