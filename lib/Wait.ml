open Core
open Progress
module Ansi = Terminal.Style

let apply_color color s = Ansi.(code color) ^ s ^ Ansi.(code none)

let unlimited_bar min_interval =
  let frames =
    let width = 10 in
    List.init width ~f:(fun i ->
      String.concat
        ~sep:""
        (List.init width ~f:(fun x ->
           if x = i then
             apply_color (Ansi.fg @@ Color.ansi `cyan) ">>"
           else
             apply_color Ansi.faint "=")))
  in
  let spin = Line.spinner ~min_interval ~frames () in
  Line.(spin ++ spin ++ spin ++ spin ++ spin)


let run condition =
  with_reporter
    (unlimited_bar (Some (Progress.Duration.of_ms 80.)))
    (fun report ->
       Caml_threads.Thread.create
         (fun () ->
            while !condition do
              report ();
              Caml_threads.Thread.delay 0.1
            done)
         ()
       |> ignore)
