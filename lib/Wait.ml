open Progress
module Ansi = Terminal.Style

let apply_color color s = Ansi.(code color) ^ s ^ Ansi.(code none)

let pick_colour =
  let i = ref 0 in
  let colours = [| `magenta; `blue; `cyan; `green; `yellow; `red |] in
  fun () ->
    i := (!i + 1) mod Array.length colours;
    Color.ansi colours.(!i)


let unlimited_bar min_interval =
  let frames =
    let width = 10 in
    List.init width (fun i ->
      String.concat
        ""
        (List.init width (fun x ->
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
       Thread.create
         (fun () ->
            while !condition do
              report ();
              Thread.delay 0.1
            done)
         ()
       |> ignore)
