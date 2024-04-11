type t = {
  path: string;
  line: int;
  char: int;
  kind: string;
  code: string;
  msg: string;
}

and info = t

val parse : string list -> info list
