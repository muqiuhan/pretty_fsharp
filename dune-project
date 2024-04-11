(lang dune 3.15)

(name pretty_fsharp)

(generate_opam_files true)

(source
 (github muqiuhan/pretty_fsharp))

(authors "Muqiu Han")

(maintainers "Muqiu Han")

(license MIT)

(documentation https://github.com/muqiuhan/pretty_fsharp)

(package
 (name pretty_fsharp)
 (synopsis "Improving F# compilation output")
 (description "Improving F# compilation output")
 (depends ocaml dune core core_unix ocolor progress)
 (tags
  (compiler diagnostics error)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
