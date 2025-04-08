(lang dune 3.7)

(name subprocess)

(generate_opam_files true)

(source
 (github ninjaaron/ocaml-subprocess))

(authors "Aaron Christianson")

(maintainers "Aaron Christianson")

(license MPL-2.0)

(documentation https://github.com/ninjaaron/ocaml-subprocess)

(package
 (name subprocess)
 (synopsis "Nice interfaces handling I/O with Unix commands")
 (description "A longer description is in the README")
 (depends (ocaml (>= 5.1)) dune (ppx_expect :with-test))
 (tags
  (unix processes io commands)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
