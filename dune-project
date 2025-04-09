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
 (description
   "\| Subprocess is a library for safely and (relatively) easily working
   "\| launching processes with commands on Unix systems, with a focus on I/O.
   "\| It is intended to be easier to use than the `Unix` module.
   "\|
   "\| Checkout the documentation at
   "\| https://ninjaaron.github.io/ocaml-subprocess/subprocess/index.html
 )
 (depends (ocaml (>= 5.1)) dune (ppx_expect :with-test))
 (tags
  (unix processes io commands)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
