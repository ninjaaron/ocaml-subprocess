Subprocess is a library for safely and (relatively) easily working
launching processes with commands on Unix systems. It is vaguely
inspired by Python's `subprocess` module, but do not expect it to have
quite the same API if you are familiar with that. It's intended to be
easier to use than the `Unix` module.

There is documentation. Use `dune build @odoc` in the repository to
generate the html documentation (in
`_build/default/_doc/_html/index.html`) or simply open
[lib/subprocess.mli](lib/subprocess.mli) to read the documentation
inline in the comments on the interface (from which the documentation
is generated).