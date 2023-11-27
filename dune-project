(lang dune 3.7)

(name aoc)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Kim Nguyễn")

(maintainers "Kim Nguyễn")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name aoc)
 (synopsis "Advent of Code Solutions")
 (description "This bundles together solutions for the https://adventofcode.com programming event.")
 (depends ocaml dune)
 (tags
  (topics "algorithm" "programming")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
