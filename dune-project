(lang dune 3.24)

(name register-machines)
(using menhir 3.0)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name register-machines)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
  (ocaml (>= 4.05.0)) 
  (menhir (>= 20260209))
  (cmdliner (>= 2.1.1)))
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
