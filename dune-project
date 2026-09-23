(lang dune 3.24)

(name register-machines)
(using menhir 3.0)

(generate_opam_files true)

(source
 (github michaelleejl/register-machines))

(authors "Michael Lee <michael.lee@cl.cam.ac.uk>")

(maintainers "Michael Lee <michael.lee@cl.cam.ac.uk>")

(license MIT)

(package
 (name register-machines)
 (synopsis "A compiler for register machines")
 (description "A compiler for register machines for the University of Cambridge Part IB Computation Theory Course")
 (depends 
  (ocaml (>= 4.05.0)) 
  (menhir (>= 20260209))
  (cmdliner (>= 2.1.1))))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
