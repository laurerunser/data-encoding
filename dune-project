(lang dune 3.0)

(name data-encoding)

(generate_opam_files true)

(source
 (gitlab nomadic-labs/data-encoding))

(authors "Nomadic Labs")

(maintainers "Nomadic Labs")

(license "MIT")

(documentation https://nomadic-labs.gitlab.io/data-encoding)

(version 1.99.0)

(package
 (name data-encoding)
 (synopsis "Backend-independent codec declaration")
 (description "")
 (depends
   (ocaml (>= 4.14))
   dune
   (json-data-encoding (= :version))
   (binary-data-encoding (= :version))
   optint
   (ppx_expect :with-test)
   (ocamlformat (and :with-test (= 0.20.1)))
 )
)

(package
 (name json-data-encoding)
 (synopsis "JSON specific codecs and de/serialisation")
 (description "")
 (depends
   (ocaml (>= 4.14))
   dune
   optint
   (qcheck (and :with-test (>= 0.19)))
   (qcheck-core :with-test)
   (ppx_expect :with-test)
   (ocamlformat (and :with-test (= 0.20.1)))
 )
)

(package
 (name binary-data-encoding)
 (synopsis "Binary codecs and de/serialisation")
 (description "")
 (depends
   (ocaml (>= 4.14))
   dune
   optint
   suspendable-buffers
   (qcheck (and :with-test (>= 0.19)))
   (qcheck-core :with-test)
   (ppx_expect :with-test)
   (ocamlformat (and :with-test (= 0.20.1)))
 )
)

(package
 (name suspendable-buffers)
 (synopsis "Buffers with user-controled allocation")
 (description "")
 (depends
   (ocaml (>= 4.14))
   dune
   (ocamlformat (and :with-test (= 0.20.1)))
 )
)
