(lang dune 3.0)

(name data-encoding)

(generate_opam_files true)

(source
 (gitlab nomadic-labs/data-encoding))

(authors "Nomadic Labs")

(maintainers "Nomadic Labs")

(license "MIT")

(documentation https://nomadic-labs.gitlab.io/data-encoding)

(package
 (name data-encoding)
 (synopsis "Backend-independent codec declaration")
 (description "")
 (depends
   (ocaml (>= 4.14))
   dune
   (json-data-encoding (= :version))
   (binary-data-encoding (= :version))
   (qcheck (and :with-test (>= 0.19)))
   (qcheck-alcotest :with-test)
   (alcotest (and :with-test (>= 1.5.0)))
   (ppx_expect :with-test)
 )
)

(package
 (name json-data-encoding)
 (synopsis "JSON specific codecs and de/serialisation")
 (description "")
 (depends
   (ocaml (>= 4.14))
   dune
   (qcheck (and :with-test (>= 0.19)))
   (qcheck-alcotest :with-test)
   (alcotest (and :with-test (>= 1.5.0)))
   (ppx_expect :with-test)
 )
)

(package
 (name binary-data-encoding)
 (synopsis "Binary codecs and de/serialisation")
 (description "")
 (depends
   (ocaml (>= 4.14))
   dune
   (qcheck (and :with-test (>= 0.19)))
   (qcheck-alcotest :with-test)
   (alcotest (and :with-test (>= 1.5.0)))
   (ppx_expect :with-test)
 )
)
