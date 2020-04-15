(lang dune 2.0)

(package
  (name murder-generator)
  (synopsis "A program to generate random murder parties")
  (description "This program combines storylets to create a custom scenario.")
	(license AGPL-3.0-or-later)
	(maintainers "Martin Constantino–Bodin <martin.bodin@ens-lyon.org>")
	(source (github Mbodin/murder-generator))
	(homepage "https://mbodin.github.io/murder-generator/"))

(explicit_js_mode)

(using menhir 2.0)

