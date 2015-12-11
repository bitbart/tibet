XML_LIGHT = xml-light

.PHONY: clean


all:
	ocamlopt -c errors.ml
	ocamlopt -c tools.ml
	ocamlopt -c tipi.ml
	sed -i '1iopen Tipi;;open Tools;;open Errors;;' extTipi.ml
	ocamlopt -c extTipi.ml
	sed -i '1iopen Tipi;;open Tools;;open Errors;;open ExtTipi;;open Unix;;' python.ml
	ocamlopt -c -pp camlp4o python.ml
	sed -i '1iopen Tipi;;open Tools;;open Errors;;open ExtTipi;;open Python;;' kindsystem.ml
	ocamlopt -c kindsystem.ml
	sed -i '1iopen Errors;;open Tipi;;open ExtTipi;;open Python;;' mapping.ml
	ocamlopt -c mapping.ml
	sed -i '1iopen Errors;;open Tipi;;open Mapping;;open Printf;;' toXML.ml
	ocamlopt -c toXML.ml
	sed -i '1iopen Tipi;;open Mapping;;open ToXML;;open Errors;;' fromXML.ml
### ocamlopt -c fromXML.ml
	ocamlopt -c -I +xml-light xml-light.cmxa fromXML.ml
	sed -i '1iopen Errors;;open FromXML;;' cparser.ml
### ocamlopt -c -pp camlp4o cparser.ml
	ocamlopt -c -I +xml-light xml-light.cmxa -pp camlp4o cparser.ml
	sed -i '1iopen Errors;;open Tipi;;open ExtTipi;;open Mapping;;open ToXML;;open FromXML;;open Python;;open Kindsystem;;' monitor.ml
	ocamlopt -c -I +xml-light xml-light.cmxa monitor.ml
	sed -i '1iopen Tipi;;open ExtTipi;;open Mapping;;open Kindsystem;;open ToXML;;open FromXML;;open Cparser;;open Monitor;;' main.ml
	ocamlopt -c -I +xml-light xml-light.cmxa main.ml

	ocamlopt -o ctu unix.cmxa str.cmxa xml-light.cmxa errors.cmx tools.cmx tipi.cmx extTipi.cmx python.cmx kindsystem.cmx mapping.cmx toXML.cmx fromXML.cmx cparser.cmx monitor.cmx main.cmx


test:
	ocamlopt -c errors.ml
	ocamlopt -c tools.ml
	ocamlopt -c tipi.ml
	ocamlopt -c extTipi.ml
	ocamlopt -c -pp camlp4o python.ml
	ocamlopt -c kindsystem.ml
	ocamlopt -c mapping.ml
	ocamlopt -c toXML.ml
	ocamlopt -c fromXML.ml
	ocamlopt -c -pp camlp4o cparser.ml
	ocamlopt -c monitor.ml

	ocamlopt -c tests/testPython.ml
	ocamlopt -I tests -c tests/main.ml

	ocamlopt -o applicationTest unix.cmxa str.cmxa xml-light.cmxa errors.cmx tools.cmx tipi.cmx extTipi.cmx python.cmx kindsystem.cmx  mapping.cmx toXML.cmx fromXML.cmx cparser.cmx monitor.cmx tests/testPython.cmx tests/main.cmx


clean:
	@'rm' *.cmo *.cmi *.cmx *.o ctu 2>/dev/null || true
	@'rm' tests/*.cmi tests/*.cmx tests/*.o applicationTest 2>/dev/null || true 
