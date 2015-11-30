PACKAGE=enumerators
SOURCES=32/beint.ml 32/beint.mli 64/beint.ml 64/beint.mli enumerator.ml enumerator.mli enumerators.mllib
TESTS=tests.ml
TARGET_NAMES=enumerator.cmi $(PACKAGE).cma
NATIVE_TARGET_NAMES=$(PACKAGE).cmxa $(PACKAGE).a
TARGETS=$(addprefix _build/, $(TARGET_NAMES))
NATIVE_TARGETS=$(addprefix _build/, $(NATIVE_TARGET_NAMES))

.PHONY: all byte opt check check_coverage check_coverage_html install uninstall clean

all: byte opt

byte: $(TARGETS)

opt: $(NATIVE_TARGETS)

_build/%: $(SOURCES)
	ocamlbuild -use-ocamlfind $*

check: $(SOURCES) $(TESTS)
	ocamlbuild -use-ocamlfind tests.native --

check_coverage:
	ocamlbuild -use-ocamlfind -package bisect_ppx tests.native -- -runner sequential
	(cd _build && bisect-ppx-report ../bisect*.out -summary-only -text /dev/stdout)
	rm -f bisect*.out

check_coverage_html:
	ocamlbuild -use-ocamlfind -package bisect_ppx tests.native -- -runner sequential
	(cd _build && bisect-ppx-report ../bisect*.out -html ../coverage)
	rm -f bisect*.out

install: uninstall
	ocamlfind install $(PACKAGE) META $(TARGETS) -optional $(NATIVE_TARGETS)

uninstall:
	ocamlfind remove $(PACKAGE)

clean:
	rm -rf coverage
	ocamlbuild -clean
