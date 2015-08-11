PACKAGE=enumerators
SOURCES=32/beint.ml 32/beint.mli 64/beint.ml 64/beint.mli enumerator.ml enumerator.mli enumerators.mllib
TARGET_NAMES=enumerator.cmi $(PACKAGE).cma $(PACKAGE).cmxa $(PACKAGE).a
TARGETS=$(addprefix _build/, $(TARGET_NAMES))

.PHONY: all check check_coverage check_coverage_html install uninstall clean

all: $(TARGETS)

_build/%: $(SOURCES)
	ocamlbuild -use-ocamlfind $*

check:
	ocamlbuild -use-ocamlfind tests.native --

tests.bisect:
	ocamlbuild -use-ocamlfind -package bisect_ppx tests.native -- -runner sequential
	mv tests.native $@

check_coverage: tests.bisect
	./tests.bisect -runner sequential
	(cd _build && bisect-ppx-report ../bisect*.out -summary-only -text /dev/stdout)
	rm -f bisect*.out

check_coverage_html: tests.bisect
	./tests.bisect -runner sequential
	(cd _build && bisect-ppx-report ../bisect*.out -html ../coverage)
	rm -f bisect*.out

install: uninstall
	ocamlfind install $(PACKAGE) $(TARGETS) META

uninstall:
	ocamlfind remove $(PACKAGE)

clean:
	rm -rf coverage
	ocamlbuild -clean
