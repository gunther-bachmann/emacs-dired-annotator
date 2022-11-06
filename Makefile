.DEFAULT_GOAL := test

all: pre_package pkg_dash_el pkg_uuid compile test

pre_package:
	[ -d "build" ] || mkdir "build"

pkg_dash_el_clone: pre_package
	cd build ; \
	[ -d "dash.el" ] || git clone https://github.com/magnars/dash.el

pkg_dash_el: pkg_dash_el_clone
	cd build/dash.el ; \
	git fetch --tags ; \
	git checkout -b latest 2.19.1 || echo "using existing latest branch"

pkg_uuid_clone: pre_package
	cd build ; \
	[ -d "emacs-uuid" ] || git clone https://github.com/nicferrier/emacs-uuid

pkg_uuid: pkg_uuid_clone
	cd build/emacs-uuid ; \
	git checkout 1519bfeb0e31602b840bc8dd35d7c7e732c159fe -b latest  || echo "using existing latest branch"

compile: pkg_uuid pkg_dash_el
	@echo "byte compile:"
	emacs -q -l init.el --batch -f batch-byte-compile dired-annotator.el 2>&1 | tee compile.out.txt
	@cat compile.out.txt | grep -qe '^dired-annotator\.el:[0-9]*:[0-9]*: '  && { echo "compiler warnings exist"; exit 1; } || echo -n ""

test: compile
	@echo "test:"
	emacs -q -l init.test.el --batch --eval "(ert-run-tests-batch)" | tee test.out.txt

clean:
	rm build/ -rf
	rm *.elc
	rm compile.out.txt
	rm test.out.txt
