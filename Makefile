EMACS ?= emacs

test:
	$(EMACS) -Q --batch -L . -f batch-byte-compile web-search.el
	$(EMACS) -Q --batch -L . -l tests/web-search-tests.el -f ert-run-tests-batch-and-exit
	shellcheck web-search
	bash tests/web-search-tests
