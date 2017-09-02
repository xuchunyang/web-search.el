EMACS ?= emacs

test:
	$(EMACS) -Q --batch -f batch-byte-compile web-search.el
	$(EMACS) -Q --batch -L . -l web-search-tests.el -f ert-run-tests-batch-and-exit
