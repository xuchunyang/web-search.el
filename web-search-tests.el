;;; web-search-tests.el --- Tests for web-search.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>

;;; Commentary:

;; To learn about writing testing with `ert', refer to (info "(ert) Top")

;;; Code:

(require 'ert)
(require 'web-search)

(ert-deftest web-search-providers--format-url:reddit ()
  "If query looks like '/r/subreddit query', handle it differently."
  (should (equal (web-search-providers--format-url:reddit "vim")
                 "https://www.reddit.com/search?q=vim"))
  (should (equal (web-search-providers--format-url:reddit "/r/emacs vim")
                 "https://www.reddit.com/r/emacs/search?q=vim&restrict_sr=on"))
  (should (equal (web-search-providers--format-url:reddit "/r/emacs")
                 "https://www.reddit.com/search?q=%2Fr%2Femacs"))
  (should (equal (web-search-providers--format-url:reddit "/r/emacs/")
                 "https://www.reddit.com/search?q=%2Fr%2Femacs%2F"))
  (should (equal (web-search-providers--format-url:reddit "/r/emacs/foo")
                 "https://www.reddit.com/search?q=%2Fr%2Femacs%2Ffoo")))

;;; web-search-tests.el ends here
