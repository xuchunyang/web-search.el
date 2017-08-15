;;; web-search.el --- Open a web search  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
;; URL: https://github.com/xuchunyang/web-search.el
;; Package-Requires: ((seq "2.3") (emacs "24"))
;; Keywords: web, search
;; Version: 0.1.0

(defconst web-search-version "0.1.0" "Version number of `web-search.el'.")

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Open a web search.  Idea from https://github.com/zquestz/s
;;
;; To use, in Emacs, use the command `web-search', in Terminal, use the Bash
;; script ./web-search under the same directory.
;;
;; Please see https://github.com/xuchunyang/web-search.el for documentation.

;;; Code:

(require 'seq)


;;; Custom

(defgroup web-search nil
  "Open a web search."
  :group 'tools)

(defcustom web-search-providers
  '(("Bing"              "https://www.bing.com/search?q=%s" "Search")
    ("Gist"              "https://gist.github.com/search?q=%s" "Code")
    ("GitHub"            "https://github.com/search?q=%s" "Code")
    ("Google"            "https://www.google.com/search?q=%s" "Search")
    ("Hacker News"       "https://hn.algolia.com/?q=%s" "Tech-News")
    ("MacPorts"          "https://www.macports.org/ports.php?by=name&substr=%s")
    ("Stack Overflow"    "https://stackoverflow.com/search?q=%s" "Code")
    ("Wikipedia"         "https://en.wikipedia.org/wiki/Special:Search?search=%s" "Education")
    ("YouTube"           "https://www.youtube.com/results?search_query=%s"))
  "Search providers, a list of (NAME URL TAG1 TAG2 ...).
URL must contains a %s token for the query string."
  :group 'web-search
  ;; Well, because I can't figure out how to write a more specific type
  :type 'sexp)

(defcustom web-search-default-provider "Google"
  "Default search provider."
  :group 'web-search
  :type (let ((providers
               (mapcar (lambda (p)
                         `(string :tag ,(car p) :value ,(car p)))
                       web-search-providers)))
          `(choice ,@providers)))


;;; Internal

(defun web-search--tags ()
  (seq-uniq (seq-mapcat #'cddr web-search-providers)))

(defun web-search--find-providers (tag)
  "Return a list of providers which is tagged by TAG."
  (or (seq-filter (lambda (p) (seq-contains (cddr p) tag)) web-search-providers)
      (error "Unknown tag '%s'" tag)))

(defun web-search--format-url (query provider)
  "Format a URL for search QUERY on PROVIDER.
PROVIDER can be a string (the name of one provider) or a
list (one provider, i.e., one element of `web-search-providers')."
  (let ((url (cond
              ((listp provider)
               (cadr provider))
              ((stringp provider)
               (cadr (seq-find (lambda (elt)
                                 (string= (downcase provider)
                                          (downcase (car elt))))
                               web-search-providers))))))
    (if url
        (format url (url-hexify-string query))
      (error "Unknown provider '%S'" provider))))

(defun web-search--format-urls (query providers)
  (mapcar (lambda (provider) (web-search--format-url query provider))
          providers))


;;; Commands

;;;###autoload
(defun web-search (query &optional providers tag)
  "Search for QUERY on website(s).

Without prefix argument, search on `web-search-default-provider'.
One C-u, choose a provider.
Two C-u, choose a tag (notes that a tag can match multiple providers)."
  (interactive
   (let* ((providers
           (if (equal current-prefix-arg '(4)) ; One C-u
               (let* ((default web-search-default-provider)
                      (prompt (format "Provider (default %s): " default)))
                 (list (completing-read prompt web-search-providers nil t nil nil default)))
             (list web-search-default-provider)))
          (tag
           (when (equal current-prefix-arg '(16)) ; Two C-u
             (completing-read "Tag: " (web-search--tags))))
          (query
           (let ((initial (if (use-region-p)
                              (buffer-substring (region-beginning) (region-end))
                            (current-word)))
                 (prompt (format "Search %s for: "
                                 (if tag
                                     (format "about %s on %s" tag
                                             (mapconcat #'identity
                                                        (mapcar #'car
                                                                (web-search--find-providers tag))
                                                        ", "))
                                   (concat "on " (mapconcat #'identity providers ", "))))))
             (read-string prompt initial))))
     (list query providers tag)))
  (setq providers (or (and tag (web-search--find-providers tag))
                      providers
                      (list web-search-default-provider)))
  (mapc #'browse-url (web-search--format-urls query providers)))


;;; Batch

(defun web-search-batch-print-usage ()
  (princ (format "\
Web search from the terminal.

Usage: %s <query> [options]

Options:
  -h, --help              display help
  -l, --list-providers    list supported providers
      --list-tags         list available tags
  -o, --output            output only mode
  -p, --provider string   search provider (default \"%s\")
  -t, --tag string        search tag
  -v, --verbose           verbose mode
      --version           display version
" (or (getenv "WEB_SEARCH")
      "emacs -Q --batch -l web-search.el -f web-search-batch --")
web-search-default-provider)))

(defun web-search-batch ()
  (unless noninteractive
    (funcall (if (fboundp 'user-error) #'user-error #'error)
             "`web-search-batch' can only be called in batch mode"))

  (setq argv (delete "--" argv))

  (when (or (seq-contains argv "-h")
            (seq-contains argv "--help"))
    (web-search-batch-print-usage)
    (kill-emacs 0))

  (when (seq-contains argv "--version")
    (princ (format "web-search.el %s\nGNU Emacs %s\n"
                   web-search-version emacs-version))
    (kill-emacs 0))

  (when (or (seq-contains argv "-l")
            (seq-contains argv "--list-providers"))
    (mapc (lambda (p) (princ (format "%s\n" (car p)))) web-search-providers)
    (kill-emacs 0))

  (when (seq-contains argv "--list-tags")
    (mapc (lambda (s) (princ (format "%s\n" s))) (sort (web-search--tags) #'string<))
    (kill-emacs 0))

  (let (arg
        verbose-mode
        output-only-mode
        query
        provider
        tag
        providers
        urls)
    (while (setq arg (pop argv))
      (pcase arg
        ((or "-v" "--verbose") (setq verbose-mode t))
        ((or "-o" "--output")  (setq output-only-mode t))
        ((or "-p" "--provider") (setq provider (pop argv)))
        ((or "-t" "--tag") (setq tag (pop argv)))
        (_ (setq query (if query
                           (concat query " " arg)
                         arg)))))
    (unless query
      (web-search-batch-print-usage)
      (kill-emacs 1))
    
    (setq providers (or (and tag (web-search--find-providers tag))
                        (and provider (list provider))
                        (list web-search-default-provider)))
    (setq urls (web-search--format-urls query providers))
    (when (or verbose-mode output-only-mode)
      (mapc (lambda (url) (princ (format "%s\n" url))) urls))
    (unless output-only-mode
      (mapc (lambda (url)
              (let ((proc (browse-url url)))
                (when (processp proc)
                  (accept-process-output proc))))
            urls))))

(provide 'web-search)
;;; web-search.el ends here
