;;; web-search.el --- Open a web search  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
;; Package-Requires: ((seq "1.11") (emacs "24"))
;; Keywords: web, search

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

;; Inspired by https://github.com/zquestz/s

;;; Code:

(require 'seq)

(defvar web-search-providers
  '(("Google" "https://www.google.com/search?q=%s")
    ("GitHub" "https://github.com/search?utf8=✓&q=%s" "Code")
    ("Wikipedia" "https://en.wikipedia.org/wiki/Special:Search?search=%s" "Education")))

(defvar web-search-default-provider "Google")

(defun web-search--tags ()
  (seq-uniq (seq-mapcat #'cddr web-search-providers)))

(defun web-search--find-providers (tag)
  "Return a list of providers which is tagged by TAG."
  (seq-filter (lambda (p) (seq-contains (cddr p) tag)) web-search-providers))

(defun web-search--format-url (query provider)
  "Format a URL for search QUERY on PROVIDER.
PROVIDER can be a string (the name of one provider) or a
list (one provider, i.e., one element of `web-search-providers')."
  (let ((url (cond
              ((listp provider) (car provider))
              ((stringp provider) (cadr (assoc provider web-search-providers))))))
    (if url
        (format url (url-hexify-string query))
      (error "Unknown provider '%S'" provider))))

(defun web-search--format-urls (query providers)
  (mapcar (lambda (provider) (web-search--format-url query provider))
          providers))

;;;###autoload
(defun web-search (query &optional providers tag)
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
                                                        (web-search--find-providers tag)
                                                        ", "))
                                   (concat "on " (mapconcat #'identity providers ", "))))))
             (read-string prompt initial))))
     (list query providers tag)))
  (setq providers (or (and tag (web-search--find-providers tag))
                      providers
                      (list web-search-default-provider)))
  (mapc #'browse-url (web-search--format-urls query providers)))

(provide 'web-search)
;;; web-search.el ends here
