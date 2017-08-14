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

(defun web-search-tags ()
  (seq-uniq (seq-mapcat #'cddr web-search-providers)))

(defun web-search-provides-with-tag (tag)
  (seq-filter (lambda (p) (seq-contains (cddr p) tag)) web-search-providers))

(defun web-search-format-url (query provider)
  (let ((url (cadr (assoc provider web-search-providers))))
    (if url
        (format url (url-hexify-string query))
      (error "Provider named '%s' was not found" provider))))

;;;###autoload
(defun web-search (query &optional providers tag)
  (interactive
   (list
    (let ((initial (if (use-region-p)
                       (buffer-substring (region-beginning) (region-end))
                     (current-word))))
      (read-string "Web Search: " initial))
    (when (equal current-prefix-arg '(4)) ; One C-u
      (let* ((default web-search-default-provider)
             (prompt (format "Provider (default %s): " default)))
        (completing-read prompt web-search-providers nil t nil nil default)))
    (when (equal current-prefix-arg '(16)) ; Two C-u
      (completing-read "Tag: " (web-search-tags)))))
  (unless providers
    (setq providers (list web-search-default-provider)))
  (when tag
    (setq providers (web-search-provides-with-tag tag)))
  (dolist (p providers)
    (browse-url (web-search-format-url query p))))

(provide 'web-search)
;;; web-search.el ends here
