;;; publish.el --- Build site with org-publish -*- lexical-binding: t -*-

;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;;
;;;
;;; This file is NOT part of GNU Emacs.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Run `org-publish-all' to generate the site.
;;
;; Thanks to https://gitlab.com/ambrevar/ambrevar.gitlab.io and
;; https://gitlab.com/pages/org-mode for providing example sites to follow.

;;; Code:

(add-to-list 'load-path "./lisp")

(require 'htmlize)
(require 'ox-publish)
(require 'webfeeder)

(setq debug-on-error t
      make-backup-files nil
      org-export-with-section-numbers nil
      org-export-with-title t
      org-export-with-date t
      org-export-with-toc nil
      org-html-doctype "html5"
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-html5-fancy t
      org-html-htmlize-output-type 'css
      org-html-metadata-timestamp-format "%Y-%m-%d"
      org-html-preamble  t
      org-html-postamble t
      org-publish-timestamp-directory "./cache/")

(setq org-html-head-extra
      "<link rel=\"stylesheet\" href=\"/style.css\"/>
<link rel=\"alternate\" type=\"application/atom+xml\" href=\"/atom.xml\" title=\"Alex Griffin\"/>
<link rel=\"icon\" type=\"image/x-icon\" href=\"/favicon.ico\"/>")

(setq org-html-preamble-format '(("en" "<nav>
<div class=\"container split\">
<a class=\"brand\" href=\"/\">Alex Griffin</a>
<span>
<a class=\"nav-link\" href=\"/about/\">About</a>
<a class=\"nav-link\" href=\"/post/\">Archives</a>
</span>
</div>
</nav>
<div class=\"container\">
<p class=\"date\">%d</p>
</div>
")))

(setq org-html-postamble-format '(("en" "<footer>
<div class=\"container\">
&copy; 2019 Alex Griffin. Unless otherwise noted, licensed under the
<a href=\"https://creativecommons.org/licenses/by-sa/4.0/\">CC BY-SA 4.0</a>.
</div>
</footer>
")))

(defun ajgrf/newest-entry-on-home-sitemap (title list)
  "Sitemap function to copy newest entry to the homepage.
TITLE is ignored.  LIST is a representation of the entries."
  (let* ((newest (caadr list))
         (link   (car (org-element-parse-secondary-string newest '(link))))
         (path   (org-element-property :path link)))
    (copy-file (concat "./content/post/" path)
               "./content/index.org"
               t)))

(defun ajgrf/multi-sitemap (title list)
  "Sitemap function to call multiple sitemap functions in turn.
TITLE and LIST are passed to each function."
  (ajgrf/newest-entry-on-home-sitemap title list)
  (org-publish-sitemap-default title list))

(setq org-publish-project-alist
      `(("site-posts"
         :base-directory "./content/post"
         :recursive t
         :publishing-directory "./public/post"
         :publishing-function org-html-publish-to-html
         :auto-sitemap t
         :sitemap-title "Archives"
         :sitemap-filename "index.org"
         :sitemap-function ajgrf/multi-sitemap
         :sitemap-style list
         :sitemap-sort-files anti-chronologically)
        ("site-other"
         :base-directory "./content"
         :recursive t
         :exclude "post/.*"
         :publishing-directory "./public"
         :publishing-function org-html-publish-to-html)
        ("site-static"
         :base-directory "./static"
         :recursive t
         :publishing-directory "./public"
         :base-extension any
         :publishing-function org-publish-attachment)
        ("site"
         :components ("site-posts" "site-other" "site-static"))))

(defun ajgrf/publish ()
  "Publish site, including web feeds."
  (org-publish-all)
  (setq webfeeder-default-author "Alex Griffin <a@ajgrf.com>")
  (webfeeder-build
   "atom.xml"
   "./public"
   "https://ajgrf.com/"
   (delete "post/index.html"
           (mapcar (lambda (f) (replace-regexp-in-string ".*/public/" "" f))
                   (directory-files-recursively "public/post" "index.html")))
   :title "Alex Griffin"))

(provide 'publish)
;;; publish.el ends here
