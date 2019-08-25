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

(setq make-backup-files nil
      org-export-with-section-numbers nil
      org-export-with-title t
      org-export-with-date t
      org-export-with-toc nil
      org-html-htmlize-output-type 'css
      org-html-metadata-timestamp-format "%B %-d, %Y"
      org-publish-timestamp-directory "./cache/")

(setq ajgrf/preamble "
<nav>
  <div class=\"container split\">
    <a class=\"brand\" href=\"/\">Alex Griffin</a>
    <span>
      <a class=\"nav-link\" href=\"/about/index.html\">About</a>
      <a class=\"nav-link\" href=\"/post/index.html\">Archives</a>
    </span>
  </div>
</nav>
")

(setq ajgrf/postamble "
<div class=\"container split\">
  <span>&copy; 2019 Alex Griffin. All rights reserved.</span>
  <span class=\"date\">%d</span>
</div>
")

(setq org-html-preamble  t
      org-html-preamble-format  `(("en" ,ajgrf/preamble))
      org-html-postamble t
      org-html-postamble-format `(("en" ,ajgrf/postamble)))

(setq org-publish-project-alist
      `(("site-org"
         :base-directory "./content/post"
         :recursive t
         :publishing-directory "./public/post"
         :publishing-function org-html-publish-to-html
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-head-extra "<link rel='stylesheet' href='/style.css'/>"
         :auto-sitemap t
         :sitemap-title "Archives"
         :sitemap-filename "index.org"
         :sitemap-style list
         :sitemap-sort-files anti-chronologically)
        ("site-about"
         :base-directory "./content/about"
         :publishing-directory "./public/about"
         :publishing-function org-html-publish-to-html
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-head-extra "<link rel='stylesheet' href='/style.css'/>")
        ("site-static"
         :base-directory "./static"
         :recursive t
         :publishing-directory "./public"
         :base-extension any
         :publishing-function org-publish-attachment)
        ("site"
         :components ("site-org" "site-about" "site-static"))))

(provide 'publish)
;;; publish.el ends here
