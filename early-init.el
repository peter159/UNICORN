;; init.el --- MainEntry                            -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  <lipe6002@SHA-LPC-03254>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; Prefer loading newest compiled .el file
(customize-set-variable 'load-prefer-newer noninteractive)

(setq byte-compile-warnings '(cl-functions))

(if (functionp 'json-serialize)
    (message "Native JSON is available")
  (message "Native JSON is *not* available"))

(if (and (fboundp 'native-comp-available-p)
	 (native-comp-available-p))
    (progn
      (message "Native compilation is available")
      ;; native-compile all Elisp files under a site-lisp/local directory
      (native-compile-async (expand-file-name "site-lisp" user-emacs-directory) 'recursively)
      (setq package-native-compile t
	    native-comp-async-report-warnings-errors nil
            ;; Make native compilation happens asynchronously
            native-comp-deferred-compilation nil))
  (message "Native complation is *not* available"))

(setq package-enable-at-startup nil)

(setq load-prefer-newer noninteractive)

(setq frame-inhibit-implied-resize t)

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "data/eln-cache" user-emacs-directory)))
  (add-to-list 'native-comp-eln-load-path (expand-file-name "data/eln-cache" user-emacs-directory))
  )

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
