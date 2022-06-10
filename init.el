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

;;; require package manager, config archives source and initialize all
(require 'package)

;; set http proxy, not need when set `git config --global http.proxy' in terminal
(defvar global-httpproxy "127.0.0.1:12333")
(setq url-proxy-services `(("http" . , global-httpproxy)
			   ("https" . ,global-httpproxy)
			   ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))

;; use mirror
(setq package-check-signature nil)	; to avoid signature fail for package
(setq-default package-archives '(("gnu"   . "http://elpa.zilongshanren.com/gnu/")
				 ("melpa" . "http://elpa.zilongshanren.com/melpa/")))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path)
  (push (expand-file-name "lisp" user-emacs-directory) load-path)
  (push (expand-file-name "list/init-shell" user-emacs-directory) load-path))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
	  (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)
(add-subdirs-to-load-path)

;; Initialize packages unless it's done
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; ;; Display the total loading time in the minibuffer
(defun display-startup-echo-area-message ()
  "Display startup echo area message."
  (message "Initialized in %s" (emacs-init-time)))

;; (profiler-cpu-start)
;; (profiler-memory-start)

;; requre features from lisp
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-custom-vars)
(require 'init-general-functions)
(require 'init-package-management)
(require 'init-default)
(require 'init-font)
(require 'init-ui)

(require 'init-evil)
(require 'init-corfu)
(require 'init-dired)
(require 'init-tool)
(require 'init-vertico)
(require 'init-ibuffer)
(require 'init-window)
(require 'init-layout)

(require 'init-highlight)
(require 'init-version-control)
(require 'init-project)
(require 'init-yasnippet)
(require 'init-treemacs)

(require 'init-program-basis)
(require 'init-flycheck)
(require 'init-lsp)
;; (require 'init-lsp-bridge)
(require 'init-lsp-python)
(require 'init-lsp-golang)
(require 'init-lsp-ess)
(require 'init-elisp)
(require 'init-c-c++)
(require 'init-java)
(require 'init-org)
;; (require 'init-eaf)
(require 'init-dockerfile)
(require 'init-html)
(require 'init-markdown)
(require 'init-json)
(require 'init-yaml)
(require 'init-sql)

(require 'init-shell)
(require 'init-misc)

(require 'leader-core-functions)
(require 'leader-key-binding)

(when (file-exists-p custom-file)
  (load-file custom-file))
