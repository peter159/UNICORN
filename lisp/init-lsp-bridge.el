;;; init-lsp.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  <peter.linyi@DESKTOP-PMTGUNT>
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

(mark-time-here)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(add-to-list 'load-path "<path-to-lsp-bridge>")

(require 'lsp-bridge)             ;; load lsp-bridge
(global-corfu-mode)               ;; use corfu as completion ui

(require 'lsp-bridge-orderless)   ;; make lsp-bridge support fuzzy match, optional
(require 'lsp-bridge-icon)        ;; show icon for completion items, optional

;; Enable auto completion in elisp mode.
(dolist (hook (list
               'emacs-lisp-mode-hook
               ))
  (add-hook hook (lambda ()
                   (setq-local corfu-auto t)
                   )))

;; Enable lsp-bridge.
(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'python-mode-hook
               'ruby-mode-hook
               'lua-mode-hook
               'rust-mode-hook
               'elixir-mode-hook
               'go-mode-hook
               'haskell-mode-hook
               'haskell-literate-mode-hook
               'dart-mode-hook
               'scala-mode-hook
               'typescript-mode-hook
               'typescript-tsx-mode-hook
               'js2-mode-hook
               'js-mode-hook
               'rjsx-mode-hook
               'tuareg-mode-hook
               'latex-mode-hook
               'Tex-latex-mode-hook
               'texmode-hook
               'context-mode-hook
               'texinfo-mode-hook
               'bibtex-mode-hook
               'clojure-mode-hook
               'clojurec-mode-hook
               'clojurescript-mode-hook
               'clojurex-mode-hook
               'sh-mode-hook
               'web-mode-hook
               ))
  (add-hook hook (lambda ()
                   (setq-local corfu-auto nil)  ;; let lsp-bridge control when popup completion frame
                   (lsp-bridge-mode 1)
                   )))

(use-package format-all
  :ensure t
  :hook
  (lsp-mode . format-all-mode)
  (gfm-mode . format-all-mode)
  ;; (format-all-mode . format-all-ensure-formatter)
  :config
  (global-set-key (kbd "M-f") 'format-all-buffer))

;; optionally if you want to use debugger
(use-package dap-mode
  :ensure t)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(provide 'init-lsp)
(message "init-lsp loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-lsp.el ends here
