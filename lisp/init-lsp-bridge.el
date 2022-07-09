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

(use-package dumb-jump)

;;; pip install epc
(use-package lsp-bridge
  :quelpa (lsp-bridge :fetcher github
  		              :repo "manateelazycat/lsp-bridge"
  		              :files ("*"))
  :preface
  ;; 融合 `lsp-bridge' `find-function' 以及 `dumb-jump' 的智能跳转
  (defun unicorn/lsp-bridge-jump ()
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (evil-goto-definition))
     ((eq major-mode 'org-mode)
      (org-agenda-open-link))
     (lsp-bridge-mode
      (lsp-bridge-find-def))
     (t
      (require 'dumb-jump)
      (dumb-jump-go))))

  (defun unicorn/lsp-bridge-jump-back ()
    (interactive)
    (cond
     ((member major-mode unicorn-lsp-active-modes)
      (lsp-bridge-return-from-def))
     (t
      (require 'dumb-jump)
      (dumb-jump-back))))

  :init
  (require 'lsp-bridge)
  ;; (setq lsp-bridge-python-command (expand-file-name "lsp-bridge/bin/python" (getenv "WORKON_HOME")))
  :config
  ;; don't show lsp-bridge-info in modeline
  (setq mode-line-misc-info (delete '(lsp-bridge-mode (" [" lsp-bridge--mode-line-format "] "))
                                    mode-line-misc-info))

  ;; (setq-local evil-goto-definition-functions '(lsp-bridge-jump))

  (define-key evil-motion-state-map (kbd "C-o") #'unicorn/lsp-bridge-jump-back)
  (define-key evil-motion-state-map "gR" #'lsp-bridge-rename)
  (define-key evil-motion-state-map "gr" #'lsp-bridge-find-references)
  (define-key evil-normal-state-map "gi" #'lsp-bridge-find-impl)
  (define-key evil-motion-state-map "gd" #'unicorn/lsp-bridge-jump)
  (define-key evil-motion-state-map "gs" #'lsp-bridge-restart-process)
  (define-key evil-normal-state-map "gh" #'lsp-bridge-lookup-documentation)

  (evil-add-command-properties #'lsp-bridge-jump :jump t)

  (evil-define-key 'normal lsp-bridge-ref-mode-map
    (kbd "RET") 'lsp-bridge-ref-open-file-and-stay
    "q" 'lsp-bridge-ref-quit)

  (global-lsp-bridge-mode))

(provide 'init-lsp-bridge)
(message "init-lsp-bridge loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-lsp.el ends here
