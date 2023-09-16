;;; init-company.el ---                              -*- lexical-binding: t; -*-

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

;;; corfu related, when not graphic use company
(when (not (display-graphic-p))
  (use-package company
    :ensure t
    :init
    (setq company-minimum-prefix-length 1)
    (setq company-idle-delay 0)
    (global-company-mode t))
  (use-package company-flx
    :ensure t
    :after (company)
    :init
    (company-flx-mode 1))
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-j") #'company-select-next)
    (define-key company-active-map (kbd "C-k") #'company-select-previous)))

(defun nasy/orderless-dispatch-flex-first (_pattern index _total)
  "orderless-flex for corfu."
  (and (eq index 0) 'orderless-flex))

(defun nasy/setup-corfu ()
  "Setup corfu."
  (setq-local orderless-matching-styles '(orderless-flex)
              orderless-style-dispatchers nil)
  (add-hook 'orderless-style-dispatchers #'nasy/orderless-dispatch-flex-first nil 'local))

;; use corfu instead
(use-package nerd-icons :ensure t)

(use-package corfu
  :ensure t
  :bind (:map corfu-map
	      ("C-M-m" . corfu-move-to-minibuffer))
  :init
  (setq corfu-cycle t
        corfu-auto t
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator
        corfu-preview-current nil
        corfu-auto-delay 0.1
        corfu-auto-prefix 1
        )
  (when (> (frame-pixel-width) 3000) (custom-set-faces '(corfu-default ((t (:height 1.3))))))
  (global-corfu-mode)
  (defun corfu-beginning-of-prompt ()
    "Move to beginning of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (car completion-in-region--data)))
  (defun corfu-end-of-prompt ()
    "Move to end of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (cadr completion-in-region--data)))
  (define-key corfu-map [remap move-beginning-of-line] #'corfu-beginning-of-prompt)
  (define-key corfu-map [remap move-end-of-line] #'corfu-end-of-prompt)
  (with-eval-after-load 'lsp-mode
    (setq lsp-completion-provider :none) ;; we use Corfu!
    (defun petmacs/lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless))) ;; Configure orderless
    (add-hook 'lsp-completion-mode-hook #'petmacs/lsp-mode-setup-completion)))

;; M-x kind-icon-preview-all to reset and preview all icons after installation
(use-package kind-icon
  :ensure t
  :quelpa (kind-icon :fetcher github
  		     :repo "jdtsmith/kind-icon"
  		     :files ("*.el"))
  :after corfu
  :init
  (require 'kind-icon)
  ;; to compute blended backgrounds correctly
  (when (icons-displayable-p)
    (setq ;; kind-icon-default-face 'corfu-default
     kind-icon-use-icons nil
     kind-icon-mapping
     `(
       (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
       (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
       (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
       (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
       (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
       (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
       (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
       (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
       (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
       (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
       (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
       (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
       (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
       (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
       (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
       (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
       (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
       (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
       (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
       (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
       (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
       (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
       (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
       (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
       (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
       (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
       (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
       (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
       (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
       (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
       (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
       (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
       (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
       (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
       (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
       (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face)))
    )
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; ;; Use dabbrev with Corfu!
(use-package dabbrev
  :ensure t
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

(use-package cape
  :ensure t
  :preface
  (defun petmacs/set-lsp-capfs ()
    (setq-local completion-at-point-functions
		(list (cape-super-capf
		       #'yasnippet-capf
		       #'lsp-completion-at-point)
		      #'cape-file
		      #'cape-dabbrev)))
  ;; :bind (("C-M-o" . cape-file))
  :hook (lsp-completion-mode . petmacs/set-lsp-capfs)
  :init (setq cape-dabbrev-min-length 2
	      cape-dabbrev-check-other-buffers nil)
  :config
  ;; 默认用这三个补全后端
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(provide 'init-corfu)
(message "init-corfu loaded in '%.2f' seconds" (get-time-diff time-marked))
;;; init-company.el ends here
