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

;;; corfu related
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
(when (display-graphic-p)
  (use-package all-the-icons :ensure t :if (display-graphic-p))

  ;; (use-package corfu
  ;;   :ensure t
  ;;   :init
  ;;   (setq corfu-cycle t
  ;;         corfu-auto t
  ;;         corfu-quit-at-boundary t
  ;;         corfu-quit-no-match t
  ;;         corfu-preview-current nil
  ;; 	  corfu-echo-documentation nil
  ;;         corfu-preselect-first t
  ;;         corfu-auto-delay 0.1
  ;;         corfu-auto-prefix 1)
  ;;   (global-corfu-mode))
  (use-package corfu
    :bind (:map corfu-map
		("C-M-m" . corfu-move-to-minibuffer))
    ;; :hook (corfu-mode . corfu-indexed-mode)
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

  (use-package kind-all-the-icons
    :ensure nil				;already in site-list
    :init
    ;; (require 'all-the-icons)
    (require 'kind-all-the-icons)
    (add-to-list 'corfu-margin-formatters #'kind-all-the-icons-margin-formatter))

  ;; ;; Use dabbrev with Corfu!
  (use-package dabbrev
    ;; Swap M-/ and C-M-/
    :ensure t
    :bind (("M-/" . dabbrev-completion)
           ("C-M-/" . dabbrev-expand)))

  (use-package cape
    :ensure t
    :bind (("C-M-o"   . cape-file))
    :init
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)))

(provide 'init-corfu)
(message "init-corfu loaded in '%.2f' seconds" (get-time-diff time-marked))
;;; init-company.el ends here
