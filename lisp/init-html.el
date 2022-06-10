;;; init-html.el ---                                 -*- lexical-binding: t; -*-

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
;; ref: https://ithelp.ithome.com.tw/articles/10202632
;; ref: https://ithelp.ithome.com.tw/articles/10205908

;;

;;; Code:

(mark-time-here)

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.vue\\'")
  :hook
  (html-mode . web-mode)
  ;; (web-mode . electric-spacing-mode)
  (web-mode . (lambda()
		(lsp-deferred)))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-css-colorization t)
  ;; (set-face-attribute 'web-mode-html-tag-face nil :foreground "royalblue")
  ;; (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "powderblue")
  ;; (set-face-attribute 'web-mode-doctype-face nil :foreground "lightskyblue")
  (setq web-mode-content-types-alist
        '(("vue" . "\\.vue\\'")))
  (define-key web-mode-map (kbd "M-n") 'web-mode-navigate)
  (define-key web-mode-map (kbd "<tab>") 'web-mode-fold-or-unfold)
  (define-key web-mode-map (kbd "M-o") 'browse-url-of-file)
  )

(use-package emmet-mode
  :ensure t
  :hook
  (html-mode . emmet-mode)
  (web-mode . emmet-mode))

(use-package vue-mode
  :mode "\\.vue\\'"
  :hook (vue-mode . prettier-js-mode)
  :config
  (add-hook 'vue-mode-hook #'lsp)
  (setq prettier-js-args '("--parser vue")))

(provide 'init-html)
(message "init-html loaded in '%.2f' seconds" (get-time-diff time-marked))
;;; init-html.el ends here
