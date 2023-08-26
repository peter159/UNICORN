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

(defun unicorn/run-ts-file ()
  "Compile and run the current TypeScript file."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (base-name (file-name-sans-extension file-name))
         (js-file-name (concat base-name ".js")))
    (progn
      (message "Compiling %s to %s..." file-name js-file-name)
      (call-process "tsc" nil nil nil file-name))
    (message "Running %s..." js-file-name)
    (async-shell-command (concat "node " js-file-name))))

(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-mode)
  :config
  (setq typescript-indent-level 2)
  (define-key typescript-mode-map (kbd "C-c C-b") 'unicorn/run-ts-file)
  )

;; 确保 use-package 包已经安装
;; 如果没有安装，你可以通过以下命令进行安装
;; M-x package-install RET use-package RET

(require 'use-package)

;; 安装和配置 tide 包，它是一个 TypeScript 交互式开发环境
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; 安装和配置 web-mode 包，用于支持 TSX
(use-package web-mode
  :ensure t
  :mode (("\\.tsx\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-quoting nil) ;; 禁止自动插入引号
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode)))))

;; 定义一个辅助函数，用于在正确的环境中启动 tide
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  (company-mode +1))

(provide 'init-ts)
(message "init-ts loaded in '%.2f' seconds" (get-time-diff time-marked))
;;; init-html.el ends here
