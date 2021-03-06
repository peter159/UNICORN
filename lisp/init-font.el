;;; init-font.el ---                                 -*- lexical-binding: t; -*-

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

(setq-local unicorn-font-size 11)
;; setup english word font and size
(set-face-attribute 'default nil :font (format "JetBrains Mono-%S" unicorn-font-size)) ; Fira Code Retina-%S; Fira Code Retina-%S
(set-face-attribute 'fixed-pitch-serif nil :family "Iosevka")
;; (set-face-attribute 'default nil :font (format "Fira Code Retina-%S" 12)) ; Fira Code Retina-%S; Fira Code Retina-%S
(setq-default line-spacing 0.2)

;; fix the delay when showing text in chinese
(dolist (charset '(kana han cjk-misc bopomofo))
  (if (display-graphic-p)		;to avoid error 'fontset tty' in linux shell environment
      (set-fontset-font (frame-parameter nil 'font) charset
			;; (font-spec :family "Microsoft Yahei" :size 12))
			(font-spec :family "等距更纱黑体 SC" :size unicorn-font-size)))
  )

;; (use-package fontify-face :ensure t)

;; add pyim support,ref:http://www.mianquan.net/tutorial/pyim/setting.md
(use-package pyim
  :ensure t
  :demand t
  :config
  ;; 激活 basedict 拼音词库
  (use-package pyim-basedict
    :ensure t
    :config (pyim-basedict-enable))
  (setq default-input-method "pyim")
  ;; 我使用全拼
  (setq pyim-default-scheme 'quanpin)
  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是:
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)
  ;; 使用 pupup-el 来绘制选词框, 如果用 emacs26, 建议设置
  ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
  ;; 手动安装 posframe 包。
  (use-package posframe :ensure t)
  (setq pyim-page-tooltip 'posframe)
  ;; 选词框显示5个候选词
  (setq pyim-page-length 20)
  ;; 让 Emacs 启动时自动加载 pyim 
  (add-hook 'emacs-startup-hook
            #'(lambda () (pyim-restart-1 t)))

  :bind
  (("M-j" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
   ("C-;" . pyim-delete-word-from-personal-buffer)))

(provide 'init-font)
(message "init-font loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-font.el ends here
