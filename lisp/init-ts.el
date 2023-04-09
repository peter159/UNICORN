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

(provide 'init-ts)
(message "init-ts loaded in '%.2f' seconds" (get-time-diff time-marked))
;;; init-html.el ends here
