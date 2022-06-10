;;; init-org.el ---                                  -*- lexical-binding: t; -*-

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

;; sudo apt install python3-pip
;; /usr/bin/python3 -m pip install pip setuptools --upgrade
;; python3 install install-eaf.py
(use-package eaf
  :load-path (lambda () (expand-file-name "site-lisp/emacs-application-framework" user-emacs-directory))
  :init
  (setq eaf-python-command "/home/linyi/miniconda3/envs/eaf/bin/python")
  (let ((default-directory (expand-file-name "site-lisp/emacs-application-framework/app" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path))
  (require 'eaf)
  (require 'eaf-all-the-icons)
  (require 'eaf-browser)
  (require 'eaf-markdown-previewer)
  (require 'eaf-evil)
  )

(provide 'init-eaf)
(message "init-eaf loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-org.el ends here
