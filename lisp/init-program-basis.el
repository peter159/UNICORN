;;; init-program-basis.el ---                        -*- lexical-binding: t; -*-

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

(mark-time-here)

(use-package prettify-utils
  :ensure t
  :quelpa
  (prettify-utils :repo "Ilazki/prettify-utils.el"
		  :fetcher github))

;; (use-package pretty-code
;;   :ensure nil
;;   :init
;;   (require 'pretty-code)
;;   (pretty-code-add-hook 'python-mode-hook     '((:def "def")
;;     					        (:lambda "lambda")))
;;   (pretty-code-add-hook 'emacs-lisp-mode-hook '((:def "defun")
;; 						(:lambda "lambda"))))

(use-package electric-operator
  :ensure t
  :hook ((c-mode-common . electric-operator-mode)
         (python-mode . electric-operator-mode)
	 (go-mode . electric-operator-mode)
	 ((ess-r-mode inferior-ess-r-mode-hook ess-r-package-mode) . electric-operator-mode)
         (electric-operator-mode . (lambda ()
                                     (electric-operator-add-rules-for-mode 'c++-mode
                                                                           (cons "*" nil)
                                                                           (cons "&" nil))
                                     (electric-operator-add-rules-for-mode 'c-mode
                                                                           (cons "*" nil))
				     (electric-operator-add-rules-for-mode 'go-mode
									   (cons ":=" " := "))
				     (electric-operator-add-rules-for-mode 'ess-mode
									   (cons "=" " = "))
				     ))))


(provide 'init-program-basis)
(message "init-program-basis loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-program-basis.el ends here
