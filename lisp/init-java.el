;;; init-java.el ---                                 -*- lexical-binding: t; -*-

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

(use-package lsp-java
  :hook (java-mode . (lambda ()
		       (require 'lsp-java)
		       (lsp-deferred)))
  :init
  (setq lsp-java-import-maven-enabled t
	lsp-java-implementations-code-lens-enabled t
	lsp-java-save-actions-organize-imports t
	;; latest jdtls requires java >= 11 to work
	lsp-java-java-path "/opt/jdk11/bin/java"
	lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx6G" "-Xms100m")
	;; Runtime name must be one of: “J2SE-1.5”, “JavaSE-1.6”, “JavaSE-1.7”, “JavaSE-1.8” etc
	;; lsp-java-configuration-runtimes '[(:name "JavaSE-1.8"
	;; 				   :path "/opt/jdk/")
	;; 				  (:name "JavaSE-11"
	;; 				   :path "/opt/jdk11/"
	;; :default t)]
	lsp-java-folding-range-enabled t)
  )

(use-package mvn
  :preface
  (defun petmacs/mvn-clean-compile ()
    "Recompile using maven."
    (interactive)
    (mvn-clean)
    (mvn-compile)))

(use-package maven-test-mode)

(provide 'init-java)
(message "init-java loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-java.el ends here
