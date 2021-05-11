;;; inferior-python-mode-helper.el ---               -*- lexical-binding: t; -*-

;; Copyright (C) 2021  osmatsuda

;; Author: osmatsuda <osmatsuda@gmail.com>
;; Keywords: tools

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

(defconst inferior-python-mode-helper--mname "inferior-python-mode-helper-temp-id")
(defconst inferior-python-mode-helper--pycode "
##########################################
#include <inferior-python-mode-helper.py>#
##########################################")

(defun inferior-python-mode-helper--init ()
  (add-hook 'comint-input-filter-functions #'inferior-python-mode-helper--input-filter 0 t)
  (add-hook 'comint-preoutput-filter-functions #'inferior-python-mode-helper--preoutput-filter 0 t)
  (let ((p (python-shell-get-process)))
    (python-shell-send-string inferior-python-mode-helper--pycode p)
    (python-shell-accept-process-output p)))

(defun inferior-python-mode-helper--input-filter (input)
  (let ((read-buffer-completion-ignore-case t))
    (pcase (string-trim input
			(rx (+ (in ";" cntrl blank)))
			(rx (+ (in ";" cntrl blank))))
      ((and (pred (string-match (eval `(rx (seq bos
						,inferior-python-mode-helper--mname
						"."
						(group (or "cd" "cd_b" "pwd"))
						eos)))))
	    (app (match-string 1) cmd))
       (pcase cmd
	 ("pwd" (concat inferior-python-mode-helper--mname "._pwd()"))
	 ("cd" (concat inferior-python-mode-helper--mname
		       (format "._chdir(path='%s', cmd='cd')"
			       (expand-file-name
				(read-directory-name
				 "Change working directory: "
				 default-directory default-directory t)))))
	 ("cd_b" (concat
		  inferior-python-mode-helper--mname
		  (format "._chdir(path='%s', cmd='cd_b')"
			  (expand-file-name
			   (buffer-local-value
			    'default-directory
			    (get-buffer (read-buffer
					 "Change to Buffer’s directory: " nil t
					 #'(lambda (b)
					     (let ((bn (if (consp b) (car b) b)))
					       (and (null
						     (string-match (rx (seq bol (in " " "*")))
								   bn))
						    (not (null
							  (buffer-local-value
							   'default-directory
							   (get-buffer bn))))))))))))))))
      (_ input))))

(defun inferior-python-mode-helper--preoutput-filter (output)
  (let ((command-effect
	 (string-match (eval `(rx (seq bos
				       ,(concat "_" inferior-python-mode-helper--mname "_output_start_")
				       (group (* anything))
				       ,(concat "_" inferior-python-mode-helper--mname "_output_end_")
				       eos)))
		       output)))
    (if command-effect
	(inferior-python-mode-helper--preoutput-effects
	 (with-temp-buffer (insert (match-string 1 output))
			   (goto-char (point-max))
			   (eval-last-sexp t)))
      output)))

(defun inferior-python-mode-helper--preoutput-effects (pl)
  (let ((cmd (plist-get pl 'command))
	(data (plist-get pl 'data))
	(result (plist-get pl 'result)))
    (pcase cmd
      ((rx (seq bol (or "cd" "cd_b" "pwd") eol))
       (cd-absolute data)))
    result))

(provide 'inferior-python-mode-helper)
;;; inferior-python-mode-helper.el ends here
