;;; inferior-python-mode-helper.el ---               -*- lexical-binding: t; -*-

;; Copyright (C) 2022  osmatsuda

;; Author: osmatsuda <osmatsuda@gmail.com>
;; Keywords: tools, python

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

;; Utilities for inferior-python-mode
;; See https://github.com/osmatsuda/inferior-python-mode-helper

;;; Code:
(require 'comint)
(require 'cl-macs)
(require 'python)


(defconst inferior-python-mode-helper--mname
  "#######################################
   #                                     #
   # inferior-python-mode-helper-temp-id #
   #                                     #
   #######################################")

(defconst inferior-python-mode-helper--pycode "
###############################################
#                                             #
#'((include . inferior-python-mode-helper.py))#
#                                             #
###############################################")


(defvar-local inferior-python-mode-helper--default-sender nil)
(defvar-local inferior-python-mode-helper--cleanup-tmps nil)

(defun inferior-python-mode-helper ()
  (add-hook 'comint-input-filter-functions #'inferior-python-mode-helper--input-filter 0 t)
  (add-hook 'comint-preoutput-filter-functions #'inferior-python-mode-helper--preoutput-filter 0 t)

  (let ((p (python-shell-get-process)))
    (python-shell-send-string inferior-python-mode-helper--pycode p)
    (python-shell-accept-process-output p)))


(defconst inferior-python-mode-helper--rx-single
  (eval `(rx (seq bos
                  (* (in ";" cntrl blank))
                  ,inferior-python-mode-helper--mname
                  "."
                  (group (or "cd" "cd_b" "pwd"))
                  eos))))

(defconst inferior-python-mode-helper--rx-expr
  (eval `(rx (seq symbol-start
                  ,inferior-python-mode-helper--mname
                  "."
                  (group (or (seq (or "eval_expr"
				      "open_b"
				      "open_f")
				  word-end)
			     (or "write_b(")))))))


(defun inferior-python-mode-helper--input-filter (input)
  (let ((read-buffer-completion-ignore-case t))
    (pcase (string-trim-right input (rx (+ (in ";" cntrl blank))))
      
      ;; case for the single-use commands
      ((and (pred (string-match inferior-python-mode-helper--rx-single))
            (app (match-string 1) cmd))
       (setq inferior-python-mode-helper--default-sender comint-input-sender)
       (let ((send-str
              (pcase cmd
                ("pwd" (concat inferior-python-mode-helper--mname "._pwd()\n"))
                ("cd" (concat inferior-python-mode-helper--mname
                              (format "._chdir(path='%s', cmd='cd')\n"
                                      (expand-file-name
                                       (read-directory-name
                                        "Change working directory: "
                                        default-directory default-directory t)))))
                ("cd_b" (concat
                         inferior-python-mode-helper--mname
                         (format "._chdir(path='%s', cmd='cd_b')\n"
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
                                                                  (get-buffer bn)))))))))))))))))
         (setq comint-input-sender (inferior-python-mode-helper--sender-fn send-str))))
      
      ;; case for expressions
      ((and (pred (string-match inferior-python-mode-helper--rx-expr))
            input)
       (when (null inferior-python-mode-helper--default-sender)
         (setq inferior-python-mode-helper--default-sender comint-input-sender))

       (let ((send-str
              (catch 'inferior-python-mode-helper--except-expand
                (let ((start (match-end 0))
                      (s-str (concat (substring-no-properties input 0 (match-beginning 0))
                                     (inferior-python-mode-helper--expand-cmd (match-string 1 input)))))
                  (while (string-match inferior-python-mode-helper--rx-expr input start)
		    (let ((memo-start (match-end 0)))
		      (setq s-str (concat s-str
					  (substring-no-properties input start (match-beginning 0))
					  (inferior-python-mode-helper--expand-cmd (match-string 1 input))))
		      (setq start memo-start)))
                  (setq s-str (concat s-str
                                      (substring-no-properties input start)
                                      "\n"))
                  s-str))))
         (setq comint-input-sender (inferior-python-mode-helper--sender-fn send-str))))
      
      (_ input))))


(defun inferior-python-mode-helper--sender-fn (send-str)
  (if (< (string-bytes send-str) 1024)
      #'(lambda (proc _)
          (comint-send-string proc send-str))
    (let ((tmp-fname (python-shell--save-temp-file send-str)))
      #'(lambda (proc _)
          (comint-send-string
	   proc
           (format "__PYTHON_EL_eval_file('','%s',True)\n" tmp-fname))))))


(defun inferior-python-mode-helper--expand-cmd (cmd)
  (pcase cmd
    ("eval_expr"
     (inferior-python-mode-helper--expand-eval_expr))
    ("open_b"
     (inferior-python-mode-helper--expand-open_b))
    ("open_f"
     (inferior-python-mode-helper--expand-open_f))
    ("write_b("
     (inferior-python-mode-helper--expand-write_b))
    (_ (throw 'inferior-python-mode-helper--except-expand
              "raise Exception(\"inferior-python-mode-helper-unknown\")\n"))))


(defun inferior-python-mode-helper--expand-write_b ()
  (concat inferior-python-mode-helper--mname
          ".write_b(buffername=\""
          (read-buffer "Destination Buffer: " nil t
                           #'(lambda (b)
                               (let ((bn (if (consp b)
					     (car b)
					   b)))
                                 (and (not (string-prefix-p " " bn))
                                      (not (string= (buffer-name) bn))
                                      (not (buffer-local-value 'buffer-read-only (get-buffer bn)))))))
          "\", value="))


(defun inferior-python-mode-helper--expand-open_f ()
  (let ((filename (read-file-name "Find file: " nil nil t nil
                                  #'(lambda (fn)
                                      (if (or (string-prefix-p "#" fn)
                                              (file-symlink-p fn))
                                          nil
					fn))))
        (mode (completing-read "Mode (default r): "
                               '("r" "rb" "w" "wb" "a" "ab") nil t nil nil "r")))
    (when (file-directory-p filename)
      (throw 'inferior-python-mode-helper--except-expand
             "raise Exception(\"Selected a directory\")\n"))
    (when (and (string-prefix-p "w" mode)
               (file-exists-p filename))
      (let ((yn ""))
        (while (not (or (string= yn "Y")
			(string= yn "n")))
          (setq yn (read-from-minibuffer
                    (concat filename "exists. Mode " mode " will truncate the file. Continue? [Y/n]: "))))
        (when (string= yn "n")
          (throw 'inferior-python-mode-helper--except-expand
                 "None\n"))))
    (concat "open(\"" (expand-file-name filename) "\", \"" mode "\")")))


(defun inferior-python-mode-helper--expand-open_b ()
  (let* ((contents (with-current-buffer
                       (read-buffer "Source Buffer: " nil t
				    #'(lambda (b)
					(let ((bn (if (consp b)
						      (car b)
						    b)))
					  (and (not (string-prefix-p " " bn))
                                               (not (eq (buffer-local-value 'major-mode (get-buffer bn))
							'dired-mode))))))
                     (buffer-substring-no-properties (point-min) (point-max))))
         (tmp-fname (when (>= (string-bytes contents) 1024)
                      (python-shell--save-temp-file contents)))
         (send-str (when (null tmp-fname)
                     (let ((iostrio (concat "io.StringIO("
                                            (string-join (split-string (format "%S" contents)"\n")
							 "\\n")
                                            ")")))
                       (when (< (string-bytes iostrio) 1024)
                         iostrio)))))
    (or send-str
        (concat inferior-python-mode-helper--mname
                "."
                (format "_open_tmp(%S)"
			(or tmp-fname (python-shell--save-temp-file contents)))))))


(defun inferior-python-mode-helper--expand-eval_expr ()
  (cl-labels
      ((pair (item)
             (concat (if (stringp (car item))
                         (format "%S" (car item))
                       (let ((stred (str (car item))))
                         (if (string-prefix-p "\"" stred)
                             stred
                           (format "%S" stred))))
                     ":"
                     (str (cdr item))))
       (pairs-c (items)
                (unless (null items)
                  (cons (pair (car items))
                        (pairs-c (cdr items)))))
       (pairs-v (items)
                (unless (= (length items) 0)
                  (cons (pair (aref items 0))
                        (pairs-v (seq-drop items 1)))))
       (pairs (items)
              (if (vectorp items)
                  (pairs-v items)
                (pairs-c items)))

       (array-c (items)
                (unless (null items)
                  (cons (if (stringp (car items))
                            (format "%S" (car items))
                          (str (car items)))
                        (array-c (cdr items)))))
       (array-v (items)
                (unless (= (length items) 0)
                  (cons (let ((head (aref items 0)))
                          (if (stringp head)
                              (format "%S" head)
                            (str head)))
                        (array-v (seq-drop items 1)))))
       (array (items)
              (if (vectorp items)
                  (array-v items)
                (array-c items)))       
       
       (kmp-mnu-itm-details (items)
                            (cond
                             ((null items) nil)
                             ((and (consp (car items))
                                   (not (keymapp (car items))))
                              (concat (str (format "%S" (car items)))
                                      (unless (null (cdr items)) ",")
                                      (kmp-mnu-itm-details (cdr items))))
                             (t
                              (concat (str (car items))
                                      (unless (null (cdr items)) ",")
                                      (kmp-mnu-itm-details (cdr items))))))
       (key-repr (&rest chars)
                 (key-description `[,@chars]))
       (keymapping (item)
                   (pcase item
                     ((and `(,type menu-item . ,details)
                           (guard (not (consp type))))
                      (concat (format "%S:" (key-repr `,type 'menu-item))
                              "[" (kmp-mnu-itm-details `,details) "]"))
                     ((and `(,type ,name ,help . ,binding)
                           (guard (and (not (consp type))
                                       (stringp name)
                                       (stringp help)
                                       (or (symbolp binding) (keymapp binding)))))
                      (concat (format "%S:" (key-repr `,type `,name (concat "(" `,help ")")))
                              (str `,binding)))
                     ((and `(,type ,name . ,binding)
                           (guard (and (not (consp type))
                                       (stringp name)
                                       (or (symbolp binding) (keymapp binding)))))
                      (concat (format "%S:" (key-repr `,type `,name))
                              (str `,binding)))
                     ((and `(,type . ,binding)
                           (guard (and (not (consp type))
                                       (or (symbolp binding) (keymapp binding)))))
                      (concat (format "%S:" (key-repr `,type))
                              (str `,binding)))
                     ((pred stringp)
                      (format "%S:%S" "prompt" item))
                     ((pred char-table-p)
                      (concat (format "%S:{" "char-table")
                              (mapconcat #'identity (chr-tbl-pairs item) ",")
                              "}"))
                     ((pred vectorp)
                      (concat (format "%S:[" "char-table-vector")
                              (mapconcat #'char-to-string item ",")
                              "]"))))
       (keymappings (items)
                    (unless (null items)
                      (concat (keymapping (car items))
                              (unless (null (cdr items)) ",")
                              (keymappings (cdr items)))))

       (chr-tbl-key-repr (key)
                         (if (consp key)
                             (format "%d .. %d" (car key) (cdr key))
                           (format "%d" key)))
       (chr-tbl-pairs (table)
                      (let (stock)
                        (map-char-table #'(lambda (k v)
                                            (setq stock
                                                  (cons (format "%S:%s"
                                                                (chr-tbl-key-repr k)
                                                                (str v))
                                                        stock)))
                                        table)
                        (reverse stock)))

       (cons-items (items)
                   (let ((car-itm (car items))
                         (cdr-itm (cdr items)))
                     (cons (str car-itm)
                           (cond
                            ((null cdr-itm) nil)
                            ((not (consp cdr-itm))
                             (cons (str cdr-itm) nil))
                            (t (cons-items cdr-itm))))))

       (record-items (stock rec index+1)
		     (if (= index+1 0)
			 stock
		       (setq stock (cons (str (aref rec (1- index+1))) stock))
		       (record-items stock rec (1- index+1))))
       
       (str (expr &optional top-level)
            (cond ((booleanp expr)
                   (if top-level
                       (if expr "True" "False")
                     (if expr "True" "None")))
                  ((or (proper-list-p expr)
                       (vectorp expr))
                   (cond
                    ((keymapp expr)
                     (concat (format "{%S:{" "keymap")
                             (keymappings (cdr expr))
                             "}}"))
                    ((cl-every #'consp expr)
                     (concat "{"
                             (mapconcat #'identity (pairs expr) ",")
                             "}"))
                    (t
                     (concat "["
                             (mapconcat #'identity (array expr) ",")
                             "]"))))
                  ((and (consp expr) (listp expr))
                   (concat "["
                           (mapconcat #'identity (cons-items expr) ",")
                           "]"))
		  ((recordp expr)
		   (concat "["
			   (mapconcat #'identity (record-items nil expr (length expr)) ",")
			   "]"))
                  ((hash-table-p expr)
                   (concat "{"
                           (mapconcat #'identity
                                      (reverse
                                       (cl-reduce #'(lambda (stock k)
                                                      (cons (pair (cons k (gethash k expr)))
                                                            stock))
                                                  (hash-table-keys expr)
                                                  :initial-value nil))
                                      ",")
                           "}"))
		  ((char-table-p expr)
                   (concat "{"
                           (mapconcat #'identity (chr-tbl-pairs expr) ",")
                           "}"))
                  ((bool-vector-p expr)
                   (format "%S"
                           (cl-reduce #'(lambda (stock bool)
                                          (concat stock (if bool "1" "0")))
                                      expr :initial-value "")))
                  ((symbolp expr)
                   (format "%S" (symbol-name expr)))
                  ((stringp expr)
                   (format "%S" (substring-no-properties expr)))
                  (t
                   (format "%S" expr)))))
    (condition-case err
        (str (eval-expression (read--expression "Eval: ")) t)
      (error (format "raise Exception('%S')" err)))))


(defconst inferior-python-mode-helper--rx-cmdoutput
  (eval `(rx (seq symbol-start
                  ,(concat "_" inferior-python-mode-helper--mname "_output_beg_")
                  (group (* anything))
		  ,(concat "_" inferior-python-mode-helper--mname "_output_end_")
		  "\n"
		  (group (regexp python-shell-prompt-regexp))))))

(defconst inferior-python-mode-helper--rx-cmdoutput-start
  (eval `(rx (seq symbol-start
		  ,(concat "_" inferior-python-mode-helper--mname "_output_beg_'(")))))

(defconst inferior-python-mode-helper--rx-cmdoutput-end
  (eval `(rx (seq (group (* anything))
		  ,(concat "_" inferior-python-mode-helper--mname "_output_end_")
		  "\n"
		  (group (regexp python-shell-prompt-regexp))))))

(defvar-local inferior-python-mode-helper--cmdoutput-end-sample nil)


(defun inferior-python-mode-helper--preoutput-filter (output)
  (unless (null inferior-python-mode-helper--default-sender)
    (setq comint-input-sender inferior-python-mode-helper--default-sender)
    (setq inferior-python-mode-helper--default-sender nil))

  (let ((sample-length (length inferior-python-mode-helper--rx-cmdoutput-end))
	(tmp-buffer-name (concat " *infpm preoutput <" (buffer-name) ">*"))
	prefix
	body
	suffix)
    (cond
     ;; case: _output_beg_..._output_end_ >>>
     ((string-match inferior-python-mode-helper--rx-cmdoutput output)

      (with-current-buffer (get-buffer-create tmp-buffer-name)
	(insert (match-string 1 output)))
      (setq prefix (substring output 0 (match-beginning 0))
	    suffix (match-string 2 output)
	    body (inferior-python-mode-helper--side-effects tmp-buffer-name))
      (kill-buffer tmp-buffer-name))

     ;; case: _output_beg_...
     ((string-match inferior-python-mode-helper--rx-cmdoutput-start output)
      
      (let ((prefix-pair (format "'(prefix %S "
				 (string-to-vector
				  (substring output 0 (match-beginning 0)))))
	    (body-part (substring output (match-end 0))))
	(with-current-buffer (get-buffer-create tmp-buffer-name)
	  (insert-before-markers prefix-pair body-part))
	(setq inferior-python-mode-helper--cmdoutput-end-sample
	      (substring body-part (- sample-length)))))

     ;; case: ..._output_end_ >>>
     ((and inferior-python-mode-helper--cmdoutput-end-sample
	   (string-match inferior-python-mode-helper--rx-cmdoutput-end output))
      
      (with-current-buffer tmp-buffer-name
	(insert-before-markers output))
      (setq inferior-python-mode-helper--cmdoutput-end-sample nil
	    prefix (inferior-python-mode-helper--preoutput-prefix tmp-buffer-name)
	    suffix (match-string 2 output)
	    body (inferior-python-mode-helper--side-effects tmp-buffer-name))
      (kill-buffer tmp-buffer-name))
     
     ;; case: inside of _output_beg_..._output_end_
     ((and inferior-python-mode-helper--cmdoutput-end-sample
	   (> (length output) sample-length))
      
      (with-current-buffer tmp-buffer-name
	(insert-before-markers output))
      (setq inferior-python-mode-helper--cmdoutput-end-sample
	    (substring output (- sample-length))))

     ;; case: flushed the rest of the end tag
     ((and inferior-python-mode-helper--cmdoutput-end-sample
	   (string-match inferior-python-mode-helper--rx-cmdoutput-end
			 (concat inferior-python-mode-helper--cmdoutput-end-sample
				 output)))

      (with-current-buffer tmp-buffer-name
	(insert-before-markers output))
      (let ((searched (concat inferior-python-mode-helper--cmdoutput-end-sample
			      output)))
	(setq inferior-python-mode-helper--cmdoutput-end-sample nil
	      prefix (inferior-python-mode-helper--preoutput-prefix tmp-buffer-name)
	      suffix (match-string 2 searched)
	      body (inferior-python-mode-helper--side-effects tmp-buffer-name)))
      (kill-buffer tmp-buffer-name)))
    
    (if (or prefix body suffix)
	(concat prefix body suffix)
      (if inferior-python-mode-helper--cmdoutput-end-sample
	  ""
	output))))


(defun inferior-python-mode-helper--side-effects (buffername)
  (let (outputs)
    (with-current-buffer buffername
      (goto-char (point-min))
      (forward-sexp)
      (setq outputs (cons (inferior-python-mode-helper--side-effects-main (eval-last-sexp t))
			  outputs))
      (while (re-search-forward inferior-python-mode-helper--rx-cmdoutput-start nil t)
	(backward-char 2)
	(forward-sexp)
	(setq outputs (cons (inferior-python-mode-helper--side-effects-main (eval-last-sexp t))
			    outputs))))
    (cl-reduce #'(lambda (accm str)
		   (if (= (length str) 0)
		       accm
		     (concat str accm)))
	       outputs :initial-value "")))


(defun inferior-python-mode-helper--side-effects-main (plst)
  (let ((cmd (plist-get plst 'command))
        (data (plist-get plst 'data))
        (result (plist-get plst 'result)))
    (setq result
          (if (arrayp result)
              (concat result)
            (unless (null result)
              (format "%S" result))))
    (cond
     ((member cmd '("cd" "cd_b" "pwd"))
      (unless (null result)
	(setq result (concat result "\n")))
      (cd-absolute data))
     
     ((string-match "^write_b\\[\\([ 0-9]*\\)]$" cmd)
      (if (get-buffer data)
	  (with-current-buffer data
	    (insert-before-markers
	     result
	     (unless (null result)
	       (concat (mapcar #'string-to-number (split-string (match-string 1 cmd))))))
	    (setq result nil))
	(setq result (concat "### No buffer named " data)))))
    
    (if (null result)
        ""
      result)))


(defun inferior-python-mode-helper--preoutput-prefix (buffername)
  (with-current-buffer buffername
    (let (plst
	  eval-point)
      (goto-char (point-min))
      (forward-sexp)
      (setq eval-point (point)
	    plst (eval-last-sexp t))
      (delete-region eval-point (point))
      (plist-get plst 'prefix))))


(provide 'inferior-python-mode-helper)
;;; inferior-python-mode-helper.el ends here
