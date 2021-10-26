;;; make.el ---                                      -*- lexical-binding: t; -*-
;; 
;;

(let* (;; Configures:
       ;; 
       ;; id: top-level name of the helper command
       (id "_cmd")

       ;; ins-dir: install directory
       ;;   When this value is nil, you should copy a built file (
       ;;   build/inferior-python-mode-helper.el or build/inferior-python-mode-helper.elc)
       ;;   to your library directory.
       (ins-dir nil)

       ;;
       ;;
       (tmp-id "inferior-python-mode-helper-temp-id")
       (src-el "inferior-python-mode-helper.el")
       (src-py-buf (find-file-noselect "inferior-python-mode-helper.py"))
       (build-dir (pcase "build"
		    ((and dir (pred file-directory-p)) dir)
		    (dir (make-directory dir) dir)))
       (target (concat default-directory build-dir "/" src-el))
       target-buf)
  (when (file-exists-p target)
    (delete-file target))
  (copy-file src-el target)
  (setq target-buf (find-file-noselect target))
  (with-current-buffer target-buf

    ;; replace ‘tmp-id’ to ‘id’
    (goto-char (point-min))
    (while (re-search-forward (eval `(rx (seq symbol-start
					      ,tmp-id
					      symbol-end)))
			      nil t)
      (delete-char (- (match-beginning 0) (match-end 0)))
      (insert id))

    ;; insert python-source
    (goto-char (point-min))
    (let (box-start
	  width
	  box-end)

      ;; search target-box
      (when (re-search-forward (rx (seq bol "#####" (+ "#") eol)) nil t)
	(setq box-start (match-beginning 0) box-end (match-end 0))
	(setq width (- box-end box-start))
	(forward-line)
	(while (and (= ?# (char-after))
		    (goto-char (+ (point) width))
		    (= ?# (char-before)))
	  (setq box-end (point))
	  (unless (string-match (rx (seq bol (+ "#") eol))
				(buffer-substring (- (point) width) (point)))
	    (forward-line)))
	(when (and (>= (- box-end box-start) (* 3 width))
		   (re-search-backward (rx (seq "include" (* " ")
						"<" (group (+ nonl)) ">"))
				       box-start t))
	  (when (string= (match-string-no-properties 1)
			 (file-name-nondirectory (buffer-file-name src-py-buf)))
	    (goto-char box-start)
	    (delete-char (- box-end box-start))

	    ;; get lines from src-py
	    (with-current-buffer src-py-buf
	      (beginning-of-buffer)
	      (push-mark)
	      (end-of-line)
	      (while (< (point) (point-max))
		(let ((line (buffer-substring-no-properties (region-beginning) (region-end))))
		  (when (> (length line) 0)
		    (when (string-match (eval `(rx (seq symbol-start
						       ,tmp-id
						       symbol-end)))
					line)
		      (setq line (replace-match id t t line)))
		    ;; paste lines to target
		    (with-current-buffer target-buf
		      (insert (format "%S" line))
		      (delete-char -1)
		      (push-mark)
		      (beginning-of-line)
		      (delete-char 1)
		      (goto-char (region-end))
		      (insert "\n"))))
		(forward-line)
		(push-mark)
		(end-of-line))))
	  (delete-char -1))))

    ;; final process
    (save-buffer)
    ;(mapc #'kill-buffer (list src-py-buf target-buf (current-buffer)))
    (byte-compile-file target)
    (when ins-dir
      (unless (directory-name-p ins-dir)
	(setq ins-dir (concat ins-dir "/")))
      (unless (file-directory-p ins-dir)
	(make-directory ins-dir t))
      (mapc #'(lambda (file)
		(copy-file file ins-dir 1 t))
	    (list target (concat target "c"))))))
