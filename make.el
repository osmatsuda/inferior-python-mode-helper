;;; make.el ---                                      -*- lexical-binding: t; -*-
;; 
;;

(let* (;; Configures:
       ;;
       ;;
       ;; Default module name of the helper command
       (module-name "_el")

       ;; 
       ;;
       ;;
       ;; 
       (tmp-id "inferior-python-mode-helper-temp-id")
       (src-el "inferior-python-mode-helper.el")
       (src-py "inferior-python-mode-helper.py")
       (src-py-buf (find-file-noselect src-py))
       (target (concat
		default-directory
		(pcase "build"
		  ((and dir (pred file-directory-p)) dir)
		  (dir (make-directory dir) dir))
		"/" src-el))
       target-buf)
  (let ((_module-name (read-from-minibuffer
		       (concat "Set a module name [default: " module-name "]: "))))
    (unless (string-empty-p _module-name)
      (setq module-name _module-name)))
  ;; setup target file
  (when (file-exists-p target)
    (delete-file target))
  (copy-file src-el target)
  (setq target-buf (find-file-noselect target))

  (load (concat default-directory "comment-box-search-forward.el"))
  (with-current-buffer target-buf

    ;; setup inferior-python-mode-helper--mname with `module-name`
    (goto-char (point-min))
    (let-alist (comment-box-search-forward "#" nil t)
      (when (string= .content tmp-id)
	(delete-region .beginning .end)
	(insert module-name)))

    ;; insert python-source from `src-py`
    (let-alist (comment-box-search-forward "#")
      (if (string= (symbol-name .content.include) src-py)
	  (delete-region .beginning .end)
	(error "There is no region to include `src-py`")))

    (with-current-buffer src-py-buf
      (beginning-of-buffer)
      (while (< (point) (point-max))
	(let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
	  (when (> (length line) 0)
	    (when (string-match (eval `(rx (seq symbol-start ,tmp-id symbol-end)))
				line)
	      (setq line (replace-match module-name t t line)))
	    (with-current-buffer target-buf
	      (insert (format "%S" line))
	      (delete-char -1)
	      (push-mark)
	      (beginning-of-line)
	      (delete-char 1)
	      (goto-char (region-end))
	      (insert "\n"))))
	(forward-line)))
    (delete-char -1)

    ;; final process
    (save-buffer))
  (mapc #'kill-buffer (list src-py-buf target-buf))
  (byte-compile-file target))

