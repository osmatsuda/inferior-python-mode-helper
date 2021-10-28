;;; comment-box-search-forward.el ---               -*- lexical-binding: t; -*-

(require 'rect)

(defun comment-box-search-forward (&optional line-elem bound eval-as-string)
  "Search rectangle made by comment symbols. Then evaluate the contents.
Return a a-list like '(beginning end width height content)"
  (unless line-elem (if (and (stringp comment-start)
			     (> (length comment-start) 0))
			(setq line-elem comment-start)
		      (error "you must specify `line-elem` argument")))
  (unless bound (setq bound (point-max)))
  (let* ((re-h-line (eval `(rx (>= 5 ,line-elem))))
	 (re-cont-line (eval `(rx (seq bol (+ ,line-elem)
				       (group (+? any))
				       (+ ,line-elem) eol))))
	 h-line-length
	 (rect-start (save-excursion
		       (re-search-forward re-h-line bound nil 1)
		       (setq h-line-length (length (match-string 0)))
		       (match-beginning 0)))
	 (rect-end (save-excursion
		     (re-search-forward re-h-line bound nil 2)
		     (unless (= (length (match-string 0)) h-line-length)
		       (error "unrectangled box"))
		     (match-end 0)))
	 (rect-size (rectangle-dimensions rect-start rect-end))
	 (content ""))
    (goto-char rect-start)
    (rectangle-next-line)
    (dotimes (n (- (cdr rect-size) 2))
      (let ((line (buffer-substring-no-properties (point) (+ (point) (car rect-size)))))
	(unless (string-match re-cont-line line)
	  (error "unrectangled box"))
	(setq content (concat content (match-string 1 line) "\n")))
      (rectangle-next-line))
    (goto-char rect-end)
    (list (cons 'beginning rect-start)
	  (cons 'end rect-end)
	  (cons 'width (car rect-size))
	  (cons 'height (cdr rect-size))
	  (cons 'content (if eval-as-string
			     (string-trim content)
			   (with-temp-buffer
			     (insert content)
			     (eval-last-sexp t)))))))

(provide 'comment-box-search-forward)
