



(defun extract-domain (url)
  (awhen (subseq url 8 (fifth (cl-ppcre:all-matches "/" url)))
    (list (subseq it 4 (third (cl-ppcre:all-matches "\\." it)))
	  (subseq it (1+ (third (cl-ppcre:all-matches "\\." it)))))))

(defun search-terms (url)
  (awhen (or (extract-url-parameter "query" url)
	     (extract-url-parameter "q" url))
    (split-sequence:split-sequence #\+ it)))

(with-open-file (in "/home/vb/4Nov2007")
  (do ((line (read-line in nil)
	     (read-line in nil)))
      ((null line))
    (awhen (split-sequence:split-sequence #\space line)
      (print (cons (search-terms (fourth it))
		   (extract-domain (fourth it)))))))
