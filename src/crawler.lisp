(in-package :com.sondesh.database)

(defun html->list (html)
  (net.html.parser:parse-html html))

(defun fetch (url)
  (multiple-value-bind (html status-code
			     headers
			     uri
			     http-stream
			     must-close)
      (drakma:http-request url :user-agent :firefox)
    (values (html->list html)
	    status-code
	    headers
	    uri)))

(defun extract (html tag)
  (if (not html)
      nil
      (if (atom (car html))
	  (if (eql tag (car html))
	      html
	      (extract (cdr html) tag))
	  (or (extract (car html) tag)
	      (extract (cdr html) tag)))))


(defun flatten (x) 
  (labels ((rec (x acc) 
	     (cond ((null x) acc) 
		   ((atom x) (cons x acc)) 
		   (t (rec (car x) (rec (cdr x) acc)))))) 
    (rec x nil)))

(defun prune (test tree) 
  (labels ((rec (tree acc) 
	     (cond ((null tree) (nreverse acc)) 
		   ((consp (car tree)) (rec (cdr tree) 
					    (cons (rec (car tree) nil) acc))) 
		   (t (rec (cdr tree) 
			   (if (funcall test (car tree)) 
			       acc 
			       (cons (car tree) acc))))))) 
    (rec tree nil)))

(defun extract-all (tree test)
  (labels ((rec (tree acc)
	     (cond ((null tree) acc)
		   ((consp (car tree)) (rec (cdr tree)
					    (nconc (rec (car tree) nil) acc)))
		   (t (rec (cdr tree)
			   (if (funcall test (car tree))
			       (cons tree acc)
			       acc))))))
    (rec tree '())))

(defun linkp (x) (eql x :a))

(defparameter *relative-url*
  (cl-ppcre:create-scanner "(~/|/|\\./)([-A-Za-z0-9_\\$\\.\\+\\!\\*\\(\\),;:@&=\\?/~\\#\\%]|\\\\)+"))

(defparameter *absolute-url*
  (cl-ppcre:create-scanner "([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}|(((news|telnet|nttp|file|http|ftp|https)://)|(www|ftp)[-A-Za-z0-9]*\\.)[-A-Za-z0-9\\.]+)(:[0-9]*)?/[-A-Za-z0-9_\\$\\.\\+\\!\\*\\(\\),;:@&=\\?/~\\#\\%]*[^]'\\.}>\\),\\\"]"))

(defparameter *hostname*
  "([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}|(((news|telnet|nttp|file|http|ftp|https)://)|(www|ftp)[-A-Za-z0-9]*\\.)[-A-Za-z0-9\\.]+)(:[0-9]*)?")

(defun generate-urls (hostname links 
		      &key (test (let ((host (cl-ppcre:create-scanner hostname)))
				   #'(lambda (link) (cl-ppcre:all-matches host link)))))
  (remove-if-not 
   #'(lambda (link) (funcall test link))
   (mapcar #'(lambda (ahref) 
	       (let ((link (third ahref)))
		 (if (cl-ppcre:all-matches *relative-url* link)
		     (format nil "~A~A" hostname link)
		     link)))
	   links)))

(defun crawl-within-one-page (start)
  (let ((html (fetch start))
	(hostname (or (first (cl-ppcre:all-matches-as-strings *hostname* start)) "")))
    (generate-urls hostname
		   (extract-all html #'linkp)
		   :test (let ((host (cl-ppcre:create-scanner start)))
			   #'(lambda (link) (cl-ppcre:all-matches host link))))))

(defstruct url
  (url nil :type sequence)
  (fetched nil :type symbol)
  (html () :type list))
