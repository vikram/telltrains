(in-package :com.sondesh.database)

(defun read-formated-line (fields line &key (comma-separated-p nil))
  (if comma-separated-p
      (split-sequence:split-sequence #\comma line)
      (let ((pos 0)
	    (result nil))
	(dolist (field fields)
	  (unless (eql :ignore (car (last field)))
	    (let ((answer (subseq line pos (+ pos (second field)))))
	      (push (if (listp (car (last field)))
			(cl-ppcre:split " " answer)
			(if (eql (car (last field)) :trim)
			    (string-trim '(#\space) answer)
			    answer))
		    result)))
	  (incf pos (second field)))
	(nreverse result))))

(defmacro field-setup ((name file-name &key (comma-separated-p nil) (test 'identity)) &rest fields)
  (with-gensyms (record)
    `(progn
       ,@(mapcar #'(lambda (record) `(defstruct (,(second record) (:type list))
				       ,@(mapcar #'car (remove-if #'(lambda (field) (eql :ignore (car (last field)))) (third record)))))
		 fields)
       (defparameter ,name
	 (data-file-reader ,comma-separated-p ,test ,file-name ,@fields)))))

(defmacro data-file-reader (comma-separated-p test filepath &rest fields)
  (with-gensyms (line var field-defs field scanner in results val)
    `#'(lambda () 
	 (let ((,field-defs nil)
	       (,results nil))
	   (dolist (,field ',fields)
	     (let ((,scanner (cl-ppcre:create-scanner (car ,field))))
	       (push (cons (car ,field) ,scanner) ,field-defs)))
	   (with-open-file (,in ,(sondesh-file-path filepath))
	     (do ((,line (read-line ,in nil)
			 (read-line ,in nil)))
		 ((null ,line))
	       (when ,line
		 (awhen (remove-if-not #'(lambda (,var) (cl-ppcre:all-matches (cdr (assoc (car ,var) ,field-defs :test #'string=)) ,line)) ',fields)
		    (let ((,val (read-formated-line (third (car it)) (trim ,line) :comma-separated-p ,comma-separated-p)))
		      (if (funcall #',test ,val)
			  (push ,val ,results)))))))
	   (nreverse ,results)))))