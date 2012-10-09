(in-package :com.sondesh.database)

(defparameter *permitted-route-trains* (make-hash-table :test 'equal))
(defparameter *permitted-journeys-loaded* nil)


(defun upload-permitted->db ()
  (progn
    (with-train-database!
      (clsql:execute-command "delete from permitted"))
    (dolist (crs-code (hash-keys *permitted-routes*))
      (let ((ht (make-hash-table :test 'equal))
	    (algorithm (if (member crs-code *aresults* :test #'string=)
			   "a*"
			   "bfs")))
	(print (cons "doing" crs-code))
	(load-permitted-trains crs-code ht)
	(with-train-database!
	  (maphash #'(lambda (k v)
		       (let ((id (next-id)))
			 (clsql:update-records-from-instance 
			  (make-instance 'permitted
					 :id id
					 :from-crs-code crs-code
					 :to-crs-code (subseq k 3 6)
					 :done-london nil
					 :algorithm algorithm
					 :answers (write-to-string v)
					 :timestamp (multiple-value-bind (se mi ho da mo ye) (decode-universal-time (now) -1)
						      (clsql:make-date :year ye :month mo :day da :hour ho :minute mi :second se))
					 :version 1))))
		   ht))))))

(defun load-permitted-trains (from-crs-code &optional (ht *permitted-route-trains*))
  (with-open-file (in (sondesh-file-path (concatenate 'string "additional/" from-crs-code ".trains")))
    (dolist (trains (read in))
      (setf (gethash (concatenate 'string from-crs-code (first trains)) ht)
	    (car (rest trains))))))

(defun load-all-permitted-trains ()
  (dolist (crs-code (hash-keys *permitted-routes*))
    (load-permitted-trains crs-code))
  (setf *permitted-journeys-loaded* t))

(defun add-brackets (crs)
  (with-open-file (out (sondesh-file-path (concatenate 'string "additional/" crs ".1")) 
		       :direction :output :if-exists :supersede)
    (write-line "(" out)
    (with-open-file (in (sondesh-file-path (concatenate 'string "additional/" crs ".trains")) 
			:direction :input)
      (do ((line (read-line in nil)
		 (read-line in nil)))
	  ((null line))
	(when line
	  (write-line line out))))
    (write-line ")" out))
  (rename-file (sondesh-file-path (concatenate 'string "additional/" crs ".1"))
	       (sondesh-file-path (concatenate 'string "additional/" crs ".trains"))))

(defun add-all-brackets ()
  (dolist (crs-code (hash-keys *permitted-routes*))
    (let ((pleasedo nil))
      (with-open-file (in (sondesh-file-path (concatenate 'string "additional/" crs-code ".trains")))
	(setf pleasedo (not (listp (first (read in))))))
      (when pleasedo
	(add-brackets crs-code)))))

(defun generate-permitted-trains (start end &key (fn #'generate-permitted-routes-trains-a*))
  (let ((permitted-routes (make-hash-table :test 'equal))
	(i 0))
    (maphash #'(lambda (k v)
		 (when (and (< i end) (>= i start))
		   (setf (gethash k permitted-routes) v))
		 (incf i))
	     *permitted-routes*)
    (funcall fn permitted-routes)))
   
(defun generate-permitted-routes-trains (routes)
  (maphash #'(lambda (start-crs-code value) 
	       (unless (member start-crs-code 
			       '()
			       :test #'string=)
		 (with-open-file (out (sondesh-file-path (concatenate 'string "additional/" start-crs-code ".trains"))
				      :direction :output 
				      :if-exists :supersede)
		   (dolist (route (group value :test #'string=))
		     (print (concatenate 'string start-crs-code " " (first route) " started") t)
		     (print (list (first route) 
				  (delete-duplicates 
				   (mapcar #'(lambda (journey) 
					       (mapcar #'(lambda (train) 
							   (let ((train-info (third train)))
							     (+ (* (first train-info) 1000000000000000)
								(* (convert-bit-string-to-integer (second train-info)) 1000000000000)
								(* (third train-info) 1000000)
								(fourth train-info))))
						       (cdr journey)))
					   (possible-journeys-routes-a* (find-station-by-code start-crs-code) 
									(find-station-by-code (first route))
									:time (string->dttm "2007-06-04 Mon 06:00:00 +0100 (BST)")
									:time-fn (make-time-test-fn2)))
				   :test #'equal)) 
			    out)
		     (print (concatenate 'string start-crs-code " " (first route) " done") t)))))
	   routes))

(defun generate-permitted-routes-trains-a* (routes)
  (maphash #'(lambda (start-crs-code value) 
	       (unless (member start-crs-code 
			       *aresults*
			       :test #'string=)
		 (with-open-file (out (sondesh-file-path (concatenate 'string "additional/" start-crs-code ".trains"))
				      :direction :output 
				      :if-exists :supersede)
		   (dolist (route (group value :test #'string=))
		     (print (concatenate 'string start-crs-code " " (first route) " started") t)
		     (let ((results (delete-duplicates 
				   (mapcar #'(lambda (journey) 
					       (map-path #'(lambda (train) 
							     (let ((train-info (train-info train)))
							       (+ (* (first train-info) 1000000000000000)
								  (* (convert-bit-string-to-integer (second train-info)) 
								     1000000000000)
								  (* (third train-info) 1000000)
								  (fourth train-info))))
							 journey))
					   (possible-journeys-routes-a* (find-station-by-code start-crs-code) 
									(find-station-by-code (first route))
									:time now
									:time-fn (make-time-test-fn2)))
				   :test #'equal)))
		       (print (list (first route) results) out)
		       (print (cons (concatenate 'string start-crs-code " " (first route) " done") (length results)) t))))))
	   routes))

