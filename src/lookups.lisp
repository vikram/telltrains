(in-package :com.sondesh.database)

(defparameter *results* (make-hash-table))

(defun add-times-slow (t1 t2)
  (let* ((x (or (and (not t1) "0:0") (and (string= t1 "") "0:0") t1))
	 (y (or (and (not t2) "0:0") (and (string= t2 "") "0:0") t2))
	 (1hour (parse-integer (first (cl-ppcre:split ":" x))))
	 (1min (parse-integer (second (cl-ppcre:split ":" x))))
	 (2hour (parse-integer (first (cl-ppcre:split ":" y))))
	 (2min (parse-integer (second (cl-ppcre:split ":" y)))))
    (hourmin->shorttime (+ 1hour 2hour) (+ 1min 2min))))

(defun hrmin->short-time (hr min)
  (declare (fixnum hr min) (optimize (speed 3) (safety 0)))
  (let ((s (make-string 5 :initial-element #\:)))
    (multiple-value-bind (h1 h2)
	(floor (mod hr 24) 10)
      (multiple-value-bind (m1 m2)
	  (floor min 10)
	(setf (char s 0) (digit-char h1))
	(setf (char s 1) (digit-char h2))
	(setf (char s 3) (digit-char m1))
	(setf (char s 4) (digit-char m2))))
    s))

(defun add-times (time1 time2)
  (declare (simple-base-string time1 time2) (optimize (speed 3) (safety 0)))
  (multiple-value-bind (hr1 min1) (split-time time1)
    (multiple-value-bind (hr2 min2) (split-time time2)
      (declare (simple-base-string hr1 min1 hr2 min2))
      (multiple-value-bind (hr min)
	  (floor (the fixnum (+ (the fixnum (* (the fixnum (parse-integer hr1)) 60))
				(the fixnum (parse-integer min1))
				(the fixnum (* (the fixnum (parse-integer hr2)) 60))
				(the fixnum (parse-integer min2))))
		 60)
      (hrmin->short-time hr min)))))

(defun journey-start (trains)
  (second (first trains)))

(defun journey-end (trains)
  (fourth (car (last trains))))

(defun journey-duration (trains)
  (add-times 
   (reduce #'add-times trains :key #'sixth)
   (map-subsequent-two #'journey-time
		       trains #'add-times)))

(defun journey-time (t1 t2) 
  (declare (sequence t1 t2) (optimize (speed 3) (safety 0)))
  (if (not t2)
      "00:00"
      (multiple-value-bind (hr min) 
	  (floor (the fixnum (timedifference-nd (second t2) (fourth t1))) 60)
	(hrmin->short-time hr min))))

(defun train-name (from to day month hour min)
  (make-logic-symbol (concatenate 'string from to day month hour min)))

(defun extract-all-options (html-list)
  (let ((train-number 0))
    (mapcar #'(lambda (optionBox) (list (incf train-number)
					(mapcar #'extract-option-leg 
						(remove-html-node-if-not 
						 optionBox 
						 #'(lambda (element) (element= element :div "optionSummary" ""))))))
	  (remove-html-node-if-not
	   html-list
	   #'(lambda (element) (element= element :div "optionBox" ""))))))

(defun extract-option-leg (html-list)
  (aif (remove-html-node-if-not
	html-list
	#'(lambda (element) (element= element :td "bold" "")))
       (let ((type (string-trim '(#\Space #\Tab #\Return #\linefeed) (second (third it)))))
	 (list (second (third (first it))) 
	       (subseq (string-trim '(#\Space #\Tab #\Return #\linefeed) (second (first it))) 0 5)
	       (second (third (second it))) 
	       (subseq (string-trim '(#\Space #\Tab #\Return #\linefeed) (second (second it))) 0 5)
	       (if (string= "Underground&nbsp;" type)
		   "underground"
		   type)
	       (second (fourth it))))))

(defun fix-tube-change-glitches (journey &optional (time "0:-2"))
  (list (car journey)
	(append
	(MAP-SUBSEQUENT-TWO 
	 #'(lambda (t1 t2) 
	     (if (not t2)
		 (list t1)
		 (if (string= (fourth t1) (second t2))
		     (list
		      (list (first t1) (second t1) (third t1) (add-times (fourth t1) time) (fifth t1) (add-times (sixth t1) time)))
		     (list t1))))
	     (cadr journey) #'append)
	(if (= 1 (length (cadr journey)))
	    nil
	    (last (cadr journey))))))

(defun train< (time1 time2 &optional (current-time (current-short-time)))
  (< (timedifference-nd time1 current-time)
     (timedifference-nd time2 current-time)))

(defun getChanges (changes number-of-changes)
  (unless (equal '("-") changes)
    (let ((changesURL (third (car (second (second changes))))))
      (map0-n #'(lambda (n) 
		  (if (= n number-of-changes)
		      (list (format-nr-time (extract-url-parameter (format nil "ddt~A" (1+ n)) changesURL))
			    (find-station (extract-url-parameter (format nil "ol~A" (1+ n)) changesURL))
			    (extract-url-parameter (format nil "tm~A" (1+ n)) changesURL))
		      (list (format-nr-time (extract-url-parameter (format nil "ddt~A" (1+ n)) changesURL))
			    (find-station (extract-url-parameter (format nil "ol~A" (1+ n)) changesURL))
			    (extract-url-parameter (format nil "tm~A" (1+ n)) changesURL))))
	      number-of-changes))))

