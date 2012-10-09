(in-package :com.sondesh.database)

(defun filterTrains-by-time (trains now)
  (if trains
      (member
       (first
	(sort (copy-list (mapcar #'first trains))
	     #'(lambda (j1 j2) 
		 (< 
		  (timedifference-nd (second (first (cadr j1))) (current-short-time now)) 
		  (timedifference-nd (second (first (cadr j2))) (current-short-time now))))))
       trains
       :test #'(lambda (j1 jt2) 
		 (let ((j2 (first jt2)))
		   (and (= (car (first j1)) (car (first j2)))
			(= (cdr (first j1)) (cdr (first j2)))
			(equal (cadr j1) (cadr j2))))))
      nil))

(defun filter-spurious-journey (journeys &optional (acc '()))
  (if (not journeys)
      (reverse acc)
      (let* ((journey (first journeys))
	     (candidates (remove-adjacent-if-not
			 #'(lambda (jour)
			     (and (string=
				   (second (display-train-summary (first (cdr jour))))
				   (second (display-train-summary (first (cdr journey)))))
				  (= (car (display-train-terminates-at (first (cdr jour))))
				     (car (display-train-terminates-at (first (cdr journey)))))))
			 journeys)))
	(filter-spurious-journey  
	 (remove-if 
	  #'(lambda (jour)
	      (and (string=
		    (second (display-train-summary (first (cdr jour))))
		    (second (display-train-summary (first (cdr journey)))))
		   (= (car (display-train-terminates-at (first (cdr jour))))
		      (car (display-train-terminates-at (first (cdr journey)))))))
	  journeys)
	 (if candidates
	     (cons (best-candidate candidates) acc)
	     (cons (first journeys) acc))))))

(defun best-candidate (candidates)
  (first
   (sort
    candidates
    #'<
    :key #'(lambda (candidate) (short-time->mins (journey-duration (car (cdr (car candidate)))))))))

(defun timeline-date (leaving)
  (let* ((lo (and leaving (string/= "" leaving) 
		  (or (and (string= (subseq leaving 0 1) "#") (subseq leaving 1 (length leaving))) 
		      leaving)))
	 (l (or (and lo (stylized-day->caldate leaving)) ""))
	 (now (or (and (string/= l "") 
		       (< (- (now) (string->dttm l)) 0)
		       (string->dttm (format nil "~A 03:59:00 +0100 (GMT)" l)))
		  (now)))
	 (leaving-date (dttm->string now :format :caldate)))
    (values now leaving-date)))

(defun no-journeys (from to)
  (with-html
    (:p (fmt "Nothing found for journeys from ~A to ~A" from to))))

(defun timeline ()
  (let* ((params (break-as-parameters (request-uri)))
	 (from (first params))
	 (to (second params))
	 (leaving (third params))
	 (real-from (real-station-by-letters from))
	 (real-to (real-station-by-letters to)))
    (multiple-value-bind (now leaving-date) 
	(timeline-date leaving)
      (aif (and real-from real-to 
		(not (equal real-from real-to))
		(find-trains real-from real-to now))
	   (journey-page it real-from real-to leaving-date now)
	   (no-journeys from to)))))

(defun traintimes ()
  (let* ((params (break-as-parameters (request-uri)))
	 (from (first params))
	 (to (second params))
	 (leaving (third params))
	 (real-from (find-station-by-exact-name from))
	 (real-to (find-station-by-exact-name to)))
    (multiple-value-bind (now leaving-date) 
	(timeline-date leaving)
      (aif (and real-from
		real-to
		(not (equal real-from real-to))
		(find-trains real-from real-to now))
	   (journey-page it real-from real-to leaving-date now)
	   (no-journeys from to)))))




