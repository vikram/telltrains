(defun is-decoded-p (stn)
  (position (station-crs-code stn) *all-crses* :test #'string=))

(defun decoded-group-for-station-p (stn)
  (is-decoded-p (find-station-by-code (station-group-crs-code stn))))
  
(defun are-decoded-stns (from to)
  (let ((results '()))
    (if (is-decoded-p from)
	(push from results)
	(if (decoded-group-for-station-p from)
	    (push (find-station-by-code (station-group-crs-code from)) results)
	    (return-from are-decoded-stns nil)))
    (if (is-decoded-p to)
	(push to results)
	(if (decoded-group-for-station-p to)
	    (push (find-station-by-code (station-group-crs-code to)) results)
	    (return-from are-decoded-stns nil)))
    (if (= 2 (length results))
	(reverse results)
	nil)))

(defun routeing-journeys-db (from to time)
  (aif (are-decoded-stns from to)
       (let ((results (decode-journeys (station-crs-code (first it)) (station-crs-code (second it)) (weekday :time time))))
	 (if (and (equal from (first it))
		  (equal to (second it)))
	     results
	     (filter-routeing-journeys from to results)))
       (journeys+directs-db
	(remove-if
	 #'not
	 (mapcar
	  (expand-journey from to time)
	  (accumulate-journeys-db-routed from to time)))
	from
	to
	time)))

(defun filter-routeing-journeys (from to results)
  (remove-if-not
   #'(lambda (journey)
       (and journey
	    (equal from (display-train-from (first journey)))
	    (equal to (display-train-to (car (last journey))))
	    (display-train-stops (first journey))
	    (display-train-stops (car (last journey)))))
   (mapcar 
    #'(lambda (journey)
	(case (length journey)
	  (0 nil)
	  (1 (list 
	      (make-display-train<-trainid 
	       (first (display-train-terminates-at (first journey))) 
	       from
	       to)))
	  (t (append
	      (list
	       (make-display-train<-trainid 
		(first (display-train-terminates-at (first journey))) 
		from
		(display-train-to (first journey))))
	      (butlast (rest journey))
	      (list
	       (make-display-train<-trainid 
		(first (display-train-terminates-at (first journey))) 
		(display-train-from (first journey))
		to))))))
    results)))
