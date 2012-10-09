(in-package :com.sondesh.database)

(defun possible-journeys-a* (start end &key (time (get-universal-time)) (time-fn (make-time-test-fn :time time)) (summary-sorted-p nil))
  (possible-journeys-routes-a* (FIRST (find-station-by-letters start))
			      (first (find-station-by-letters end))
			      :time time
			      :time-fn time-fn
			      :summary-sorted-p summary-sorted-p))

(defun possible-journeys-routes-a* (from to &key (time (get-universal-time)) (time-fn (make-time-test-fn :time time)) (summary-sorted-p nil))
  (declare (list from to)
	   (optimize (speed 3) (safety 0)))
  (let ((results '())
	(route-from (copy-list (find-station-by-code (route-crs from))))
	(route-to (copy-list (find-station-by-code (route-crs to)))))
    (setf (station-tiploc-codes route-from) (station-tiploc-codes from))
    (setf (station-tiploc-codes route-to) (station-tiploc-codes to))
    (awhen (permitted-trips (station-crs-code route-from) (station-crs-code route-to))
      (dolist (path it)
	(let ((detailed-path (append (list route-from) (rest (butlast (reverse (map-path #'find-station-by-code path)))) (list route-to))))
	  (setf results (append (permitted-route->trains detailed-path (car (last detailed-path)) :time-fn time-fn) results)))))
    (setf results (append (permitted-route->trains (list route-from route-to) route-to :time-fn time-fn) results))
    (if summary-sorted-p
	(sort-result-summarize-a* results time)
	results)))

(defun sort-result-summarize-a* (results time)
  (declare (list results)
	   (optimize (speed 3) (safety 0)))
  (sort 
   (delete-duplicates 
    (mapcar #'journey-summary-a* results) 
    :test #'equal)
   (make-journey< time)))

(defun journey-summary-a* (path)
  (let ((result '())
	(previous nil))
    (map-path #'(lambda (train)
		  (if previous 
		      (if (train= train previous)
			  (progn
			    (push (stops->info (train-stops train) (train-to previous) (train-from train)) result)
			    (setf previous nil))
			  (progn
			    (push (train-summary previous) result)
			    (setf previous train)))
		      (setf previous train)))
	      path)
    (when previous
      (push (train-summary previous) result))
    result))

(defstruct (train (:print-function print-train))
  from to stops info)

(defun train= (t1 t2)
  (= (first (train-info t1)) (first (train-info t2))))

(defun print-train (train &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "#<Train :id ~A :summary ~A>" (first (train-info train)) (train-summary train)))

(defun train-summary (train)
  (stops->info (train-stops train) (train-from train) (train-to train)))

(defun train-stops->tiplocs (train)
  (mapcar #'fifth (train-stops train)))

(defun readjust-train-a* (train detailed-path)
  (let ((stops (reverse (train-stops->tiplocs train))))
    (dolist (rp (reverse (cdr (member (station-crs-code (train-from train))
				      detailed-path
				      :key #'station-crs-code :test #'string=))))
      (awhen (stn-in-p rp stops)
	(when (validate-readjusted-train-p (reverse stops) (station-tiploc-codes (train-from train)) (station-tiploc-codes rp))
	  (let ((cleaned-rp (copy-list rp)))
	    (when (or (station-in-group-p cleaned-rp) (station-group-crs-code cleaned-rp))
	      (setf (station-tiploc-codes cleaned-rp) (list (first it))))
	    (return-from readjust-train-a*
	      (make-train :from (train-to train)
			  :to cleaned-rp
			  :stops (train-stops train)
			  :info (train-info train)))))))))

(defun direct-trains (from to time-fn path)
  (remove-if #'not
	     (mapcar #'(lambda (train) 
			 (aif (readjust-train-a*
				(make-train :from from
					    :to from
					    :stops (first train)
					    :info (car (last train)))
				path)
			      it
			      (let ((crs (gethash (first (stn-in-p to (mapcar #'fifth (first train)))) *tiploc->crs*)))
				(when crs
				  (let ((temp-to (copy-list (find-station-by-code crs))))
				    (setf (station-crs-code temp-to) (station-crs-code to))
				    (make-train :from from
						:to temp-to
						:stops (first train)
						:info (car (last train)))))
				)))
		     (find-direct-trains-all-stops from to :time-fn time-fn))))

(defun stop= (stop1 stop2)
  (string= (station-crs-code stop1) (station-crs-code stop2)))

(defun good-neighbor-old-p (option)
  #'(lambda (train)
      (let ((dur (timedifference-nd (second (train-summary train))
				    (fourth (train-summary option))))
	    (time-for-change (parse-integer (station-minimum-change-time (train-to option)))))
	(declare (fixnum dur time-for-change))
	(let ((r1 (not (stn-in-p (train-from option) (train-stops->tiplocs train))))
	      (r2 (<= time-for-change (1+ dur)))
	      (r3 (< dur (+ 180 time-for-change)))
	      (r4 (date-matches-p (train-info option) (train-info train)))
	      (r5 (string/= (station-crs-code (train-from train)) (station-crs-code (train-to train)))))
	  (dbg :search-results ";; good-neighbor? train ~A train-from ~A train-to ~A"
	       (list train (train-from train) (train-to train)))
	  (dbg :search-results ";; good-neighbor? dur:~A time-for-change:~A train-option:~A train-info ~A stn:~A change:~A dur-change:~A dates:~A not-same:~A" 
	       (list dur time-for-change (train-info train) (train-info option) r1 r2 r3 r4 r5))
	  (and r1 r2 r3 r4 r5)))))

(defun good-neighbor-p (option)
  #'(lambda (train)
      (let ((dur (timedifference-nd (second (train-summary train))
				    (fourth (train-summary option))))
	    (time-for-change (parse-integer (station-minimum-change-time (train-to option)))))
	(declare (fixnum dur time-for-change))
	(let ((r2 (<= time-for-change (1+ dur)))
	      (r3 (< dur (* 12 60)))
	      (r4 (date-matches-p (train-info option) (train-info train)))
	      (r5 (string/= (station-crs-code (train-from train)) (station-crs-code (train-to train)))))
	  (dbg :search-results ";; good-neighbor? train ~A train-from ~A train-to ~A"
	       (list train (train-from train) (train-to train)))
	  (dbg :search-results ";; good-neighbor? dur:~A time-for-change:~A train-option:~A train-info ~A change:~A dur:~A dates:~A not-same:~A" 
	       (list dur time-for-change (train-info train) (train-info option) r2 r3 r4 r5))
	  (and r2 r3 r4 r5)))))

(defun permitted-train-neighbors (detailed-path time-fn)
  #'(lambda (train journey)
      (when train
	(let ((results nil))
	  (if (train-stops-at-to-p train)
	    (let* ((readjusted (readjust-train-a* train detailed-path))
		   (next (next-stop (member (train-to train) 
					    detailed-path
					    :test #'(lambda (s1 s2) 
						      (string= (station-crs-code s1)
							       (station-crs-code s2))))
				    (train-to train)))
		   (trains (direct-trains (train-to train)
					  next
					  (funcall (funcall time-fn 'change) (fourth (train-summary train)))
					  detailed-path)))
	      (dbg :search-debug ";;next-tiplocs ~A" (list (station-tiploc-codes next)))
	      (dbg :search-results ";;current-train ~A " (list train))
	      (dbg :search-results ";;options train ~A " (list trains))
	      (if readjusted
		  (progn
		    (dbg :search ";; neighbors last-train-stop:~A readjusted-to:~A next-stop:~A" (list (station-crs-code (train-to train)) 
												       (station-crs-code (train-to readjusted)) 
												       (station-crs-code next)))
		    (setf results 
			  (if (not (stop= (train-to readjusted) (train-to train)))
			      (cons readjusted (remove-if-not (good-neighbor-p train) trains))
			      (remove-if-not (good-neighbor-p train) trains))))
		  (progn
		    (dbg :search ";; neighbors last-train-stop:~A next-stop:~A" (list (station-crs-code (train-to train)) (station-crs-code next)))
		    (setf results (remove-if-not (good-neighbor-p train) trains)))))
	    (awhen (readjust-train-a* train detailed-path)
	      (list it)))
	  (dbg :search-results ";;final-options ~a" (list results))
	  results))))

(defun direct-trains->path (from to time-fn detailed-path)
   (mapcar #'(lambda (train) 
	       (let ((cost (short-time->mins (journey-duration (list (train-summary train))))))
		 (make-path :state train
			    :cost-so-far (short-time->mins (journey-duration (list (train-summary train))))
			    :total-cost (+ cost (funcall (journey-left detailed-path) train)))))
	   (direct-trains from to time-fn detailed-path)))

(defun train-stops-at-to-p (train)
  (stn-in-p (train-to train) (train-stops->tiplocs train)))

(defun journey-cost (state1 state2)
  (- 
   (short-time->mins
    (add-times
     (journey-duration (list (train-summary state1) (train-summary state2)))
     (if (= (first (train-info state1)) (first (train-info state2)))
	 "00:00"
	 "00:15")))
   (short-time->mins (journey-duration (list (train-summary state1))))))

(defun journey-cost-old (state1 state2)
  (- 
   (short-time->mins
    (add-times
     (journey-duration (list (train-summary state1) (train-summary state2)))
     (if (= (first (train-info state1)) (first (train-info state2)))
	 "00:00"
	 "00:15")))
   (short-time->mins (journey-duration (list (train-summary state1))))))

(defun max-time-stops (stop1 stop2)
  (let ((total-time 0)
	(total-num 0))
    (dolist (tiploc1 (station-tiploc-codes stop1))
      (dolist (tiploc2 (station-tiploc-codes stop2))
	(awhen (gethash (concatenate 'string tiploc1 tiploc2) *connectivity*)
	  (let ((max (connectivity-maximum it)))
	    (declare (fixnum total-time total-num max)
		     (optimize (speed 3) (safety 0)))
	    (incf total-time max)
	    (incf total-num)))))
    (float (/ total-time total-num))))
  
(defun journey-left (detailed-path)
  #'(lambda (train)
      (let ((whats-left (member (train-to train) detailed-path :test #'(lambda (s1 s2) (string= (station-crs-code s1) (station-crs-code s2))))))
	(+ (average-time-path whats-left) (* 60 (length whats-left))))))

(defun average-time-path (path &optional (accumulator 0))
  (if (not (cdr path))
      accumulator
      (average-time-path (rest path) (+ accumulator (average-time-stops (first path) (second path))))))

(defun clear-cache ()
  (progn
    (clear-memoize 'find-direct-trains-all-stops)
    (clear-memoize 'find-simple-direct-trains)
    (clear-memoize 'stops->info)
    (clear-memoize 'yymmdd->simple-date)
    (clear-memoize 'find-trains-tiplocs)))

(defun permitted-route->trains (detailed-path dest 
				&key (time (get-universal-time)) 
			        (time-fn (make-time-test-fn :time time)))
  (let ((results '()))
    (clear-cache)
    (dolist (path (direct-trains->path (first detailed-path) (second detailed-path) time-fn detailed-path))
      (dbg :search-results ";; option:~A" (list path))
      (a*-search
       (list path)
       #'(lambda (path) 
	   (when (string= (station-crs-code dest) (station-crs-code (train-to (path-state path))))
	     (dbg :search-results-done ";; added this path:~A" (list path))
	     (push path results)
	     t))
       (permitted-train-neighbors detailed-path time-fn)
       #'journey-cost
       (journey-left detailed-path)
       #'(lambda (state1 state2)
	   (= (first (train-info state1)) (first (train-info state2))))))
    (clear-cache)
    results))

;;find the pair which has the shortest distance
;;then pick those for the small stations.

(defun find-trains-old (from to &optional (time (now)))
  (let ((route-from (find-station-by-code (route-crs from)))
	(route-to (find-station-by-code (route-crs to))))
  (if (and (station-routeing-location-p route-from) (station-routeing-location-p route-to))
      (find-saved-trains-db from to time)
      (let ((results (our-get-unique-trains-changes-no-summary from to time)))
	(if (station-routeing-location-p route-from)
	    (dolist (route-crs (from-routeing-points (station-crs-code from) to))
	      (multiple-value-bind (options add-these)
		  (readjust-journeys (our-get-unique-trains-changes-no-summary from (find-station-by-code route-crs) time) to)
		(dolist (saved options)
		  (push (best-result 
			 (trains->journeys 
			  saved 
			  (direct-trains->path (train-to (path-state saved)) to 
					       (funcall (funcall (make-time-test-fn :time time) 'change) 
							(fourth (train-summary (path-state saved))))
					       (list (find-station-by-code route-crs) to))) time) 
			results))
		(dolist (saved add-these)
		  (push saved results))))
	    (if (station-routeing-location-p route-to)
		(dolist (route-crs (to-routeing-points (station-crs-code to) from))
		  (multiple-value-bind (options add-these)
		      (readjust-journeys (our-get-unique-trains-changes-no-summary (find-station-by-code route-crs) to time) to)
		    (dolist (direct (direct-trains->path from (find-station-by-code route-crs)
							 (make-time-test-fn :time time) (list from (find-station-by-code route-crs))))
		      (push (best-result
			     (trains->journeys direct options)
			     time)
			    results))
		    (dolist (saved options)
		      (push saved results))))
		(dolist (route-crs-from (station-routeing-points from))
		  (dolist (route-crs-to (station-routeing-points to))
		    (let ((from-directs (direct-trains->path 
					 from 
					 (find-station-by-code route-crs-from)
					 (make-time-test-fn :time time) 
					 (list from (find-station-by-code route-crs-from))))
			  (saved (our-get-unique-trains-changes-no-summary 
				  (find-station-by-code route-crs-from) 
				  (find-station-by-code route-crs-to) 
				  time)))
		      (dolist (from-direct from-directs)
			(dolist (choice (trains->journeys from-direct saved))
			  (push (best-result 
				 (trains->journeys choice 
						   (direct-trains->path (find-station-by-code route-crs-to) to
									(funcall (funcall (make-time-test-fn :time time) 'change) 
										 (fourth (train-summary (path-state choice))))
									(list (find-station-by-code route-crs-to) to))) time) 
				results))))))))
	(mapcar 
	 #'(lambda (journey) (cons (funcall (journey-pos time) (display-journey-summary journey)) journey))
	 (sort 
	  (delete-duplicates 
	   (mapcar #'(lambda (j) (reverse (map-path #'(lambda (train)
							(make-display-train
							 :from (train-from train)
							 :to (train-to train)
							 :summary (train-summary train)
							 :stops (adjust-stops (train-stops train) (train-from train) (train-to train))
							 :terminates-at (car (last (train-stops train))))) j))) 
		   (remove-if #'not results))
	   :test #'equal
	   :key #'display-journey-summary)
	  (make-journey< time)
	  :key #'display-journey-summary))))))

(defun find-all-trains (from to time)
  (multiple-value-bind (type start end)
      (pick-closest-match from to)
    (if (string= start end)
	(direct-display-trains (station-crs-code from) (station-crs-code to) time)
	(let ((journeys (routeing-journeys-db (find-station-by-code start) (find-station-by-code end) time)))
	  (case type
	    (none journeys)
	    (source 
	     (merge-journeys (direct-display-trains (station-crs-code from) start time)
			     journeys))
	    (target 
	     (merge-journeys journeys
			     (direct-display-trains end (station-crs-code to) time)))
	    (both 
	     (merge-journeys (merge-journeys (direct-display-trains (station-crs-code from) start time)
					     journeys)
			     (direct-display-trains end (station-crs-code to) time))))))))

(defun find-trains-to-encode (from to time)
  (cleanup-saved-trains-db
   (remove-if #'not
	      (routeing-journeys-db from to time :ignore-shortest-p t))
   time))

(defun find-trains (from to &optional (time (now)))
  (cleanup-saved-trains-db
   (remove-if #'not
	      (find-all-trains from to time))
   time))

(defun merge-journeys (first second)
  (mapcar #'(lambda (journey)
	      (best-match journey second))
	  first))

(defun end-time+change (end-time change-at)
  (multiple-value-bind (hr mins)
      (floor (add-minutes
	      (short-time->hrmins end-time)
	      (parse-integer (station-minimum-change-time change-at)))
	     100)
    (hrmin->short-time hr mins)))

(defun best-match (journey set)
  (let* ((end-time (end-time+change (fourth (display-train-summary (car (last journey))))
				    (display-train-to (car (last journey)))))
	 (sorted (sort (remove-if #'not
				  (mapcar #'(lambda (jour)
					      (if (string/= (first (display-train-summary (first jour)))
							    (third (display-train-summary (car (last journey)))))
						  nil
						  (if (= (first (display-train-terminates-at (first jour)))
							 (first (display-train-terminates-at (car (last journey)))))
						      (cons 0 jour)
						      (cons (timedifference-nd 
							     (second (display-train-summary (first jour)))
							     (fourth (display-train-summary (car (last journey)))))
							    jour))))
					  set))
		       #'<
		       :key #'car)))
    (awhen (cdr (first sorted))
      (if (journey-already-stops-at-p journey (display-train-to (car (last it))) :all-stops t)
	  (truncate-journey journey (display-train-to (car (last it))))
	  (if (= (first (display-train-terminates-at (first it)))
		 (first (display-train-terminates-at (car (last journey)))))
	      (append (butlast journey)
		      (list (expand-train (car (last journey)) (display-train-to (car (last it))))))
	      (if (last-journey-overlaps-p it journey)
		  (expand-last-train journey it)
		  (append journey it)))))))

(defun last-journey-overlaps-p (trains journey)
  (not 
   (zerop 
    (length
     (remove-if
      #'not
      (mapcar #'(lambda (train) 
		  (journey-already-stops-at-p
		   trains
		   (display-train-from train)
		   :all-stops t))
	      journey))))))

(defun expand-last-train (journey trains)
  (let ((result '())
	(from (first 
	       (remove-if 
		#'not
		(mapcar #'(lambda (train) 
			    (aif (journey-already-stops-at-p trains (display-train-from train) :all-stops t)
				 (display-train-from train)
				 nil))
			journey))))
	(found nil))
    (dolist (train journey)
      (when (not found)
	(if (not (equal (display-train-from train) from))
	    (push train result)
	    (dolist (tr trains)
	      (if (journey-already-stops-at-p (list tr) from :all-stops t)
		  (let ((stops (second (train-details (first (display-train-terminates-at tr))))))
		    (push
		     (make-display-train
		      :from from
		      :to (display-train-to tr)
		      :summary (stops->info stops from (display-train-to tr))
		      :stops (adjust-stops stops from (display-train-from tr))
		      :terminates-at (display-train-terminates-at tr))
		     result))
		  (when found
		    (push tr result)))))))
    (nreverse result)))

(defun expand-train (train to)
  (let ((stops (second (train-details (first (display-train-terminates-at train))))))
    (make-display-train
     :from (display-train-from train)
     :to to
     :summary (stops->info stops (display-train-from train) to)
     :stops (adjust-stops stops (display-train-from train) to)
     :terminates-at (display-train-terminates-at train))))

(defun journey-already-stops-at-p (journey to &key (all-stops t))
  (let ((stops (if (not all-stops)
		   (mappend #'display-train-stops journey)
		   (mappend (compose #'second #'train-details #'first #'display-train-terminates-at) journey))))
    (member (station-tiploc-codes to)
	    (mapcar #'fifth stops)
	    :test #'(lambda (lst ber) (member ber lst :test #'string=)))))

(defun truncate-journey (journey to)
  (let ((result '()))
    (dolist (train journey)
      (if (journey-already-stops-at-p (list train) to :all-stops t)
	  (let ((stops (second (train-details (first (display-train-terminates-at train))))))
	    (push (make-display-train
		   :from (display-train-from train)
		   :to to
		   :summary (stops->info stops (display-train-from train) to)
		   :stops (adjust-stops stops (display-train-from train) to)
		   :terminates-at (display-train-terminates-at train))
		  result))
	  (push train result)))
    (nreverse result)))

(defun from-routeing-points (from stn)
  (let ((paths (mapcar #'(lambda (rp) (cons rp (station-link from rp))) (station-routeing-points stn)))
	(results (station-routeing-points stn)))
    (dolist (path paths)
      (let ((check-within (remove-if #'(lambda (p) (string= (car p) (car path))) paths)))
	(dolist (with check-within)
	  (when (subsetp (reverse (map-path #'identity (cdr with))) (reverse (map-path #'identity (cdr path))) :test #'string=)
	    (setf results (remove (car path) results))))))
    results))

(defun to-routeing-points (to stn)
  (let ((paths (mapcar #'(lambda (rp) (cons rp (station-link rp to))) (station-routeing-points stn)))
	(results (station-routeing-points stn)))
    (dolist (path paths)
      (let ((check-within (remove-if #'(lambda (p) (string= (car p) (car path))) paths)))
	(dolist (with check-within)
	  (when (subsetp (reverse (map-path #'identity (cdr with))) (reverse (map-path #'identity (cdr path))) :test #'string=)
	    (setf results (remove (car path) results))))))
    results))

(defun readjust-journeys (journeys to)
  (let ((results '()) (add-these '()))
    (dolist (journey journeys)
      (aif (readjust-journey journey to)
	   (progn
	     (setf (train-to (path-state it)) to)
	     (push it add-these))
	   (push journey results)))
    (values results add-these)))

(defun readjust-journey (journey to)
  (if (stn-in-p to (mapcar #'fifth (train-stops (path-state journey))))
      journey
      (if (path-previous journey)
	  (readjust-journey (path-previous journey) to)
	  nil)))

(defun best-result (journeys time)
  (if (atom journeys)
      journeys
      (if (cdr journeys)
	  (first
	   (sort
	    journeys
	    #'(lambda (p1 p2) (funcall (make-journey< time) (journey-summary-a* p1) (journey-summary-a* p2)))))
	  (first journeys))))

(defun trains->journeys (starter options)
  (mapcar #'(lambda (option-cons)
	      (let* ((option (car option-cons))
		     (cost (+ (journey-cost (path-state starter) (cdr option-cons))
			      (path-cost-so-far starter)))
		     (previous starter))
		(dolist (train (reverse (map-path #'identity option)))
		  (let ((x (make-path :state train
				      :previous previous)))
		    (setf previous x)))
		previous))
	  (remove-if-not (good-neighbor-p (path-state starter)) 
			 (mapcar #'(lambda (option)
				     (cons option
					   (funcall (compose #'first #'(lambda (path) (map-path #'identity path))) option)))
				 options)
			 :key #'cdr)))

(defun pick-closest (first-set second-set)
  (first
   (sort 
    (mappend #'(lambda (crs1)
		 (mapcar #'(lambda (crs2) (list (station-link-distance crs1 crs2) crs1 crs2))
			 second-set))
	     first-set)
    #'<
    :key #'first)))

(defun pick-closer (set source)
  (third (pick-closest (list source) set)))

(defun pick-closest-match (from to)
  (let ((route-from (find-station-by-code (route-crs from)))
	(route-to (find-station-by-code (route-crs to))))
    (if (and (station-routeing-location-p route-from)
	     (station-routeing-location-p route-to))
	(values 'none (station-crs-code from) (station-crs-code to))
	(if (and (not (station-routeing-location-p route-from))
		 (station-routeing-location-p route-to))
	    (values 'source
		    (pick-closer (station-routeing-points from) (station-crs-code to))
		    (station-crs-code to))
	    (if (and (station-routeing-location-p route-from)
		     (not (station-routeing-location-p route-to)))
		(values 'target
			(station-crs-code from)
			(pick-closer (station-routeing-points to) (station-crs-code from)))
		(aif (pick-closest (station-routeing-points from) (station-routeing-points to))
		     (values 'both
			     (second it)
			     (third it))))))))
		    
(defun direct-display-trains (fromcrs tocrs time)
  (let ((from (find-station-by-code fromcrs))
	(to (find-station-by-code tocrs)))
    (sorted-journeys
     (mapcar #'(lambda (path)
		(list (make-display-train 
		       :from (train-from (path-state path))
		       :to (train-to (path-state path))
		       :summary (stops->info (train-stops (path-state path)) (train-from (path-state path)) (train-to (path-state path)))
		       :terminates-at (car (last (train-stops (path-state path))))
		       :stops (adjust-stops (train-stops (path-state path)) (train-from (path-state path)) (train-to (path-state path))))))
	    (direct-trains->path from to (make-time-test-fn :time time) (list from to)))
     time)))

(defun sorted-journeys (journeys time)
  (sort 
   (delete-duplicates 
    journeys
    :test #'equal
    :key #'display-journey-summary)
   (make-journey< time)
   :key #'display-journey-summary))
