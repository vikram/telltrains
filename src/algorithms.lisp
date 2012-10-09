(in-package :com.sondesh.database)

(defstruct queue (:type 'sequence)
	   length keys values removed)

(defun empty? (queue)
  (<= (queue-length queue) 0))

(defun extract-min! (queue)
  (when (not (empty? queue))
    (let ((lst nil))
      (dotimes (i 344)
	(unless (member i (queue-removed queue) :test #'=)
	  (push (cons (aref (queue-keys queue) (1+ i)) i) lst)))
      (awhen (cdr (first (sort lst #'< :key #'car)))
	(push it (queue-removed queue))
	(decf (queue-length queue))
	it))))

(defun decrease-key (queue index key) 
  (setf (aref (queue-keys queue) index) key))

;; (setf (aref dist (gethash neighbour *hubs*)) alt)

(defun waiting-time->now (hrmins now-hrmins)
  (mtimedifference-nd hrmins now-hrmins))

(defun dijkstra (source &optional (now (now)) (now-hrmins (current-hrmins now)) (without-fixed-p nil))
  (let* ((dist (make-array '(345) :element-type 'fixnum :initial-element most-positive-fixnum))
	 (previous (make-array '(345) :element-type 'sequence :initial-element nil))
	 (Q (make-queue :keys dist :length 344)))
    (setf (aref dist (gethash source *hubs*)) 0)
    (while (not (empty? Q))
      (let* ((closest-index (1+ (extract-min! Q)))
	     (closest (svref *hubs-array* closest-index)))	
	(when (or (string= closest source) (aref previous closest-index))
	  (dolist (neighbour (neighbours closest))
	    (let ((waiting-time (if (string= closest source)
				    0
				    (waiting-time->now 
				     (connection-arrival-time (aref previous closest-index))
				     now-hrmins))))
	      (awhen (elementary-connections closest neighbour 
					     (make-time-test-fn :time (if (string= closest source) 
									  now
									  (+ now (* 60 waiting-time))))
					     (if (string= closest source)
						 now-hrmins
						 (connection-arrival-time (aref previous closest-index)))
					     (aref previous closest-index))
		(let ((alt (+ (aref dist closest-index)
			      (connection-length it (aref previous closest-index) now-hrmins))))
		  (when (< alt (aref dist (gethash neighbour *hubs*)))
		    (decrease-key Q (gethash neighbour *hubs*) alt)
		    (setf (aref previous (gethash neighbour *hubs*)) it))))))
	  (unless without-fixed-p
	    (dolist (neighbour (fixed-neighbours closest))
	      (awhen (gethash (gethash neighbour *hubs*) (svref *fixed-neighbours* closest-index))
		(let ((alt (+ (aref dist closest-index)
			      (cdr it)
			      (parse-integer (station-minimum-change-time (find-station-by-code closest))))))
		  (when (< alt (aref dist (gethash neighbour *hubs*)))
		    (let ((start-time (if (string= closest source)
					  now-hrmins
					  (add-minutes
					   (connection-arrival-time (aref previous closest-index))
					   (parse-integer (station-minimum-change-time (find-station-by-code closest)))))))
		      (decrease-key Q (gethash neighbour *hubs*) alt)
		      (setf (aref previous (gethash neighbour *hubs*)) 
			    (transfer->connection closest neighbour it start-time))))))))
	  )))
    (values previous dist)))

(defun add-minutes (mtime minutes)
  (multiple-value-bind (hr min)
      (multiple-value-bind (h1 m1)
	  (floor mtime 100)
	(floor (+ (* 60 h1) (+ m1 minutes)) 60))
    (if (> hr 23)
	(+ (* 100 (- hr 24)) min)
	(+ (* 100 hr) min))))

(defun transfer->connection (from to connection start-time)
  (let ((train-id (car (find (car connection) *fixed-modes* :key #'cdr :test #'string=))))
    (list (list train-id '(1 1 1 1 1 1 1 1)
		(list (parse-integer (dttm->string (now) :format :shortyymmdd))
		      (parse-integer (dttm->string (+ 86400 86400 (now)) :format :shortyymmdd))))
	  (list train-id "I" start-time start-time from)
	  (list train-id "I" (add-minutes start-time (cdr connection)) (add-minutes start-time (cdr connection)) to))))

(defun connection-length (connection previous now-hrmins)
   (mtimedifference-nd 
    (connection-arrival-time connection)
    (if previous
	(connection-arrival-time previous)
	now-hrmins)))

(defun connection-arrival-time (connection)
  (fourth (third connection)))

(defun connection-departure-time (connection)
  (third (second connection)))

(defun connection-source (connection)
  (fifth (second connection)))

(defun connection-target (connection)
  (fifth (third connection)))

(defun shortest-journey (connections source target &optional (journey '()))
  (let ((target-index (gethash target *hubs*)))
    (awhen (aref connections target-index)
      (push it journey)
      (if (string= source (connection-source it))
	  (mapcar #'(lambda (connection)
		      (cons (list
			     (station-display-name (find-station-by-code (fifth (second connection))))
			     (station-display-name (find-station-by-code (fifth (third connection)))))
			    connection))
		  journey)
	  (shortest-journey connections source (connection-source it) journey)))))

(defun store-results (time-point some-date &key (db-p nil) (supersede t) (heaped t))
  (let ((hrmins (current-hrmins time-point))
	(date (parse-integer (dttm->string time-point :format :shortyymmdd))))
    (print (cons date hrmins) t)
    (dotimes (j 344)
      (print (list time-point j))
      (when (or supersede db-p (not (probe-file (sondesh-file-path (format nil "~A/~A-~A.gz" some-date time-point (1+ j))))))
	(let ((connections (if heaped 
			       (dijkstra-heaped (aref *hubs-array* (1+ j)) time-point hrmins)
			       (dijkstra (aref *hubs-array* (1+ j)) time-point hrmins))))
	  (if db-p
	      (with-train-database!
		(clsql:update-records-from-instance
		 (make-instance 'dijkstra
				:id (next-id)
				:source j
				:timepoint (write-to-string time-point)
				:day (weekday :time time-point)
				:trains (write-to-string connections))))
	      (progn 
		(with-open-file (stream (sondesh-file-path (format nil "~A/~A.~A" some-date time-point (1+ j))) 
					:direction :output :if-exists :supersede)
		  (print connections stream))
		(gzip-stream:gzip (sondesh-file-path (format nil "~A/~A.~A" some-date time-point (1+ j)))
				  (sondesh-file-path (format nil "~A/~A-~A.gz" some-date time-point (1+ j))))
		(delete-file (sondesh-file-path (format nil "~A/~A.~A" some-date time-point (1+ j)))))))))))

(defun process-day (some-time-in-a-day supersede &key (db-p nil))
  (let ((date (dttm->string some-time-in-a-day :format :caldate)))
    (ensure-directories-exist (sondesh-file-path (concatenate 'string date "/")))
    (dolist (i '(0 15 30 45 60 100 180 210 240 270 285 300 315 330 340 350 360 370 380 390 400 410 420
		 425 430 435 440 445 450 455 460 465 470 475 480 485 490 495 500 505 510 515
		 520 525 530 535 540 545 550 555 560 565 570 575 580 585 590 595 600 615 630
		 645 660 675 690 720 750 780 810 840 870 900 930 940 950 955 960 965 970 975 
		 980 985 990 995 1005 1010 1015 1020 1025 1030 1035 1040 1045 1050 1055 1060 1065 1075 1085 1095 1105 1115 1125
		 1135 1145 1155 1165 1175 1185 1195 1200 1230 1260 1290 1320 1350 1365 1375 1390 1405 1420 1435))
      (STORE-RESULTS (+ (string->dttm date) (* 60 i) -1) date :db-p db-p :supersede supersede))
    (write-time-points date)
    (write-hashtable (sondesh-file-path (concatenate 'string date "/routeing-uid.trains")) *uid-trains*)
    (write-hashtable (sondesh-file-path (concatenate 'string date "/routeing-train.uids")) *train-uids*)))

(defun write-time-points (date)
  (let ((timepoints (all-time-points date)))
    (with-open-file (out (sondesh-file-path (format nil "~A/timepoints" date)) 
			 :direction :output :if-exists :supersede)
      (print timepoints out))))

(defun read-time-points (date)
  (with-open-file (in (sondesh-file-path (format nil "~A/timepoints" date)))
    (read in)))
    
(defun all-time-points (date)
  (delete-duplicates
   (remove-if 
    #'not
    (mapcar #'(lambda (path)
		(let ((ans (first (split-sequence:split-sequence #\- (pathname-name path)))))
		  (awhen (parse-integer ans :junk-allowed t)
		    (if (> it 500)
			ans))))
	    (directory (make-pathname :name :wild :type :wild 
				      :defaults (sondesh-file-path (concatenate 'string date "/"))))))
   :test #'string=))

(defun journeys-between (source target date)
  (mapcar 
   #'(lambda (timepoint)
       (let ((results '()))
	 (gzip-stream:gunzip 
	  (sondesh-file-path (format nil "~A/~A-~A.gz" date timepoint (gethash source *hubs*)))
	  (sondesh-file-path (format nil "~A/~A.~A" date timepoint (gethash source *hubs*))))
	 (with-open-file (in (sondesh-file-path (format nil "~A/~A.~A" date timepoint (gethash source *hubs*))))
	   (setf results (shortest-journey (read in) source target)))
	 (delete-file (sondesh-file-path (format nil "~A/~A.~A" date timepoint (gethash source *hubs*))))
	 results))
   (read-time-points date)))

;;;; :element-type 'gzip-stream::octet)))

(defun routeing-journeys (from-crs to-crs date &optional (time (now)))
  (let ((journeys (delete-duplicates
		   (mapcar
		    #'(lambda (journey)
			(let ((results '()))
			  (dolist (stop journey)
			    (let* ((train-id (caadr stop)))
			      (if (and (first results) (= (caar (first results)) train-id))
				  (setf (cddr (first results)) (last stop))
				  (push (cdr stop) results))))
			  (reverse results)))
		    (journeys-between from-crs to-crs date))
		   :test #'equal))
	(routeing-uids (read-hashtable (sondesh-file-path (concatenate 'string date "/routeing-uid.trains")))))
    (journeys+directs 
     (mapcar
      #'(lambda (jour) 
	  (mapcar #'(lambda (train) 
		      (if (<= (caar train) 5)
			  (special-train 
			   (caar train)
			   (car (last (first (cdr train))))
			   (car (last (second (cdr train)))) 
			   train)
			  (train-details 
			   (first (decode-train 
				   (floor (first (gethash (gethash (caar train) routeing-uids) *train-uids*)) 10))))))
		  jour))
      journeys)
     (find-station-by-code from-crs)
     (find-station-by-code to-crs)
     time)))

(defun special-train (trainid from-crs to-crs train)
  (list 
   (list trainid (second (car train)) (first (third (car train))) (second (third (car train))))
   (list
    (append (butlast (first (cdr train)))
	    (list (first (station-tiploc-codes (find-station-by-code from-crs)))))
    (append (butlast (second (cdr train))) 
	    (list (first (station-tiploc-codes (find-station-by-code to-crs))))))))

(defun dijkstra-heaped (source &optional (now (now)) 
			(now-hrmins (current-hrmins now)) (without-fixed-p nil))
  (let* ((previous (make-array '(345) :element-type 'sequence :initial-element nil))
	 (Q (MAKE-HEAP 344)))
    (decreaseKey Q (gethash source *hubs*) 0)
    (while (> (heap-size Q) 0)
      (let* ((closest-node (popmin Q))
	     (closest-index (FibonacciHeapNode-userObject closest-node))
	     (closest (svref *hubs-array* closest-index)))	
	(when (or (string= closest source) (aref previous closest-index))
	  (dolist (neighbour (neighbours closest))
	    (let ((waiting-time (if (string= closest source)
				    0
				    (waiting-time->now 
				     (connection-arrival-time (aref previous closest-index))
				     now-hrmins))))
	      (awhen (elementary-connections closest neighbour 
					     (make-time-test-fn :time (if (string= closest source) 
									  now
									  (+ now (* 60 waiting-time))))
					     (if (string= closest source)
						 now-hrmins
						 (connection-arrival-time (aref previous closest-index)))
					     (aref previous closest-index))
		(let ((alt (+ (FibonacciHeapNode-priority closest-node)
			      (connection-length it (aref previous closest-index) now-hrmins))))
		  (when (< alt (dist Q (gethash neighbour *hubs*)))
		    (decreasekey Q (gethash neighbour *hubs*) alt)
		    (setf (aref previous (gethash neighbour *hubs*)) it)))))))
	  (unless without-fixed-p
	    (dolist (neighbour (fixed-neighbours closest))
	      (awhen (gethash (gethash neighbour *hubs*) (svref *fixed-neighbours* closest-index))
		(let ((alt (+ (FibonacciHeapNode-priority closest-node)
			      (cdr it)
			      (parse-integer (station-minimum-change-time (find-station-by-code closest))))))
		  (when (< alt (dist Q (gethash neighbour *hubs*)))
		    (let ((start-time (if (string= closest source)
					  now-hrmins
					  (add-minutes
					   (connection-arrival-time (aref previous closest-index))
					   (parse-integer (station-minimum-change-time (find-station-by-code closest)))))))
		      (decreaseKey Q (gethash neighbour *hubs*) alt)
		      (setf (aref previous (gethash neighbour *hubs*)) 
			    (transfer->connection closest neighbour it start-time))))))))))
    previous))

(defun gzip-folder (date)
  (dolist (timepoint (read-time-points date))
    (dotimes (i 344)
      (gzip-stream:gzip (sondesh-file-path (format nil "~A/~A.~A" date timepoint (1+ i)))
			(sondesh-file-path (format nil "~A/~A-~A.gz" date timepoint (1+ i))))
      (delete-file (sondesh-file-path (format nil "~A/~A.~A" date timepoint (1+ i)))))))

(defun combine-crs-files (dates source)
  (dolist (date dates)
    (with-open-file (out (sondesh-file-path (format nil "~A/~A" date (gethash source *hubs*)))
			 :direction :output :if-exists :supersede)
      (dolist (timepoint (read-time-points date))
	(gzip-stream:gunzip (sondesh-file-path (format nil "~A/~A-~A.gz" date timepoint (gethash source *hubs*)))
			    (sondesh-file-path (format nil "~A/~A.~A" date timepoint (gethash source *hubs*))))
	(with-open-file (in (sondesh-file-path (format nil "~A/~A.~A" date timepoint (gethash source *hubs*))))
	  (write (cons timepoint (read in)) :stream out))
	(delete-file (sondesh-file-path (format nil "~A/~A.~A" date timepoint (gethash source *hubs*))))))
    (gzip-stream:gzip (sondesh-file-path (format nil "~A/~A" date (gethash source *hubs*)))
		      (sondesh-file-path (format nil "~A/~A.gz" date (gethash source *hubs*))))
    (delete-file (sondesh-file-path (format nil "~A/~A" date (gethash source *hubs*))))))

(defun dijkstra-files->db (date)
  (let ((uids (read-hashtable 
	       (sondesh-file-path (format nil "~A/routeing-uid.trains" date))))
	(timepoints (read-time-points date)))
    (dotimes (i 344)
      (print (cons date (1+ i)))
      (with-train-database!
	(dolist (timepoint timepoints)
	  (gzip-stream:gunzip (sondesh-file-path (format nil "~A/~A-~A.gz" date timepoint (1+ i)))
			      (sondesh-file-path (format nil "~A-~A.~A" date timepoint (1+ i))))
	  (let ((inputs (with-open-file (in (sondesh-file-path (format nil "~A-~A.~A" date timepoint (1+ i))))
			  (read in)))
		(results (make-array 345)))
	    (dotimes (j 344)
	      (awhen (aref inputs (1+ j))
		(if (> (caar it) 5)
		    (if (or (gethash (caar it) uids)
			    (gethash (caar it) *uid-trains*))
			(setf (aref results (1+ j))
			      (cons (or (gethash (caar it) uids)
					(gethash (caar it) *uid-trains*))
				    (gethash (car (last (second it))) *hubs*)))
			(print (list 'not (caar it) date)))
		    (setf (aref results (1+ j))
			  (cons (cdr (find (caar it) *fixed-modes* :key #'car :test #'=))
				(gethash (car (last (second it))) *hubs*))))))
	    (clsql:update-records-from-instance 
	     (make-instance 'dijkstra
			    :id (next-id)
			    :source (1+ i)
			    :timepoint timepoint
			    :day (weekday :time (read-from-string timepoint))
			    :trains (write-to-string results))))
	  (delete-file (sondesh-file-path (format nil "~A-~A.~A" date timepoint (1+ i)))))))))

(defun find-saved-trains-db (from to &optional (time (now)))
  (nreverse
   (filter-adjacent 
    (delete-duplicates 
     (remove-if #'not
		(mapcar
		 #'(lambda (journey)
		     (let ((summary (funcall (journey-pos time) (display-journey-summary journey))))
		       (if (reasonable-change-window (second summary))
			   (cons summary journey)
			   nil)))
		 (routeing-journeys-db from to time)))
     :test #'equal
     :key (compose #'second #'first)))))

(defun cleanup-saved-trains-db (trains &optional (time (now)))
  (nreverse
   (filter-adjacent 
    (delete-duplicates 
     (remove-if #'not
		(mapcar
		 #'(lambda (journey)
		     (let ((summary (funcall (journey-pos time) (display-journey-summary journey))))
		       (if (reasonable-change-window (second summary))
			   (cons summary journey)
			   nil)))
		 trains))
     :test #'equal
     :key (compose #'second #'first)))))

(defun shortest-journey-db (connections source-index target-index &optional (journey '()))
  (awhen (aref connections target-index)
    (when (listp it)
      (push (cons (car it) target-index) journey)
      (if (= source-index (cdr it))
	  journey
	  (shortest-journey-db connections source-index (cdr it) journey)))))

(defun same-as-previous-p (prev train)
  (and prev (> (caaar prev) 5)
       (= (caaar prev) (caaar train))))
	   
(defun better-than-previous-p (prev-display train)
  (let ((start (first (display-train-stops prev-display)))
	(stops (mapcar #'fifth (cadar train))))
    (all-members (list (fifth start)) stops :test #'string=)))

(defun make-train-other (train start end)
  (let ((summary (stops->info (car (rest (car train))) start end)))
    (make-display-train 
     :from start
     :to end
     :summary (if (> (caaar train) 5)
		  summary
		  (append (butlastn summary 2) (list (string-capitalize (fixed-index->mode (caaar train))) (sixth summary))))
     :stops (adjust-stops (car (last (car train))) start end)
     :terminates-at (car (last (car (rest (car train))))))))

(defun journeys+directs-db (journeys from to time)
  (delete-duplicates
     (sort 
      (remove-if #'not 
		 (mapcar #'(lambda (journey)
			     (let ((prev nil)
				   (result nil))
			       (dolist (train journey)
				 (let ((display (make-train-other
						 (if (numberp (cdr train))
						     train
						     (car train))
						 (if (or (same-as-previous-p prev train)
							 (and (car result) (better-than-previous-p (car result) train)))
						     (display-train-from (car result))
						     (if prev
							 (find-station-by-code 
							  (if (numberp (cdr prev))
							      (aref *hubs-array* (cdr prev))
							      (cdr prev)))
							 from))
						 (find-station-by-code 
						  (if (numberp (cdr train))
						      (aref *hubs-array* (cdr train))
						      (cdr train))))))
				   (if (or (same-as-previous-p prev train)
					   (and (car result) (better-than-previous-p (car result) train)))
				       (setf result (cons display (cdr result)))
				       (push display result))
				   (setf prev train)))
			       (nreverse result)))
			(if journeys
			    journeys
			    (mapcar #'(lambda (jour) (list
						      (cons 
						       (list 
							(list
							 (train-info (path-state jour))
							 (train-stops (path-state jour))))
						       (station-crs-code to))))
				    (direct-trains->path from to (make-time-test-fn :time time) (list from to))))))
      (make-journey< time)
      :key #'display-journey-summary)
     :test #'(lambda (j1 j2)
	       (equal (mapcar #'display-train-summary j1)
		      (mapcar #'display-train-summary j2)))))

(defun routeing-journeys-db (from to time &key (ignore-shortest-p nil))
  (if (and (position (station-crs-code from) *all-crs-preprocessed* :test #'string=)
	   (position (station-crs-code to) *all-crs-preprocessed* :test #'string=)
	   (not ignore-shortest-p))
      (decode-journeys (station-crs-code from) (station-crs-code to) (weekday :time time))
      (journeys+directs-db
       (remove-if
	#'not
	(mapcar
	 (expand-journey from to time)
	 (accumulate-journeys-db-routed from to time)))
       from
       to
       time)))

(defun expand-journey (from to time)
  #'(lambda (jour-src)  
      (let* ((timepoint (cdr jour-src))
	     (jour-source (car jour-src))
	     (source (cdr jour-source))
	     (jour (car jour-source))
	     (trs (mapcar 
		  #'(lambda (train) 
		      (if (member (car train) '("TUBE" "WALK" "TRANSFER" "METRO" "BUS") :test #'string=)
			  train
			  (awhen (train-uid-details (car train) :timepoint timepoint)
			    (if (stn-in-stops (find-station-by-code (aref *hubs-array* (cdr train))) (second it))
				(cons it (cdr train))
				nil))))
		  jour)))
	(if (not (zerop (count nil trs)))
	    (progn
	      (log-error from to time jour trs)
	      nil)
	    (populate-fixed trs
			    source
			    (gethash (station-crs-code to) *hubs*)
			    (subst-date->time (dttm->string time :format :date) (read-from-string timepoint)))))))

(defun train-uid-details (uid &key (timepoint nil))
  (awhen (gethash uid *train-uids*)
    (let* ((trains (mapcar 
		   #'(lambda (code)
		       (train-details 
			(first (decode-train 
				(floor code 10)))))
		   it))
	   (results (remove-if-not #'(lambda (train) 
				       (if timepoint
					   (or (train-valid-p (first train) 
							      :time (read-from-string timepoint))
					       (train-valid-p (first train) 
							      :time (+ 86400 (read-from-string timepoint))))
					   train))
				   trains)))
      (values (first results)
	      (rest results)))))

(defun arrival-time (train dest)
  (awhen (find (station-tiploc-codes (find-station-by-code (aref *hubs-array* dest)))
	       (cadr (car train))
	       :test #'(lambda (stops stop) (member (fifth stop) stops :test #'string=)))
    (if (string= "O" (second it))
	(third it)
	(fourth it))))

(defun fixed-journey (source-index target-index)
  (gethash target-index (svref *fixed-neighbours* source-index)))

(defun populate-fixed (journeys from-index to-index time &optional (prev nil) (results nil))
  (if (not journeys)
      (reverse results)
      (if (not (stringp (car (first journeys)))) ;;not-fixed
	  (populate-fixed (rest journeys) from-index to-index time (first journeys) (cons (first journeys) results))
	  (let* ((trainid (fixed-mode->index (car (first journeys))))
		 (start (if prev
			    (add-minutes 
			     (arrival-time prev (cdr prev))
			     (parse-integer 
			      (station-minimum-change-time 
			       (find-station-by-code (aref *hubs-array* (cdr (first journeys)))))))
			    (current-hrmins time)))
		 (end (add-minutes 
		       start
		       (if prev
			   (cdr (fixed-journey (cdr prev) (cdr (first journeys))))
			   (cdr (fixed-journey from-index (cdr (first journeys)))))))
		 (this (cons 
			(list 
			 (list trainid '(1 1 1 1 1 1 1 1) 70101 91231)
			 (list 
			  (list trainid "I" start start
				(first (station-tiploc-codes (find-station-by-code (aref *hubs-array* 
											 (if prev
											     (cdr prev)
											     from-index))))))
			  (list trainid "I" end end
				(first (station-tiploc-codes (find-station-by-code (aref *hubs-array* 
											 (cdr (first journeys)))))))))
			(cdr (first journeys)))))
	    (populate-fixed (cdr journeys) from-index to-index time this (cons this results))))))

(defun accumulate-journeys-db (from to time)
  (delete-duplicates
   (mappend 
    #'(lambda (result)
	(remove-if
	 #'not
	 (mapcar #'(lambda (crs) 
		     (let ((journey (shortest-journey-db 
				     (read-from-string (fourth result)) 
				     (first result) 
				     (gethash crs *hubs*))))
		       (if (minor-station-start-end-p journey from to)
			   nil
			   (cons (cons journey (first result)) (second result)))))
		 (station-all-crs-codes to))))
    (with-train-database
      (clsql:query 
       (format nil
	       "select source,timepoint,day,trains from DIJKSTRA where (~{source=~A~^ or ~}) and day=~A order by source,day,timepoint"
	       (mapcar #'(lambda (crs) (gethash crs *hubs*)) (station-all-crs-codes from))
	       (weekday :time time)))))
   :test #'equal))

(defun minor-station-start-end-p (journey from to)
  (or (intersection (mapcar #'cdr journey)
		    (mapcar #'(lambda (crs) (gethash crs *hubs*)) (station-all-crs-codes from))
		    :test #'=)
      (intersection (butlast (mapcar #'cdr journey))
		    (mapcar #'(lambda (crs) (gethash crs *hubs*)) (station-all-crs-codes to))
		    :test #'=)))

(defun journey-length (journey source target timepoint)
  (let* ((from (find-station-by-code (aref *hubs-array* source)))
	 (to (find-station-by-code (aref *hubs-array* target)))
	 (jour (car (journeys+directs-db
		     (list (funcall (expand-journey from to timepoint) journey))
		     from to timepoint))))
    (when jour
      (timedifference-nd (fourth (display-train-summary (car (last jour))))
			 (dttm->string timepoint :format :time)))))

(defun dijkstra->shortest-db ()
  (dotimes (i 344)
    (let ((source (1+ i)))
      (when (> source 189)
	(dotimes (day 7)
	  (let ((timepoint-connections
		 (with-train-database
		   (clsql:query (format 
				 nil 
				 "select timepoint,trains from DIJKSTRA 
                                  where source = ~A and day=~A order by timepoint"
				 source day)))))
	    (dotimes (j 344)
	      (let ((target (1+ j)))
		(when (/= target source)
		  (with-train-database! 
		    (let ((journeys 					    
			   (mapcar 
			    #'(lambda (timepoint-connection) 
				(let ((connections (read-from-string (second timepoint-connection)))
				      (timepoint (car timepoint-connection)))
				  (cons (shortest-journey-db connections source target) timepoint)))
			    timepoint-connections)))
		      (clsql:update-records-from-instance
		       (make-instance 'shortest
				      :id (next-id)
				      :source source
				      :target target
				      :day day
				      :journeys (write-to-string journeys)))
		      (print (list source target day (dttm->string (now)))))))))))))))
	  
(defun init-dijkstra (time-of-day)
  (let ((date (dttm->string time-of-day :format :caldate)))
    (small-init)
    (init :only-routeing t :only-next-two-days-p date :start-date date :end-date (dttm->string (+ (* 2 86400) time-of-day) :format :caldate))))

(defun repopulate-dijkstra (time-of-day)
  (let* ((day (weekday :time time-of-day))
	 (max (* 10
		 (ceiling (or (caar (with-train-database
				  (clsql:query "select max(day) from DIJKSTRA"))) 0) 10))))
    (init-dijkstra time-of-day)
    (with-train-database!
      (clsql:execute-command (format nil "update DIJKSTRA set day=~A where day = ~A" (+ max 1) day)))
    (process-day time-of-day t)))

(defun cleanup-trains-folder (date)
  (dolist (timepoint (read-time-points date))
    (dotimes (i 344)
      (delete-file (sondesh-file-path (format nil "~A/~A-~A.gz" date timepoint (1+ i)))))))

(defun redo-preprocessed (&optional (time (string->dttm *start-date*)))
  (dolist (i '(0 1 2 3 4 5 6))
    (repopulate-dijkstra (+ time (* 86400 i)))
    (clear-all-connections)
    (dijkstra-files->db (dttm->string (+ time (* 86400 i)) :format :caldate))
    (cleanup-trains-folder (dttm->string (+ time (* 86400 i)) :format :caldate))))

(defun determine-routes ()
  (with-open-file (out (sondesh-file-path "routes") :direction :output :if-does-not-exist :create :if-exists :append)
    (dolist (source 
	      (remove-if-not 
	       #'(lambda (grp)
		   (member (station-crs-code grp) '("G56") :test #'string=))
	       *station-groups*))
      (dolist (target (remove-if #'(lambda (x) (equal source x)) *station-groups*))
	(dolist (day '(0 1 2 3 4 5 6))
	  (awhen (delete-duplicates 
		  (mapcar #'(lambda (journey)
			      (cons (gethash (station-crs-code 
					      (first (find-station-by-letters (first (display-train-summary (first (cdr journey))))))) *hubs*)
				    (gethash (station-crs-code (display-train-to (car (last (cdr journey))))) *hubs*)))
			  (find-trains source target (+ 3398371200 (* day 86400))))
		  :test #'equal)
	    (print (list (station-crs-code source) (station-crs-code target) day it) out)
	    (print (list (station-crs-code source) (station-crs-code target) day it))))))))

(defun reduce-crs-indexes (lst)
  (reduce #'(lambda (a b)
	      (if a
		  (cons
		   (if (not (member (car b) (car a) :test #'=))
		       (cons (car b) (car a))
		       (car a))
		   (if (not (member (cdr b) (cdr a) :test #'=))
		       (cons (cdr b) (cdr a))
		       (cdr a)))
		  b))
	  lst
	  :initial-value '(() . ())))

(defun accumulate-journeys-db-routed (from to time)
  (let* ((day (weekday :time time))
         (starts (or (mapcar #'car (gethash (route-key (station-crs-code from) (station-crs-code to) day) *routes*))
                     (mapcar #'(lambda (crs) (gethash crs *hubs*)) (station-all-crs-codes from))))
         (ends (or (mapcar #'cdr (gethash (route-key (station-crs-code from) (station-crs-code to) day) *routes*))
                   (mapcar #'(lambda (crs) (gethash crs *hubs*)) (station-all-crs-codes to)))))
    (delete-duplicates
     (mappend 
      #'(lambda (result)
          (remove-if
           #'not
           (mapcar #'(lambda (crs-index) 
                       (let ((journey (shortest-journey-db 
                                       (read-from-string (fourth result)) 
                                       (first result) 
                                       crs-index)))
                         (if (minor-station-start-end-p journey from to)
                             nil
                             (cons (cons journey (first result)) (second result)))))
                   ends)))
      (with-train-database
	(clsql:query (format nil
			     "select source,timepoint,day,trains from DIJKSTRA 
                              where (~{source=~A~^ or ~}) and day=~A order by source,day,timepoint"
			     starts
			     day))))
     :test #'equal)))

(defun move-connections (&key (start-from nil))
  (dolist (source-targets (if start-from
			      (member start-from (combinations *all-crses*) :key #'car :test #'string=)
			      (combinations *all-crses*)))
    (let ((source (car source-targets)))
      (dolist (target (cadr source-targets))
	(dotimes (day 7)
	  (with-train-database!
	    (clsql:update-records-from-instance
	     (make-instance 'shortest
			    :id (next-id)
			    :source source
			    :target target
			    :day day
			    :journeys (encoded-journeys source target day))))
	  (print (list source target day)))))))

(defun move-connections-in-group (&key (start-from nil))
  (let ((combos (combinations
		 '("BMO" "BHM" "BSW" "NOT" "BAN" "LAN" "PMH" "PMS" "DBY" "BFR" "CST" "CHX" "EUS"
		   "FST" "KGX" "LST" "LBG" "MYB" "PAD" "STP" "VIC" "WAT" "WAE" "BMS" "WHC" "WMW"
		   "FNB" "FNN" "BLY" "BDQ" "BDI" "BTN" "BPW" "BRI" "PFR" "PFM" "POT" "CTM" "GCR"
		   "LDS" "EDB" "MCO" "MAN" "MCV" "HUD" "DOC" "PLY" "SHF" "DKG" "DPD" "DKT" "MCN"
		   "WOF" "WOS" "MBR" "RDH" "ECR" "LVJ" "LVC" "LIV" "LVS" "EXC" "EXD" "EXT" "FKG"
		   "FKK" "FOD" "RMF" "GLC" "GLQ" "ASF" "SWA" "HKC" "HAC" "HFX" "WML" "HGS" "HLD"
		   "SVG" "MTH" "HNX" "INK" "KDY" "LEW" "LBO" "MDB" "MDE" "MDW" "SOU" "SOA" "RAM"
		   "NCT" "NNG" "STE" "SRC" "SRH" "RDG" "RDW" "RUN" "RUE" "SCA" "STO" "TOM" "SMR"
		   "SGB" "SOC" "SOE" "SOV" "SST" "SSD" "WSB" "WEY" "WKK" "WKF" "WBQ" "WAC" "WHD"
		   "WHP" "WGN" "WGW" "WXC" "WRX" "YVJ" "YVP")))
	(additionals '("ABD" "ACT" "AFK" "AHV" "BBN" "BDM" "BFD" "BGN" "BKG" "BKJ" "BNY" "BON" "BSK"
		       "BTB" "BTG" "BTH" "CAR" "CBG" "CDF" "CHD" "CLJ" "CLY" "COL" "COV" "CPM" "CRE"
		       "CRS" "CRV" "CTR" "CYP" "DAR" "DEE" "DFD" "DID" "DON" "DVP" "ELY" "EPS" "ERL"
		       "ESL" "FAV" "FPK" "GFD" "GLD" "GOO" "GRA" "GRP" "GTW" "GUI" "HAV" "HAZ" "HFD"
		       "HHY" "HNH" "HOO" "HRH" "HUL" "HUY" "INV" "IPS" "KET" "KMK" "KNG" "LCN" "LDY"
		       "LEI" "LWS" "MIJ" "MMO" "NCL" "NMP" "NRW" "NTN" "NUN" "NWP" "OKM" "OXF" "PBO"
		       "PMR" "PRE" "PTH" "PYG" "RET" "RGL" "RMD" "RUG" "SAL" "SAY" "SBY" "SHR" "SHT"
		       "SLO" "SLR" "SMK" "SNS" "SOT" "SPT" "SRA" "STG" "SUO" "SUR" "SWI" "SYB" "TAU"
		       "TBD" "TON" "TUH" "WFJ" "WIJ" "WIM" "WKM" "WOK" "WVH" "YRK"
		       "G02" "G43" "G58" "G64" "G23" "G09" "G01" "G06" "G55" "G45" "G62" "G03" "G04"
		       "G05" "G22" "G07" "G14" "G16" "G11" "G20" "G40" "G21" "G25" "G10" "G41" "G35"
		       "G42" "G60" "G08" "G18" "G12" "G44" "G61" "G37" "G13" "G38" "G57" "G39" "G69"
		       "G15" "G53" "G28" "G67" "G70" "G50" "G51" "G17" "G52" "G19" "G26" "G68" "G47"
		       "G29" "G24" "G71" "G48" "G54" "G63" "G27" "G66" "G46" "G33" "G30" "G31" "G32"
		       "G34" "G49" "G36")))
  (dolist (source-targets (if start-from
			      (member start-from combos :key #'car :test #'string=)
			      combos))
    (let ((source (car source-targets)))
      (dolist (target (remove-if #'(lambda (crs) (string= (station-group-crs-code (find-station-by-code crs))
							  (station-group-crs-code (find-station-by-code source))))
				 (union (cadr source-targets)
					additionals :test #'string=)))
	(dotimes (day 7)
	  (with-train-database!
	    (clsql:update-records-from-instance
	     (make-instance 'shortest
			    :id (next-id)
			    :source source
			    :target target
			    :day day
			    :journeys (encoded-journeys source target day))))
	  (print (list source target day))))))))

#|
(remove-if #'(lambda (crs)
				     (string= (station-group-crs-code (find-station-by-code crs))
					      (station-group-crs-code (find-station-by-code source))))
				 (cadr source-targets)))
|#

(defparameter *all-crs-preprocessed* 
  '("G36" "G49" "G34" "G32" "G31" "G30" "G33" "G46" "G66" "G27" "G63" "G54" "G48"
 "G71" "G24" "G29" "G47" "G68" "G26" "G19" "G52" "G17" "G51" "G50" "G70" "G67"
 "G28" "G53" "G15" "G69" "G39" "G57" "G38" "G13" "G37" "G61" "G44" "G12" "G18"
 "G08" "G60" "G42" "G35" "G41" "G10" "G25" "G21" "G40" "G20" "G11" "G16" "G14"
 "G07" "G22" "G05" "G04" "G03" "G62" "G45" "G55" "G06" "G01" "G09" "G23" "G64"
 "G58" "G43" "G02" "YRK" "WVH" "WOK" "WKM" "WIM" "WIJ" "WFJ" "TUH" "TON" "TBD"
 "TAU" "SYB" "SWI" "SUR" "SUO" "STG" "SRA" "SPT" "SOT" "SNS" "SMK" "SLR" "SLO"
 "SHT" "SHR" "SBY" "SAY" "SAL" "RUG" "RMD" "RGL" "RET" "PYG" "PTH" "PRE" "PMR"
 "PBO" "OXF" "OKM" "NWP" "NUN" "NTN" "NRW" "NMP" "NCL" "MMO" "MIJ" "LWS" "LEI"
 "LDY" "LCN" "KNG" "KMK" "KET" "IPS" "INV" "HUY" "HUL" "HRH" "HOO" "HNH" "HHY"
 "HFD" "HAZ" "HAY" "HAV" "GUI" "GTW" "GRP" "GRA" "GOO" "GLD" "GFD" "RUN" "RUE"
 "HNX" "LPY" "HTH" "HDG" "SYA" "WML" "MIA" "MSR" "RAM" "HLY" "MTH" "SST" "SSD"
 "BAR" "CNF" "LAN" "SMR" "SGB" "BLY" "MKC" "FOD" "LIT" "ELD" "RDH" "BAN" "KGS"
 "HKC" "HAC" "WEH" "BHO" "WHC" "WMW" "SVS" "STO" "TOM" "HLD" "LPR" "LBO" "BWS"
 "SIL" "KDY" "MNC" "GLT" "INK" "NQU" "WXC" "WRX" "SCA" "SEM" "NCT" "NNG" "TRO"
 "WSB" "BAW" "FNB" "FNN" "FKG" "FKK" "CMO" "ATB" "BEE" "NOT" "EAG" "MBR" "TBY"
 "DVY" "MCN" "DHN" "HUD" "MIR" "HFX" "SOW" "GWN" "LLE" "SWA" "GDP" "RMF" "YVJ"
 "YVP" "DTW" "WOF" "WOS" "WGN" "WGW" "UPW" "WEY" "WHD" "WHP" "WBQ" "WAC" "WKK"
 "WKF" "NRB" "SRS" "STE" "SRC" "SRH" "TTH" "HIT" "SVG" "SOC" "SOE" "SOV" "MBK"
 "RDB" "SOU" "SOA" "SDN" "SWG" "DOR" "SHF" "MHS" "RDG" "RDW" "BDH" "CSA" "FRM"
 "FTN" "HLS" "PTC" "PMH" "PMS" "CFD" "PFR" "PFM" "POT" "GLH" "DPT" "DOC" "KEY"
 "PLY" "DGT" "MCO" "MAN" "MCV" "SFD" "SLD" "MDB" "MDE" "MDW" "EDG" "BKQ" "LVJ"
 "LVC" "LIV" "MRF" "SDL" "LVS" "LEW" "NWX" "SAJ" "CRG" "EGF" "GRF" "LDS" "MIK"
 "HGS" "SLQ" "CNM" "GCR" "GLC" "GLQ" "SPR" "ASF" "EXC" "EXD" "EXT" "SJP" "DAM"
 "EDB" "HYM" "SGL" "DKG" "DPD" "DKT" "GOM" "BLP" "DBY" "DFI" "LGE" "PEA" "SPO"
 "ECR" "SCY" "WCY" "CTM" "GLM" "RTR" "SOO" "BKL" "BMS" "ORP" "PET" "SRT" "BPW"
 "BRI" "LWH" "SRD" "FIT" "BTN" "HOV" "BDQ" "BDI" "AST" "BMO" "BHM" "BSW" "DUD"
 "BET" "BFR" "CST" "CHX" "EUS" "FST" "KGX" "KCM" "LST" "LBG" "MYB" "ZMG" "OLD"
 "PAD" "STP" "VIC" "WAT" "WAE" "CTK" "FPK" "FAV" "ESL" "ERL" "EPS" "ELY" "DVP"
 "DON" "DID" "DFD" "DEE" "DAR" "CYP" "CTR" "CRV" "CRS" "CRE" "CPM" "COV" "COL"
 "CLY" "CLJ" "CHD" "CDF" "CBG" "CAR" "BTH" "BTG" "BTB" "BSK" "BON" "BNY" "BKJ"
 "BKG" "BGN" "BFD" "BDM" "BBN" "AHV" "AFK" "ACT" "ABD"))

(defun encoded-journeys (source target day)
  (format nil "(~{(~{~A~^ ~})~})" 
	  (mapcar #'(lambda (journey)
		      (mapcar #'(lambda (train) 			  
				  (encode-journey-train
				   (position (station-crs-code (display-train-from train)) *ALL-CRS-PREPROCESSED* :test #'string=)
				   (position (station-crs-code (display-train-to train)) *ALL-CRS-PREPROCESSED* :test #'string=)
				   (first (display-train-terminates-at train))
				   (short-time->hrmins (second (display-train-summary train)))
				   (short-time->hrmins (fourth (display-train-summary train)))))
			      (cdr journey)))
		  (find-trains-to-encode (find-station-by-code source)
					 (find-station-by-code target)
					 (+ 3404422800 (* 86400 (+ 7 day)))))))

(defun encode-journey-train (from-crs-index to-crs-index trainid start end)
  (+ (* 100000000000000000 from-crs-index)
     (* 100000000000000 to-crs-index)
     (* 100000000 trainid)
     (* 10000 start)
     end))

(defun decode-journey-train (encoded)
  (multiple-value-bind (a b)
      (floor encoded 10000)
    (multiple-value-bind (c d)
	(floor a 10000)
      (multiple-value-bind (e f)
	  (floor c 1000000)
	(multiple-value-bind (g h)
	    (floor e 1000)
	  (list g h f d b))))))

(defun hrmins->short-time (hrmins)
  (multiple-value-bind (hr min) (floor hrmins 100)
    (hrmin->short-time hr min)))

(defun make-display-train<-trainid (trainid start end)
  (let ((stops (cadr (train-details trainid))))
    (make-display-train
     :from start
     :to end
     :summary (stops->info stops start end)
     :stops (adjust-stops stops start end)
     :terminates-at (car (last stops)))))

(defun decode-journeys (source target day)
  (mapcar 
   #'(lambda (journey)
       (mapcar #'(lambda (train)
		   (let* ((decoded-train (decode-journey-train train))
			  (source (first decoded-train))
			  (target (second decoded-train))
			  (trainid (third decoded-train))
			  (st (fourth decoded-train))
			  (et (fifth decoded-train))
			  (train-details (train-details trainid))
			  (start (find-station-by-code (nth source *all-crs-preprocessed*)))
			  (end (find-station-by-code (nth target *all-crs-preprocessed*)))
			  (stops (car (rest train-details))))
		     (if (> 6 trainid)
			 (let* ((s (first (station-tiploc-codes start)))
				(e (first (station-tiploc-codes end))))
			   (make-display-train
			    :from start
			    :to end
			    :summary (list (station-display-name start)
					   (hrmins->short-time st)
					   (station-display-name end)
					   (hrmins->short-time et)
					   "Tube"
					   (hrmins->short-time (mtimedifference-nd et st)))
			    :stops (list (list trainid "I" st st s)
					 (list trainid "I" et et e))
			    :terminates-at (list trainid "I" et et e)))
			 (make-display-train
			  :from start
			  :to end
			  :summary (stops->info stops start end)
			  :stops (adjust-stops stops start end)
			  :terminates-at (car (last stops))))))
	       journey))
   (with-train-database
     (read-from-string 
      (caar 
       (clsql:query 
	(format nil "select journeys from SHORTEST where day=~A and source='~A' and target='~A'" day source target)))))))

