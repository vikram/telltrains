(in-package :com.sondesh.database)

(defun train->display (trainid from-crs to-crs &optional (start nil) (end nil))
  (let* ((from (find-station-by-code from-crs))
	 (to (find-station-by-code to-crs))
	 (stops 
	  (if (<= trainid 5)
	      (list
	       (list trainid "O" start 0 (first (station-tiploc-codes from)))
	       (list trainid "T" 0 end (first (station-tiploc-codes to))))
	      (car (rest (train-details trainid))))))
    (make-display-train 
     :from from
     :to to
     :summary (stops->info stops from to)
     :stops (adjust-stops stops from to)
     :terminates-at (car (last stops)))))

(defun trains->location-list (display-trains)
  (let ((i 0))
    (mapcar #'(lambda (tiploc) 
		(progn
		  (incf i)
		  (cons tiploc i)))
	    (delete-duplicates (flatten (mapcar (compose #'(lambda (stops) (mapcar #'fifth stops)) #'display-train-stops) 
						display-trains)) :test #'string=))))

(defun trains->locations (display-trains dest)
  (format nil "~{~A~^ , ~}"
	  (mapcar #'(lambda (stop)
		      (let ((stn (find-station-by-code (gethash (car stop) *tiploc->crs*))))
			(format 
			 nil 
			 "{simpleContent: '~A',maximizedContent: '',latitude: ~A,longitude: ~A,zoom: ~A,icon: '/images/station.gif'}"
			 (cdr stop)
			 (station-latitude stn) 
			 (station-longitude stn)
			 (if (string= (station-crs-code stn) (station-crs-code dest)) 15 10))))
		  (consolidate-stops (mappend #'display-train-stops display-trains)))))

(defun consolidate-stops (stops)
  (reverse (consolidate-stop (first stops) (rest stops))))

(defun consolidate-stop (stop rest &optional acc)
  (if rest
      (if (string= (fifth stop) (fifth (first rest)))
	  (consolidate-stop (second rest) (nthcdr 2 rest)
			    (cons  
			     (cons (fifth stop) 
				   (format nil "Arrives ~A at ~A, change here leave at ~A"
					   (station-display-name (find-station-by-code (gethash (fifth stop) *tiploc->crs*)))
					   (time-ampm (multiple-value-bind (hr min)
							  (floor (fourth stop) 100)
							(format nil "~A:~A" hr min)))
					   (time-ampm (multiple-value-bind (hr min)
							  (floor (third (first rest)) 100)
							(format nil "~A:~A" hr min)))))
			     acc))
	  (consolidate-stop (first rest) (rest rest)
			    (cons
			     (cons (fifth stop) 
				   (format nil (if acc "Arrives ~A at ~A" "Leaves ~A at ~A")
					   (station-display-name (find-station-by-code (gethash (fifth stop) *tiploc->crs*)))
					   (time-ampm (multiple-value-bind (hr min)
							  (floor (if acc (fourth stop) (third stop)) 100)
							(format nil "~A:~A" hr min)))))
			     acc)))
      (if stop
	  (cons 
	   (cons (fifth stop) (format nil "Arrives ~A at ~A"
				      (station-display-name (find-station-by-code (gethash (fifth stop) *tiploc->crs*)))
				      (time-ampm (multiple-value-bind (hr min)
						     (floor (fourth stop) 100)
						   (format nil "~A:~A" hr min)))))
	   acc)
	  acc)))

(defun journey-stops ()
  (aif (remove-if
	#'not
	(mapcar #'(lambda (j)
		    (let ((t-c (split-sequence:split-sequence #\; j)))
		      (when (= (length (second t-c)) 14)
			(train->display (parse-integer (first t-c)) 
					(subseq (second t-c) 0 3) 
					(subseq (second t-c) 3 6)
					(parse-integer (subseq (second t-c) 6 10))
					(parse-integer (subseq (second t-c) 10 14))))))
		(split-sequence:split-sequence #\: (get-parameter "journey"))))
       (stops->div it)
       (with-html-output-to-string (*standard-output* nil :prologue nil)
	 (:body
	  (fmt "~A" "Sorry details of this journey are not available. Please try the search ")
	  (:a :href "/" (fmt "~A" "again"))
	  (fmt "~A" ".")))))

(defun stops->div (display-trains)
  (let* ((tiploc-loc (trains->location-list display-trains))
	 (to-tiploc (fifth (car (last (display-train-stops (car (last display-trains)))))))
	 (to (find-station-by-code (gethash to-tiploc *tiploc->crs*)))
	 (map (if *prod*
		  "http://maps.google.com/maps?file=api&v=2&key=ABQIAAAAfzASmKvmIGhgVZBphgbIMhSbOCASmIHfcyCoxgLWrOePWQVFNhScF6BwHPam3my7bT67NmWM1eTM4g" 
		  "http://maps.google.com/maps?file=api&v=2&key=ABQIAAAAfzASmKvmIGhgVZBphgbIMhR6tmuByN1FdjigMD8V6EroZwPzZhTLlWSerUDOwnHIKScDxbaZSSKvNA")))
    (with-html-output-to-string (*standard-output* nil :prologue nil)
      (:html
       (:head
	(:link :type "text/css" :rel "stylesheet" :href *sitecss*)
	(:script :type "text/javascript" :src *sitejs*)
	(:script :src map :type "text/javascript")
	(:script :src "http://www.google-analytics.com/urchin.js" :type "text/javascript")
	(:script :type "text/javascript"
		       "_uacct = 'UA-1779260-1';
                        urchinTracker();"))
       (:body :style "overflow: hidden;"
	      (:div :id "map" :style "height: 400px; width: 400px; float :left;")
	      (:div :id "sidebar" :style "width: 220px; height: 400px; float: right; overflow-y: auto;"
		    (:table :class "stops" 
			    (loop for i from 0
			          for (display-train summary start) in (mapcar #'(lambda (display-train)
										   (list display-train
											 (display-train-summary display-train)
											 (first (display-train-stops display-train))))
									       display-trains)
			       do (htm
				   (:tbody
				    (:tr :class "selected"
					 (:td (:a :class "map" 
						  :id (trim (fifth start))
						  :tabindex (cdr (assoc (fifth start) tiploc-loc :test #'string=))
						  (fmt "~A" (string-capitalize (first summary)))))
					 (:td (fmt "~A" (time-ampm (second summary)))))
				    (loop for j from i
				       for stop in (rest (display-train-stops display-train))
				       do (htm
					   (:tr :class (if (oddp j) "odd" "even")
						(:td
						 (:a :class "map" 
						     :id (trim (fifth stop)) 
						     :tabindex (cdr (assoc (fifth stop) tiploc-loc :test #'string=)) 
						     :href "#"
						     (fmt "~A" (string-capitalize (tiploc->station-name (fifth stop))))))
						(:td
						 (fmt "~A" (time-ampm (multiple-value-bind (hr min)
									  (floor (fourth stop) 100)
									(format nil "~A:~A" hr min)))))))))))))
	(:script :type "text/javascript"
		 (fmt "// <![CDATA[
                            $(document).ready(function() {
                                 var locations = { locations: [~A]};
                                 var mymap = $('#map').gmaps({
                                          data: locations,
                                	  center: [~A , ~A],
                                          zoom: 15
                                 });
                                 $('#' + '~A').click();
                            });
                       // ]]>" 
		      (trains->locations display-trains to)
		      (station-latitude to)
		      (station-longitude to)
		      (trim to-tiploc)
		      )))))))


