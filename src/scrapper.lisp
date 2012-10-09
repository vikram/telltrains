(in-package :com.sondesh.database)

(defun get-timetable-with-fares (from to day month hour)
  (let ((cookie-jar (make-instance 'drakma:cookie-jar)))
      (drakma:http-request "http://ojp.nationalrail.co.uk/"
			  :method :get
			  :cookie-jar cookie-jar)
     (let ((cookie (car (drakma:cookie-jar-cookies cookie-jar))))
       (setf (drakma:cookie-jar-cookies cookie-jar) 
	     (list cookie
		   (make-instance 'drakma:cookie :name "s_cc" :value "true" :domain (drakma:cookie-domain cookie))
		   (make-instance 'drakma:cookie :name "s_sq" :value "%5B%5BB%5D%5D" :domain (drakma:cookie-domain cookie))))
       (drakma:http-request (format nil "http://~A/en/pj/jp" (drakma:cookie-domain cookie))
			    :method :get
			    :cookie-jar cookie-jar)
       (drakma:http-request (format nil "http://~A/en/pj/jp/" (drakma:cookie-domain cookie))
			    :method :post
			    :parameters `(("IsJavaScriptEnabled" . "false")
					  ("referer" . "kb_homepage")
					  ("from.searchTerm" . ,from)
					  ("to.searchTerm" . ,to)
					  ("via.searchTerm" . "")
					  ("timeOfOutwardJourney.day" . ,day)
					  ("timeOfOutwardJourney.month" . ,month)
					  ("hidYear" . "")
					  ("timeOfOutwardJourney.hour" . ,hour)
					  ("timeOfOutwardJourney.minute" . "00")
					  ("timeOfOutwardJourney.arrivalOrDeparture" . "DEPART")
					  ("timeOfOutwardJourney.firstOrLast" . "FIRST")
					  ("timeOfReturnJourney.day" . "")
					  ("timeOfReturnJourney.month" . "")
					  ("timeOfReturnJourney.hour" . "00")
					  ("timeOfReturnJourney.minute" . "15")
					  ("timeOfReturnJourney.arrivalOrDeparture" . "DEPART")
					  ("timeOfReturnJourney.firstOrLast" . "FIRST")
					  ("maxChanges" . "-1")
					  ("planjourney" . "SEARCH"))
			    :cookie-jar cookie-jar)
       (drakma:http-request (format nil "http://~A/en/pj/tt" (drakma:cookie-domain cookie))
			    :method :get
			    :cookie-jar cookie-jar)
       (drakma:http-request (format nil "http://~A/en/pj/tt" (drakma:cookie-domain cookie))
			    :method :post
			    :parameters '(("buttonPressed" . "GET_FARE_OPTIONS")
					  ("postByParam" . "CHECK FARES")
					  ("fareEnquiry.numberOfAdults" . "1")
					  ("fareEnquiry.numberOfChildren" . "0")
					  ("fareEnquiry.railcardCode" . "")
					  ("fareEnquiry.numberOfRailcards" . "1")
					  ("fareEnquiry.totalRailcards" . "0")
					  ("fareEnquiry.fareClass" . "ANY")
					  ("fareEnquiry.searchStrategy" . "FASTEST"))
			    :cookie-jar cookie-jar)
       (drakma:http-request (format nil "http://~A/en/pj/fa" (drakma:cookie-domain cookie))
			    :method :get
			    :cookie-jar cookie-jar)
       (sleep 5)
       (drakma:http-request (format nil "http://~A/en/pj/fa?uniqueId=~A" (drakma:cookie-domain cookie) (floor (* (random 0.1) 100000001)))
			    :method :get
			    :cookie-jar cookie-jar))))

(defun get-timetable (from to day month hour)
  (let ((cookie-jar (make-instance 'drakma:cookie-jar)))
      (drakma:http-request "http://ojp.nationalrail.co.uk/"
			  :method :get
			  :cookie-jar cookie-jar)
     (let ((cookie (car (drakma:cookie-jar-cookies cookie-jar))))
       (setf (drakma:cookie-jar-cookies cookie-jar) 
	     (list cookie
		   (make-instance 'drakma:cookie :name "s_cc" :value "true" :domain (drakma:cookie-domain cookie))
		   (make-instance 'drakma:cookie :name "s_sq" :value "%5B%5BB%5D%5D" :domain (drakma:cookie-domain cookie))))
       (drakma:http-request (format nil "http://~A/en/pj/jp" (drakma:cookie-domain cookie))
			    :method :get
			    :cookie-jar cookie-jar)
       (drakma:http-request (format nil "http://~A/en/pj/jp/" (drakma:cookie-domain cookie))
			    :method :post
			    :parameters `(("IsJavaScriptEnabled" . "false")
					  ("referer" . "kb_homepage")
					  ("from.searchTerm" . ,from)
					  ("to.searchTerm" . ,to)
					  ("via.searchTerm" . "")
					  ("timeOfOutwardJourney.day" . ,day)
					  ("timeOfOutwardJourney.month" . ,month)
					  ("hidYear" . "")
					  ("timeOfOutwardJourney.hour" . ,hour)
					  ("timeOfOutwardJourney.minute" . "00")
					  ("timeOfOutwardJourney.arrivalOrDeparture" . "DEPART")
					  ("timeOfOutwardJourney.firstOrLast" . "FIRST")
					  ("timeOfReturnJourney.day" . "")
					  ("timeOfReturnJourney.month" . "")
					  ("timeOfReturnJourney.hour" . "00")
					  ("timeOfReturnJourney.minute" . "15")
					  ("timeOfReturnJourney.arrivalOrDeparture" . "DEPART")
					  ("timeOfReturnJourney.firstOrLast" . "FIRST")
					  ("maxChanges" . "-1")
					  ("planjourney" . "SEARCH"))
			    :cookie-jar cookie-jar)
       (drakma:http-request (format nil "http://~A/en/pj/tt" (drakma:cookie-domain cookie))
			    :method :get
			    :cookie-jar cookie-jar))))

(defun get-timetable-changes (from to day month hour min)
  (let ((cookie-jar (make-instance 'drakma:cookie-jar)))
      (drakma:http-request "http://ojp.nationalrail.co.uk/"
			  :method :get
			  :cookie-jar cookie-jar)
     (let ((cookie (car (drakma:cookie-jar-cookies cookie-jar))))
       (setf (drakma:cookie-jar-cookies cookie-jar) 
	     (list cookie
		   (make-instance 'drakma:cookie :name "s_cc" :value "true" :domain (drakma:cookie-domain cookie))
		   (make-instance 'drakma:cookie :name "s_sq" :value "%5B%5BB%5D%5D" :domain (drakma:cookie-domain cookie))))
       (drakma:http-request (format nil "http://~A/en/pj/jp" (drakma:cookie-domain cookie))
			    :method :get
			    :cookie-jar cookie-jar)
       (drakma:http-request (format nil "http://~A/en/pj/jp/" (drakma:cookie-domain cookie))
			    :method :post
			    :parameters `(("IsJavaScriptEnabled" . "false")
					  ("referer" . "kb_homepage")
					  ("from.searchTerm" . ,from)
					  ("to.searchTerm" . ,to)
					  ("via.searchTerm" . "")
					  ("timeOfOutwardJourney.day" . ,day)
					  ("timeOfOutwardJourney.month" . ,month)
					  ("hidYear" . "")
					  ("timeOfOutwardJourney.hour" . ,hour)
					  ("timeOfOutwardJourney.minute" . ,min)
					  ("timeOfOutwardJourney.arrivalOrDeparture" . "DEPART")
					  ("timeOfOutwardJourney.firstOrLast" . "FIRST")
					  ("timeOfReturnJourney.day" . "")
					  ("timeOfReturnJourney.month" . "")
					  ("timeOfReturnJourney.hour" . "00")
					  ("timeOfReturnJourney.minute" . "15")
					  ("timeOfReturnJourney.arrivalOrDeparture" . "DEPART")
					  ("timeOfReturnJourney.firstOrLast" . "FIRST")
					  ("maxChanges" . "-1")
					  ("planjourney" . "SEARCH"))
			    :cookie-jar cookie-jar)
       (drakma:http-request (format nil "http://~A/en/pj/tt" (drakma:cookie-domain cookie))
			    :method :get
			    :cookie-jar cookie-jar)
       (drakma:http-request (format nil "http://~A/en/pj/jd2" (drakma:cookie-domain cookie))
			    :method :get
			    :cookie-jar cookie-jar))))

(defun live-departures (from to)
  (mapcar #'(lambda (train) 
	      (let ((actual (car (cdar (cddr train))))
		    (expected (car (cdadr (cddr train)))))
		(cons actual
		      (cond ((or (string= "On time" expected) 
				 (string= "No report" expected)
				 (string= "Starts here" expected))
			     actual)
			    (t expected)))))
	  (remove-html-node-if-not
	   (net.html.parser:parse-html
	    (drakma:http-request (format nil 
					 "http://www.livedepartureboards.co.uk/ldb/Sumdep.aspx?T=~A&S=~A&J=JourneyDetails.asp" 
					 from 
					 to)
				 :method :get))
	   #'(lambda (element) (or (element= element :tr "row2" "")
				   (element= element :tr "row1" ""))))))


(defun get-trains-changes (from to start-end-list)
  (let ((results (make-hash-table :test 'equal))
	(processes (length start-end-list)))
    (dolist (journey start-end-list)
      (acl-compat.mp:process-run-function 
       "single" 
       #'(lambda (journey) 
	   (dolist (result (single-times-changes from 
						 to 
						 (first journey) 
						 (second journey) 
						 (format nil "~A" (third journey)) 
						 (format nil "~A" (fourth journey))))
	     (when (not (gethash result results))
	       (setf (gethash result results) nil)))
	   (decf processes))
       journey))
    (while (> processes 0)
      (sleep 1))
    (cdr (hash-table->alist results))))

(defun single-fares (html-list)
  (mapcar #'(lambda (element) 
	      (aif (find-html-node element :a)
		   (string-trim '(#\space #\return #\tab #\Newline) (cadr it))))
	  (remove-html-node-if-not
	   (find-html-node html-list :table :id "singleFareTable")
	   #'(lambda (element) (element= element :td "borderCell" "")))))

(defun time-table (html-list)
  (mapcar #'(lambda (element) (cadr element))
	(remove-html-node-if-not
	 (find-html-node 
	  html-list :table :id "ResultsTable")
	 #'(lambda (element) (element= element :td "borderCell singleCellwidth" "")))))

(defun merge-times-with-fares (fares times)
  (let ((time-table (group-by-nth times 5))
	(counter 0)
	(results (make-hash-table :test 'equal)))
    (dolist (fare fares)
      (when fare
	(setf (gethash fare results) (cons (list (cond ((< counter 5) 'advance)
						       ((< counter 10) 'saver)
						       (t 'open))
						 (nth (mod counter 5) time-table))
					   (gethash fare results))))
      (incf counter))
    (cdr (hash-table->alist results))))

(defun single-times-fares (from to day month hour)
  (let ((html-results (net.html.parser:parse-html (get-timetable-with-fares from to day month hour))))
    (merge-fares-times
     (single-fares html-results)
     (time-table html-results))))

(defun single-times (from to day month hour)
  (let* ((slot (make-logic-symbol (concatenate 'string from to day month hour "no-changes")))
	 (html-results (net.html.parser:parse-html 
			(aif (gethash slot *results*)
			     it
			     (setf *results* (get-timetable from to day month hour))))))
    (group-by-nth
     (time-table html-results)
     5)))

(defun merge-fares-times (fares times)
  (let ((time-table (group-by-nth times 5))
	(counter -1))
    (labels ((add-fare (fare init-value type train)
	       (progn
		 (setf (gethash train init-value) (cons (list type fare)
						    (gethash train init-value)))
		 init-value)))
      (cdr (hash-table->alist
	    (reduce #'(lambda (init fare)
			(progn
			  (incf counter)
			  (if fare
			      (add-fare fare 
					init
					(cond ((< counter 5) 'advance)
					      ((< counter 10) 'saver)
					      (t 'open))
					(nth (mod counter 5) time-table))
			      init)))
		    fares :initial-value (make-hash-table :test 'equal)))))))

(defun get-oneway-allday-timetable (from to day month)
  (cdr (hash-table->alist
	(reduce #'(lambda (init fare-trains)
		    (progn
		      (setf (gethash (car fare-trains) init)
			    (append (cdr fare-trains) (gethash (car fare-trains) init)))
		      init))
		(mappend #'(lambda (hour) (single-times-fares from to day month (format nil "~A" hour)))
			 '(05 06 07 08 09 10 11 12 15 16 17 19 21))
		:initial-value (make-hash-table :test 'equal)))))

(defun get-unique-trains (from to day month start end)
  (sort 
   (delete-duplicates 
    (get-trains from to day month (map0-n #'(lambda (n) 
					     (format nil "~A" (+ start n))) 
					 (- end start)))
    :test #'(lambda (a b) (string= (third (car a)) (third (car b)))))
   #'string< :key #'(lambda (x) (third (car x)))))

(defun get-fares (from to day month hours)
  (let ((results (make-hash-table :test 'equal))
	(processes (length hours)))
    (dolist (hr hours)
      (acl-compat.mp:process-run-function 
       "single" 
       #'(lambda (hour) 
	   (dolist (result (single-times-fares from to day month (format nil "~A" hour)))
	     (print hr t)
	     (when (not (gethash (car result) results))
	       (setf (gethash (car result) results) (cdr result))))
	   (decf processes))
       hr))
    (while (> processes 0)
      (sleep 1))
    (cdr (hash-table->alist results))))

(defun get-trains (from to day month hours)
  (let ((results (make-hash-table :test 'equal))
	(processes (length hours)))
    (dolist (hr hours)
      (acl-compat.mp:process-run-function 
       "single" 
       #'(lambda (hour) 
	   (dolist (result (single-times from to day month (format nil "~A" hour)))
	     (when (not (gethash result results))
	       (setf (gethash result results) nil)))
	   (decf processes))
       hr))
    (while (> processes 0)
      (sleep 1))
    (cdr (hash-table->alist results))))

(defun single-times-changes (from to day month hour min)
  (let* ((slot (train-name from to day month hour min))
	 (html-results (net.html.parser:parse-html 
		       (aif (gethash slot *results*)
			    it
			    (setf (gethash slot *results*) (get-timetable-changes from to day month hour min))))))
    (extract-all-options html-results)))

(defun get-unique-trains-changes (from to start-end-list)
  (mapcar #'(lambda (j) (fix-tube-change-glitches (car j)))
	  (sort 
	   (delete-duplicates 
	    (get-trains-changes from to start-end-list)
	    :test #'(lambda (a b) (equal (cdr (car a)) (cdr (car b)))))
	   #'(lambda (t1 t2)
	       (let* ((starttime (format nil "~A:00" (third (first (start-2-end 2)))))
		      (time1 (or (and (string< starttime (second (caadar t1))) (second (caadar t1)))
				 (add-times (second (caadar t1)) "24:00")))
		      (time2 (or (and (string< starttime (second (caadar t2))) (second (caadar t2)))
				 (add-times (second (caadar t2)) "24:00"))))
		 (if (string= time1 time2)
		     (string< (journey-duration (cadar t1)) (journey-duration (cadar t2)))
		     (train< time1 time2)))))))

(ucw:defcomponent single-times-fares-hour-query (ucw:standard-component)
  ((from :initarg :from :accessor from :initform "")
   (to :initarg :to :accessor to :initform "")
   (day :initarg :day :accessor day :initform "")
   (month :initarg :month :accessor month :initform "")
   (hour :initarg :hour :accessor hour :initform ""))
  (:default-initargs :content-type "text/xml;")
  (:render ()
	   (with-slots (from to day month hour) single-times-fares-hour-query
	     (let ((first t))
	       (<:as-is "{ results: [")
	       (dolist (result (get-fares from to day month (map0-n #'(lambda (n) (format nil "~A" (+ n (parse-integer hour)))) 2)))
		 (if first
		     (setf first nil)
		     (<:as-is ","))
		 (<:as-is "{ train: ")
		 (let* ((train (car result))
			(changes (getChanges (eighth train) (parse-integer (sixth train)))))
		   (<:as-is (format nil "{ from: \"~A\", to: \"~A\", leaving: \"~A\", arriving: \"~A\", duration: \"~A\", changes: \"~A\", startem: \"~A\", durem: \"~A\", leavingnum: \"~A\", breaks: [" (find-station (first train)) (find-station (second train)) (third train) (fourth train) (fifth train) (sixth train) (round (timedifference (third train) "05:00")) (round (timedifference (fifth train) "00:00")) (timenum (third train))))
		   (let ((first-change t))
		     (dolist (change changes)
		       (if first-change
			   (setf first-change nil)
			   (<:as-is ","))
		       (<:as-is (format nil "{ start: \"~A\", leaving: \"~A\", type: \"~A\"}" (first change) (second change) (third change))))))
		 (<:as-is "]}, fares: [")
		 (let ((first-fare t))
		   (dolist (fare (cdr result))
		     (if first-fare
			 (setf first-fare nil)
			 (<:as-is ","))
		     (<:as-is (format nil "{ type: \"~A\", price: ~S}" (first fare) (second fare)))))
		 (<:as-is "] }"))
	       (<:as-is "] }")))))

(defun get-street ()
  (let ((second 0)
	(first 0))
    (dotimes (i 87)
      (setf first (1+ i))
      (with-open-file (out (format nil "/home/vb/houses/~A" first) :direction :output :if-exists :supersede)
	(multiple-value-bind (str code)
	    (drakma:http-request (format nil "http://www.houseprices.co.uk/streets/~A/" first)
				 :method :get)
	  (write str :stream out)))
      (dotimes (j 87)
	(setf second (1+ j))
	(with-open-file (out (format nil "/home/vb/houses/~A.~A" first second) :direction :output :if-exists :supersede)
	  (multiple-value-bind (str code)
	      (drakma:http-request (format nil "http://www.houseprices.co.uk/streets/~A/~A" first second)
				   :method :get)
	    (write str :stream out)))
	(print (cons first second) t)))))


(defun write-street-urls ()
  (with-open-file (out "/home/vb/houses/streets.url" :direction :output :if-exists :supersede)
    (dotimes (i 87)
      (dotimes (j 87)
	(with-open-file (in (format nil "/home/vb/houses/~A.~A" (1+ i) (1+ j)))
	  (awhen (net.html.parser:parse-html (read in))
	    (dolist (li (remove-if-not 
			 #'listp
			 (funcall (compose #'cdr #'(lambda (c) (nth 6 c)) #'cdar #'cdar #'cdddar #'cddadr)
				  it)))
	      (write-line (concatenate 'string "http://www.houseprices.co.uk" (third (first (cadr li)))) out))))))))

(defun cant-make-runner (ht)
  (= 10 (length ht)))

(defun use-runner! (ht i)
  (setf ht (push i ht)))

(defun release-runner! (ht i)
  (delete i ht :test #'=))

(defun get-each-street-details ()
  (let ((i 0))
    (with-open-file (in "/home/vb/houses/streets.url")
      (do ((line (read-line in nil)
		 (read-line in nil)))
	  ((null line))
	(when line
	  (awhen (fourth (split-sequence:split-sequence #\/ line))
	    (unless (probe-file (format nil "/home/vb/houses/streets/~A" it))
	      (incf i)
	      (funcall
	       #'(lambda (name li j)
		   (with-open-file (out (format nil "/home/vb/houses/streets/~A" name) :direction :output)
		     (write (drakma:http-request li) :stream out)
		     (print (cons name j) t)))
	       it
	       line
	       i))))))))

(defparameter *postcode-scanner*  
  (cl-ppcre:create-scanner 
   "\\b[A-PR-UWYZ][A-HK-Y0-9][A-HJKSTUW0-9]?[ABEHMNPRVWXY0-9]? {1,2}[0-9][ABD-HJLN-UW-Z]{2}\\b|GIR 0AA" 
   :case-insensitive-mode t))

(defun write-addresses (starters file)
  (with-open-file (out (sondesh-file-path "pcs/houses/addresses") :direction :output 
		       :if-exists :append :if-does-not-exist :create)
    (with-open-file (in (sondesh-file-path (format nil "pcs/houses/streets.~A" file)))
      (do ((line (read-line in nil)
		 (read-line in nil)))
	  ((null line))
	(when line
	  (awhen (fourth (split-sequence:split-sequence #\/ line))
	    (when (cl-ppcre:all-matches-as-strings starters it)
	      (if (probe-file (sondesh-file-path (format nil "pcs/houses/streets/~A" it)))
		  (with-open-file (in-street (sondesh-file-path (format nil "pcs/houses/streets/~A" it)))
		    (let ((html (make-array (file-length in-street) :element-type 'character)))
		      (read-sequence html in-street)
		      (let ((addresses
			     (or 
			      (funcall (compose #'cddar #'cdar #'cdar #'cdadr #'sixth #'cdar #'cddar #'cdr #'cdr) (net.html.parser:parse-html html))
			      (funcall (compose #'cddar #'cdar #'cdar #'cdadr #'seventh #'third #'cddar #'cdr) (net.html.parser:parse-html html))
			      (funcall (compose #'cddar #'cdar #'cdar 'cdadr #'sixth #'cdr #'caddar #'cdr) (net.html.parser:parse-html html))
			      (funcall (compose #'cddar #'cdar #'cdar #'cdadr #'seventh #'third #'caddr) (net.html.parser:parse-html html)))))
			(if addresses
			    (progn
			      (dolist (address (mapcar 
						#'(lambda (tr) (second (ninth tr)))
						addresses))
				(when address
				  (write-line address out)))
			      (print (cons 'done it)))
			    (print (list 'cannot-parse it addresses))))))
		  (print (cons 'missing-file it))))))))))

(defparameter *short-postcodes* (make-hash-table :test 'equal))

(defun load-short-codes ()
  (with-open-file (in "/home/vb/houses/uk-postcodes.cvs")
    (do ((line (read-line in nil)
	       (read-line in nil)))
	((null line))
      (when line
	(let ((record (split-sequence:split-sequence #\, line)))
	  (setf (gethash (first record) *SHORT-POSTCODES*) (cons (parse-float (fourth record)) (parse-float (fifth record)))))))))

(defun extract-exact-latlong (short1 short2)
  (awhen (second 
	  (seventh 
	   (third 
	    (cadr 
	     (net.html.parser:parse-html
	      (drakma:http-request
	       (format nil "http://www.houseprices.co.uk/map.php?pc=~A+~A&show=s" short1 short2)))))))
    (cl-ppcre:all-matches-as-strings "-?[0-9]+\\.[0-9]+" it)))

(defun first-parse (x)
  (let ((i 0))
  (with-open-file (in "/home/vb/houses/addresses")
    (do ((line (read-line in nil)
	       (read-line in nil)))
	((null line))
      (when line
	(awhen (first (cl-ppcre:all-matches-as-strings *postcode-scanner* line))
	  (with-train-database!
	    (unless (clsql:query (format nil "select latitude,longitude from postcode where post='~A'" it))
	      (let* ((short1 (first (split-sequence:split-sequence #\space it)))
		     (short2 (second (split-sequence:split-sequence #\space it)))
		     (latlong (extract-exact-latlong short1 short2)))
		(print (list "not-found" it latlong
			     (format nil
				     "http://72.32.115.139/service_s.php?pc=~A+~A&bounds=((~A,~A)(~A,~A))"
				     short1 short2 
				     (-  (parse-float (second latlong)) 0.057127991)
				     (-  (parse-float (car latlong)) 0.057685219)
				     (+  (parse-float (second latlong)) 0.055978088)
				     (+  (parse-float (car latlong)) 0.054486939))))
		(clsql:update-records-from-instance 
			 (make-instance 'postcode
					:id (next-id)
					:short short1
					:post it
					:latitude (second latlong)
					:longitude (first latlong)))
		(when (> (incf i) x)
		  (return-from first-parse))
		(with-input-from-string (in
					 (drakma:http-request (format nil
								      "http://72.32.115.139/service_s.php?pc=~A+~A&bounds=((~A,~A)(~A,~A))"
								      short1 short2 
								      (-  (parse-float (second latlong)) 0.057127991)
								      (-  (parse-float (car latlong)) 0.057685219)
								      (+  (parse-float (second latlong)) 0.055978088)
								      (+  (parse-float (car latlong)) 0.054486939))))
		  (do ((line (read-line in nil)
			     (read-line in nil)))
		      ((null line))
		    (when (and line (cl-ppcre:all-matches-as-strings "^R" line))
		      (let ((answer (split-sequence:split-sequence #\| line)))
			(print (list "answer" answer))
			(clsql:update-records-from-instance 
			 (make-instance 'postcode
					:id (next-id)
					:short (first (split-sequence:split-sequence #\space (second answer)))
					:post (second answer)
					:latitude (fourth answer)
					:longitude (third answer))))))))))))))))


(defun cleanup-postcodes (&key (init nil))
(with-open-file (out (sondesh-file-path "pcs/postcodes") :direction :output :if-exists :supersede)
  (with-open-file (in (sondesh-file-path "pcs/uk-postcodes.csv"))
    (do ((line (read-line in nil)
	       (read-line in nil)))
	((null line))
      (when line
	(awhen (split-sequence:split-sequence #\comma line)
	  (format out "~A,~A,~A~%" (first it) (parse-float (fourth it)) (parse-float (fifth it)))))))
  (with-open-file (in (sondesh-file-path "pcs/Uk-Postcodes-Towns.csv"))
    (do ((line (read-line in nil)
	       (read-line in nil)))
	((null line))
      (when line
	(awhen (split-sequence:split-sequence #\comma line)
	  (format out "~A,~A,~A~%" (first it) (parse-float (fifth it)) (parse-float (sixth it)))))))
  (with-open-file (in (sondesh-file-path "pcs/open.postcodes"))
    (do ((line (read-line in nil)
	       (read-line in nil)))
	((null line))
      (when line
	(awhen (split-sequence:split-sequence #\tab line)
	  (format out "~A,~A,~A~%" (first it) (parse-float (second it)) (parse-float (third it)))))))
  (with-open-file (in (sondesh-file-path "pcs/osm.postcodes"))
    (do ((line (read-line in nil)
	       (read-line in nil)))
	((null line))
      (when line
	(awhen (split-sequence:split-sequence #\space line)
	  (when (> (length it) 4)
	    (format out "~A ~A,~A,~A~%" 
		    (third it) 
		    (fourth it) 
		    (parse-float (first it)) 
		    (parse-float (second it))))))))
  (when (not init)
  (dolist (file '("pcs/anglia.asc" "pcs/midlands.asc" "pcs/ne.asc" "pcs/nw.asc" "pcs/sc.asc" "pcs/scotland.asc"
		  "pcs/selon.asc" "pcs/sw.asc"))
    (let ((done nil))
      (with-open-file (in (sondesh-file-path file))
	(do ((line (read-line in nil)
		   (read-line in nil)))
	    ((null line))
	  (when (and line (not done))
	    (awhen (split-sequence:split-sequence #\comma line)
	      (if (or (string= (first it) "") (not (third it)))
		  (setf done t)
		  (format out "~A,~A,~A~%" 
			  (read-from-string (third it))
			  (parse-float (second it) :junk-allowed t)
			  (parse-float (first it) :junk-allowed t))))))))))))

(defun analyze-different (type start shouldntbelong)
    (labels ((write-type (stream city line)
	       (unless (or (string= city "")
			   (cl-ppcre:all-matches-as-strings "[0-9]" city)
			   (remove-if-not #'(lambda (word) (member (string-upcase word) shouldntbelong :test #'string=))
					  (split-sequence:split-sequence #\space city)))
		 (write-line (format nil "~A,~A" city line) stream))))
    (with-open-file (out (sondesh-file-path (format nil "pcs/~A.possibles" type))
			 :direction :output :if-exists :supersede :if-does-not-exist :create)
      (with-open-file (in (sondesh-file-path "/pcs/houses/addresses"))
	(do ((line (read-line in nil)
		   (read-line in nil)))
	    ((null line))
	  (awhen (and line (split-sequence:split-sequence #\comma line))
	    (casequal (length it)
		      (3 (write-type out (trim (nth start it)) line))
		      (4 (write-type out (trim (nth (+ 1 start) it)) line))
		      (t (progn
			   (write-type out (trim (nth (+ 1 start) it)) line)
			   (write-type out (trim (nth (+ 2 start) it)) line))))))))))

(defparameter *street-words* '(close road street court lane walk avenue way parade crescent drive bridge))
(defparameter *estate-words* '(terrace place gardens grove court manor))
(defparameter *city-words* '(town))
