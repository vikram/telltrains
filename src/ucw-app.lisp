(in-package :com.sondesh.database)

(defparameter *show-changes* t)
(defparameter *fb-api-key*  "4c1a3787cab3d55a42609ad10355d9f3")
(defparameter *fb-secret* "4cec5eb627fb1213e817a0e7d276fabe")
(defparameter *prod* t)
(defparameter *debug* nil)
(defparameter *unpacked* nil)
(defparameter *image-path* "http://www.telltrains.com/images/")
(defparameter *js-path* "http://www.telltrains.com/javascripts/")
(defparameter *css-path* "http://www.telltrains.com/stylesheets/")

(defun image-path (image-name)
  (if *prod*
      (concatenate 'string *image-path* image-name)
      (concatenate 'string "http://82.153.32.254/images/" image-name)))

(defun css-path (css-name)
  (if *prod*
      (concatenate 'string *css-path* css-name)
      (concatenate 'string "http://82.153.32.254/stylesheets/" css-name)))

(defun js-path (js-name)
  (if *prod*
      (concatenate 'string *js-path* js-name)
      (concatenate 'string "http://82.153.32.254/javascripts/" js-name)))

(defun bar-image (class &key (type 'main))
  (casequal (string-downcase class)
    ("tube" (case type
	      (stop (image-path "bluebarstop.png"))
	      (main (image-path "brownbar.png"))
	      (left (image-path "leftbrownbar.png"))
	      (right (image-path "rightbrownbar.png"))))
    ("walk" (case type
	      (stop (image-path "bluebarstop.png"))
	      (main (image-path "bluebar.png"))
	      (left (image-path "leftbluebar.png"))
	      (right (image-path "rightbluebar.png"))))
    ("bus" (case type
	     (stop (image-path "bluebarstop.png"))
	      (main (image-path "bluebar.png"))
	      (left (image-path "leftbluebar.png"))
	      (right (image-path "rightbluebar.png"))))
    (t (case type
	 (stop (image-path "bluebarstop.png"))
	 (main (image-path "bluebar.png"))
	 (left (image-path "leftbluebar.png"))
	 (right (image-path "rightbluebar.png"))))))

(ucw:defcomponent homepage (ucw:standard-component)
  ((user :initarg :user :accessor user :initform nil))
  (:render ()
	   (with-slots (user) homepage
	     (<:html
	      (<:head
	       (<:title (<:as-is "telltrains: easiest way to find and compare uk trains and fares"))
	       (<:link :rel "stylesheet" :href (css-path "sheet.css"))
	       (<:link :rel "stylesheet" :href (css-path "gmlightbox.css"))
	       (<:link :rel "icon" :href (image-path "transparent.ico"))
	       (<:script :type "text/javascript" :src (js-path "gmlightbox.js"))
	       (<:script :type "text/javascript" :src (js-path "prototype.js"))
	       (<:script :type "text/javascript" :src (js-path "scriptaculous.js"))
	       (<:script 
		:src 
		"http://maps.google.com/maps?file=api&v=2&key=ABQIAAAAfzASmKvmIGhgVZBphgbIMhSbOCASmIHfcyCoxgLWrOePWQVFNhScF6BwHPam3my7bT67NmWM1eTM4g" 
		:type "text/javascript")
	       (<:script :src "http://www.google-analytics.com/urchin.js" :type "text/javascript")
	       (<:script :type "text/javascript"
			 (<:as-is "_uacct='UA-1779260-1';
                                   urchinTracker();")))
	      (<:body
	       (<:div :id "container"
		      (<:h1 (<:a "Telltrains"))
		      (let ((from "") (to "") (leaving "") (travelers 1) (leaving-time ""))
			(<ucw:simple-form :method "get"
					  :class "telltrains"
					  :action (find-single-fares homepage from to leaving travelers leaving-time)
					  (<:table
					   (<:tr (<:td (<:label :for "from" "From (city or station)"))
						 (<:td (<ucw:text :name "from" 
								  :id "fromstation" 
								  :autocomplete "off" 
								  :style "width: 200px" 
								  :class "text"
								  :accessor from)))
					   (<:tr (<:td (<:label :for "to" "To (city or station)"))
						 (<:td (<ucw:text :name "to" 
								  :autocomplete "off"
								  :id "tostation" 
								  :style "width: 200px" 
								  :class "text"
								  :accessor to))))
					  (<:input :type "submit" 
						   :name "find trains" 
						   :value "Search")))
		      (<:div :class "auto_complete"
			     :id "fromstation_values"
			     :style "display: none; z-index: 1;"))
 	      (<:script :type "text/javascript"
			(<:as-is
			 "new Ajax.Autocompleter('fromstation', 'fromstation_values',
                                                'suggest-stations', {frequency: 0.4, minChars: 2});"
			 "new Ajax.Autocompleter('tostation', 'fromstation_values',
                                                'suggest-stations', {frequency: 0.4, minChars: 2});"
			 )))
		))))

(ucw:defcomponent station-index (ucw:standard-component)
  ((user :initarg :user :accessor user))
  (:default-initargs :title "telltrains: most popular british rail stations"
    :content-type "text/html; charset=utf-8;")
  (:render ()
	   (with-slots (user) station-index
	     (<:html
	      (<:head
	       (<:title (<:as-is "Telltrains: most popular british rail station"))
	       (<:link :rel "icon" :href (image-path "transparent.ico"))
	       (<:meta :name "robots" :content "index, follow")
	       (<:meta :name "description" :content "New graphical journey planner for Britain's National Rail network. train times; commuter friendly; commuter social network; train running information.")
	       (<:meta :content "national rail, british rail, most popular stations, commonly used stations, hubs, trains, railway, uk rail travel, train times, train timetables, train information, uk trains, uk railway, late trains, rail commuter, journey planner, social network" :name "keywords")
	       (<:script :src "http://www.google-analytics.com/urchin.js" :type "text/javascript")
	       (<:script :type "text/javascript"
			 (<:as-is "_uacct = 'UA-1779260-1';
                                           urchinTracker();")))
	      (<:body 
	       (<:h1 (<:a :href "/" (<:as-is "Telltrains - beta")))
	       (<:p
		(<:as-is "List of the most popular stations")
		(<:ol 
		 (mapcar #'(lambda (crs) (let ((stn (find-station-by-code crs)))
					     (<:li (<:a :href (format nil "/journeysfrom/~A" (station-display-name stn))
							(<:as-is (format nil "train times & timetables from ~A" (station-display-name stn)))))))
			 (sort (copy-list *all-crses*)
			       #'>
			       :key (compose #'station-rank #'find-station-by-code))))))))))

(defun direct-trains-count (crs1 crs2)
  (let ((results 0))
    (dolist (tip1 (station-tiploc-codes (find-station-by-code crs1)))
      (dolist (tip2 (station-tiploc-codes (find-station-by-code crs2)))
	(awhen (gethash (concatenate 'string tip1 tip2) *connectivity*)
	  (incf results (connectivity-number it)))))
    results))

(ucw:defcomponent station-journeys (ucw:standard-component)
  ((user :initarg :user :accessor user)
   (from :initarg :from :accessor from))
  (:default-initargs :title "telltrains: most popular british rail stations"
    :content-type "text/html; charset=utf-8;")
  (:render ()
	   (with-slots (from) station-journeys
	     (<:html
	      (<:head
	       (<:title (<:as-is (format nil "Telltrains: most popular journeys from ~A" from)))
	       (<:link :rel "icon" :href (image-path "transparent.ico"))
	       (<:meta :name "robots" :content "index, follow")
	       (<:meta :name "description" :content "New graphical journey planner for Britain's National Rail network. train times; commuter friendly; commuter social network; train running information.")
	       (<:meta :content "national rail, british rail, most popular stations, commonly used stations, hubs, trains, railway, uk rail travel, train times, train timetables, train information, uk trains, uk railway, late trains, rail commuter, journey planner, social network" :name "keywords")
	       (<:script :src "http://www.google-analytics.com/urchin.js" :type "text/javascript")
	       (<:script :type "text/javascript"
			 (<:as-is (format nil "_uacct = 'UA-1779260-1';
                                              urchinTracker();")))
	      (<:body 
	       (<:h1 (<:as-is (format nil "Train times for most popular journey timetables from ~A" from)))
	       (let* ((stn (first (find-station-by-letters from)))
		      (crs (station-crs-code stn))
		      (rest-of-crses (remove-if #'(lambda (x)
						    (member crs (cons (station-group-crs-code (find-station-by-code x))
								      (station-all-crs-codes (find-station-by-code x)))
							    :test #'string=))
						*all-crses*)))
		 (when (> (length (station-all-crs-codes stn)) 1)
		   (let ((first t))
		     (<:div 
		      (<:ul :style "list-style-type:none; left: -40px; position: relative;"
			    (<:li :style "display: inline;" (<:as-is (format nil "Stations in ~A ..." from)))
				(mapcar #'(lambda (crs) 
					    (let ((stn (find-station-by-code crs)))
					      (if first
						  (setf first nil)
						  (<:as-is ","))
					      (<:li :style "padding:4px 5px 2px 9px; display: inline;"
					       (<:a :href (format nil "/journeysfrom/~A" (station-display-name stn))
						    (<:as-is (station-display-name stn))))))
					(sort (copy-list (station-all-crs-codes stn))
					      #'> :key (compose #'station-rank #'find-station-by-code)))))))
		  (<:div (<:a :href "/" (<:as-is "Home")))
		  (<:div :style "padding-left: 15px;" (<:as-is "=>") (<:a :href "/stations" (<:as-is "Train times for most popular stations")))
		  (<:div :style "padding-left: 30px;" (<:as-is "=>") (<:as-is from))
		  (<:ol :style "padding-left: 60px;"
			(mapcar 
			 #'(lambda (other-crs) 
			     (let ((other-stn (find-station-by-code other-crs)))
			       (<:li (<:a :href (format nil "/timetable/~A/~A" 
							(station-display-name stn) (station-display-name other-stn))
					  (<:as-is (format nil "train times & timetable from ~A to ~A" 
							   (station-display-name stn) (station-display-name other-stn)))))))
			 (sort rest-of-crses 
			       #'(lambda (crs1 crs2)
				   (let ((dt1 (direct-trains-count (station-crs-code stn) crs1))
					 (dt2 (direct-trains-count (station-crs-code stn) crs2)))
				     (if (= dt1 dt2) 
					 (> (+ (station-rank stn) (station-rank (find-station-by-code crs1)))
					    (+ (station-rank stn) (station-rank (find-station-by-code crs2))))
					 (> dt1 dt2))))))))))))))

;;google maps key ABQIAAAAfzASmKvmIGhgVZBphgbIMhSbOCASmIHfcyCoxgLWrOePWQVFNhScF6BwHPam3my7bT67NmWM1eTM4g

(ucw:defaction auto-suggest-stations ((homepage homepage) input)
  (ucw:call 'suggest-stations-html :input input))

(ucw:defcomponent suggest-stations-html-old (ucw:standard-component)
  ((input :initarg :input :accessor input :initform ""))
  (:default-initargs :content-type "text/xml;")
  (:render ()
	   (with-slots (input) suggest-stations-html-old
	     (<:ul :class "stations"
		   (mapcar #'(lambda (result) 
			       (<:li :class "station"
				     :id (station-crs-code result)
				     (<:div :class "displayname"
					    (<:as-is (station-display-name result)))
				     (<:div :class "location"
					    (<:span :class "informal" (<:as-is (station-location result))))
				     (when (station-latitude result)
				       (<:div :class "link"
					      (<:span :class "informal"
						      (<:a :href 
							   (format nil 
								   "http://maps.google.com/?ie=UTF8&z=6&ll=~A,~A&spn=0.339115,0.6427&om=1"
								   (station-latitude result) (station-longitude result))
							   :onclick "gm_ShowMap(this); return false;"
							   :title (station-name result) (<:as-is "map")))))))
			   (topn (find-station-by-letters input) 10))))))

(ucw:defcomponent suggest-stations-html (ucw:standard-component)
  ((input :initarg :input :accessor input :initform ""))
  (:default-initargs :content-type "text/xml;")
  (:render ()
	   (with-slots (input) suggest-stations-html
	     (mapcar #'(lambda (result) 
			 (<:as-html (station-display-name result) "|" (station-crs-code result) #\Newline))
		     (topn (find-station-by-letters input) 10)))))

(ucw:defcomponent postcode-html (ucw:standard-component)
  ((input :initarg :input :accessor input :initform ""))
  (:default-initargs :content-type "text/xml;")
  (:render ()
	   (with-slots (input) postcode-html
	     (mapcar #'(lambda (result) 
			 (<:as-html (first result) "|" (second result) "," (third result) #\Newline))
		     (topn (find-postcode-by-letters input) 10)))))

(ucw:defcomponent trainjourneys (ucw:standard-component)
  ((user :initarg :user :accessor user)
   (from :initarg :from :accessor from :initform nil)
   (to :initarg :to :accessor to :initform nil)
   (leaving :initarg :leaving :accessor leaving :initform "")
   (leaving-time :initarg :leaving-time :accessor leaving-time :initform ""))
  (:default-initargs 
    :content-type "text/html; charset=utf-8;")
  (:render ()
	   (with-slots (from to leaving leaving-time) trainjourneys
	     (let* ((l (or (and leaving (stylized-day->caldate leaving)) ""))
		    (now (or (and (string/= l "") 
				 (< (- (now) (string->dttm l)) 0)
				 (string->dttm (format nil "~A 01:00:01 +0100 (BST)" l)))
			    (now)))
		  (leaving-date (dttm->string now :format :caldate)))
	     (multiple-value-bind (se mi ho da mo ye dd dst1 tz1) (decode-universal-time now -1)
	       (declare (ignore se ye dd dst1 tz1 mi da mo) (fixnum ho))
	       (let* ((real-from (find-station-by-exact-name from))
		      (real-to (find-station-by-exact-name to))
		      (display-trains (if (and from to)
				  (find-trains real-from real-to now)
				  nil))
		      (trains (mapcar #'first display-trains)))
		 (<:div :id "journeys-json-div" :style "display:none;" 
			(<:span :id "journeys-json" 
				(<:as-is (journeys->json trains ho leaving-date :new-id-p t))))
		 (links-and-date-ajax leaving-date :new-id-p t)
		 (train-journeys-with-changes display-trains (now) leaving-date :new-id-p t)))))))

(ucw:defcomponent suggest-stations (ucw:standard-component)
  ((input :initarg :input :accessor input :initform ""))
  (:default-initargs :content-type "text/xml;")
  (:render ()
	   (with-slots (input) suggest-stations
	     (let ((first t))
	       (<:as-is "{ results: [")
	       (dolist (result (find-station-by-letters input))
		 (progn
		   (if first
		       (setf first nil)
		       (<:as-is ","))
		   (<:as-is (format nil "{ id: \"~A\", value: \"~A\", info: \"~A\"}" 
				    (station-crs-code result) 
				    (station-name result) 
				    (station-location result)))))
	     (<:as-is "] }")))))

(defun left-indent (pos)
  (format nil "left: ~Apx;" pos))

(defun align-time (time now)
  (- (round (timedifference time "05:00" :next-day-p (string< time (current-short-time now)))) 50))

(defun train-journeys-with-changes (results now leaving-date &key (new-id-p nil))
  (let ((train-number 1) (today-p t))
    (dolist (result results)
      (setf today-p (train-journey train-number (rest result) now leaving-date today-p new-id-p))
      (incf train-number))))

(defun get-range (journeys journey-time-fn)
  (aif (mapcar #'(lambda (ordered-journey)
		   (let ((journey (car (cdr ordered-journey))))
		     (funcall journey-time-fn journey)))
	       journeys)
       (if (string< (car (last it)) (first it))
	   (cons (first it) (list "24:00"))
	   (cons (first it) (last it)))))

(defun get-start-range (journeys)
  (get-range journeys #'journey-start))

(defun get-end-range (journeys)
  (get-range journeys #'journey-end))

(defun time->slider-value (time hour &optional (next-day-p nil))
  (if next-day-p
      (+ 96 (floor (timedifference time (format nil "~2,'0d:00" hour)) 15))
      (floor (timedifference time (format nil "~2,'0d:00" hour)) 15)))

(defun convert-range->slider (range hour)
  (cons (time->slider-value (first range) hour)
	(time->slider-value (second range) hour)))

(defun train-journey (train-number display-trains now leaving-date today-p new-id-p)
  (let* ((trains (display-journey-summary display-trains))
	 (tn train-number)
	 (duration (journey-duration trains))
	 (durlen (round (timedifference (journey-duration trains) "00:00")))
	 (deptime (second (first trains)))
	 (arrtime (fourth (car (last trains))))
	 (id (format nil "J~A~A" 
		     (remove #\: deptime) 
		     (remove #\: arrtime))))
    (declare (fixnum tn durlen))
    (when today-p
      (when (string= (dttm->string (now) :format :caldate) leaving-date)
	(setf today-p
	      (< 0 (timedifference (journey-start trains) (current-short-time))))))
    (setf id (format nil "~A~A" id 		     
		     (if today-p
			 leaving-date
			 (dttm->string (+ (string->dttm leaving-date) (* 24 3600)) :format :caldate))))
    (<:div :class "red" :style (format nil "top: ~Aem; left: ~Apx; width: ~Apx;" 
				       (+ 1 (* 3 (- tn 1))) ;train number
				       (ALIGN-TIME deptime now)
				       (+ 400 durlen))
	   :id (format nil "~A~A" 
		       id 
		       (if new-id-p
			   "other"
			   ""))
	   (<:div :class "deptime" (<:as-is (time-ampm deptime)))
	   (<:div :class "arrtime" :style (left-indent (+ durlen 65)) 
		  (<:as-is (format nil "~A (~A)" (time-ampm arrtime) (time-hrmin duration)))
		  (let ((src (stops->request display-trains)))
		    (<:a :href src
			 :onclick (format nil "javascript:urchinTracker('~A');" src)
			 :id (format nil "a~Astops" id) 
			 :class "thickbox"
			 :title (format nil "Leaving ~A at ~A arriving ~A at ~A (~A)" 
					(string-capitalize (first (first trains)))
					(time-ampm (second (first trains)))
					(string-capitalize (third (car (last trains))))
					(time-ampm (fourth (car (last trains))))
					(time-hrmin (journey-duration trains))))))
	   (cond ((= (length trains) 1) 
		   (list (<:img :class (string-downcase (fifth (first trains))) :src (bar-image (fifth (first trains)) :type 'left)
				:alt (format nil "~A from ~A" (fifth (first trains)) (first (first trains))) 
				:title (format nil "~A from ~A" (fifth (first trains)) (first (first trains))) 
				:width "5")
			 (<:img :class (string-downcase (fifth (first trains))) :src (bar-image (fifth (first trains)) :type 'main)
				:alt (format nil "leaving at ~A" (second (first trains)))
				:title (format nil "leaving at ~A" (second (first trains)))
				:width (format nil "~A" (- durlen 10)) :style "left: 55px;")
			 (<:img :class (string-downcase (fifth (first trains))) :src (bar-image (fifth (first trains)) :type 'right)
				:alt (format nil "arriving ~A at ~A" (third (first trains)) (fourth (first trains)))
				:title (format nil "arriving ~A at ~A" (third (first trains)) (fourth (first trains)))
				:width "5" 
				:style (left-indent (- (+ durlen 50) 5)))
			 (<:div :class "changeat" 
				:style "left: 0px;" 
				(<:as-is (short-name (first (first trains)))))
			 (<:div :class "changeat" 
				:style (left-indent 
					(if (> (name->pixels (short-name (first (first trains)))) (+ durlen 65))
					    (+ (name->pixels (short-name (first (first trains)))) 20)
					    (+ durlen 65)))
				(<:as-is (short-name (third (first trains)))))))
		 (t (graph-trains-changes display-trains deptime durlen))))
    today-p))

(defun journey-title (display-trains tiploc-loc)
  (let ((summary (display-train-summary (first display-trains)))
	(start (first (display-train-stops (first display-trains))))
	(end (car (last (display-train-stops (car (last display-trains)))))))
    (list
     (<:as-is (format nil "~A leaving " (fifth summary)))
     (<:a :class "map" :tabindex (cdr (assoc (fifth start) tiploc-loc :test #'string=))
	  (<:as-is (string-capitalize (first summary))))
     (<:as-is (format nil " at ~A arriving " (time-ampm (second summary))))
     (<:a :class "map" :tabindex (cdr (assoc (fifth end) tiploc-loc :test #'string=))
	  (<:as-is (string-capitalize (third summary))))
     (<:as-is (format nil " at ~A" (time-ampm (fourth summary)))))))

(defun tiploc->station-name (tiploc)
  (station-display-name 
   (find-station-by-code
    (gethash tiploc *tiploc->crs*))))

(defun stops->request (display-trains)
  (format nil "/journeystops?journey=~A&TB_iframe=true"
	  (join-with (mapcar #'(lambda (display-train)
				 (format nil "~a;~a~a~4,'0d~4,'0d" 
					 (first (display-train-terminates-at display-train)) 
					 (station-crs-code (display-train-from display-train))
					 (station-crs-code (display-train-to display-train))
					 (short-time->hrmins (second (display-train-summary display-train)))
					 (short-time->hrmins (fourth (display-train-summary display-train)))))
			     display-trains)
		     ":")))

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

(ucw:defcomponent journey-stops (ucw:standard-component)
  ((journey :initarg :journey :accessor journey :initform ""))
  (:default-initargs :content-type "text/xml;")
  (:render ()
	   (with-slots (journey) journey-stops
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
			     (split-sequence:split-sequence #\: journey)))
		  (stops->div it)
		  (<:body
		   (<:as-is "Sorry details of this journey are not available. Please try the search ")
		   (<:a :href "/" (<:as-is "again"))
		   (<:as-is "."))))))

(defun stops->div (display-trains)
  (let ((i 0)
	(tiploc-loc (trains->location-list display-trains)))
    (<:html
     (<:head
      (if *unpacked*
	  (progn
	    (<:link :rel "stylesheet" :href (css-path "thickbox.css"))
	    (<:link :rel "stylesheet" :href (css-path "prod.css"))
	    (<:script :type "text/javascript" :src (js-path "261007-prod.js"))
	    (<:script :type "text/javascript" :src (js-path "jquery-1.2.1.min.js"))
	    (<:script :type "text/javascript" :src (js-path "jq_gmaps.js")))
	  (progn
	    (<:link :rel "stylesheet" :href (css-path "2210071214.css"))
	    (<:script :type "text/javascript" :src (js-path "261007.min.js"))))
      (if *prod*
	  (<:script 
	   :src 
	   "http://maps.google.com/maps?file=api&v=2&key=ABQIAAAAfzASmKvmIGhgVZBphgbIMhSbOCASmIHfcyCoxgLWrOePWQVFNhScF6BwHPam3my7bT67NmWM1eTM4g" 
	   :type "text/javascript")
	  (<:script 
	   :src 
	   "http://maps.google.com/maps?file=api&v=2&key=ABQIAAAAfzASmKvmIGhgVZBphgbIMhR6tmuByN1FdjigMD8V6EroZwPzZhTLlWSerUDOwnHIKScDxbaZSSKvNA" 
	   :type "text/javascript"))
      (<:script :src "http://www.google-analytics.com/urchin.js" :type "text/javascript")
      (<:script :type "text/javascript"
		(<:as-is "_uacct = 'UA-1779260-1';
                                           urchinTracker();")))
     (<:body :style "overflow: hidden;"
      (<:div :id "map" :style "height: 400px; width: 400px; float :left;")
      (<:div :id "sidebar" :style "width: 220px; height: 400px; float: right; overflow-y: auto;"
	     (<:table :class "stops" 
			  (mapcar #'(lambda (display-train)
				      (let ((summary (display-train-summary display-train))
					    (start (first (display-train-stops display-train))))
					(incf i)
					(<:tbody
					 (<:tr :class "selected"
					       (<:td (<:a :class "map" :id (trim (fifth start)) 
							  :tabindex (cdr (assoc (fifth start) tiploc-loc :test #'string=))
							  (<:as-is (string-capitalize (first summary)))))
					       (<:td (<:as-is (time-ampm (second summary)))))
					 (mapcar #'(lambda (stop)
						     (incf i)
						     (<:tr :class (if (oddp i) "odd" "even")
							   (<:td
							    (<:a :class "map" :id (trim (fifth stop)) 
								 :tabindex (cdr (assoc (fifth stop) tiploc-loc :test #'string=)) 
								 :href "#"
								 (<:as-is (string-capitalize (tiploc->station-name (fifth stop))))))
							   (<:td
							    (<:as-is
							     (time-ampm (multiple-value-bind (hr min)
									    (floor (fourth stop) 100)
									  (format nil "~A:~A" hr min)))))))
						 (rest (display-train-stops display-train))))))
				  display-trains))))
      (let* ((to-tiploc (fifth (car (last (display-train-stops (car (last display-trains)))))))
	     (to (find-station-by-code (gethash to-tiploc *tiploc->crs*))))
	(<:script :type "text/javascript"
		(<:as-is (format nil "// <![CDATA[
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

(defun stops->img (train deptime left)
  (let ((stops (butlast (rest (display-train-stops train)))))
    (print stops)
    (mapcar #'(lambda (stop)
		(let ((stoptime (- (third stop) (fourth stop))))
		  (<:img :class (string-downcase (fifth (display-train-summary train))) 
			 :src (bar-image (fifth (display-train-summary train)) :type 'stop)
			 :style (format nil "z-index:2; ~A" 
					(left-indent 
					 (+ left
					    (round 
					     (mtimedifference-nd 
					      (fourth stop) 
					      (parse-integer (format nil "~A~A" (subseq deptime 0 2) (subseq deptime 3 5))))))))
			 :width (format nil "~A" (if (= 0 stoptime)
						     1
						     stoptime)))))
	    stops)))

(defun start-position (proposed-pos)
  (fourth (first proposed-pos)))

(defun end-position (proposed-pos)
  (second (first (last proposed-pos))))

(defun change-size (change)
  (third change))

(defun slot-size (slot)
  (nth 9 slot))

(defun slot-size-other (slot)
  (nth 11 slot))

(defun slot-start (slot)
  (second slot))

(defun slot-start-other (slot)
  (nth 5 slot))

(defun change-positions (display-trains proposed-pos)
  (let* ((start-pos (start-position proposed-pos))
	 (end-pos (end-position proposed-pos))
	 (changes (remove-if-not #'(lambda (x) (eql 'change (first x))) proposed-pos))
	 (trains (remove-if-not #'(lambda (x) (eql 'train (first x))) proposed-pos))
	 (slots (change-slots trains)))
    (if (> (reduce #'+ changes :key #'third) (+ (- end-pos start-pos) (* 10 (length changes))))
	(if (> (name->pixels "click for details") (- end-pos start-pos))
	    '()
	    (list (list "click for details" (float (/ end-pos 2)) 0)))
	(let ((results (map 'list 
			   #'(lambda (change slot display-train)
			       (list (string-capitalize (short-name (third (display-train-summary display-train))))
				     (if (> (slot-size slot) (change-size change))
					 (- (+ (slot-start slot) (float (/ (slot-size slot) 2)))
					    (float (/ (change-size change) 2)))
					 (if (> (slot-size-other slot) (change-size change))
					       (- (+ (slot-start-other slot) (float (/ (slot-size-other slot) 2))) 
						  (float (/ (change-size change) 2)))
					       0))
				     (change-size change)))
			   changes slots (butlast display-trains))))
	  (aif (or (> (length results) (length (remove-if #'(lambda (res) (zerop (second res))) results)))
		   (run-over-each-other-p results start-pos end-pos))
	       	(if (> (name->pixels "click for details") (- end-pos start-pos))
		    '()
		    (list (list "click for details" (float (/ end-pos 2)) 0)))
	       results)))))

(defun run-over-each-other-p (results start-pos end-pos)
  (or (not (> (second (first results)) start-pos))
      (changes-run-over-each-other-p results)
      (> (+ (second (car (last results))) (third (car (last results))))
	 end-pos)))

(defun changes-run-over-each-other-p (results)
  (if (not (cdr results))
      nil
      (if (> (+ (second (first results)) (third (first results)) 5)
	     (second (second results)))
	  t
	  (changes-run-over-each-other-p (rest results)))))

(defun change-slots (trains)
  (map-subsequent-two #'(lambda (t1 t2)
			  (list 
			   (list 
			    :start (+ (third t1) (fifth t1))
			    :end (fifth t2)
			    :other-start (fifth t1)
			    :other-end (+ (third t2) (fifth t2))
			    :size (- (fifth t2) (+ (third t1) (fifth t1)))
			    :other-size (- (+ (third t2) (fifth t2)) (fifth t1)))))
		      trains #'append))

(defun graph-trains-pos (display-trains deptime durlen)
  (let ((trains (display-journey-summary display-trains)))
    (append (list (list :start 0 :size (name->pixels (short-name (first (first trains))))))
	    (mapcar #'(lambda (display-train) 
			(let* ((train (display-train-summary display-train))
			       (leftlen (round (timedifference-nd (second train) deptime))))
			  (list 'train
				:width (- (round (timedifference (sixth train) "00:00"))
					  (if (= leftlen 0) 10 0))
				:left (- (+ 55 leftlen) (if (= leftlen 0) 0 10)))))
		    display-trains)
	    (when *show-changes*
	      (let ((i 0)
		  (prev (name->pixels (short-name (first (first trains))))))
	      (MAP-SUBSEQUENT-TWO #'(lambda (t1 t2) 
				      (let* ((short-name (short-name (third t1)))
					     (left (change-leftpos t1 t2 deptime short-name prev)))
					(incf i)
					(setf prev (+ left (name->pixels short-name)))
					(list (list 'change
						    :size (name->pixels short-name)
						    :changeat left))))
				  trains #'append)))
	    (list (list :end (- (+ durlen 50) 5) :size (name->pixels (short-name (third (first trains)))))))))

(defun graph-trains-changes (display-trains deptime durlen)
  (let ((trains (display-journey-summary display-trains)))
    (append (list (<:img :class (string-downcase (fifth (first trains))) :src (bar-image (fifth (first trains)) :type 'left)
		       :alt (format nil "~A from ~A" (fifth (first trains)) (first (first trains)))
		       :title (format nil "~A from ~A" (fifth (first trains)) (first (first trains)))
		       :width "5")
		  (<:img :class (string-downcase (fifth (car (last trains)))) :src (bar-image (fifth (car (last trains))) :type 'right)
			 :alt (format nil "arriving ~A at ~A" (third (car (last trains))) (fourth (car (last trains))))
			 :title (format nil "arriving ~A at ~A" (third (car (last trains))) (fourth (car (last trains))))
			 :width "5" :style (left-indent (- (+ durlen 50) 5)))
		  (<:div :class "changeat" 
			 :style "left: 5px;" 
			 (<:as-is (short-name (first (first trains)))))
		  (<:div :class "changeat" 
			 :style (left-indent (+ durlen 65))
			 (<:as-is (short-name (third (car (last trains)))))))
	    (mapcar #'(lambda (display-train) 
			(let* ((train (display-train-summary display-train))
			       (leftlen (round (timedifference-nd (second train) deptime))))
			  (list (<:img :class (string-downcase (fifth train)) 
				       :src (bar-image (fifth train) :type 'main)
				       :alt (format nil "~A leaving at ~A" (fifth train) (second train))
				       :title (format nil "~A leaving ~A at ~A" (fifth train) (first train) (second train))
				       :width (format nil "~A" (- (round (timedifference (sixth train) "00:00")) 
								  (if (= leftlen 0) 10 0)))
				       :style (format nil "left: ~Apx;" (- (+ 55 leftlen) (if (= leftlen 0) 0 10)))))))
		    display-trains)
	    (mapcar #'(lambda (change-pos)
			(<:div :class "changeat" 
			       :style (format nil "left: ~Apx;" 
					      (second change-pos))
			       (<:as-is (first change-pos))))
		    (if *show-changes*
			(change-positions display-trains (graph-trains-pos display-trains deptime durlen))
			(list (list "click for details" (float (/ (end-position (graph-trains-pos display-trains deptime durlen)) 2)) 0))))
	    (let ((i 0)
		  (prev (name->pixels (short-name (first (first trains))))))
	      (MAP-SUBSEQUENT-TWO #'(lambda (t1 t2) 
				      (let* ((short-name (short-name (third t1)))
					     (left (change-leftpos t1 t2 deptime short-name prev)))
					(incf i)
					(setf prev (+ left (name->pixels short-name)))
					(list (<:img :class "change" :src (image-path "change.png")
						     :alt (if *show-changes* (third t1) "change here")
						     :title (if *show-changes* (third t1) "")
						     :width (format nil "~A" (round (timedifference-nd (second t2) (fourth t1))))
						     :style (format nil 
								    "left: ~Apx;" 
								    (- (+ 55 (round (timedifference-nd (fourth t1) deptime))) 10))))))
				  trains #'append)))))

(defun change-leftpos (t1 t2 deptime name prev)
  (let* ((change-window (round (timedifference-nd (second t2) (fourth t1))))
	 (last-end-point (+ 60 (round (timedifference-nd (fourth t1) deptime))))
	 (name-length (name->pixels name))
	 (mid-point (floor (/ change-window 2)))
	 (it (- (+ change-window last-end-point) name-length)))
    (if (> 5 (abs (- it prev)))
	(if (> mid-point name-length)
	    (+ last-end-point mid-point 5)
	    (+ 10 it))
	(+ prev 10))))

(defun name->pixels (name)
  (* (length name) 5.5))

(defun short-name (name)
  (let ((frags (cl-ppcre:split "[ -]" name))	
	(result '()))
    (dolist (frag frags)
      (if (member (string-downcase frag) *common-names* :test #'string=)
	  (push (string-capitalize (shortened-name (string-downcase frag))) result)
	  (push (string-capitalize frag) result)))
    (reduce #'(lambda (a1 a2) (concatenate 'string a1 " " a2)) (reverse result))))

(defun shortened-name (name)
  (casequal name
    ("airport" "apt")
    ("north" "nth")
    ("south" "sth")
    ("junction" "jn")
    ("london" "lon")
    ("east" "est")
    ("west" "wst")
    ("harbour" "hbr")
    (t (let* ((n (car (split-sequence:split-sequence #\- name)))
	      (l (length n)))
	 (trim (concatenate 'string (subseq n 0 1)
			    (cl-ppcre:regex-replace-all "[aeiouAEIOU]" (subseq n 1 (1- l)) "")
			    (subseq n (1- l) l)))))))
    
(defun lines (s e left height)
  (<:div :id "lines" :style (format nil "left: ~Apx; height: ~Apx;" left height)
	 (<:div 
	  (map1-n #'(lambda (n) 
		      (list (<:div :class (if (oddp n)
					      "even"
					      "odd")
				   :style (format nil "left: ~Apx;" (* 60 (- n 4))))))
		  (- e s)))))

(defun timebar (start end left)
  (<:div :id "markers" :style (format nil "left: ~Apx;" left)
	 (<:div 
	  (map1-n #'(lambda (n) 
		      (list (<:div :class (if (oddp n) 
					    "odd"
					    "even")
				:style (format nil "left: ~Apx;" (* 60 (- n 4)))
				(<:as-is (let ((hour (mod (+ start n) 24)))
					   (hour->stylized-time hour))))))
		    (- end start)))))

(defun start-2-end (journeys)
  (multiple-value-bind (sea mia hoa daa moa yea dda dsta1 tza1)
      (decode-universal-time (now) -1)
    (declare (ignore sea yea dda daa moa dsta1 tza1 hoa) (fixnum mia))
    (multiple-value-bind (x y) (ceiling mia 15)
      (declare (ignore x))
      (map0-n #'(lambda (n)
		  (multiple-value-bind (se mi ho da mo ye dd dst1 tz1)
		      (decode-universal-time (+ (now) (- (* 3600 n) (* y 60))) -1)
		    (declare (ignore se ye dd dst1 tz1) (fixnum ho mi))
		    (list (format nil "~A" da)
			  (aref +month-names+ (1- mo))
			  ho 
			  mi)))
	      journeys))))

(ucw:defcomponent errorfeedback (ucw:standard-component)
  ((user :initarg :user :accessor user)
   (from :initarg :from :accessor from :initform nil)
   (to :initarg :to :accessor to :initform nil)
   (leaving :initarg :leaving :accessor leaving :initform "")
   (leaving-time :initarg :leaving-time :accessor leaving-time :initform ""))
  (:default-initargs :title "telltrains: error has occured"
    :content-type "text/html; charset=utf-8;")
  (:render ()
	   (with-slots (from to leaving) errorfeedback
	     (<:html
	      (<:head
	       (<:title (<:as-is "Telltrains: an error has occured"))
	       (<:link :rel "icon" :href (image-path "transparent.ico"))
	       (<:script :src "http://www.google-analytics.com/urchin.js" :type "text/javascript")
	       (<:script :type "text/javascript"
			 (<:as-is "_uacct = 'UA-1779260-1';
                                           urchinTracker();")))
	      (<:body 
	       (<:h1 (<:a :href "/" (<:as-is "Telltrains")))
	       (<:p
		(<:as-is "oops Telltrains broke! Here are a range of excuses....")
		(<:ol 
		 (<:li "We are still in beta")
		 (<:li "Your search was probably too complicated and we didn't finish evaluating the journeys.")
		 (<:li (<:as-is (format nil "Maybe taking a train to ~A isn't a very good idea" to))))
		(<:as-is "I've logged this as a problem, and it will be fixed soon.")(<:br)
		(<:as-is "Please leave us your email address so that we can write back to you when it is fixed.")(<:br)
		(<:as-is "Don't worry we won't spam you, We hate the stuff.")(<:br) (<:br) 
		(let ((email ""))
		  (<ucw:simple-form :method "get"
				    :action (accept-email-address errorfeedback email)
				    (<:div
				     (<:label :for "email" "Email address:")
				     (<ucw:text :name "email" 
						:id "useremail"
						:autocomplete "off" 
						:size 40
						:maxlength "80"
						:class "field text"
						:accessor email)
				     (<:input :id "submit" :class "btTxt" :type "submit" :value "Submit")))))
	       (<:p
		(<:a :href "/" (<:as-is "You could try again, this has been known to work."))))))))

(ucw:defcomponent givefeedback (ucw:standard-component)
  ((user :initarg :user :accessor user)
   (from :initarg :from :accessor from :initform nil)
   (to :initarg :to :accessor to :initform nil)
   (leaving :initarg :leaving :accessor leaving :initform "")
   (leaving-time :initarg :leaving-time :accessor leaving-time :initform ""))
  (:default-initargs :title "telltrains: error has occured"
    :content-type "text/html; charset=utf-8;")
  (:render ()
	   (with-slots (from to leaving) givefeedback
	     (<:html
	      (<:head
	       (<:title (<:as-is "Telltrains: please tell us what you think"))
	       (<:link :rel "icon" :href (image-path "transparent.ico"))
	       (<:script :src "http://www.google-analytics.com/urchin.js" :type "text/javascript")
	       (<:script :type "text/javascript"
			 (<:as-is "_uacct = 'UA-1779260-1';
                                           urchinTracker();")))
	      (<:body 
	       (<:h1 (<:a :href "/" (<:as-is "Telltrains")))
	       (<:p
		(<:as-is "Telltrains is in beta, we would love to hear suggestions, ideas, opinion or criticism.")
		(<:br) (<:br) 
		(let ((email "") (comments ""))
		  (<ucw:simple-form :method "get"
				    :action (accept-feedback1 givefeedback email comments)
				    (<:div
				     (<:div
				     (<:label :for "email" "Email address:")
				     (<ucw:text :name "email" 
						:id "useremail"
						:autocomplete "off" 
						:size 40
						:maxlength "80"
						:class "field text"
						:accessor email))
				     (<:div
				     (<:label :for "comments" "Comments:")
				     (<ucw:textarea :name "comments" 
						    :id "usercomments"
						    :rows 15
						    :cols 60
						    :class "field text"
						    :accessor comments))
				     (<:input :id "submit" :class "btTxt" :type "submit" :value "Submit")))))
	       (<:p
		(<:a :href "/" (<:as-is "You could try again, this has been known to work."))))))))

(ucw:defaction accept-feedback1 ((givefeedback givefeedback) email comments)
  (progn
    (accept-feedback-db (from givefeedback) (to givefeedback) email comments)
    (ucw:call 'accepted-email-address :from (from givefeedback) :to (to givefeedback) :email email)))

(ucw:defaction accept-email-address ((errorfeedback errorfeedback) email)
  (progn
    (accept-feedback-db (from errorfeedback) (to errorfeedback) email "Error occured")
    (ucw:call 'accepted-email-address :from (from errorfeedback) :to (to errorfeedback) :email email)))

(defun accept-feedback-db (from to email comments)
  (with-train-database
    (clsql:update-records-from-instance
     (make-instance 'feedback 
		    :id (next-id)
		    :from-stn from
		    :to-stn to
		    :timestamp (write-to-string (now))
		    :email email
		    :comments comments))))

(ucw:defcomponent accepted-email-address (ucw:standard-component)
  ((user :initarg :user :accessor user)
   (from :initarg :from :accessor from :initform nil)
   (to :initarg :to :accessor to :initform nil)
   (email :initarg :email :accessor email :initform nil))
  (:default-initargs :title "telltrains: thank you"
    :content-type "text/html; charset=utf-8;")
  (:render ()
	   (with-slots (from to email) accepted-email-address
	     (<:html
	      (<:head
	       (<:title (<:as-is "Telltrains: thank you."))
	       (<:link :rel "icon" :href (image-path "transparent.ico"))
	       (<:script :src "http://www.google-analytics.com/urchin.js" :type "text/javascript")
	       (<:script :type "text/javascript"
			 (<:as-is "_uacct = 'UA-1779260-1';
                                           urchinTracker();")))
	      (<:body 
	       (<:h1 (<:a :href "/" (<:as-is "Telltrains")))
	       (<:p
		(<:as-is "Thank you. We'll get back to you as soon as we have a fix for the problem."))
		(<:a :href "/" (<:as-is "Try again")))))))

(defun timeline-date (leaving)
  (let* ((lo (and leaving (string/= "" leaving) 
		  (or (and (string= (subseq leaving 0 1) "#") (subseq leaving 1 (length leaving))) 
		      leaving)))
	 (l (or (and lo (stylized-day->caldate leaving)) ""))
	 (now (or (and (string/= l "") 
		       (< (- (now) (string->dttm l)) 0)
		       (string->dttm (format nil "~A 01:00:01 +0100 (BST)" l)))
		  (now)))
	 (leaving-date (dttm->string now :format :caldate)))
    (values now leaving-date)))

(ucw:defcomponent cached (ucw:standard-component)
  ((from-crs :initarg :from-crs :accessor from-crs)
   (to-crs :initarg :to-crs :accessor to-crs)
   (leaving :initarg :leaving :accessor leaving :initform ""))
  (:default-initargs :title "telltrains: train times and timetables for british rail commuters"
    :content-type "text/html; charset=utf-8;")
  (:render ()
	   (with-slots (from-crs to-crs leaving) cached
	       (<:as-is
		(with-open-file (in (format nil "/home/vb/sondesh/www/sitemap/html/~A-~A-~A.html" from-crs to-crs "15-10-2007"))
		  (let* ((len (file-length in))
			 (seq (make-array len :element-type 'character :fill-pointer t)))
		    (setf (fill-pointer seq) (read-sequence seq in))
		    seq))))))

(defun real-station-by-letters (letters)
  (let* ((candidates (find-station-by-letters letters))
	 (group (find-station-by-code (route-crs (first candidates)))))
    (if (member group candidates :test #'equal)
	group
	(first candidates))))	

(ucw:defcomponent timeline (ucw:standard-component)
  ((user :initarg :user :accessor user :initform nil)
   (from :initarg :from :accessor from :initform nil)
   (to :initarg :to :accessor to :initform nil)
   (leaving :initarg :leaving :accessor leaving :initform "")
   (leaving-time :initarg :leaving-time :accessor leaving-time :initform "")
   (session-key :initarg :session-key :accessor session-key :initform "")
   (uid :initarg :uid :accessor uid :initform "")									     
   (session :initarg session :accessor session :initform nil))
  (:default-initargs :title "telltrains: train times and timetables for british rail commuters"
    :content-type "text/html; charset=utf-8;")
  (:render ()
	   (with-slots (from to leaving leaving-time user session uid) timeline
	     (multiple-value-bind (now leaving-date) 
		 (timeline-date leaving)
	       (when (string/= uid "")
		 (setf user "facebook"))
	       (multiple-value-bind (se mi ho da mo ye dd dst1 tz1) (decode-universal-time now -1)
		 (declare (ignore se ye dd dst1 tz1 mi da mo) (fixnum ho))
		 (let* ((real-from (if user
				       (find-station-by-exact-name from)
				       (real-station-by-letters from)))
			(real-to (if user
				     (find-station-by-exact-name to)
				     (real-station-by-letters to)))
			(display-trains 
			 (filtertrains-by-time 
			  (if (and from to (not (equal real-from real-to)))
			      (if *debug*
				  (find-trains real-from real-to now)
				  (filter-spurious-journey 
				   (find-trains real-from real-to now)))
			      nil)
			  now))
			(trains (mapcar #'first display-trains)))
		   (if (or (and trains from to) (and (not trains) (not from) (not to)))
	             (<:html
		      (<:head
		       (<:title (<:as-is (if (and from to)
					     (format nil "Telltrains: train times timetable for journeys from ~A to ~A leaving ~A" 
						     (if (string= (station-group-crs-code real-from) (station-crs-code real-from))
							 (format nil "~{~A~^, ~}" 
								 (mapcar (compose #'string-capitalize #'station-display-name #'find-station-by-code) 
									 (topn (sort (copy-list (station-all-crs-codes real-from))
										     #'> :key (compose #'station-rank #'find-station-by-code)) 3)))
							 (string-capitalize (station-display-name real-from)))
						     (if (string= (station-group-crs-code real-to) (station-crs-code real-to))
							 (format nil "~{~A~^, ~}" 
								 (mapcar (compose #'string-capitalize #'station-display-name #'find-station-by-code) 
									 (topn (sort (copy-list (station-all-crs-codes real-to))
										     #'> :key (compose #'station-rank #'find-station-by-code)) 3)))
							 (string-capitalize (station-display-name real-to)))
						     (stylized->day (string->dttm leaving-date)))
					     "Telltrains: train times timetable for train journeys")))
		       (<:link :rel "icon" :href (image-path "transparent.ico"))
		       (<:meta :name "y_key" :content "0a53d0a3da763141")
		       (<:meta :name "verify-v1" :content "HLKhUt6V7k4rQlFX+C8v9z/bL62M/ccksoBiWLVLuBE=")
		       (<:meta :name "robots" :content "index, follow")
		       (<:meta :name "description" :content "New graphical journey planner for Britain's National Rail network. train times; commuter friendly; commuter social network; train running information.")
		       (<:meta :content "national rail, british rail, trains, railway, uk rail travel, train times, train timetables, train information, uk trains, uk railway, late trains, rail commuter, journey planner, social network" :name "keywords")
		       (<:script :src "http://www.google-analytics.com/urchin.js" :type "text/javascript")
		       (<:script :type "text/javascript"
				 (<:as-is "_uacct = 'UA-1779260-1';
                                           urchinTracker();")))
		      (<:body :class "main" :style (if trains "text-align: left;" "text-align: left; overflow:hidden;")
			      (if (and from to)
				  (options trains 
					   (string-upcase (station-display-name real-from)) 
					   (string-upcase (station-display-name real-to)) 
					   ho timeline leaving-date (if (string/= user "") user nil))
				  (options trains "" "" ho timeline leaving-date user))
			      (when trains
				(let ((height (* 20 (length trains))))
				  (<:div :id "outerDiv" 
				       (timebar (- ho 2) (+ ho 36) 180)
				       (<:div :id "innerDiv"
					      (<:div :id "timecontainer"
						     (<:div :id "timeline" 
							    :style (format nil "left: ~Apx; height: ~Apx;" (- 180 (* 60 (- ho 3))) height)
							    (<:div :id "trains"
								   (train-journeys-with-changes display-trains now leaving-date))))
					      (<:div :id "now"
						     :style (format nil "left: 0px; width: ~Apx; height: ~Apx;" 
								    (- (timedifference-nd (current-short-time now) 
											  (if (= ho 0)
											      "23:00"
											      (format nil "~2,'0d:00" (1- ho))))
								       (floor (/ (- now (now)) 60)))
								    (* 1.1 height)))
					      (lines (- ho 1) (+ ho 36) 180 height)))))
			      (when (or (not user) (string= user "exact")) (footer))
			      (if *unpacked*
				  (progn
				    (<:link :rel "stylesheet" :href (css-path "prod.css"))
				    (<:link :rel "stylesheet" :href (css-path "jquery.autocomplete.css"))
				    (<:link :rel "stylesheet" :href (css-path "thickbox.css"))
				    (<:script :type "text/javascript" :src (js-path "jquery-1.2.1.js"))
				    (<:script :type "text/javascript" :src (js-path "261007-prod.js"))
				    (<:script :type "text/javascript" :src (js-path "dimensions-1.0rc1/jquery.dimensions.js"))
				    (<:script :type "text/javascript" :src (js-path "center.js"))
				    (<:script :type "text/javascript" :src (js-path "thickbox.js"))
				    (<:script :type "text/javascript" :src (js-path "jquery.history.js"))
				    (<:script :type "text/javascript" :src (js-path "jquery.autocomplete.js")))
				  (progn
				    (if (string/= uid "")
					(<:link :rel "stylesheet" :href (css-path "fbauto.css"))
					(<:link :rel "stylesheet" :href (css-path "2210071214.css")))
				    (<:script :type "text/javascript" :src (js-path "261007.min.js"))))
			      (<:script :type "text/javascript" 
					:src "http://yui.yahooapis.com/2.2.2/build/yahoo-dom-event/yahoo-dom-event.js")
			      (<:script :type "text/javascript" 
					:src "http://yui.yahooapis.com/2.2.2/build/dragdrop/dragdrop-min.js")
			      (<:script :type "text/javascript" 
					:src "http://yui.yahooapis.com/2.2.2/build/slider/slider-min.js")
			      (<:script :type "text/javascript"
					(let ((number-of-journeys (length trains)))
					  (<:as-is (format nil "// <![CDATA[
                                   var journeys = ~A;
                                   var numberOfJourneys = ~A;
                                   var sliderValues = [~A];
                                   var numRoutes = ~A;
                                   var xFactor = ~A;
                                   var originalleft = ~A;
                                   var stations = '~A';
                                   var pagedate = '~A';
                                   var pattern = ~A;
                                   var fb = ~A;
$(document).ready(function() {
    $('#container').center('horizontal');
    adjust();
    tb_init('a.thickbox, area.thickbox, input.thickbox');//pass where to apply thickbox
    imgLoader = new Image();// preload image
    imgLoader.src = tb_pathToImage;
    $.historyInit(loadtrains);
    $(\"a[@rel='history']\").click(function(){
	    var hash = this.href;
	    hash = hash.replace(/^.*\\//,'');
	    $.historyLoad(hash);
	    return false;
    });

    if (journeys.length >0) {

	slider = YAHOO.widget.Slider.getHorizSlider('tracktime', 'handleleaving',0,360);

	slider.subscribe('change', scrollJourneys);//slideStart, slideEnd
	slider.setValue(journeys[0].starttime, true, true);

	outerDivTop = stripPx($('#outerDiv').css('top'));
	$('#innerDiv').scroll(scrollTrains);
	
	$('div.red').each(function(){
		$(this).click(function(){
			$('#a' + this.id + 'stops').click();});});
    }  
    else {
	$('#fromstation').autocomplete('suggest-stations',
				       {
					   delay:10,
					       minChars:1,
					       onItemSelect:selectFrom,
					       onFindValue:selectFrom,
					       matchSubset:null,
					       cacheData:false,
					       autoFill:false
					       });
                $('#tostation').autocomplete('suggest-stations',
                                             {
                                                delay:10,
						     minChars:1,
						     onItemSelect:selectTo,
						     onFindValue:selectTo,
						     matchSubset:null,
						     cacheData:false,
						     autoFill:false
						     });
                Calendar.setup({
			inputField     :    'leaving',     // id of the input field
			    ifFormat       :    '%d-%m-%Y',     // format of the input field 
			    button         :    'pickdate',  // trigger button (well, IMG in our case)
			    weekNumbers    :    false,
			    singleClick    :    true
			    });
    }
});
                                 // ]]>" 
				 (journeys->json trains ho leaving-date)
				 number-of-journeys
				 (if trains
				     (journey->starttime (first trains))
				     0)
				 (length (routes trains))
				 (xfactor trains)
				 (- 180 (* 60 (- ho 3)))
				 (if trains
				     (format nil "/~A/~A/" 
					     (string-upcase (station-display-name real-from)) 
					     (string-upcase (station-display-name real-to)))
				     "")
				 (dttm->string now :format :caldate)
				 (if trains
				     (time-pattern->json (time-pattern (mapcar (compose #'second #'first #'cadr) trains)))
				     "[]")
				 (if (string= user "facebook") "true" "false")
				 ))))))
		     (ucw:send-redirect 
		      (escape-url (format nil "/errorfeedback/~A/~A/~A/" from to leaving-date))))))))))

(defun footer ()
  (<:div :id "footer"
	 (<:p
	  (<:a :title "About the telltrains team" 
	       :href "/about" 
	       :rel "nofollow"
	       :onClick "javascript:urchinTracker ('/about');"
	       (<:as-is "About"))
	  (<:as-is ".")
	  (<:a :title "The official telltrains blog" 
	       :href "http://telltrains.wordpress.com/" 
	       :onClick "javascript:urchinTracker ('/blog');"
	       (<:as-is "Blog"))
	  (<:as-is ".")
	  (<:a :title "Terms of service" 
	       :href "/terms" 
	       :rel "nofollow"
	       :onClick "javascript:urchinTracker ('/terms');"
	       (<:as-is "Terms"))
	  (<:as-is ".")
	  (<:a :title "Telltrains support and feedback forums" 
	       :href "/forums/index.php" 
	       :onClick "javascript:urchinTracker ('/forums');"
	       (<:as-is "Forums"))
	  (<:as-is ".")
	  (<:a :title "Most popular stations" 
	       :href "/stations" 
	       :onClick "javascript:urchinTracker ('/stations');"
	       (<:as-is "Stations"))	       
	  (<:as-is ".")
	  (<:a :title "Contact us" 
	       :href "/contact" 
	       :rel "nofollow"
	       :onClick "javascript:urchinTracker ('/contact');"
	       (<:as-is "Contact")))))

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

(defun time-pattern (times)
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (time times)
      (awhen (split-sequence:split-sequence #\: time)
	(if (gethash (first it) ht)
	    (push (second it) (gethash (first it) ht))
	    (setf (gethash (first it) ht) (list (second it))))))
    ht))

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

(defun journey->starttime (journey)
  (float (* 360/1440 (- (short-time->mins (second (car (cadr journey)))) 60))))

(defun xfactor (trains)
  (aif (get-start-range trains)
       (/ (timedifference-nd (second it) (first it)) 18 (length trains))
       1))

(defun changes-list (journeys)
  (mapcar #'(lambda (changes) (cons (format nil "~A" changes)
				    (cond ((= changes 1) "Direct train")
					  (t (format nil "~A or fewer changes" (1- changes))))))
	  (sort 
	   (delete-duplicates 
	    (mapcar #'(lambda (journey) 
			(let ((legs (cadr journey)))
			  (length legs)))
		    journeys))
	   #'<)))

(defun route-text (routes)
  (let ((i -1))
    (mapcar #'(lambda (route) (cons (incf i)
				    (if (cdr route)
					(format nil "Change at ~{~A~^ then ~}" (cdr route))
					(format nil "Direct train from ~A" (car route)))))
	    routes)))

(defmethod ucw:update-url ((component homepage) url)
  (make-instance 'ucw::uri
		 :path "timetable"
		 :query '()
		 ))

(defun url-params (fromcrs tocrs from to leaving)
  (format nil "/~A/~A/~A" 
	  (if (string/= "" fromcrs)
	      (station-display-name (find-station-by-code fromcrs))
	      from)
	  (if (string/= "" tocrs)
	      (station-display-name (find-station-by-code tocrs))
	      to)
	  (if leaving
	      leaving
	      "")))

(defun timeline-url (fromcrs tocrs from to leaving)
  (escape-url (format nil 
		      (if (and tocrs fromcrs (string/= tocrs "") (string/= fromcrs ""))
			  "/traintimes~A" 
			  "/timetable~A")
		      (url-params fromcrs tocrs from to leaving))))

(ucw:defaction find-single-fares ((timeline timeline) from to leaving travelers leaving-time) 
  (ucw:send-redirect (timeline-url "" "" from to leaving)))

(ucw:defaction find-single-fares ((homepage homepage) from to leaving travelers leaving-time)
  (ucw:call 'timeline :from from :to to :leaving leaving :leaving-time leaving-time))

(ucw:defaction find-single-times ((timeline timeline) from to leaving travelers leaving-time fromcrs tocrs)
  (ucw:send-redirect (timeline-url fromcrs tocrs from to leaving)))

(ucw:defaction fbjourneys ((timeline timeline) fromcrs tocrs uid)
  (progn
    (save-mycommute uid fromcrs tocrs)
    (ucw:call 'timeline :uid uid :from (station-display-name (find-station-by-code fromcrs)) :to (station-display-name (find-station-by-code tocrs)) :user "facebook")))

(ucw:defaction return-journey ((timeline timeline))
  (ucw:send-redirect (timeline-url "" "" (to timeline) (from timeline) (leaving timeline))))

(ucw:defaction feedback ((timeline timeline))
  (ucw:send-redirect (escape-url (format nil "/givefeedback/~A/~A/~A/" (from timeline) (to timeline) (leaving timeline)))))

(defun options (trains from to ho cmp leaving-date &optional (user nil))
  (<:div :id "container"
	 (when (or (not user) (string= user "exact")) (<:h1 (<:a :href "/" (<:as-is "Telltrains - beta"))))
	 (if (not trains)
	     (let ((newfrom "") (newto "") (leaving "") (travelers 1) (leaving-time "") (fromcrs "") (tocrs ""))
	       (<ucw:simple-form :method "get"
			   :class "telltrains"
			   :onsubmit (when (and user (string= user "results"))
				       "parent.addjourney($('#fromstation').get()[0].value,$('#tostation').get()[0].value); 
                                        parent.tb_remove(); return false;")
			   :action (if (and (uid cmp) (string/= (uid cmp) ""))
				       (fbjourneys cmp fromcrs tocrs (uid cmp))
				       (find-single-times cmp newfrom newto leaving travelers leaving-time fromcrs tocrs))
			   (<:div :class "info"
				  (<:h2 (<:as-is (if user "" "Search for journeys"))))
			   (<:div :id "journey"
				  (<:ul :class "fromto"
					(<:li :class "formlist"
					      (<:span :class "formlist"
						      (<ucw:text :name "from" 
								 :id "fromstation" 
								 :autocomplete "off" 
								 :size 22
								 :maxlength "50"
								 :class "field text"
								 :value from
								 :accessor newfrom)
						      (<ucw:hidden :id "fromcrs" :name "fromcrscode" :accessor fromcrs)
						      (<:label :for "from" "From (city or station)"))
					      (<:span :class "formlist"
						      (<ucw:text :name "to" 
								 :id "tostation" 
								 :autocomplete "off" 
								 :size 22
								 :maxlength "50"
								 :class "field text"
								 :value to
								 :accessor newto)
						      (<ucw:hidden :id "tocrs" :name "tocrscode" :accessor tocrs)
						      (<:label :for "to" "To (city or station)"))
					      (unless user
						(<:span :class "formlist"
						      (<ucw:text :id "leaving"
								 :name "leaving"
								 :class "field text"
								 :size 10
								 :maxlength "50"
								 :value (dttm->string (now) :format :caldate)
								 :accessor leaving)
						      (<:label :for "leaving" "DD-MM-YYYY"))
					      (<:a :id "pickdate" 
						   :class "button"
						   (<:img :class "icon" 
							  :src (image-path "calendar.gif")
							  :alt "Pick date.")))
					      (<:div :class "auto_complete"
						     :id "fromstation_values"
						     :style "display: none; z-index: 10;")
					      (<:input :id "search" :class "button" :type "submit" 
						       :value (if user 
								  (if (string= "facebook" user)
								      "Setup commute"
								      "Add journey")
								  "Search")))))))
	     (unless (string= user "facebook")
	       (<:div :class "info"
		    (<:h2 
		     (<:as-is "Trains from ")
		     (<:a :href (format nil "/journeysfrom/~A" (string-upcase from))
			  :class "journeylinks"
			  (<:as-is (string-capitalize from)))
		     (<:as-is " to ")
		     (<:a :href (format nil "/journeysfrom/~A" (string-upcase to))
			  :class "journeylinks"
			  (<:as-is (string-capitalize to))))
		    (<:div :id "return" :style "right: 0px; position: absolute; margin: -20px 20px 5pt;"
			   (<:a :href "/"
				:title "Home"
				:class "journeylinks"
				(<:as-is "Home"))
			   (<:as-is ".")
			   (<:a :href (timeline-url "" "" (to cmp) (from cmp) (leaving cmp))
				:title "Show return trip"
				:class "journeylinks"
				(<:as-is "Return trip"))
			   (<:as-is ".")
			   (<:a :href "/addjourney?TB_iframe=true"
				:class "thickbox journeylinks" 
				:title "Add another journey to page"
				(<:as-is "Add journey"))
			   ))))
    (when trains		
      (<:div :id "sliders"
	     (<:div :id "tracktime"
		    (<:div :class "hour" :style "left: 15px;" (<:as-is "2"))
		    (<:div :class "hour" :style "left: 45px;" (<:as-html "4"))
		    (<:div :class "hour" :style "left: 75px;" (<:as-is "6"))
		    (<:div :class "hour" :style "left: 105px;" (<:as-is "8"))
		    (<:div :class "hour" :style "left: 135px;" (<:as-is "10"))
		    (<:div :class "hour" :style "left: 165px;" (<:as-is "12"))
		    (<:div :class "hour" :style "left: 195px;" (<:as-is "2"))
		    (<:div :class "hour" :style "left: 225px;" (<:as-is "4"))
		    (<:div :class "hour" :style "left: 255px;" (<:as-is "6"))
		    (<:div :class "hour" :style "left: 285px;" (<:as-is "8"))
		    (<:div :class "hour" :style "left: 315px;" (<:as-is "10"))
		    (<:div :class "hour" :style "left: 345px;" (<:as-is "12"))
		    (<:div :id "handleleaving" :class "sliderhandle"))
	     (if (string= user "facebook")
		 (<:div :id "nextprev"
			(<:as-html (concatenate 'string "Departing...  " 
						(stylized->day (string->dttm leaving-date)))))
		 (<:div :id "nextprev"
		      (links-and-date leaving-date from to)
		      (<:div :id "loadgif")))))))

(defun escape-url (url)
  (cl-ppcre:regex-replace-all " " url "%20"))

(defun links-and-date-ajax (leaving-date &key (new-id-p nil))
  (<:div :id (if new-id-p (format nil "date~A" leaving-date) "date")
	 (<:as-html "Departing...  ")
	 (when (> (days-from-today-date leaving-date) 0)
	   (<:a :id (if new-id-p (format nil "prevday~A" leaving-date) "prevday")
		:rel "history"
		:href (format nil "/~A" (dttm->string (- (string->dttm leaving-date) (* 24 3600)) 
						      :format :caldate))
		:title "previous day"
		(<:as-html "<<"))
	   (<:as-is " "))
	 (<:span :id (if new-id-p (format nil "datestring~A" leaving-date) "datestring")
		 (<:as-is (if (string/= "" leaving-date)
			      (concatenate 'string (stylized->day (string->dttm leaving-date)) " (" leaving-date ")")
			      leaving-date)))
	 (<:as-is " ")
	 (<:a :id (if new-id-p (format nil "nextday~A" leaving-date) "nextday")
	      :rel "history" 
	      :title "next day"
	      :href (format nil "/~A" (dttm->string (+ (string->dttm leaving-date) (* 24 3600)) 
						    :format :caldate))
	      (<:as-html ">>"))))

(defun links-and-date (leaving-date from to &key (new-id-p nil))
  (<:div :id (if new-id-p (format nil "date~A" leaving-date) "date")
	 (<:as-html "Departing...  ")
	 (when (> (days-from-today-date leaving-date) 0)
	   (<:a :id (if new-id-p (format nil "prevday~A" leaving-date) "prevday")
		:href (timeline-url 
		       ""
		       ""
		       from
		       to
		       (dttm->string (- (string->dttm leaving-date) (* 24 3600)) 
				     :format :caldate))
		(<:as-html "<<"))
	   (<:as-is " "))
	 (<:span :id (if new-id-p (format nil "datestring~A" leaving-date) "datestring")
		 (<:as-is (if (string/= "" leaving-date)
			      (concatenate 'string (stylized->day (string->dttm leaving-date)) " (" leaving-date ")")
			      leaving-date)))
	 (<:as-is " ")
	 (<:a :id (if new-id-p (format nil "nextday~A" leaving-date) "nextday")
	      :href (timeline-url
		     ""
		     ""
		     from
		     to
		     (dttm->string (+ (string->dttm leaving-date) (* 24 3600)) 
				   :format :caldate))
	      (<:as-html ">>"))))

(defun remove-immediate-duplicates (list &optional (acc '()))
  (if (not list)
      (reverse acc)
      (if (member (first list) (rest list) :test #'string=)
	  (remove-immediate-duplicates (rest list) acc)
	  (remove-immediate-duplicates (rest list) (push (first list) acc)))))

(defun start-range-trains (trains ho)
  (let ((plus-one nil))
    (aif (remove-immediate-duplicates
	  (mapcar #'(lambda (ordered-journey)
		      (let ((journey (car (cdr ordered-journey))))
			(awhen (first (split-sequence:split-sequence #\: (journey-start journey)))
			  (if (and (>= (parse-integer it) ho) (not plus-one))
			      it
			      (progn
				(setf plus-one t)
				(concatenate 'string it "+1"))))))
		  trains))
	 (if (second (split-sequence:split-sequence #\+ (car (last it))))
	     (append it (list (format nil "~A+1" (1+ (parse-integer 
						      (first 
						       (split-sequence:split-sequence #\+ (car (last it)))))))))
	     (append it (list (format nil "~A" (1+ (parse-integer 
						    (first 
						     (split-sequence:split-sequence #\+ (car (last it)))))))))))))
			      

(defun end-range-trains (trains ho)
  (let ((plus-one nil))
    (aif (remove-immediate-duplicates
	  (mapcar #'(lambda (ordered-journey)
		      (let ((journey (car (cdr ordered-journey))))
			(awhen (first (split-sequence:split-sequence #\: (journey-end journey)))
			  (if (and (>= (parse-integer it) ho) (not plus-one))
			      it
			      (progn
				(setf plus-one t)
				(concatenate 'string it "+1"))))))
		  trains))
	 (if (second (split-sequence:split-sequence #\+ (car (last it))))
	     (append it (list (format nil "~A+1" (1+ (parse-integer 
						      (first 
						       (split-sequence:split-sequence #\+ (car (last it)))))))))
	     (append it (list (format nil "~A" (1+ (parse-integer 
						    (first 
						     (split-sequence:split-sequence #\+ (car (last it))))))))))))) 

(defun time-options (range type hr)
    (list
     (<:span :class "formlist"
	     (<:select :id (format nil "~A_start" type) :name (format nil "~A_start" type) :class "field select"
		     :onchange "filterJourneys();"
		       (let ((i 0))
			 (mapcar #'(lambda (hour) 
				     (incf i)
				     (if (= i 1)
					 (<:option :value (time->slider (car hour) hr)
						   :selected "selected"
						   (<:as-is (cdr hour)))
					 (<:option :value (time->slider (car hour) hr)
						   (<:as-is (cdr hour)))))
				 range)))
	     "and")
     (<:span :class "formlist"
	     (<:select :id (format nil "~A_end" type) :name (format nil "~A_end" type) :class "field select"
		       :onchange "filterJourneys();"
		       (let ((i 0))
			 (mapcar #'(lambda (hour) 
				     (incf i)
				     (if (= i (length range))
					 (<:option :value (time->slider (car hour) hr)
						   :selected  "selected"
						   (<:as-is (cdr hour)))
					 (<:option :value (time->slider (car hour) hr)
						   (<:as-is (cdr hour)))))
				 range))))))

(defun time->slider (time ho)
  (if (second (split-sequence:split-sequence #\+ time))
      (time->slider-value (first (split-sequence:split-sequence #\+ time)) ho t)
      (time->slider-value time ho)))

(defun time-options-old (startend type hr)
  (when startend
    (let* ((start (first startend))
	   (end (second startend))
	   (range (hour-range start end))
	   (numHours (length range))
	   (ho (if (= hr 0) 23 hr)))
    (list
     (<:span :class "formlist"
	     (<:select :id (format nil "~A_start" type) :name (format nil "~A_start" type) :class "field select"
		     :onchange "filterJourneys();"
		       (let ((i 0))
			 (mapcar #'(lambda (hour) 
				     (incf i)
				     (if (= i 1)
					 (<:option :value (time->slider-value (car hour) (- ho 1))
						   :selected "selected"
						   (<:as-is (cdr hour)))
					 (<:option :value (time->slider-value (car hour) (- ho 1))
						   (<:as-is (cdr hour)))))
				 range)))
	     "and")
     (<:span :class "formlist"
	     (<:select :id (format nil "~A_end" type) :name (format nil "~A_end" type) :class "field select"
		       :onchange "filterJourneys();"
		       (let ((i 0))
			 (mapcar #'(lambda (hour) 
				     (incf i)
				     (if (= i numHours)
					 (<:option :value (time->slider-value (car hour) ho)
						   :selected  "selected"
						   (<:as-is (cdr hour)))
					 (<:option :value (time->slider-value (car hour) ho)
						   (<:as-is (cdr hour)))))
				 range))))))))

(defparameter *next-day-scanner* (cl-ppcre:create-scanner " next day"))

(defun cleanup-hour-range (range)
  (sort
   (mapcar #'(lambda (hour)
	       (if (second (split-sequence:split-sequence #\+ hour))
		   (cons (format nil "~2,'0d:00+1" (first (split-sequence:split-sequence #\+ hour)))
			 (hour->stylized-time-long (parse-integer (first (split-sequence:split-sequence #\+ hour)) 
								  :junk-allowed t) t))
		   (cons (format nil "~2,'0d:00" hour)
			 (hour->stylized-time-long (parse-integer hour)))))
	   range)
   #'(lambda (s1 s2) (let ((s1t (cl-ppcre:all-matches *next-day-scanner* (cdr s1)))
			   (s2t (cl-ppcre:all-matches *next-day-scanner* (cdr s2))))
		       (cond ((or (and s1t s2t) (and (not s1t) (not s2t)))
			      (string< (car s1) (car s2)))
			     (s1t nil)
			     (t t))))))

(defun hour-range (start end)
  (multiple-value-bind (m1 min) 
      (ceiling (parse-integer (second (cl-ppcre:split ":" end))) 60)
    (declare (ignore m1))
    (let ((start-hour (parse-integer (first (cl-ppcre:split ":" start))))
	  (end-hour (parse-integer (first (cl-ppcre:split ":" (add-times end (format nil "0:~A" (* -1 min))))))))
      (map0-n #'(lambda (n) 
		  (let ((hour (parse-integer 
			       (first 
				(cl-ppcre:split 
				 ":" 
				 (add-times (format nil "~A:00" start-hour) (format nil "~A:00" n)))))))
		    (cons (format nil "~2,'0d:00" hour) (hour->stylized-time hour))))
	      (if (and (< start-hour 24) (< end-hour start-hour))
		  (- (+ 24 end-hour) start-hour)
		  (ceiling (timedifference-nd (format nil "~A:00" end-hour) (format nil "~A:00" start-hour)) 60))))))

(defun time-pattern->json (ht)
  (let ((elements '()))
    (maphash #'(lambda (k v)
		 (push (format nil "{hr:~A,mins:[~{~A~^, ~}]}"
			       k (nreverse v))
		       elements))
	     ht)
    (format nil "[~{~A~^, ~}]" (nreverse elements))))

(defun journeys->json (trains ho leaving-date &key (new-id-p nil))
  (let ((routes (routes trains)) (today-p t))
    (format nil "[~{~A~^, ~}]"
	    (mapcar #'(lambda (journey) 
			(let ((legs (cadr journey)))
			  (when today-p
			    (when (string= (dttm->string (now) :format :caldate) leaving-date)
			      (setf today-p
				    (< 0 (timedifference (journey-start legs) (current-short-time))))))
			  (format nil "{id:'J~A~A~A~A',start:~A,end:~A,changes:~A,peak:~A,route:~A,stopping:false,weekend:false,starttime:~A,pos:~A}"
				  (remove #\: (journey-start legs))
				  (remove #\: (journey-end legs))
				  (if today-p
				      leaving-date
				      (dttm->string (+ (string->dttm leaving-date) (* 24 3600)) :format :caldate))
				  (if new-id-p "other" "")
				  (time->slider-value (journey-start legs) ho (< (caar journey) 0))
				  (time->slider-value (journey-end legs) ho (< (cdar journey) 0))
				  (length legs)
				  (peak-time legs)
				  (route-index legs routes)
				  (journey->starttime journey)
				  (timedifference-nd (journey-start legs) (current-short-time)))))
		    trains))))

(defun route-index (legs routes)
  (- (length routes)
     (length (member (extract-route legs) routes :test #'route=))))

(defun extract-route (legs)
  (mapcar #'car legs))

(defun routes (trains)
  (delete-duplicates
   (mapcar #'(lambda (journey) 
	       (extract-route (cadr journey)))
	   trains)
   :test #'route=))

(defun route= (r1 r2)
  (string=
   (reduce #'(lambda(x y) (concatenate 'string x y)) r1)
   (reduce #'(lambda(x y) (concatenate 'string x y)) r2)))

(defun peak-time (legs)
  (let ((start (journey-start legs)))
    (if (and (string> start "06:00")
	     (string< start "10:00"))
	"true"
	"false")))

;loggedinuser
;profileowner

(ucw:defcomponent fbinstall (ucw:standard-component)
  ((installed :initarg :installed :accessor installed :initform "")
   (auth_token :initarg auth_token :accessor auth_token :initform ""))
  (:default-initargs 
      :content-type "text/xml; charset=utf-8;")
  (:render ()
	   (with-slots (installed auth_token) fbinstall
	     (let ((session (if (and auth_token (string/= auth_token ""))
				(facebook:establish-facebook-session *fb-api-key* *fb-secret* auth_token)
				nil)))
	       (print (cons auth_token (describe session)))
	       (<:p (<:as-is "Thanks for installing myCommute"))))))

(ucw:defcomponent fbremove (ucw:standard-component)
  ((removed :initarg :removed :accessor removed :initform "")
   (uid :initarg uid :accessor uid :initform ""))
  (:default-initargs 
      :content-type "text/xml; charset=utf-8;")
  (:render ()
	   (with-slots (removed uid) fbremove
	     (<:p (<fb:name :uid "loggedinuser") (<:as-is " thanks for using myCommute")))))

(defun get-mycommute (uid apikey)
  (when (string= apikey *fb-api-key*)
    (with-train-database
      (first (clsql:query (format nil "select fromcrs, tocrs from MYCOMMUTE where uid='~A'" uid))))))

(defun save-mycommute (uid from to)
  (with-train-database!
    (if (not (get-mycommute uid *fb-api-key*))
	(clsql:update-records-from-instance 
	 (make-instance 'mycommute
			:id (next-id)
			:uid (format nil "~A" uid)
			:fromcrs from
			:tocrs to))
	(clsql:execute-command (format nil "update MYCOMMUTE set fromcrs='~A', tocrs='~A' where uid='~A'" from to uid)))))

(ucw:defcomponent fbin (ucw:standard-component)
  ((user :initarg :user :accessor user :initform nil)
   (from :initarg :from :accessor from :initform nil)
   (to :initarg :to :accessor to :initform nil)
   (leaving :initarg :leaving :accessor leaving :initform "")
   (leaving-time :initarg :leaving-time :accessor leaving-time :initform "")
   (session-key :initarg :session-key :accessor session-key :initform "")
   (uid :initarg :uid :accessor uid :initform "")		
   (setup :initarg :setup :accessor setup :initform nil)
   (return-trip :initarg :return-trip :accessor return-trip :initform nil)
   (home :initarg :home :accessor home :initform t)
   (session :initarg session :accessor session :initform nil))
  (:default-initargs :title "telltrains: train times and timetables for british rail commuters"
    :content-type "text/html; charset=utf-8;")
  (:render ()
	   (with-slots (from to leaving leaving-time user setup return-trip session session-key uid home) fbin
	     (multiple-value-bind (now leaving-date) 
		 (timeline-date leaving)
	       (when (string/= uid "")
		 (setf user "facebook")
		 (update-session uid session-key)
		 (awhen (get-mycommute uid *fb-api-key*)
		   (setf from (first it))
		   (setf to (second it))))
	       (let* ((real-from (if (and user (string= user "exact"))
				       (find-station-by-exact-name from)
				       (if (and user (string= user "facebook"))
					   (find-station-by-code from)
					   (real-station-by-letters from))))
			(real-to (if (and user (string= user "exact"))
				     (find-station-by-exact-name to)
				     (if (and user (string= user "facebook"))
					 (find-station-by-code to)
					 (real-station-by-letters to)))))
		   (if (and real-from real-to (not setup))
		       (<fb:fbml
			(<fb:google-analytics :uacct "UA-1779260-1" 
					      :page (format nil "myCommute?uid=~A&date=~A&time=~A" 
							    uid leaving-date (current-hrmins now)))
			(<fb:dashboard 
			 (<fb:action :href "mycommute?src=live" (<:as-is "Trains"))
			 (<fb:action :href "mycommute?src=return" (<:as-is "Return journey"))
			 (<fb:action :href "mycommute?src=setup" (<:as-is "Setup commute"))
			 (<fb:action :href "mycommute?src=friends" (<:as-is "Invite Friends"))
			 (<fb:action :href "mycommute?src=feedback" (<:as-is "Report problems")))
			(<:div :style "margin: 0pt auto; position: relative; top: -35px; left: 145px; width: 100%;"
			       (<:h3 (<:as-is (format nil ": Trains from ~A to ~A " 
						      (if return-trip
							  (station-display-name real-to)
							  (station-display-name real-from))
						      (if return-trip
							  (station-display-name real-from)
							  (station-display-name real-to))))
				     (<:span :style "color: #BF290B;" " by Telltrains")))
			(<fb:iframe :src (concatenate 'string (if *prod* "http://www.telltrains.com" "http://82.153.32.254" )
						      (if return-trip
							  (timeline-url to
									from								       
									(station-display-name real-to)
									(station-display-name real-from) 
									leaving-date)
							  (timeline-url from 
								    to 
								    (station-display-name real-from) 
								    (station-display-name real-to)
								    leaving-date)))
				    :width "100%"
				    :height "500px;"
				    :frameborder "0"
				    :scrolling "false"
				    :style "position: relative; top: -30px;"
				    ))
		       (<fb:fbml	
			(<fb:google-analytics :uacct "UA-1779260-1" :page (format nil "setupmyCommute?uid=~A" uid))		
			(<fb:dashboard 
			 (<fb:action :href "mycommute?src=live" (<:as-is "Trains"))
			 (<fb:action :href "mycommute?src=return" (<:as-is "Return journey"))
			 (<fb:action :href "mycommute?src=setup" (<:as-is "Setup commute"))
			 (<fb:action :href "mycommute?src=friends" (<:as-is "Invite Friends"))
			 (<fb:action :href "mycommute?src=feedback" (<:as-is "Report problems")))
			(<:div :style "margin: 0pt auto; position: relative; top: -35px; left: 145px; width: 100%;"
			       (<:h3 ": Setup your commute " (<:span :style "color: #BF290B;" " by Telltrains")))
			(<fb:iframe :src (format nil "http://~A/setup~A" 
						  (if *prod* "www.telltrains.com" "82.153.32.254")
						  (if home "home" "work"))
				    :width "100%"
				    :height "300px;"
				    :style "position: relative; top: -20px;"
				    :scrolling "false"
				    :frameborder "0")
			)))))))

(ucw:defaction showcommute ((fbin fbin) fromcrs tocrs)
  (let ((session (session fbin)))
    (save-mycommute (facebook:uid session) fromcrs tocrs)
    (ucw:call 'fbin
	      :from fromcrs
	      :to tocrs
	      :session session)))

(ucw:defaction fbjourneys ((fbin fbin) fromcrs tocrs uid)
  (progn
    (save-mycommute uid fromcrs tocrs)
    (ucw:call 'fbin :uid uid :from (station-display-name (find-station-by-code fromcrs)) :to (station-display-name (find-station-by-code tocrs)) :user "facebook")))

(defun excluded-uids (uid session-key)
  (let ((session (facebook:make-session 
		  :api-key *fb-api-key*
		  :secret *fb-secret*
		  :session-key session-key
		  :uid uid)))
    (facebook:execute-query session
			    (format nil "SELECT uid FROM user WHERE has_added_app=1 and uid IN (SELECT uid2 FROM friend WHERE uid1 = ~A)" uid))))

(ucw:defcomponent invite-friends (ucw:standard-component)
  ((session-key :initarg :session-key :accessor session-key :initform "")
   (uid :initarg :uid :accessor uid :initform ""))
  (:default-initargs :title "telltrains: train times and timetables for british rail commuters"
    :content-type "text/html; charset=utf-8;")
  (:render ()
	   (with-slots (session-key uid) invite-friends
	     (<fb:fbml
	      (<fb:google-analytics :uacct "UA-1779260-1" :page (format nil "inviteFriends?uid=~A" uid))		
	      (<fb:dashboard 
	       (<fb:action :href "mycommute?src=live" (<:as-is "Trains"))
	       (<fb:action :href "mycommute?src=return" (<:as-is "Return journey"))
	       (<fb:action :href "mycommute?src=setup" (<:as-is "Setup commute"))	       
	       (<fb:action :href "mycommute?src=friends" (<:as-is "Invite Friends"))
	       (<fb:action :href "mycommute?src=feedback" (<:as-is "Report problems")))
	      (<:div :style "margin: 0pt auto; position: relative; top: -35px; left: 145px; width: 100%;"
		     (<:h3 " : Invite friends " (<:span :style "color: #BF290B;" " by Telltrains")))
	      (<fb:request-form :action "mycommute?src=frominvite"
				:method "POST"
				:invite "true"
				:type "myCommute"
				:content "Check out the -myCommute- application from Telltrains. It lets you view trains between work and home.<fb:req-choice url=\"http://apps.facebook.com/mycommute?a=ONINSTALL&fbsrc=inviteAddApp\" label=\"Add Application!\" /><fb:req-choice url=\"http://apps.facebook.com/apps/application.php?api_key=4c1a3787cab3d55a42609ad10355d9f3&fbsrc=inviteLearnM\" label=\"Learn More!\" />"
				(<fb:multi-friend-selector
				 :showborder "false" 
				 :actiontext "Invite your friends to myCommute."
				 :exclude_ids (format nil "[~{~A~^, ~}]" (mapcar #'cdar (excluded-uids uid session-key)))
				 :max "20"))))))

(defun get-session (uid)
  (with-train-database
    (caar (clsql:query (format nil "select sessionkey from FBUSER where uid='~A'" uid)))))

(defun update-session (uid session-key)
  (with-train-database!
    (if (not (get-session uid))
	(clsql:update-records-from-instance 
	 (make-instance 'fbuser
			:id (next-id)
			:uid (format nil "~A" uid)
			:sessionkey session-key
			:lastupdate (format nil "~A" (now))))
	(clsql:execute-command (format nil "update FBUSER set sessionkey='~A', lastupdate='~A' where uid='~A'" session-key (now) uid)))
    (let ((session (facebook:make-session 
		    :api-key *fb-api-key*
		    :secret *fb-secret*
		    :session-key session-key
		    :uid uid)))
      (facebook:set-profile-markup session 
				   (aif (get-mycommute uid *fb-api-key*)
					(progn
					  (UpdateProfile session (first it) (second it))					
					  (format nil "<fb:ref handle=\"~A~A\"/>" (first it) (second it)))
					"<fb:if-is-own-profile>
    <fb:profile-action url=\"http://apps.facebook.com/mycommute/mycommute?src=setup\">Setup your commute</fb:profile-action>
    <fb:else>
        <fb:if-is-app-user uid=\"profileowner\">
            <fb:profile-action url=\"http://apps.facebook.com/mycommute/mycommute?src=live\">View <fb:name uid=\"profileowner\" firstnameonly=\"true\" possessive=\"true\" /> commute</fb:profile-action>
            <fb:else>
                <fb:profile-action url=\"http://apps.facebook.com/mycommute/mycommute?src=invite\">Invite <fb:name uid=\"profileowner\" firstnameonly=\"true\" /> to myCommute</fb:profile-action>
            </fb:else>
        </fb:if-is-app-user>
    </fb:else>
</fb:if-is-own-profile>")
				   uid))))

(defun updateProfile (session from to)
  (let* ((first-train (caadar (first (find-trains (find-station-by-code from) (find-station-by-code to)))))
	 (markup (format nil "<fb:if-is-own-profile>
    <fb:profile-action url=\"http://apps.facebook.com/mycommute/mycommute?src=live\">Next train ~A is at ~A</fb:profile-action><fb:wide><p>Hello</p></fb:wide>
    <fb:else>
        <fb:if-is-app-user uid=\"profileowner\">
            <fb:profile-action url=\"http://apps.facebook.com/mycommute/mycommute?src=live\">View <fb:name uid=\"profileowner\" firstnameonly=\"true\" possessive=\"true\" /> commute</fb:profile-action>
            <fb:else>
                <fb:profile-action url=\"http://apps.facebook.com/mycommute/mycommute?src=invite\">Invite <fb:name uid=\"profileowner\" firstnameonly=\"true\" /> to myCommute</fb:profile-action>
            </fb:else>
        </fb:if-is-app-user>
    </fb:else>
</fb:if-is-own-profile>"
			 "home" (second first-train))))
    (setRefHandle session (concatenate 'string from to) markup)))

(defun setRefHandle (session handle markup)
  (facebook:session-request
   session "facebook.fbml.setRefHandle"
   `(("handle" . ,handle)
     ("fbml" . ,markup))))

(defun update-all-sessions ()
  (with-train-database
    (clsql:query "select uid,sessionkey,lastupdate from FBUSER")))

(ucw:defcomponent mycommute-setup (ucw:standard-component)
  ((user :initarg :user :accessor user :initform nil)
   (home :initarg :home :accessor home :initform t)
   (session-key :initarg :session-key :accessor session-key :initform "")
   (uid :initarg :uid :accessor uid :initform "")									     
   (session :initarg session :accessor session :initform nil))
  (:default-initargs :title "telltrains: train times and timetables for british rail commuters"
    :content-type "text/html; charset=utf-8;")
  (:render ()
	   (with-slots (session-key user session uid) mycommute-setup
	     (when (string/= uid "")
	       (setf user "facebook"))
	     (<:html
	      (<:head
	       (<:title (<:as-is "telltrains: train times and timetables for british rail commuters"))
	       (<:link :rel "icon" :href (image-path "transparent.ico"))
	       (<:link :rel "stylesheet" :href (css-path "fbform.css")))
	      (<:body :class "main" :id "setup" :style "text-align: left; overflow:hidden; background: #F7F7F7 none repeat scroll 0%;"
		      (<:div :class "profile_note" 
			     (<:strong "Note:")
			     (<:as-is " Add stations for home and work and pick the times."))
		      (<:div :class "editor_panel clearfix" :id "content"
			     (let ((stationhome "") (stationwork "") (homehr1 "") (homehr2 "") (homemin1 "") (homemin2 "")
				   (workhr1 "") (workhr2 "") (workmin1 "") (workmin2 "") (fromcrs "") (tocrs ""))
			       (<ucw:simple-form :id "profile_form" :name "profile_form" :method "post"
						 :action (fbjourneys mycommute-setup fromcrs tocrs uid)
				       (<:table :cellspacing "0" :border "0" :class "editor basic"
						(<:tbody
						 (<:tr
						  (<:td :class "label" "Time you leave home between")
						  (<:td
						   (<ucw:select :id "homehr1"
								:accessor homehr1
								(<ucw:option :value 6 "6 am")
								(<ucw:option :value 7 "7 am")
								(<ucw:option :value 8 "8 am")
								(<ucw:option :value 9 "9 am")
								(<ucw:option :value 10 "10 am"))
						   (<:as-is " and ")
						   (<ucw:select :id "homehr2"
								:accessor homehr2
								(<ucw:option :value 7 "7 am")
								(<ucw:option :value 8 "8 am")
								(<ucw:option :value 9 "9 am")
								(<ucw:option :value 10 "10 am")
								(<ucw:option :value 11 :selected "true" "11 am"))))
						 (<:tr
						  (<:td :class "label" "Home station:")
						  (<:td (<ucw:text :value "" 
								   :size 19
								   :autocomplete "off"
								   :class "inputtext" 
								   :name "stationhome" 
								   :accessor stationhome
								   :id "homestation")
							(<ucw:hidden :id "fromcrs" :name "fromcrscode" :accessor fromcrs)))
						 (<:tr (<:td :class "label") 
						       (<:td (<:div (<:a :href "#" 
									 :id "home" 
									 :onClick "gm_ShowMap(this); return false;" 
									 "Add another home station..."))))
						 (<:tr (<:td) (<:td (<:div :class "divider")))
						 (<:tr
						  (<:td :class "label" "Time you leave work between")
						  (<:td
						   (<ucw:select :id "workhr1"
								:accessor workhr1
								(<ucw:option :value 15 "3 pm")
								(<ucw:option :value 16 "4 pm")
								(<ucw:option :value 17 "5 pm")
								(<ucw:option :value 18 "6 pm")
								(<ucw:option :value 19 "7 pm")
								(<ucw:option :value 19 "8 pm"))
						   (<:as-is " and ")
						   (<ucw:select :id "workhr2"
								:accessor workhr2
								(<ucw:option :value 16 "4 pm")
								(<ucw:option :value 17 "5 pm")
								(<ucw:option :value 18 "6 pm")
								(<ucw:option :value 19 "7 pm")
								(<ucw:option :value 20 "8 pm")
								(<ucw:option :value 21 :selected "true" "9 pm"))))
						 (<:tr 
						       (<:td :class "label" "Work station:")
						       (<:td (<ucw:text :value "" 
									:size 19
									:class "inputtext" 
									:name "stationwork" 
									:accessor stationwork
									:id "workstation")
							     (<ucw:hidden :id "tocrs" :name "tocrscode" :accessor tocrs)))
						 (<:tr (<:td :class "label") 
						       (<:td (<:div (<:a :href "#" 
									 :id "work" 
									 :onClick "gm_ShowMap(this); return false;" 
									 "Add another work station..."))))
						 (<:tr (<:td) (<:td (<:div :class "divider")))
						 (<:tr
						  (<:td)
						  (<:td 
							(<:div :class "buttons" :style "text-align: left; left: -4px; position: relative; padding: 5px 0px 0px;"
							       (<:input :type "submit" 
									:value "Save Changes" 
									:name "savename" 
									:id "save" 
									:class "inputsubmit")
							       (<:input :type "button" 
									:value "Cancel" 
									:name "cancelname" 
									:id "cancel" 
									:class "inputbutton")))))))))
		      (<:script :type "text/javascript" :src (js-path "011107.js"))
		      (<:script :type "text/javascript"
				(<:as-is 
				 (format nil 
		   "// <![CDATA[
                       var journeys = []; var numberOfJourneys = 0; var sliderValues = []; var numRoutes = 0; var xFactor = 0;
                       var originalleft = 0; var stations = ''; var pagedate = ''; var pattern = 0; var fb = true;
                       $(document).ready(function() {
                          $('#container').center('horizontal');
                          adjust();
                          $('#homestation').autocomplete('suggest-stations',
                                            { delay:10, minChars:1, onItemSelect:selectFrom,
			                      onFindValue:selectFrom, matchSubset:null, cacheData:false, autoFill:false });
                          $('#workstation').autocomplete('suggest-stations',
                                            { delay:10, minChars:1, onItemSelect:selectTo, 
                                              onFindValue:selectTo, matchSubset:null, cacheData:false, autoFill:false });
                         }); // ]]>" ))))))))

(ucw:defaction fbjourneys ((mycommute-setup mycommute-setup) fromcrs tocrs uid)
  (progn
    (save-mycommute uid fromcrs tocrs)
    (ucw:call 'timeline 
	      :uid uid 
	      :from (station-display-name (find-station-by-code fromcrs)) 
	      :to (station-display-name (find-station-by-code tocrs)) :user "facebook")))

