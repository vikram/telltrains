(in-package :com.sondesh.database)

(defmacro links-and-date (leaving-date from to)
  `(with-html-output (*standard-output*)
     (:div :id "date"
	   (fmt "~A" "Departing...  ")
	   (when (> (days-from-today-date ,leaving-date) 0)
	     `(with-html-output (*standard-output*)
		(:a :id "prevday"
		    :rel "nofollow"
		    :href (timeline-url "" "" ,from ,to (dttm->string (- (string->dttm ,leaving-date) (* 24 3600)) :format :caldate))
		    (fmt "~A" "<<"))
		(fmt "~A" " ")))
	   (:span :id "datestring"
		  (fmt "~A" (if (string/= "" ,leaving-date)
				(concatenate 'string (stylized->day (string->dttm ,leaving-date)) " (" ,leaving-date ")")
				,leaving-date)))
	   (fmt "~A" " ")
	   (:a :id "nextday"
	       :rel "nofollow"
	       :href (timeline-url "" "" ,from ,to (dttm->string (+ (string->dttm ,leaving-date) (* 24 3600)) :format :caldate))
	       (fmt "~A" ">>")))))

(defun train-journeys-with-changes (results now leaving-date &key (new-id-p nil) (google-p nil))
  (let ((train-number 1) (today-p t))
    (dolist (result results)
      (setf today-p (train-journey train-number (rest result) now leaving-date today-p new-id-p google-p))
      (incf train-number))))

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

(defun train-journey (train-number display-trains now leaving-date today-p new-id-p google-p)
  (let* ((trains (display-journey-summary display-trains))
	 (tn train-number)
	 (duration (journey-duration trains))
	 (durlen (round (timedifference (journey-duration trains) "00:00")))
	 (deptime (second (first trains)))
	 (arrtime (fourth (car (last trains))))
	 (src (stops->request display-trains))
	 (id (format nil "J~A~A" 
		     (remove #\: deptime) 
		     (remove #\: arrtime))))
    (when today-p
      (when (string/= (dttm->string (now) :format :caldate) leaving-date)
	(setf today-p
	      (< 0 (timedifference (journey-start trains) (current-short-time))))))
    (setf id (format nil "~A~A" id 		     
		     (if today-p
			 leaving-date
			 (dttm->string (+ (string->dttm leaving-date) (* 24 3600)) :format :caldate))))
    (with-html-output (*standard-output*)
      (:div :class "red" :style (format nil "top: ~Aem; left: ~Apx; width: ~Apx;" 
				      (+ 1 (* 3 (- tn 1))) ;train number
				      (ALIGN-TIME deptime now)
				      (+ 400 durlen))
	   :id (format nil "~A~A" 
		       id 
		       (if new-id-p
			   "other"
			   ""))
	   (:div :class "deptime" (fmt "~A" (time-ampm deptime)))
	   (:div :class "arrtime" :style (left-indent (+ durlen 65))
		 (fmt "~A (~A)" (time-ampm arrtime) (time-hrmin duration))
		 (:a :href (format nil src)
		     :rel "nofollow"
		     :onclick (format nil "javascript:urchinTracker(\"~A\");" src)
		     :id (format nil "a~Astops" id) 
		     :class "thickbox"
		     :title (format nil "Leaving ~A at ~A arriving ~A at ~A (~A)" 
				    (string-capitalize (first (first trains)))
				    (time-ampm (second (first trains)))
				    (string-capitalize (third (car (last trains))))
				    (time-ampm (fourth (car (last trains))))
				    (time-hrmin (journey-duration trains)))))
	   (cond ((= (length trains) 1) 
		  (with-html-output (*standard-output*)
		    (:img :class (string-downcase (fifth (first trains))) 
			  :src (format nil "~A" (bar-image (fifth (first trains)) :type 'left))
			  :alt (format nil "~A from ~A to ~A" (fifth (first trains)) (first (first trains)) (third (first trains)))
			  :title (format nil "~A from ~A to ~A" (fifth (first trains)) (first (first trains)) (third (first trains)))
			  :width "5")
		    (:img :class (string-downcase (fifth (first trains))) 
			  :src (format nil "~A" (bar-image (fifth (first trains)) :type 'main))
			  :alt (format nil "leaving at ~A" (second (first trains)))
			  :title (format nil "leaving at ~A" (second (first trains)))
			  :width (format nil "~A" (- durlen 10)) :style "left: 55px;")
		    (:img :class (string-downcase (fifth (first trains))) 
			  :src (format nil "~A" (bar-image (fifth (first trains)) :type 'right))
			  :alt (format nil "arriving ~A at ~A" (third (first trains)) (fourth (first trains)))
			  :title (format nil "arriving ~A at ~A" (third (first trains)) (fourth (first trains)))
			  :width "5" 
			  :style (left-indent (- (+ durlen 50) 5)))
		    (:div :class "changeat" 
			  :style "left: 0px; font-size: 7.75pt;" 
			  (fmt "~A" (aif (first (first trains))
					 (if google-p it (short-name it)))))
		    (:div :class "changeat" 
			  :style (format nil "left:~Apx; font-size: 7.75pt;"
				  (if (> (name->pixels (short-name (first (first trains)))) (+ durlen 65))
				      (+ (name->pixels (short-name (first (first trains)))) 20)
				      (+ durlen 65)))
			  (fmt "~A" (aif (third (first trains))
					 (if google-p it (short-name it)))))))
		 (t (graph-trains-changes display-trains deptime durlen google-p)))))
    today-p))

(defun graph-trains-changes (display-trains deptime durlen google-p)
  (let ((trains (display-journey-summary display-trains)))
    (with-html-output (*standard-output*)
      (:img :class (string-downcase (fifth (first trains))) 
	    :src (format nil "~A" (bar-image (fifth (first trains)) :type 'left))
	    :alt (format nil "~A from ~A to ~A leaving at ~A arriving at ~A" 
			 (fifth (first trains)) (first (first trains)) 
			 (third (car (last trains))) (second (first trains)) (fourth (car (last trains))))
	    :title (format nil "~A from ~A" (fifth (first trains)) (first (first trains)))
	    :width "5")
      (:img :class (string-downcase (fifth (car (last trains)))) 
	    :src (format nil "~A" (bar-image (fifth (car (last trains))) :type 'right))
	    :alt (format nil "arriving ~A at ~A" (third (car (last trains))) (fourth (car (last trains))))
	    :title (format nil "arriving ~A at ~A" (third (car (last trains))) (fourth (car (last trains))))
	    :width "5" :style (left-indent (- (+ durlen 50) 5)))
      (:div :class "changeat" 
	    :style "left: 5px; font-size: 7.75pt;" 
	    (fmt "~A" (aif (first (first trains)) (if google-p it (short-name it)))))
      (:div :class "changeat" 
	    :style (format nil "left: ~Apx; font-size: 7.75pt;" (+ durlen 65))
	    (fmt "~A" (aif (third (car (last trains))) (if google-p it (short-name it)))))
      (loop for (train . leftlen) in 
	   (mapcar #'(lambda (display-train)
		       (cons (display-train-summary display-train)
			     (round (timedifference-nd (second (display-train-summary display-train)) deptime))))
		   display-trains)
	   do (htm
	       (:img :class (string-downcase (fifth train)) 
		     :src (format nil "~A" (bar-image (fifth train) :type 'main))
		     :alt (format nil "~A leaving at ~A" (fifth train) (second train))
		     :title (format nil "~A leaving ~A at ~A" (fifth train) (first train) (second train))
		     :width (format nil "~A" (round-to-zero (- (round (timedifference (sixth train) "00:00"))
							       (if (= leftlen 0) 10 0))))
		     :style (format nil "left: ~Apx;" (- (+ 55 leftlen)
							 (if (= leftlen 0) 0 10))))))
      (loop for change-pos in (if google-p
				  (list (list "click for details" 
					      (float (/ (end-position 
							 (graph-trains-pos display-trains deptime durlen)) 2)) 0))
				  (change-positions display-trains (graph-trains-pos display-trains deptime durlen)))
	   do (htm
	       (:div :class "changeat" 
		     :style (format nil "left: ~Apx; font-size: 6.75pt;" (second change-pos))
		     (fmt "~A" (first change-pos)))))
      (loop for (t1 . t2) in (reverse (pairs trains))
	   do (htm
	       (:img :class "change" 
		     :src "/images/change.png"
		     :alt (third t1)
		     :title (third t1)
		     :width (format nil "~A" (round (timedifference-nd (second t2) (fourth t1))))
		     :style (format nil "left: ~Apx;" 
				    (- (+ 55 (round (timedifference-nd (fourth t1) deptime))) 10))))))))

(defun round-to-zero (number)
  (if (> 0 number)
      0
      number))

(defun timebar (start end left)
  (with-html-output (*standard-output*)
     (:div :id "markers" :style (format nil "left: ~Apx;" left)
	   (:div 
	    (loop for i from 1 to (- end start)
		 do (htm
		     (:div :class (if (oddp i) "odd" "even")
			   :style (format nil "left: ~Apx;" (* 60 (- i 4)))
			   (fmt "~A" (hour->stylized-time (mod (+ start i) 24))))))))))

(defun lines (s e left height)
  (with-html-output (*standard-output*)  
    (:div :id "lines" :style (format nil "left: ~Apx; height: ~Apx;" left height)
	  (:div 
	   (loop for i from 1 to (- e s)
	      do (htm
		  (:div :class (if (oddp i) "even" "odd")
			:style (format nil "left: ~Apx;" (* 60 (- i 4))))))))))

(defun journey-page (display-trains real-from real-to leaving-date now)
  (let* ((from (string-capitalize (station-display-name real-from)))
	 (to (string-capitalize (station-display-name real-to)))
	 (from-station-names (if (string= (station-group-crs-code real-from) (station-crs-code real-from))
				 (format nil "~{~A~^, ~}" 
					(mapcar (compose #'string-capitalize #'station-display-name #'find-station-by-code) 
						(topn (sort (copy-list (station-all-crs-codes real-from))
							    #'> :key (compose #'station-rank #'find-station-by-code)) 3)))
				(string-capitalize (station-display-name real-from))))
	 (to-station-names (if (string= (station-group-crs-code real-to) (station-crs-code real-to))
			      (format nil "~{~A~^, ~}" 
				      (mapcar (compose #'string-capitalize #'station-display-name #'find-station-by-code) 
					      (topn (sort (copy-list (station-all-crs-codes real-to))
							  #'> :key (compose #'station-rank #'find-station-by-code)) 3)))
			      (string-capitalize (station-display-name real-to))))
	 (trains (mapcar #'first display-trains))
	 (number-of-journeys (length trains))
	 (height (* 20 number-of-journeys))
	 (google-p nil) ;;(or t (cl-ppcre:all-matches-as-strings "Googlebot" (user-agent))))
	 (now (string->dttm (dttm->string now :format :caldate))))
    (multiple-value-bind (se mi ho da mo ye dd dst1 tz1) (decode-universal-time now 0)
      (declare (ignore se ye dd dst1 tz1) (fixnum ho))
      (multiple-value-bind (se1 mi1 ho1 da1 mo1 ye1 dd1 dst11 tz11) (decode-universal-time (now) 0)
	(declare (ignore se1 ye1 dd1 dst11 tz11) (fixnum ho1))
      (with-html
	(:html
	 (:head
	  (:title (fmt "Telltrains: train times from ~A to ~A leaving ~A" 
		       from-station-names
		       to-station-names
		       (stylized->day (string->dttm leaving-date))))
	  (:link :rel "icon" :href "/images/transparent.ico")
	  (:meta :name "y_key" :content "0a53d0a3da763141")
	  (:meta :name "verify-v1" :content "HLKhUt6V7k4rQlFX+C8v9z/bL62M/ccksoBiWLVLuBE=")
	  (:meta :name "robots" :content "index, follow")
	  (:meta :name "description" :content "Telltrains is a new graphical journey planner for British National Rail network. train times; commuter friendly; commuter social network; train running information.")
	  (:meta :content (format nil "unofficial, national rail, british rail, timetable, trains, railway, uk rail travel, train times, train timetables, train information, uk trains, uk railway, late trains, rail commuter, journey planner, from ~A, to ~A" from-station-names to-station-names) :name "keywords")
	  (:link :type "text/css" :rel "stylesheet" :href *sitecss*))
	 (:body :class "main" :style "text-align: left; overflow: hidden;"
		(:div :id "container"
		      (:h1 (:a :href "/" (fmt "~A" "Telltrains - beta")))
		      ;(top-of-results-page)
		      (:div :class "info"
			    (:h2 (fmt "Trains from ~A to ~A"
				      from-station-names
				      to-station-names))
			    (:div :id "return" :style "right: 0px; position: absolute; margin: -20px 20px 5pt;"
				  (tradedoubler-deep-link 
				   (cl-ppcre:regex-replace-all " - any station" (string-downcase from) "") 
				   (cl-ppcre:regex-replace-all " - any station" (string-downcase to) "") da1 mo1 ho1 mi1)
				  (fmt "~A" ".")
				  (:a :href "/"	:title "Home" :class "journeylinks" (fmt "~A" "Home"))
				  (fmt "~A" ".")
				  (:a :href (timeline-url "" "" to from leaving-date t) 
				      :rel "nofollow"
				      :title "Show return trip" :class "journeylinks" (fmt "~A" "Return trip"))
				  (fmt "~A" ".")
				  (:a :href "/addjourney?TB_iframe=true&height=300" :class "thickbox journeylinks" 
				      :rel "nofollow"
				      :title "Add another journey to page" (fmt "Add journey"))))
		      (:div :id "sliders"
			    (:div :id "tracktime"
				  (:div :class "hour" :style "left: 15px;" (fmt "~A" "2"))
				  (:div :class "hour" :style "left: 45px;" (fmt "~A" "4"))
				  (:div :class "hour" :style "left: 75px;" (fmt "~A" "6"))
				  (:div :class "hour" :style "left: 105px;" (fmt "~A" "8"))
				  (:div :class "hour" :style "left: 135px;" (fmt "~A" "10"))
				  (:div :class "hour" :style "left: 165px;" (fmt "~A" "12"))
				  (:div :class "hour" :style "left: 195px;" (fmt "~A" "2"))
				  (:div :class "hour" :style "left: 225px;" (fmt "~A" "4"))
				  (:div :class "hour" :style "left: 255px;" (fmt "~A" "6"))
				  (:div :class "hour" :style "left: 285px;" (fmt "~A" "8"))
				  (:div :class "hour" :style "left: 315px;" (fmt "~A" "10"))
				  (:div :class "hour" :style "left: 345px;" (fmt "~A" "12"))
				  (:div :id "handleleaving" :class "sliderhandle"))
			    (:div :id "nextprev"
				  (links-and-date leaving-date from to)
				  (:div :id "loadgif"))
			        (:a
				 :style "position: absolute; left: 650px; top: -10px;"
				 :rel "nofollow"
				 :href (tradedoubler-url 
					(cl-ppcre:regex-replace-all " - any station" (string-downcase from) "") 
					(cl-ppcre:regex-replace-all " - any station" (string-downcase to) "") 
					da1 mo1 ho1 mi1)
				 :title "Check Availability"
				 :target "_blank"
				 :onclick (format 
					   nil "javascript:urchinTracker(\"deeptradedouble\~A\~A\");" 
					   (cl-ppcre:regex-replace-all " - any station" (string-downcase from) "") 
					   (cl-ppcre:regex-replace-all " - any station" (string-downcase to) ""))
				 (:img :src "/images/check_availability.gif"))))
		      (:div :id "outerDiv" 
			  (timebar (- ho 2) (+ ho 36) 180)
			  (:div :id "innerDiv"
				(:div :id "timecontainer"
				      (:div :id "timeline" 
					    :style (format nil "left: ~Apx; height: ~Apx;" 
							   (- 180 (* 60 (- ho 3))) height)
					    (:div :id "trains"
						  (train-journeys-with-changes display-trains now leaving-date 
									       :google-p google-p))))
				(:div :id "now"
				      :style (format nil "left: 0px; width: ~Apx; height: ~Apx;" 
						     (- (timedifference-nd (current-short-time now) 
									   (if (= ho 0)
									       "23:00"
									       (format nil "~2,'0d:00" (1- ho))))
							(floor (/ (- now (now)) 60)))
						     (* 1.1 height)))
				(lines (- ho 1) (+ ho 36) 180 height))) 
		      (footer)
		      ;(urchin)
		      (statscounter)
		      (:script :type "text/javascript" :src *sitejs*)
		      (:script :type "text/javascript" 
			       :src "http://yui.yahooapis.com/2.2.2/build/yahoo-dom-event/yahoo-dom-event.js")
		      (:script :type "text/javascript" 
			       :src "http://yui.yahooapis.com/2.2.2/build/dragdrop/dragdrop-min.js")
		      (:script :type "text/javascript" 
			       :src "http://yui.yahooapis.com/2.2.2/build/slider/slider-min.js")
		      ;(:script :src "/javascripts/astrack.js" :type "text/javascript")
		      (:script :type "text/javascript"
				(fmt "// <![CDATA[
                                   var journeys = ~A;
                                   var numberOfJourneys = ~A;
                                   var sliderValues = [~A];
                                   var numRoutes = ~A;
                                   var xFactor = ~A;
                                   var originalleft = ~A;
                                   var stations = '~A';
                                   var pagedate = '~A';
                                   var fb = false;
                                   var first = ~A;
                                   $(document).ready(function() {
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

                                              slider = YAHOO.widget.Slider.getHorizSlider('tracktime', 'handleleaving',0,360);
                                              slider.subscribe('change', scrollJourneys);//slideStart, slideEnd
                                              slider.setValue(first, true, true);

                                              outerDivTop = stripPx($('#outerDiv').css('top'));
                                              $('#innerDiv').scroll(scrollTrains);
	
                                              $('div.red').each(function(){
                                                        	  $(this).click(function(){
                                                             			$('#a' + this.id + 'stops').click();});});

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
				 (first-start-time display-trains (now))
				 )))))))))

(defun start-time-hrmins (it)
  (short-time->mins (journey-start (cadr (car it)))))

(defun first-start-time (trains now)
  (awhen (first (filterTrains-by-time trains now))
    (parse-integer (format nil "~A" (float (/ (* 360 (start-time-hrmins it)) 1440))) :junk-allowed t)))
  
