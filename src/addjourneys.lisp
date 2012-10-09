(in-package :com.sondesh.database)

(defun addjourneys ()
  (with-html 
    (:html
     (:head
      (:title "Telltrains: train times timetable for train journeys")
      (:link :rel "icon" :href "/images/transparent.ico")
      (:meta :name "y_key" :content "0a53d0a3da763141")
      (:meta :name "verify-v1" :content "HLKhUt6V7k4rQlFX+C8v9z/bL62M/ccksoBiWLVLuBE=")
      (:meta :name "robots" :content "index, follow")
      (:meta :name "description" :content "New graphical journey planner for Britain National Rail network. train times; commuter friendly; commuter social network; train running information.")
      (:meta :content "national rail, british rail, trains, railway, uk rail travel, train times, train timetables, train information, uk trains, uk railway, late trains, rail commuter, journey planner, social network" :name "keywords")
      (:link :type "text/css" :rel "stylesheet" :href *sitecss*))
     (:body :class "main" :style "text-align: left; overflow: hidden; background-color: #FFFFFF;"
	    (:div :id "container" :style "width: 600px;"
		  (:form :method :get 
			 :class "telltrains"
			 (:div :class "info"
			       (:h2 (fmt "~A" "Compare multiple routes...")))
			 (:div :id "journey"
			       (:ul :class "fromto"
				    (:li :class "formlist"
					 (:span :class "formlist"
						(:input :type :text
							:name "from" :id "fromstation" :class "field text"
							:autocomplete "off" :size "22" :maxlength "50")
						(:hidden :id "fromcrs" :name "fromcrscode")
						(:label :for "from" (fmt "~A" "From (city or station)")))
					 (:span :class "formlist"
						(:input :type :text
							:name "to" :id "tostation" :class "field text"
							:autocomplete "off" :size "22" :maxlength "50")
						(:hidden :id "tocrs" :name "tocrscode")
						(:label :for "to" (fmt "~A" "To (city or station)")))
					 (:div :class "auto_complete"
					       :id "fromstation_values"
					       :style "display: none; z-index: 10;")
					 (:input :id "search" :class "button" :type "submit" :value "Add journey"))))))
	    (urchin)
	    (:script :type "text/javascript" :src *sitejs*)
	    (:script :type "text/javascript"
		     (fmt "~A" "// <![CDATA[
                                   var journeys = [];
                                   var numberOfJourneys = 0;
                                   var sliderValues = [0];
                                   var numRoutes = 0;
                                   var xFactor = 1;
                                   var originalleft = 0;
                                   var stations = '';
                                   var pagedate = '07-11-2007';
                                   var fb = false;
                                   $(document).ready(function() {
                                      $('#container').center('horizontal');
                                      $('#container').css('left', 50);

                                      $('#fromstation').autocomplete('suggest-stations', { delay:10, minChars:1, onItemSelect:selectFrom, onFindValue:selectFrom, matchSubset:null, cacheData:false, autoFill:false});
                                      $('#tostation').autocomplete('suggest-stations', { delay:10, minChars:1, onItemSelect:selectTo, onFindValue:selectTo, matchSubset:null, cacheData:false, autoFill:false});
                                      $('#search').click(function (){ 
                                                        parent.addjourney($('#fromstation').get()[0].value,$('#tostation').get()[0].value); 
                                                        parent.tb_remove(); 
                                                        return false;
                                                       });
                                    });
                                 // ]]>"))
	    ))))

(defun trainjourneys ()
  (let* ((params (break-as-parameters (request-uri)))
	 (from (first params))
	 (to (second params))
	 (leaving (third params))
	 (real-from (find-station-by-exact-name from))
	 (real-to (find-station-by-exact-name to)))
    (multiple-value-bind (now leaving-date) 
	(timeline-date leaving)
      (aif (and (not (equal real-from real-to))
		(filtertrains-by-time (filter-spurious-journey (find-trains real-from real-to now)) now))
	   (multiple-value-bind (se mi ho da mo ye dd dst1 tz1) (decode-universal-time now 0)
	     (declare (ignore se ye dd dst1 tz1 mi da mo) (fixnum ho))
	     (let ((trains (mapcar #'first it))
		   (display-trains it))
	       (with-html
		 (:div :id "journeys-json-div" :style "display:none;" 
		       (:span :id "journeys-json" 
			      (fmt "~A" (journeys->json trains ho leaving-date t))))
		 (links-and-date-ajax leaving-date t)
		 (train-journeys-with-changes display-trains (now) leaving-date :new-id-p t))))))))

(defun links-and-date-ajax (leaving-date new-id-p)
  (with-html-output (*standard-output*)
    (:div :id (if new-id-p (format nil "date~A" leaving-date) "date")
	  (fmt "~A" "Departing...  ")
	  (when (> (days-from-today-date leaving-date) 0)
	    (with-html-output (*standard-output*)
	      (:a :id (if new-id-p (format nil "prevday~A" leaving-date) "prevday")
		  :rel "history"
		  :href (format nil "/~A" (dttm->string (- (string->dttm leaving-date) (* 24 3600)) 
							:format :caldate))
		  :title "previous day"
		  (fmt "~A" "<<"))
	      (fmt "~A" " ")))
	  (:span :id (if new-id-p (format nil "datestring~A" leaving-date) "datestring")
		 (fmt "~A" (if (string/= "" leaving-date)
			       (concatenate 'string (stylized->day (string->dttm leaving-date)) " (" leaving-date ")")
			       leaving-date)))
	  (fmt "~A" " ")
	  (:a :id (if new-id-p (format nil "nextday~A" leaving-date) "nextday")
	      :rel "history" 
	      :title "next day"
	      :href (format nil "/~A" (dttm->string (+ (string->dttm leaving-date) (* 24 3600)) :format :caldate))
	      (fmt "~A" ">>")))))
