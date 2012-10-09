(in-package :com.sondesh.database)

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

(defun escape-url (url)
  (cl-ppcre:regex-replace-all " " url "%20"))

(defun timeline-url (fromcrs tocrs from to leaving &optional (exact nil))
  (let ((fromcrs (or fromcrs ""))
	(tocrs (or tocrs "")))
    (escape-url
     (format nil
	    (if (or exact
		    (and (string/= tocrs "") (string/= fromcrs "")))
		"/traintimes~A" 
		"/timetable~A")
	    (url-params fromcrs tocrs from to leaving)))))

(defun home ()
  (if (and (or (get-parameter "fromstation") (get-parameter "fromcrs") (get-parameter "from"))
	   (or (get-parameter "tostation") (get-parameter "tocrs") (get-parameter "to")))
      (redirect (timeline-url (get-parameter "fromcrs") (get-parameter "tocrs") 
			      (get-parameter "from") (get-parameter "to") (get-parameter "leaving") t))
      (aif (all-url-parts (request-uri))
	   (let* ((from (first it))
		  (to (second it))
		  (leaving (third it))
		  (real-from (find-station-by-exact-name from))
		  (real-to (find-station-by-exact-name to)))
	     (multiple-value-bind (now leaving-date) 
		 (timeline-date leaving)
	       (aif (and (not (equal real-from real-to))
			 (filtertrains-by-time (filter-spurious-journey (find-trains real-from real-to now)) now))
		    (journey-page it real-from real-to leaving-date now)
		    (homepage))))
	   (homepage))))

(defun homepage ()
      (with-html 
	(:html
	 (:head
	  (:title "Telltrains: train times timetable for train journeys")
	  (:link :rel "icon" :href "/images/transparent.ico")
	  (:meta :name "y_key" :content "0a53d0a3da763141")
	  (:meta :name "verify-v1" :content "HLKhUt6V7k4rQlFX+C8v9z/bL62M/ccksoBiWLVLuBE=")
	  (:meta :name "robots" :content "index, follow")
	  (:meta :name "description" :content "New graphical journey planner for Britain National Rail network. train times; commuter friendly; commuter social network; train running information.")
	  (:meta :content "national rail, british rail, trains, railway, uk rail travel, train times, train timetables, train information, uk trains, uk railway, late trains, rail commuter, journey planner" :name "keywords")
	  (:link :type "text/css" :rel "stylesheet" :href *sitecss*))
	 (:body :class "main" :style "text-align: left;"
	    (:div :id "container"
		  (:h1 (:a :href "/" (fmt "~A" "Telltrains - beta")))
		  (:form :method :get
			 :class "telltrains"
			 (:div :class "info"
			       (:h2 (fmt "~A" "Search for journeys")))
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
					 (:span :class "formlist"
						(:input :type :text
							:id "leavingdate" :name "leaving" :class "field text"
							:size "10" :maxlength "50"
							:value (dttm->string (now) :format :caldate))
						(:label :for "leaving" (fmt "~A" "DD-MM-YYYY")))
					 (:a :id "pickdate" 
					     :class "button"
					     (:img :class "icon" 
						   :src "/images/calendar.gif"
						   :alt "Pick date."))
					 (:div :class "auto_complete"
					       :id "fromstation_values"
					       :style "display: none; z-index: 10;")
					 (:input :id "search" :class "button" :type "submit" :value "Search"))))))
	    (:div :id "outerDiv"
		  (:div :id "innerDiv"))
	    (footer)
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
                                      adjust();
                                      $('#fromstation').autocomplete('suggest-stations', { delay:10, minChars:1, onItemSelect:selectFrom, onFindValue:selectFrom, matchSubset:null, cacheData:false, autoFill:false});
                                      $('#tostation').autocomplete('suggest-stations', { delay:10, minChars:1, onItemSelect:selectTo, onFindValue:selectTo, matchSubset:null, cacheData:false, autoFill:false});
                                    });
                                 // ]]>"))
	    ))))

                  
