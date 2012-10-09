(in-package :com.sondesh.database)

(defparameter *ranked-crses* 
  (sort (copy-list *all-crses*)
	#'>
	:key (compose #'station-rank #'find-station-by-code)))

(defun direct-trains-count (crs1 crs2)
  (let ((results 0))
    (dolist (tip1 (station-tiploc-codes (find-station-by-code crs1)))
      (dolist (tip2 (station-tiploc-codes (find-station-by-code crs2)))
	(awhen (gethash (concatenate 'string tip1 tip2) *connectivity*)
	  (incf results (connectivity-number it)))))
    results))

(defun station-index ()
  (with-html
    (:html
     (:head
      (:title "Telltrains: most popular british rail station")
      (:link :rel "shortcut icon" :href "/images/transparent.ico" :type "image/x-icon")
      (:meta :name "robots" :content "index, follow")
      (:meta :name "description" :content "Station index for the new graphical journey planner for British National Rail network. train times timetable; commuter friendly; commuter social network; train running information.")
      (:meta :content "national rail, british rail, most popular stations, commonly used stations, hubs, trains, railway, uk rail travel, train times, train timetables, 
                       train information, uk trains, uk railway, late trains, rail commuter, journey planner, social network" :name "keywords")
      (urchin))
     (:body 
      (:h1 (:a :href "/" "Telltrains - beta"))
      (:p "List of the most popular stations")
      (:ol 
       (loop for crs in *ranked-crses*
	    do (htm
		(:li (:a :href (format nil "/journeysfrom/~A" 
				       (station-display-name (find-station-by-code crs))) 
			 (fmt "train times & timetables from ~A" 
			      (station-display-name (find-station-by-code crs))))))))
      (statscounter)))))

(defun station-journeys ()
  (let* ((from (first (break-as-parameters (request-uri))))
         (stn (real-station-by-letters from))
         (my-name (station-display-name stn))
         (crs (station-crs-code stn))
         (rest-of-crses (copy-list
                         (remove-if #'(lambda (x)
                                        (member crs (cons (station-group-crs-code (find-station-by-code x))
                                                          (station-all-crs-codes (find-station-by-code x)))
                                                :test #'string=))
                                    *all-crses*))))
    (with-html
      (:html
       (:head
        (:title (fmt "Telltrains: most popular journeys from ~A" from))
        (:link :rel "icon" :href "/images/transparent.ico")
        (:meta :content "index, follow" :name "robots")
        (:meta :content (format nil "Timetable for ~A for Britain National Rail network. UK and British train times timetable; commuter friendly; commuter social network; train running information." from) :name "description")
        (:meta :content (format nil "national rail, british rail, most popular stations, commonly used stations, hubs, trains, railway, uk rail travel, train times, train timetables, train information, 
                         uk trains, uk railway, late trains, rail commuter, journey planner, social network, ~A" from) :name "keywords"))
       (:body 
        (:h2 (fmt "Train times and timetables from ~A railway station" from))	    
	(:div
               (:ul :style "list-style-type:none; left: -40px; position: relative;"
                    (:li :style "display: inline;" (fmt "Stations in ~A ..." from)
                         (loop for i from 0
                            for name in (mapcar (compose #'station-display-name #'find-station-by-code) 
                                                (sort (copy-list (station-all-crs-codes stn))
                                                      #'> :key (compose #'station-rank #'find-station-by-code)))
                            do (htm
                                (fmt "~A" (if (= i 0) "" ","))
                                (:li :style "padding:4px 5px 2px 9px; display: inline;"
                                     (:a :href (format nil "/journeysfrom/~A" name)
                                         (fmt "~A" name))))))))
        (:div :style "font-size: 12pt;"
	 (left-side-of-timetable-page)
         (:div :style "position: relative; padding-left: 125px;"
               (:div (:a :href "/" (fmt "~A" "Home")))
               (:div :style "padding-left: 15px;" (fmt "~A" "=>") (:a :href "/stations" (fmt "~A" "Train times for most popular stations")))
               (:div :style "padding-left: 30px;" (fmt "~A" "=>") (fmt "~A" from))
               (:ol :style "padding-left: 60px;"
                    (loop for other-name in (mapcar (compose #'station-display-name #'find-station-by-code)
                                                    (sort rest-of-crses 
                                                          #'(lambda (crs1 crs2)
                                                              (let ((dt1 (direct-trains-count (station-crs-code stn) crs1))
                                                                    (dt2 (direct-trains-count (station-crs-code stn) crs2)))
                                                                (if (= dt1 dt2) 
                                                                    (> (+ (station-rank stn) (station-rank (find-station-by-code crs1)))
                                                                       (+ (station-rank stn) (station-rank (find-station-by-code crs2))))
                                                                    (> dt1 dt2))))))
                       do (htm
                           (:li (:a :href (format nil "/timetable/~A/~A" my-name other-name)
                                    (fmt "train times and timetable from ~A to ~A" 
                                         (cl-ppcre:regex-replace-all " - any station" my-name "")
                                         (cl-ppcre:regex-replace-all " - any station" other-name "")))))))))
	(urchin)
	(statscounter)
        (:script :src "/javascripts/astrack.js" :type "text/javascript"))))))

;name of station
;address
;map
;shortened homepage with from already selected
;only links to direct trains maybe common routes and the next 5 trains
;links to national-rail wikipedia

(defun suggest-stations ()
  (with-html-output-to-string (*standard-output* nil :prologue nil)
    (loop for result in (topn (find-station-by-letters (get-parameter "q")) 10)
	 do (htm
	     (fmt "~A|~A~A" (station-display-name result) (station-crs-code result) #\Newline)))))

