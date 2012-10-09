(in-package :com.sondesh.database)

;;;; detailed station information. One time extraction

(defun get-station (station-code)
  (with-open-file (out (format nil "/home/vb/sondesh/src/database/~A.html" station-code) 
		       :direction :output :if-exists :supersede)
    (print (net.html.parser:parse-html (drakma:http-request (format nil "http://www.nationalrail.co.uk/stations/~A.html" station-code)
							    :method :get)) out))
  station-code)

(defun get-details-for-all-stations ()
  (dolist (station *stations*)
    (get-station (symbol-name (first station)))))

(defparameter *postcode-scanner*
  (cl-ppcre:create-scanner "\\b[A-PR-UWYZ][A-HK-Y0-9][A-HJKSTUW0-9]?[ABEHMNPRVWXY0-9]? {1,2}[0-9][ABD-HJLN-UW-Z]{2}\\b|GIR 0AA" :case-insensitive-mode t))

(defun postcode (address)
  (let ((pos (cl-ppcre:all-matches *POSTCODE-SCANNER* address)))
    (if pos
	(subseq address (first pos) (second pos))
	"")))

(defun station-details (station-code)
  (with-open-file (in (format nil "/home/vb/sondesh/src/database/~A.html" station-code))
    (aif (find-html-node (read in) :div :class "floatleftlink")
	 (let ((address (trim-funny (reduce #'(lambda (a b) 
					  (if (string/= "," (subseq a (1- (length a))))
					      (concatenate 'string a ", " b)
					      (concatenate 'string a b)))
				      (remove-if #'(lambda (x) (eq x :br))
						 (nthcdr 4 it))))))
	   (list (trim-funny (car (nthcdr 4 it)))
		 address
		 (postcode address)
		 (trim (second (third it))))))))

(defun write-all-station-details ()
  (with-open-file (out "/home/vb/sondesh/src/database/station.details"
		       :direction :output :if-exists :supersede)
    (print (sort
	    (mapcar #'(lambda (station) 
			(append station
				(station-details (first station))))
		    *stations*)
	    #'string< :key #'third)
	   out)))

(defparameter *space-scanner* (cl-ppcre:create-scanner " "))

(defun get-lat-long-for-postcode (postcode)
  (unless (string= "" postcode)
    (aif (drakma:http-request (format nil 
				      "http://streetmap.co.uk/streetmap.dll?MfcISAPICommand=GridConvert&name=~A&type=PostCode" 
				      (cl-ppcre:regex-replace *space-scanner* postcode "+"))
			      :method :get)
	 (unless (string= "" it)
	   (let ((latlong (cl-ppcre:all-matches-as-strings "\\([0-9 .-]+\\)" it)))
	     (when latlong
	       (list (subseq (first latlong) 2 (- (length (first latlong)) 2))
		     (subseq (second latlong) 2 (- (length (second latlong)) 2)))))))))


(defparameter *stations1* (with-open-file (in (sondesh-file-path "st.pc")) (read in)))

(mapcar #'(lambda (stn) (if (or (not (station-longitude stn))
				(not (station-latitude stn))
				(string= "" (station-longitude stn))
				(string= "" (station-latitude stn)))
			    (aif (assoc (station-crs-code stn) *stations1* :test #'string=)
				 (progn
				   (setf (station-latitude stn) (station-latitude it))
				   (setf (station-longitude stn) (station-latitude it))
				   stn)
				 stn)
			    stn))
	       *stations*)


(defun cleaned-up (name)
  (let ((funny (cl-ppcre:create-scanner '(:char-class #\Newline #\linefeed #\return #\> #\< #\, #\. #\) #\( #\; #\: #\? #\! #\@))))
    (aif (cl-ppcre:all-matches funny name)
	 (trim (subseq name 0 (first it)))
	 name)))

#|

(defun get-lat-long-for-place (place)
  (unless (string= "" place)
    (aif (drakma:http-request 
	  (format 
	   nil 
	   "http://mmw.multimap.com/API/geocode/1.2/public_api?output=json&callback=MMGeocoder.prototype._GeneralJSONCallback&qs=~A&countryCode=GB"
	   (cl-ppcre:regex-replace-all *space-scanner* place "+"))
	  :method :get)
	 (with-input-from-string (stream (subseq it 42 (1- (length it))))
	   (json:decode-json stream)))))

(mapcar #'(lambda (stn) (if (or (not (station-longitude stn))
				(not (station-latitude stn))
				(string= "" (station-longitude stn))
				(string= "" (station-latitude stn)))
			    (let ((results (get-lat-long-for-place (cleaned-up (station-name stn)))))
			      (if (find '(locationcount . 1) (car results) :test #'equal)
				  (awhen (cdr (find 'point 
						    (car (cdr
							  (find 'location (car results)
								:key #'(lambda (x) (if (listp x) (car x) nil)))))
						    :key #'car))
				    (setf (station-latitude stn) (cdr (assoc 'lat it)))
				    (setf (station-longitude stn) (cdr (assoc 'lon it)))
				    stn)
	                          stn))
			    stn))
	       *stations*)

(defparameter *stn* nil)

(let ((i 0))
  (dolist (stn *stations*)
    (if (or (not (station-longitude stn))
	    (not (station-latitude stn))
	    (string= "" (station-longitude stn))
	    (string= "" (station-latitude stn)))
	(let ((result (try-this-name (concatenate 'string (cleaned-up (station-name stn)) " railway station") stn)))
	  (if (or (not (station-latitude result))
		  (string= "" (station-latitude result)))
	      (push (try-this-name 
		     (cleaned-up (station-name stn)) stn) *stn*)
	      (push result *stn*)))
	(push stn *stn*))
    (print (incf i) t)))

(nreverse *stn*)

(remove-if-not #'(lambda (stn) (or (not (station-longitude stn))
				(not (station-latitude stn))
				(string= "" (station-longitude stn))
				(string= "" (station-latitude stn))))
	       *stations*))

|#

(defun try-this-name (name stn)
  (let ((results (get-lat-long-for-place name)))
    (cond ((find '(locationcount . 1) (car results) :test #'equal)
	   (awhen (cdr (find 'point 
			     (car (cdr
				   (find 'location (car results)
					 :key #'(lambda (x) (if (listp x) (car x) nil)))))
			     :key #'car))
	     (setf (station-latitude stn) (cdr (assoc 'lat it)))
	     (setf (station-longitude stn) (cdr (assoc 'lon it)))
	     stn))
	  ((find '(locationcount . 2) (car results) :test #'equal)
	   (progn
	     (dolist (var (cdr (nth 3 (car results))))
	       (print name t)
	       (print stn t)
	       (print (cadr var) t)
	       (when (eql 'y (read))
		 (let ((point (cdr (nth 3 (cadr results)))))
		   (setf (station-latitude stn) (cdr (assoc 'lat point)))
		   (setf (station-longitude stn) (cdr (assoc 'lon point)))
		   (return-from try-this-name stn))))
	     stn))
	  (t stn))))

(field-setup (*station-names-reader* "data/RJTTF851.MSN")
	     ("^A    [A-Z]+" physical-station ((record-type 1 :ignore)
					      (reserved 4 :ignore)
					      (station-name 26 :trim)
					      (reserved 4 :ignore)
					      (cate-interchange-status 1 (blank 0 1 2 3 9))
					      (tiploc-code 7)
					      (crs-reference-code 3)
					      (reserved 3 :ignore)
					      (crs-code 3)
					      (ordnance-surver-grid-ref-east 5)
					      (is-estimate 1 (blank E))
					      (ordnance-surver-grid-ref-north 5)
					      (minimum-change-time 2)
					      (reserved 1 :ignore)
					      (footnote-code 1)
					      (reserved 11 :ignore)
					      (sub-sector-code 3))))

(field-setup (*alias-reader* "data/RJTTF851.MSN")
	     ("^L    [A-Z]+" alias ((record-type 1 :ignore)
				    (reserved 4 :ignore)
				    (name 26 :trim)
				    (reserved 5 :ignore)
				    (alias 26 :trim)
				    (reserved 20 :ignore))))

(field-setup (*group-reader* "data/RJTTF851.MSN")
	     ("^G    [A-Z]+" station-group ((record-type 1 :ignore)
					   (reserved 4 :ignore)
					   (group-name 26 :trim)
					   (reserved 5 :ignore)
					   (members-crs-codes 39 (list 3 1))
					   (reserved 6 :ignore))))

(field-setup (*routeing-location-reader* "data/RJTTF851.MSN")
	     ("^V    [A-Z]+" routeing ((record-type 1 :ignore)
				      (reserved 4 :ignore)
				      (location-name 26 :trim)
				      (reserved 5 :ignore)
				      (location-crs-codes 16 (list 3 1))
				      (reserved 29 :ignore))))

(defun verify-station-details ()
  (with-open-file (in "/home/vb/sondesh/src/database/station.locations")
    (remove-if #'(lambda (x) (= (length x) 19)) (read in))))

(defun read-station-details ()
  (let ((stns (funcall *station-names-reader*))
	(grp (funcall *group-reader*))
	(rt (funcall *routeing-location-reader*))
	(al (funcall *alias-reader*)))
    (mapcar #'(lambda (stn) 
		(let ((old-stn (find (physical-station-crs-code stn) *stations* :key #'first :test #'string=))
		      (group (find (physical-station-crs-code stn) grp
				   :key #'station-group-members-crs-codes
				   :test #'(lambda (crscode codes) (member crscode codes :test #'string=))))
		      (route-location (find (physical-station-crs-code stn) rt 
					    :key #'routeing-location-crs-codes 
					    :test #'(lambda (crscode codes) (member crscode codes :test #'string=))))
		      (alias (find (physical-station-station-name stn) al)))
		  (make-station :crs-code (physical-station-crs-code stn)
				:name (physical-station-station-name stn)
				:display-name (physical-station-station-name stn)
				:rank 0
				:nicknames (remove-if #'not (append (list (physical-station-station-name stn) (station-name old-stn) (if alias (alias-alias alias) nil))
								    (station-nicknames old-stn)))
				:postcode (station-postcode old-stn)
				:location  (station-location old-stn)
				:toc  (station-toc old-stn)
				:fulladdress  (station-fulladdress old-stn)
				:latitude  (station-latitude old-stn)
				:longitude  (station-longitude old-stn)
				:crs-reference (physical-station-crs-reference-code stn)
				:tiploc (physical-station-tiploc-code stn)
				:minimum-change-time (physical-station-minimum-change-time stn)
				:footnote-code (physical-station-footnote-code stn)
				:sub-sector-code (physical-station-sub-sector-code stn)
				:in-group-p (if group t nil)
				:group-name (station-group-name group)
				:routeing-location-p (if route-location t nil))))		      
	    stns)))

(field-setup (*location-reader* "data/RJRG0121.RGY" :comma-separated-p t)
	     ("^[0-9]" locations ((uic-code 3)
				  (comma 1 :ignore)
				  (nlc-code 4)
				  (comma 1 :ignore)
				  (group-code 4)
				  (comma 1 :ignore)
				  (crs-code 3)
				  (comma 1 :ignore)
				  (county-code 2)
				  (comma 1 :ignore)
				  (zone-code 4)
				  (comma 1 :ignore)
				  (start-date 8)
				  (comma 1 :ignore)
				  (end-date 8))))

(field-setup (*station-file-reader* "data/RJRG0121.RGS" :comma-separated-p t)
	     ("^[A-Z]" station-station ((station-id 3)
					(comma 1 :ignore)
					(rp1-id 3)
					(comma 1 :ignore)
					(rp2-id 3)
					(comma 1 :ignore)
					(rp3-id 3)
					(comma 1 :ignore)
					(rp4-id 3)
					(comma 1 :ignore)
					(stn-group-id 3))))

(field-setup (*routeing-points-reader* "data/RJRG0121.RGP")
	     ("^[A-Z]" routeing ((point 3))))

(defun add-station-routeing-guide-details (stations)
  (let ((grp (funcall *station-group-reader*))
	(stn (funcall *station-file-reader*))
	(rp (funcall *routeing-points-reader*)))
    (mapcar #'(lambda (s) (progn 
			    (when (find (station-crs-code s) rp :key #'first :test #'string=)
			      (setf (station-routeing-location-p s) t))
			    (aif (find (station-crs-code s) stn :key #'first :test #'string=)
				 (if (string/= (sixth it) "")
				     `(,@s ,(sixth it) ,(remove-if #'(lambda (x) (string= "" x)) (butlast (nthcdr 1 it) 1)))
				     `(,@s nil ,(remove-if #'(lambda (x) (string= "" x)) (butlast (nthcdr 1 it) 1))))
				 `(,@s nil nil))))
	    stations)))

(defun score-popularity-of-stn (tiploc)
  (with-train-database
    (let* ((results (clsql:query (format nil "select doesnt_stop,departs,arrives from stop where tiploc_code='~A'" tiploc)))
	   (stops (length (remove-if-not #'(lambda (x) (string= x "f")) results 
				    :key #'first)))
	   (startend (length (remove-if-not #'(lambda (x) (and (string= (first x) "f")
						       (or (and (string= (second x) "t")
								(string= (third x) "f"))
						       (and (string= (second x) "f")
							    (string= (third x) "t")))))
				    results))))
      (float 
       (+ (/ stops 2728318)
	  (/ startend
	     (+ 281798 281798)))))))


(mapcar #'(lambda (stn) (progn
			  (setf (station-rank stn) (score-popularity-of-stn (station-tiploc stn)))
			  stn))
	*stations*)

(defun find-duplicates (lst &optional (test #'eql) (acc '()))
  (if (not lst)
      (nreverse acc)
      (if (member (first lst) (rest lst) :test test)
	  (find-duplicates (rest lst) test (push (first lst) acc))
	  (find-duplicates (rest lst) test acc))))

(field-setup (*station-group-reader* "data/RJRG0121.RGG" :comma-separated-p t)
	     ("^[A-Z]" station-group-station ((group-id 3)
					      (comma 1 :ignore)
					      (main 3))))

(defun find-groups (lst &optional (acc '()))
  (labels ((make-group (stn rank others)
	     (progn
	       (setf (station-rank stn) rank)
	       (setf (station-display-name stn) (short-group-name (station-display-name stn)))
	       (setf (station-tiploc-codes stn) (delete-duplicates (flatten (append (station-tiploc-codes stn)
										    (mappend #'station-tiploc-codes others)))
								   :test #'string=))
	       (setf (station-crs-code stn) (station-group-crs-code stn))
	       stn)))
    (let ((fr (copy-list (first lst)))
	  (grp (funcall *station-group-reader*)))
      (if (not lst)
	  (nreverse acc)
	  (if (or (not (station-group-crs-code fr)) 
		  (string= (station-group-crs-code fr) "")
		  (find fr acc :test #'(lambda (stn1 stn2) (string= (station-group-crs-code stn1)
								    (station-group-crs-code stn2)))))
	      (find-groups (rest lst) acc)
	      (aif (remove-if-not #'(lambda (stn) (string= (station-group-crs-code fr)
							   (station-group-crs-code stn)))
				  (rest lst))
		   (let ((group (find (station-group-crs-code fr) grp :key #'first :test #'string=))
			 (rank (+ (station-rank fr) (reduce #'+ it :key #'station-rank))))
		     (if (string= (second group) (station-crs-code fr))
			 (find-groups (rest lst) (push (make-group fr rank it) acc))
		       (let ((main (copy-list (find (second group) it :key #'station-crs-code :test #'string=))))
			 (find-groups (rest lst) (push (make-group main rank it) acc)))))
		   (find-groups (rest lst) acc)))))))

(defun short-group-name (name)
  (let ((frags (cl-ppcre:split "[ -]" name))
	(common-names '("london" "junction" "airport" "east" "west" "north" "south" "thames")))
    (if (not (member (string-downcase (first frags)) common-names :test #'string=))
	(concatenate 'string (first frags) " (any station)")
	(concatenate 'string (first frags) " " (second frags) " (any station)"))))


(defun trains->db ()
  (let ((started nil)
	(train nil)
	(train-id 0)
	(i 0)
	(stop-id 0))
    (with-train-database
      (clsql:execute-command "delete from schedules")
      (clsql:execute-command "delete from stop")
      (with-open-file (in (sondesh-file-path "data/RJTTF851.MCA"))
	(do ((line (read-line in nil)
		   (read-line in nil)))
	    ((null line))
	  (when line
	    (if (string= "BS" (subseq line 0 2))
		(progn
		  (setf started t)
		  (setf train (read-formated-line *BASIC-SCHEDULE-RECORD-DEFN* line))
		  (setf train-id (encode-train-id train))
		  (add-train->schedules train train-id)
		  (print (incf i) t))
		(when started
		  (casetest (subseq line 0 2) string=
		    ("LO" (let ((orig (read-formated-line *origin-station-record-defn* line)))
			    (clsql:update-records-from-instance
			     (make-instance 'stop 
					    :id (incf stop-id)
					    :tiploc-code (second orig)
					    :stop-train-id (parse-integer train-id)
					    :departs t
					    :arrives nil
					    :doesnt-stop nil
					    :scheduled-pass nil
					    :arrival-time nil
					    :departure-time (parse-integer (fifth orig))
					    :platform (sixth orig)))))
		    ("LI" (let ((int (read-formated-line *intermediate-station-record-defn* line)))
			    (clsql:update-records-from-instance
			     (make-instance 'stop 
					    :id (incf stop-id)
					    :tiploc-code (second int)
					    :stop-train-id (parse-integer train-id)
					    :departs t
					    :arrives t
					    :doesnt-stop (not (string= "     " (sixth int)))
					    :scheduled-pass (parse-integer (aif (string= "" (trim (sixth int)))
										"0"
										(subseq (sixth int) 0 4)))
					    :arrival-time (parse-integer (seventh int))
					    :departure-time (parse-integer (eighth int))
					    :platform (ninth int)))))
		    ("LT" (let ((dest (read-formated-line *terminating-station-record-defn* line)))
			    (setf started nil)
			    (clsql:update-records-from-instance
			     (make-instance 'stop 
					    :id (incf stop-id)
					    :tiploc-code (second dest)
					    :stop-train-id (parse-integer train-id)
					    :departs nil
					    :arrives t
					    :doesnt-stop nil
					    :scheduled-pass nil
					    :arrival-time (parse-integer (fifth dest))
					    :departure-time nil
					    :platform (sixth dest))))))))))))))
(field-setup (*routeing-point-reader* "data/RJRG0121.RGP")
	     ("^[A-Z]" routeing-poing ((crs-grp-code 3))))

(field-setup (*routeing-point-reader* "data/RJRG0121.RGN")
	     ("^[A-Z]" routeing-poing ((crs-grp-code 3))))

(funcall *ROUTEING-POINT-READER*)


 
(mapcar #'(lambda (stn) (progn
			  (when (member (station-crs-code stn) '("ABD" "ACT" "AFK" "AHV" "BBN" "BDM" "BFD" "BGN" "BKG" "BKJ" "BNY" "BON" "BSK"
 "BTB" "BTG" "BTH" "CAR" "CBG" "CDF" "CHD" "CLJ" "CLY" "COL" "COV" "CPM" "CRE"
 "CRS" "CRV" "CTR" "CYP" "DAR" "DEE" "DFD" "DID" "DON" "DVP" "ELY" "EPS" "ERL"
 "ESL" "FAV" "FPK" "G01" "G02" "G03" "G04" "G05" "G06" "G07" "G08" "G09" "G10"
 "G11" "G12" "G13" "G14" "G15" "G16" "G17" "G18" "G19" "G20" "G21" "G22" "G23"
 "G24" "G25" "G26" "G27" "G28" "G29" "G30" "G31" "G32" "G33" "G34" "G35" "G36"
 "G37" "G38" "G39" "G40" "G41" "G42" "G43" "G44" "G45" "G46" "G47" "G48" "G49"
 "G50" "G51" "G52" "G53" "G54" "G55" "G56" "G57" "G58" "G59" "G60" "G61" "G62"
 "G63" "G64" "G65" "G66" "G67" "G68" "G69" "G70" "G71" "GFD" "GLD" "GOO" "GRA"
 "GRP" "GTW" "GUI" "HAV" "HAY" "HAZ" "HFD" "HHY" "HNH" "HOO" "HRH" "HUL" "HUY"
 "INV" "IPS" "KET" "KMK" "KNG" "KPA" "LCN" "LDY" "LEI" "LTV" "LWS" "MIJ" "MMO"
 "NCL" "NMP" "NRW" "NTN" "NUN" "NWP" "OKM" "OXF" "PBO" "PMR" "PRE" "PTH" "PYG"
 "RET" "RGL" "RMD" "RUG" "SAL" "SAY" "SBY" "SEV" "SHR" "SHT" "SLO" "SLR" "SMK"
 "SNS" "SOT" "SPT" "SRA" "STG" "SUO" "SUR" "SWI" "SYB" "TAM" "TAU" "TBD" "TON"
 "TUH" "WFJ" "WIJ" "WIM" "WKM" "WOK" "WRU" "WVH" "YRK")
				    :test #'string=)
			    (setf (station-routeing-location-p stn) t))
			  stn))
	*station-groups*)


(let ((results (make-hash-table :test 'equal)))
  (mapcar #'(lambda (station)
	      (aif (gethash (physical-station-crs-code station) results)
		   (setf (gethash (physical-station-crs-code station) results) 
			 (cons (physical-station-tiploc-code station) it))
		   (setf (gethash (physical-station-crs-code station) results) 
			 (cons (physical-station-tiploc-code station) nil))))
	   (funcall *station-names-reader*))
  (mapcar #'(lambda (stn)
	      (setf (station-tiploc-codes stn)
		    (delete-duplicates (append (station-tiploc-codes stn) (gethash (station-crs-code stn) results))
				      :test #'string=))
	      stn)
	  *stations*))

(let ((left-scanner (cl-ppcre:create-scanner "\\("))
      (right-scanner (cl-ppcre:create-scanner "\\)")))
  (defun replace-parens (string)
    (cl-ppcre:regex-replace right-scanner (cl-ppcre:regex-replace left-scanner string "- ") "")))

(mapcar #'(lambda (stn)
	    (progn
	      (setf (station-name stn) (replace-parens (station-name stn)))
	      (setf (station-display-name stn) (replace-parens (station-display-name stn)))
	      (setf (station-nicknames stn) (delete-duplicates (mapcar #'replace-parens (station-nicknames stn))
							       :test #'(lambda (s1 s2) (string= (string-downcase s1) (string-downcase s2)))))
	      stn))
	*stations*)

(mapcar #'(lambda (stn-grp)
	    (progn
	      (setf (station-nicknames stn-grp) (list (station-display-name stn-grp) 
						      (trim-funny (first (split-sequence:split-sequence #\- (station-display-name stn-grp))))))
	      stn-grp))
	*station-groups*)

;;test
(let ((results '()))
  (dolist (stn *stations*)
    (aif (find-station-by-letters (station-display-name stn))
	 (unless (string= (station-crs-code (car it)) (station-crs-code stn))
	   (push (cons :fucked (list stn (car it))) results))
	 (push (cons :nothing stn) results)))
  results)

