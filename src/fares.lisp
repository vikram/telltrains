(in-package :com.sondesh.database)

(field-setup (*nfm64-reader* "data/nfm64")
	     ("^[0-9]" nfm64-fare ((from-nlc-code 4)
				   (to-nlc-code 4)
				   (route-code 5)
				   (ticket-code 3)
				   (fare-price 6))))

(defun upload-nfm64 ()
  (let ((tracing-num 0))
    (with-train-database
      (with-open-file (in (sondesh-file-path "data/nfm64"))
	(do ((line (read-line in nil)
		   (read-line in nil)))
	    ((null line))
	  (when line
	    (print (incf tracing-num) t)
	    (clsql:update-records-from-instance
	     (make-instance 'nfm64
			    :id (next-id)
			    :from-nlc-code (subseq line 0 4)
			    :to-nlc-code (subseq line 4 8)
			    :route-code (subseq line 8 13)
			    :ticket-code (subseq line 13 16)
			    :fare-price (parse-integer (subseq line 16 22)))
	     )))))))

(defun upload-flow-details ()
  (let ((tracing-num 0))
    (with-train-database
      (with-open-file (in (sondesh-file-path "data/RJFAF785.FFL"))
	(do ((line (read-line in nil)
		   (read-line in nil)))
	    ((null line))
	  (print (incf tracing-num) t)
	  (when line
	    (if (cl-ppcre:all-matches "^[IADR]F" line)
		(clsql:update-records-from-instance
		 (make-instance 'ffl-details
				:id (next-id)
				:update-marker-record-type (subseq line 0 2)
				:origin (subseq line 2 6)
				:destination (subseq line 6 10)
				:route-code (subseq line 10 15)
				:status-code (subseq line 15 18)
				:usage-code (subseq line 18 19)
				:direction (subseq line 19 20)
				:end-date (subseq line 20 28)
				:start-date (subseq line 28 36)
				:toc (subseq line 36 39)
				:cross-london (subseq line 39 40)
				:ns-disc (subseq line 40 41)
				:publication (subseq line 41 42)
				:flow-id (subseq line 42 49)))
		(when (cl-ppcre:all-matches "^[IADR]T" line)
		  (clsql:update-records-from-instance
		   (make-instance 'ffl-fare
				  :id (next-id)
				  :update-marker-record-type (subseq line 0 2)
				  :flow-id (subseq line 2 9)
				  :ticket-code (subseq line 9 12)
				  :fare (subseq line 12 20)
				  :restriction-code (subseq line 20 22)))))))))))

(field-setup (*ticket-types-reader* "data/RJFAF785.TTY")
	     ("^[IADR]" ticket-type ((update-marker 1)
				    (ticket-code 3)
				    (end-date 8)
				    (start-date 8)
				    (quote-date 8)
				    (description 15)
				    (tkt-class 1)
				    (tkt-type 1)
				    (tkt-group 1)
				    (last-valid-day 8)
				    (max-passengers 3)
				    (min-passengers 3)
				    (max-adults 3)
				    (min-adults 3)
				    (max-children 3)
				    (min-children 3)
				    (restricted-by-date 1)
				    (restricted-by-train 1)
				    (restricted-by-area 1)
				    (validity-code 2)
				    (atb-description 20)
				    (lul-xlondon-issue 1)
				    (reservation-required 1)
				    (capri-code 3)
				    (lul-93 1)
				    (uts-code 2)
				    (time-restriction 1)
				    (free-pass-lul 1)
				    (package-mkr 1)
				    (fare-multiplier 3)
				    (discount-category 2))))

			      
(defparameter *ticket-types*
  (funcall *ticket-types-reader*))

(defun ffl-fare (from-nlc-code to-nlc-code)
  (with-train-database
    (clsql:query (format nil "select ffl_fare.flow_id,ffl_fare.ticket_code,ffl_fare.fare,ffl_fare.restriction_code 
                                     from ffl_details, ffl_fare where ffl_fare.flow_id=ffl_details.flow_id
                                     and ffl_details.origin='~A' and ffl_details.destination='~A'"
			 from-nlc-code to-nlc-code))))

(defun journey-fares (from to)
  (let ((fare (append (ffl-fare (station-nlc-group-code from)
			    (station-nlc-group-code to))
		      (ffl-fare (station-nlc-group-code from)
				(station-nlc-code to))
		      (ffl-fare (station-nlc-code from)
				(station-nlc-group-code to))
		      (ffl-fare (station-nlc-code from)
				(station-nlc-code to)))))
    (mapcar #'(lambda (fare-details)
		(cons fare-details (ticket-type-details (second fare-details))))
	    fare)))

(defun ticket-type-details (ticket-code)
  (find ticket-code *ticket-types* :key #'second :test #'string=))

;;
