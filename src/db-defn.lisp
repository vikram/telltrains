(in-package :com.sondesh.database)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *sqlite* nil)

  (defparameter *db* (if *sqlite*
			 '("/home/vb/sondesh/src/database/trains.db")
			 '("localhost" "trains" "vb" "railroad")))

  (defparameter *process-db* (make-hash-table :test 'equal))

  (defmacro with-train-database (&body body)
    (with-gensyms (db-var connected result)
      `(multiple-value-bind (,db-var ,connected)
	   (get-train-database)
	 (let ((,result nil))
	   (unwind-protect
		(setf ,result (progn ,@body))
	     (unless ,connected (remove-train-database ,db-var)))
	   ,result))))

  (defmacro with-train-db ((db) &body body)
    (with-gensyms (db-var connected result)
      `(multiple-value-bind (,db-var ,connected)
	   (get-train-database)
	 (let ((,result nil))
	   (unwind-protect
		(setf ,db ,db-var)
	     (setf ,result (progn ,@body))
	     (setf ,db nil)
	     (unless ,connected (remove-train-database ,db-var)))
	   ,result))))

  (defmacro with-train-transaction! (&body body)
    `(clsql:with-transaction (:database (get-train-database))
       ,@body))

  (defmacro with-train-database! (&body body)
    (with-gensyms (db-var connected result)
      `(multiple-value-bind (,db-var ,connected)
	   (get-train-database)
	 (let ((,result nil))
	   (unwind-protect
		(clsql:with-transaction (:database ,db-var)
		  (setf ,result (progn ,@body)))
	     (unless ,connected (remove-train-database ,db-var)))
	   ,result))))

  (defun clear-all-connections ()
    (progn
      (setf *process-db* (make-hash-table))
      (clsql:disconnect-pooled)))

  (defun get-repos-for-current-process ()
    (gethash sb-thread:*current-thread* *process-db*))

  (defun get-database-for-current-process ()
    (get-repos-for-current-process))

  (defun setup-database-for-current-process! (it)
    (progn
      (setf (gethash sb-thread:*current-thread* *process-db*) it)
      (get-database-for-current-process)))      

  (defun remove-train-database (db)
    (let ((db (gethash sb-thread:*current-thread* *process-db*)))
      (remhash sb-thread:*current-thread* *process-db*)
      (clsql:disconnect :database db)))

  (defun get-train-database ()
    (aif (get-database-for-current-process)
	 (values it t)
	 (awhen (clsql:connect *db*
			       :if-exists :old
			       :pool t
			       :make-default nil
			       :database-type (if *sqlite* :sqlite3 :mysql))
	   (setup-database-for-current-process! it)
	   (values it nil))))

  (defparameter *table-symbols* (make-hash-table :test 'equal))

  (defun table (name db)
    `(progn
       (clsql:create-view-from-class ',name :database ,db)
       (setf (gethash ,(symbol-name name) *table-symbols*) ',name)))
     
   (clsql:def-view-class nfm64 ()
    ((id :accessor id
	 :type clsql::fixnum
	 :initarg :id
	 :db-constraints (:not-null :primary-key :unique))
     (from-nlc-code :initarg :from-nlc-code :accessor from-nlc-code
		   :type clsql::string)
     (to-nlc-code :initarg :to-nlc-code :accessor to-nlc-code
		   :type clsql::string)
     (route-code :initarg :route-code :accessor route-code
		   :type clsql::string)
     (ticket-code :initarg :ticket-code :accessor ticket-code
		   :type clsql::string)
     (fare-price :initarg :fare-price :accessor fare-price
		       :type clsql::fixnum)))

  (clsql:def-view-class schedules ()
    ((id :accessor id
	 :type clsql::fixnum
	 :initarg :id
	 :db-constraints (:not-null :primary-key :unique))
     (train-uid :initarg :train-uid :accessor train-uid
		   :type clsql::string)
     (date-runs-from :initarg :date-runs-from :accessor date-runs-from
		   :type clsql::date)
     (date-runs-to :initarg :date-runs-to :accessor date-runs-to
		   :type clsql::date)
     (days-run :initarg :days-runs :accessor days-runs
		   :type clsql::string)
     (bank-holiday :initarg :bank-holiday :accessor bank-holiday
		   :type clsql::string)
     (train-status :initarg :train-status :accessor train-status
		   :type clsql::string)
     (train-category :initarg :train-category :accessor train-category
		   :type clsql::string)
     (train-identity :initarg :train-identity :accessor train-identity
		   :type clsql::string)
     (head-code :initarg :head-code :accessor head-code
		   :type clsql::string)
     (course-indicator :initarg :course-indicator :accessor course-indicator
		       :type clsql::string)
     (profit-centre-code :initarg :profit-centre-code :accessor profit-centre-code
		   :type clsql::string)
     (business-sector :initarg :business-sector :accessor business-sector
		   :type clsql::string)
     (power-type :initarg :power-type :accessor power-type
		   :type clsql::string)
     (timing-load :initarg :timing-load :accessor timing-load
		   :type clsql::string)
     (train-speed :initarg :speed :accessor train-speed
		   :type clsql::string)
     (operating-chars :initarg :operating-chars :accessor operating-chars
		   :type clsql::string)
     (train-class :initarg :class :accessor train-class
		   :type clsql::string)
     (sleepers :initarg :sleepers :accessor sleepers
		   :type clsql::string)
     (reservations :initarg :reservations :accessor reservations
		   :type clsql::string)
     (connect-indicator :initarg :connect-indicator :accessor connect-indicator
		   :type clsql::string)
     (catering-code :initarg :catering-code :accessor catering-code
		   :type clsql::string)
     (service-branding :initarg :service-branding :accessor service-branding
		   :type clsql::string)
     (stp-indicator :initarg :stp-indicator :accessor stp-indicator
		   :type clsql::string)
     (stops :initarg :stops :accessor schedule-stops
	    :type clsql::string)
     (usually-late-by :initarg :usually-late-by :accessor usually-late-by
		      :type clsql::fixnum)))    

  (clsql:def-view-class stop ()
    ((id :accessor id
	 :type clsql::fixnum
	 :initarg :id
	 :db-constraints (:not-null :primary-key :unique))
     (train-id :accessor stop-train-id :initarg :stop-train-id
	       :type clsql::fixnum)
     (tiploc-code :initarg :tiploc-code :accessor tiploc-code
		  :type clsql::string)
     (departs :initarg :departs :accessor departs
	      :type clsql::boolean)
     (arrives :initarg :arrives :accessor arrives
	      :type clsql::boolean)
     (doesnt-stop :initarg :doesnt-stop :accessor doesnt-stop
		  :type clsql::boolean)
     (scheduled-pass :initarg :scheduled-pass :accessor :scheduled-pass
		     :type clsql::integer)
     (arrival-time :initarg :arrival-time :accessor :arrival-time
		     :type clsql::integer)
     (departure-time :initarg :departure-time :accessor :departure-time
		     :type clsql::integer)
     (platform :initarg :platform :accessor :platform
	       :type clsql::string)))

  (clsql:def-view-class permitted ()
    ((id :accessor id
	 :type clsql::fixnum
	 :initarg :id
	 :db-constraints (:not-null :primary-key :unique))
     (from-crs-code :initarg :from-crs-code :accessor from-crs-code
		  :type clsql::string)
     (to-crs-code :initarg :to-crs-code :accessor to-crs-code
		  :type clsql::string)
     (done-london :initarg :done-london :accessor done-london
		  :type clsql::boolean)
     (algorithm :initarg :algorithm :accessor algorithm
		:type clsql::string)
     (answers :initarg :answers :accessor answers
		:type clsql::string)
     (timestamp :initarg :timestamp :accessor timestamp
		:type clsql::date)
     (version :initarg :version :accessor version
	      :type clsql::integer)))

  (clsql:def-view-class route ()
    ((id :accessor id
	 :type clsql::fixnum
	 :initarg :id
	 :db-constraints (:not-null :primary-key :unique))
     (from-crs-code :initarg :from-crs-code :accessor from-crs-code
		  :type clsql::string)
     (to-crs-code :initarg :to-crs-code :accessor to-crs-code
		  :type clsql::string)
     (done-london :initarg :done-london :accessor done-london
		  :type clsql::boolean)
     (answers :initarg :answers :accessor answers
		:type clsql::string)
     (timestamp :initarg :timestamp :accessor timestamp
		:type clsql::date)
     (version :initarg :version :accessor version
	      :type clsql::integer)))

(defmacro add-table-to-store (name)
  (with-gensyms (db)
    `(progn
       (with-train-db (,db)
	 ,(table name db)))))

(clsql:def-view-class ffl-fare ()
    ((id :accessor id
	 :type clsql::fixnum
	 :initarg :id
	 :db-constraints (:not-null :primary-key :unique))
     (flow-id :initarg :flow-id :accessor flow-id
	      :type clsql::string)
     (update-marker-record-type :initarg :update-marker-record-type :accessor update-marker-record-type
				:type clsql::string)
     (ticket-code :initarg :ticket-code :accessor ticket-code
		  :type clsql::string)
     (fare :initarg :fare :accessor fare
	   :type clsql::string)
     (restriction-code :initarg :restriction-code :accessor restriction-code :type clsql::string)))

(clsql:def-view-class postcode ()
  ((id :accessor id
       :type clsql::fixnum
       :initarg :id
       :db-constraints (:not-null :primary-key :unique))
   (short :initarg :short :accessor short
	  :type clsql::string)
   (post :initarg :post :accessor post
	 :type clsql::string)
   (latitude :initarg :latitude :accessor latitude
	     :type clsql::string)
   (longitude :initarg :longitude :accessor longitude
	      :type clsql::string)))

(clsql:def-view-class ffl-details ()
    ((id :accessor id
	 :type clsql::fixnum
	 :initarg :id
	 :db-constraints (:not-null :primary-key :unique))
     (flow-id :accessor flow-id
	 :type clsql::string
	 :initarg :flow-id)
     (update-marker-record-type :initarg :update-marker-record-type :accessor update-marker-record-type
				:type clsql::string)
     (origin :initarg :origin :accessor origin
	     :type clsql::string)
     (destination :initarg :destination :accessor destination
		  :type clsql::string)
     (route-code :initarg :route-code :accessor route-code
		 :type clsql::string)
     (status-code :initarg :status-code :accessor status-code
		  :type clsql::string)
     (usage-code :initarg :usage-code :accessor usage-code
		 :type clsql::string)
     (direction :initarg :direction :accessor direction
		:type clsql::string)
     (end-date :initarg :end-date :accessor end-date
	       :type clsql::string)
     (start-date :initarg :start-date :accessor start-date
		 :type clsql::string)
     (toc :initarg :toc :accessor toc
	  :type clsql::string)
     (cross-london :initarg :cross-london :accessor cross-london
		   :type clsql::string)
     (ns-disc :initarg :ns-disc :accessor ns-disc
	      :type clsql::string)
     (publication :initarg :publication :accessor publication
		  :type clsql::string)))

(clsql:def-view-class preprocessed ()
  ((id :accessor id
       :type clsql::fixnum
       :initarg :id
       :db-constraints (:not-null :primary-key :unique))
   (source :initarg :source :accessor source
	   :type clsql::integer)
   (target :initarg :target :accessor target
	   :type clsql::integer)
   (timepoint :initarg :timepoint :accessor timepoint
	      :type clsql::string)
   (day :initarg :day :accessor :day :type clsql::integer)
   (train-uid :initarg :train-uid :accessor train-uid
	      :type clsql::string)
   (start :initarg :start :accessor start
	      :type clsql::integer)))

(clsql:def-view-class mycommute ()
  ((id :accessor id
       :type clsql::fixnum
       :initarg :id
       :db-constraints (:not-null :primary-key :unique))
   (uid :initarg :uid :accessor uid
	   :type clsql::string)
   (fromcrs :initarg :fromcrs :accessor fromcrs
	   :type clsql::string)
   (tocrs :initarg :tocrs :accessor tocrs
	      :type clsql::string)))

(clsql:def-view-class fbuser ()
  ((id :accessor id
       :type clsql::fixnum
       :initarg :id
       :db-constraints (:not-null :primary-key :unique))
   (uid :initarg :uid :accessor uid
	:type clsql::string)
   (sessionkey :initarg :sessionkey :accessor sessionkey
	       :type clsql::string)
   (lastupdate :initarg :lastupdate :accessor lastupdate
	       :type clsql::string)))

(clsql:def-view-class dijkstra ()
  ((id :accessor id
       :type clsql::fixnum
       :initarg :id
       :db-constraints (:not-null :primary-key :unique))
   (source :initarg :source :accessor source
	   :type clsql::integer)
   (timepoint :initarg :timepoint :accessor timepoint
	      :type clsql::string)
   (day :initarg :day :accessor :day :type clsql::integer)
   (trains :initarg :trains :accessor trains
	   :type (clsql::varchar 50000))))

(clsql:def-view-class shortest ()
  ((id :accessor id
       :type clsql::fixnum
       :initarg :id
       :db-constraints (:not-null :primary-key :unique))
   (source :initarg :source :accessor source
	   :type clsql::string)
   (target :initarg :target :accessor target
	   :type clsql::string)
   (day :initarg :day :accessor :day :type clsql::integer)
   (journeys :initarg :journeys :accessor journeys
	   :type (clsql::varchar 500000))))

(clsql:def-view-class dijkstra-connections ()
  ((id :accessor id
       :type clsql::fixnum
       :initarg :id
       :db-constraints (:not-null :primary-key :unique))
   (source :initarg :source :accessor source
	   :type clsql::integer)
   (day :initarg :day :accessor :day :type clsql::integer)
   (connections :initarg :connections :accessor connections
	   :type (clsql::varchar 5000000))))

(clsql:def-view-class feedback ()
  ((id :accessor id
       :type clsql::fixnum
       :initarg :id
       :db-constraints (:not-null :primary-key :unique))
   (from-stn :initarg :from-stn :accessor from-stn
	     :type (clsql::varchar 100))
   (to-stn :initarg :to-stn :accessor to-stn
	   :type (clsql::varchar 100))
   (timestamp :initarg :timestamp :accessor timestamp
	      :type (clsql::varchar 100))
   (email :initarg :email :accessor email
	  :type (clsql::varchar 100))
   (comments :initarg :comments :accessor comments
	   :type (clsql::varchar 10000))))

  (defmacro defstore ()
    (with-gensyms (db)
      `(progn
	 (with-train-db (,db)
	   (clsql:create-sequence 'ids :database ,db)
	   ,(table 'nfm64 db)
	   ,(table 'schedules db)
	   ,(table 'stop db)
	   ,(table 'permitted db)
	   ,(table 'route db)
	   ,(table 'ffl-fare db)
	   ,(table 'ffl-details db)
	   ,(table 'preprocessed db)
	   ,(table 'postcode db)
	   ,(table 'dijkstra db)
	   ,(table 'shortest db)
	   ,(table 'dijkstra-connections db)
	   ,(table 'feedback db)
	   (clsql:create-index "my_fblast" :on (if *sqlite* "preprocessed" "trains.FBUSER") :attributes '("lastupdate") :database ,db)
	   (clsql:create-index "my_fbuid" :on (if *sqlite* "preprocessed" "trains.FBUSER") :attributes '("uid") :database ,db)
	   (clsql:create-index "my_uid" :on (if *sqlite* "preprocessed" "trains.MYCOMMUTE") :attributes '("uid") :database ,db)
	   (clsql:create-index "my_from" :on (if *sqlite* "preprocessed" "trains.MYCOMMUTE") :attributes '("fromcrs") :database ,db)
	   (clsql:create-index "my_to" :on (if *sqlite* "preprocessed" "trains.MYCOMMUTE") :attributes '("tocrs") :database ,db)
	   (clsql:create-index "f_from" :on (if *sqlite* "preprocessed" "trains.FEEDBACK") :attributes '("from_stn") :database ,db)
	   (clsql:create-index "f_to" :on (if *sqlite* "preprocessed" "trains.FEEDBACK") :attributes '("to_stn") :database ,db)
	   (clsql:create-index "c_day" :on (if *sqlite* "preprocessed" "trains.DIJKSTRA_CONNECTIONS") :attributes '("day") :database ,db)
	   (clsql:create-index "c_source" :on (if *sqlite* "preprocessed" "trains.DIJKSTRA_CONNECTIONS") :attributes '("source") :database ,db)
	   (clsql:create-index "s_day" :on (if *sqlite* "preprocessed" "trains.SHORTEST") :attributes '("day") :database ,db)
	   (clsql:create-index "s_target" :on (if *sqlite* "preprocessed" "trains.SHORTEST") :attributes '("target") :database ,db)
	   (clsql:create-index "s_source" :on (if *sqlite* "preprocessed" "trains.SHORTEST") :attributes '("source") :database ,db)
	   (clsql:create-index "d_day" :on (if *sqlite* "preprocessed" "trains.DIJKSTRA") :attributes '("day") :database ,db)
	   (clsql:create-index "d_source" :on (if *sqlite* "preprocessed" "trains.DIJKSTRA") :attributes '("source") :database ,db)
	   (clsql:create-index "d_timepoint" :on (if *sqlite* "preprocessed" "trains.DIJKSTRA") :attributes '("timepoint") :database ,db)
	   (clsql:create-index "preprocessed_day" :on (if *sqlite* "preprocessed" "trains.PREPROCESSED") :attributes '("day") :database ,db)
	   (clsql:create-index "preprocessed_source" :on (if *sqlite* "preprocessed" "trains.PREPROCESSED") :attributes '("source") :database ,db)
	   (clsql:create-index "preprocessed_target" :on (if *sqlite* "preprocessed" "trains.PREPROCESSED") :attributes '("target") :database ,db)
	   (clsql:create-index "preprocessed_start" :on (if *sqlite* "preprocessed" "trains.PREPROCESSED") :attributes '("start") :database ,db)
	   (clsql:create-index "preprocessed_timepoint" :on (if *sqlite* "preprocessed" "trains.PREPROCESSED") :attributes '("timepoint") :database ,db)
	   (clsql:create-index "postcode_short" :on (if *sqlite* "postcode" "trains.POSTCODE") :attributes '("short") :database ,db)
	   (clsql:create-index "postcode_post" :on (if *sqlite* "postcode" "trains.POSTCODE") :attributes '("post") :database ,db)
	   (clsql:create-index "ffldetails_nlc" :on (if *sqlite* "ffl_details" "trains.FFL_DETAILS") :attributes '("origin" "destination") :database ,db)
	   (clsql:create-index "ffldetails_flowid" :on (if *sqlite* "ffl_details" "trains.FFL_DETAILS") :attributes '("flow_id") :database ,db)
	   (clsql:create-index "fflfare_flowid" :on (if *sqlite* "ffl_fare" "trains.FFL_FARE") :attributes '("flow_id") :database ,db)
	   (clsql:create-index "permittedindex_route" :on (if *sqlite* "permitted" "trains.PERMITTED") :attributes '("from_crs_code" "to_crs_code") :database ,db)
	   (clsql:create-index "stopsindex_tiploc_code" :on (if *sqlite* "stop" "trains.STOP") :attributes '("tiploc_code") :database ,db)
	   (clsql:create-index "stopsindex_trainid" :on (if *sqlite* "stop" "trains.STOP") :attributes '("train_id") :database ,db)
	   (clsql:create-index "stopsindex_doesnt" :on (if *sqlite* "stop" "trains.STOP") :attributes '("doesnt_stop") :database ,db)
	   (clsql:create-index "stopsindex_departs" :on (if *sqlite* "stop" "trains.STOP") :attributes '("departs") :database ,db)
	   (clsql:create-index "stopsindex_arrives" :on (if *sqlite* "stop" "trains.STOP") :attributes '("arrives") :database ,db)
	   (clsql:create-index "stopsindex_arrival_time" :on (if *sqlite* "stop" "trains.STOP") :attributes '("arrival_time") :database ,db)
	   (clsql:create-index "stopsindex_departure_time" :on (if *sqlite* "stop" "trains.STOP") :attributes '("departure_time") :database ,db)
	   (clsql:create-index "nfm64index_fromnlc" :on (if *sqlite* "nfm64" "trains.NFM64") :attributes '("from_nlc_code") :database ,db)
	   (clsql:create-index "nfm64index_tonlc" :on (if *sqlite* "nfm64" "trains.NFM64") :attributes '("to_nlc_code") :database ,db)
	   (clsql:create-index "nfm64index_route" :on (if *sqlite* "nfm64" "trains.NFM64") :attributes '("route_code") :database ,db)
	   )))))
	       
(defun next-id ()
  (clsql:sequence-next 'ids :database (get-database-for-current-process)))

(defmethod clsql:query :around (expr &key (database nil) (result-types :auto) (flatp nil) (field-names t))
  (call-next-method expr :database (get-database-for-current-process) :result-types result-types :flatp flatp :field-names field-names))

(defmethod clsql:execute-command :around (expr &key (database nil))
  (call-next-method expr :database (get-database-for-current-process)))

(defmethod clsql:update-records-from-instance :around (obj &key (database nil))
  (call-next-method obj :database (get-database-for-current-process)))

(defun sql-select (&rest select-all-args)
  (apply #'clsql:select (append select-all-args (list :database (get-database-for-current-process)))))
