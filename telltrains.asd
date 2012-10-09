;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :com.sondesh.database.system)
    (defpackage :com.sondesh.database.system 
      (:use :asdf :cl))))

(in-package :com.sondesh.database.system)

(defsystem :sondesh 
    :name "database"
    :author "Vikram Bhandoh <vikram@sondesh.com>"
    :version "0.2"
    :description "All code for the sondesh database."
    :depends-on (:cl-ppcre :md5 :FiveAM :uffi :cl-html-parse :gzip-stream :split-sequence
		 :acl-compat :clsql-mysql :s-http-client :hunchentoot :cl-facebook :cl-who)
    :components
    ((:static-file "sondesh.asd")
     (:module :src
	      :components
	      ((:file "packages")
	       (:file "date" :depends-on ("packages"))
	       (:file "utilities" :depends-on ("packages" "date"))
	       (:file "fibonacci" :depends-on ("packages"))
	       (:file "importer" :depends-on ("packages" "date" "utilities"))
	       (:file "stations" :depends-on ("packages" "date" "importer" "utilities"))
	       (:file "lookups" :depends-on ("packages" "utilities" "date" "stations" "importer"))
	       (:file "db-defn" :depends-on ("packages" "date" "importer" "utilities" "stations"))       
	       (:file "train-reader" :depends-on ("packages" "date" "importer" "utilities" "stations" "db-defn"))
	       (:file "trains" :depends-on ("packages" "date" "importer" "utilities" "train-reader" "stations" "db-defn" "lookups"))
	       (:file "train-astar" :depends-on ("packages" "date" "importer" "utilities" "train-reader" "stations" 
							    "db-defn" "lookups" "trains"))
	       (:file "algorithms" :depends-on ("packages" "date" "importer" "utilities" "train-reader" "stations" 
							    "db-defn" "lookups" "trains" "fibonacci"))
	       (:file "fares" :depends-on ("packages" "date" "importer" "utilities" "stations" "db-defn"))
	       (:file "streams" :depends-on ("packages" "utilities" "lookups" "stations" "importer"))
	       (:file "tests" :depends-on ("packages" "db-defn" "utilities" "stations" "date" "fibonacci" "trains" "train-astar" "algorithms"))
	       (:file "hunchen-utils" :depends-on ("packages" "utilities" "date"))
	       (:file "ads" :depends-on ("packages" "utilities" "date"))
	       (:file "changes" :depends-on ("packages" "utilities" "date"))
	       (:file "journeystops" :depends-on ("packages" "utilities" "date"))
	       (:file "station-urls" :depends-on ("packages" "utilities" "lookups" "stations" "importer" "trains" "train-astar" "hunchen-utils"))
	       (:file "homepage" :depends-on ("packages" "utilities" "lookups" "stations" "importer" "trains" "train-astar" "hunchen-utils"))
	       (:file "timeline" :depends-on ("packages" "utilities" "lookups" "stations" "importer" "trains" "train-astar" "hunchen-utils" "changes"))
	       (:file "addjourneys" :depends-on ("packages" "utilities" "lookups" "stations" "importer" "trains" "train-astar" "hunchen-utils" "changes" "timeline"))
	       (:file "terms" :depends-on ("packages" "utilities" "lookups" "stations" "importer" "trains" "train-astar" "hunchen-utils" "changes"))
	       (:file "timeline-entrypoints" :depends-on ("packages" "utilities" "lookups" "stations" "importer" 
								     "trains" "train-astar" "hunchen-utils" "timeline"))
	       (:file "hunchentoot" :depends-on ("packages" "utilities" "lookups" "stations" "importer" "trains" "train-astar" 
						 "hunchen-utils" "station-urls" "homepage" "timeline-entrypoints" "journeystops" "terms" "timeline"))))))

;;(defmethod asdf:perform ((op asdf:test-op) (system (eql (find-system :sondesh))))
;;  (funcall (intern (string :run!) (string :com.sondesh.database)) 
;;	   :com.sondesh.database))
;;	       (:file "fb" :depends-on ("packages" "utilities"))
;;	       (:file "ucw-app" :depends-on ("packages" "utilities" "lookups" "stations" "importer" "trains" "train-astar" "fb"))
;;	       (:file "araneida" :depends-on ("packages" "utilities" "lookups" "ucw-app"))

