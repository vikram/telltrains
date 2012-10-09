(in-package :com.sondesh.database)

(defparameter *common-names* '("london" "junction" "airport" "east" "west" "north" "south" "thames" "station" "any" "harbour"))
(defparameter *common-bit-in-names* '("junction" "airport" "station" "any" "bus" "port" "park"))
(defstruct trie (value nil) (arcs nil))
(defparameter *trie-deleted* 'deleted)
(defparameter *station-trie* (make-trie))
(defparameter *postcode-trie* (make-trie))

(defun follow-arc (component extend? trie)
  (let ((arc (assoc component (trie-arcs trie))))
    (cond ((not (null arc)) (cdr arc))
	  ((not extend?) nil)
	  (t (let ((new-trie (make-trie)))
	       (push (cons component new-trie)
		     (trie-arcs trie))
	       new-trie)))))

(defun find-trie (key extend? trie)
  (cond ((null trie) nil)
	((atom key)
	 (if key
	     (follow-arc key extend? trie)
	     trie))
	(t (find-trie
	    (cdr key) extend?
	    (find-trie
	     (car key) extend?
	     (find-trie 
	      "." extend? trie))))))

(defun put-trie (key trie value)
  (let ((found (find-trie key t trie)))
    (if (trie-value found)
	(setf (trie-value found) (cons value (trie-value found)))
	(setf (trie-value found) (list value)))))

(defun get-trie (key trie)
  (let* ((key-trie (find-trie key nil trie))
	 (val (if key-trie (trie-value key-trie))))
    (if (or (null key-trie) (eql val *trie-deleted*))
	(values nil nil)
	(values val t))))

(defun delete-trie (key trie)
  (put-trie key trie *trie-deleted*))

(defun recurse-trie-for-value (trie &optional (acc '()))
  (if (not trie)
      acc
      (if (atom trie)
	  (if (trie-value trie)
	      (recurse-trie-for-value (trie-arcs trie) (append (trie-value trie) acc))
	      (recurse-trie-for-value (trie-arcs trie) acc))
	  (recurse-trie-for-value (cdr trie) 
				  (recurse-trie-for-value (cdr (car trie)) acc)))))

(defun find-station-by-code-letters (letters)
  #'(lambda (code)
      (let* ((stn (copy-list (find-station-by-code code)))
	     (name (find letters (station-nicknames stn) 
			 :test #'string-subseq=)))
	(when (and name (string/= (string-downcase name) (string-downcase (station-display-name stn))))
	  (setf (station-display-name stn)
		(string-upcase (concatenate 'string name "( " (station-display-name stn) " )"))))
	stn)))

(defun string-subseq= (letts nickname)
  (if (> (length letts) (length nickname))
      (string= (subseq (string-downcase letts) 0 (length nickname)) (string-downcase nickname))
      (string= (subseq (string-downcase nickname) 0 (length letts)) (string-downcase letts))))

(defun find-station-by-exact-name (name)
  (let* ((lower-name (string-downcase name))
	 (stns (mapcar (find-station-by-code-letters lower-name)
		       (delete-duplicates (recurse-trie-for-value 
					   (find-trie (map 'list #'identity lower-name) nil *station-trie*))
					  :test #'string=))))
    (or
     (find lower-name stns :test #'string= :key (compose #'string-downcase #'station-display-name))
      (real-station-by-letters name))))

(defun find-station-by-letters (letters)
  (let ((lower-letters (string-downcase letters)))
    (sort
     (mapcar (find-station-by-code-letters lower-letters)
	     (delete-duplicates (recurse-trie-for-value 
				 (find-trie (map 'list #'identity lower-letters) nil *station-trie*))
				:test #'string=))
     #'(lambda (s1 s2)
	 (let ((s1-name (string-downcase (station-display-name s1)))
	       (s2-name (string-downcase (station-display-name s2))))
	   (cond ((and (string= s1-name lower-letters) (string/= s2-name lower-letters)) t)
		 ((and (string= s2-name lower-letters) (string/= s1-name lower-letters)) nil)
		 ((= (station-rank s1) (station-rank s2)) (< (edit-distance s1-name lower-letters) 
							     (edit-distance s2-name lower-letters)))
		 (t (> (station-rank s1) (station-rank s2)))))))))

(defun real-station-by-letters (letters)
  (let* ((candidates (find-station-by-letters letters))
	 (group (find-station-by-code (route-crs (first candidates)))))
    (if (member group candidates :test #'equal)
	group
	(first candidates))))

(defstruct (station (:type list)) 
  crs-code name display-name rank nicknames postcode location toc fulladdress latitude longitude
  tiploc-codes crs-reference minimum-change-time footnote-code sub-sector-code routeing-location-p 
  in-group-p group-name nlc-code nlc-group-code group-crs-code routeing-points all-crs-codes 
  original-tiplocs original-crs-codes)

(defun combine-duplicates (lst &optional (acc '()))
  (let ((fr (copy-list (first lst))))
    (if (not lst)
	(nreverse acc)
	(if (find fr acc :test #'(lambda (stn1 stn2) (string= (station-crs-code stn1)
							      (station-crs-code stn2))))
	    (combine-duplicates (rest lst) acc)
	    (aif (remove-if-not #'(lambda (stn) (string= (station-crs-code fr)
							 (station-crs-code stn)))
				(rest lst))
	       (progn
		 (setf (station-tiploc-codes fr) (append (station-tiploc-codes fr)
							 (mapcar #'station-tiploc-codes it)))
		 (setf (station-rank fr) (+ (station-rank fr) (reduce #'+ it :key #'station-rank)))
		 (combine-duplicates (rest lst) (push fr acc)))
	       (progn
		 (setf (station-tiploc-codes fr) (station-tiploc-codes fr))
		 (combine-duplicates (rest lst) (push fr acc))))))))

(defparameter *stations* 
  (COMBINE-DUPLICATES 
   (remove-if #'(lambda (stn) (= (station-rank stn) 0))
	      (with-open-file (in (sondesh-file-path "station.details")) (read in)))))

(defparameter *stations-hash* (make-hash-table :test 'equal))

(defun name-starts (name)
  (cons name 
	(map-two #'(lambda (a b) (list (subseq name b)))
		 (cl-ppcre:all-matches
		  (cl-ppcre:create-scanner "[ ]") 
		  (cl-ppcre:regex-replace-all " - any station" name ""))
		 #'append)))

(defparameter *station-groups* 
  (with-open-file (in (sondesh-file-path "station.groups")) (read in)))

(defun load-stations ()
  (progn
    (dolist (stn *stations*)
      (if (gethash (station-crs-code stn) *stations-hash*)
	  (if (not (station-latitude (gethash (station-crs-code stn) *stations-hash*)))
	      (setf (gethash (station-crs-code stn) *stations-hash*) stn))
	  (setf (gethash (station-crs-code stn) *stations-hash*) stn))
      (dolist (nickname (station-nicknames stn))
	(dolist (nick (name-starts (string-downcase nickname)))
	  (unless (member nick *common-bit-in-names* :test #'string-subseq=)
	    (put-trie (map 'list #'identity nick) *station-trie* (station-crs-code stn))))
	(put-trie (map 'list #'identity (string-downcase nickname)) *station-trie* (station-crs-code stn))))
    (dolist (stngrp *station-groups*)
      (dolist (nickname (station-nicknames stngrp))
	(dolist (nick (name-starts (string-downcase nickname)))
	  (unless (member nick *common-bit-in-names* :test #'string-subseq=)
	    (put-trie (map 'list #'identity nick) *station-trie* (station-crs-code stngrp))))
	(put-trie (map 'list #'identity (string-downcase nickname)) *station-trie* (station-crs-code stngrp))))))

(defun find-station-by-letters-old (letters)
  (let ((scanner (cl-ppcre:create-scanner (format nil "\\b~A" letters) :case-insensitive-mode t))
	(lower-letters (string-downcase letters)))
    (sort 
     (append
      (remove-if-not #'(lambda (station) 
			 (cl-ppcre:all-matches scanner (reduce #'(lambda (a b) (concatenate 'string a "," b))
							       (station-nicknames station))))
		     *stations*)
      (remove-if-not #'(lambda (station) 
			 (cl-ppcre:all-matches scanner (reduce #'(lambda (a b) (concatenate 'string a "," b))
							       (station-nicknames station))))
		     *station-groups*))
     #'(lambda (s1 s2) 
	 (let ((s1-name (string-downcase (station-display-name s1)))
	       (s2-name (string-downcase (station-display-name s2))))
	   (cond ((and (string= s1-name lower-letters) (string/= s2-name lower-letters)) t)
		 ((and (string= s2-name lower-letters) (string/= s1-name lower-letters)) nil)
		 ((= (station-rank s1) (station-rank s2)) (< (edit-distance s1-name lower-letters) 
							     (edit-distance s2-name lower-letters)))
		 (t (> (station-rank s1) (station-rank s2)))))))))
			   
(defun find-station (code)
  (aif (assoc code *stations* :test #'string=)
       (station-name it)
       ""))

(load-stations)

(defparameter *tiploc->crs* (make-hash-table :test 'equal :size 8000))

(dolist (stn *stations*)
  (dolist (tiploc (station-tiploc-codes stn))
    (if (gethash tiploc *TIPLOC->CRS*)
	(print (list "problem" stn tiploc))
	(setf (gethash tiploc *tiploc->crs*) (station-crs-code stn)))))

(defun find-station-by-code (code)
  (or (gethash code *stations-hash*)
      (assoc code *station-groups* :test #'string=)))

(defun load-tube-stations ()
  (let ((start 0))
    (with-open-file (in (sondesh-file-path "tubes"))
      (mapcar #'(lambda (tube)
		  (let ((crs (format nil "TUBE~A" (incf start))))
		    (make-station
		     :crs-code crs
		     :name (second tube)
		     :display-name (concatenate 'string (second tube) " tube")
		     :rank 0.1
		     :nicknames (second tube)
		     :postcode (first tube)
		     :location ""
		     :toc "Transport for London"
		     :fulladdress (concatenate 'string (first tube) " London")
		     :latitude (third tube)
		     :longitude (fourth tube)
		     :tiploc-codes nil
		     :crs-reference crs
		     :minimum-change-time "5"
		     :footnote-code nil
		     :sub-sector-code nil
		     :routeing-location-p nil
		     :in-group-p nil
		     :group-name nil
		     :nlc-code nil
		     :nlc-group-code nil
		     :group-crs-code nil
		     :routeing-points nil
		     :all-crs-codes (list crs))))
	      (read in)))))

(defun load-postcodes ()
  (let ((i 0))
    (with-open-file (in (sondesh-file-path "pcs/pcs"))
      (do ((line (read-line in nil)
		 (read-line in nil)))
	  ((null line))
	(awhen (and line (split-sequence:split-sequence #\comma line))
	  (when (zerop (mod (incf i) 1000))
	    (print (cons i it)))
	  (put-trie (map 'list #'identity (string-downcase (first it))) *postcode-trie* it))))))

(defun find-postcode-by-letters (letters)
  (let ((lower-letters (string-downcase letters)))
    (recurse-trie-for-value 
     (find-trie (map 'list #'identity lower-letters) nil *postcode-trie*))))

;;(load-postcodes)

(defun cleanup-groups ()
  (dolist (grp *station-groups*)
    (let ((name (subseq (station-display-name grp) 0 (- (length (station-display-name grp)) 14))))
      (dolist (crs (station-all-crs-codes grp))
	(unless (cl-ppcre:all-matches-as-strings (string-capitalize name) (string-capitalize (station-display-name (find-station-by-code crs))))
	  (setf (station-tiploc-codes grp) (remove-if
					    #'(lambda (tiploc) (member tiploc (station-tiploc-codes (find-station-by-code crs)) :test #'string=))
					    (station-tiploc-codes grp)))
	  (setf (station-all-crs-codes grp) (remove crs (station-all-crs-codes grp) :test #'string=))
	  (setf (station-group-crs-code (find-station-by-code crs)) nil)
	  (setf (station-routeing-points (find-station-by-code crs)) (list (station-crs-code grp))))))))

(cleanup-groups)
