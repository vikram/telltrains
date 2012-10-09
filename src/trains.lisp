(in-package :com.sondesh.database)

;;rest of the functionality

;; figure out routes

(defun tree-search (states goal-p successors combiner)
  "Find a state that satisfies goal-p.  Start with states,
  and search according to successors and combiner."
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) *fail*)
        ((funcall goal-p (first states)) (first states))
        (t (tree-search
             (funcall combiner
                      (funcall successors (first states))
                      (rest states))
             goal-p successors combiner))))

(defun prepend (x y) "Prepend y to start of x" (append y x))

(defun breadth-first-search (start goal-p successors)
  "Search old states first until goal is reached."
  (tree-search (list start) goal-p successors #'prepend))

(defun sorter (cost-fn)
  "Return a combiner function that sorts according to cost-fn."
  #'(lambda (new old)
      (sort (append new old) #'< :key cost-fn)))
  
(defun best-first-search (start goal-p successors cost-fn)
  "Search lowest cost states first until goal is reached."
  (tree-search (list start) goal-p successors (sorter cost-fn)))

(defun beam-search (start goal-p successors cost-fn beam-width)
  "Search highest scoring states first until goal is reached,
  but never consider more than beam-width states at a time."
  (tree-search (list start) goal-p successors 
               #'(lambda (old new)
                   (let ((sorted (funcall (sorter cost-fn) old new)))
                     (if (> beam-width (length sorted))
                         sorted
                         (subseq sorted 0 beam-width))))))

(defun neighbors (stn-crs-code)
  (mapcar #'short-path-link-to-crs-grp-code
	  (gethash stn-crs-code *station-links*)))

(defconstant earth-diameter 12765.0
  "Diameter of planet earth in kilometers.")

(defun air-distance-likelyhood-for-train-both (city1 city2)
  (/ (air-distance city1 city2)
     (+ (station-rank (find-station-by-code city1))
	(station-rank (find-station-by-code city2)))))

(defun air-distance-likelyhood-for-train (city1 city2)
  (/ (air-distance city1 city2)
     (station-rank (find-station-by-code city2))))

(defun direct-connectivity (city1 city2)
  (let ((result 1))
    (dolist (from-tiploc (station-tiploc-codes (find-station-by-code city1)))
      (dolist (to-tiploc (station-tiploc-codes (find-station-by-code city2)))
	(awhen (gethash (concatenate 'string from-tiploc to-tiploc) *connectivity*)
	  (incf result it))))
    result))

(defun air-distance (city1 city2)
  "The great circle distance between two cities."
  (let ((d (distance (xyz-coords city1) (xyz-coords city2))))
    ;; d is the straight-line chord between the two cities,
    ;; The length of the subtending arc is given by:
    (* earth-diameter (asin (/ d 2)))))

(defun lat-long (city)
  (let* ((stn (find-station-by-code city)))
    (values (deg->radians (read-from-string (station-latitude stn)))
	    (deg->radians (read-from-string (station-longitude stn))))))

(defun other-air-distance (city1 city2)
  (multiple-value-bind (a1 b1) (lat-long city1)
    (multiple-value-bind (a2 b2) (lat-long city2)
      (* (/
	  (acos 
	   (+
	    (* (cos a1) (cos b1) (cos a2) (cos b2))
	    (* (cos a1) (sin b1) (cos a2) (sin b2))
	    (* (sin a1) (sin a2))))
	  360)
	 pi 
	 earth-diameter))))

(defun xyz-coords (city)
  "Returns the x,y,z coordinates of a point on a sphere.
  The center is (0 0 0) and the north pole is (0 0 1)."
  (let* ((stn (find-station-by-code city))
	 (psi (deg->radians (station-latitude stn)))
	 (phi (deg->radians (station-longitude stn))))
  (declare (double-float psi phi)
	   (optimize (speed 3) (safety 0)))
    (list (* (cos psi) (cos phi))
	  (* (sin psi) (sin phi))
          (sin psi))))

(defun distance (point1 point2)
  "The Euclidean distance between two points.
  The points are coordinates in n-dimensional space."
  (sqrt (reduce #'+ (mapcar #'(lambda (a b) 
				(declare (double-float a b)
					 (optimize (speed 3) (safety 0)))
				(expt (- a b) 2))
                            point1 point2))))

(defun deg->radians (deg)
  (declare (single-float deg)
	   (optimize (speed 3) (safety 0)))
  (* deg (the double-float pi) 0.0055555557))

(defun deg->radians-orig (deg)
  "Convert degrees and minutes to radians."
  (* (+ (truncate deg) (* (rem deg 1) 100/60)) pi 1/180))

(defun trip (start dest &optional (beam-width 1))
  "Search for the best path from the start to dest."
  (beam-search
    (make-path :state start)
    (is dest :key #'path-state :test #'string=)
    (path-saver #'neighbors #'air-distance
                #'(lambda (c) (air-distance c dest)))
    #'path-total-cost
    beam-width))

(defstruct (path (:print-function print-path))
  state (previous nil) (cost-so-far 0) (total-cost 0))

(defun is (value &key (key #'identity) (test #'eql))
  "Returns a predicate that tests for a given value."
  #'(lambda (path) (funcall test value (funcall key path))))

(defun path-saver (successors cost-fn cost-left-fn)
  #'(lambda (old-path)
      (let ((old-state (path-state old-path)))
        (mapcar
          #'(lambda (new-state)
              (let ((old-cost
                      (+ (path-cost-so-far old-path)
                         (funcall cost-fn old-state new-state))))
                (make-path
                  :state new-state
                  :previous old-path
                  :cost-so-far old-cost
                  :total-cost (+ old-cost (funcall cost-left-fn
                                                   new-state)))))
          (funcall successors old-state)))))

(defun map-path (fn path)
  "Call fn on each state in the path, collecting results."
  (if (null path)
      nil
      (cons (funcall fn (path-state path))
            (map-path fn (path-previous path)))))

(defun iter-wide-search (start goal-p successors cost-fn
                          &key (width 1) (max 100))
  "Search, increasing beam width from width to max.
  Return the first solution found at any width."
  (dbg :search "; Width: ~d" width)
  (unless (> width max)
    (or (beam-search start goal-p successors cost-fn width)
        (iter-wide-search start goal-p successors cost-fn
                           :width (+ width 1) :max max))))

(defun graph-search (states goal-p successors combiner
                     &optional (state= #'eql) old-states)
  "Find a state that satisfies goal-p.  Start with states,
  and search according to successors and combiner.  
  Don't try the same state twice."
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) *fail*)
        ((funcall goal-p (first states)) (first states))
        (t (graph-search
             (funcall
               combiner
               (new-states states successors state= old-states)
               (rest states))
             goal-p successors combiner state=
             (adjoin (first states) old-states
                     :test state=)))))

(defun new-states (states successors state= old-states)
  "Generate successor states that have not been seen before."
  (remove-if
    #'(lambda (state)
        (or (member state states :test state=)
            (member state old-states :test state=)))
    (funcall successors (first states))))

(defun next2 (x) (list (+ x 1) (+ x 2)))

(defun a*-search (paths goal-p successors cost-fn cost-left-fn
                  &optional (state= #'eql) old-paths)
  "Find a path whose state satisfies goal-p.  Start with paths,
  and expand successors, exploring least cost first.
  When there are duplicate states, keep the one with the
  lower cost and discard the other."
  (if (not paths)
      (dbg :search ";; Search: no paths" nil)
      (dbg :search ";; Search: ~a" (list paths)))
  (cond
    ((null paths) *fail*)
    ((funcall goal-p (first paths))
     (progn
       (dbg :search-done ";; found: ~a" (list paths))
       (values (first paths) (rest paths))))
    (t (let* ((path (pop paths))
              (state (path-state path)))
	 (dbg :search ";; Considering: ~a :state ~A" (list path state))
         ;; Update PATHS and OLD-PATHS to reflect
         ;; the new successors of STATE:
         (setf old-paths (insert-path path old-paths))
	 (let ((states (funcall successors state path)))
	   (if (not states)
	       (dbg :search ";; Successors: none" nil)
	       (dbg :search ";; Successors: ~a" (list states)))
	   (dolist (state2 states)
	     (let* ((cost (+ (path-cost-so-far path)
			     (funcall cost-fn state state2)))
		    (cost2 (funcall cost-left-fn state2))
		    (path2 (make-path
			    :state state2 :previous path
			    :cost-so-far cost
			    :total-cost (+ cost cost2)))
		    (old nil))
	       ;; Place the new path, path2, in the right list:
	       (dbg :search ";; Next: ~a cost:~a cost-left:~a path2:~a" (list state2 cost cost2 path2))
	       (cond
		 ((setf old (find-path state2 paths state=))
		  (when (better-path path2 old)
		    (setf paths (insert-path
				 path2 (delete old paths)))))
		 ((setf old (find-path state2 old-paths state=))
		  (when (better-path path2 old)
		    (setf paths (insert-path path2 paths))
		    (setf old-paths (delete old old-paths))))
		 (t (setf paths (insert-path path2 paths)))))))
	 (dbg :search ";;Calling with: ~a" (list paths))
	 ;; Finally, call A* again with the updated path lists:
	 (a*-search paths goal-p successors cost-fn cost-left-fn
		    state= old-paths)))))

(defun find-path (state paths state=)
  "Find the path with this state among a list of paths."
  (find state paths :key #'path-state :test state=))

(defun better-path (path1 path2)
  "Is path1 cheaper than path2?"
  (let ((c1 (path-total-cost path1))
	(c2 (path-total-cost path2)))
    (if (< (abs (- c1 c2)) 0.01)
	(> (length (path-states path1)) (length (path-states path2)))
	(< c1 c2))))

(defun better-path-orig (path1 path2)
  "Is path1 cheaper than path2?"
  (< (path-total-cost path1) (path-total-cost path2)))

(defun insert-path (path paths)
  "Put path into the right position, sorted by total cost."
  ;; MERGE is a built-in function
  (merge 'list (list path) paths #'< :key #'path-total-cost))

(defun path-states (path)
  "Collect the states along this path."
  (if (null path)
      nil
      (cons (path-state path)
            (path-states (path-previous path)))))

(defun search-all (start goal-p successors cost-fn beam-width)
  "Find all solutions to a search problem, using beam search."
  ;; Be careful: this can lead to an infinite loop.
  (let ((solutions nil))
    (beam-search
      start #'(lambda (x)
                (when (funcall goal-p x) (push x solutions))
                nil)
      successors cost-fn beam-width)
    solutions))

(defun a-trip (start dest)
  "Search for the best path from the start to dest."
  (let ((result (a*-search
		 (list (make-path :state start))
		 (is dest :test #'string= :key #'path-state)
		 #'(lambda (state path) (neighbors state))
		 #'air-distance
		 #'(lambda (c) (air-distance c dest))
		 #'string=)))
    (unless (eql result 'FAIL)
      (nreverse
       (path-states result)))))

(defun station-link-distance (from to)
  (dolist (dest (gethash from *station-links*))
    (when (string= (second dest) to)
      (return-from station-link-distance (read-from-string (third dest)))))
  (air-distance from to))

(defmemoize 
    station-link-dist 
    #'equal
    (from to)
  (path-total-cost (station-link from to)))

(defun station-link (from to)
  (let ((result (a*-search
		 (list (make-path :state (route-crs (find-station-by-code from))))
		 #'(lambda (path) (string= (route-crs (find-station-by-code (path-state path))) (route-crs (find-station-by-code to))))
		 #'(lambda (state path) 
		     (remove-if #'(lambda (res) (member res (map-path #'identity path) :test #'string=))
				(delete-duplicates (flatten (mapcar #'neighbors (station-all-crs-codes 
										 (find-station-by-code
										  (route-crs (find-station-by-code state)))))) :test #'string=)))
		 #'station-link-distance
		 #'(lambda (c) (air-distance c to))
		 #'string=)))
    (dbg :search-dist "station-distance :~A" (list result))
    result))

(defun route-crs (stn)
  (if (and (station-group-crs-code stn) (string/= "" (station-group-crs-code stn)))
      (station-group-crs-code stn)
      (station-crs-code stn)))

(defun map-station-names (crs-codes)
  (mapcar #'(lambda (x) (station-name (assoc x *stations* :test #'string=))) crs-codes))

(defun permitted-neighbors (permitted-route &key (allow-through-london-p nil))
  #'(lambda (stn-crs-code path)
      (let ((route (rest permitted-route)))
	(awhen (mapcar #'first
		     (remove-if-not #'(lambda (link) 
					(member (second link) route :test #'string=))
				    (gethash stn-crs-code *linked-stations*)))
	  (dbg :search ";;neighbors ~A" (list it))
	  (remove-if #'(lambda (crs) (member crs (map-path #'identity path) :test #'string=))
		     (if allow-through-london-p
			 it
			 (remove-if-not #'(lambda (x) (string/= "G01" x)) it)))))))

(defun find-permitted-routes (start-crs dest-crs)
  "Also generates the london routes"
  (remove-if-not #'(lambda (route) (string= (first route) dest-crs))
		 (gethash start-crs *permitted-routes*)))

(defun permitted-trips (start dest &key (allow-through-london-p nil))
  (let* ((start-stn (find-station-by-code start))
	 (dest-stn (find-station-by-code dest))
	 (start-crs (if (station-routeing-location-p start-stn)
			(route-crs start-stn)
			(first (station-routeing-points start-stn))))
	 (dest-crs (if (station-routeing-location-p dest-stn)
		       (route-crs dest-stn)
		       (first (station-routeing-points dest-stn)))))
    (clear-memoize 'station-link-dist)
    (permitted-route->trips (find-permitted-routes start-crs dest-crs) start-crs dest-crs :allow-through-london-p allow-through-london-p)))

(defun permitted-route->trips (permitted-routes start-crs dest-crs &key (allow-through-london-p nil))
  (let ((results '()))
    (dolist (permitted-route permitted-routes)
      (let ((int (sort (permitted-route-trip (list (make-path :state start-crs))
						   dest-crs permitted-route allow-through-london-p)
		       (compose #'not #'better-path))))
	(dolist (result (delete-duplicates int :test #'(lambda (p1 p2) (< (abs (- (path-total-cost p1) (path-total-cost p2))) 0.01))))
	  (push result results))))
    (setf results (sort (delete-duplicates results :test #'path=) #'better-path))
    (remove-if-not #'(lambda (p) (< (abs (- (path-total-cost p) (path-total-cost (first results)))) 10)) results)))

(defun permitted-route-trip-very-old (starts dest-crs permitted-route allow-through-london-p &optional (acc '()))
  (let ((results '()))
    (awhen (a*-search
	    starts
	    (is dest-crs :test #'string= :key #'path-state)
	    (permitted-neighbors permitted-route :allow-through-london-p allow-through-london-p)
	    #'air-distance
	    #'(lambda (c) (air-distance c dest-crs))
	  #'string=)
      (unless (eql *fail* it)
	(push it results)))))

(defun permitted-route-trip (starts dest-crs permitted-route allow-through-london-p &optional (acc '()))
  (multiple-value-bind (it others)
      (a*-search
       starts
       (is dest-crs :test #'string= :key #'path-state)
       (permitted-neighbors permitted-route :allow-through-london-p allow-through-london-p)
       #'station-link-dist
       #'(lambda (c) (station-link-dist c dest-crs))
       #'string=)
    (if (eql it 'FAIL)
	acc
	(permitted-route-trip 
	 (filter-others others it)
	 dest-crs permitted-route allow-through-london-p (cons it acc)))))

(defun filter-others (others it)
  (remove-if-not #'(lambda (other)
		     (< (abs (- (path-total-cost other) (path-total-cost it))) 80))
		 others))

(defun permitted-route-trip-old (starts dest-crs permitted-route allow-through-london-p &optional (acc '()))
  (multiple-value-bind (it others)
      (a*-search
       starts
       (is dest-crs :test #'string= :key #'path-state)
       (permitted-neighbors permitted-route :allow-through-london-p allow-through-london-p)
       #'air-distance
       #'(lambda (c) (air-distance c dest-crs))
       #'string=)
    (if (eql it 'FAIL)
	acc
	(permitted-route-trip (remove-if-not #'(lambda (other)
						 (< (- (path-total-cost other)
						       (path-total-cost it))
						    5))
					     others)
			      dest-crs permitted-route allow-through-london-p (cons it acc)))))

(defun path= (p1 p2)
  (and (= (path-total-cost p1) (path-total-cost p2))
       (equal (map-path (compose #'station-display-name #'find-station-by-code) p1)
	      (map-path (compose #'station-display-name #'find-station-by-code) p2))))

(defun print-path (path &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "#<Path ~,1f km: ~{~:(~a~)~^ - ~}>"
          (path-total-cost path) 
	  (reverse (map-path #'identity path))))

(defun find-simple-direct-trains-list (from-tiploc to-tiploc &key (passing-through-dest nil))
  (labels ((stops-for-tiploc (tiploc)
	     (decode-stops (gethash (tiploc->id tiploc) *schedules*))))
    (let ((fromlist (stops-for-tiploc from-tiploc))
	  (tolist (stops-for-tiploc to-tiploc)))
      (let ((from (sort (mapcar #'(lambda (train) (first train))
			  (remove-if-not #'(lambda (train) 
					     (/= 0 (third train)))
					 fromlist)) #'<))
	    (to (sort (mapcar #'(lambda (train) (first train))
			(remove-if-not #'(lambda (train)
					   (if (not passing-through-dest)
					       (/= 0 (fourth train))
					     t))
				       tolist)) #'<)))
	(declare (sequence fromlist tolist from to)
		 (optimize (speed 3) (safety 0)))
	(print from)
	(print to)
	 (if (longer to from)
	     (intersection from to :test #'eq)
	     (intersection to from :test #'eq))))))

(defun make-train-vector (lst)
  (aif (sort (mapcar #'(lambda (train) (first train)) lst) #'<)
       (let* ((len (length it))
	      (result (make-array (list len)))
	      (i 0))
	 (dolist (var it)
	   (setf (svref result i) var)
	   (incf i))
	 (values result len))
       (values (make-array '(0)) 0)))

(defun vec-intersect (from to from-len to-len)
  (declare (simple-vector from to) (fixnum from-len to-len)
	    (optimize (speed 3) (safety 0)))
  (cond ((or (not from-len) (not to-len)) nil)
	((or (zerop from-len) (zerop to-len)) nil)
	((or (not from) (not to)) nil)
	((> from-len to-len) (intersect to from to-len from-len))
	(t (intersect from to from-len to-len))))

(defun intersect (a b alen blen)
  (declare (simple-vector a b) (fixnum alen blen)
	   (optimize (speed 3) (safety 0)))
  (let ((results '()))
    (dotimes (i alen)
      (declare (integer i))
      (let ((obj (svref a i)))
	(let ((it (finder obj b 0 (the fixnum (- blen 1)))))
	  (if it
	      (push it results)))))
    results))

(defun finder (obj vec start end)
  (declare (simple-vector vec) (fixnum obj start end)
	   (optimize (speed 3) (safety 0)))
  (let ((range (- end start)))
    (declare (fixnum range))
    (if (>= 0 range)
	(if (eq obj (svref vec start))
	    obj
	    nil)
	(let* ((mid (+ start (the fixnum (round (/ range 2)))))
	       (obj2 (svref vec mid)))
	  (declare (fixnum mid obj2))
	  (if (< obj obj2)
	      (finder obj vec start (- mid 1))
	      (if (> obj obj2)
		  (finder obj vec (+ mid 1) end)
		  obj))))))
  
(defun valid-orig-dest (train-ids from-tiploc to-tiploc)
  (declare (sequence train-ids)
	   (optimize (speed 3) (safety 0)))
  (remove-if-not
   #'(lambda (train) (member (tiploc->id to-tiploc) 
			     (member (tiploc->id from-tiploc)
				     (mapcar #'fifth (cdr train))
				     :test #'=)
			     :test #'=))
   (mapcar #'(lambda (trainid) (cons (decode-train (car (gethash trainid *trains*)))
				     (decode-stops (cdr (gethash trainid *trains*)))))
	   train-ids)))

#|
  (remove-if-not #'(lambda (trainid) 
		     (let ((train-stops (mapcar #'fifth (decode-stops (cdr (gethash trainid *trains*))))))
		       (member (tiploc->id to-tiploc) 
			       (member (tiploc->id from-tiploc) train-stops :test #'=) :test #'=)))
		 train-ids))
|#

(defmemoize 
    stops->info 
    #'equal
    (all-stops from to)
  (mstops->info all-stops from to))

(defun mstops->info (all-stops from to)
  (let* ((tiplocs (append (station-original-tiplocs from)
			  (station-original-tiplocs to)))
	 (start-end (remove-if-not #'(lambda (stop) 
				       (member (fifth stop) tiplocs
					       :test #'string=))
				   all-stops))
	 (start (first start-end))
	 (end (car (last start-end)))
	 (st (num->time (third start)))
	 (et (num->time (fourth end))))
    (list (tiploc->station-name (fifth start)) st
	  (tiploc->station-name (fifth end)) et
	  "Train" (train-duration st et))))

(defun find-direct-trains (orig dest)
  (find-direct-trains-between-stations (first (find-station-by-letters orig))
				       (first (find-station-by-letters dest))))

(defun trainid->train-stops (trainid)
   (mapcar (compose #'id->tiploc #'fifth)
	   (decode-stops (rest (gethash trainid *trains*)))))

(defun find-simple-direct-trains (from-tiploc to-tiploc &key (passing-through-dest nil) (passing-through-orig nil))
  (labels ((stops-for-tiploc (tiploc)
	     (decode-stops (gethash (tiploc->id tiploc) *schedules*))))
    (let ((fromlist (stops-for-tiploc from-tiploc))
	  (tolist (stops-for-tiploc to-tiploc)))
      (multiple-value-bind (from from-len) 
	  (make-train-vector 
	   (remove-if-not #'(lambda (train) (if (not passing-through-orig)
						(/= 0 (third train))
						t))
			  fromlist))
	(multiple-value-bind (to to-len)
	    (make-train-vector 
	     (remove-if-not #'(lambda (train) (if (not passing-through-dest)
						  (/= 0 (fourth train))
						  t))
			    tolist))
	(declare (sequence fromlist tolist) (simple-vector from to) (fixnum from-len to-len)
		 (optimize (speed 3) (safety 0)))
	(valid-orig-dest
	 (vec-intersect from to from-len to-len)
	 from-tiploc to-tiploc))))))

(defun
    find-direct-trains-all-stops 
    (from to &key (time (now)) (time-fn (make-time-test-fn :time time)) (passing-through-dest t) (passing-through-orig nil))
  (let ((results '()))
    (dolist (train (find-trains-from-to from to :passing-through-dest passing-through-dest :passing-through-orig passing-through-orig))
      (when (train-time-test-p time-fn train)
	(let* ((all-stops (all-stops (rest train)))
	       (start-end (stops->info all-stops from to)))
	  (unless (time-test-p time-fn start-end)
	    (push (list all-stops (list start-end) (first train)) results)))))
    results))

(defun train-time-test-p (time-fn train)
  (funcall (funcall time-fn 'train) (first train)))

(defun time-test-p (time-fn start-end)
  (funcall (funcall time-fn 'short) (second start-end)))

(defun find-trains-from-to (from to &key (passing-through-dest nil) (passing-through-orig nil))
  (find-trains-tiplocs (station-tiploc-codes from) (station-tiploc-codes to) passing-through-dest passing-through-orig))

(defun mfind-trains-tiplocs (from to passing-through-dest passing-through-orig)
  (let ((results '()))
    (dolist (from-tiploc from)
      (dolist (to-tiploc to)
	(setf results (union results 
			     (find-simple-direct-trains from-tiploc to-tiploc 
							:passing-through-dest passing-through-dest
							:passing-through-orig passing-through-orig)
			     :key #'caar
			     :test #'=))))
    (delete-duplicates results :key #'caar :test #'=)))
  
(defmemoize find-trains-tiplocs #'equal
    (from to passing-through-dest passing-through-orig)
  (mfind-trains-tiplocs from to passing-through-dest passing-through-orig))

(defun find-direct-trains-between-stations (from to &key (time (now)))
  (let ((results (find-direct-trains-all-stops from to :passing-through-dest nil :time time)))
    (sort 
     (mapcar #'second results)
     (make-journey< time))))

(defun make-journey< (&optional (time (now)))
  #'(lambda (t1 t2)
      (let* ((starttime (format nil "~2,'0d:00" (time->hr time)))
	     (time1 (or (and (string< starttime (second (car t1))) (second (car t1)))
			(add-times (second (car t1)) "24:00")))
	     (time2 (or (and (string< starttime (second (car t2))) (second (car t2)))
			(add-times (second (car t2)) "24:00"))))
	(if (string= time1 time2)
	    (string< (journey-duration t1) (journey-duration t2))
	    (train< time1 time2 (current-short-time time))))))

(defun train-duration (start-time end-time)
  (multiple-value-bind (hr min) 
      (floor (timedifference-nd end-time start-time) 60)
    (hrmin->short-time hr min)))

(defun num->time (num)
  (hrmin->short-time (floor num 100)
		     (rem num 100)))

(defun tiploc->station-name-slow (tiploc)
  (station-display-name
   (find tiploc *stations*
	 :test #'(lambda (x y) (member x (station-tiploc-codes y) :test #'string=)))))

(defun tiploc->station-name (tiploc)
  (aif (routeing-tiploc-p tiploc)
       (station-display-name (find-station-by-code it))
       (tiploc->station-name-slow tiploc)))

(defun train-valid-p (decoded-train &key (time (get-universal-time)))
  (let ((from (date2time (yymmdd->simple-date (format nil "~6,'0d" (third decoded-train)))))
	(to (date2time (yymmdd->simple-date (format nil "~6,'0d" (fourth decoded-train))))))
    (multiple-value-bind (se mi ho da mo ye dw dst tz) (decode-universal-time time 0)
      (and (= 1 (nth (1+ dw) (second decoded-train)))
	   (>= time from)
	   (<= time to)))))

(defun possible-journeys (start end &key (time (get-universal-time)) (time-fn (make-time-test-fn :time time)) (summary-sorted-p nil))
  (possible-journeys-routes (FIRST (find-station-by-letters start))
			    (first (find-station-by-letters end))
			    :time time
			    :time-fn time-fn
			    :summary-sorted-p summary-sorted-p))

(defun possible-journeys-routes (from to &key (time (get-universal-time)) (time-fn (make-time-test-fn :time time)) (summary-sorted-p nil))
  (declare (list from to)
	   (optimize (speed 3) (safety 0)))
  (let ((results '()))
    (clear-memoize 'find-direct-trains-all-stops)
    (clear-memoize 'stops->info)
    (clear-memoize 'yymmdd->simple-date)
    (aif (permitted-trips (route-crs from) (route-crs to))
	 (dolist (path it)
	   (let ((detailed-path (reverse (map-path #'find-station-by-code path))))
	     (setf results (append (train-with-changes detailed-path (car (last detailed-path)) :time-fn time-fn) results))))
	 (setf results nil))
    (clear-memoize 'find-direct-trains-all-stops)
    (clear-memoize 'stops->info)
    (clear-memoize 'yymmdd->simple-date)
    (if summary-sorted-p
	(sort-result-summarize results time)
	results)))

(defun detailed-paths-trips (paths)
  (mapcar #'(lambda (path) (reverse (map-path #'find-station-by-code path))) paths))

(defun sort-result-summarize (results time)
  (declare (list results)
	   (optimize (speed 3) (safety 0)))
  (sort 
   (delete-duplicates 
    (mapcar #'journey-summary results) 
    :test #'equal)
   (make-journey< time)))

(defun train-with-changes (detailed-path dest 
			   &key (time (get-universal-time)) 
			        (time-fn (make-time-test-fn :time time))
			        (options (journey-leg->trains (first detailed-path) 
							      (second detailed-path) detailed-path time-fn)))
  (declare (list detailed-path options) (function time-fn)
	   (optimize (speed 3) (safety 0)))
  (let ((results '())
	(do-again nil))
    (dolist (option options)
      (when (latest-stop option)
	(if (string= (the simple-base-string (station-crs-code (latest-stop option)))
		     (the simple-base-string (station-crs-code dest)))
	    (push option results)
	    (progn
	      (aif (filter-legs (journey-leg->trains (latest-stop option)
						     (next-stop 
						      (member (latest-stop option) detailed-path 
							      :test #'(lambda (s1 s2) (string= (the simple-base-string (station-crs-code s1))
											       (the simple-base-string (station-crs-code s2)))))
						      (latest-stop option))
						     detailed-path
						     time-fn)
				option)
		   (let ((result (best-journey it option time detailed-path)))
		     (setf do-again t)
		     (push (append (list (append (car option) (car result))) (append (cdr result) (cdr option))) results)))))))
    (if do-again
	(train-with-changes detailed-path dest :time-fn time-fn :options results)
	(mapcar #'(lambda (result) (append (list (car result)) (reverse (cdr result)))) results))))

(defun filter-legs (journey-legs option)
  (declare (list journey-legs)
	   (optimize (speed 3) (safety 0)))
  (remove-if-not #'(lambda (journey-leg)
		     (let ((dur (timedifference-nd (journey-start-time journey-leg)
						   (journey-end-time option)))
			   (time-for-change (parse-integer (station-minimum-change-time (latest-stop option)))))
		       (declare (fixnum dur time-for-change))
		       (and (not (stn-in-p (second-last-stop option) (journey-stops journey-leg)))
			    (<= time-for-change dur)
			    (< dur (+ 180 time-for-change))
			    (date-matches-p (third (car (last (cdr option)))) (third (car (cdr journey-leg)))))))
		 journey-legs))
	
(defun date-matches-p (t1 t2)
  (declare (list t1 t2)
	   (optimize (speed 3) (safety 0)))
  (let ((from1 (third t1))
	(to1 (fourth t1))
	(from2 (third t2))
	(to2 (fourth t2)))
    (declare (fixnum from1 to1 from2 to2))
    (and (<= from1 to2) 
	 (<= from2 to1)
	 (equal (second t1) (second t2)))))

(defun journey< (t1 t2 &optional (time (now)))
  (let* ((starttime (format nil "~2,'0d:00" (time->hr time)))
	 (time1 (or (and (string< starttime (second (car t1))) (second (car t1)))
		    (add-times (second (car t1)) "24:00")))
	 (time2 (or (and (string< starttime (second (car t2))) (second (car t2)))
		    (add-times (second (car t2)) "24:00"))))
    (if (string= time1 time2)
	(string< (journey-duration t1) (journey-duration t2))
	(train< time1 time2 (current-short-time time)))))

(defun best-journey (journeys option time detailed-path)
  (first (sort journeys
	       #'(lambda (r1 r2)
		   (aif (= (far-along-on-route r1 detailed-path) 
			   (far-along-on-route r2 detailed-path))
			(journey<  
			 (journey-summary (append (list (append (car option) (car r1))) (append (cdr r1) (cdr option))))
			 (journey-summary (append (list (append (car option) (car r2))) (append (cdr r2) (cdr option))))
			 time)
			(< (far-along-on-route r1 detailed-path) 
			   (far-along-on-route r2 detailed-path)))))))

(defun journey-leg->trains (from to detailed-path time-fn)
  (awhen (mapcar #'(lambda (train) (readjust-train train detailed-path from))
		 (append (find-direct-trains-all-stops from to :time-fn time-fn)
			 (find-direct-trains-all-stops from (car (last detailed-path)) :time-fn time-fn)))
    it))

(defstruct display-train
  from to pos summary stops terminates-at)

(defun readjust-train (train detailed-path from &key (adjust-stops-p nil))
  (let ((stops (mapcar #'fifth (reverse (first train)))))
    (dolist (rp (reverse (cdr detailed-path)))
      (awhen (stn-in-p rp stops)
	(when (validate-readjusted-train-p (reverse stops) (station-tiploc-codes from) (station-tiploc-codes rp))
	  (let ((cleaned-rp (copy-list rp)))
	    (setf (station-tiploc-codes cleaned-rp) (list (first it)))
	    (return-from readjust-train 
	      (if (not adjust-stops-p)
		  (list (list from cleaned-rp) 
			(list (first train) (stops->info (first train) from cleaned-rp) 
			      (car (last train))))
		  (make-display-train
		   :from from
		   :to cleaned-rp
		   :summary (stops->info (first train) from cleaned-rp)
		   :stops (adjust-stops (car (last train)) from cleaned-rp)
		   :terminates-at (car (last (first train))))
		    ))))))))

(defun adjust-stops (train-stops from cleaned-rp)
  (reverse (STN-IN-STOPS cleaned-rp (reverse (STN-IN-STOPS from train-stops)))))

(defun stn-in-stops (stn stops)
  (aif (stn-in-p stn (mapcar #'fifth stops))
       (member (first it) stops :test #'string= :key #'fifth)))

(defun validate-readjusted-train-p (train-stops from-tiplocs to-tiplocs)
  (dolist (from from-tiplocs)
    (dolist (to to-tiplocs)
      (if (member to (member from train-stops :test #'string=) :test #'string=)
	  (return-from validate-readjusted-train-p t)))))

(defun next-stop (path last-stop)
  (second path))

(defun next-stop-other (path last-stop)
  (car (first
   (sort 
    (mapcar 
     #'(lambda (next) 
	 (cons next
	       (reduce #'+
		 (mapcar #'(lambda (tiploc)
			     (aif (gethash (concatenate 'string (car (station-tiploc-codes last-stop)) tiploc) *connectivity*)
				  it
				  0))
			 (station-tiploc-codes next)))))
     (if (cddr path)
	 (butlast (cdr path))
	 (cdr path)))
    #'>
    :key #'cdr))))

(defun journey-stops (journey-so-far)
  (mapcar #'fifth (first (car (cdr journey-so-far)))))

(defun far-along-on-route (route detailed-path)
  (length (member (latest-stop route) detailed-path :test #'(lambda (s1 s2) (string= (station-crs-code s1) (station-crs-code s2))))))

(defun journey-start-time (journey-so-far)
  (second (second (car (cdr journey-so-far)))))

(defun journey-end-time (journey-so-far)
  (fourth (second (car (cdr journey-so-far)))))

(defun latest-stop (journey-so-far)
    (car (last (car journey-so-far))))

(defun second-last-stop (journey-so-far)
  (car (last (butlast (car journey-so-far)))))

(defun stn-in-p (stn stops)
  (let ((options (all-members (station-tiploc-codes stn) stops :test #'string=)))
    (sort
     options
     #'>
     :key #'(lambda (option) (station-rank (find-station-by-code (gethash option *tiploc->crs*)))))))
    
(defun journey-summary (journey)
  (mapcar #'(lambda (train) (second train)) (cdr journey)))

(defparameter *aresults*
  '("G08" "G07" "G06" "G05" "G04" "G03" "G02" "G01" "FPK" "FAV" "ESL" "ERL" "EPS"
    "ELY" "DVP" "DON" "DID" "DFD" "DEE" "DAR" "CYP" "CTR" "CRV" "CRS" "CRE" "CPM"
    "COV" "COL" "CLY" "CLJ" "CHD" "CDF" "CBG" "CAR" "BTH" "BTG" "BTB" "BSK" "BON"
    "BNY" "BKJ" "BKG" "BGN" "BFD" "BDM" "BBN" "AHV" "AFK" "ACT" "ABD"
    "G10" "G11" "G12" "G13" "G14" "G15" "G16" "G17" "G18" "G19"
    "G20" "G21" "G22" "G23" "G24" "G25" "G26" "G27" "G28" "G29" "G30" "G31" "G32"
    "G33" "G34" "G35" "G36" "G37" "G38" "G39" "G40" "G41" "G42" "G43" "G44" "G45"
    "G46" "G47" "G48" "G49" "G50" "G51" "G52" "G53" "G54" "G55" "G56" "G57" "G58"
    "G59" "G60" "G61" "G62" "G63" "G64" "G65" "G66" "G67" "G68" "G69" "G70" "G71"
    "GFD" "GLD" "GOO" "GRA" "GRP" "GTW" "GUI" "HAV" "G09"))

(defun make-time-test-fn (&key (time (get-universal-time)))
  (multiple-value-bind (se mi ho da mo ye dw dst tz) (decode-universal-time time 0)
    (let ((time-date (+ da (* 100 mo) (* 10000 (rem ye 100))))
	  (short-time (current-short-time time)))
      #'(lambda (dispatch)
	  (case dispatch
	    (time #'(lambda () short-time))
	    (fulltime #'(lambda () time))
	    (change-window #'(lambda (mins) (make-time-test-fn :time (+ (* 60 mins) time))))
	    (change #'(lambda (short-time1) (awhen (timedifference short-time1 short-time)
					      (if (< it 0)
						  (make-time-test-fn :time (+ (* 60 (+ 1440 it)) time))
						  (make-time-test-fn :time (+ (* 60 it) time))))))
	    (short #'(lambda (time1) (if (< (timedifference time1 short-time) 0)
					 (<= (timedifference "08:00" time1) 0)
					 (< (timedifference time1 short-time) 0))))
	    (train #'(lambda (decoded-train)
		       (declare (fixnum time-date) (optimize (speed 3) (safety 0)))
		       (and (= 1 (the fixnum (nth (1+ dw) (second decoded-train))))
			    (>= time-date (the fixnum (third decoded-train)))
			    (<= time-date (the fixnum (fourth decoded-train)))))))))))

(defun make-time-test-fn2 (&key (time (get-universal-time)))
  (multiple-value-bind (se mi ho da mo ye dw dst tz) (decode-universal-time time 0)
    (declare (ignore se mi ho dw dst tz) (fixnum da mo ye))
    (let ((time-date (+ da (* 100 mo) (* 10000 (rem ye 100)))))
      #'(lambda (dispatch)
	  (case dispatch
	    (change #'(lambda (short-time1) (awhen (timedifference short-time1 (current-short-time time))
					      (if (< it 0)
						  (make-time-test-fn2 :time (+ (* 60 (+ 1440 it)) time))
						  (make-time-test-fn2 :time (+ (* 60 it) time))))))
	    (short #'(lambda (time1) nil))
	    (train #'(lambda (decoded-train)
		       (declare (fixnum time-date) (optimize (speed 3) (safety 0)))
		       (and (>= time-date (the fixnum (third decoded-train)))
			    (<= time-date (the fixnum (fourth decoded-train)))))))))))

(defun permitted-journey-details (from-crs-code)
    #'(lambda (journey) 
	(let ((rs (mapcar (compose #'first #'decode-train) journey)))
	  (if (member from-crs-code *aresults* :test #'string=) 
	      (mapcar #'train-details (reverse (delete-duplicates rs :test #'=)))
	      (mapcar #'train-details rs)))))

(defun our-get-unique-trains-changes (from to time)
  (let ((from-crs-code (route-crs from))
	(to-crs-code (route-crs to))
	(time-fn (make-time-test-fn :time time)))
    (journeys+directs 
     (if (permitted-trips from-crs-code to-crs-code)
	 (remove-if-not
	  #'(lambda (journey) (include-journey-p from to time-fn journey))
	  (mapcar (permitted-journey-details from-crs-code)
		  (get-permitted-journeys from-crs-code to-crs-code)))
	 nil)
     from to time)))

(defun journeys+directs (journeys from to time)
   (sort 
    (remove-if #'not (mapcar #'(lambda (journey) (summarize-journey-from-to journey from to))
			     (if journeys
				 journeys
				 (mapcar #'(lambda (jour) (list 
							   (list
							    (train-info (path-state jour))
							    (train-stops (path-state jour)))))
					 (direct-trains->path from to (make-time-test-fn :time time) (list from to))))))
    (make-journey< time)
    :key #'display-journey-summary))

(defun summarize-journey-from-to (journey origin to &optional (acc nil))
  (labels ((summary (train start end)
	     (let ((sum (stops->info (car (rest train)) start end)))
	       (if (string= (first sum) (third sum))
		   nil
		   (make-display-train
		    :from start
		    :to end
		    :summary (stops->info (car (rest train)) start end)
		    :stops (adjust-stops (car (last train)) start end)
		    :terminates-at (car (last (car (rest train))))))))
	   (destination (train next-train)
	     (find-station-by-code 
	      (gethash 
	       (fifth (first 
		       (intersection (first (rest train)) (first (rest next-train)) :key #'fifth 
				     :test #'(lambda (stop1 stop2) (or (string= stop1 stop2)
								       (string= 
									(gethash stop1 *tiploc->crs*) 
									(gethash stop2 *tiploc->crs*)))))))
	       *tiploc->crs*))))
    (let ((dest nil))
      (if (not (second journey))
	  (aif (summary (first journey) origin to)
	       (reverse (cons it acc))
	       nil)
	  (progn
	    (setf dest (destination (first journey) (second journey)))
	    (aif (summary (first journey) origin dest)
		 (summarize-journey-from-to (rest journey) dest to (cons it acc))
		 nil))))))

(defun display-journey-summary (journey)
  (mapcar #'display-train-summary journey))

(defun include-journey-p (from to time-fn journey) 
  (let ((stops (mappend #'(lambda (train)
			    (mapcar #'fifth (second train)))
			journey)))
    (and
     (stn-in-p from stops)
     (stn-in-p to stops)
     (funcall (funcall time-fn 'train) (first (first journey))))))

(defun our-get-unique-trains-changes-no-summary (from to time)
  (let ((from-crs-code (route-crs from))
	(to-crs-code (route-crs to))
	(time-fn (make-time-test-fn :time time)))
    (aif (mapcar #'(lambda (path)
		     (reverse (map-path #'find-station-by-code path)))
		 (permitted-trips from-crs-code to-crs-code))
	 (append
	  (let ((journeys (get-permitted-journeys from-crs-code to-crs-code))
		(cleaned-paths (mapcar #'(lambda (path) (append (list from) (butlast (cdr path)) (list to))) it)))
	    (remove-if #'not
	    (mapcar
	     #'(lambda (journey) (detailed-journey->path journey cleaned-paths))
	     (remove-if-not #'(lambda (journey)
			       (let ((stops (mappend #'(lambda (train)
							    (mapcar #'fifth (second train)))
						     journey)))
				 (and
				  (stn-in-p from stops)
				  (stn-in-p to stops)
				  (funcall (funcall time-fn 'train) (first (first journey))))))
			   (mapcar #'(lambda (journey) 
				       (let ((rs (mapcar (compose #'first #'decode-train) journey)))
					 (if (member from-crs-code *aresults* :test #'string=) 
					     (mapcar #'train-details (reverse (delete-duplicates rs :test #'=)))
					     (mapcar #'train-details rs))))
				   journeys)))))
	  (direct-trains->path from to (make-time-test-fn :time time) (list from to)))
	 (direct-trains->path from to (make-time-test-fn :time time) (list from to)))))

(defparameter *only-directs* '(("G01" . "GRA")
			       ("GRA" . "G01")))

(defun get-permitted-journeys-db (from-crs-code to-crs-code)
  (with-train-database
    (awhen (caar (clsql:query 
		  (format nil
			  "select answers from permitted where permitted.from_crs_code='~A' and permitted.to_crs_code='~A'"
			  from-crs-code to-crs-code)))
      (if (member (cons from-crs-code to-crs-code) *only-directs* :test #'equal)
	  (remove-if-not #'(lambda (j) (< (length j) 3)) (read-from-string it))
	  (read-from-string it)))))

(defun get-permitted-journeys (from-crs-code to-crs-code)
  (declare (dynamic-extent ht))
  (aif (get-permitted-journeys-db from-crs-code to-crs-code)
       it
       nil))

#|
       (let* ((key (concatenate 'string from-crs-code to-crs-code))
	      (ht (make-hash-table :test 'equal)))
	 (unless *permitted-journeys-loaded* 
	   (load-permitted-trains from-crs-code ht))
	 (gethash key ht))))
|#

(defun detailed-journey->path (journey detailed-paths)
  (labels ((train-maker (it journey)
	     (make-train :from (first (find-station-by-letters (first (second (second it)))))
			 :to (first (find-station-by-letters (third (second (second it)))))
			 :stops (car (rest (first journey)))
			 :info (first (first journey))))
	   (summary->path (journey path from result)
	     (if (not journey)
		 result
		 (aif (readjust-train (rest (first journey)) path from)
		      (if (string= (station-crs-code (second (first it))) (station-crs-code (car (last path))))
			  (make-path :state (train-maker it journey) :previous result)
			  (summary->path (rest journey) path (second (first it)) 
					 (make-path :state (train-maker it journey) 
						    :previous result)))
		      nil))))
    (dolist (path detailed-paths)
      (awhen (summary->path journey path (first path) nil)
	(return-from detailed-journey->path it)))
    nil))

(defun summarize-journey (journey detailed-paths)
  (labels ((summary (journey path from result)
	     (if (not journey)
		 result
		 (aif (readjust-train (rest (first journey)) path from :adjust-stops-p t)      
		      (if (string= (station-crs-code (display-train-to it)) (station-crs-code (car (last path))))
			  (push it result)
			  (summary (rest journey) path (display-train-to it) (push it result)))
		      nil))))
    (dolist (path detailed-paths)
      (aif (summary journey path (first path) nil)
	   (return-from summarize-journey (reverse it))))
    nil))

(defun reasonable-change-window (journey-summary)
  (map-subsequent-two #'(lambda (t1 t2)
			  (when (and t1 t2)
			    (let ((diff (timedifference (second t2) (fourth t1))))
			      (when (or (and (< diff 0) (> (timedifference (second t2) (fourth t1) :next-day-p t) 300))
					(> diff 300))
				(return-from reasonable-change-window nil)))))
	   journey-summary #'append)
  t)

(defun find-saved-trains (from to &optional (time (now)))
  (nreverse
   (filter-adjacent 
    (delete-duplicates 
     (remove-if #'not
		(mapcar 
		 #'(lambda (journey)
		     (let ((summary (funcall (journey-pos time) (display-journey-summary journey))))
		       (if (reasonable-change-window (second summary))
			   (cons summary journey)
			   nil)))
		 (our-get-unique-trains-changes from to time)))
     :test #'equal
    :key (compose #'second #'first)))))

(defun journey-pos (time)
  #'(lambda (x) 
      (let ((start (timedifference (journey-start x) (current-short-time time)))
	    (end  (timedifference (journey-end x) (current-short-time time))))
	(if (and (< start 0) (> end 0))
	    (list (cons start (+ (timedifference-nd (journey-end x) (current-short-time time))
				 (timedifference "00:00" "23:59")))
		  x)
	    (list (cons start end) x)))))

(defparameter *precomputed-routes* '())
(defparameter *crs->id* (make-hash-table :test 'equal :size 8000 :rehash-size 1.1))
(defparameter *id->crs* (make-hash-table :test 'equal :size 8000 :rehash-size 1.1))

(let ((id 0))
  (defun crs->id (crs)
    (aif (gethash crs *crs->id*)
	 it
	 (progn
	   (setf (gethash crs *crs->id*) (incf id))
	   (setf (gethash id *id->crs*) crs)
	   id))))

(defun id->crs (id)
  (gethash id *id->crs*))

(defun read-precomputed-routes ()
  (if (probe-file (sondesh-file-path "permitted-paths.precomputed"))
      (progn
	(with-open-file (in (sondesh-file-path "permitted-paths.precomputed") :direction :input)
	  (setf *precomputed-routes* (read in)))
	(setf *crs->id* (read-hashtable (sondesh-file-path "crs.id")))
	(setf *id->crs* (read-hashtable (sondesh-file-path "id.crs"))))
      (let ((results '()))
	(maphash #'(lambda (k v)
		     (dolist (route (group v :test #'string=))
		       (dolist (path-list (mapcar #'(lambda (path)
						      (let ((results '()))
							(map-path #'(lambda (state) (push state results)) path)
							(mapcar #'crs->id results)))
						  (permitted-trips k (first route))))
			 (push path-list results))))
		 *permitted-routes*)
	(with-open-file (out (sondesh-file-path "permitted-paths.precomputed") :direction :output :if-exists :supersede)
	  (write results :stream out))
	(write-hashtable (sondesh-file-path "crs.id") *crs->id*)
	(write-hashtable (sondesh-file-path "id.crs") *id->crs*)
	(dolist (crs1 (hash-keys *crs->id*))
	  (let ((stop1 (find-station-by-code crs1)))
	    (dolist (crs2 (hash-keys *crs->id*))
	      (let ((stop2 (find-station-by-code crs2)))
		(when (string/= crs1 crs2)
		  (setf (gethash (concatenate 'string crs1 crs2) *AVERAGE-TIME-STOPS*)
			(average-time-stops stop1 stop2)))))))
	(write-hashtable (sondesh-file-path "average.timestops") *average-time-stops*)
	(dolist (crs (hash-keys *crs->id*))
	  (dolist (tiploc (station-tiploc-codes (find-station-by-code crs)))
	    (setf (gethash (make-logic-symbol tiploc) *routeing-tiplocs*)
		  (station-crs-code (find tiploc *stations*
					  :test #'(lambda (x y) (member x (station-tiploc-codes y) :test #'string=)))))))
	(write-hashtable (sondesh-file-path "routeing.tiplocs") *routeing-tiplocs*)
	(setf *precomputed-routes* results))))

;;(read-precomputed-routes)

(defun push-paths ()
  (with-train-database!
    (maphash #'(lambda (k v)
		 (dolist (route (group v :test #'string=))
		   (dolist (path-list (mapcar #'(lambda (path)
						  (let ((results '()))
						    (map-path #'(lambda (state) (push state results)) path)
						    (mapcar #'crs->id results)))
					    (permitted-trips k (first route))))
		     (let ((id (next-id)))
		     (print id)
		     (clsql:update-records-from-instance 
		      (make-instance 'route
				     :id id
				     :from-crs-code k
				     :to-crs-code (first route)
				     :done-london nil
				     :answers (write-to-string path-list)
				     :timestamp (multiple-value-bind (se mi ho da mo ye) (decode-universal-time (now) 0)
						  (clsql:make-date :year ye :month mo :day da :hour ho :minute mi :second se))
				     :version 1))))))
		 *permitted-routes*)))

(defun recompute-journeys (from-crs-code to-crs-code)
  (let ((results (delete-duplicates 
		  (mapcar #'(lambda (journey) 
			      (map-path #'(lambda (train) 
					    (let ((train-info (train-info train)))
					      (+ (* (first train-info) 1000000000000000)
						 (* (convert-bit-string-to-integer (second train-info)) 
						    1000000000000)
				  (* (third train-info) 1000000)
				  (fourth train-info))))
					journey))
			  (possible-journeys-routes-a* (find-station-by-code from-crs-code) 
						       (find-station-by-code to-crs-code)
						       :time (now)
						       :time-fn (make-time-test-fn2)))
		  :test #'equal)))
    (with-train-database! 
      (clsql:update-records-from-instance 
       (make-instance 'permitted
		      :id (aif 
			   (clsql:query 
			    (format nil 
				    "select id from permitted where from_crs_code='~A' and to_crs_code='~A'" 
				    from-crs-code 
				    to-crs-code))
			   (progn
			     (clsql:execute-command (format nil "delete from permitted where id=~A" (parse-integer (caar it))))
			     (parse-integer (caar it)))
			   (1+ (parse-integer (caar (clsql:query (format nil "select max(id) from permitted"))))))
		      :from-crs-code from-crs-code
		      :to-crs-code to-crs-code
		      :done-london nil
		      :algorithm "a*"
		      :answers (write-to-string results)
		      :timestamp (multiple-value-bind (se mi ho da mo ye) (decode-universal-time (now) 0)
				   (clsql:make-date :year ye :month mo :day da :hour ho :minute mi :second se))
		      :version 2)))))

(defun connections->list (start end connections mtime length)
  (let ((fn (compose #'third #'simple-decode-train-stop2 #'first)))
    (if (< end start)
	start
	(progn
	  (let* ((center (+ start (floor (- end start) 2)))
		 (mid (if (= center length)
			  (1- length)
			  center))
		 (time (funcall fn (svref connections mid))))
	    (if (> time mtime)
		(connections->list start (1- mid) connections mtime length)
		(if (< time mtime)
		    (if (= center length)
			0
			(connections->list (1+ mid) end connections mtime length))
		    mid)))))))

(defun array->list (array start end)
  (let ((results '()))
    (when (= end start)
      (setf start (1- end)))
    (dotimes (i (- end start))
      (push (svref array (+ start i)) results))
    (nreverse results)))

(defun elementary-connections (from to time-test-fn mtime previous)
  (let* ((transfer-time (+ 3 (parse-integer (station-minimum-change-time (find-station-by-code from)))))
	 (transfer-time-test-fn (funcall (funcall time-test-fn 'change-window) transfer-time))
	 (connects (gethash (gethash to *hubs*) (svref *neighbours* (gethash from *hubs*))))
	 (start (connections->list 0 (car connects) (cdr connects) mtime (car connects))))
      (dolist (i (map0-n #'(lambda (n) (if (>= (+ start n) (car connects))
					   (- (+ start n) (car connects))
					   (+ start n)))
			     (1- (car connects))))
	(let* ((connection (aref (cdr connects) i))
	       (train (train-details (first (first connection)) :routeing-p t))
	       (stops (second train))
	       (shorttime (multiple-value-bind (hr min)
			      (floor (rem (second (first connection)) 10000) 100)
			    (hrmin->short-time hr min))))
	  (when (and train (train-valid-p (first train) :time (funcall (funcall time-test-fn 'fulltime))))
	    (unless (if (or (not previous) (= (first (first connection)) (first (first previous))))
			(funcall (funcall time-test-fn 'short) shorttime)
			(funcall (funcall transfer-time-test-fn 'short) shorttime))
	      (return-from elementary-connections 
		(list (first train)
		      (find from stops :test #'string= :key #'fifth)
		      (find to stops :test #'string= :key #'fifth)))))))))

(defun fixed-neighbours (from)
  (svref *fixed-neighbours-consolidate* (gethash from *hubs*)))

(defun neighbours (from)
  (svref *neighbours-consolidate* (gethash from *hubs*)))

(defun filter-adjacent (journeys &optional (acc nil))
  (if (not journeys)
      acc
      (if (not (second journeys))
	  (cons (first journeys) acc)
	  (if (comparable-journeys-p (first journeys) (second journeys))
	      (if (better-journey-p (first journeys) (second journeys))
		  (filter-adjacent (cons (first journeys) (cddr journeys)) acc)
		  (filter-adjacent (cdr journeys) acc))
	      (filter-adjacent (cdr journeys) (cons (first journeys) acc))))))

(defun comparable-journeys-p (j1 j2)
  (let ((t1 (first (second (first j1))))
	(t2 (first (second (first j2)))))
    (< (abs (timedifference (second t1) (second t2))) 3)))

(defun better-journey-p (j1 j2)
  (let ((jd1 (short-time->mins (journey-duration (second (first j1)))))
	(jd2 (short-time->mins (journey-duration (second (first j2))))))
    (or (< jd1 jd2)
	(and (= jd1 jd2) (< (length (second (first j1))) (length (second (first j2))))))))

