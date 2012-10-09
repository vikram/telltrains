(in-package :com.sondesh.database)

(defun name->pixels (name)
  (* (length name) 5.5))

(defun short-name (name)
  (let ((frags (cl-ppcre:split "[ -]" name))	
	(result '()))
    (dolist (frag frags)
      (if (member (string-downcase frag) *common-names* :test #'string=)
	  (push (string-capitalize (shortened-name (string-downcase frag))) result)
	  (push (string-capitalize frag) result)))
    (reduce #'(lambda (a1 a2) (concatenate 'string a1 " " a2)) (reverse result))))

(defun shortened-name (name)
  (casequal name
    ("airport" "apt")
    ("north" "nth")
    ("south" "sth")
    ("junction" "jn")
    ("london" "lon")
    ("east" "est")
    ("west" "wst")
    ("harbour" "hbr")
    (t (let* ((n (car (split-sequence:split-sequence #\- name)))
	      (l (length n)))
	 (trim (concatenate 'string (subseq n 0 1)
			    (cl-ppcre:regex-replace-all "[aeiouAEIOU]" (subseq n 1 (1- l)) "")
			    (subseq n (1- l) l)))))))

(defun time-pattern (times)
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (time times)
      (awhen (split-sequence:split-sequence #\: time)
	(if (gethash (first it) ht)
	    (push (second it) (gethash (first it) ht))
	    (setf (gethash (first it) ht) (list (second it))))))
    ht))

(defun journey->starttime (journey)
  (float (* 360/1440 (- (short-time->mins (second (car (cadr journey)))) 60))))

(defun get-range (journeys journey-time-fn)
  (aif (mapcar #'(lambda (ordered-journey)
		   (let ((journey (car (cdr ordered-journey))))
		     (funcall journey-time-fn journey)))
	       journeys)
       (if (string< (car (last it)) (first it))
	   (cons (first it) (list "24:00"))
	   (cons (first it) (last it)))))

(defun get-start-range (journeys)
  (get-range journeys #'journey-start))

(defun get-end-range (journeys)
  (get-range journeys #'journey-end))

(defun time->slider-value (time hour &optional (next-day-p nil))
  (if next-day-p
      (+ 96 (floor (timedifference time (format nil "~2,'0d:00" hour)) 15))
      (floor (timedifference time (format nil "~2,'0d:00" hour)) 15)))

(defun time->slider (time ho)
  (if (second (split-sequence:split-sequence #\+ time))
      (time->slider-value (first (split-sequence:split-sequence #\+ time)) ho t)
      (time->slider-value time ho)))

(defun peak-time (legs)
  (let ((start (journey-start legs)))
    (if (and (string> start "06:00")
	     (string< start "10:00"))
	"true"
	"false")))

(defun extract-route (legs)
  (mapcar #'car legs))

(defun route= (r1 r2)
  (string=
   (reduce #'(lambda(x y) (concatenate 'string x y)) r1)
   (reduce #'(lambda(x y) (concatenate 'string x y)) r2)))

(defun route-index (legs routes)
  (- (length routes)
     (length (member (extract-route legs) routes :test #'route=))))

(defun routes (trains)
  (delete-duplicates
   (mapcar #'(lambda (journey) 
	       (extract-route (cadr journey)))
	   trains)
   :test #'route=))

(defun make-id (legs leaving-date today-p new-id-p)
  (format nil "J~A~A~A~A"
	  (remove #\: (journey-start legs))
	  (remove #\: (journey-end legs))
	  (if today-p
	      leaving-date
	      (dttm->string (+ (string->dttm leaving-date) (* 24 3600)) :format :caldate))
	  (if new-id-p "other" "")))

(defun journeys->json (trains ho leaving-date &optional (new-id-p nil))
  (let ((routes (routes trains)) (today-p t))
    (format nil "[~{~A~^, ~}]"
	    (mapcar #'(lambda (journey) 
			(let ((legs (cadr journey)))
			  (when today-p
			    (when (string/= (dttm->string (now) :format :caldate) leaving-date)
			      (setf today-p
				    (< 0 (timedifference (journey-start legs) (current-short-time))))))
			  (format nil "{id:'~A',start:~A,end:~A,changes:~A,peak:~A,route:~A,stopping:false,weekend:false,starttime:~A,pos:~A}"
				  (make-id legs leaving-date today-p new-id-p)
				  (time->slider-value (journey-start legs) ho (< (caar journey) 0))
				  (time->slider-value (journey-end legs) ho (< (cdar journey) 0))
				  (length legs)
				  (peak-time legs)
				  (route-index legs routes)
				  (journey->starttime journey)
				  (timedifference-nd (journey-start legs) (current-short-time)))))
		    trains))))

(defun xfactor (trains)
  (aif (get-start-range trains)
       (/ (timedifference-nd (second it) (first it)) 18 (length trains))
       1))

(defun left-indent (pos)
  (format nil "left: ~Apx;" pos))

(defun align-time (time now)
  (- (round (timedifference time "05:00" :next-day-p (string< time (current-short-time now)))) 50))

(defun change-leftpos (t1 t2 deptime name prev)
  (let* ((change-window (round (timedifference-nd (second t2) (fourth t1))))
	 (last-end-point (+ 60 (round (timedifference-nd (fourth t1) deptime))))
	 (name-length (name->pixels name))
	 (mid-point (floor (/ change-window 2)))
	 (it (- (+ change-window last-end-point) name-length)))
    (if (> 5 (abs (- it prev)))
	(if (> mid-point name-length)
	    (+ last-end-point mid-point 5)
	    (+ 10 it))
	(+ prev 10))))

(defun start-position (proposed-pos)
  (fourth (first proposed-pos)))

(defun end-position (proposed-pos)
  (second (first (last proposed-pos))))

(defun change-size (change)
  (third change))

(defun slot-size (slot)
  (nth 9 slot))

(defun slot-size-other (slot)
  (nth 11 slot))

(defun slot-start (slot)
  (second slot))

(defun slot-start-other (slot)
  (nth 5 slot))

(defun change-positions (display-trains proposed-pos)
  (let* ((start-pos (start-position proposed-pos))
	 (end-pos (end-position proposed-pos))
	 (changes (remove-if-not #'(lambda (x) (eql 'change (first x))) proposed-pos))
	 (trains (remove-if-not #'(lambda (x) (eql 'train (first x))) proposed-pos))
	 (slots (change-slots trains)))
    (if (> (reduce #'+ changes :key #'third) (+ (- end-pos start-pos) (* 10 (length changes))))
	(if (> (name->pixels "click for details") (- end-pos start-pos))
	    '()
	    (list (list "click for details" (float (/ end-pos 2)) 0)))
	(let ((results (map 'list 
			   #'(lambda (change slot display-train)
			       (list (string-capitalize (short-name (third (display-train-summary display-train))))
				     (if (> (slot-size slot) (change-size change))
					 (- (+ (slot-start slot) (float (/ (slot-size slot) 2)))
					    (float (/ (change-size change) 2)))
					 (if (> (slot-size-other slot) (change-size change))
					       (- (+ (slot-start-other slot) (float (/ (slot-size-other slot) 2))) 
						  (float (/ (change-size change) 2)))
					       0))
				     (change-size change)))
			   changes slots (butlast display-trains))))
	  (aif (or (> (length results) (length (remove-if #'(lambda (res) (zerop (second res))) results)))
		   (run-over-each-other-p results start-pos end-pos))
	       	(if (> (name->pixels "click for details") (- end-pos start-pos))
		    '()
		    (list (list "click for details" (float (/ end-pos 2)) 0)))
	       results)))))

(defun run-over-each-other-p (results start-pos end-pos)
  (or (not (> (second (first results)) start-pos))
      (changes-run-over-each-other-p results)
      (> (+ (second (car (last results))) (third (car (last results))))
	 end-pos)))

(defun changes-run-over-each-other-p (results)
  (if (not (cdr results))
      nil
      (if (> (+ (second (first results)) (third (first results)) 5)
	     (second (second results)))
	  t
	  (changes-run-over-each-other-p (rest results)))))

(defun change-slots (trains)
  (map-subsequent-two #'(lambda (t1 t2)
			  (list 
			   (list 
			    :start (+ (third t1) (fifth t1))
			    :end (fifth t2)
			    :other-start (fifth t1)
			    :other-end (+ (third t2) (fifth t2))
			    :size (- (fifth t2) (+ (third t1) (fifth t1)))
			    :other-size (- (+ (third t2) (fifth t2)) (fifth t1)))))
		      trains #'append))

(defun bar-image (class &key (type 'main))
  (casequal (string-downcase class)
    ("tube" (case type
	      (stop "/images/bluebarstop.png")
	      (main "/images/brownbar.png")
	      (left "/images/leftbrownbar.png")
	      (right "/images/rightbrownbar.png")))
    ("walk" (case type
	      (stop "/images/bluebarstop.png")
	      (main "/images/bluebar.png")
	      (left "/images/leftbluebar.png")
	      (right "/images/rightbluebar.png")))
    ("bus" (case type
	     (stop "/images/bluebarstop.png")
	     (main "/images/bluebar.png")
	     (left "/images/leftbluebar.png")
	     (right "/images/rightbluebar.png")))
    (t (case type
	 (stop "/images/bluebarstop.png")
	 (main "/images/bluebar.png")
	 (left "/images/leftbluebar.png")
	 (right "/images/rightbluebar.png")))))

(defun graph-trains-pos (display-trains deptime durlen)
  (let ((trains (display-journey-summary display-trains)))
    (append (list (list :start 0 :size (name->pixels (short-name (first (first trains))))))
	    (mapcar #'(lambda (display-train) 
			(let* ((train (display-train-summary display-train))
			       (leftlen (round (timedifference-nd (second train) deptime))))
			  (list 'train
				:width (- (round (timedifference (sixth train) "00:00"))
					  (if (= leftlen 0) 10 0))
				:left (- (+ 55 leftlen) (if (= leftlen 0) 0 10)))))
		    display-trains)
	    (let ((i 0)
		  (prev (name->pixels (short-name (first (first trains))))))
	      (MAP-SUBSEQUENT-TWO #'(lambda (t1 t2) 
				      (let* ((short-name (short-name (third t1)))
					     (left (change-leftpos t1 t2 deptime short-name prev)))
					(incf i)
					(setf prev (+ left (name->pixels short-name)))
					(list (list 'change
						    :size (name->pixels short-name)
						    :changeat left))))
				  trains #'append))
	    (list (list :end (- (+ durlen 50) 5) :size (name->pixels (short-name (third (first trains)))))))))


