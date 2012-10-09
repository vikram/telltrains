(in-package :com.sondesh.database)

(defun sitemapindex ()
  (with-open-file (out "/home/vb/sondesh/www/sitemapindex.xml" 
		       :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-line "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" out)
    (write-line "<sitemapindex xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/siteindex.xsd\" xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">" out)
    (write-line "<sitemap>" out)
    (write-line "<loc>http://www.telltrains.com/sitemap/sitemap.xml</loc>" out)
    (write-line "<lastmod>2007-10-22</lastmod>" out)
    (write-line "</sitemap>" out)
    (dolist (crs (sort (copy-list *all-crses*) #'> :key (compose #'station-rank #'find-station-by-code)))
      (write-line "<sitemap>" out)
      (write-line (format nil "<loc>http://www.telltrains.com/sitemap/sitemap~A.xml</loc>" 
			  crs) out)
      (write-line "<lastmod>2007-10-22</lastmod>" out)
      (write-line "</sitemap>" out)
      (sitemap crs
	       (remove-if #'(lambda (x)
			      (member crs (cons (station-group-crs-code (find-station-by-code x))
						(station-all-crs-codes (find-station-by-code x)))
				      :test #'string=))
			  *all-crses*)))
    (write-line "</sitemapindex>" out)))

(defun sitemap (crs lst)
  (with-open-file (out (format nil "/home/vb/sondesh/www/sitemap/sitemap~A.xml" crs)
		       :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-line "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" out)
    (write-line "<urlset xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd\" xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">" out)
    (dolist (to (sort (copy-list lst)
		      #'(lambda (crs1 crs2)
			  (> (* (+ (station-rank (find-station-by-code crs)) (station-rank (find-station-by-code crs1))) (direct-trains-count crs crs1))
			     (* (+ (station-rank (find-station-by-code crs)) (station-rank (find-station-by-code crs2))) (direct-trains-count crs crs2))))))
      (awhen (* (round (+ (* 80 (station-rank (find-station-by-code crs)) 
			     (station-rank (find-station-by-code to))) 0.6) 0.01) 0.01)
	  (when (> it 1)
	    (print (list crs to it)))
	  (write-line "<url>" out)
	  (write-line 
	   (format nil "<loc>~A</loc>" 
		   (format nil "http://www.telltrains.com/timetable/~A/~A/today"
			   (station-name-for-url crs)
			   (station-name-for-url to)))
	   out)
	  (write-line "<lastmod>2007-10-22</lastmod>" out)
	  (write-line "<changefreq>daily</changefreq>" out)
	  (write-line (format nil "<priority>~A</priority>" 
			      (if (> 0.5 it)
				  0.5
				  (if (> it 1) 1 it)))
		      out)
	  (write-line "</url>" out)))
    (write-line "</urlset>" out)))

;;	  (timeline-from-to crs to "15-10-2007")
#|
(defun timeline-from-to (from to leaving)
  (with-open-file (out (format nil "/home/vb/sondesh/www/sitemap/html/~A-~A-~A.html" from to leaving)
		       :direction :output :if-exists :supersede :if-does-not-exist :create)
    (multiple-value-bind (str code)
	(drakma:http-request 
	 (escape-url (format nil "http://192.168.0.6/timetable/~A/~A/~A"
			     (station-display-name (find-station-by-code from))
			     (station-display-name (find-station-by-code to))
			     leaving)))
      (write str :stream out :escape nil))))
|#

(defun station-name-for-url (crs)
  (casequal crs
    ("AHV" "ASH-VALE")
    ("BTH" "BATH-SPA")
    ("G18" "LIVERPOOL-ANY-STATION")
    ("G32" "WEST-HAMPSTEAD")
    (t (first (split-sequence:split-sequence #\space (station-display-name (find-station-by-code crs)))))))

(defun sitemap-popular ()
  (let ((lst (sort
		(mappend #'(lambda (combo)
			     (mapcar #'(lambda (crs) (cons (car combo) crs)) (cadr combo)))
			 (combinations *all-crses*))
		#'(lambda (crs1 crs2)
		    (< (+ (station-rank (find-station-by-code (car crs1))) (station-rank (find-station-by-code (cdr crs1))))
		       (+ (station-rank (find-station-by-code (car crs2))) (station-rank (find-station-by-code (cdr crs2)))))))
	  ))
  (with-open-file (out "/home/vb/sondesh/www/sitemap.xml"
		       :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-line "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" out)
    (write-line "<urlset xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd\" xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">" out)
    (write-line "<url>" out)
    (write-line "<loc>http://www.telltrains.com/stations</loc>" out)
    (write-line "<changefreq>monthly</changefreq>" out)
    (write-line "<priority>0.85</priority>" out)
    (write-line "<lastmod>2007-11-22</lastmod>" out)
    (write-line "</url>" out)
    (dolist (crs (sort (copy-list *all-crses*)
		       #'(lambda (crs1 crs2)
			   (> (station-rank (find-station-by-code crs1))
			      (station-rank (find-station-by-code crs2))))))
      (write-line "<url>" out)
      (write-line (format nil "<loc>http://www.telltrains.com/journeysfrom/~A</loc>" (station-name-for-url crs)) out)
      (write-line "<changefreq>weekly</changefreq>" out)
      (write-line (format nil "<priority>~A</priority>" (if (> (station-rank (find-station-by-code crs)) 0.003)
							    1.00
							    (/ (station-rank (find-station-by-code crs)) 0.003)))
		  out)
      (write-line "<lastmod>2007-11-22</lastmod>" out)
      (write-line "</url>" out))      
    (dolist (crses lst) 
      (write-line "<url>" out)
      (write-line 
       (format nil "<loc>~A</loc>" 
	       (format nil "http://www.telltrains.com/timetable/~A/~A/today"
		       (station-name-for-url (car crses))
		       (station-name-for-url (cdr crses))))
       out)
      (write-line "<lastmod>2007-11-22</lastmod>" out)
      (write-line "<changefreq>weekly</changefreq>" out)
      (write-line (format nil "<priority>~A</priority>" 
			  (/ (+ (station-rank (find-station-by-code (car crses)))
				(station-rank (find-station-by-code (cdr crses))))
			     0.25))
		  out)
      (write-line "</url>" out))
  (write-line "</urlset>" out))))


(defun simple-sitemap ()
  (let ((lst (topn (sort
		(mappend #'(lambda (combo)
			     (mapcar #'(lambda (crs) (cons (car combo) crs)) (cadr combo)))
			 (combinations *all-crses*))
		#'(lambda (crs1 crs2)
		    (> (+ (station-rank (find-station-by-code (car crs1))) (station-rank (find-station-by-code (cdr crs1))))
		       (+ (station-rank (find-station-by-code (car crs2))) (station-rank (find-station-by-code (cdr crs2)))))))
		   2500)))
  (with-open-file (out "/home/vb/sondesh/www/sitemap.xml"
		       :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-line "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" out)
    (write-line "<urlset xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd\" xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">" out)
    (write-line "<url>" out)
    (write-line "<loc>http://www.telltrains.com/stations</loc>" out)
    (write-line "<changefreq>monthly</changefreq>" out)
    (write-line "<priority>0.85</priority>" out)
    (write-line "<lastmod>2007-11-22</lastmod>" out)
    (write-line "</url>" out)
    (dolist (crses lst) 
      (write-line "<url>" out)
      (write-line 
       (format nil "<loc>~A</loc>" 
	       (format nil "http://www.telltrains.com/timetable/~A/~A/today"
		       (station-name-for-url (car crses))
		       (station-name-for-url (cdr crses))))
       out)
      (write-line "<lastmod>2007-11-22</lastmod>" out)
      (write-line "<changefreq>weekly</changefreq>" out)
      (write-line (format nil "<priority>~A</priority>" 
			  (/ (+ (station-rank (find-station-by-code (car crses)))
				(station-rank (find-station-by-code (cdr crses))))
			     0.25))
		  out)
      (write-line "</url>" out))
  (write-line "</urlset>" out))))


