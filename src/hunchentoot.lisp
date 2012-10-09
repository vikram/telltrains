(in-package :com.sondesh.database)

(setf *default-log-level* :warning)

(defvar *utf-8* (flex:make-external-format :utf-8 :eol-style :lf))

(setf *hunchentoot-default-external-format* *utf-8*)

(setq *dispatch-table*
      (nconc
       (mapcar (lambda (args)
		 (apply #'create-prefix-dispatcher args))
	       '(("/stations" station-index)
		 ("/journeysfrom" station-journeys)
		 ("/journeystops" journey-stops)
		 ("/suggest-stations" suggest-stations)
		 ("/timetable" timeline)
		 ("/timeline" timeline)
		 ("/traintimes" traintimes)
		 ("/terms" terms)
		 ("/about" aboutus)
		 ("/contact" contact)
		 ("/addjourney" addjourneys)
		 ("/trainjourneys" trainjourneys)
		 ("/status" diagnostics)
		 ("/" home)
		 ))
       (list #'default-dispatcher)))

(defun upgrade (&key (reader nil) (stations nil) (port 2001))
  (let ((load-compile #'(lambda (file) (load (compile-file (format nil "/home/vb/sondesh/src/~A.lisp" file))))))
    (funcall load-compile "packages")
    (funcall load-compile "date")
    (funcall load-compile "utilities")
    (funcall load-compile "fibonacci")
    (funcall load-compile "importer")
    (when stations (funcall load-compile "stations"))
    (funcall load-compile "lookups")
    (funcall load-compile "db-defn")
    (when reader (funcall load-compile "train-reader"))
    (funcall load-compile "trains")
    (funcall load-compile "train-astar")
    (funcall load-compile "algorithms")
    (funcall load-compile "fares")
    (funcall load-compile "streams")
    (funcall load-compile "tests")
    (funcall load-compile "hunchen-utils")
    (funcall load-compile "ads")
    (funcall load-compile "changes")
    (funcall load-compile "journeystops")
    (funcall load-compile "station-urls")
    (funcall load-compile "homepage")
    (funcall load-compile "timeline")
    (funcall load-compile "addjourneys")
    (funcall load-compile "terms")
    (funcall load-compile "timeline-entrypoints")
    (funcall load-compile "hunchentoot")
    (hunchentoot:stop-server *server*)
    (setf *server* (start-server :port port))))

#|

		 ("/feedback" http://www.telltrains.com/forums/post.php?fid=2


       (mapcar (lambda (args)
		 (apply #'create-regex-dispatcher args)
		 '(("^[a-z A-Z0-9%\\-#]+" timeline)))))) ;from to

		 ("/fbinstall" fbinstall) ;installed auth_token
		 ("/fbremove" fbremove) ;fb_sig_uninstall fb_sig_time fb_sig_added fb_sig_user fb_sig
		 ("/setuphome" mycommute-setup) ;fb_sig_user fb_sig_session_key :home t
		 ("/setupwork" mycommute-setup) ;fb_sig_user fb_sig_session_key :home nil
		 ("/fb" fbin) ;fb_sig_user fb_sig_session_key
		 ("/fbmycommute-return" fbin-return) ;fb_sig_session_key fb_sig_user
		 ("/fbmycommute-live" fbin-live) ;fb_sig_session_key fb_sig_user
		 ("/fbmycommute-setup" fbin-setup) ;fb_sig_session_key fb_sig_user
		 ("/fbmycommute-setuphome" fbin-setup) ;fb_sig_session_key fb_sig_user
		 ("/fbmycommute-setupwork" fbin-setup) ;fb_sig_session_key fb_sig_user
		 ("/fbmycommute-friends" invite-friends) ;fb_sig_session_key fb_sig_user
		 ("/fbmycommute-invite" invite-friend) ;fb_sig_session_key fb_sig_user
		 ("/fbmycommute-feedback" givefeedback)))
|#


