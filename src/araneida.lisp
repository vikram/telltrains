(in-package :com.sondesh.database)

(setf ucw:*inspect-components* nil)

(defvar *www-root*
  (merge-pathnames
   (make-pathname :directory '(:relative "www"))
   (asdf:component-pathname (asdf:find-system :sondesh))))

;;application

(defparameter *sondesh-application*
  (make-instance 'ucw:cookie-session-application
                 :url-prefix "/"
                 :www-roots (list *www-root*)
                 :dispatchers (list (ucw:action-dispatcher)
				    (ucw:url-dispatcher "fbmycommute"
				      (ucw:with-request-params ((fb_sig_user "") (fb_sig_session_key "") (src ""))
					  (ucw:context.request ucw:*context*)
					(casequal src 
						  ("return" (ucw:call 'fbin :session-key fb_sig_session_key :uid fb_sig_user :setup nil :return-trip t))
						  ("setup" (ucw:call 'fbin :session-key fb_sig_session_key :uid fb_sig_user :setup t :home t))
						  ("setuphome" (ucw:call 'fbin :session-key fb_sig_session_key :uid fb_sig_user :setup t :home t))
						  ("setupwork" (ucw:call 'fbin :session-key fb_sig_session_key :uid fb_sig_user :setup t :home nil))
						  ("live" (ucw:call 'fbin :session-key fb_sig_session_key :uid fb_sig_user :setup nil :home t))
						  ("friends" (ucw:call 'invite-friends :uid fb_sig_user :session-key fb_sig_session_key))
						  ("invite" (ucw:call 'invite-friend :uid fb_sig_user :session-key fb_sig_session_key))
						  ("feedback" (ucw:call 'givefeedback)))))
				    (ucw:url-dispatcher "setuphome"
				      (ucw:with-request-params ((fb_sig_user "") (fb_sig_session_key ""))
					  (ucw:context.request ucw:*context*)
					(ucw:call 'mycommute-setup :session-key fb_sig_session_key :uid fb_sig_user :home t)))
				    (ucw:url-dispatcher "setupwork"
				      (ucw:with-request-params ((fb_sig_user "") (fb_sig_session_key ""))
					  (ucw:context.request ucw:*context*)
					(ucw:call 'mycommute-setup :session-key fb_sig_session_key :uid fb_sig_user :home nil)))
				    (ucw:url-dispatcher "fb"
				      (ucw:with-request-params ((fb_sig_user "") (fb_sig_session_key ""))
					  (ucw:context.request ucw:*context*)
					(ucw:call 'fbin :session-key fb_sig_session_key :uid fb_sig_user)))
				    (ucw:url-dispatcher ""
				      (ucw:with-request-params ((fb_sig_user "") (fb_sig_session_key ""))
					  (ucw:context.request ucw:*context*)
					(ucw:call 'timeline :session-key fb_sig_session_key :uid fb_sig_user)))
				    (ucw:url-dispatcher "addjourney"
				      (ucw:call 'timeline :user "results"))
				    (ucw:regexp-dispatcher "^(suggest-stations.ucw|)$"
				      (ucw:with-request-params ((input ""))
					  (ucw:context.request ucw:*context*)
					(ucw:call 'suggest-stations :input input)))
				    (ucw:regexp-dispatcher "^(journeystops|)$"
				      (ucw:with-request-params ((journey ""))
					  (ucw:context.request ucw:*context*)
					(ucw:call 'journey-stops :journey journey)))
				    (ucw:regexp-dispatcher "^(postcode|)$"
				      (ucw:with-request-params ((input "") (from "") (to "") (q ""))
					  (ucw:context.request ucw:*context*)
					(ucw:call 'postcode-html :input (cond ((string/= "" input) input)
									 ((string/= "" from) from)
									 ((string/= "" to) to)
									 ((string/= "" q) q)
									 (t "")))))
				    (ucw:regexp-dispatcher "^(suggest-stations|)$"
				      (ucw:with-request-params ((input "") (from "") (to "") (q ""))
					  (ucw:context.request ucw:*context*)
					(ucw:call 'suggest-stations-html :input (cond ((string/= "" input) input)
										      ((string/= "" from) from)
										      ((string/= "" to) to)
										      ((string/= "" q) q)
										      (t "")))))
				    (ucw:url-dispatcher "stations"
				      (ucw:call 'station-index))
				    (ucw:regexp-dispatcher "^journeysfrom"
				      (let* ((uri (ucw::raw-uri (ucw:context.request ucw:*context*)))
					     (uri-broken-up (cl-ppcre:all-matches-as-strings "\\b[a-z A-Z0-9%-]+" uri))
					     (l (length uri-broken-up))
					     (name (cl-ppcre:regex-replace-all "%20" (nth 1 uri-broken-up) " ")))
					(when name
					  (ucw:call 'station-journeys :from name))))
				    (ucw:regexp-dispatcher "^trainjourneys"
				      (if (not (ucw::parameters (ucw:context.request ucw:*context*)))
					  (let* ((uri (ucw::raw-uri (ucw:context.request ucw:*context*)))
						 (uri-broken-up (cl-ppcre:all-matches-as-strings "\\b[a-z A-Z0-9%-]+" uri))
						 (l (length uri-broken-up))
						 (leaving (cl-ppcre:regex-replace-all "%20" (nth 1 uri-broken-up) " "))
						 (arriving (cl-ppcre:regex-replace-all "%20" (nth 2 uri-broken-up) " "))
						 (date (nth 3 uri-broken-up)))
					    (when (and leaving arriving)
					      (ucw:call 'trainjourneys :from leaving :to arriving
							:leaving date :leaving-time "")))
					  (ucw:with-request-params ((from "") (to "") (leaving ""))
					      (ucw:context.request ucw:*context*)
					    (ucw:call 'trainjourneys :from from :to to 
						      :leaving leaving :leaving-time ""))))
				    (ucw:regexp-dispatcher "^cached"
				      (if (not (ucw::parameters (ucw:context.request ucw:*context*)))
					  (let* ((uri (ucw::raw-uri (ucw:context.request ucw:*context*)))
						 (uri-broken-up (cl-ppcre:all-matches-as-strings "\\b[a-z A-Z0-9%\\-#]+" uri))
						 (l (length uri-broken-up))
						 (leaving (cl-ppcre:regex-replace-all "%20" (nth 1 uri-broken-up) " "))
						 (arriving (cl-ppcre:regex-replace-all "%20" (nth 2 uri-broken-up) " "))
						 (date (nth 3 uri-broken-up)))
					    (when (and leaving arriving)
					      (ucw:call 'cached :from-crs leaving :to-crs arriving
							:leaving date)))
					  (ucw:with-request-params ((from "") (to "") (leaving ""))
					      (ucw:context.request ucw:*context*)
					    (ucw:call 'cached :from-crs from :to-crs to 
						      :leaving leaving))))
				    (ucw:regexp-dispatcher "^timetable"
				      (let* ((uri (ucw::raw-uri (ucw:context.request ucw:*context*)))
					     (uri-broken-up (cl-ppcre:all-matches-as-strings "\\b[a-z A-Z0-9%\\-#]+" uri))
					     (l (length uri-broken-up))
					     (leaving (if (> l 1) (cl-ppcre:regex-replace-all "%20" (nth 1 uri-broken-up) " ") nil))
					     (arriving (if (> l 2) (cl-ppcre:regex-replace-all "%20" (nth 2 uri-broken-up) " ") nil))
					     (date (if (> l 3) (nth 3 uri-broken-up) nil)))
					(if (and leaving arriving)
					    (ucw:call 'timeline :from leaving :to arriving
						      :leaving date :leaving-time "")
					    (when (not (ucw::parameters (ucw:context.request ucw:*context*)))
					      (ucw:with-request-params ((from "") (to "") (leaving ""))
						  (ucw:context.request ucw:*context*)
						(ucw:call 'timeline :from from :to to 
							  :leaving leaving :leaving-time ""))))))
				    (ucw:regexp-dispatcher "^traintimes"
				      (let* ((uri (ucw::raw-uri (ucw:context.request ucw:*context*)))
					     (uri-broken-up (cl-ppcre:all-matches-as-strings "\\b[a-z A-Z0-9%\\-#]+" uri))
					     (l (length uri-broken-up))
					     (leaving (if (> l 1) (cl-ppcre:regex-replace-all "%20" (nth 1 uri-broken-up) " ") nil))
					     (arriving (if (> l 2) (cl-ppcre:regex-replace-all "%20" (nth 2 uri-broken-up) " ") nil))
					     (date (if (> l 3) (nth 3 uri-broken-up) nil)))
					(if (and leaving arriving)
					    (ucw:with-request-params ((fb_sig_user "")
								      (fb_sig_session_key ""))
						(ucw:context.request ucw:*context*)
					      (ucw:call 'timeline :from leaving :to arriving
							:leaving date :leaving-time "" :user "exact" :uid fb_sig_user :session-key fb_sig_session_key))
					    (when (not (ucw::parameters (ucw:context.request ucw:*context*)))
					      (ucw:with-request-params ((from "") (to "") (leaving "") (fb_sig_user "") (fb_sig_session_key ""))
						  (ucw:context.request ucw:*context*)
						(ucw:call 'timeline :from from :to to 
							  :leaving leaving :leaving-time "" :user "exact" :uid fb_sig_user :session-key fb_sig_session_key))))))
				    (ucw:regexp-dispatcher "^givefeedback"
				      (if (not (ucw::parameters (ucw:context.request ucw:*context*)))
					  (let* ((uri (ucw::raw-uri (ucw:context.request ucw:*context*)))
						 (uri-broken-up (cl-ppcre:all-matches-as-strings "\\b[a-z A-Z0-9%\\-#]+" uri))
						 (l (length uri-broken-up))
						 (leaving (if (> l 1) (cl-ppcre:regex-replace-all "%20" (nth 1 uri-broken-up) " ") nil))
						 (arriving (if (> l 2) (cl-ppcre:regex-replace-all "%20" (nth 2 uri-broken-up) " ") nil))
						 (date (if (> l 3) (nth 3 uri-broken-up) nil)))
					    (ucw:call 'givefeedback :from leaving :to arriving
						      :leaving date :leaving-time ""))
					  (ucw:with-request-params ((from "") (to "") (leaving ""))
					      (ucw:context.request ucw:*context*)
					    (ucw:call 'givefeedback :from from :to to 
						      :leaving leaving :leaving-time ""))))
				    (ucw:regexp-dispatcher "^errorfeedback"
				      (if (not (ucw::parameters (ucw:context.request ucw:*context*)))
					  (let* ((uri (ucw::raw-uri (ucw:context.request ucw:*context*)))
						 (uri-broken-up (cl-ppcre:all-matches-as-strings "\\b[a-z A-Z0-9%\\-#]+" uri))
						 (l (length uri-broken-up))
						 (leaving (if (> l 1) (cl-ppcre:regex-replace-all "%20" (nth 1 uri-broken-up) " ") nil))
						 (arriving (if (> l 2) (cl-ppcre:regex-replace-all "%20" (nth 2 uri-broken-up) " ") nil))
						 (date (if (> l 3) (nth 3 uri-broken-up) nil)))
					    (ucw:call 'errorfeedback :from leaving :to arriving
						      :leaving date :leaving-time ""))
					  (ucw:with-request-params ((from "") (to "") (leaving ""))
					      (ucw:context.request ucw:*context*)
					    (ucw:call 'errorfeedback :from from :to to 
						      :leaving leaving :leaving-time ""))))
				    (ucw:regexp-dispatcher "^fbinstall"	
				      (ucw:with-request-params ((auth_token "") (installed ""))
					  (ucw:context.request ucw:*context*)
					(ucw:call 'fbinstall :installed installed :auth_token auth_token)))
				    (ucw:regexp-dispatcher "^fbremove"
				      (ucw:with-request-params ((fb_sig_uninstall "")
								(fb_sig_time "")
								(fb_sig_added "")
								(fb_sig_user "")
								(fb_sig_api_key "")
								(fb_sig ""))
					  (ucw:context.request ucw:*context*)
					(ucw:call 'fbremove :removed fb_sig_uninstall :uid fb_sig_user)))
				    (ucw:regexp-dispatcher "^[a-z A-Z0-9%\\-#]+"
				      (if (not (ucw::parameters (ucw:context.request ucw:*context*)))
					    (let* ((uri (ucw::raw-uri (ucw:context.request ucw:*context*)))
						   (uri-broken-up (cl-ppcre:all-matches-as-strings "\\b[a-z A-Z0-9%\\-#]+" uri))
						   (leaving (cl-ppcre:regex-replace-all "%20" (nth 0 uri-broken-up) " "))
						   (arriving (cl-ppcre:regex-replace-all "%20" (nth 1 uri-broken-up) " ")))
					      (when (and leaving arriving)
						(ucw:call 'timeline :from leaving :to arriving)))
					    (ucw:call 'timeline))))
                 :debug-on-error nil))

(defun googlebot-p (request)
  (string= "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)" 
	   (cdr (find "User-Agent" (ucw::headers request) :test #'string= :key #'car))))

(defparameter *log-folder* (if (directory "/home/vb/logs")
			       "/home/vb/logs/"
			       "/Users/vb/logs/"))

(defun start-web-server ()
  (progn
    (ucw:create-server :backend '(:httpd :host "127.0.0.1" :port 2001)
		       :log-root-directory (make-pathname :name nil :type nil
							  :directory *log-folder*)
		       :log-level (if *prod* 
				      ucw::+info+
				      ucw::+debug+)
		       :start-p t)
    (ucw:register-application ucw:*default-server* *sondesh-application*)))

(if (and ucw:*default-server* (ucw::server.started ucw:*default-server*))
    (progn
      (ucw:shutdown-server ucw:*default-server*)
      (setf ucw:*default-server* nil)
      (start-web-server))
    (start-web-server))
