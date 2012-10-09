(in-package :com.sondesh.database)

(defun fbinstall ()
  (let ((installed (get-parameter "installed"))
	(auth_token (get-parameter "auth_token")))
    (with-html-output (*standard-output*)
      (:p "Thanks for installing myCommute"))))

(defun fbremove ()
  (let ((uninstalled (get-parameter "fb_sig_uninstall"))
	(sig_time (get-parameter "fb_sig_time"))
	(uid (get-parameter "fb_sig_user"))
	(sig (get-parameter "fb_sig")))
    (with-html-output (*standard-output*)
      (:p (:fb-name :uid uid)
	  " thanks for using myCommute."))))

(defmacro with-fb (urlname &body body)
  `(let* ((uid (or (get-parameter "fb_sig_user") ""))
	  (url ,urlname)
	  (session-key (or (get-parameter "fb_sig_session_key") "")))
     (with-html-output-to-string (*standard-output* nil :prologue nil)
       (:fb-fbml
	(:fb-google-analytics :uacct "UA-1779260-1" :page (format nil "~AmyCommute?uid=~A" url uid))
	(:fb-dashboard 
	 (:fb-action :href "live" "Trains")
	 (:fb-action :href "return" "Return journey")
	 (:fb-action :href "setup" "Setup commute")
	 (:fb-action :href "friends" "Invite Friends")
	 (:fb-action :href "feedback" "Report problems"))
	(:div :style "margin: 0pt auto; position: relative; top: -35px; left: 145px; width: 100%;"
	      (:h3 ": Setup your commute " (:span :style "color: #BF290B;" " brought to you by Telltrains")))
	,@body))))

(defun fbin-basic (urlname)
  (with-fb urlname
    (:fb-iframe :src (format nil "http://~A/~A" 
			     (if *prod* "www.telltrains.com" "82.153.32.254")
			     url)
		:width "100%"
		:height "600px;"
		:style "position: relative; top: -20px;"
		:scrolling "false"
		:frameborder "0")))

(defun fbin-setup ()
  (fbin-basic "setup"))

(defun fbin-return ()
  (fbin-base "return"))

(defun fbin-live ()
  (fbin-base "live"))

(defun fbin ()
  (let* ((uid (or (get-parameter "fb_sig_user") ""))
	 (session-key (or (get-parameter "fb_sig_session_key") "")))
    

(defun excluded-uids (uid session-key)
  (let ((session (facebook:make-session 
		  :api-key *fb-api-key*
		  :secret *fb-secret*
		  :session-key session-key
		  :uid uid)))
    (awhen (facebook:execute-query 
	    session
	    (format nil "SELECT uid FROM user WHERE has_added_app=1 and uid IN (SELECT uid2 FROM friend WHERE uid1 = ~A)" uid))
      (if (eql (caar it) 'ERROR_CODE)
	  nil
	  it))))

(defun invite-friends ()
  (with-fb "invite"
    (:fb-request-form :action "mycommute?src=frominvite"
		     :method "POST"
		     :invite "true"
		     :type "myCommute"
		     :content "Check out the -myCommute- application from Telltrains. It lets you view trains between work and home.<fb:req-choice url=\"http://apps.facebook.com/mycommute?a=ONINSTALL&fbsrc=inviteAddApp\" label=\"Add Application!\" /><fb:req-choice url=\"http://apps.facebook.com/apps/application.php?api_key=4c1a3787cab3d55a42609ad10355d9f3&fbsrc=inviteLearnM\" label=\"Learn More!\" />"
		     (:fb-multi-friend-selector
		      :showborder "false" 
		      :actiontext "Invite your friends to myCommute."
		      :exclude_ids (format nil "[~{~A~^, ~}]" (mapcar #'cdar (excluded-uids uid session-key)))
		      :max "20"))))

(defun setup ()
  (let* ((fb_sig_user (get-parameter "fb_sig_user"))
	 (fb_sig_session_key (get-parameter "fb_sig_session_key"))
	 (existing '())
	 (nextid (length existing)))
    (with-html-output-to-string (*standard-output* nil :prologue nil)
      (:html
       (:head
	(:title "telltrains: train times and timetables for british rail commuters")
	(:link :rel "icon" :href (image-path "transparent.ico"))
	(:link :rel "stylesheet" :href (css-path "fbform.css")))
       (:body :class "main" :id "setup" :style "text-align: left; overflow:hidden; background: #F7F7F7 none repeat scroll 0%;"
	      (:div :class "profile_note" 
		    (:strong "Note:")
		    " Add stations for home and work and pick the times.")
	      (:div :class "editor_panel clearfix" :id "content"
		    (:form :method :post
			   :id "profile_form" :name "profile_form"
			   (:table :cellspacing "0" :border "0" :class "editor basic"
				   (:tbody :id "mycommute"
				    (loop for i from 0
				       for (home . work) in existing
				       do (htm
					   (:tr :id (format nil "addhome~A" i)
						(:td :class "label" "Home station")
						(:td
						 (:input :type :text :value home
							 :name "from" :id "fromstation" :class "inputtext"
							 :autocomplete "off" :size "22" :maxlength "50")
						 (:hidden :id "fromcrs" :name "fromcrscode")))
					   (:tr :id (format nil "addwork~A" i)
						(:td :class "label" "Work station")
						(:td
						 (:input :type :text :value work
							 :name "to" :id "tostation" :class "inputtext"
							 :autocomplete "off" :size "22" :maxlength "50")
						 (:hidden :id "tocrs" :name "tocrscode")))
					   (:tr :id (format nil "link~A" i) 
						(:td :class "label") 
						(:td (:div (:a :href "#" 
							       :id (format nil "home~A" i)
							       :onClick (fmt "remove('~A'); return false;" i)
							       "Remove this journey..."))))
					   (:tr (:td) (:td (:div :class "divider")))))
				    (:tr :id "mycommutebuttons" (:td)
					 (:td 
					  (:div :class "buttons" :style "text-align: left; left: -4px; position: relative; padding: 5px 0px 0px;"
						(:input :type "submit" 
							:value "Save Changes" 
							:name "savename" 
							:id "save" 
							:class "inputsubmit")
						(:input :type "button" 
							:value "Cancel" 
							:name "cancelname" 
							:id "cancel" 
							:class "inputbutton"))))))))
	    (:script :type "text/javascript" :src *sitejs*)
	    (:script :type "text/javascript"
		     (fmt
		   "// <![CDATA[
                       var journeys = []; var numberOfJourneys = 0; var sliderValues = []; var numRoutes = 0; var xFactor = 0;
                       var originalleft = 0; var stations = ''; var pagedate = ''; var pattern = 0; var fb = true;
                       var nextjourney = ~A;

                       $(document).ready(function() {
                          $('#container').center('horizontal');
                          adjust();
                          $('#fromstation').autocomplete('suggest-stations',
                                            { delay:10, minChars:1, onItemSelect:selectFrom,
			                      onFindValue:selectFrom, matchSubset:null, cacheData:false, autoFill:false });
                          $('#tostation').autocomplete('suggest-stations',
                                            { delay:10, minChars:1, onItemSelect:selectTo, 
                                              onFindValue:selectTo, matchSubset:null, cacheData:false, autoFill:false });
                        addjourneyform();
                         }); // ]]>" 
		   nextid)))))))


("/fb" fbin) ;fb_sig_user fb_sig_session_key
("/fbmycommute-return" fbin-return) ;fb_sig_session_key fb_sig_user
("/fbmycommute-live" fbin-live) ;fb_sig_session_key fb_sig_user
("/fbmycommute-setup" fbin-setup) ;fb_sig_session_key fb_sig_user
("/fbmycommute-friends" invite-friends) ;fb_sig_session_key fb_sig_user
("/fbmycommute-invite" invite-friend) ;fb_sig_session_key fb_sig_user
("/fbmycommute-feedback" givefeedback)))

    (awhen (get-mycommute uid *fb-api-key*)
      (setf from (first it))
      (setf to (second it)))
	       (let* ((real-from (if (and user (string= user "exact"))
				       (find-station-by-exact-name from)
				       (if (and user (string= user "facebook"))
					   (find-station-by-code from)
					   (real-station-by-letters from))))
			(real-to (if (and user (string= user "exact"))
				     (find-station-by-exact-name to)
				     (if (and user (string= user "facebook"))
					 (find-station-by-code to)
					 (real-station-by-letters to)))))
		   (if (and real-from real-to (not setup))
		       (<fb:fbml
			(<fb:google-analytics :uacct "UA-1779260-1" 
					      :page (format nil "myCommute?uid=~A&date=~A&time=~A" 
							    uid leaving-date (current-hrmins now)))
			(<fb:dashboard 
			 (<fb:action :href "mycommute?src=live" (<:as-is "Trains"))
			 (<fb:action :href "mycommute?src=return" (<:as-is "Return journey"))
			 (<fb:action :href "mycommute?src=setup" (<:as-is "Setup commute"))
			 (<fb:action :href "mycommute?src=friends" (<:as-is "Invite Friends"))
			 (<fb:action :href "mycommute?src=feedback" (<:as-is "Report problems")))
			(<:div :style "margin: 0pt auto; position: relative; top: -35px; left: 145px; width: 100%;"
			       (<:h3 (<:as-is (format nil ": Trains from ~A to ~A " 
						      (if return-trip
							  (station-display-name real-to)
							  (station-display-name real-from))
						      (if return-trip
							  (station-display-name real-from)
							  (station-display-name real-to))))
				     (<:span :style "color: #BF290B;" " by Telltrains")))
			(<fb:iframe :src (concatenate 'string (if *prod* "http://www.telltrains.com" "http://82.153.32.254" )
						      (if return-trip
							  (timeline-url to
									from								       
									(station-display-name real-to)
									(station-display-name real-from) 
									leaving-date)
							  (timeline-url from 
								    to 
								    (station-display-name real-from) 
								    (station-display-name real-to)
								    leaving-date)))
				    :width "100%"
				    :height "500px;"
				    :frameborder "0"
				    :scrolling "false"
				    :style "position: relative; top: -30px;"
				    ))


    (update-session uid session-key)
