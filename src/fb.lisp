;; -*- lisp -*-

(in-package :com.sondesh.database.facebook)

;;;; * YACLML tags mapping to FBML tags.

;; TODO this file badly needs factoring, see def-html-tag for examples to factor out attribute groups

;; ** Helper macros and function for defining the tag macros.

(defmacro def-fb-tag (name &rest attributes)
  (let ((fbname name))
  (case name
    ('error (setf name 'fb-error))
    ('message (setf name 'fb-message))
    ('random (setf name 'fb-random))
    ('time (setf name 'fb-time))
    ('if (setf name 'fb-if)))
    (let ((effective-attributes attributes)
	  (tag-name (if (stringp name)
			name
			(string-downcase (symbol-name name)))))
      (labels ((gen-attr-var-name (attr)
		 (if (stringp attr)
		     (intern (string-upcase (hyphenize attr)))
		     attr))
	       (hyphenize (str)
		 (coerce (iter (for c :in-vector str)
			       (if (upper-case-p c)
				   (progn
				     (collect #\-)
				     (collect (char-downcase c)))
				   (collect c)))
			 'string)))
	(let ((tag-symbol (intern (string-upcase (hyphenize tag-name))))
	      (fbtag-name (concatenate 'string "fb:" (string-downcase (symbol-name fbname)))))
	  `(progn
	     (export ',tag-symbol)
	     (yaclml:deftag ,tag-symbol
		 (&attribute ,@(mapcar #'gen-attr-var-name effective-attributes)
			     &body body)
	       (yaclml:emit-open-tag ,fbtag-name
				     (list ,@(mapcar (lambda (attr)
						       `(cons 
							 ,(if (stringp attr)
							      attr
							      (string-downcase (symbol-name attr)))
							 ,(gen-attr-var-name attr)))
						     effective-attributes)))
	       (yaclml:emit-body body)
	       (yaclml:emit-close-tag ,fbtag-name))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun concat-symbol (&rest args)
   "Concatenate symbols or strings to form an interned symbol"
   (intern (format nil "~{~a~}" args))))

(def-fb-tag google-analytics uacct page)

(def-fb-tag name
    uid firstnameonly linked lastnameonly possessive reflexive shownetwork
    useyou ifcantsee capitalize subjectid)

(def-fb-tag iframe src smartsize frameborder scrolling style width height)

(def-fb-tag user uid)

(def-fb-tag grouplink  groupid)

(def-fb-tag pronoun uid useyou possessive reflexive objective usethey capitalize)

(def-fb-tag profile-pic uid size linked)

(def-fb-tag is-in-network network uid)

(def-fb-tag if-can-see uid what)

(def-fb-tag if-can-see-photo pid uid)

(def-fb-tag if-is-app-user uid)

(def-fb-tag if-is-friends-with-viewer uid)

(def-fb-tag if-is-group-member gid uid role)

(def-fb-tag if-is-user uid)

(def-fb-tag if-is-own-profile)

(def-fb-tag else)    

(def-fb-tag if-user-has-added-app uid)

(def-fb-tag wide)
(def-fb-tag narrow)
(def-fb-tag profile-action url)

(def-fb-tag user-table cols)
(def-fb-tag user-item uid)

(def-fb-tag subtitle seeallurl) ;;this must be a canvas url
(def-fb-tag action href title) ;;renders a link
(def-fb-tag title)

(def-fb-tag visible-to-owner)
(def-fb-tag visible-to-user uid)
(def-fb-tag visible-to-app-users)
(def-fb-tag visible-to-added-app-users)


(def-fb-tag comments xid canpost candelete numposts callbackurl returnurl showform)
(def-fb-tag friend-selector uid name idname include_me exclude_ids) ;;to pick a friends name
(def-fb-tag multi-friend-selector actiontext showborder rows max exclude_ids bypass)
(def-fb-tag multi-friend-input width border_color include_me max exclude_ids)

(def-fb-tag mobile) ;;content for mobile-facebook

(def-fb-tag switch) ;;just like a switch
(def-fb-tag default)

(def-fb-tag user-agent includes excludes) ;;browser

(def-fb-tag submit form_id)

(def-fb-tag js-string var)

(def-fb-tag fbml version) ;;defines a block of fbml

(def-fb-tag fbmlversion)

(def-fb-tag redirect url)
(def-fb-tag ref url handle) ;;gets fbml from the url and renders it

(def-fb-tag share-button class href) ;; can contain meta and link tags, lets people share resource
;;<meta name="medium" content="blog"/>
;;<meta name="title" content="Leonidas in All of Us"/>
;;<meta name="video_type" content="application/x-shockwave-flash"/>
;;<meta name="video_height" content="345"/>
;;<meta name="video_width" content="473"/>
;;<meta name="description" content="That's the lesson 300 teaches us."/>
;;<link rel="image_src" href="http://9.content.collegehumor.com/d1/ch6/f/6/collegehumor.b38e36094735a0.jpg"/>
;;<link rel="video_src" href="http://www.collegehumor.com/moogaloop/moogaloop.swf?clip_id=1757757&autoplay=true"/>
;;<link rel="target_url" href="http://www.collegehumor.com/video:1757757"/>

(def-fb-tag attachement-preview)

(def-fb-tag notif-page) ;content to be shown for a notifications on the user's notification page
(def-fb-tag notif-email) ;content to be put in the email body
(def-fb-tag notif-subject) ;the subject to be put into the email subject from notifications.send

(def-fb-tag req-choice url label) ;a button to be shown at the bottom of a request after notifications.sendRequest

(def-fb-tag request-form type content invite action method) ;;send request to selected users

(def-fb-tag explanation message decoration)
(def-fb-tag success message decoration)

(def-fb-tag dialog id cancel_button)
(def-fb-tag dialog-title)
(def-fb-tag dialog-content)
(def-fb-tag dialog-button type value close_dialog href form_id clickrewriteurl clickrewriteid clickrewriteform)

(def-fb-tag wall)
(def-fb-tag wallpost uid)
(def-fb-tag wallpost-action href)

(def-fb-tag dashboard) ;can contain action
(def-fb-tag create-button href title)
(def-fb-tag help href title)

(def-fb-tag header icon decoration) ;;standard title header
(def-fb-tag mediaheader uid)
(def-fb-tag header-title)
(def-fb-tag owner-action href) ;; should have a title for the link

(def-fb-tag editor action width labelwidth)
(def-fb-tag editor-text label name value maxlength id)
(def-fb-tag editor-textarea label name rows)
(def-fb-tag editor-time label name value)
(def-fb-tag editor-month name value)
(def-fb-tag editor-divider)
(def-fb-tag editor-buttonset)
(def-fb-tag editor-button value name)
(def-fb-tag editor-cancel)
(def-fb-tag editor-custom label id) ;; can hold the other stuff like select, etc.

;;some attributes clicktoshow clicktohide clicktotoggle clickthrough
(def-fb-tag error message decoration)
(def-fb-tag message message)
(def-fb-tag if if value)
(def-fb-tag random)
(def-fb-tag random-option weight)

(def-fb-tag tabs)
(def-fb-tag tab-item href title align selected)
