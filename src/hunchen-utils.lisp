(in-package :com.sondesh.database)

(defparameter *show-changes* t)
(defparameter *fb-api-key*  "4c1a3787cab3d55a42609ad10355d9f3")
(defparameter *fb-secret* "4cec5eb627fb1213e817a0e7d276fabe")
(defparameter *prod* t)
(defparameter *debug* nil)
(defparameter *unpacked* nil)
(defparameter *image-path* "http://www.telltrains.com/images/")
(defparameter *js-path* "http://www.telltrains.com/javascripts/")
(defparameter *css-path* "http://www.telltrains.com/stylesheets/")
(defparameter *sitejs* "/javascripts/10Dec0933.min.js")
(defparameter *sitecss* "/stylesheets/09Nov1251.css")
(defparameter *analytics* "UA-2993192-1")

(defun urchin ()
  (with-html-output (*standard-output*)
	  (:script :src "http://www.google-analytics.com/urchin.js" :type "text/javascript")
	  (:script :type "text/javascript"
		   "_uacct = 'UA-1779260-1';
                    urchinTracker();")))

(defun statscounter ()
  (with-html-output (*standard-output*)
    (:script :type "text/javascript"
	     "var sc_project=3244583;
              var sc_invisible=0;
              var sc_partition=35;
              var sc_security='be61a1e7';")
    (:script :type "text/javascript" 
	     :src "http://www.statcounter.com/counter/counter_xhtml.js")
    (:noscript
     (:div :class "statcounter"
	   (:a :class "statcounter" :href " http://www.statcounter.com/"
	       (:img :class "statcounter" :src "http://c36.statcounter.com/3244583/0/be61a1e7/0/ " :alt "website metrics"))))))

(defun image-path (image-name)
  (if *prod*
      (concatenate 'string *image-path* image-name)
      (concatenate 'string "http://82.153.32.254/images/" image-name)))

(defun css-path (css-name)
  (if *prod*
      (concatenate 'string *css-path* css-name)
      (concatenate 'string "http://82.153.32.254/stylesheets/" css-name)))

(defun js-path (js-name)
  (if *prod*
      (concatenate 'string *js-path* js-name)
      (concatenate 'string "http://82.153.32.254/javascripts/" js-name)))

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     ,@body))

(defun break-as-parameters (uri)
  (rest (all-url-parts uri)))

(defun all-url-parts (uri)
  (mapcar #'(lambda (param) (cl-ppcre:regex-replace-all "%20" param " "))
	  (cl-ppcre:all-matches-as-strings "\\b[a-z A-Z0-9%\\-#]+" uri)))
  

(defmacro footer ()
  `(with-html-output (*standard-output*)
     (:div :id "footer" :style "width: 550px;"
	   (:p
	    (:span :style "padding: 5px; font-size: 90%;" (fmt "~A" "&copy;2007 Telltrains Limited"))
	    (fmt "~A" ".")
	    (:a :title "About the telltrains team" 
		:href "/about" 
		:onClick "javascript:urchinTracker (\"/about\");"
		(fmt "~A" "About"))
	    (fmt "~A" ".")
	    (:a :title "The official telltrains blog" 
		:href "http://telltrains.wordpress.com/" 
		:onClick "javascript:urchinTracker (\"/blog\");"
		(fmt "~A" "Blog"))
	    (fmt "~A" ".")
	    (:a :title "Terms of service" 
		:href "/terms" 
		:onClick "javascript:urchinTracker (\"/terms\");"
		(fmt "~A" "Terms"))
	    (fmt "~A" ".")
	    (:a :title "Telltrains support and feedback forums" 
		:href "/forums/index.php" 
		:onClick "javascript:urchinTracker (\"/forums\");"
		(fmt "~A" "Forums"))
	    (fmt "~A" ".")
	    (:a :title "Most popular stations" 
		:href "/stations" 
		:onClick "javascript:urchinTracker (\"/stations\");"
		(fmt "~A" "Stations"))	       
	    (fmt "~A" ".")
	    (:a :title "Contact us" 
		:href "/contact" 
		:onClick "javascript:urchinTracker (\"/contact\");"
		(fmt "~A" "Contact"))))))
