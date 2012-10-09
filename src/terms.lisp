(in-package :com.sondesh.database)

(defun frame (title text-fn)
  (with-html
    (:html
       (:head
	(:title (fmt "Telltrains: train times timetable for train journeys - ~A" title))
	(:link :rel "icon" :href "/images/transparent.ico")
	(:meta :name "y_key" :content "0a53d0a3da763141")
	(:meta :name "verify-v1" :content "HLKhUt6V7k4rQlFX+C8v9z/bL62M/ccksoBiWLVLuBE=")
	(:meta :name "robots" :content "index, follow")
	(:meta :name "description" :content "New graphical journey planner for Britain National Rail network. train times; commuter friendly; commuter social network; train running information.")
	(:meta :content "national rail, british rail, trains, railway, uk rail travel, train times, train timetables, train information, uk trains, uk railway, late trains, rail commuter, journey planner, social network" :name "keywords")
	(:link :type "text/css" :rel "stylesheet" :href *sitecss*)
	(:script :src "http://www.google-analytics.com/urchin.js" :type "text/javascript")
	(:script :type "text/javascript"
		 "_uacct = 'UA-1779260-1';
                      urchinTracker();"))
       (:body :class "main" :style "text-align: left;"
	      (:div :id "container"
		    (:h1 (:a :href "/" "Telltrains - beta"))
		    (funcall text-fn))
	      (:div :id "outerDiv"
		   (:div :id "innerDiv"))
	      (footer)
	      (:script :type "text/javascript" :src *sitejs*)
	      (:script :type "text/javascript"
		     (fmt "~A" "// <![CDATA[
                                   var journeys = [];
                                   var numberOfJourneys = 0;
                                   var sliderValues = [0];
                                   var numRoutes = 0;
                                   var xFactor = 1;
                                   var originalleft = 0;
                                   var stations = '';
                                   var pagedate = '07-11-2007';
                                   var fb = true;
                                   $(document).ready(function() {
                                      $('#container').center('horizontal');
                                      adjust();
                                   });
                                 // ]]>" 
			  ))))))

(defun diagnostics ()
  (aif (get-parameter "reboot")
       (progn
	 (clear-all-connections)
	 (test-mobile))
       (test-mobile)))

(defun test-mobile ()
  (with-train-database
    (clsql:query "select id from ROUTE where id=1")
    (with-html
      (:html
       (:head
	(:META :http-equiv "Content-Type" :content "text/html")
	(:META :HTTP-EQUIV "expires" :CONTENT "0")
	(:META :HTTP-EQUIV "cache-control" :CONTENT "no-cache")
	(:META :HTTP-EQUIV "pragma" :CONTENT "no-cache")
	(:title "telltrains mobile"))
       (:body
	(:p "DB connections good."))))))

(defun terms ()
  (frame "Terms of Service"
	 #'(lambda ()
	     (with-html-output (*standard-output*)
	       (:div :id "textual"
		    (:h2 "Terms of Service")
		    (:p (:small "Last updated: November 8, 2007"))
		    (:h3 "Agreement Between the User and Telltrains Limited")
		    (:p "THESE TERMS AND CONDITIONS ARE THE CONTRACT BETWEEN YOU AND Telltrains Limited.")
		    (:p "PLEASE READ THEM CAREFULLY BEFORE SIGNING UP FOR OR USING THE TELLTRAINS SERVICE.")
		    (:p "Your use of Telltrains is at your sole risk. The service is provided on an 'AS IS' and 'as available' basis. This web site (\"Telltrains\") is offered as a service to you, the user, by Telltrains Limited. Your use of this Web Site constitutes your agreement to all terms, conditions and notices contained on the Web Site. If you do not agree with these terms and conditions then you are not authorised to use this Web Site.")
		    (:h3 "Personal and Non-Commercial use only")
		    (:p "This Web Site is for your personal and non-commercial use. You may not modify, copy, distribute, transmit, display, perform, reproduce, publish, license, create derivative works from, transfer, or sell any information, products or services obtained from this Web Site.")
		    (:h3 "Legal Identity")
		    (:p "Telltrains is the journey planning website of Telltrains Limited, a company registered in England (No. 6420197), whose registered office is at: 33 Chandos Court, London N14 7AA")
		    (:h3 "Liability Disclaimer")
		    (:p "Telltrains Limited makes no express or implied warranty of the quality or suitability of this service for any purpose. This Web Site is for the provision of information only. Telltrains Limited uses reasonable endeavours to check the accuracy of information published or made available on this Web Site. You should note however that Telltrains Limited does not warrant that such information will be error free and the user acknowledges that the information, products, and services published or made available on this Web Site may include inaccuracies or typographical errors. Changes are periodically made to the information herein. Telltrains Limited and/or its respective suppliers may make improvements and/or changes in this Web Site at any time.")
		    (:p
"Telltrains Limited and/or its respective suppliers make no representations about the suitability of the information, products, and services contained on this Web Site for any purpose. Telltrains Limited shall use reasonable care and skill in carrying out the services contained in this Web Site. To the maximum extent permitted by law, Telltrains Limited and/or its respective suppliers hereby disclaim all warranties, terms and conditions with regard to this information, products, and services, including all implied warranties, terms and conditions, by statute, collaterally or otherwise, of satisfactory quality, fitness for a particular purpose, title, and noninfringement. In no event shall Telltrains Limited and/or its suppliers be liable for any loss of profit, loss of opportunity, loss of business, loss of revenue, wasted time, wasted costs, indirect, incidental, special, or consequential loss arising out of or in any way connected with the use of this Web Site or with the delay or inability to use this Web Site, or for any information, products, and services obtained through this Web Site, or otherwise arising out of the use of this Web Site, whether based on contract, tort, strict liability or otherwise, even if Telltrains Limited or any of its suppliers has been advised of the possibility of damages. Except in relation to liability for death or personal injury, for which no limit applies, the liability of Telltrains Limited and/or its respective suppliers for direct loss arising out of the use of this Web Site, whether based on contract, tort, strict liability or otherwise, is limited to the total value of the transaction under which the claim arises for any one event or series of connected events.")
		    (:p
		     "We will do our best to preserve and protect your data, but make no guarantees that it wont get lost, corrupted, or inadvertently leaked. You should back up critical data or take other steps to protect your vital information.")
		    (:p
		     "You expressly agree that Telltrains Limited WILL NOT be liable for any indirect, incidental, special, consequential or exemplary damages, including but not limited to, damages for loss of profits, goodwill, use, data or other intangible losses (even if Treefly has been advised of the possibility of such damages), resulting from: (i) the use or the inability to use Telltrains Limited; (ii) the cost of procurement of substitute goods or services resulting from any goods, data, information or services purchased or obtained through or from Telltrains Limited; (iii) unauthorized access to or alteration of your transmissions or data; (iv) statements or conduct of any third party on Telltrains; (v) termination of your account; or (vi) any other matter relating to Telltrains. If you are not satisfied with Telltrains, your sole and exclusive remedy is to stop using it. IN NO EVENT WILL TELLTRAINS BE LIABLE TO YOU FOR ANY AMOUNT IN EXCESS OF THE AMOUNT THAT YOU HAVE ACTUALLY PAID DIRECTLY TO TELLTRAINS IN THE TWELVE (12) MONTHS IMMEDIATELY PRECEDING THE EVENT GIVING RISE TO YOUR CLAIM.")
		    (:p "The disclaimers and limitations of liability set forth in these terms and conditions are an essential basis of your bargain with Telltrains Limited for the use of Telltrains.
                         We reserve the right to modify the service at any time."))))))

(defun aboutus ()
  (frame "About Us"
	 #'(lambda ()
	     (with-html-output (*standard-output*)
	       (:div :id "textual"
		     (:h2 "About Telltrains")
		     (:h3 "Why create another journey planner?")
		     (:p "My name is Vikram and I created Telltrains because I am a commuter and most existing train journey planners are boring and not
                          as useful as they could be. I want the journey planner to give fast, accurate results which can be understood at a glance.")
                     (:p "Telltrains has been made better by feedback I have recieved from a lot of people. Without this feedback I wouldn't have been
                          able to continue to make refinements to this first of a kind journey planner.")
		     (:h3 "People")
		     (:p "I created and run this site with the feedback and advice of my friends, family and users.")
		     (:h3 "How often is the data updated?")
		     (:p "The data was last updated on the 11th of November 2007. We try and update the data every week. So it's reasonably current. 
                          If in doubt check out the national rail website.")
		     (:h3 "You")
		     (:p "You can make Telltrains better by reporting issues and ask for new things which might be useful to you.
                          There are a lot of things that I am working on to make this site better and more meaningful to the commuter.")
                     (:p "The site is currently in beta, which means that it needs your help Feedback, encouragement, questions, bug 
                          reports are all welcome. You can do this by dropping me a line at " 
			 (:a :href "mailto:enginedriver@telltrains.com" "enginedriver at telltrains dot com") 
			 " or by using the " 
			 (:a :href "/forums/index.php" 
			     :onClick "javascript:urchinTracker (\"/forums\");"
			     (fmt "~A" "Forums"))))))))

(defun contact ()
  (frame "Contact"
	 #'(lambda ()
	     (with-html-output (*standard-output*)
	       (:div :id "textual"
		     (:h2 "Contact us")
		     (:h3 "Tell us what you think")
		     (:p "You can make Telltrains better by reporting issues and ask for new things which might be useful to you.
                          There are a lot of things that I am working on to make this site better and more meaningful to the commuter.")
                     (:p "The site is currently in beta, which means that it needs your help Feedback, encouragement, questions, bug 
                          reports are all welcome. You can do this by dropping me a line at " 
			 (:a :href "mailto:enginedriver@telltrains.com" "enginedriver at telltrains dot com") 
			 " or by using the " 
			 (:a :href "/forums/index.php" 
			     :onClick "javascript:urchinTracker (\"/forums\");"
			     (fmt "~A" "Forums")))
		     (:p "Telltrains can only improve with your input. Be a part of creating a better journey planner.
                          Finally, I try and get back to email within a few hours, but as there is only one of me, this can sometimes
                          be difficult. So, if you don't get a response right away, please don't get mad."))))))
