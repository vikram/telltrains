(in-package :com.sondesh.database)

(5am:def-suite :com.sondesh.database)
(5am:in-suite :com.sondesh.database)

(defparameter *test-now* 3400390614)
(defparameter *test-string* "2007-10-03 Wed 08:57:03 +0000 (GMT)")

(defun write-test-result (name)
  (ensure-directories-exist (sondesh-file-path (concatenate 'string "tests/")))
  (with-open-file (out (sondesh-file-path (concatenate 'string "tests/" (symbol-name name))) :direction :output :if-exists :supersede)
    (write * :stream out)))

(5am:test accumulator-RDG-BTN
  (5am:is (= 262 (length (accumulate-journeys-db (find-station-by-code "G24") (find-station-by-code "G04") *test-now*)))))

(5am:test London-Stirling
  (let ((results (find-trains (find-station-by-code "G01") (find-station-by-code "STG") *test-now*)))
    (5am:is (= 38 (length results)))
    (5am:is (equal (mapcar #'car results)
		   (with-open-file (in (sondesh-file-path (concatenate 'string "tests/" (symbol-name 'London-Stirling))))
		     (read in))))))

(defmacro result-checker (name start end)
  (with-gensyms (results answers)
    `(5am:test ,name
       (let ((,results (find-trains (find-station-by-code ,start) (find-station-by-code ,end) *test-now*))
	     (,answers (with-open-file (in (sondesh-file-path (concatenate 'string "tests/" (symbol-name ',name))))
			 (read in))))
	 (5am:is (equal (mapcar #'car ,results) ,answers))))))

(result-checker london-stirling "G01" "STG")
(result-checker palmersgreen-brighton "PAL" "BTN")
(result-checker brighton-palmersgreen "BTN" "PAL")
(result-checker finsburypark-brighton "FPK" "BTN")
(result-checker london-brighton "G01" "BTN")
(result-checker guildford-londonlivstreet "GLD" "LST")
