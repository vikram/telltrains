(in-package :com.sondesh.database)

(defvar eof (gensym))

(defmacro with-gensyms (syms &body body)
  "Create gensyms, useful for creating macros."   ; LMH
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

; Control

(defmacro bind (&rest args)
  `(multiple-value-bind ,@args))

(defun ifnot (bad val) 
   (unless (eql bad val) val))

(defmacro nullor (x y)
  (with-gensyms (g)
    `(let ((,g ,x))
       (if (zerop (length ,g)) ,y ,g))))

(defmacro until (test &rest body)
  `(do ()
       (,test)
     ,@body))

(defmacro casequal (val &rest clauses)
  (let ((g (gensym)))
    `(let ((,g ,val))
       (cond ,@(mapcar #'(lambda (cl)
                           `(,(if (eql (car cl) t)
                                  t
                                  `(equal ,g ,(car cl)))
                             ,@(cdr cl)))
                       clauses)))))

(defun string->hex (str)
  (with-output-to-string (out)
    (dotimes (i (length str))
      (let ((c (char str i)))
        (format out "~2,'0X" (char-code c))))))
              
(defun hex->string (str)
  (if (evenp (length str))
      (with-output-to-string (out) 
        (let ((*read-base* 16))
          (dotimes (i (/ (length str) 2)) 
            (princ (code-char (read-from-string str  nil nil
                                                :start (* i 2)
                                                :end (+ (* i 2) 2)))
                   out))))
      (error (format nil "odd-length hex string: ~A" str))))

; Errors & Debugging

(defun ero (&rest args)
  (print (if (cdr args) args (car args))
         *error-output*))

(defmacro safely (expr)
  (with-gensyms (ret err)
    `(bind (,ret ,err) (ignore-errors ,expr)
       (if (typep ,err 'error)
           nil
           (values ,ret ,err)))))

(defmacro in-case-error (expr err)
  (with-gensyms (val cond)
    `(bind (,val ,cond) (ignore-errors ,expr)
       (if (typep ,cond 'error)
           (progn
             ,err
             (error ,cond))
           ,val))))

; Files

(defmacro with-infile (var fname &rest body)
  (with-gensyms (v c f)
    `(let ((,f ,fname))
       (in-case-error
         (with-open-file (,var ,f :direction :input)
           ,@body)
         (format *error-output* "Error reading from ~s.~%" ,f)))))

(defmacro with-outfile (var fname &rest body)
  (with-gensyms (v c f)
    `(let ((,f ,fname))
       (in-case-error
         (with-open-file (,var ,f :direction :output
                                  :if-exists :supersede)
           ,@body)
         (format *error-output* "Error writing to ~s.~%" ,f)))))

(defmacro with-outfiles (pairs &rest body)
  (if (null pairs)
      `(progn ,@body)
      (if (oddp (length pairs))
          (error "Odd length arg to with-outfiles")
          `(with-outfile ,(car pairs) ,(cadr pairs)
             (with-outfiles ,(cddr pairs) ,@body)))))

(defmacro with-collect ((&rest collectors) &body forms)
  "Evaluate forms, collecting objects into lists.
Within the FORMS, you can use local macros listed among collectors,
they are returned as multiple values.
E.g., (with-collect (c1 c2) (dotimes (i 10) (if (oddp i) (c1 i) (c2 i))))
 ==> (1 3 5 7 9); (0 2 4 6 8) [2 values]
In CLISP, push/nreverse is about 1.25 times as fast as pushing into the
tail, so this macro uses push/nreverse on CLISP and push into the tail
on other lisps (which is 1.5-2 times as fast as push/nreverse there)."
  #+clisp
  (let ((ret (mapcar (lambda (cc) (gensym (format nil "~s-RET-" cc)))
                     collectors)))
    `(let (,@ret)
      (declare (list ,@ret))
      (macrolet ,(mapcar (lambda (co re) `(,co (form) `(push ,form ,',re)))
                         collectors ret)
        ,@forms
        (values ,@(mapcar (lambda (re) `(sys::list-nreverse ,re)) ret)))))
  #-clisp
  (let ((ret (mapcar (lambda (cc) (gensym (format nil "~s-RET-" cc)))
                     collectors))
        (tail (mapcar (lambda (cc) (gensym (format nil "~s-TAIL-" cc)))
                      collectors))
        (tmp (mapcar (lambda (cc) (gensym (format nil "~s-TMP-" cc)))
                     collectors)))
    `(let (,@ret ,@tail)
      (declare (list ,@ret ,@tail))
      (macrolet ,(mapcar (lambda (co re ta tm)
                           `(,co (form)
                             `(let ((,',tm (list ,form)))
                               (if ,',re (setf (cdr ,',ta) (setf ,',ta ,',tm))
                                   (setf ,',re (setf ,',ta ,',tm))))))
                         collectors ret tail tmp)
        ,@forms
        (values ,@ret)))))


(defun hash-table->alist (ht)
  "Return the alist with the same data as the hash-table.
Actually, the first element is the test: '(eql (key0 . val0) (key1 . val1)).
The inverse is `alist->hash-table'."
  (declare (hash-table ht))
  (cons (hash-table-test ht)
        (with-collect (co)
          (with-hash-table-iterator (iter ht)
            (loop (multiple-value-bind (re kk vv) (iter)
                    (unless re (return))
                    (co (cons kk vv))))))))

(defun alist->hash-table (alist &optional (value-fn #'identity))
  "Return the new hash-table based on this alist.
The inverse is `hash-table->alist'."
  (declare (list alist))
  (let ((ht (make-hash-table :test (car alist))))
    (dolist (co (cdr alist) ht)
      (setf (gethash (car co) ht) (funcall value-fn (cdr co))))))

(defmethod print-object ((ht hash-table) (out stream))
  (if *print-readably*
      (format out "~s" (hash-table->alist ht))
      (call-next-method)))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun map-two (function list combiner)
  (cond ((null list) nil)
	((null (cddr list)) (funcall function (first list) (second list)))
	(t (funcall combiner (funcall function (first list) (second list))
		    (map-two function (cddr list) combiner)))))

(defmacro with-gentemps (syms &body body)
  "Create gensyms, useful for creating macros."   ; LMH
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gentemp)))
                 syms)
     ,@body))

(defun mappend (fn &rest lsts)
  (declare (function fn))   ; LMH
  "Nondestructive form of mapcan."   ; LMH
  (apply #'append (apply #'mapcar fn lsts)))

;;;; ********************************************************************************
;;;; Section 15.1, Macros Returning Functions: Building Functions
;;;; ********************************************************************************

;;; fn subsumes fif, fint, fun

(defmacro fn (expr)
  "Make the function according to the expression,
   e.g., (fn (and ingegerp oddp)) makes the function
   #'(lambda (x) (and (integerp x) (oddp x)))."   ; LMH
 `#',(rbuild expr))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
          (build-compose (cdr expr))
          (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
                          `(,(rbuild f) ,g))
                      fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
                   (if fns
                       `(,(rbuild (car fns))
                         ,(rec (cdr fns)))
                       g)))
          (rec fns)))))


;;;; ********************************************************************************
;;;; Section 4.7, Utility Functions: Symbols and Strings
;;;; ********************************************************************************

(defun mkstr (&rest args)
  "Make a string out of the printed representations of the arguments."   ; LMH
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "Make a symbol out of the printed representations of the arguments."   ; LMH
  (values (intern (apply #'mkstr args))))

(defun make-logic-symbol (string)
  "Convert string to symbol, preserving case, except for AND/OR/NOT/FORALL/EXISTS."
  (cond ((find string '(and or not forall exists) :test #'string-equal))
        ((lower-case-p (char string 0)) 
	 (symb (string-upcase string)))
	((equal string "Nil") '|Nil|)
        (t (intern (string-upcase string)))))

(defun compose (&rest fns)
  "Compose the functions."   ; LMH
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
	(declare (function fn1))			; LMH
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

;;;; ********************************************************************************
;;;; Section 14.1, Anaphoric Macros: Anaphoric Variants
;;;; ********************************************************************************

(defmacro awhen (test-form &body body)
  `(let ((it ,test-form))
     (declare (ignorable it))
     (when it
       ,@body)))

(defmacro aif (test-form then-form &optional else-form)
  "Anaphoric if: use `it' in then-form, else-form to
   refer to result of the test-form."   ; LMH
  `(let ((it ,test-form))
     (declare (ignorable it))		; LMH
     (if it ,then-form ,else-form)))


(defmacro acond2 (&rest clauses)
  "Anaphoric cond for two returned values: use `it' in body to
   refer to result of the test.  Test can return two values,
   when is satisfied if either is true, but `it' refers to first."   ; LMH
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (val (gensym))
            (win (gensym)))
        `(multiple-value-bind (,val ,win) ,(car cl1)
           (if (or ,val ,win)
               (let ((it ,val))
		 (declare (ignorable it)); LMH
		 ,@(cdr cl1))
               (acond2 ,@(cdr clauses)))))))

(defun gensym? (s)
  (and (symbolp s) (not (symbol-package s))))

;;utilities

(defun simple? (x) (or (atom x) (eq (car x) 'quote)))

(defun vars-in (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr) atom?)
	     (vars-in (cdr expr) atom?))))

(defun user-var? (x)
  (and (var? x)
       (not (cl-ppcre:all-matches ".[a-zA-Z]+[0-9]+" (symbol-name x)))))

(defun var? (x)
  (and (symbolp x)
       (eq (char (symbol-name x) 0) #\?)
       (< 1 (length (symbol-name x)))))

(defun match (x y &optional binds)
  (acond2
   ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
   ((binding x binds) (match it y binds))
   ((binding y binds) (match x it binds))
   ((var? x) (values (cons (cons x y) binds) t))
   ((var? y) (values (cons (cons y x) binds) t))
   ((and (consp x) (consp y) (match (car x) (car y) binds))
    (match (cdr x) (cdr y) it))
   (t (values nil nil))))

(defun binding (x binds)
  (labels ((recbind (x binds)
	     (aif (assoc x binds)
		  (or (recbind (cdr it) binds)
		      it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))


(defmacro with-places (pat seq &body body)
  "Destructuring bind on generalized variables."   ; LMH
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(wplac-ex (destruc pat gseq #'atom) body))))

(defun destruc (pat seq &optional (atom? #'atom) (n 0))
  (declare (function atom?) (fixnum n))   ; LMH
  (if (null pat)
      nil
      (let ((rest (cond ((funcall atom? pat) pat)
                        ((eq (car pat) '&rest) (cadr pat))
                        ((eq (car pat) '&body) (cadr pat))
                        (t nil))))
       (if rest
           `((,rest (subseq ,seq ,n)))
           (let ((p (car pat))
                 (rec (destruc (cdr pat) seq atom? (1+ n))))
             (if (funcall atom? p)
                 (cons `(,p (elt ,seq ,n))
                       rec)
                 (let ((var (gensym)))
                   (cons (cons `(,var (elt ,seq ,n))
                               (destruc p var atom?))
                         rec))))))))

(defun wplac-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(symbol-macrolet ,(mapcar #'(lambda (b)
                                     (if (consp (car b))
                                         (car b)
                                         b))
                                 binds)
        ,(wplac-ex (mapcan #'(lambda (b)
                               (if (consp (car b))
                                   (cdr b)))
                           binds)
                   body))))


(defun make-alist (keys values)
  (let ((results nil))
    (dotimes (x (length keys))
      (push (cons (elt keys x) (elt values x)) results))
    results))

(defun mapcar-filter (fn list predicate)
  (mapcar fn (remove-if-not predicate list)))

(defun page-start-end (members start end)
  (let ((len (length members)))
    (cond ((>= len end) (list (1- start) end))
	  ((< len start) (list 0 0))
	  (t (list (1- start) len)))))

(defun safe-subseq (seq start end)
  (aif (page-start-end seq start end)
       (subseq seq (first it) (second it))))


(defun get-posix-details (file)
  (let ((results nil))
    (with-open-file (in file :direction :input)
      (do ((line (read-line in nil :eof nil)
		 (read-line in nil :eof nil)))
	  ((eql line :eof))
	(when (not (string= "#" (subseq line 0 1)))
	  (push (cl-ppcre:split ":" line) results))))
    results))

(defun get-posix-id (file)
  #'(lambda (string)
      (parse-integer 
       (third
	(car 
	 (remove-if-not #'(lambda (details) (string= string (car details))) (get-posix-details file)))))))

(defun get-posix-user-id (user)
  (funcall (get-posix-id "/etc/passwd") user))

(defun get-posix-group-id (group)
  (funcall (get-posix-id "/etc/group") group))

(defun memoize (fn name &key (test #'eql))
  (declare (function fn))
  (let ((cache (make-hash-table :test test)))
    (setf (get name 'memo) cache)
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args)))))))

(defmacro defmemoize (name test args &body body)
  "Create a function which memoizes its arguments."
  (let ((nm (gensym)))
   `(flet ((,nm ,args ,@body))
      (setf (get ',name 'test) ,test)
      (setf (symbol-function ',name) (memoize (function ,nm) ',name :test ,test)))))

(defun clear-memoize (fn-name)
  (awhen (get fn-name 'memo)
    (clrhash it)))

;(make-hash-table :test (get fn-name 'test)))))

(defun agree (x y)
  (if (or (null x) (null y))
      t
      (if (equal (car x) (car y))
          (agree (cdr x) (cdr y)))))

(defun assocify (source)
  (labels ((rec (source acc)
             (let ((rest (cddr source)))
               (if (consp rest)
                   (rec rest (cons (cons (car source) (cadr source)) acc))
                   (nreverse (cons (cons (car source) (cadr source)) acc))))))
    (if source (rec source nil) nil)))

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun careql (x y)
  (and (consp x) (eql (car x) y)))

(defmacro carin (x &rest args)
  (with-gensyms (g)
    `(let ((,g ,x))
       (and (consp ,g) (in (car ,g) ,@args)))))

(defun carat (x)
  (if (consp x) (car x) x))

(define-modify-macro pushend (obj)
  (lambda (place obj)
    (nconc place (list obj))))

(define-modify-macro merge-into (obj fn)
  (lambda (place obj fn)
    (merge 'list place (list obj) fn)))

(defun firstn (lst n)
  (if (or (null lst) (<= n 0))
      nil
      (cons (car lst)
            (firstn (cdr lst) (- n 1)))))

(defun butlastn (seq n)
  (subseq seq 0 (- (length seq) n)))

(defun rotlist (x)      
  (if (cdr x) 
      (cons (car (last x)) (butlast x))
      x))

(defun prefix (pref str)
  (search pref str :end2 (min (length pref) (length str))))
    
(defun suffix (str suff)
  (search suff str :start2 (- (length str) (length suff))))

(defun insert-before (before after lst)
  (cond ((null lst) nil)
        ((eql (car lst) after)
         (cons before lst))
        (t (cons (car lst) (insert-before before after (cdr lst))))))

(defun insert-after (before after lst)
  (cond ((null lst) nil)
        ((eql (car lst) before)
         (cons before (cons after (cdr lst))))
        (t (cons (car lst) (insert-after before after (cdr lst))))))

(defmacro pull-nth (n place)
  (multiple-value-bind (vars forms var set access)
                       (get-setf-method place)
    (let ((g (gensym)))
      `(let* ((,g ,n)  
              ,@(mapcar #'list vars forms)
              (,(car var) (delete-nth ,g ,access)))
         ,set))))
     
(defun delete-nth (n lst)
  (cond ((< n 0) (error "Bad arg to delete-nth"))
        ((= n 0) (cdr lst))
        (t (let ((rest (nthcdr (1- n) lst)))
             (pop (cdr rest))
             lst))))

(defmacro push-nth (n obj place)
  (multiple-value-bind (vars forms var set access)
                       (get-setf-method place)
    (with-gensyms (g h) 
      `(let* ((,g ,n)
              (,h ,obj)
              ,@(mapcar #'list vars forms)
              (,(car var) (ninsert-nth ,g ,h ,access)))
         ,set))))
    
(defun ninsert-nth (n obj lst)
  (if (< n 0)
      (error "Bad arg to ninsert-nth")
      (let ((rest (nthcdr n lst)))
        (push obj (cdr rest))
        lst)))
  
(defun insert-elt-after (elt ins lst)
  (if (null lst)
      nil     
      (if (eql (car lst) elt)
          (cons (car lst) (cons ins (cdr lst)))
          (cons (car lst) (insert-elt-after elt ins (cdr lst))))))

; Iteration

(defmacro do-all (var x &rest body)
  (with-gensyms (g)
    `(let ((,g ,x))
       (if (consp ,g)
           (dolist (,var ,g) ,@body)
           (let ((,var ,g)) ,@body)))))

(defmacro dolists (pairs &rest body)
  (with-gensyms (f)
    (let ((parms (mapcar #'(lambda (p) (declare (ignore p)) (gensym)) pairs)))
      `(labels ((,f ,parms
                  (when (or ,@parms)
                    (let ,(mapcar #'(lambda (p g)
                                      (list (car p) `(car ,g)))
                                  pairs
                                  parms)
                      ,@body)
                    (,f ,@(mapcar #'(lambda (g) `(cdr ,g))
                                  parms)))))
         (,f ,@(mapcar #'cadr pairs))))))

(defmacro do3 (v1 v2 v3 list &rest body)
  (with-gensyms (g h)
    `(let ((,g ,list))
       (do ((,h ,g (cdr ,h)))
           ((null ,h) nil)
         (let ((,v1 (car ,h))
               (,v2 (if (cdr ,h) (cadr ,h) (car ,g)))
               (,v3 (if (cdr ,h)
                        (if (cddr ,h)
                            (third ,h)
                            (car ,g))
                        (if (cdr ,g)
                            (second ,g)
                            (car ,g)))))
           ,@body)))))

; Assumes 3 args.  Inefficient.

(defmacro do-cyclic (parms source &rest body)
  (let ((s (gensym)))
    `(let ((,s ,source))
       (case (length ,s)
         (0 nil)
         (1 (let ((,(first parms) (first ,s))
                  (,(second parms) (first ,s))
                  (,(third parms) (first ,s)))
              ,@body))
         (2 (let ((,(first parms) (second ,s))
                  (,(second parms) (first ,s))
                  (,(third parms) (second ,s)))
              ,@body)
            (let ((,(first parms) (first ,s))
                  (,(second parms) (second ,s))
                  (,(third parms) (first ,s)))
              ,@body))
         (t (do-tuples/c ,parms (rotlist ,s)
              ,@body))))))

(defmacro do-plist (v1 v2 plist &rest body)
  (with-gensyms (rec rest pl)
    `(labels ((,rec (,v1 ,v2 ,rest)
                 ,@body
                 (when ,rest
                   (,rec (car ,rest) (cadr ,rest) (cddr ,rest)))))
       (let ((,pl ,plist))
         (when (consp ,pl)
           (,rec (car ,pl) (cadr ,pl) (cddr ,pl)))))))

; Hash Tables

(defun nonempty-ht (ht)
  (maphash #'(lambda (k v) (return-from nonempty-ht t))
           ht)
  nil)

(defun ht-car (ht)
  (maphash #'(lambda (k v) (return-from ht-car v))
           ht))

(defun hash-keys (ht)
  (let ((acc nil))
    (maphash #'(lambda (k v) 
                 (declare (ignore v)) 
                 (push k acc))
             ht)
    acc))

(defun hash-vals (ht)
  (let ((acc nil))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (push v acc))
             ht)
    acc))

(defun hash-pairs (ht)
  (let ((acc nil))
    (maphash #'(lambda (k v)
                 (push (cons k v) acc))
             ht)
    acc))

(defun somehash (fn ht)
  (maphash #'(lambda (k v)
               (when (funcall fn v)
                 (return-from somehash v)))
           ht)
  nil)
     
(defun key-match (ht1 ht2)
  (maphash #'(lambda (k v)
               (declare (ignore v))
               (when (gethash k ht2)
                 (return-from key-match k)))
           ht1)
  nil)

; Date & Time

(defun time->hr (time)
  (multiple-value-bind (s m h) (decode-universal-time time 0)
    h))

(defun time-string ()
  (multiple-value-bind (s m h) (get-decoded-time)
    (format nil "~A:~2,,,'0@A:~2,,,'0@A" h m s)))

(defvar months
  '#("Jan" "Feb" "Mar" "Apr" "May" "June" "Jul" "Aug" "Sept" "Oct" "Nov" "Dec"))

(defun date-string ()
  (multiple-value-bind (ig no re d mo y) (get-decoded-time)
    (declare (ignore ig no re))
    (format nil "~A ~A ~A" d (svref months (1- mo)) y)))

(defun date+time-string (&optional (u (get-universal-time)))
  (multiple-value-bind (s m h d mo y) (decode-universal-time u 0)
    (format nil "~A ~A ~A ~A:~2,,,'0@A:~2,,,'0@A"
                d (svref months (1- mo)) y h m s)))

;;aserve

(defvar ch-alpha 0)
(defvar ch-space 1)
(defvar ch-sep   2)  ; separators

(defvar *syntax-table*
    (let ((arr (make-array 100000 :initial-element ch-alpha)))
      
      ; the default so we don't have to set it
      #+ignore (do ((code (char-code #\!) (1+ code)))
	  ((> code #.(char-code #\~)))
	(setf (svref arr code) ch-alpha))
      
      (setf (svref arr (char-code #\space)) ch-space)
      (setf (svref arr (char-code #\Page)) ch-space)
      (setf (svref arr (char-code #\tab)) ch-space)
      (setf (svref arr (char-code #\return)) ch-space)
      (setf (svref arr (char-code #\linefeed)) ch-space)
       arr))

(defun join-with (lst str)
  (reduce #'(lambda (string el) (concatenate 'string string str el)) lst))

(defun replaced-string-length (str repl-alist)
  (declare (simple-string str)
	   (optimize (speed 3) (safety 0) (space 0)))
    (do* ((i 0 (1+ i))
	  (orig-len (length str))
	  (new-len orig-len))
	 ((= i orig-len) new-len)
      (declare (fixnum i orig-len new-len))
      (let* ((c (char str i))
	     (match (assoc c repl-alist :test #'char=)))
	(declare (character c))
	(when match
	  (incf new-len (1- (length
			     (the simple-string (cdr match)))))))))

(defun substitute-chars-strings (str repl-alist)
  "Replace all instances of a chars with a string. repl-alist is an assoc
list of characters and replacement strings."
  (declare (simple-string str)
	   (optimize (speed 3) (safety 0) (space 0)))
  (do* ((orig-len (length str))
	(new-string (make-string (replaced-string-length str repl-alist)))
	(spos 0 (1+ spos))
	(dpos 0))
      ((>= spos orig-len)
       new-string)
    (declare (fixnum spos dpos) (simple-string new-string))
    (let* ((c (char str spos))
	   (match (assoc c repl-alist :test #'char=)))
      (declare (character c))
      (if match
	  (let* ((subst (cdr match))
		 (len (length subst)))
	    (declare (fixnum len)
		     (simple-string subst))
	    (dotimes (j len)
	      (declare (fixnum j))
	      (setf (char new-string dpos) (char subst j))
	      (incf dpos)))
	(progn
	  (setf (char new-string dpos) c)
	  (incf dpos))))))

(defun substitute-string-for-char (procstr match-char subst-str)
  "Substitutes a string for a single matching character of a string"
  (substitute-chars-strings procstr (list (cons match-char subst-str))))

(defun add-sql-quotes (s)
  (substitute-string-for-char s #\' "''"))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defun time->values (seconds)
  (multiple-value-bind (min sec) (floor seconds 60)
    (multiple-value-bind (hour min) (floor min 60)
      (multiple-value-bind (day hour) (floor hour 24)
	(values day hour min sec)))))

(defun time2date (num)
  "Convert the universal time (GMT) to date."
  (declare (real num))
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time num 0)
    (declare (ignore se mi ho))
    (make-date :ye ye :mo mo :da da :dd (floor num +day-sec+))))

(defun stylized-time-diff (time)
  (let* ((date (time2date time))
	 (days (days-between date)))
    (cond ((< 0 days) (format nil " ~A days ago" days))
	  (t (multiple-value-bind (se1 mi1 ho1 da1 mo1 ye1 dd1 dst1 tz1) (decode-universal-time time 0)
	       (multiple-value-bind (se2 mi2 ho2 da2 mo2 ye2 dd2 dst2 tz2) (decode-universal-time (get-universal-time) 0)
		 (cond ((< 0 (- ho2 ho1)) (format nil " ~A hours ago" (- ho2 ho1)))
		       (t (format nil " ~A mins ago" (- mi2 mi1))))))))))	      

(defun stylized-dttm (time)
  (let* ((date (time2date time))
	 (days (days-between date)))
    (multiple-value-bind (se mi ho da mo ye dd dst1 tz1)
	(decode-universal-time time 0)
      (cond ((< 0 days) (format nil "~A ~A" (aref +month-names+ (1- (date-mo date))) (date-da date)))
	    (t (format nil "~A:~2,'0d ~A" ho mi (if (< ho 12) "am" "pm")))))))

(defun stylized-time (date-as-string)
  (stylized-dttm (string->dttm date-as-string)))

(defun time-ampm (time)
  (let* ((hour-min (cl-ppcre:split ":" time))
	 (hour (parse-integer (first hour-min)))
	 (min (parse-integer (second hour-min))))
    (format nil "~A:~2,'0d ~A" 
	    (if (> hour 12)
		(- hour 12)
		hour)
	    min 
	    (if (< hour 12) "AM" "PM"))))

(defun time-hrmin (time)
  (let* ((hour-min (cl-ppcre:split ":" time))
	 (hour (parse-integer (first hour-min)))
	 (min (parse-integer (second hour-min)))
	 (min-string (if (= 0 min) "" (format nil "~A min" min))))
    (case hour
      (0 min-string)
      (1 (format nil "1 hour~A~A" (if (= 0 min) "" ", ") min-string))
      (t (format nil "~A hours~A~A" hour (if (= 0 min) "" ", ") min-string)))))

(defun skip (seq &key (step 0))
  (mapa-b #'(lambda (i) (nth i seq)) 0 (1- (length seq)) step))

(uffi:def-function ("gethostname" c-gethostname)
    ((name (* :unsigned-char))
     (len :int))
  :returning :int)

(defun gethostname ()
  "Returns the hostname"
  (uffi:with-foreign-object (name '(:array :unsigned-char 256))
    (if (zerop (c-gethostname (uffi:char-array-to-pointer name) 256))
	(uffi:convert-from-foreign-string name)
	(error "gethostname() failed."))))

(defmacro make-md5 (from to-list subject)
  (let ((to (gensym)) (rest (gensym)))
    `(let ((,to ,to-list)
	   (,rest (list ,from ,subject)))
       (format nil "~(~{~2,'0X~}~)" 
	       (map 'list #'identity 
		    (md5:md5sum-sequence 
		     (apply #'concatenate 'string (append ,rest ,to))))))))

(defun write-hashtable-stream (stream ht)
  (when ht
    (maphash #'(lambda (key value)
		 (print (cons key value) stream)) 
	     ht)))

(defun write-hashtable (file ht)
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (write-hashtable-stream stream ht)))

(defun read-hashtable-stream (stream)
  (let* ((ht (make-hash-table :test #'equal)))
    (loop for line = (read stream nil nil)
       until (null line)
       do
	 (setf (gethash (car line) ht) (cdr line)))
    ht))

(defun read-hashtable (file)
  (with-open-file (stream file :direction :input)
    (read-hashtable-stream stream)))

(defun read-array (file)
  (with-open-file (stream file :direction :input)
    (read stream)))

(defun write-array (array file)
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (write array :stream stream)))

(defun trec (rec &optional (base #'identity) (leaf #'atom))
  (declare (function rec))   ; LMH
  "Tree traverser: Recurse down a tree.  Rec is a function
   that takes three arguments, the first is the tree,
   the second is the result of the left branch,
   the third is the result of the right branch.  Base is a function called
   or value returned if at a leaf.  Differs from ttrav in that
   it need not traverse the whole tree. "   ; LMH
  (labels
    ((self (tree)
       (if (funcall leaf tree)
           (if (functionp base)
               (funcall base tree)
               base)
           (funcall rec tree
                        #'(lambda ()
                            (self (car tree)))
                        #'(lambda ()
                            (if (cdr tree)
                                (self (cdr tree))))))))
    #'self))

;;; Example for rfind-if:
;;;   (trec #'(lambda (o l r) (or (funcall l) (funcall r)))
;;;	    #'(lambda (tree) (and (oddp tree) tree))))

(defmacro atrec (rec &optional (base 'it) (leaf 'atom))
  "Anaphoric tree recursion: current tree is 'it, left subtree is 'left
   right subtree is 'right."   ; LMH
  (let ((lfn (gensym)) (rfn (gensym)))
    `(trec #'(lambda (it ,lfn ,rfn)
	       (declare (ignorable it) (function ,lfn ,rfn))   ; LMH
               (#+cltl2 symbol-macrolet #+symbolics clos:symbol-macrolet ((left (funcall ,lfn))
                                 (right (funcall ,rfn)))
                 ,rec))
           #'(lambda (it) (declare (ignorable it)) ,base)
	   #'(lambda (it) (declare (ignorable it)) ,leaf))))

(defmacro on-trees (rec base leaf &rest trees)
  "Anaphoric tree recursion, for defining named functions"   ; LMH
  `(funcall (atrec ,rec ,base ,leaf) ,@trees))

(defun fint (fn &rest fns)
  (declare (function fn))			; LMH
  "Function intersection: a function that is the
   AND of each function called on the argument."   ; LMH
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
	(declare (function chain))		; LMH correct?
        #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))

(defun prune (test tree)
  (labels ((rec (tree acc)
	     (cond ((null tree) (nreverse acc))
		   ((consp (car tree))
		    (rec (cdr tree)
			 (cons (rec (car tree) nil) acc)))
		   (t (rec (cdr tree)
			   (if (funcall test (car tree))
			       acc
			       (cons (car tree) acc)))))))
    (rec tree nil)))

(defun lrec (rec &optional base)
  (declare (function rec))			; LMH
  "Function to define flat list recurser.
   Rec is a function that takes two arguments, the first
   will be the car of the list, the second is a function
   that will produce the cdr.  Base is a function to be called
   or value when the list is empty.
   For example, a length function could be defined as
   (lrec #'(lambda (x f) (1+ (funcall f))) 0)."   ; LMH
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                              #'(lambda ()
                                  (self (cdr lst)))))))
    #'self))

(defmacro alrec (rec &optional base)
  "Anaphoric list recurser (lrec): use `it' to refer to the current
   car of the list, and `rec' to the function rec itself.
   every on #'oddp,
   (alrec (and (oddp it) rec) t) is the equivalent of
   (lrec #'(lambda (x f) (and (oddp x) (funcall f))) t)."   ; LMH
  (let ((gfn (gensym)))
    `(lrec #'(lambda (it ,gfn)
	       (declare (ignorable it) (function ,gfn))   ; LMH
               (#+cltl2 symbol-macrolet #+symbolics clos:symbol-macrolet ((rec (funcall ,gfn)))
                 ,rec))
           ,base)))

(defmacro on-cdrs (rec base &rest lsts)
  "Anaphoric list recursion, for defining named functions,
   e.g., (defun our-every (fn lst) (on-cdrs (and (funcall fn it) rec) t lst))."   ; LMH
  `(funcall (the function (alrec ,rec #'(lambda () ,base))) ,@lsts))   ; LMH the function

(defun map0-n (fn n)
  (declare (function fn) (fixnum n))   ; LMH
  "Apply the fn to the list of numbers 0...n."  ; LMH
  (mapa-b fn 0 n))

(defun map1-n (fn n)
;  (declare (function fn) (fixnum n))   ; LMH
  "Apply the fn to the list of numbers 1...n."  ; LMH
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
 ; (declare (function fn) (fixnum a b step))   ; LMH
  "Apply the fn to the list of numbers a...b, stepping with step."  ; LMH
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
  ;  (declare (fixnum i))   ; LMH
    (push (funcall fn i) result)))

(defun map-subsequent-two (function list combiner)
  (declare (function function combiner) (list list))
  (cond ((null list) nil)
	((null (cddr list)) (funcall function (first list) (second list)))
	(t (funcall combiner (funcall function (first list) (second list))
		    (map-subsequent-two function (cdr list) combiner)))))

;;(map-subsequent-two function (cdr list) (accumulate combiner (funcall function (first list) (second list))))

(defun trim (string)
  (string-trim '(#\Newline #\linefeed #\return #\space) string))

(defun trim-funny (string)
  (string-trim '(#\Newline #\linefeed #\return #\space #\, #\. #\) #\( #\; #\: #\? #\! #\@) string))

(defun find2 (fn lst)
  "Find the first element of lst that satisfies fn, returning
   both the element and result of fn.  Like find, but also returns
   the result of calling fn."   ; LMH
  (declare (function fn))   ; LMH
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  "Test if x occurs before y in lst.
   Returns true if second element doesn't occur at all."   ; LMH
  (declare (function test))   ; LMH
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  "Test if x occurs after y in lst."   ; LMH
  (declare (function test))   ; LMH
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun split-if (fn lst)
  "Split the list into two at the first element that satisfies fn."   ; LMH
  (declare (function fn))   ; LMH
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

(defun most (scoring-fn lst)
  "Return the element and the score that returns the highest
   values of the scoring-fn.  Scoring-fn is a function that
   returns a fixnum."   ; LMH
  (declare (function scoring-fn))			; LMH
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall scoring-fn wins)))
	(declare (fixnum max))			; LMH scoring-fn must return fixnum
        (dolist (obj (cdr lst))
          (let ((score (funcall scoring-fn obj)))
	    (declare (fixnum score))		; LMH scoring-fn must return fixnum
            (when (> score max)
              (setq wins obj
                    max  score))))
        (values wins max))))

(defun best (predicate lst)
  "The element of lst for which the predicate always returns t when called
   with other elements in lst, like (car (sort lst predicate)) but
   potentially more efficient."   ; LMH
  (declare (function predicate))   ; LMH
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall predicate obj wins)
              (setq wins obj)))
        wins)))

(defun mostn (scoring-fn lst)
  "Return a list of all elements and the score that return the highest
   values of the scoring-fn.  Scoring-fn is a function that
   returns a fixnum."   ; LMH
  (declare (function scoring-fn))			; LMH
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall scoring-fn (car lst))))
	(declare (fixnum max))			; LMH scoring-fn must return fixnum
        (dolist (obj (cdr lst))
          (let ((score (funcall scoring-fn obj)))
	    (declare (fixnum score))		; LMH scoring-fn must return fixnum
            (cond ((> score max)
                   (setq max    score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

(defun freqs (seq &key (test #'eql) (key #'identity))
  "Return an alist of (num . freq) of elements of the SEQ.
The alist is sorted by decreasing frequencies. TEST defaults to `eql'."
  (declare (sequence seq) (type (function (t t) t) test)
           (type (function (t) t) key))
  (unless (null seq)
    (sort
     (reduce (lambda (res el)
               (let ((fi (assoc el res :test test)))
                 (cond (fi (incf (cdr fi)) res) ((acons el 1 res)))))
             seq :key key :initial-value nil)
     #'> :key #'cdr)))

(defun hour->stylized-time-long (hour &optional (next-day-p nil))
  (if next-day-p
      (cond ((= hour 12) "noon next day")
	    ((= hour 0) "midnight next day")
	    ((< hour 12) (format nil "~Aam next day" hour))
	    (t (format nil "~Apm next day" (- hour 12))))
      (cond ((= hour 12) "noon today")
	    ((= hour 0) "midnight today")
	    ((< hour 12) (format nil "~Aam today" hour))
	    (t (format nil "~Apm today" (- hour 12))))))

(defun hour->stylized-time (hour)
  (cond ((= hour 12) "noon")
	((= hour 0) "midnight")
	((< hour 12) (format nil "~Aam" hour))
	(t (format nil "~Apm" (- hour 12)))))

(defun read-table (tablename)
  (let* ((name (if (symbolp tablename)
		   (string-downcase (symbol-name tablename))
		   tablename))
	 (path (make-pathname :directory *default-database* :name name)))
    (if (probe-file path)
	(read-hashtable path)
	(make-hash-table :test 'equal))))

(defun write-table (tablename table)
  (let* ((name (if (symbolp tablename)
		   (string-downcase (symbol-name tablename))
		   tablename))
	 (path (make-pathname :directory *default-database* :name name)))
    (write-hashtable path table)))

#|
(defun create-table-field (table field)
  `(setf (symbol-function ',(make-logic-symbol (concatenate 'string (symbol-name table) "-" (symbol-name field))))
	#'(lambda (alist) 
	    (cdr (assoc ',field alist)))))

;;table defs

(defmacro def-table (table &rest fields)
  `(progn
     (setf (symbol-function ',(make-logic-symbol (concatenate 'string "make-" (symbol-name table))))
	   #'(lambda (,@fields)
	       (list ,@(mapcar #'(lambda (field) `(cons ',field ,field)) fields))))
     ,@(mapcar #'(lambda (field) (create-table-field table field)) fields)))

(def-table id last-id)

(defstruct (station (:type list)) code length downcase-name name)

(defparameter *stations* 
  (with-open-file (in "/home/vb/sondesh/src/database/station.codes" :direction :input)
    (mapcar #'(lambda (pair) (make-station :code (car pair) 
					   :length (length (cdr pair)) 
					   :downcase-name (string-downcase (cdr pair))
					   :name (cdr pair)))
	    (read in))))

|#


(defun current-short-time (&optional (time (now)))
  (multiple-value-bind (se mi ho da mo ye dd dst1 tz1)
      (decode-universal-time time 0)
    (declare (ignore se ye da mo dd dst1 tz1) (fixnum ho mi))
    (format nil "~2,'0d:~2,'0d" ho mi)))

(defun now ()
  (get-universal-time))

(defun hourmin->shorttime (hour min)
  (let ((hr (if (< hour 24) 
		hour
		(mod hour 24))))
  (multiple-value-bind (s m h d mo y) (decode-universal-time 
				       (+ (string->dttm (format nil "2007-04-02 Mon ~A:0:0 +0000 (GMT)" hr))
					  (* 60 min))
				       0)
    (declare (ignore s d mo y))
    (format nil "~2,'0d:~2,'0d" h m))))

(defun current-hour ()
  (multiple-value-bind (se mi ho da mo ye dd dst1 tz1)
      (decode-universal-time (now) 0)
    (declare (ignore se ye dd dst1 tz1 mi da mo) (fixnum ho))
    ho))

(defun current-hrmins (&optional (time (now)))
  (multiple-value-bind (se mi ho da mo ye dd dst1 tz1)
      (decode-universal-time time 0)
    (declare (ignore se ye dd dst1 tz1 da mo) (fixnum ho))
    (+ (* 100 ho) mi)))

(defun element= (list tag class id)
  (labels ((element-part (element part)
	     (aif (member part element)
		  (cadr it)
		  "")))
    (cond ((atom list) nil)
	  ((atom (car list)) (and (eql tag (car list))
				  (string= "" class)
				  (string= "" id)
				  list))
	  (t (and (eql tag (caar list))
		  (or (string= "" class) (string= (element-part (car list) :class) class))
		  (or (string= "" id) (string= (element-part (car list) :id) id))
		  list)))))
	
(defun element-p (list)
  (and list
       (not (atom list))
       (or (and (atom (car list)) (symbolp (car list)))
	   (not (remove-if #'atom (car list))))
       (cdr list)))

(defun find-html-node (html-list tag &key (class "") (id ""))
  (cond ((null html-list) nil)
	((element-p html-list) 
	 (or (element= html-list tag class id)
	     (some #'(lambda (part) (find-html-node part tag :class class :id id))
		   (cdr html-list))))
	(t nil)))

(defun remove-html-node-if-not (html-list test)
  (labels ((remove-html-rec (list accumulator)
	     (cond ((null list) accumulator)
		   ((element-p list)
		    (if (funcall test list)
			(cons list accumulator)
			(let ((result accumulator))
			  (dolist (element (cdr list))
			    (setf result (remove-html-rec element result)))
			  result)))
		   (t accumulator))))
    (nreverse (remove-html-rec html-list '()))))

(defun group (source &key (key #'car) (rest #'cdr) (test #'eql) (accumulator '()))
  (if (null source)
      accumulator
      (let* ((rest-of-them (remove-if-not #'(lambda (a) 
					      (funcall test 
						       (funcall key a)
						       (funcall key (first source))))
					  (rest source)))
	     (grouped (cons (funcall key (first source))
			    (cons (funcall rest (first source))
				  (mapcar rest rest-of-them)))))
	(group (set-difference source (cons (first source) rest-of-them)) 
	       :key key :rest rest :test test :accumulator (push grouped accumulator)))))

(defun group-by-nth (source n &optional (counter 0) (accumulator '()))
  (if (or (null source)
	  (atom source))
      (mapcar #'reverse accumulator)
      (progn
	(aif (nth (mod counter n) accumulator)
	     (setf (nth (mod counter n) accumulator) (cons (car source) (nth (mod counter n) accumulator)))
	     (push (list (car source)) accumulator))
	(group-by-nth (cdr source) n (+ 1 counter) accumulator))))

(defun timedifference-nd (time1 time2)
  (timedifference time1 time2 :next-day-p (string< time1 time2)))

(defun time-diff-short-time (time1 time2 &key (next-day-p (string< time1 time2)))
  (time-diff 
   (string->dttm 
    (if (not next-day-p)
	(format nil "2007-04-02 Mon ~A:38 +0000 (GMT)" time1)
	(format nil "2007-04-03 Mon ~A:38 +0000 (GMT)" time1)))
   (string->dttm 
    (format nil "2007-04-02 Mon ~A:38 +0000 (GMT)" time2))))

(defmemoize timedifference-old #'equal (time1 time2 &key (next-day-p nil))
  (declare (optimize (speed 3) (safety 0)))
  (round 
   (the single-float
     (*
      (the fixnum (time-diff-short-time time1 time2 :next-day-p next-day-p))
      16.666666))))

(defun mtimedifference-nd (time1 time2)
  (multiple-value-bind (hr1 min1) (floor time1 100)
    (multiple-value-bind (hr2 min2) (floor time2 100)
      (declare (fixnum time1 time2 hr1 hr2 min1 min2)
	       (optimize (speed 3) (safety 0)))
      (- (+ (the fixnum (* (the fixnum (+ (if (< time1 time2) 24 0) hr1)) 60)) min1) (+ (the fixnum (* hr2 60)) min2)))))

(defun split-time (time)
  (declare (simple-string time) (optimize (speed 3) (safety 0)))
  (let* ((len (length time))
	 (pos (if (= len 5)
		  2
		  (if (= len 3)
		      1
		      (position #\: time)))))
    (values (subseq time 0 pos) (subseq time (1+ pos) len))))

(defun timedifference (time1 time2 &key (next-day-p nil))
  (declare (simple-string time1 time2) (optimize (speed 3) (safety 0)))
  (multiple-value-bind (hr1 min1) (split-time time1)
    (multiple-value-bind (hr2 min2) (split-time time2)
      (declare (simple-base-string hr1 min1 hr2 min2))
      (mtimedifference (the fixnum (+ (the fixnum (* (the fixnum (parse-integer hr1)) 100))
				      (the fixnum (parse-integer min1))))
		       (the fixnum (+ (the fixnum (* (the fixnum (parse-integer hr2)) 100))
				      (the fixnum (parse-integer min2))))
		       :next-day-p next-day-p))))

(defun mtimedifference (time1 time2 &key (next-day-p nil))
  (multiple-value-bind (hr1 min1) (floor time1 100)
    (multiple-value-bind (hr2 min2) (floor time2 100)
      (declare (fixnum time1 time2 hr1 hr2 min1 min2)
	       (optimize (speed 3) (safety 0)))
      (if next-day-p
	  (- (+ (the fixnum (* (the fixnum (+ 24 hr1)) 60)) min1) (+ (the fixnum (* hr2 60)) min2))
	  (- (+ (the fixnum (* (the fixnum hr1) 60)) min1) (+ (the fixnum (* hr2 60)) min2))))))

(defun short-time->mins (time)
  (+ (* 60 (parse-integer (subseq time 0 2)))
     (parse-integer (subseq time 3 5))))

(defun short-time->hrmins (time)
  (+ (* 100 (parse-integer (subseq time 0 2)))
     (parse-integer (subseq time 3 5))))

(defun train-time->short (time)
  (multiple-value-bind (hr min) (floor time 100)
    (format nil "~2,'0d:~2,'0d" hr min)))

(defun timenum (time)
  (format nil "~A~A" (subseq time 0 2) (subseq time 3 5)))

(defun extract-url-parameter (parameter-name url)
  (aif (cl-ppcre:all-matches-as-strings (format nil "[?&]~A=[^&]+(&|$)" parameter-name) url)
       (unless (string= "" (car it))
	 (if (string= "&" (subseq (car it) (1- (length (car it))) (length (car it))))
	     (subseq (car it) (+ 2 (length parameter-name)) (1- (length (car it))))
	     (subseq (car it) (+ 2 (length parameter-name)) (length (car it)))))))

(defun format-nr-time (nrTime)
  (if (>= 12 (length nrTime))
      "00:00"
      (format nil "~A:~A" (subseq nrTime 9 11) (subseq nrTime 11 13))))

(defmacro sondesh-file-path (name)
  `(if (probe-file "/home/vb/sondesh/src/database/")
       (format nil "/home/vb/sondesh/src/database/~A" ,name)
       (format nil "/Users/vb/sondesh/src/database/~A" ,name)))

(defun find-duplicates (sequence &key (key #'identity) (test #'eql))
  (labels ((dupes (item seq acc)
	     (if (not seq)
		 acc
		 (progn
		   (when (member (funcall key item) seq :key key :test test)
		     (push item acc)
		     (print item t))
		   (dupes (car seq) (cdr seq) acc)))))
    (dupes (car sequence) (cdr sequence) '())))

(let ((cache-vec (make-array 10 :adjustable t)) diag)
  (defun edit-distance (s1 s2 &key (test #'eql))
    "The edit (Levenshtein) distance between strings
 (actually, arbitrary vectors).
See <http://www.merriampark.com/ld.htm>
<http://www.cut-the-knot.org/do_you_know/Strings.shtml>."
    (let ((l1 (length s1)) (l2 (length s2)))
      (unless (>= (length cache-vec) l1)
	(adjust-array cache-vec l1))
      (setq diag 0)
      (loop :for i :from 0 :below l1 :do (setf (aref cache-vec i) (1+ i)))
      (loop :for j :from 0 :below l2 :and c2 :across s2 :do
	 (loop :for i :from 0 :below l1 :and c1 :across s1
	    :for old = (aref cache-vec i) :do
	    (shiftf diag (aref cache-vec i)
                (min (if (funcall test c1 c2) diag (1+ diag))
                     (1+ (aref cache-vec i))
                     (1+ (if (zerop i) (1+ j) (aref cache-vec (1- i))))))))
      (aref cache-vec (1- l1)))))

(sb-impl::defmacro-mundanely casetest (keyform test &body cases)
  (sb-impl::case-body 'case keyform cases t test nil nil nil))

(defun longer (x y)
  "Test if list x is longer than list y.  More efficient
   than calling length twice."   ; LMH
  (declare (sequence x y))   ; LMH
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun topn (lst n &optional (acc '()))
  (if (> n 0)
      (aif (cdr lst)
	   (topn it (1- n) (cons (car lst) acc))
	   (nreverse (cons (car lst) acc)))
      (nreverse acc)))

;;debug stuff

(defparameter *dbg-ids* nil)
(defparameter *fail* 'fail)

(defun dbg (id format-string args)
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (if args
	(apply #'format *debug-io* format-string args)
	(apply #'format *debug-io* format-string (list "none found")))))

(defun debug-ids (&rest ids)
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug-ids (&rest ids)
  (setf *dbg-ids* (if (null ids) nil
		      (set-difference *dbg-ids* ids))))

(defun debug-indent (id indent format-string &rest args)
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ "  " *debug-io*))
    (apply #'format *debug-io* format-string args)))

(defun stylized-day->caldate (day)
  (cond ((string= (string-upcase day) "TODAY") (dttm->string (now) :format :caldate))
	((string= (string-upcase day) "TOMORROW") (dttm->string (+ (now) 86400) :format :caldate))
	(t (aif (handler-case (string->dttm day)
		  (simple-type-error () nil))
		day
		(dttm->string (now) :format :caldate)))))
(defparameter *whitespace-chars* 
  '(#\space #\tab #\newline #\return #\linefeed #\page))

(defun whitespacep (char)
  (find char *whitespace-chars*))

(defun parse-float (string &key (start 0) end (radix 10) junk-allowed)
  "Converts a substring of STRING, as delimited by START and END, to a 
   floating point number, if possible. START and END default to the 
   beginning and end of the string. RADIX must be between 2 and 36. 
   A floating point number will be returned if the string consists of an
   optional string of spaces and an optional sign, followed by a string
   of digits optionally containing a decimal point, and an optional e or
   E followed by an optionally signed integer. The use of e/E to indicate
   an exponent only works for RADIX = 10. Returns the floating point
   number, if any, and the index for the first character after the number."

  ;; END defaults to the end of the string
  ;; We don't accomplish this by sticking (end (length string)) in the 
  ;; lambda list because I've encountered too many implementations that 
  ;; don't handle such properly. Also, this will work ok if somebody calls
  ;; the function with :end nil.
  (setq end (or end (length string))) 

  ;; Skip over whitespace. If there's nothing but whitespace, signal an error.
  (let ((index (or (position-if-not #'whitespacep string :start start :end end)
                   (if junk-allowed
                       (return-from parse-float (values nil end))
                     (error "No non-whitespace characters in number."))))
        (minusp nil) (decimalp nil) (found-digit nil) 
        (before-decimal 0) (after-decimal 0) (decimal-counter 0)
        (exponent 0)
        (result 0))
    (declare (fixnum index))

    ;; Take care of optional sign.
    (let ((char (char string index)))
      (cond ((char= char #\-)
             (setq minusp t)
             (incf index))
            ((char= char #\+)
             (incf index))))

    (loop
     (when (= index end) (return nil))
     (let* ((char (char string index))
            (weight (digit-char-p char radix)))
       (cond ((and weight (not decimalp))
              ;; A digit before the decimal point
              (setq before-decimal (+ weight (* before-decimal radix))
                    found-digit t))
             ((and weight decimalp)
              ;; A digit after the decimal point
              (setq after-decimal (+ weight (* after-decimal radix))
                    found-digit t)
              (incf decimal-counter))
             ((and (char= char #\.) (not decimalp))
	      ;; The decimal point
              (setq decimalp t))
             ((and (char-equal char #\e) (= radix 10))
	      ;; E is for exponent
              (multiple-value-bind (num idx) 
                  (parse-integer string :start (1+ index) :end end
                                 :radix radix :junk-allowed junk-allowed)
                (setq exponent (or num 0)
                      index idx)
		(when (= index end) (return nil))))
             (junk-allowed (return nil))
             ((whitespacep char)
              (when (position-if-not #'whitespacep string
                                     :start (1+ index) :end end)
                (error "There's junk in this string: ~S." string))
              (return nil))
             (t
              (error "There's junk in this string: ~S." string))))
     (incf index))

    ;; Cobble up the resulting number
    (setq result (float (* (+ before-decimal
                              (* after-decimal 
                                 (expt radix (- decimal-counter))))
                           (expt radix exponent))))

    ;; Return the result
    (values
     (if found-digit
         (if minusp (- result) result)
       (if junk-allowed
           nil
         (error "There's no digits in this string: ~S" string)))
     index)))

(defun all-members (items list &key (test #'eql))
  (remove-if #'not
	     (mapcar
	      #'(lambda (item) (first (member item list :test test)))
	      items)))

(defun remove-adjacent-if-not (predicate sequence &optional (acc '()) (found nil))
  (if (not sequence)
      acc
      (if (funcall predicate (first sequence))
	  (remove-adjacent-if-not predicate (rest sequence) (cons (first sequence) acc) t)
	  (if found
	      acc
	      (remove-adjacent-if-not predicate (rest sequence) acc nil)))))

(defun dates-contained-p (range-start range-end start end)
  (and (>= end range-start)
       (>= range-end start)))

(defun log-error (from to time jour trs)
  (with-open-file (out (sondesh-file-path "errors.log") :direction :output :if-exists :append :if-does-not-exist :create)
    (print (list (station-crs-code from) (station-crs-code to) time jour trs))
    (write (list (station-crs-code from) (station-crs-code to) time jour trs) :stream out)))

(defun combinations (bag)
  (mapcar #'(lambda (e) (cons e (list (remove e bag :count 1 :test #'eq))))
	  bag))

(defun pairs (lst &optional (acc nil))
  (if (null lst)
      acc
      (if (second lst)
	  (pairs (rest lst) (cons (cons (first lst) (second lst)) acc))
	  acc)))
  
