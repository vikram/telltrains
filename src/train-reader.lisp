(in-package :com.sondesh.database)

(defparameter *all-crses*
  (union '("YRK" "WVH" "WOK" "WKM" "WIM" "WIJ" "WFJ" "TUH" "TON" "TBD" "TAU" "SYB" "SWI"
 "SUR" "SUO" "STG" "SRA" "SPT" "SOT" "SNS" "SMK" "SLR" "SLO" "SHT" "SHR" "SBY"
 "SAY" "SAL" "RUG" "RMD" "RGL" "RET" "PYG" "PTH" "PRE" "PMR" "PBO" "OXF" "OKM"
 "NWP" "NUN" "NTN" "NRW" "NMP" "NCL" "MMO" "MIJ" "LWS" "LEI" "LDY" "LCN" "KNG"
 "KMK" "KET" "IPS" "INV" "HUY" "HUL" "HRH" "HOO" "HNH" "HHY" "HFD" "HAZ"
 "HAV" "GUI" "GTW" "GRP" "GRA" "GOO" "GLD" "GFD" "FPK" "FAV" "ESL" "ERL" "EPS"
 "ELY" "DVP" "DON" "DID" "DFD" "DEE" "DAR" "CYP" "CTR" "CRV" "CRS" "CRE" "CPM"
 "COV" "COL" "CLY" "CLJ" "CHD" "CDF" "CBG" "CAR" "BTH" "BTG" "BTB" "BSK" "BON"
 "BNY" "BKJ" "BKG" "BGN" "BFD" "BDM" "BBN" "AHV" "AFK" "ACT" "ABD")
	 (mapcar #'station-crs-code *station-groups*)))

|#
'("YRK" "WVH" "WOK" "WKM" "WIM" "WIJ" "WFJ" "TUH" "TON" "TBD" "TAU" "SYB" "SWI"
 "SUR" "SUO" "STG" "SRA" "SPT" "SOT" "SNS" "SMK" "SLR" "SLO" "SHT" "SHR" "SBY"
 "SAY" "SAL" "RUG" "RMD" "RGL" "RET" "PYG" "PTH" "PRE" "PMR" "PBO" "OXF" "OKM"
 "NWP" "NUN" "NTN" "NRW" "NMP" "NCL" "MMO" "MIJ" "LWS" "LEI" "LDY" "LCN" "KNG"
 "KMK" "KET" "IPS" "INV" "HUY" "HUL" "HRH" "HOO" "HNH" "HHY" "HFD" "HAZ" "HAY"
 "HAV" "GUI" "GTW" "GRP" "GRA" "GOO" "GLD" "GFD" "RUN" "G71" "RUE" "G70" "HNX"
 "LPY" "HTH" "HDG" "SYA" "G69" "WML" "MIA" "MSR" "G68" "RAM" "HLY" "G67" "MTH"
 "SST" "G66" "SSD" "BAR" "CNF" "G64" "LAN" "SMR" "G63" "SGB" "G62" "BLY" "MKC"
 "G61" "FOD" "LIT" "ELD" "G60" "RDH" "G58" "BAN" "KGS" "HKC" "G57" "HAC"
 "WEH" "BHO" "WHC" "G55" "WMW" "SVS" "STO" "G54" "TOM" "G53" "HLD" "LPR" "G52"
 "LBO" "BWS" "SIL" "G51" "KDY" "MNC" "GLT" "G50" "INK" "NQU" "WXC" "G49" "WRX"
 "G48" "SCA" "SEM" "NCT" "G47" "NNG" "TRO" "G46" "WSB" "BAW" "FNB" "G45" "FNN"
 "FKG" "G44" "FKK" "CMO" "ATB" "BEE" "G43" "NOT" "EAG" "G42" "MBR" "TBY" "DVY"
 "G41" "MCN" "DHN" "G40" "HUD" "MIR" "G39" "HFX" "SOW" "GWN" "LLE" "G38" "SWA"
 "GDP" "G37" "RMF" "YVJ" "G36" "YVP" "DTW" "WOF" "G35" "WOS" "WGN" "G34" "WGW"
 "UPW" "G33" "WEY" "WHD" "G32" "WHP" "WBQ" "G31" "WAC" "WKK" "G30" "WKF" "NRB"
 "SRS" "STE" "SRC" "G29" "SRH" "TTH" "HIT" "G28" "SVG" "SOC" "SOE" "G27" "SOV"
 "MBK" "RDB" "SOU" "G26" "SOA" "SDN" "SWG" "DOR" "G25" "SHF" "MHS" "RDG" "G24"
 "RDW" "BDH" "CSA" "FRM" "FTN" "HLS" "PTC" "PMH" "G23" "PMS" "CFD" "PFR" "PFM"
 "G22" "POT" "GLH" "DPT" "DOC" "KEY" "G21" "PLY" "DGT" "MCO" "MAN" "G20" "MCV"
 "SFD" "SLD" "MDB" "MDE" "G19" "MDW" "EDG" "BKQ" "LVJ" "LVC" "LIV" "MRF" "SDL"
 "G18" "LVS" "G17" "LEW" "NWX" "SAJ" "CRG" "EGF" "GRF" "G16" "LDS" "MIK" "G15"
 "HGS" "SLQ" "CNM" "G14" "GCR" "GLC" "GLQ" "SPR" "G13" "ASF" "EXC" "EXD" "G12"
 "EXT" "SJP" "DAM" "G11" "EDB" "HYM" "SGL" "DKG" "DPD" "G10" "DKT" "GOM" "BLP"
 "G09" "DBY" "DFI" "LGE" "PEA" "SPO" "G08" "ECR" "SCY" "WCY" "G07" "CTM" "GLM"
 "RTR" "SOO" "BKL" "G06" "BMS" "ORP" "PET" "SRT" "BPW" "G05" "BRI" "LWH" "SRD"
 "FIT" "G04" "BTN" "HOV" "BDQ" "G03" "BDI" "AST" "BMO" "BHM" "G02" "BSW" "DUD"
 "BET" "BFR" "CST" "CHX" "EUS" "FST" "KGX" "KCM" "LST" "LBG" "MYB" "ZMG" "OLD"
 "PAD" "STP" "VIC" "WAT" "G01" "WAE" "CTK" "FPK" "FAV" "ESL" "ERL" "EPS" "ELY"
 "DVP" "DON" "DID" "DFD" "DEE" "DAR" "CYP" "CTR" "CRV" "CRS" "CRE" "CPM" "COV"
 "COL" "CLY" "CLJ" "CHD" "CDF" "CBG" "CAR" "BTH" "BTG" "BTB" "BSK" "BON" "BNY"
 "BKJ" "BKG" "BGN" "BFD" "BDM" "BBN" "AHV" "AFK" "ACT" "ABD"))
#|

(defparameter *fixed-modes*
  '((1 . "METRO")
    (2 . "BUS")
    (3 . "WALK")
    (4 . "TRANSFER")
    (5 . "TUBE")))

(defparameter *starter-train-id* 1000)
(defparameter *max-train-id* *starter-train-id*)

(defparameter *hubs* nil)

(defparameter *hubs-array* (make-array '(345) :element-type 'string))

(let ((hub (make-hash-table :test 'equal)))
    (dolist (crs-pair 
'(("ABD" . 344) ("ACT" . 343) ("AFK" . 342) ("AHV" . 341) ("BBN" . 340)
 ("BDM" . 339) ("BFD" . 338) ("BGN" . 337) ("BKG" . 336) ("BKJ" . 335)
 ("BNY" . 334) ("BON" . 333) ("BSK" . 332) ("BTB" . 331) ("BTG" . 330)
 ("BTH" . 329) ("CAR" . 328) ("CBG" . 327) ("CDF" . 326) ("CHD" . 325)
 ("CLJ" . 324) ("CLY" . 323) ("COL" . 322) ("COV" . 321) ("CPM" . 320)
 ("CRE" . 319) ("CRS" . 318) ("CRV" . 317) ("CTR" . 316) ("CYP" . 315)
 ("DAR" . 314) ("DEE" . 313) ("DFD" . 312) ("DID" . 311) ("DON" . 310)
 ("DVP" . 309) ("ELY" . 308) ("EPS" . 307) ("ERL" . 306) ("ESL" . 305)
 ("FAV" . 304) ("FPK" . 303) ("CTK" . 302) ("WAE" . 301) ("WAT" . 300)
 ("VIC" . 299) ("STP" . 298) ("PAD" . 297) ("OLD" . 296) ("ZMG" . 295)
 ("MYB" . 294) ("LBG" . 293) ("LST" . 292) ("KCM" . 291) ("KGX" . 290)
 ("FST" . 289) ("EUS" . 288) ("CHX" . 287) ("CST" . 286) ("BFR" . 285)
 ("BET" . 284) ("DUD" . 283) ("BSW" . 282) ("BHM" . 281) ("BMO" . 280)
 ("AST" . 279) ("BDI" . 278) ("BDQ" . 277) ("HOV" . 276) ("BTN" . 275)
 ("FIT" . 274) ("SRD" . 273) ("LWH" . 272) ("BRI" . 271) ("BPW" . 270)
 ("SRT" . 269) ("PET" . 268) ("ORP" . 267) ("BMS" . 266) ("BKL" . 265)
 ("SOO" . 264) ("RTR" . 263) ("GLM" . 262) ("CTM" . 261) ("WCY" . 260)
 ("SCY" . 259) ("ECR" . 258) ("SPO" . 257) ("PEA" . 256) ("LGE" . 255)
 ("DFI" . 254) ("DBY" . 253) ("BLP" . 252) ("GOM" . 251) ("DKT" . 250)
 ("DPD" . 249) ("DKG" . 248) ("SGL" . 247) ("HYM" . 246) ("EDB" . 245)
 ("DAM" . 244) ("SJP" . 243) ("EXT" . 242) ("EXD" . 241) ("EXC" . 240)
 ("ASF" . 239) ("SPR" . 238) ("GLQ" . 237) ("GLC" . 236) ("GCR" . 235)
 ("CNM" . 234) ("SLQ" . 233) ("HGS" . 232) ("MIK" . 231) ("LDS" . 230)
 ("GRF" . 229) ("EGF" . 228) ("CRG" . 227) ("SAJ" . 226) ("NWX" . 225)
 ("LEW" . 224) ("LVS" . 223) ("SDL" . 222) ("MRF" . 221) ("LIV" . 220)
 ("LVC" . 219) ("LVJ" . 218) ("BKQ" . 217) ("EDG" . 216) ("MDW" . 215)
 ("MDE" . 214) ("MDB" . 213) ("SLD" . 212) ("SFD" . 211) ("MCV" . 210)
 ("MAN" . 209) ("MCO" . 208) ("DGT" . 207) ("PLY" . 206) ("KEY" . 205)
 ("DOC" . 204) ("DPT" . 203) ("GLH" . 202) ("POT" . 201) ("PFM" . 200)
 ("PFR" . 199) ("CFD" . 198) ("PMS" . 197) ("PMH" . 196) ("PTC" . 195)
 ("HLS" . 194) ("FTN" . 193) ("FRM" . 192) ("CSA" . 191) ("BDH" . 190)
 ("RDW" . 189) ("RDG" . 188) ("MHS" . 187) ("SHF" . 186) ("DOR" . 185)
 ("SWG" . 184) ("SDN" . 183) ("SOA" . 182) ("SOU" . 181) ("RDB" . 180)
 ("MBK" . 179) ("SOV" . 178) ("SOE" . 177) ("SOC" . 176) ("SVG" . 175)
 ("HIT" . 174) ("TTH" . 173) ("SRH" . 172) ("SRC" . 171) ("STE" . 170)
 ("SRS" . 169) ("NRB" . 168) ("WKF" . 167) ("WKK" . 166) ("WAC" . 165)
 ("WBQ" . 164) ("WHP" . 163) ("WHD" . 162) ("WEY" . 161) ("UPW" . 160)
 ("WGW" . 159) ("WGN" . 158) ("WOS" . 157) ("WOF" . 156) ("DTW" . 155)
 ("YVP" . 154) ("YVJ" . 153) ("RMF" . 152) ("GDP" . 151) ("SWA" . 150)
 ("LLE" . 149) ("GWN" . 148) ("SOW" . 147) ("HFX" . 146) ("MIR" . 145)
 ("HUD" . 144) ("DHN" . 143) ("MCN" . 142) ("DVY" . 141) ("TBY" . 140)
 ("MBR" . 139) ("EAG" . 138) ("NOT" . 137) ("BEE" . 136) ("ATB" . 135)
 ("CMO" . 134) ("FKK" . 133) ("FKG" . 132) ("FNN" . 131) ("FNB" . 130)
 ("BAW" . 129) ("WSB" . 128) ("TRO" . 127) ("NNG" . 126) ("NCT" . 125)
 ("SEM" . 124) ("SCA" . 123) ("WRX" . 122) ("WXC" . 121) ("NQU" . 120)
 ("INK" . 119) ("GLT" . 118) ("MNC" . 117) ("KDY" . 116) ("SIL" . 115)
 ("BWS" . 114) ("LBO" . 113) ("LPR" . 112) ("HLD" . 111) ("TOM" . 110)
 ("STO" . 109) ("SVS" . 108) ("WMW" . 107) ("WHC" . 106) ("BHO" . 105)
 ("WEH" . 104) ("HAC" . 103) ("HKC" . 102) ("KGS" . 101) ("BAN" . 100)
 ("RDH" . 99) ("ELD" . 98) ("LIT" . 97) ("FOD" . 96) ("MKC" . 95) ("BLY" . 94)
 ("SGB" . 93) ("SMR" . 92) ("LAN" . 91) ("CNF" . 90) ("BAR" . 89) ("SSD" . 88)
 ("SST" . 87) ("MTH" . 86) ("HLY" . 85) ("RAM" . 84) ("MSR" . 83) ("MIA" . 82)
 ("WML" . 81) ("SYA" . 80) ("HDG" . 79) ("HTH" . 78) ("LPY" . 77) ("HNX" . 76)
 ("RUE" . 75) ("RUN" . 74) ("GFD" . 73) ("GLD" . 72) ("GOO" . 71) ("GRA" . 70)
 ("GRP" . 69) ("GTW" . 68) ("GUI" . 67) ("HAV" . 66) ("HAY" . 65) ("HAZ" . 64)
 ("HFD" . 63) ("HHY" . 62) ("HNH" . 61) ("HOO" . 60) ("HRH" . 59) ("HUL" . 58)
 ("HUY" . 57) ("INV" . 56) ("IPS" . 55) ("KET" . 54) ("KMK" . 53) ("KNG" . 52)
 ("LCN" . 51) ("LDY" . 50) ("LEI" . 49) ("LWS" . 48) ("MIJ" . 47) ("MMO" . 46)
 ("NCL" . 45) ("NMP" . 44) ("NRW" . 43) ("NTN" . 42) ("NUN" . 41) ("NWP" . 40)
 ("OKM" . 39) ("OXF" . 38) ("PBO" . 37) ("PMR" . 36) ("PRE" . 35) ("PTH" . 34)
 ("PYG" . 33) ("RET" . 32) ("RGL" . 31) ("RMD" . 30) ("RUG" . 29) ("SAL" . 28)
 ("SAY" . 27) ("SBY" . 26) ("SHR" . 25) ("SHT" . 24) ("SLO" . 23) ("SLR" . 22)
 ("SMK" . 21) ("SNS" . 20) ("SOT" . 19) ("SPT" . 18) ("SRA" . 17) ("STG" . 16)
 ("SUO" . 15) ("SUR" . 14) ("SWI" . 13) ("SYB" . 12) ("TAU" . 11) ("TBD" . 10)
 ("TON" . 9) ("TUH" . 8) ("WFJ" . 7) ("WIJ" . 6) ("WIM" . 5) ("WKM" . 4)
 ("WOK" . 3) ("WVH" . 2) ("YRK" . 1)))
      (setf (gethash (car crs-pair) hub) (cdr crs-pair))
      (setf (svref *hubs-array* (cdr crs-pair)) (car crs-pair)))
    (setf *hubs* hub))

(defparameter *average-time-stops* (make-hash-table :test 'equal))
(defparameter *station-links* nil)
(defparameter *permitted-routes* nil)
(defparameter *linked-stations* nil)
(defparameter *permitted-route-trains* (make-hash-table :test 'equal :size 38000 :rehash-size 1.1))
(defparameter *tiploc->id* (make-hash-table :test 'equal :size 8000 :rehash-size 1.1))
(defparameter *id->tiploc* (make-hash-table :test 'equal :size 8000 :rehash-size 1.1))
(defparameter *schedules* (make-hash-table :test 'equal :size 8000 :rehash-size 1.1))
(defparameter *trains* (make-hash-table :test 'equal :size 500000 :rehash-size 1.01))
(defparameter *train-uids* (make-hash-table :test 'equal :size 200000 :rehash-size 1.01))
(defparameter *uid-trains* (make-hash-table :test 'equal :size 200000 :rehash-size 1.01))
(defparameter *connectivity* (make-hash-table :test 'equal :size 150000 :rehash-size 1.1))
(defparameter *links* 0)
(defparameter *routeing-tiplocs* (make-hash-table))

(defparameter *neighbours* (make-array '(345) :element-type 'hash-table))
(defparameter *neighbours-consolidate* (make-array '(345) :element-type 'hash-table))
(defparameter *fixed-neighbours* (make-array '(345) :element-type 'hash-table))
(defparameter *fixed-neighbours-consolidate* (make-array '(345) :element-type 'hash-table))

(defparameter *routes* (make-hash-table :size 32843 :test 'eql))

(defun route-key (from-crs to-crs day)
  (make-logic-symbol (concatenate 'string "k" (write-to-string day) from-crs to-crs)))

(defun write-routes ()
  (let ((results '()))
    (with-open-file (out (sondesh-file-path "routes") :direction :output :if-exists :supersede)
      (maphash #'(lambda (key value)
		   (push
		    (list (subseq (symbol-name key) 2 5)
			  (subseq (symbol-name key) 5 8)
			  (parse-integer (subseq (symbol-name key) 1 2))
			  value) 
		    results))
	       *routes*)
      (write results :stream out))))

(defun load-routes ()
  (with-open-file (in (sondesh-file-path "routes"))
    (dolist (route (read in))
      (setf (gethash (route-key (first route)
				(second route)
				(third route))
		     *routes*)
	    (fourth route)))))

(defun read-fixed ()
  (with-open-file (in (sondesh-file-path "data/RJTTF851.FLF"))
    (let ((results '()))
      (do ((line (read-line in nil)
		 (read-line in nil)))
	  ((null line))
      (when (and line (string= "AD" (subseq line 0 2)))
	(let ((mode (split-sequence:split-sequence #\space (subseq line 17)))
	      (mins (first (split-sequence:split-sequence 
			    #\space 
			    (first (cl-ppcre:all-matches-as-strings "[0-9]+ MINUTES"
								    (subseq line 17)))))))
	  (push 
	   (list (first mode) (third mode) (fifth mode) mins)
	   results))))
      (dolist (fixed results)
	(when (and (gethash (third fixed) *hubs*) (gethash (second fixed) *hubs*))
	  (setf (gethash (gethash (third fixed) *hubs*)
			 (svref *fixed-neighbours* (gethash (second fixed) *hubs*)))
		(cons (first fixed) (parse-integer (fourth fixed))))
	  (setf (gethash (gethash (second fixed) *hubs*)
			 (svref *fixed-neighbours* (gethash (third fixed) *hubs*)))
		(cons (first fixed) (parse-integer (fourth fixed)))))))))

(defun fixed-mode->index (mode)
  (car (find mode *fixed-modes* :key #'cdr :test #'string=)))

(defun fixed-index->mode (mode)
  (cdr (find mode *fixed-modes* :key #'car :test #'=)))

(defun push-neighbour (from to from-stop to-stop)
  (push (list from-stop to-stop)
	(gethash to (svref *neighbours* from))))

(defstruct (connectivity (:type vector))
  total-time number minimum maximum)

(defun tiploc->index (tiploc)
  (let ((crs (gethash tiploc *tiploc->crs*)))
    (aif (gethash crs *hubs*)
	 it
	 (gethash (station-group-crs-code (find-station-by-code crs)) *hubs*))))

(defun CONVERT-INTEGER-TO-BIT-STRING (integer)
  (declare (fixnum integer)
	   (optimize (speed 3) (safety 0)))
   "Convert the integer to a bit string"
   (if (< integer 1) '(0)
   (loop with number = integer
       for 2-power = (biggest-2-divisor integer)
           then (/ 2-power 2)
       until (< 2-power 1)
       collect (if (< number 2-power) 0
                   (and (setf number (- number 2-power)) 1))
       into bits
      finally (return bits))))
 
(defun padd-bits (bits pad-upto)
  (declare (list bits) (fixnum pad-upto)
	   (optimize (speed 3) (safety 0)))
  (padd-these-bits (length bits) bits pad-upto))

(defun padd-these-bits (num bits pad-upto)
  (declare (list bits) (fixnum pad-upto num)
	   (optimize (speed 3) (safety 0)))
  (if (< num pad-upto)
      (padd-these-bits (incf num) (cons 0 bits) pad-upto)
      bits))

(defun BIGGEST-2-DIVISOR (integer)
  (declare (fixnum integer)
	   (optimize (speed 3) (safety 0)))
   "Find the larger power of 2 in the integer"
   (loop for 2-power = 1 then (the fixnum (+ 2-power 2-power))
       until (> 2-power integer)
       finally (return (/ 2-power 2))))
 
(defun encode-train-stop (trainstop dept arrt trainid)
  (let ((doesnt-stop (if (string= trainstop "P")
                        1 0))
       (arrives (if (or (string= trainstop "T") (string= trainstop "I"))
                    1 0))
       (departs (if (or (string= trainstop "O") (string= trainstop "I"))
                    1 0)))
     (convert-bit-string-to-integer (append (padd-bits (convert-integer-to-bit-string trainid) 19)
                                          (list doesnt-stop arrives departs) 
                                          (padd-bits (convert-integer-to-bit-string dept) 12)
					  (padd-bits (convert-integer-to-bit-string arrt) 12)))))
 
(defun decode-train-stop (num)
  (let ((bits (padd-bits (convert-integer-to-bit-string num) 46)))
     (list 
      (convert-bit-string-to-integer (subseq bits 0 19))
      (cond ((= 1 (car (subseq bits 19 20))) "P")
          ((and (= 1 (car (subseq bits 20 21))) (= 1 (car (subseq bits 21 22)))) "I")
          ((= 1 (car (subseq bits 20 21))) "T")
          (t "O"))
      (convert-bit-string-to-integer (subseq bits 22 34))
      (convert-bit-string-to-integer (subseq bits 34 46)))))
 
(defun CONVERT-BIT-STRING-TO-INTEGER (bit-string)
  (declare (list bit-string)
	   (optimize (speed 3) (safety 0)))
   "Convert the bit string to an integer using powers of 2"
   (loop for bit in (reverse (copy-list bit-string))
       for 2-power = 1 then (+ 2-power 2-power)
       summing (the fixnum (if (= bit 1) 2-power 0))))

(defun encode-simple-train (id start end days)
  (+ (* id 1000000000000000)
     (* (convert-bit-string-to-integer days)
	1000000000000)
     (* start 1000000)
     end))

(defun encode-train (train id)
  (encode-simple-train id 
		       (parse-integer (nth 3 train)) 
		       (parse-integer (nth 4 train))
		       (cons (if (and (nth 6 train) (not (string= " " (nth 6 train)))) 1 0)
			     (map 'list #'(lambda (c) (- (char-code c) 48)) (nth 5 train)))))

(defun decode-train (num)
  (list 
   (floor num 1000000000000000)
   (padd-bits (convert-integer-to-bit-string (rem (floor num 1000000000000) 1000)) 8)
   (rem (floor num 1000000) 1000000)
   (rem num 1000000)))  

(defun simple-encode-train-stop (trainstop tiploc-id dept arrt trainid)
  (let ((doesnt-stop (if (string= trainstop "P")
			 1 0))
	(arrives (if (or (string= trainstop "T") (string= trainstop "I"))
		     2 0))
	(departs (if (or (string= trainstop "O") (string= trainstop "I"))
		     4 0)))
    (+ (* 1000000000 1000000 tiploc-id)
       (* trainid 1000000000)
       (* (+ doesnt-stop arrives departs) 100000000)
       (* dept 10000)
       arrt)))

(defun simple-encode-train-stop2 (trainstop tiploc-id dept arrt trainid)
  (let ((doesnt-stop (if (string= trainstop "P")
			 1 0))
	(arrives (if (or (string= trainstop "T") (string= trainstop "I"))
		     2 0))
	(departs (if (or (string= trainstop "O") (string= trainstop "I"))
		     4 0)))
    (list trainid (+ (* 10000 arrt) dept) (+ (* 10 tiploc-id) (+ doesnt-stop arrives departs)))))

(defun simple-decode-train-stop2 (lst)
  (let ((trainid (first lst))
	(num2 (second lst))
	(num3 (third lst)))
    (declare (fixnum trainid num2 num3) (sequence lst) (optimize (speed 3) (safety 0)))
    (multiple-value-bind (arrt dept) (floor num2 10000)
      (multiple-value-bind (tiploc bit) (floor num3 10)
	(list trainid
	      (case bit
		(1 "P")
		(6 "I")
		(2 "T")
		(4 "O"))
	      dept
	      arrt
	      tiploc)))))

(defun simple-decode-train-stop (num)
  (declare (bignum num) (optimize (speed 3) (safety 0)))
  (multiple-value-bind (a1 arrt) (floor num 10000)
    (multiple-value-bind (a2 dept) (floor a1 10000)
      (multiple-value-bind (a3 bit) (floor a2 10)
	(let ((trainstop (padd-bits (convert-integer-to-bit-string bit) 3)))
	  (declare (list trainstop))
	  (multiple-value-bind (tiploc trainid) (floor a3 1000000)
	    (list 
	     trainid
	     (cond ((= (the integer (third trainstop)) 1) "P")
		   ((and (= (the integer (first trainstop)) 1) (= (the integer (second trainstop)) 1)) "I")
		   ((and (= (the integer (first trainstop)) 1) (= (the integer (second trainstop)) 0)) "O")
		   (t "T"))
	     dept
	     arrt
	     tiploc)))))))

(defun simple-decode-train-stop-slow (num)
  (let ((trainstop 
	 (padd-bits
	  (convert-integer-to-bit-string 
	   (rem (floor num 100000000) 10)) 3)))
  (declare (bignum num) (list trainstop)
	   (optimize (speed 3) (safety 0)))
  (list 
   (rem (floor num 1000000000) 1000000)
   (cond ((= (the integer (third trainstop)) 1) "P")
	 ((and (= (the integer (first trainstop)) 1) (= (the integer (second trainstop)) 1)) "I")
	 ((and (= (the integer (first trainstop)) 1) (= (the integer (second trainstop)) 0)) "O")
	 (t "T"))
   (rem (floor num 10000) 10000)
   (rem num 10000)
   (floor num 1000000000000000))))

(defun yymmdd->date (date)
  (clsql::make-date :year (parse-integer (format nil "20~A" (subseq date 0 2)))
		    :month (parse-integer (subseq date 2 4)) 
		    :day (parse-integer (subseq date 4 6))))

(defmemoize 
    yymmdd->simple-date #'equal (date)
  (make-date :ye (parse-integer (format nil "20~A" (subseq date 0 2)))
	     :mo (parse-integer (subseq date 2 4)) 
	     :da (parse-integer (subseq date 4 6))))

(let ((id 0))
  (defun tiploc->id (tiploc)
    (aif (gethash tiploc *tiploc->id*)
	 it
	 (progn
	   (setf (gethash tiploc *tiploc->id*) (incf id))
	   (setf (gethash id *id->tiploc*) tiploc)
	   id))))

(defun id->tiploc (id)
  (gethash id *id->tiploc*))

;;file defns

(field-setup (*station-link-reader* "data/RJRG0121.RGD" :comma-separated-p t)
	     ("^[A-Z]" short-path-link ((from-crs-grp-code 3)
					(comma 1 :ignore)
					(to-crs-grp-code 4)
					(comma 1 :ignore)
					(distance 4))))

(field-setup (*permitted-route-reader* "data/RJRG0121.RGR" :comma-separated-p t)
	     ("^[A-Z]" permitted-route ((from 3)
					(comma 1 :ignore)
					(to 3)
					(comma 1 :ignore)
					(map-path))))

(field-setup (*linked-route-reader* "data/RJRG0121.RGL" :comma-separated-p t)
	     ("^[A-Z]" linked-route ((from 3)
				     (comma 1 :ignore)
				     (to 3)
				     (comma 1 :ignore)
				     (map 2))))

(defparameter *basic-schedule-record-defn* '((record-identity 2 :matches "BS")
					     (transaction-type 1)
					     (train-uid 6)
					     (date-runs-from 6)
					     (date-runs-to 6)
					     (days-runs 7)
					     (bank-holiday-runs 1)
					     (train-status 1)
					     (train-category 2)
					     (train-identity 4)
					     (headcode 4)
					     (course-indicator 1)
					     (profit-centre 8)
					     (business-sector 1)
					     (power-type 3)
					     (timing-load 4)
					     (speed 3)
					     (operation-chars 6)
					     (train-class 1)
					     (sleepers 1)
					     (reservations 1)
					     (connect-indicator 1)
					     (catering-code 4)
					     (service-branding 4)
					     (spare 1 :ignore)
					     (stp-indicator 1)))

(defparameter *basic-schedule-extra-details-record-defn* '((record-identity 2 :matches "BX")
							   (traction-class 4)
							   (uic-code 5)
							   (atoc-code 2)
							   (applicable-timetable-code 1)
							   (retain-train-id 8)
							   (source 1)
							   (spare 57 :ignore)))

(defparameter *origin-station-record-defn* '((record-identity 2 :matches "LO")
					     (tiploc-code 7)
					     (location-suffix 1)
					     (schedule-dept-time 5)
					     (public-dept-time 4)
					     (platform 3)
					     (line 3)
					     (engg-allowance 2)
					     (pathing-allowance 2)
					     (activity 12)
					     (performance-allowance 2)
					     (spare 37 :ignore)))

(defparameter *intermediate-station-record-defn* '((record-identity 2 :matches "LI")
						   (tiploc-code 7)
						   (location-suffix 1)
						   (scheduled-arr-time 5)
						   (scheduled-dept-time 5)
						   (schedule-pass 5)
						   (public-arr-time 4)
						   (public-dept-time 4)
						   (platform 3)
						   (line 3)
						   (path 3)
						   (activity 12)
						   (engg-allowance 2)
						   (pathing-allowance 2)
						   (performance-allowance 2)
						   (spare 20 :ignore)))

(defparameter *terminating-station-record-defn* '((record-identity 2 :matches "LO")
						  (tiploc-code 7)
						  (location-suffix 1)
						  (schedule-arr-time 5)
						  (public-arr-time 4)
						  (platform 3)
						  (path 3)
						  (activity 12)
						  (spare 12 :ignore)))

;;readers

(defun add-train->schedules (train id stops)
  (clsql:update-records-from-instance 
   (make-instance 'schedules 
		  :id id
		  :train-uid (nth 2 train)
		  :date-runs-from (yymmdd->date (nth 3 train))
		  :date-runs-to (yymmdd->date (nth 4 train))
		  :days-runs (nth 5 train)
		  :bank-holiday-runs (nth 6 train)
		  :train-status (nth 7 train)
		  :train-category (nth 8 train)
		  :train-identity (nth 9 train)
		  :headcode (nth 10 train)
		  :course-indicator (nth 11 train)
		  :profit-centre (nth 12 train)
		  :business-sector (nth 13 train)
		  :power-type (nth 14 train)
		  :timing-load (nth 15 train)
		  :speed (nth 16 train)
		  :operation-chars (nth 17 train)
		  :train-class (nth 18 train)
		  :sleepers (nth 19 train)
		  :reservations (nth 20 train)
		  :connect-indicator (nth 21 train)
		  :catering-code (nth 22 train)
		  :service-branding (nth 23 train)
		  :stp-indicator (nth 24 train)
		  :stops (join-with (mapcar #'(lambda (s) (concatenate 'string (car s) "," 
								       (format nil "~S" (cdr s)))) stops) ","))))

(defun 1-yymmdd (yymmdd)
  (add-yymmdd yymmdd (* -1 24 3600)))

(defun add-yymmdd (yymmdd seconds)
  (rem (parse-integer (dttm->string (+ (date2time (yymmdd->simple-date (format nil "~6,'0d" yymmdd))) seconds) :format :yymmdd)) 1000000))

(defun 1+yymmdd (yymmdd)
  (ADD-YYMMDD yymmdd (* 24 3600)))

(defun any-train-days-p (start end days)
  (multiple-value-bind (se mi ho da mo ye dd dst1 tz1)
      (decode-universal-time (date2time (yymmdd->simple-date (format nil "~6,'0d" start))) 0)
    (let ((num-days (1+ (floor (- (date2time (yymmdd->simple-date (format nil "~6,'0d" end)))
			       (date2time (yymmdd->simple-date (format nil "~6,'0d" start))))
			    (* 24 3600)))))
      (if (>= num-days 7)
	  (return-from any-train-days-p num-days)
	  (dotimes (i num-days)
	    (when (= (nth (mod (+ dd i 1) 7) days) 1)
	      (return-from any-train-days-p t))))
      nil)))

(defun train-in-days (train start-date end-date)
  (let ((start (parse-integer (nth 3 train)))
	(end (parse-integer (nth 4 train)))
	(days (cons (if (and (nth 6 train) (not (string= " " (nth 6 train)))) 1 0)
		    (map 'list #'(lambda (c) (- (char-code c) 48)) (nth 5 train))))
	(sd (parse-integer (dttm->string (string->dttm start-date) :format :shortyymmdd)))
	(ed (parse-integer (dttm->string (string->dttm end-date) :format :shortyymmdd))))
    (dates-contained-p sd ed start end)))
    
(defun next-two-days-p (train &key (date (dttm->string (now))))
  (let* ((now (string->dttm date))
	 (today (parse-integer (dttm->string now :format :shortyymmdd)))
	 (dayafter (parse-integer (dttm->string (+ 86400 86400 now) :format :shortyymmdd)))
	 (start (parse-integer (nth 3 train)))
	 (end (parse-integer (nth 4 train)))
	 (days (cons (if (and (nth 6 train) (not (string= " " (nth 6 train)))) 1 0)
		     (map 'list #'(lambda (c) (- (char-code c) 48)) (nth 5 train)))))
    (and (>= end today)
	 (>= dayafter start)
	 (days-match-p days now))))

(defun days-match-p (days now)
  (let ((today (weekday :time now))
	(tomorrow (weekday :time (+ 86400 now)))
	(dayafter (weekday :time (+ 86400 86400 now))))
    (or (= 1 (nth 0 days))
	(= 1 (nth (1+ today) days))
	(= 1 (nth (1+ tomorrow) days))
	(= 1 (nth (1+ dayafter) days)))))

(defun save (train-id train stops)
  (let ((s (nreverse stops))
	(stp (cond ((string= "P" (nth 24 train)) 3)
		   ((string= "O" (nth 24 train)) 1)
		   ((string= "C" (nth 24 train)) 2)
		   (t 0)))
	(encoded (encode-train train train-id))
	(train-uid (third train)))
    (setf (gethash train-id *trains*) (cons encoded s))
    (setf (gethash train-uid *train-uids*) (cons (+ (* 10 encoded) stp)
						 (gethash train-uid *train-uids*)))
    (setf (gethash train-id *uid-trains*) train-uid)
    (stops-connectivity s)))

(defun read-original (line)
  (let ((orig (read-formated-line *origin-station-record-defn* line)))
    (if (not (zerop (parse-integer (fifth orig))))
	(list "O" (second orig) (parse-integer (fifth orig)) 0)
	(list "O" (second orig) (parse-integer (fourth orig) :junk-allowed t) 0))))

(defun read-int (line)
  (let* ((int (read-formated-line *intermediate-station-record-defn* line))
	 (arrt (parse-integer (eighth int)))
	 (dept (parse-integer (seventh int))))
    (if (or (string/= "     " (sixth int)) (zerop arrt) (zerop dept))
	(list "P" (second int) 0 0)
	(list "I" (second int) 
	      (if (and arrt (not (zerop arrt))) arrt dept)
	      (if (and dept (not (zerop dept))) dept arrt)))))

(defun read-term (line)
  (let ((dest (read-formated-line *terminating-station-record-defn* line)))
    (if (not (zerop (parse-integer (fifth dest))))
	(list "T" (second dest) 0 (parse-integer (fifth dest)))
	(list "T" (second dest) 0 (parse-integer (fourth dest) :junk-allowed t)))))

(defun read-simple-trains (&key (just-read-uid nil) (only-routeing-p nil) (only-next-two-days-p nil) (start-date nil) (end-date nil))
  (let ((started nil)
	(train nil)
	(stops '())
	(tiploc-stops '())
	(how-many-stops 0)
	(i 0))
    (setf *max-train-id* *starter-train-id*)
    (setf *links* 0)
    (with-open-file (in (sondesh-file-path "data/RJTTF851.MCA"))
      (do ((line (read-line in nil)
		 (read-line in nil)))
	  ((null line))
	(when line
	  (if (string= "BS" (subseq line 0 2))
	      (progn
		(when started
		  (save *max-train-id* train stops))
		(setf train (read-formated-line *BASIC-SCHEDULE-RECORD-DEFN* line))
		(setf started t)	  
		(if (and (or (not just-read-uid) (and just-read-uid (string= (third train) just-read-uid)))
			 (or (and start-date end-date (train-in-days train start-date end-date))
			     (not only-next-two-days-p)
			     (next-two-days-p train :date only-next-two-days-p)))
		    (progn
		      (dbg :read ";;train-id :~A" (list train))
		      (incf *max-train-id*)
		      (setf how-many-stops 0)
		      (setf stops '())
		      (setf tiploc-stops '())
		      (print (incf i) t))
		    (progn
		      (setf started nil)
		      (setf train nil))))
	      (when started
		(let ((station (casetest (subseq line 0 2) string=
				 ("LO" (read-original line))
				 ("LI" (read-int line))
				 ("LT" (progn
					 (setf started nil)
					 (read-term line))))))
		  (when station
		    (if only-routeing-p
			(when (tiploc->index (second station))
			  (let* ((tiploc-id (tiploc->index (second station)))
				 (stop (simple-encode-train-stop2 (first station) tiploc-id (third station) (fourth station) *max-train-id*)))
			    (unless (string= (first station) "P")
			      (incf how-many-stops))
			    (push (cons tiploc-id stop) tiploc-stops)
			    (push stop stops)))
			(let* ((tiploc-id (tiploc->id (second station)))
			       (stop (simple-encode-train-stop2 (first station) tiploc-id (third station) (fourth station) *max-train-id*)))
			  (unless (string= (first station) "P")
			    (incf how-many-stops))
			  (push (cons tiploc-id stop) tiploc-stops)
			  (push stop stops)))
		    (unless started
		      (print (cons *max-train-id* how-many-stops) t)
		      (when (or (not only-routeing-p) (and only-routeing-p (<= 2 how-many-stops)))
			(let ((previous nil))
			  (dolist (pair tiploc-stops)
			    (if (not only-routeing-p)
				(push-stop (car pair) (cdr pair))
				(progn
				  (when (not (zerop (second (cdr pair))))
				    (when (and previous (not (zerop (second (cdr previous)))))
				      (push-neighbour (car pair) (car previous) (cdr pair) (cdr previous)))
				    (setf previous pair))))))
			(save *max-train-id* train stops))))))))))))

(defun routeing-tiploc-p (tiploc)
  (gethash (make-logic-symbol tiploc) *routeing-tiplocs*))

(defun cleanup-trains (start-date end-date &key (only-routeing-p nil))
  (print 'cleanup)
  (dolist (uid (hash-keys *train-uids*))
    (cleanup-trainids uid start-date end-date :only-routeing-p only-routeing-p)
    (unless (gethash uid *train-uids*)
      (remhash uid *train-uids*)))
  (dolist (id (hash-keys *uid-trains*))
    (unless (gethash (gethash id *uid-trains*) *train-uids*)
      (remhash id *uid-trains*))))

(defun reverse-bits (bits)
  (mapcar #'(lambda (bit) (if (= bit 0) 1 0)) bits))

(defun combine-bits (bits1 bits2)
  (let ((i -1))
    (mapcar #'(lambda (bit) 
		(progn
		  (incf i)
		  (if (= bit 1) 
		      0
		      (nth i bits2))))
	    bits1)))

#|
(cons (encode-simple-train (train-id train) 070102 070101 '(0 0 0 0 0 0 0 0)) nil))
  (maphash #'(lambda (tiploc stops)
	       (setf (gethash tiploc *schedules*)
		     (remove-if-not #'(lambda (stop) (gethash (first (simple-decode-train-stop2 stop)) *uid-trains*)) stops)))
	   *schedules*))
|#

(defun cleanup-trainids (train-uid start-date end-date &key (process-p t) (only-routeing-p nil))
  ;; STP 1 O 2 C 3 P 0 N
  (labels ((start-date (train) (fourth train))
	   (end-date (train) (fifth train))
	   (days (train) (third train))
	   (stp (train) (first train))
	   (train-id (train) (second train))
	   (replace-train-valid-dates (train new-start new-end)
	     (list (stp train) (train-id train) (days train) new-start new-end))
	   (remove-train (train)
	     (remhash (train-id train) *trains*))
	   (replace-train-dates-days (train new-start new-end days)
	     (list (stp train) (train-id train) days new-start new-end))
	   (encoded-train-valid (start end)
	     (dates-contained-p 
	      (parse-integer (dttm->string (string->dttm start-date) :format :shortyymmdd))
	      (parse-integer (dttm->string (string->dttm end-date) :format :shortyymmdd))
	      start
	      end)))
    (let* ((trains (sort (mapcar #'(lambda (train)
				   (multiple-value-bind (encoded stp) (floor train 10)
				     (cons stp (decode-train encoded))))
			       (gethash train-uid *train-uids*))
		      #'(lambda (t1 t2) (if (= (car t1) (car t2))
					    (if (= (nth 3 t1) (nth 3 t2))
						(< (nth 4 t1) (nth 4 t1))
						(< (nth 3 t1) (nth 3 t2)))
					    (> (car t1) (car t2))))))
	   (permanents (remove-if-not #'(lambda (working) (or (= (first working) 3) (= (first working) 0))) trains))
	   (changes (remove-if-not #'(lambda (working) (or (= (first working) 1) (= (first working) 2))) trains))
	   (amendments '())
	   (remove-these '()))
    (dolist (permanent permanents)
      (let ((candidates '()))
	(aif (remove-if-not #'(lambda (ch) (dates-contained-p (start-date permanent) (end-date permanent)
							      (start-date ch) (end-date ch)))
			    changes)
	     (dolist (change it)
	       (push (replace-train-valid-dates permanent (start-date permanent) (1-yymmdd (start-date change))) candidates)
	       (push (replace-train-valid-dates permanent (1+yymmdd (end-date change)) (end-date permanent)) candidates)
	       (push change candidates)
	       (push (replace-train-dates-days permanent (start-date change) (end-date change) (combine-bits (days change) (days permanent))) candidates))
	     (push permanent candidates))
	(setf amendments (append 
			  (remove-if-not #'(lambda (candidate) 
					     (and candidate 
						  (>= (end-date candidate) (start-date candidate))
						  (not (zerop (convert-bit-string-to-integer (days candidate))))
						  (any-train-days-p (start-date candidate) (end-date candidate) (days candidate))
						  (encoded-train-valid (start-date candidate) (end-date candidate))))
					 candidates)
			  amendments))))
    (setf amendments (delete-duplicates amendments :test #'equal))
    (if process-p
	(let ((encoded-trains '()))
	  (dolist (amendment amendments)
	    (let* ((stops (rest (gethash (train-id amendment) *trains*)))
		   (trainid (incf *max-train-id*))
		   (encoded-train (encode-simple-train *max-train-id* (start-date amendment) (end-date amendment) (days amendment))))
	      (print trainid)
	      (push (+ (* 10 encoded-train) (stp amendment)) encoded-trains)
	      (setf (gethash trainid *trains*)
		    (cons encoded-train
			  (let ((previous nil))
			    (mapcar #'(lambda (decoded) 
					(let ((encoded 
					       (simple-encode-train-stop2 
						(second decoded) (fifth decoded) 
						(third decoded) (fourth decoded) trainid)))
					  (if (not only-routeing-p)
					      (push-stop (fifth decoded) encoded)
					      (progn
						(when (not (zerop (second encoded)))
						  (when (and previous (not (zerop (second (cdr previous)))))
						    (push-neighbour (car previous) (fifth decoded) (cdr previous) encoded))
						  (setf previous (cons (fifth decoded) encoded)))))
					  encoded))
				    (decode-stops stops)))))
	      (setf (gethash *max-train-id* *uid-trains*) train-uid)
	      (push amendment remove-these)))
	  (setf (gethash train-uid *train-uids*) encoded-trains)
	  (dolist (amendment remove-these)
	    (remove-train amendment)))
	(print (cons 'finals amendments))))))

(defun decode-stops (stops)
  (mapcar #'simple-decode-train-stop2 stops))

(defun push-stop (tiploc-id stop)
  (if (not (gethash tiploc-id *schedules*))
      (setf (gethash tiploc-id *schedules*) (list stop))
      (setf (gethash tiploc-id *schedules*) (push stop (gethash tiploc-id *schedules*)))))

(defun all-stops (train-stops &key (index-p nil))
  (mapcar #'(lambda (stop)
	      (list (first stop) (second stop) (third stop) (fourth stop) 
		    (if index-p
			(svref *hubs-array* (fifth stop))
			(id->tiploc (fifth stop)))))
	  (remove-if-not #'(lambda (stop) 
			     (not (string= "P" (second stop))))
			 train-stops)))

(defun stops-connectivity (stops)
  (let ((decoded (all-stops (decode-stops stops))))
    (stop-connectivity (first decoded) (rest decoded))))

(defun stop-connectivity (start rest-of-stops)
  (if (null rest-of-stops)
      nil
      (let ((start-time (third start)))
	(dolist (stop rest-of-stops)
	  (let ((key (concatenate 'string (fifth start) (fifth stop)))
		(dur (mtimedifference-nd (fourth stop) start-time)))
	    (unless (or (and (= (third start) 0) (= (fourth start) 0)) (and (= (third stop) 0) (= (fourth stop) 0)))
	      (if (> dur 300)
		  (dbg :read ";;;;connectivity start-arr:~A start-dep:~A stop-arr:~A stop-dep:~A" (list (fourth start) (third start) 
													(fourth stop) (third stop))))
	      (incf *links*)
	      (aif (gethash key *connectivity*)
		   (let ((con it))
		     (incf (connectivity-total-time con) dur)
		     (incf (connectivity-number con))
		     (when (< (connectivity-maximum con) dur)
		       (setf (connectivity-maximum con) dur))
		     (when (< dur (connectivity-minimum con))
		       (setf (connectivity-minimum con) dur))
		     (setf (gethash key *connectivity*) con))
		   (setf (gethash key *connectivity*) (make-connectivity :total-time dur
									 :number 1
									 :minimum dur
									 :maximum dur))))))
	(stop-connectivity (first rest-of-stops) (rest rest-of-stops)))))

(defparameter *other-ways* (make-hash-table :test 'equal))

(defun init-hash (htable)
  (dotimes (j 344)
      (let ((ht (make-hash-table :test 'equal :size 344)))
	(dotimes (i 344)
	  (setf (gethash (1+ i) ht) nil))
	(setf (svref htable (1+ j)) ht))))

(defun consolidate-hash (htable)
  (dotimes (i 344)
    (let ((ht (svref htable (1+ i))))
      (maphash #'(lambda (key value)
		   (unless value
		     (remhash key ht)))
	       ht))))

(defun sorted-array (htable)
  (dotimes (i 344)
    (let ((ht (svref htable (1+ i))))
      (maphash #'(lambda (key value)
		   (when value
		     (let* ((connections   
			     (sort value #'< :key (compose #'third #'simple-decode-train-stop2 #'first)))
			    (results (make-array (list (length connections)))))
		       (dotimes (j (length connections))
			 (setf (svref results j) (nth j connections)))
		       (setf (gethash key ht) (cons (length connections) results)))))
	       ht))))

(defun init-neighbours (array consolidate)
  (dotimes (i (array-dimension array 0))
    (when (hash-table-p (svref array i))
      (setf
       (svref consolidate i)
       (mapcar #'(lambda (j) (svref *hubs-array* j))
	       (hash-keys (svref array i)))))))

(defun init (&key (routes t) (trains t) (only-routeing nil) (only-next-two-days-p nil) (start-date nil) (end-date nil) (just-read-uid nil) (dont-clean nil))
  (progn
    (init-hash *neighbours*)
    (init-hash *fixed-neighbours*)
    (when routes
      (clrhash *permitted-route-trains*)
      (setf *permitted-routes*
	    (let ((pr (make-hash-table :test 'equal)))
	      (dolist (route (funcall *permitted-route-reader*))
		(aif (gethash (permitted-route-from route) pr)
		     (push (rest route) (gethash (permitted-route-from route) pr))
		     (setf (gethash (permitted-route-from route) pr) (list (rest route)))))
	      pr))
      (setf *links* 0)
      (setf *station-links*
	    (let ((result (make-hash-table :test 'equal)))
	      (dolist (link (funcall *station-link-reader*))
	    (when (and (assoc (short-path-link-from-crs-grp-code link) *stations* :test #'string=)
		       (assoc (short-path-link-to-crs-grp-code link) *stations* :test #'string=))
	      (if (gethash (short-path-link-from-crs-grp-code link) result)
		  (setf (gethash (short-path-link-from-crs-grp-code link) result) 
			(append (gethash (short-path-link-from-crs-grp-code link) result)
				(list link)))
		  (setf (gethash (short-path-link-from-crs-grp-code link) result) (list link)))))
	      result))
      (setf *linked-stations*
	    (let ((ls (make-hash-table :test 'equal)))
	      (dolist (route (funcall *linked-route-reader*))
		(aif (gethash (linked-route-from route) ls)
		     (push (rest route) (gethash (linked-route-from route) ls))
		     (setf (gethash (linked-route-from route) ls) (list (rest route)))))
	      ls)))
    (clrhash *schedules*)
    (clrhash *trains*)
    (clrhash *connectivity*)
    (setf *routeing-tiplocs* (read-hashtable (sondesh-file-path "routeing.tiplocs")))
    (setf *average-time-stops* (read-hashtable (sondesh-file-path "average.timestops")))
    (setf *other-ways* (read-hashtable (sondesh-file-path "other.ways")))
    (if trains
	(progn
	  (read-simple-trains :only-routeing-p only-routeing :just-read-uid just-read-uid
			      :only-next-two-days-p only-next-two-days-p :start-date start-date :end-date end-date)
	  (unless dont-clean (cleanup-trains start-date end-date :only-routeing-p only-routeing))
	  (read-fixed)
	  (consolidate-hash *fixed-neighbours*)
	  (init-neighbours *fixed-neighbours* *fixed-neighbours-consolidate*)
	  (load-routes)
	  (if only-routeing
	      (progn
		(consolidate-hash *neighbours*)
		(sorted-array *neighbours*)
		(init-neighbours *neighbours* *neighbours-consolidate*))
	      (maphash #'(lambda (tiploc stops)
			   (setf (gethash tiploc *schedules*)
				 (remove-if-not #'(lambda (stop) (gethash (first (simple-decode-train-stop2 stop)) *trains*)) stops)))
		       *schedules*)))
	(read-processed))))

(defun write-processed ()
  (progn
    (write-hashtable (sondesh-file-path "tiploc.id") *tiploc->id*)
    (write-hashtable (sondesh-file-path "id.tiploc") *id->tiploc*)
    (write-hashtable (sondesh-file-path "trains") *trains*)
    (write-hashtable (sondesh-file-path "train.uids") *train-uids*)
    (write-hashtable (sondesh-file-path "connectivity") *connectivity*)
    (write-hashtable (sondesh-file-path "schedules") *schedules*)
    (write-hashtable (sondesh-file-path "station.links") *station-links*)
    (write-hashtable (sondesh-file-path "permitted.routes") *permitted-routes*)
    (write-hashtable (sondesh-file-path "linked.stations") *linked-stations*)
    (write-hashtable (sondesh-file-path "uid.trains") *uid-trains*)
    (write-hashtable (sondesh-file-path "routeing.tiplocs") *routeing-tiplocs*)
    (write-routes)
    (write-hashtable (sondesh-file-path "permitted.route-trains") *permitted-route-trains*)
    (write-hashtable (sondesh-file-path "other.ways") *other-ways*)
    (write-hashtable (sondesh-file-path "average.timestops") *average-time-stops*)))

(defun read-processed ()
  (progn
    (setf *links* 16991809)
    (setf *max-train-id* 402309)
    (setf *station-links* (read-hashtable (sondesh-file-path "station.links")))
    (setf *permitted-routes* (read-hashtable (sondesh-file-path "permitted.routes")))
    (setf *linked-stations* (read-hashtable (sondesh-file-path "linked.stations")))
    (setf *tiploc->id* (read-hashtable (sondesh-file-path "tiploc.id")))
    (setf *id->tiploc* (read-hashtable (sondesh-file-path "id.tiploc")))
    (setf *trains* (read-hashtable (sondesh-file-path "trains")))
    (setf *train-uids* (read-hashtable (sondesh-file-path "train.uids")))
    (setf *schedules* (read-hashtable (sondesh-file-path "schedules")))
    (setf *uid-trains* (read-hashtable (sondesh-file-path "uid.trains")))
    (setf *connectivity* (read-hashtable (sondesh-file-path "connectivity")))
    (setf *routeing-tiplocs* (read-hashtable (sondesh-file-path "routeing.tiplocs")))
    (init-hash *fixed-neighbours*)
    (read-fixed)
    (consolidate-hash *fixed-neighbours*)
    (init-neighbours *fixed-neighbours* *fixed-neighbours-consolidate*)
    (load-routes)
    (setf *permitted-route-trains* (read-hashtable (sondesh-file-path "permitted.route-trains")))
    (setf *other-ways* (read-hashtable (sondesh-file-path "other.ways")))
    (setf *average-time-stops* (read-hashtable (sondesh-file-path "average.timestops")))))

(defun average-time-stops (stop1 stop2)
  (aif (gethash (concatenate 'string (station-crs-code stop1) (station-crs-code stop2)) 
		*average-time-stops*)
       it
       (let ((total-time 0)
	     (total-num 0))
	 (dolist (tiploc1 (station-tiploc-codes stop1))
	   (dolist (tiploc2 (station-tiploc-codes stop2))
	     (awhen (gethash (concatenate 'string tiploc1 tiploc2) *connectivity*)
	       (let ((total (connectivity-total-time it))
		     (number (connectivity-number it)))
		 (declare (fixnum total number total-time total-num)
			  (optimize (speed 3) (safety 0)))
		 (incf total-time total)
		 (incf total-num number)))))
	 (if (zerop total-num)
	     60
	     (float (/ total-time total-num))))))

(defun train-details (trainid &key (routeing-p nil))
  (awhen (gethash trainid *trains*)
    (list (decode-train (first it))
	  (all-stops (decode-stops (rest it)) :index-p routeing-p))))

(defun consolidate-train-uids (start-date end-date)
  (maphash #'(lambda (uid trains)
	       (setf (gethash uid *train-uids*)
		     (remove-if-not
		      #'(lambda (train-code)
			  (awhen (train-details (first (decode-train (floor train-code 10))))
			    (and (dates-contained-p 
				  (parse-integer (dttm->string (string->dttm start-date) :format :shortyymmdd))
				  (parse-integer (dttm->string (string->dttm end-date) :format :shortyymmdd))
				  (third (first it)) (fourth (first it)))
				 (cadr it))))
		      trains)))
	   *train-uids*))

(defun small-init ()
  (progn
    (setf *schedules* (make-hash-table :test 'equal :size 8000 :rehash-size 1.1))
    (setf *trains* (make-hash-table :test 'equal :size 410000 :rehash-size 1.01))
    (setf *train-uids* (make-hash-table :test 'equal :size 100000 :rehash-size 1.01))
    (setf *uid-trains* (make-hash-table :test 'equal :size 100000 :rehash-size 1.01))
    (setf *connectivity* (make-hash-table :test 'equal :size 110000 :rehash-size 1.1))
    (setf *links* 0)
    (setf *max-train-id* *starter-train-id*)))

(defun big-init ()
  (progn
    (small-init)
    (init)))

(defparameter *directs* (make-hash-table :test 'equal :size 2736 :rehash-size 1.1))

(defun sort-out-directs ()
  (progn
    (maphash #'(lambda (key value)
	       (setf (gethash (subseq key 0 7) *directs*)
		     (cons (cons (subseq key 7 14) value) (gethash (subseq key 0 7) *directs*))))
	   *connectivity*)
    (maphash #'(lambda (key value)
		 (setf (gethash key *directs*)
		       (sort (gethash key *directs*)
			     #'>
			     :key (compose #'connectivity-number #'cdr))))
	     *directs*)))

(defparameter *start-date* "26-11-2007")
(defparameter *end-date* "02-12-2007")

(init :start-date *start-date* :end-date *end-date* :dont-clean nil :just-read-uid nil)
