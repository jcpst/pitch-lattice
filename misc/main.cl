; For generating pitch lattices
;

(defstruct pitch 
  cent ratio transposition limit degree)

(defun range-inc (max &key (min 1) (step 1))
  "Returns a range of numbers as a list
   Inspired by: https://stackoverflow.com/a/13937652/3957261"
  (loop for x
		from min 
		below (1+ max)
		by step
		collect x))

(defun factors (n)
  "Get the factors of a number"
  (remove-if 
	(lambda (x) (zerop (rem n x)))
	(range-inc n)))

(defun primep (n)
  "Determine if a number is prime
   From here: https://stackoverflow.com/a/15817766/3957261"
  (cond 
	((= 1 n) nil)
	((= 2 n) t)
	((= 3 n) t)
    ((evenp n) nil)
    (t (loop for i 
			 from 3
			 to (isqrt n)
			 by 2
			 never (zerop (mod n i))))))

(defun ratio-to-list (n)
  "Converts a ratio to a list"
  (list (numerator n) (denominator n)))

(defun power-of-twop (n)
  "Determines if a number is a power of two"
  (and (not (zerop n)) (zerop (logand n (- n 1)))))

(defun recip (n)
  "Returns the reciprocal of a ratio"
  (/ (denominator n) (numerator n)))

(defun ratio-to-cents (ratio)
  "Converts a ratio to cents"
  (* (log ratio 10) (/ 1200 (log 2 10))))

(defun flatten-ratio (ratio)
  "Brings the size of a ratio in between 1 and 2"
  (cond
	((> ratio 2) (flatten-ratio (/ ratio 2)))
	((< ratio 1) (flatten-ratio (* ratio 2)))
	(t ratio)))

(defun flip-ratio (ratio)
  "Flips the numerator and denominator in a ratio"
  (flatten-ratio (recip ratio)))

; limit
;(defun limit (ratio)
;  "Calculates the partial limit of a ratio"
;  (apply #'max (remove-if-not primep
;    ((concatenate (map 'list #'factors (ratio-to-list 3/2)))
; sum-ratios
; diff-ratios
; step
; make-pitch
; find-ordinal
; ordinal-walk
; make-set

; TODO: get-scale = sort a collection of "make-set"s by ratio size

