; For generating pitch lattices

(defstruct pitch cent ratio transposition limit degree)

(defun flatten (ls)
  "From Paul Graham's OnLisp"
  (labels ((mklist (x) (if (listp x) x (list x))))
    (mapcan #'(lambda (x) (if (atom x) (mklist x) (flatten x))) ls)))

(defun range-inc (max &key (min 1) (step 1))
  "Returns a range of numbers as a list"
  (loop for x from min below (1+ max) by step collect x))

(defun factors (n)
  "Get the factors of a number"
  (remove-if-not 
	(lambda (x) (zerop (rem n x)))
	(range-inc n)))

(defun primep (n)
  "Determine if a number is prime
   From 'Practical Common Lisp', Ch 8"
  (when (> n 1)
    (loop for i from 2 to (isqrt n) never (zerop (mod n i)))))

(defun ratio-to-list (n)
  "Converts a ratio to a list"
  (list (numerator n) (denominator n)))

(defun power-of-twop (n)
  "Determines if a number is a power of two"
  (and (not (zerop n)) (zerop (logand n (- n 1)))))

(defun ratio-to-cents (r)
  "Converts a ratio to cents"
  (* (log r 10) (/ 1200 (log 2 10))))

(defun flatten-ratio (r)
  "Brings the size of a ratio in between 1 and 2"
  (cond
    ((> r 2) (flatten-ratio (/ r 2)))
    ((< r 1) (flatten-ratio (* r 2)))
    (t r)))

(defun invert-ratio (r)
  "Returns the inversion of a ratio"
  (flatten-ratio (/ (denominator r) (numerator r))))

(defun limit (ratio)
  "Calculates the partial limit of a ratio by finding
   the larget factor among the numerator and denominator."
  (apply 
	#'max 
	(remove-if-not 
	  #'primep
	  (flatten (map 'list #'factors (ratio-to-list ratio))))))

(defun lattice-relation (prime ordinal)
  "Gets the interval to be used on a lattice walk"
  (flatten-ratio
	(case ordinal
       (:otonal (/ prime 2))
       (:utonal (/ 2 prime)))))

(defun walk (r iter)
  "Takes a walk in one direction on the pitch lattice"
  (mapcar 
	#'(lambda (x)
		(flatten-ratio 
		  (apply '* (make-list x :initial-element r)))) 
	(range-inc iter)))

(defun gen-lattice (limits size)
  "Generate collection of ratios with a given set of limits"
  (concatenate 
	'list '(1)
	(loop for x in limits collect
		  (list (walk (lattice-relation x :utonal) size)
				(walk (lattice-relation x :otonal) size)))))

; examples
; --------
; (walk (lattice-relation 5 :otonal) 3)
; -> (5/4 25/16 125/64)

; (gen-lattice '(3 5) 3)
; -> (1 ((4/3 16/9 32/27) (3/2 9/8 27/16)) ((8/5 32/25 128/125) (5/4 25/16 125/64)))

; (sort (flatten (gen-lattice '(3 5) 2)) #'<)
; -> (1 9/8 5/4 32/25 4/3 3/2 25/16 8/5 16/9)

; TODO: build a list of pitch structs from sorted lattice

