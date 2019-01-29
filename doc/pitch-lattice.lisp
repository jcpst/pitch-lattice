; For generating pitch lattices


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

(defun limit (r)
  "Calculates the partial limit of a ratio by finding
   the larget factor among the numerator and denominator."
  (apply 
    #'max 
    (remove-if-not
      #'primep
      (flatten (map 'list #'factors (ratio-to-list r))))))

(defstruct (pitch (:constructor create-pitch (r)))
  (cent (ratio-to-cents r)) 
  (interval r)
  (ordinal r) 
  (limit (limit r)) 
  (degree 3))

(defun lattice-relation (prime ordinal)
  "Gets the interval to be used on a lattice walk"
  (flatten-ratio
    (case ordinal
      (:otonal (/ prime 2))
      (:utonal (/ 2 prime)))))

; TODO: These next two functions are kinda gross.
(defun walk (r iter)
  "Takes a walk in one direction on the pitch lattice"
  (loop for x from 1 to iter collect 
        (create-pitch
          (flatten-ratio
            (apply '* (make-list x :initial-element r))))))

(defun gen-lattice (limits size)
  "Generate collection of ratios with a given set of limits"
  (loop for x in limits collect
        (list (walk (lattice-relation x :utonal) size)
              (walk (lattice-relation x :otonal) size))))

; examples
; --------
; (walk (lattice-relation 5 :otonal) 3)
; -> (5/4 25/16 125/64)

; [x] build a list of pitch structs from sorted lattice
; [ ] add graph position. ex: '(0 0) '(-1 -1) '(2 0)
;     list for as many dimensions needed (x, y, z, etc)
; [ ] calculate ratio between graph points
; [ ] create associated graphs treating a point as a new origin


; (sort (flatten (gen-lattice '(3 5) 2))
;       #'(lambda (x y) 
;         (< (pitch-interval x) 
;            (pitch-interval y))))
; -> (#S(PITCH :CENT 203.90997 :INTERVAL 9/8 :ORDINAL 9/8 :LIMIT 3 :DEGREE 3)
;     #S(PITCH :CENT 386.31366 :INTERVAL 5/4 :ORDINAL 5/4 :LIMIT 5 :DEGREE 3)
;     #S(PITCH :CENT 427.37253 :INTERVAL 32/25 :ORDINAL 32/25 :LIMIT 5 :DEGREE 3)
;     #S(PITCH :CENT 498.04498 :INTERVAL 4/3 :ORDINAL 4/3 :LIMIT 3 :DEGREE 3)
;     #S(PITCH :CENT 701.95496 :INTERVAL 3/2 :ORDINAL 3/2 :LIMIT 3 :DEGREE 3)
;     #S(PITCH :CENT 772.6273 :INTERVAL 25/16 :ORDINAL 25/16 :LIMIT 5 :DEGREE 3)
;     #S(PITCH :CENT 813.6862 :INTERVAL 8/5 :ORDINAL 8/5 :LIMIT 5 :DEGREE 3)
;     #S(PITCH :CENT 996.08997 :INTERVAL 16/9 :ORDINAL 16/9 :LIMIT 3 :DEGREE 3))
