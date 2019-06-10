(lift
 (N)
 ((array-type float N))
 (lambda (xs)
  ((toGlobal (filterSeq (lambda (x) (< x 0.5f)))) xs)))

