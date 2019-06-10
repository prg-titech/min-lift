(lift
 (N)
 ((array-type float N))
 (lambda (xs)
  ((toGlobal (filterGlb (lambda (x) (< x 0.5f)))) xs)))

