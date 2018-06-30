(lift
 (N)
 ((array-type float N))
 (lambda (xs)
  ((o (map (lambda (x) (* x 2.0f)))) xs)))

