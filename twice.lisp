(lift
 (N)
 ((array-type float N))
 (lambda (xs)
  ((mapSeq (lambda (x) (* x 2.0f))) xs)))

