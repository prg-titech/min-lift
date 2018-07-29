(lift
 (N)
 ((array-type float N))
 (lambda (xs)
  ((o (mapSeq (lambda (x) (* x 2.0f))) (mapSeq (lambda (x) (+ x 3.0f))))
   xs)))