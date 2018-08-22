(lift
 (N)
 ((array-type float N))
 (lambda (xs)
   (reduceSeq 0.0f (lambda (sum x) (+ sum x)) xs)))