(lift
 (N)
 ((array-type float N))
 (lambda (xs)
  (o (mapSeq (reduceSeq 0.0f (lambda (sum x) (+ sum x)))) (split #2) xs)))
