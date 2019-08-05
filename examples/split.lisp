(lift
 (N)
 ((array-type float N))
 (lambda (xs)
  ; (toGlobal (o (mapSeq (reduceSeq 0.0f (lambda (sum x) (+ sum x)))) (split #2) xs))))
  (
   (toGlobal
    (o
     join
     (mapSeq (reduceSeq 0.0f (lambda (sum x) (+ sum x))))
     (split #2)))
   xs)))
