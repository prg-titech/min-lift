(lift
 (N)
 ((array-type float N))
 (lambda (xs)
   (mapSeq (lambda (xs) (reduceSeq 0.0f (lambda (sum x) (+ sum x)) xs)) (split #2 xs))))
   ; (mapSeq (lambda (xs) (reduceSeq 0.0f (lambda (sum x) +))) (split #2 xs))))
