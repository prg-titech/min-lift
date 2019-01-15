(lift
 (N)
 ((array-type float N))
 (lambda (xs)
   (toGlobal (reduceSeq 0.0f (lambda (sum x)
                              (let y (+ sum x) y)) xs))))

