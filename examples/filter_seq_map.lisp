(lift
 (N)
 ((array-type float N))
 (lambda (xs)
  (toGlobal (mapSeq (lambda (x) (* x 2.0f)) (filterSeq (lambda (x) (< x 0.5f)) xs)))))

