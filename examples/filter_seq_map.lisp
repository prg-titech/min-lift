(lift
 (N)
 ((array-type float N))
 (lambda (xs)
   (let ys ((toLocal (filterSeq (lambda (x) (< x 0.5f)))) xs)
    ((toGlobal (mapSeq (lambda (x) (* x 2.0f)))) ys)
      )))


