(lift
 (N)
 ((array-type float N) (array-type float N))
 (lambda (xs ys)
  ((toGlobal (o (mapSeq (lambda (xy) (+ (get1 xy) (get2 xy)))))) (zip xs ys))))

