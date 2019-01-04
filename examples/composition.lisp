(lift
 (N)
 ((array-type float N))
 (lambda (xs)
  (toGlobal
   (o
    (mapSeq (lambda (x) (* x 2.0f)))
    (mapSeq (lambda (x) (+ x 2.0f)))
    (mapSeq (lambda (x) (+ x 1.0f)))
    xs))))

