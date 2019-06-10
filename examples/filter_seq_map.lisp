(lift
 (N)
 ((array-type float N))
 (lambda (xs)
  ((toGlobal
   (unpack ys (toLocal (filterSeq (lambda (x) (< x 0.5f)) xs))
    (pack (mapSeq (lambda (x) (* x 2.0f)))))) ys)))

