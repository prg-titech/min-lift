(lift
 (N)
 ((array-type float N))
 (lambda (xs)
  (toGlobal
     (unpack ys (toGlobal (filterGlb (lambda (x) (> x 0.5f)) xs))
    (mapGlb (lambda (x) (* x 2.0f)) ys)))))


