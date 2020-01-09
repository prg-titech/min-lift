(lift
 (N)
 ((array-type float N))
 (lambda (xs)
  ((toGlobal
    (mapGlb (lambda (x) (* x 2.0f)))) (filterGlb (lambda (x) (> x 0.5f)) xs))))

