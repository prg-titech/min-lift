(lift
 (N)
 ((array-type float N))
 (lambda (xs)
  ; (toGlobal (lambda (xs') (mapSeq (lambda (x) (* x 2.0f)) xs')) xs)))
  (toGlobal (mapSeq (lambda (x) (* x 2.0f)) xs))))

