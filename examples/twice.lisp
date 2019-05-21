(lift
 (N)
 ((array-type float N))
 (lambda (xs)
  ; この式だとAddressSpaceの決定が上手くいかない
  ; (toGlobal (mapSeq (lambda (x) (* x 2.0f)) xs))))
  ((toGlobal (mapSeq (lambda (x) (* x 2.0f)))) xs)))

