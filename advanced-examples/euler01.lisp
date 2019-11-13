(lift
 (N)
 ((array-type int N))
 (lambda (xs)
  (unpack ys
   ((toLocal (filterGlb (lambda (x)
                (or
                 (=i (mod x 3) 0) (=i (mod x 5) 0))))) xs)
   ((toGlobal (reduceSeq 0 (lambda (sum x) (+i sum x)))) ys))))


  ; ((o
  ;  (toGlobal (reduceSeq 1 (+i)))
  ;  (toLocal
  ;   (filterGlb (lambda (x)
  ;               (or
  ;                (=i (mod x 3) 0) (=i (mod x 5) 0)))))
  ;  ) xs)))

