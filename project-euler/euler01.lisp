(lift
 (N)
 ((array-type int N))
 (lambda (xs)
  (unpack ys
   ((toLocal (filterGlb (lambda (x)
                (or
                 (=i (mod x 3) 0) (=i (mod x 5) 0))))) xs)
   (pack ((toGlobal (reduceSeq 0 (+i))) ys)))))


  ; ((o
  ;  (toGlobal (reduceSeq 1 (+i)))
  ;  (toLocal
  ;   (filterGlb (lambda (x)
  ;               (or
  ;                (=i (mod x 3) 0) (=i (mod x 5) 0)))))
  ;  ) xs)))

