(lift
 (N)
 ((array-type int N))
 (lambda (xs)
  ; OK
  ; (let ys
  ;  ((toLocal (filterGlb (lambda (x)
  ;               (or
  ;                (=i (mod x 3) 0) (=i (mod x 5) 0))))) xs)
  ;  ((toGlobal (reduceSeq 0 (lambda (sum x) (+i sum x)))) ys))

  ; OK
  ; ((toGlobal (reduceSeq 0 (lambda (sum x) (+i sum x))))
  ;   ((toLocal
  ;    (filterGlb (lambda (x)
  ;                (or
  ;                 (=i (mod x 3) 0) (=i (mod x 5) 0))))) xs))

  ; OK
  ; ((lambda (x)
  ;   ((toGlobal (reduceSeq 0 (lambda (sum x) (+i sum x))))
  ;     ((toLocal
  ;      (filterGlb (lambda (x)
  ;                  (or
  ;                   (=i (mod x 3) 0) (=i (mod x 5) 0))))) x))) xs)

  ; OK
  ((o
   (toGlobal (reduceSeq 0 (lambda (sum x) (+i sum x))))
   (toLocal
    (filterGlb (lambda (x)
                (or
                 (=i (mod x 3) 0) (=i (mod x 5) 0)))))
   ) xs)

  ))
