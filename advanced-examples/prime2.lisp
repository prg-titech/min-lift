(lift
 (N)
 ((array-type int N))
 (lambda (xs)
   ((toGlobal (reduceSeq 0 (lambda (sum x) (+i sum x)))) xs)))
