(lift
 (N)
 ((array-type int N) int)
 (lambda (xs n)
   (filterGlb (lambda (x)
                (or
                  (=i x n) (not (=i (mod x n) 0)))) xs)))

