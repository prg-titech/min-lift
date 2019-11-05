(lift
 (N)
 ((array-type int N))
 (lambda (xs)
  (let square (lambda (x) (*i x x))
    (unpack ys
     ((toLocal (filterGlb (lambda (x)
                  (let sumOfDidits
                      (+i (square (/i x 1000))
                      (+i (square (mod (/i x 100) 10))
                      (+i (square (mod (/i x 10)  10))
                          (square (mod x 10)))))
                    (=i sumOfDidits x))))) xs)
     ;(pack ((toGlobal (reduceSeq 0 (lambda (sum x) (+i sum x)))) ys)))))
     (pack ys)))))

