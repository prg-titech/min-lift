(lift
 (N)
 ((array-type int N))
 (lambda (xs)
  (let fourthPower (lambda (x) (*i (*i (*i x x)(*i x x)) x))
     (let ys
      ((toLocal (filterGlb (lambda (x)
                  (and
                    (not (=i x 1))
                    (let sumOfDidits
                        (+i (fourthPower (mod (/i x 100000) 10))
                        (+i (fourthPower (mod (/i x 10000) 10))
                        (+i (fourthPower (mod (/i x 1000) 10))
                        (+i (fourthPower (mod (/i x 100) 10))
                        (+i (fourthPower (mod (/i x 10)  10))
                            (fourthPower (mod x 10)))))))
                      (=i sumOfDidits x))
                    )
                  ))) xs)
     ((toGlobal (reduceSeq 0 (lambda (sum x) (+i sum x)))) ys))
    )))

