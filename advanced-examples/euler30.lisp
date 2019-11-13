(lift
 (N)
 ((array-type int N))
 (lambda (xs)
  (let fourthPower (lambda (x) (*i (*i x x)(*i x x)))
    (unpack ys
     ((toLocal (filterGlb (lambda (x)
                  (and
                    (not (=i x 1))
                    (let sumOfDidits
                        (+i (fourthPower (/i x 1000))
                        (+i (fourthPower (mod (/i x 100) 10))
                        (+i (fourthPower (mod (/i x 10)  10))
                            (fourthPower (mod x 10)))))
                      (=i sumOfDidits x))
                    )
                  ))) xs)
     (pack ((toGlobal (reduceSeq 0 (lambda (sum x) (+i sum x)))) ys))))))

