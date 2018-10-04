(lift
 (N M K)
 ((array-type (array-type float K) M)
  (array-type (array-type float N) K))
 (lambda (A B)
  ((map (lambda (aRow)
        (o$ (map (lambda (bCol)
              (o$ (reduce + 0.0f) (map (lambda (x) (* (get x 0) (get x 1)))) (zip aRow bCol))))
         trancepose B))) A)))
