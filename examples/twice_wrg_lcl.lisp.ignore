(lift
 (N)
 ((array-type float N))
 (lambda (xs)
   (o
    join
    (mapWrg
      (toGlobal
        (mapLcl (lambda (x) (* x 2.0f)))))
    (split #8)
    xs)))

