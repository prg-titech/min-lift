(lift
 (N)
 ((array-type float N))
 (lambda (xs)
  (toGlobal
   (o
    join
    (mapWrg
     (o
      join
      (mapLcl (lambda (x) (* x 2.0f)))
      (split #2)))
    (split #8)
      ))))

