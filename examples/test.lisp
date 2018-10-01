(lift
 (N)
 ; ((array-type float N))
 ((array-type (array-type float N) N))
 (lambda (xs)
  ; (o (o (join) (mapSeq (reduceSeq 0.0f (lambda (sum x) (+ sum x))))) (split #2) xs)))
  ; (o (o (join) (mapSeq (reduceSeq 0.0f (lambda (sum x) (+ sum x))))) (split #2) xs)))
  ; (o (mapSeq (reduceSeq 0.0f (lambda (sum x) (+ sum x)))) (split #2)) xs))
  ; (o (join) (split #2) xs)))
  ; (o (join) (mapSeq (reduceSeq 0.0f (lambda (sum x) (+ sum x)))) xs)))
  ; mapSeqの型がおかしい
  ; NG
  (o (join) (mapSeq (lambda (x) x)) xs)))
  ; OK
  ; (join xs)))
  ; (mapSeq (lambda (x) 0.0f) xs)))
  ; (o (join) (split #2) xs)))
