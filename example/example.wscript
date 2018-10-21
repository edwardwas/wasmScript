(defn fib (n)
  (cond
    ((= n 1) 1)
    ((= n 2) 1)
    (True (+ (fib (- n 1)) (fib (- n 2))))
  ))

(fib 28)
