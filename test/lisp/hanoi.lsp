;; The tower of Hanoi.

(defun hanoi (n)
  (transfer 'A 'B 'C n))

(defun print-move (from to)
  (princ "Move ring from " from " to " to "\n") t)

(defun transfer (from to via n)
  (if (eq n 1)
      (print-move from to)
    (transfer from via to (- n 1))
    (print-move from to)
    (transfer via to from (- n 1))))
