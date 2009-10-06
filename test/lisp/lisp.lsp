;; Lisp routines for the SLisp interpreter.
;;
;; Portions copyright (c) 1988, 1990 Roger Rohrbach

(defun first (x) (car x))
(defun rest (x) (cdr x))
(defun second (x) (car (cdr x)))
(defun third (x) (car (cdr (cdr x))))
(defun fourth (x) (nth 3 x))
(defun fifth (x) (nth 4 x))
(defun sixth (x) (nth 5 x))
(defun seventh (x) (nth 6 x))
(defun eighth (x) (nth 7 x))
(defun ninth (x) (nth 8 x))
(defun tenth (x) (nth 9 x))
(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))
(defun caaar (x) (car (car (car x))))
(defun caadr (x) (car (car (cdr x))))
(defun cadar (x) (car (cdr (car x))))
(defun caddr (x) (car (cdr (cdr x))))
(defun cdaar (x) (cdr (car (car x))))
(defun cdadr (x) (cdr (car (cdr x))))
(defun cddar (x) (cdr (cdr (car x))))
(defun cdddr (x) (cdr (cdr (cdr x))))
(defun caaaar (x) (car (car (car (car x)))))
(defun caaadr (x) (car (car (car (cdr x)))))
(defun caadar (x) (car (car (cdr (car x)))))
(defun caaddr (x) (car (car (cdr (cdr x)))))
(defun cadaar (x) (car (cdr (car (car x)))))
(defun cadadr (x) (car (cdr (car (cdr x)))))
(defun caddar (x) (car (cdr (cdr (car x)))))
(defun cadddr (x) (car (cdr (cdr (cdr x)))))
(defun cdaaar (x) (cdr (car (car (car x)))))
(defun cdaadr (x) (cdr (car (car (cdr x)))))
(defun cdadar (x) (cdr (car (cdr (car x)))))
(defun cdaddr (x) (cdr (car (cdr (cdr x)))))
(defun cddaar (x) (cdr (cdr (car (car x)))))
(defun cddadr (x) (cdr (cdr (car (cdr x)))))
(defun cdddar (x) (cdr (cdr (cdr (car x)))))
(defun cddddr (x) (cdr (cdr (cdr (cdr x)))))

;; This function returns the Nth element of list L (N counts from zero).
(defun nth (n l)
  (cond ((atom l) l)
	((eq n 0) (car l))
	(t (nth (- n 1) (cdr l)))))

;; A reimplementation of `cond' using a list.
;; e.g:
;; (cond* '(((cdr x) (foo))
;;	    (t (bar))))
(defun cond* (x)
  (cond ((set 'cond-r (eval (caar x)))
	 (cond ((cdar x) (progn* (cdar x)))
	       (t cond-r)))
	((cdr x) (cond* (cdr x)))
	(t nil)))

;; A reimplementation of `progn' using a list.
;; e.g.:
;; (progn* '((foo)
;;	     (bar)))
(defun progn* (x)
    (cond ((cdr x) (eval (car x)) (progn* (cdr x)))
	  (t (eval (car x)))))

;; This function substitutes occurrences of OLD with NEW in L.
(defun subst (new old l)
  (cond ((atom l) (cond ((eq l old) new) (t l)))
	(t (cons (subst new old (car l))
		 (subst new old (cdr l))))))

;; Compare two s-expressions for equality.
(defun equal (x y)
  (or (and (atom x) (atom y) (eq x y))
      (and (not (atom x))
	   (not (atom y))
	   (equal (car x) (car y))
	   (equal (cdr x) (cdr y)))))

;; Create a new list containing the elements of x and y.
(defun append (x y)
  (cond ((null x) y)
	(t (cons (car x) (append (cdr x) y)))))

;; McCarthy's `among' function (returns t or nil).
(defun member (x y)
  (and (not (null y))
       (or (equal x (car y)) (member x (cdr y)))))

;; Pair the corresponding elements of two lists.
(defun pair (x y)
  (cond ((and (null x) (null y)) nil)
	((and (not (atom x)) (not (atom y)))
	 (cons (list (car x) (car y))
	       (pair (cdr x) (cdr y))))))

;; Association list selector function.
;; y is a list of the form ((u1 v1) ... (uN vN))
;; x is one of the u's (an atom)
;; here we return the pair (u v) as in most modern Lisps
(defun assoc (x y)
  (cond ((null y) nil)
	((eq (caar y) x) (car y))
	(t (assoc x (cdr y)))))

;; x is an association list
;; (sublis x y) substitutes the values in x for the keys in y.
(defun sublis (x y)
  (cond ((atom y) (_sublis x y))
	(t (cons (sublis x (car y)) (sublis x (cdr y))))))
(defun _sublis (x z)
  (cond ((null x) z)
	((eq (caar x) z) (cadar x))
	(t (_sublis (cdr x) z))))

;; Return the last element of list e.
(defun last (e)
  (cond ((atom e) nil)
	((null (cdr e)) (car e))
	(t (last (cdr e)))))

;; Reverse a list.
(defun reverse (x)
  (_reverse x nil))
(defun _reverse (x y)
  (cond ((null x) y)
	(t (_reverse (cdr x) (cons (car x) y)))))

;; remove an element from a list.
(defun remove (e l)
  (cond ((null l) nil)
	((equal e (car l)) (remove e (cdr l)))
	(t (cons (car l) (remove e (cdr l))))))

;; find the successor of the atom x in y.
(defun succ (x y)
  (cond ((or (null y) (null (cdr y))) nil)
	((eq (car y) x) (cadr y))
	(t (succ x (cdr y)))))

;; find the predecessor of the atom x in y.
(defun pred (x y)
  (cond ((or (null y) (null (cdr y))) nil)
	((eq (cadr y) x) (car y))
	(t (pred x (cdr y)))))

;; return the elements in x up to y.
(defun before (x y)
  (cond ((atom x) nil)
	((null (cdr x)) nil)
	((equal (car x) y) nil)
	((equal (cadr x) y) (cons (car x) nil))
	(t (cons (car x) (before (cdr x) y)))))

;; return the elements in x after y.
(defun after (x y)
  (cond ((atom x) nil)
	((equal (car x) y) (cdr x))
	(t (after (cdr x) y))))

(defun union (x y)
  (cond ((null x) y)
	((member (car x) y) (union (cdr x) y))
	(t (cons (car x) (union (cdr x) y)))))

(defun intersection (x y)
  (cond ((null x) nil)
	((member (car x) y)
	 (cons (car x) (intersection (cdr x) y)))
	(t (intersection (cdr x) y))))

(defun ldifference (in out)
  (cond ((null in) nil)
	((member (car in) out) (ldifference (cdr in) out))
	(t (cons (car in) (ldifference (cdr in) out)))))

(defun subsetp (a b)
  (cond ((null a) t)
	((member (car a) b) (subsetp (cdr a) b))
	(t nil)))

(defun samesetp (a b)
	(and (subsetp a b) (subsetp b a)))
