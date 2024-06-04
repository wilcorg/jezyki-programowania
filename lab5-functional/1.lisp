(defun binomial (n k)
  (cond
    ((or (= k 0) (= k n)) 1)
    ((or (< k 0) (> k n)) 0)
    (t (+ (binomial(- n 1) (- k 1)) (binomial (- n 1) k)))
  ))

(print (binomial 5 2))

;;

(defun next_row (row)
  (mapcar #'+ (append row '(0)) (append '(0) row)))

(defun generate_pascal (n)
  (let ((rows '((1))))
    (loop for i from 1 to n do
         (push (next_row (first rows)) rows))
    (nreverse rows)))

(defun binomial2 (n k)
  (nth k (nth n (generate_pascal n))))

(print (binomial2 5 2))

;;

(defun _merge (left right)
  (cond
    ((null left) right)
    ((null right) left)
    ((<= (car left) (car right))
     (cons (car left) (_merge (cdr left) right)))
    (t (cons (car right) (_merge left (cdr right))))))

(defun split-list (list)
  (let ((middle (floor (/ (length list) 2))))
    (values (subseq list 0 middle)
            (subseq list middle))))

(defun mergesort (list)
  (if (or (null list) (null (cdr list)))
      list
      (multiple-value-bind (left right) (split-list list)
        (_merge (mergesort left) (mergesort right)))))

(print (mergesort '(5 4 3 2 1)))

;;

(defun de (a b z)
  (labels ((extended_gcd (a b)
             (if (= a 0)
                 (values b 0 1)
                 (multiple-value-bind (g x y)
                     (extended_gcd (mod b a) a)
                   (values g (- y (* (floor b a) x)) x)))))
    (multiple-value-bind (g x y)
        (extended_gcd a b)
      (if (= z g)
          (values x y z)
          (error "No solution")))))

(print (de 87 -64 1))

;;

(defun prime_factors (n)
  (labels ((factorize (n divisor)
             (cond ((= n 1) nil)
                   ((zerop (mod n divisor))
                    (cons divisor (factorize (/ n divisor) divisor)))
                   (t (factorize n (1+ divisor))))))
    (factorize n 2)))

(print (prime_factors 100))

;;

(defun totient (n)
  (let ((count 0))
    (dotimes (i n count)
      (when (= (gcd (1+ i) n) 1)
        (incf count)))))

(print (totient 110))

;;

(defun totient2 (n)
  (let* ((factors (remove-duplicates (prime_factors n)))
         (result n))
    (dolist (p factors result)
      (setf result (* result (- 1 (/ 1.0 p)))))
    (floor result)))


(print (totient2 110))

;;

(defun is_prime (n)
  (cond ((<= n 1) nil)
        ((= n 2) t)
        ((= n 3) t)
        (t
         (let ((sqrt-n (isqrt n))
               (prime t))
           (do ((i 2 (1+ i)))
               ((or (> i sqrt-n) (not prime)))
             (when (= (mod n i) 0)
               (setf prime nil)))
            prime))))

(defun primes (n)
  (let ((result '()))
    (do ((i 2 (1+ i)))
        ((> i n) (reverse result))
      (when (is_prime i)
        (push i result)))))

(print (primes 110))
