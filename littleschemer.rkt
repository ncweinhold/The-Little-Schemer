(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (e lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) e) 
                (member? e (cdr lat)))))))

(define rember
  (lambda (e lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) e) (cdr lat))
      (else (cons (car lat) (rember e (cdr lat)))))))