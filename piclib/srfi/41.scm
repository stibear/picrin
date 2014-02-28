(define-library (streams Primitive)
  (import (scheme base)
	  (scheme cxr)
	  (picrin macro))

  (define-record-type stream-type
    (make-stream box)
    stream?
    (box stream-promise stream-promise!))

  (define-syntax stream-lazy
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((expr (cdr form)))
	 `(make-stream (cons 'lazy (lambda () ,expr)))))))

  (define (stream-eager expr)
    (make-stream
     (cons 'eager expr)))

  (define-syntax stream-delay
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((expr (cdr form)))
	 `(stream-lazy (stream-eager ,expr))))))

  (define (stream-force promise)
    (let ((content (stream-promise promise)))
      (case (car content)
	((eager) (cdr content))
	((lazy) (let* ((promise* ((cdr content)))
		       (content (stream-promise promise)))
		  (if (not (eqv? (car content) 'eager))
		      (begin (set-car! content (car (stream-promise promise*)))
			     (set-cdr! content (cdr (stream-promise promise*)))
			     (stream-promise! promise* content)))
		  (stream-force promise))))))

  (define stream-null (stream-delay (cons 'stream 'null)))

  (define-record-type stream-pair-type
    (make-stream-pair x y)
    stream-pair?%
    (x stream-car%)
    (y stream-cdr%))

  (define (stream-pair? obj)
    (and (stream? obj) (stream-pair?% (stream-force obj))))

  (define (stream-null? obj)
    (and (stream? obj)
	 (eqv?)))
  
  (export stream-null stream-cons stream? stream-null? stream-pair?
	  stream-car stream-cdr stream-lambda))
