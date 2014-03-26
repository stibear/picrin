(define-library (streams primitive)
  (import (scheme base)
	  (picrin macro)
	  (srfi 1))

  (define-record-type stream-type
    (make-stream box)
    stream?
    (box stream-promise stream-promise!))

  (define-syntax stream-lazy
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((expr (second form)))
	 `(make-stream (cons 'lazy (lambda () ,expr)))))))

  (define (stream-eager expr)
    (make-stream
     (cons 'eager expr)))

  (define-syntax stream-delay
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((expr (second form)))
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
	 (eqv? (stream-force obj)
	       (stream-force stream-null))))

  (define-syntax stream-cons
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((obj (second form))
	     (strm (third form)))
	 `(stream-eager (make-stream-pair (stream-delay ,obj)
					  (stream-lazy ,strm)))))))

  (define (stream-car strm)
    (cond ((not (stream? strm))
	   (error "non-stream" strm))
	  ((stream-null? strm)
	   (error "null stream" strm))
	  (else
	   (stream-force (stream-car% (stream-force strm))))))

  (define (stream-cdr strm)
    (cond ((not (stream? strm))
	   (error "non-stream" strm))
	  ((stream-null? strm)
	   (error "null stream" strm))
	  (else
	   (stream-cdr% (stream-force strm)))))

  (define-syntax stream-lambda
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((formals (second form))
	     (body (cddr form)))
	 `(lambda ,formals (stream-lazy (let () ,@body)))))))
  
  (export stream-null stream-cons stream? stream-null? stream-pair?
	  stream-car stream-cdr stream-lambda))

(define-library (streams derived)
  (import (scheme base)
	  (scheme cxr)
	  (picrin macro)
	  (srfi 1)
	  (streams primitive))

  (define-syntax define-stream
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((name (car (second form)))
	     (formal (cdr (second form)))
	     (body (cddr form)))
	 `(define ,name (stream-lambda ,formal ,@body))))))

  (define (list->stream objs)
    (letrec ((loop
	      (stream-lambda (objs)
		(if (null? objs)
		    stream-null
		    (stream-cons (car objs) (loop (cdr objs)))))))
      (if (not (list? objs))
	  (error "non-list argument" objs)
	  (loop objs))))

  (define (port->stream . port)
    (letrec ((loop
	      (stream-lambda (p)
		(let ((c (read-char p)))
		  (if (eof-object? c)
		      stream-null
		      (stream-cons c (loop p)))))))
      (let ((p (if (null? port) (current-input-port) (car port))))
	(if (not (input-port? p))
	    (error "non-input-port argument" p)
	    (loop p)))))

  (define-syntax stream
    (ir-macro-transformer
     (lambda (form inject compare?)
       (if (null? (cdr form))
	   'stream-null
	   (let ((x (second form))
		 (rst (cddr form)))
	     `(stream-cons ,x (stream ,@rst)))))))

  (define (stream->list . args)
    (let ((n (if (= 1 (length args)) #f (car args)))
	  (strm (if (= 1 (length args)) (car args) (cadr args))))
      (cond ((not (stream? strm))
	     (error "non-stream argument" strm))
	    ((and n (not (integer? n)))
	     (error "non-integer count" n))
	    ((and n (negative? n))
	     (error "negative count" n))
	    (else
	     (let loop ((n (if n n -1)) (strm strm))
	       (if (or (zero? n) (stream-null? strm))
		   '()
		   (cons (stream-car strm)
			 (loop (- n 1) (stream-cdr strm)))))))))

  (define (stream-append . strms)
    (letrec ((loop
	      (stream-lambda (strms)
		(cond ((null? (cdr strms)) (car strms))
		      ((stream-null? (car strms))
		       (loop (cdr strms)))
		      (else
		       (stream-cons (stream-car (car strms))
				    (loop (cons (stream-cdr (car strms))
						(cdr strms)))))))))
      (cond ((null? strms) stream-null)
	    ((any (lambda (x) (not (stream? x))) strms)
	     (error "non-stream argument" strms))
	    (else (loop strms)))))

  (define (stream-concat strms)
    (letrec ((loop
	      (stream-lambda (strms)
		(cond ((stream-null? strms) stream-null)
		      ((not (stream? (stream-car strms)))
		       (error "non-stream object in input stream" strms))
		      ((stream-null? (stream-car strms))
		       (loop (stream-cdr strms)))
		      (else
		       (stream-cons
			(stream-car (stream-car strms))
			(loop (stream-cons (stream-cdr (stream-car strms))
					  (stream-cdr strms)))))))))
      (if (not (stream? strms))
	  (error "non-stream argument" strms)
	  (loop strms))))

  (define stream-constant
    (stream-lambda objs
      (cond ((null? objs) stream-null)
	    ((null? (cdr objs))
	     (stream-cons (car objs) (stream-constant (car objs))))
	    (else
	     (stream-cons (car objs)
			  (apply stream-constant
				 (append (cdr objs) (list (car objs)))))))))

  (define (stream-drop n strm)
    (letrec ((loop
	      (stream-lambda (n strm)
		(if (or (zero? n) (stream-null? strm))
		    strm
		    (loop (- n 1) (stream-cdr strm))))))
      (cond ((not (integer? n))
	     (error "non-integer argument" n))
	    ((negative? n)
	     (error "negative argument" n))
	    ((not (stream? strm))
	     (error "non-stream argument" strm))
	    (else (loop strm)))))

  (define (stream-drop-while pred? strm)
    (letrec ((loop
	      (stream-lambda (strm)
		(if (and (stream-pair? strm) (pred? (stream-car strm)))
		    (loop (stream-cdr strm))
		    strm))))
      (cond ((not (procedure? pred?))
	     (error "non-procedural argument" pred?))
	    ((not (stream? strm))
	     (error "non-stream argument" strm))
	    (else (loop strm)))))

  (define (stream-filter pred? strm)
    (letrec ((loop
	      (stream-lambda (strm)
		(cond ((stream-null? strm) stream-null)
		      ((pred? (stream-car strm))
		       (stream-cons (stream-car strm)
				    (loop (stream-cdr strm))))
		      (else (loop (stream-cdr strm)))))))
      (cond ((not (procedure? pred?))
	     (error "non-procedural argument" pred?))
	    ((not (stream? strm))
	     (error "non-stream argument" strm))
	    (else (loop strm)))))

  (define (stream-fold proc base strm)
    (cond ((not (procedure? proc))
	   (error "non-procedural argment" proc))
	  ((not (stream? strm))
	   (error "non-stream argument" strm))
	  (else
	   (let loop ((base base) (strm strm))
	     (if (stream-null? strm)
		 base
		 (loop (proc (base (stream-car strm))
			     (stream-cdr strm))))))))

  (define (stream-for-each proc . strms)
    (letrec ((loop
	      (lambda (strms)
		(if (not (any stream-null? strms))
		    (begin (apply proc (map stream-car strms))
			   (stream-for-each (map stream-cdr strms)))))))
      (cond ((not (procedure? proc))
	     (error "non-procedural argument" proc))
	    ((null? strms)
	     (error "no stream arguments" strms))
	    ((any (lambda (x) (not (stream? x))) strms)
	     (error "non-stream argument" strms))
	    (else (stream-for-each strms)))))

  (define (stream-from fst . step)
    (letrec ((loop
	      (stream-lambda (fst delta)
		(stream-cons fst (loop (+ fst delta) delta)))))
      (let ((delta (if (null? step) 1 (car step))))
	(cond ((not (number? fst))
	       (error "non-numeric starting number" fst))
	      ((not (number? delta))
	       (error "nen-numeric starting step size" delta))
	      (else (loop fst delta))))))

  (define (stream-iterate proc base)
    (letrec ((loop
	      (stream-lambda (base)
		(stream-cons base (loop (proc base))))))
      (if (not (procedure? proc))
	  (error "non-procedural argument" proc)
	  (loop base))))

  (define (stream-length strm)
    (if (not (stream? strm))
	(error "non-stream argument" strm)
	(let loop ((len 0) (strm strm))
	  (if (stream-null? strm)
	      len
	      (loop (+ len 1) (stream-cdr strm))))))

  (define-syntax stream-let
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((tag (second form))
	     (names (map first (third form)))
	     (vals (map second (third form)))
	     (body (cdddr form)))
	 `((letrec ((,tag (stream-lambda ,names ,@body)))
	     ,tag)
	   ,@vals)))))

  (define (stream-map proc . strms)
    (letrec ((loop
	      (stream-lambda (strms)
		(if (any stream-null? strms)
		    stream-null
		    (stream-cons (apply proc (map stream-car strms))
				 (loop (map stream-cdr strms)))))))
      (cond ((not (procedure? proc))
	     (error "non-procedural argument" proc))
	    ((null? strms)
	     (error "no stream arguments" strms))
	    ((any (lambda (x) (not (stream? x))) strms)
	     (error "non-stream argument" strms))
	    (else (loop strms)))))

  (define-syntax stream-match
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((strm-expr (second form))
	     (clauses (cddr form)))
	 `(let ((strm ,strm-expr))
	    (cond ((not (stream? strm))
		   (error "non-stream argument" strm))
		  ,@(map (lambda (clause)
			   `((stream-match-test strm ,clause) => car))
			 clauses)
		  (else (error "pattern failure"))))))))

  (define-syntax stream-match-test
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((strm (second form))
	     (clause (third form)))
	 (if (= 3 (length clause))
	     `(stream-match-pattern ,strm ,(first clause) ()
				    (and ,(second clause)
					 (list ,(third clause))))
	     `(stream-match-pattern ,strm ,(first clause) ()
				    (list ,(second clause))))))))

  (define-syntax stream-match-pattern
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((strm (second form))
	     (acc (third form))
	     (bindings (fourth form))
	     (body (cddddr form)))
	 (cond ((null? acc)
		`(and (stream-null? ,strm) (let ,bindings ,body)))
	       ((and (list? acc) (compare? (car acc) '_))
		`(and (stream-pair? ,strm)
		      (let ((strm (stream-cdr ,strm)))
			(stream-match-pattern strm ,(cdr acc) ,bindings ,body))))
	       ((list? acc)
		`(and (stream-pair? ,strm)
		      (let ((temp (stream-car ,strm)) (strm (stream-cdr ,strm)))
			(stream-match-pattern strm ,(cdr acc)
					      ((,(car acc) temp) ,@bindings)
					      body))))
	       ((compare? acc '_)
		`(let ,bindings ,body))
	       (else
		`(let ((,acc ,strm) ,@bindings) ,body)))))))

  (define-syntax stream-of
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((expr (second form))
	     (rst (cddr form)))
	 `(stream-of-aux ,expr stream-null ,@rst)))))

  (define-syntax stream-of-aux
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((expr (second form))
	     (base (third form)))
	 (if (= 3 (length form))
	     `(stream-cons ,expr ,base)
	     (let ((clause (fourth form))
		   (rests (cddddr form)))
	       (cond ((and (list? clause) (compare? (second clause) 'in))
		      `(stream-let loop ((strm ,(third clause)))
			 (if (stream-null? strm)
			     ,base
			     (let ((,(first clause) (stream-car strm)))
			       (stream-of-aux ,expr
					      (loop (stream-cdr strm))
					      ,@rests)))))
		     ((and (list? clause) (compare? (second clause) 'is))
		      `(let ((,(first clause) ,(third clause)))
			 (stream-of-aux ,expr ,base ,@rests)))
		     (else
		      `(if ,clause
			   (stream-of-aux ,expr ,base ,@rests)
			   ,base)))))))))

  (define (stream-range fst past . step)
    (letrec ((loop
	      (stream-lambda (fst past delta lt?)
		(if (lt? fst past)
		    (stream-cons fst (loop (+ fst delta) past delta lt?))
		    stream-null))))
      (cond ((not (number? fst))
	     (error "non-numeric starting number" fst))
	    ((not (number? past))
	     (error "non-numeric ending number" past))
	    (else
	     (let ((delta (cond ((pair? step) (car step))
				((< fst past) 1)
				(else -1))))
	       (if (not (number? delta))
		   (error "non-numeric step size" delta)
		   (let ((lt? (if (< 0 delta) < >)))
		     (loop fst past delta lt?))))))))

  (define (stream-ref strm n)
    (cond ((not (stream? strm))
	   (error "non-stream argument" strm))
	  ((not (integer? n))
	   (error "non-integer argument" n))
	  ((negative? n)
	   (error "negative argument" n))
	  (else
	   (let loop ((strm strm) (n n))
	     (cond ((stream-null? strm)
		    (error "beyond end of stream" strm))
		   ((zero? n) (stream-car strm))
		   (else (loop (stream-cdr strm) (- n 1))))))))

  (define (stream-reverse strm)
    (letrec ((loop
	      (stream-lambda (strm rev)
	        (if (stream-null? strm)
		    rev
		    (loop (stream-cdr strm)
				    (stream-cons (stream-car strm) rev))))))
      (if (not (stream? strm))
	  (error "non-stream argument" strm)
	  (loop strm stream-null))))

  (define (stream-scan proc base strm)
    (letrec ((loop
	      (stream-lambda (base strm)
		(if (stream-null? strm)
		    (stream base)
		    (stream-cons base (loop (proc base (stream-car strm))
					    (stream-cdr strm)))))))
      (cond ((not (procedure? proc))
	     (error "non-procedural argument" proc))
	    ((not (stream? strm))
	     (error "non-stream argument" strm))
	    (else (loop base strm)))))

  (define (stream-take n strm)
    (letrec ((loop
	      (stream-lambda (n strm)
		(if (or (stream-null? strm) (zero? n))
		    stream-null
		    (stream-cons (stream-car strm)
				 (loop (- n 1) (stream-cdr strm)))))))
      (cond ((not (stream? strm))
	     (error "non-stream argument" strm))
	    ((not (integer? n))
	     (error "non-integer argument" n))
	    ((negative? n)
	     (error "negative argument" n))
	    (else (loop n strm)))))

  (define (stream-take-while pred? strm)
    (letrec ((loop
	      (stream-lambda (strm)
		(cond ((stream-null? strm) stream-null)
		      ((pred? (stream-car strm))
		       (stream-cons (stream-car strm)
				    (loop (stream-cdr strm))))
		      (else stream-null)))))
      (cond ((not (stream? strm))
	     (error "non-stream argument" strm))
	    ((not (procedure? pred?))
	     (error "non-procedural argument" pred?))
	    (else (loop strm)))))

  (define (stream-unfold mapper pred? generator base)
    (letrec ((loop
	      (stream-lambda (base)
	        (if (pred? base)
		    (stream-cons (mapper base) (loop (generator base)))
		    stream-null))))
      (cond ((not (procedure? mapper))
	     (error "non-procedural mapper" mapper))
	    ((not (procedure? pred?))
	     (error "non-procedural pred?" pred?))
	    ((not (procedure? generator))
	     (error "non-procedural generator" generator))
	    (else (stream-unfold base)))))

  (define (stream-unfolds gen seed)
    (letrec ((len-values
	      (lambda (gen seed)
		(call-with-values
		    (lambda () (gen seed))
		  (lambda vs (- (length vs) 1)))))
	     (unfold-result-stream
	      (stream-lambda (gen seed)
		(call-with-values
		    (lambda () (gen seed))
		  (lambda (next . results)
		    (stream-cons results (unfold-result-stream gen next))))))
	     (result-stream->output-stream
	      (stream-lambda (result-stream i)
		(let ((result (list-ref (stream-car result-stream) (- i 1))))
		  (cond ((pair? result)
			 (stream-cons
			  (car result)
			  (result-stream->output-stream
			   (stream-cdr result-stream) i)))
			((not result)
			 (result-stream->output-stream
			  (stream-cdr result-stream) i))
			((null? result) stream-null)
			(else (error "cannot happen"))))))
	     (result-stream->output-streams
	      (let loop ((i (len-values gen seed)) (outputs '()))
		(if (zero? i)
		    (apply values outputs)
		    (loop (- i 1) (cons (result-stream->output-stream i)
					outputs))))))
      (if (not (procedure? gen))
	  (error "non-procedural argument" gen)
	  (result-stream->output-streams (unfold-result-stream gen seed)))))

  (define (stream-zip . strms)
    (letrec ((loop
	      (stream-lambda (strms)
		(if (any stream-null? strms)
		    stream-null
		    (stream-cons (map stream-car strms)
				 (loop (map stream-cdr strms)))))))
      (cond ((null? strms)
	     (error "no stream argments" strms))
	    ((any (lambda (x) (not (stream? x))) strms)
	     (error "non-stream argument" strms))
	    (else (loop strms)))))
  
  (export stream-null stream-cons stream? stream-null? stream-pair? stream-car
          stream-cdr stream-lambda define-stream list->stream port->stream stream
          stream->list stream-append stream-concat stream-constant stream-drop
          stream-drop-while stream-filter stream-fold stream-for-each stream-from
          stream-iterate stream-length stream-let stream-map stream-match _
          stream-of stream-range stream-ref stream-reverse stream-scan
	  stream-take stream-take-while stream-unfold stream-unfolds stream-zip))

(define-library (srfi 41)
  (import (streams primitive)
	  (streams derived))
  (export stream-null stream-cons stream? stream-null? stream-pair? stream-car
          stream-cdr stream-lambda define-stream list->stream port->stream stream
          stream->list stream-append stream-concat stream-constant stream-drop
          stream-drop-while stream-filter stream-fold stream-for-each stream-from
          stream-iterate stream-length stream-let stream-map stream-match _
          stream-of stream-range stream-ref stream-reverse stream-scan stream-take
          stream-take-while stream-unfold stream-unfolds stream-zip))
