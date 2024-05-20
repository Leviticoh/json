(define-module (json json))

(use-modules (ice-9 pretty-print))

(export json-split
        json-parse)

(define (json-len t)
  (cond ((or (eq? (car t) 'string)
             (eq? (car t) 'symbol)
             (eq? (car t) 'number))
         (string-length (cdr t)))
        
        ((or (eq? (car t) 'array)
             (eq? (car t) 'object)
             (eq? (car t) 'item))
         (cadr t))))

(define (json-split s)
  (case (string-ref s 0)
    ((#\") (cons 'string (substring/read-only s 0 (json-len/string s))))
    ((#\t
      #\f
      #\n) (cons 'symbol (substring/read-only s 0 (json-len/symbol s))))
    ((#\[) (cons 'array  (json-split/array  s)))
    ((#\{) (cons 'object (json-split/object s)))
    (else (if (char-set-contains? (char-set-adjoin char-set:digit #\-)
                                  (string-ref s 0))
            (cons 'number (substring/read-only s 0 (json-len/number s)))
            (exit 1)))))

(define (json-len/string s)
  (+ 2 (__json-len/string__ (substring/read-only s 1) 0)))

(define (__json-len/string__ s n)
  (case (string-ref s 0)
    ((#\") n)
    ((#\\) (__json-len/string__ (substring/read-only s 2) (+ n 2)))
    (else (__json-len/string__ (substring/read-only s 1) (+ n 1)))))

(define (json-len/symbol s)
  (__json-len/symbol__ s 0))

(define (__json-len/symbol__ s n)
  (if (and (not (string-null? s))
           (char-set-contains? char-set:lower-case (string-ref s 0)))
    (__json-len/symbol__ (substring/read-only s 1) (+ n 1))
    n))

(define (json-len/number s)
  (__json-len/number__ s 0))

(define (__json-len/number__ s n)
  (if (and (not (string-null? s))
           (char-set-contains? (char-set-adjoin char-set:digit #\- #\+ #\. #\e #\E) (string-ref s 0)))
    (__json-len/number__ (substring/read-only s 1) (+ n 1))
    n))

(define (json-split/array s)
  (let ((res (__json-split/array__ (substring/read-only s 1) '() 0)))
    (cons (+ (car res) 1) (reverse (cdr res)))))

(define (__json-split/array__ s t n)
  (case (string-ref s 0)
    ((#\space #\tab #\newline #\return #\,) (__json-split/array__ (substring/read-only s 1) t (+ n 1)))
    ((#\]) (cons (+ n 1) t))
    (else (let ((elem (json-split s)))
            (__json-split/array__ (substring/read-only s (json-len elem))
                                  (cons elem t)
                                  (+ n (json-len elem)))))))

(define (json-split/object s)
  (let ((res (__json-split/object__ (substring/read-only s 1) '() 0)))
    (cons (+ (car res) 1) (reverse (cdr res)))))

(define (__json-split/object__ s t n)
  (case (string-ref s 0)
    ((#\space #\tab #\newline #\return #\,) (__json-split/object__ (substring/read-only s 1) t (+ n 1)))
    ((#\}) (cons (+ n 1) t))
    (else (let ((elem (json-split/item s)))
            (__json-split/object__ (substring/read-only s (json-len elem))
                                  (cons elem t)
                                  (+ n (json-len elem)))))))

(define (json-split/item s)
  (__json-split/item__ s '() '() 0))

(define (__json-split/item__ s k v n)
  (if (not (or (null? k)
               (null? v)))
    (cons 'item
          (cons n
                (cons k
                      v)))
    (case (string-ref s 0)
      ((#\space #\tab #\newline #\return #\:) (__json-split/item__ (substring/read-only s 1) k v (+ n 1)))
      (else (let ((val (json-split s)))
              (if (null? k)
                (__json-split/item__ (substring/read-only s (json-len val)) val v (+ n (json-len val)))
                (__json-split/item__ (substring/read-only s (json-len val)) k val (+ n (json-len val)))))))))

(define (json-parse tree)
  (cond ((eq? (car tree) 'string) (json-parse/string tree))
        ((eq? (car tree) 'symbol) (json-parse/symbol tree))
        ((eq? (car tree) 'number) (json-parse/number tree))
        ((eq? (car tree) 'array)  (json-parse/array  tree))
        ((eq? (car tree) 'object) (json-parse/object tree))
        ((eq? (car tree) 'item)   (json-parse/item   tree))))

(define str car)
(define esc cadr)
(define cmd caddr)
(define arg cadddr)
(define (cod a) (cadddr (cdr a)))

#!
(define (listqq a b c d e)
  (let ((l (list a b c d e)))
    (pretty-print l)
    l))
!#

(define listqq list)

(define (escape chr state)
  (cond ((and (char=? chr #\\)
              (null? (esc state)))
         (listqq (str state) #\\ '() '() (cod state)))
        ((and (not (null? (esc state)))
              (null? (cmd state))
              (char=? chr #\u))
         (listqq (str state) (esc state) chr '() (cod state)))
        ((and (not (null? (esc state)))
              (not (null? (cmd state)))
              (char=? (cmd state) #\u))
         (if (or (null? (arg state))
                 (< (length (arg state)) 3))
           (listqq (str state) (esc state) (cmd state) (cons chr (arg state)) (cod state))
           (let ((code (string->number (list->string (reverse (cons chr (arg state)))) 16)))
             (if (null? (cadddr (cdr state)))
               (if (>= code #xD800)
                 (listqq (str state) '() '() '() code)
                 (listqq (string-append (str state) (string (integer->char code))) '() '() '() (cod state)))
               (listqq (string-append (str state) (string (integer->char (+ #x10000
                                                                            (* (- (cadddr (cdr state))
                                                                                  #xD800)
                                                                               1024)
                                                                            (- code
                                                                               #xDC00))))) '() '() '() '())))))
        ((and (not (null? (esc state)))
              (null? (cmd state)))
         (case chr
           ((#\"
             #\\
             #\/) (listqq (string-append (str state) (string chr)) '() '() '() '()))
           ((#\b) (listqq (string-append (str state) (string #\backspace)) '() '() '() '()))
           ((#\f) (listqq (string-append (str state) (string #\ff)) '() '() '() '()))
           ((#\n) (listqq (string-append (str state) (string #\linefeed)) '() '() '() '()))
           ((#\r) (listqq (string-append (str state) (string #\return)) '() '() '() '()))
           ((#\t) (listqq (string-append (str state) (string #\tab)) '() '() '() '()))))
        (else (listqq (string-append (str state) (string chr)) '() '() '() '()))))

(define (json-parse/string tree)
  (car (string-fold escape
                    (list "" '() '() '() '())
                    (string-drop (string-drop-right (cdr tree) 1) 1))))

(define (json-parse/symbol tree)
  (let ((val (cdr tree)))
    (cond
      ((string=? val "null") '())
      ((string=? val "true") #t)
      ((string=? val "false") #f))))

(define (json-parse/array tree)
  (cons 'array
        (map json-parse (cddr tree))))

(define (json-parse/object tree)
  (cons 'object
        (map json-parse/item (cddr tree))))

(define (json-parse/item tree)
  (cons (json-parse (caddr tree))
        (json-parse (cdddr tree))))

(define (json-parse/number tree)
  (string->number (cdr tree)))
