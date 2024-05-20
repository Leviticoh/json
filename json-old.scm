(define-module (json json-old))
(export json-load)

(use-modules (ice-9 textual-ports) (ice-9 rdelim))


(define json-whitespace (char-set #\space #\tab #\newline #\return))
(define json-non-whitespace (char-set->string (char-set-complement json-whitespace)))
(define json-non-digit (char-set->string (char-set-complement (char-set-adjoin char-set:digit
                                                                               #\-
                                                                               #\+
                                                                               #\.
                                                                               #\e
                                                                               #\E))))

(define (json-get-hex port)
  (string->number (get-string-n port 4) 16))

(define (json-resolve-surrogates port)
  (let ((first-unicode (json-get-hex port)))
    (if (< first-unicode #xD800)
      (string (integer->char first-unicode))
      (if (string=? (get-string-n port 2) (string #\\ #\u ))
        (string (integer->char (+ #x10000
                                  (* (- first-unicode
                                        #xD800)
                                     1024)
                                  (- (json-get-hex port)
                                     #xDC00))))
        (exit 2)))))

(define (json-string port)
  (let ((s (read-delimited (string #\" #\\) port 'split )))
    (if (char=? (cdr s) #\\)
      (let ((escape (get-char port)))
        (case escape
          ((#\") (string-append (car s) (string escape) (json-string port)))
          ((#\\) (string-append (car s) (string escape) (json-string port)))
          ((#\/) (string-append (car s) (string escape) (json-string port)))
          ((#\b) (string-append (car s) (string #\backspace) (json-string port)))
          ((#\f) (string-append (car s) (string #\ff) (json-string port)))
          ((#\n) (string-append (car s) (string #\linefeed) (json-string port)))
          ((#\r) (string-append (car s) (string #\return) (json-string port)))
          ((#\t) (string-append (car s) (string #\tab) (json-string port)))
          ((#\u) (string-append (car s) (json-resolve-surrogates port) (json-string port)))
          ))
      (car s))))

(define (json-element port)
  (if (char=? (get-char port) #\")
    (let* ((key (json-string port))
           (first-space (read-delimited json-non-whitespace port 'split )))
      (if (char=? (cdr first-space) #\:)
        (let* ((second-space (read-delimited json-non-whitespace port 'peek ))
               (value (json-load port)))
          (cons key value))
        (exit 4)))
    (exit 5)))

(define (json-object port)
  (let* ((initial-space (read-delimited json-non-whitespace port 'peek ))
         (elem (json-element port))
         (final-space (read-delimited json-non-whitespace port 'split )))
    (case (cdr final-space)
      ((#\,) (cons elem
                   (json-object port)))
      ((#\}) (cons elem
                   '()))
      (else (exit 3)))))

(define (json-array port)
  (let* ((initial-space (read-delimited json-non-whitespace port 'peek ))
         (value (json-load port))
         (final-space (read-delimited json-non-whitespace port 'split )))
    (case (cdr final-space)
      ((#\,) (cons value
                   (json-array port)))
      ((#\]) (cons value
                   '()))
      (else (exit 6)))))

(define (json-atom port)
  (let ((lettr (get-string-n port 4)))
    (cond ((string=? lettr "null")
           '())
          ((string=? lettr "true")
           #t)
          ((string=? lettr "fals")
           (let ((lettr (string-append lettr (get-string-n port 1))))
             (if (string=? lettr "false")
               #f
               (exit 7))))
          (else (exit 7)))))

(define (json-number port)
  (let* ((nstr (read-delimited json-non-digit port 'peek))
         (val (string->number nstr 10)))
    (case val
      ((#f) (exit 8))
      (else val))))

(define (json-load port)
  (let ((first (get-char port)))
    (case first
      ((#\") (json-string port))
      ((#\{) (json-object port))
      ((#\[) (json-array  port))
      (else (cond ((char-set-contains? (char-set #\t #\f #\n) first)
                   (unget-char port first)
                   (json-atom port))
                  ((char-set-contains? (char-set-adjoin char-set:digit #\-) first)
                   (unget-char port first)
                   (json-number port))
                  (else (exit 1)))))))
