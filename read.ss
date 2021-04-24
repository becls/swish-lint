;;; Copyright 2020 Beckman Coulter, Inc.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(library (read)
  (export
   annotation
   fp->line/char
   line/char->fp
   make-code-lookup-table
   read-code
   read-token-near
   walk-annotations
   walk-defns
   walk-defns-re
   walk-refs
   walk-refs-re
   )
  (import
   (chezscheme)
   (swish imports))

  ;; give us enough access to internal annotation record for match
  (define-syntax annotation
    (let* ([annotated (read-bytevector "sneak" (string->utf8 "123"))]
           [rtd (record-rtd (car annotated))])
      (make-compile-time-value `(_ ,rtd _ _))))

  (define (read-code code)
    ;; We make up a completely fake SFD. locate-source may attempt to
    ;; load the code from file again to compute and check a crc. We
    ;; explicitly avoid locate-source, and so we only need the SFD for
    ;; get-datum/annotations.
    (let ([sfd (make-source-file-descriptor (gensym->unique-string (gensym))
                 (open-bytevector-input-port '#vu8()))]
          [ip (open-string-input-port code)])
      ;; Explicitly skip a #! line at the beginning of the file.
      (let ([start (port-position ip)])
        (if (and (eqv? (get-char ip) #\#)
                 (eqv? (get-char ip) #\!)
                 (let ([x (peek-char ip)])
                   (or (eqv? x #\space)
                       (eqv? x #\/))))
            (let lp ()
              (let ([x (read-char ip)])
                (unless (or (eof-object? x)
                            (eqv? x #\newline))
                  (lp))))
            (set-port-position! ip start)))
      (let f ([fp (port-position ip)])
        (let-values ([(x offset) (get-datum/annotations ip sfd fp)])
          (if (= offset fp)
              '()
              (cons x (f offset)))))))

  (define (make-code-lookup-table code)
    (let ([ip (open-input-string code)])
      (let lp ([fp 0] [fps '(0)])
        (let ([ch (read-char ip)])
          (cond
           [(eof-object? ch)
            (close-input-port ip)
            (list->vector (reverse fps))]
           [(eqv? ch #\newline)
            (let ([fp (fx+ fp 1)])
              (lp fp (cons fp fps)))]
           [else
            (lp (fx+ fp 1) fps)])))))

  (define (line/char->fp table line char)
    (+ (vector-ref table (fx- line 1)) (fx- char 1)))

  (define (fp->line/char table fp)
    (let loop ([lo 0] [hi (vector-length table)])
      (if (fx= (fx+ 1 lo) hi)
          (values hi (fx+ 1 (fx- fp (vector-ref table lo))))
          (let ([mid (fxsra (fx+ lo hi) 1)])
            (if (< fp (vector-ref table mid))
                (loop lo mid)
                (loop mid hi))))))

  (define (read-token-near code table line char)
    (let ([ip (open-input-string code)]
          [start (line/char->fp table line 1)]
          [fp (line/char->fp table line char)])
      (set-port-position! ip start)
      (let lp ([lt #f] [lv #f] [lb start] [le start])
        (let-values ([(type value bfp efp) (read-token ip)])
          (cond
           [(and (<= bfp fp) (< fp efp)) (values type value bfp efp)]
           [(or (> bfp fp) (eof-object? value)) (values lt lv lb le)]
           [else (lp type value bfp efp)])))))

  (define (walk-annotations x proc)
    (let ([seen (make-eq-hashtable)])
      (let walk ([x x])
        (cond
         [(pair? x)
          (walk (car x))
          (walk (cdr x))]
         [(vector? x)
          (do ([i 0 (+ i 1)]) ((= i (vector-length x)))
            (walk (vector-ref x i)))]
         [(annotation? x)
          (let ([cell (eq-hashtable-cell seen x #f)])
            (unless (cdr cell)
              (set-cdr! cell #t)
              (proc x)
              (walk (annotation-expression x))))]))))

  (define identifier "[:*+A-Za-z0-9~&!?\\/<=>^%$@_.-]+")

  (define defn-exprs
    '("(?:meta\\s+)?(?:trace-)?define(?:-[\\S]+|\\s)?\\s+\\(?"
      "set(?:-who)?!\\s+"))

  (define defn-regexp
    ;; leading paren
    ;; non-capture, any defn form
    ;; capture, identifier
    (re (format "\\((?:~a)(~a)" (join defn-exprs #\|) identifier)))

  (define (walk-defns-re text table proc)
    (let lp ([start 0])
      (match (pregexp-match-positions defn-regexp text start)
        [(,_ (,start . ,end))
         (let ([name (substring text start end)])
           (unless (string->number name)
             (proc table name (cons start end))))
         (lp end)]
        [,_ (void)])))

  (define (walk-defns annotated-code table proc)
    (define defun-kwds
      ;; Allow (keyword (name . ,_) . ,_)
      (list 'define
        'define-syntax
        'trace-define
        'trace-define-syntax
        ))
    (define def-kwds
      ;; Allow (keyword name . ,_)
      (append defun-kwds
        (list 'define-record
          'define-record-type
          'define-state-record
          'define-tuple
          'set!
          )))
    (define (guarded name name.anno)
      (cond
       [(not (symbol? name)) (void)]
       [(not (annotation? name.anno)) (void)]
       [else (proc table name (annotation-source name.anno))]))
    (walk-annotations annotated-code
      (lambda (x)
        (match x
          [`(annotation [stripped (,keyword (,name . ,_) . ,_)]
              [expression
               (,_ `(annotation [expression (,name.anno . ,_)]) . ,_)])
           (guard (memq keyword defun-kwds))
           (guarded name name.anno)]
          [`(annotation [stripped (,keyword ,name . ,_)]
              [expression (,_ ,name.anno . ,_)])
           (guard (memq keyword def-kwds))
           (guarded name name.anno)]
          [,_ (void)]))))

  (define ref-regexp (re identifier))

  (define (walk-refs-re text table proc)
    (let lp ([start 0])
      (match (pregexp-match-positions ref-regexp text start)
        [((,start . ,end))
         (let ([name (substring text start end)])
           (unless (string->number name)
             (proc table name (cons start end))))
         (lp end)]
        [#f (void)])))

  (define (walk-refs annotated-code table proc)
    (walk-annotations annotated-code
      (lambda (x)
        (match x
          [`(annotation ,source [stripped ,name])
           (guard (symbol? name))
           (proc table name source)]
          [,_ (void)]))))
  )
