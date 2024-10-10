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
   fp->line
   fp->line/char
   get-symbol-name
   line/char->fp
   make-code-lookup-table
   read-code
   read-token-near/col
   read-token-near/fp
   reason->line/msg
   walk-annotations
   walk-defns
   walk-defns-re
   walk-refs
   walk-refs-re
   )
  (import
   (chezscheme)
   (config-params)
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

  (define (fp->line table fp)
    (let-values ([(line char) (fp->line/char table fp)])
      line))

  (define (reason->line/msg reason code/table)
    (let ([msg (exit-reason->english reason)])
      (match (pregexp-match (re "[^:]*:\\s*((.*) (?:at|near) (line|char) (\\d+))") msg)
        [(,_ ,_ ,msg "line" ,line)
         (values (string->number line) msg)]
        [(,_ ,_ ,msg "char" ,fp)
         (guard code/table)
         (values
          (fp->line
           (if (string? code/table)
               (make-code-lookup-table code/table)
               code/table)
           (string->number fp))
          msg)]
        [(,_ ,msg . ,_) (values 1 msg)]
        [,_ (values 1 msg)])))

  (define (get-symbol-name x)
    (define (clean? s)
      (let ([len (string-length s)])
        (let lp ([i 0])
          (cond
           [(fx= i len) #t]
           [(char-whitespace? (string-ref s i)) #f]
           [else (lp (fx1+ i))]))))
    (cond
     [(gensym? x) (parameterize ([print-gensym #t]) (format "~s" x))]
     [(symbol? x)
      (let ([s (symbol->string x)])
        (if (clean? s)
            s
            (format "|~a|" s)))]
     [else x]))

  (define (read-token-near/col str col1)
    (read-token-near/fp str (fx- col1 1)))

  (define (read-token-near/fp str fp)
    (let ([ip (open-input-string str)])
      (let lp ([lt #f] [lv #f] [lb 0] [le 0])
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

  (define (defn-exprs)
    (list
     (format "(?:~a\\s+)?(?:trace-)?(?:~a)(?:-[\\S]+|\\s)?\\s+\\(?"
       identifier
       (join (cons "define" (map pregexp-quote (config:definition-keywords))) #\|))
     "set(?:-who)?!\\s+"))

  (define (defn-regexp)
    ;; leading paren
    ;; non-capture, any defn form
    ;; capture, identifier
    (re (format "\\((?:~a)(~a)" (join (defn-exprs) #\|) identifier)))

  (define (walk-defns-re text table proc)
    (define defn-re (defn-regexp))
    (let lp ([start 0])
      (match (pregexp-match-positions defn-re text start)
        [(,_ (,start . ,end))
         (let ([name (substring text start end)])
           (unless (string->number name)
             (proc table name (cons start end))))
         (lp end)]
        [,_ (void)])))

  (define (walk-defns annotated-code table proc)
    (define defines (join (cons "define" (map pregexp-quote (config:definition-keywords))) #\|))
    (define defun-match-regexp
      (re (format "^(?:trace-)?(?:~a)(?:-[\\S]+)?" defines)))
    (define def-match-regexp
      (re (format "^(?:trace-)?(?:~a)(?:-[\\S]+)?|^set(?:-who)?!" defines)))
    (define local-keywords (make-eq-hashtable))
    (define (keyword? keyword mre)
      (and (symbol? keyword)
           (or (eq-hashtable-ref local-keywords keyword #f)
               (let ([str (symbol->string keyword)])
                 (match (pregexp-match mre str)
                   [#f #f]
                   [,_
                    (eq-hashtable-set! local-keywords keyword #t)
                    #t])))))
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
           (guard (keyword? keyword defun-match-regexp))
           (guarded name name.anno)]
          [`(annotation [stripped (,keyword ,name . ,_)]
              [expression (,_ ,name.anno . ,_)])
           (guard (keyword? keyword def-match-regexp))
           (guarded name name.anno)]
          [`(annotation [stripped (,meta ,keyword (,name . ,_) . ,_)]
              [expression
               (,_ ,_ `(annotation [expression (,name.anno . ,_)]) . ,_)])
           (guard (and (symbol? meta) (keyword? keyword defun-match-regexp)))
           (guarded name name.anno)]
          [`(annotation [stripped (,meta ,keyword ,name . ,_)]
              [expression (,_ ,_ ,name.anno . ,_)])
           ;; Use defun here because set! is not valid with meta.
           (guard (and (symbol? meta) (keyword? keyword defun-match-regexp)))
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
