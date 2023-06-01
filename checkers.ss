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

(library (checkers)
  (export
   check-import/export
   check-line-whitespace
   make-external-checker
   make-regexp-checker
   run-optional-checkers
   )
  (import
   (chezscheme)
   (config-params)
   (os-process)
   (read)
   (swish imports)
   (tower-client)
   (trace))

  (define (run-optional-checkers uri skip-delay? annotated-code code report)
    (for-each
     (lambda (check) (check uri skip-delay? annotated-code code report))
     (optional-checkers)))

  (define (check-import/export x report)
    (let lp ([x x])
      (match x
        [`(annotation
           [stripped (library ,_ (export . ,_exports) . ,_rest)]
           [expression (,_library ,_
                        `(annotation
                          [expression (,export.anno . ,exports.anno)])
                        . ,rest)])
         (let* ([exports.anno
                 (filter
                  (lambda (x)
                    (match x
                      [`(annotation [stripped (rename . ,_)]) #f]
                      [,_ #t]))
                  exports.anno)]
                [exports (map annotation-stripped exports.anno)]
                [exports.orig (map (lambda (x) (format "~s" x)) exports)]
                [exports.sorted (sort string<? exports.orig)])
           (unless (equal? exports.orig exports.sorted)
             (report export.anno 'warning "should sort exports")
             (for-each
              (lambda (anno l r)
                (unless (equal? l r)
                  (report anno 'info "incorrectly sorted: ~a" l)))
              exports.anno exports.orig exports.sorted)))
         (lp rest)]
        [`(annotation
           [stripped (import . ,_imports)]
           [expression (,import.anno . ,imports.anno)])
         (let* ([imports.anno
                 (filter
                  (lambda (x)
                    (match x
                      [`(annotation [stripped (,first . ,_)])
                       (guard (memq first '(only except prefix rename)))
                       #f]
                      [,_ #t]))
                  imports.anno)]
                [imports (map annotation-stripped imports.anno)]
                [imports.orig (map (lambda (x) (format "~s" x)) imports)]
                [imports.sorted (sort string<? imports.orig)])
           (unless (equal? imports.orig imports.sorted)
             (report import.anno 'warning "should sort imports")
             (for-each
              (lambda (ann l r)
                (unless (equal? l r)
                  (report ann 'info "incorrectly sorted: ~a" l)))
              imports.anno imports.orig imports.sorted)))]
        [(,first . ,rest)
         (lp first)
         (lp rest)]
        [,_ (void)])))

  (define (check-line-whitespace text collapse? report)
    (define (yucky rlines text)
      (let ([lines (reverse rlines)])
        (cond
         [collapse?
          (match lines
            [() (void)]
            [(,line . ,_)
             (report line 'error "~a~@[ (~a times)~]" text
               (let ([len (length lines)])
                 (and (> len 1) len)))])]
         [else
          (for-each
           (lambda (line)
             (report line 'error text))
           lines)])))
    (define tabs '())
    (define dos '())
    (define ws '())
    (do ([ln 1 (+ ln 1)]
         [lines (split text #\newline) (cdr lines)])
        ((null? lines))
      (let* ([line (car lines)]
             [len (string-length line)])
        (when (> len 0)
          (let lp ([i 0])
            (when (< i len)
              (if (char=? (string-ref line i) #\tab)
                  (set! tabs (cons ln tabs))
                  (lp (+ i 1)))))
          (let ([last-char (string-ref line (- len 1))])
            (cond
             [(char=? last-char #\return)
              (set! dos (cons ln dos))]
             [(char-whitespace? last-char)
              (set! ws (cons ln ws))])))))
    (yucky tabs "undesirable tab character")
    (yucky dos "undesirable DOS line ending")
    (yucky ws "undesirable trailing whitespace"))

  (define (make-regexp-checker type regexp)
    (define compiled-regexp (pregexp regexp))
    (lambda (uri skip-delay? annotated-code code report)
      (do ([ln 1 (+ ln 1)]
           [lines (split code #\newline) (cdr lines)])
          ((null? lines))
        (let ([line (car lines)])
          (let lp ([x (pregexp-match-positions compiled-regexp line)])
            (match x
              [#f (void)]
              [() (void)]
              [((,start . ,end) . ,rest)
               ;; start and end are 0-based, but #(range ...) is 1-based
               (report `#(range ,ln ,(+ start 1) ,ln ,(+ end 1))
                 type
                 "~a"
                 (substring line start end))
               (lp rest)]))))))

  (define (make-external-checker ls)
    (lambda (uri skip-delay? annotated-code code report)
      (define me self)
      (define (process-input ip pid)
        (let ([c (peek-char ip)])
          (unless (eof-object? c)
            (cond
             [(char-whitespace? c)
              (get-char ip)
              (process-input ip pid)]
             [(char=? c #\{)
              (send me `#(process-json ,(json:read ip)))]
             [else
              (let ([inp (trim-whitespace (get-line ip))])
                (unless (string=? inp "")
                  (send me `#(process-error ,inp))))])
            (process-input ip pid))))
      (define (process-stderr ip pid)
        (let ([line (get-line ip)])
          (unless (eof-object? line)
            (display line (trace-output-port))
            (newline (trace-output-port))
            (process-stderr ip pid))))
      (define (run-command cmd)
        (trace-time `(run-external-checker ,@cmd)
          (match (os-process:start&link (car cmd) (cdr cmd) 'utf8
                   process-input #f process-stderr)
            [#(error ,reason)
             (throw reason)]
            [#(ok ,pid)
             (let ([m (monitor pid)])
               (let lp ([lookup-table #f])
                 (receive (after 5000
                            (kill pid 'shutdown)
                            (receive (after 5000
                                       (kill pid 'kill)
                                       (receive
                                        [`(DOWN ,@m ,_ ,_ ,err)
                                         (throw err)]))
                              [`(DOWN ,@m ,_ ,_ ,err)
                               (throw err)]))
                   [#(process-error ,str)
                    (let ([lookup-table (or lookup-table (make-code-lookup-table code))])
                      (let-values ([(line msg) (reason->line/msg str lookup-table)])
                        (report line 'error "~a" msg)
                        (lp lookup-table)))]
                   [#(process-json ,obj)
                    (let* ([msg (json:ref obj 'message #f)]
                           [type (match (json:ref obj 'type #f)
                                   ["info" 'info]
                                   ["hint" 'hint]
                                   ["warning" 'warning]
                                   [,_ 'error])]
                           [line (json:ref obj 'line #f)]
                           [line (and line (fixnum? line) (fxpositive? line) line)]
                           [col (json:ref obj 'column #f)]
                           [col (and col (fixnum? col) (fxpositive? col) col)])
                      (when msg
                        (report `#(near ,code ,line ,col) type "~a" msg))
                      (lp lookup-table))]
                   [`(DOWN ,@m ,_ ,reason)
                    (unless (eq? reason 'normal)
                      (trace-expr `(external-checker ,(exit-reason->english reason))))])))])))
      (when skip-delay?                 ; recently saved
        (let* ([filename
                (if (starts-with? uri "file:")
                    (uri->abs-path uri)
                    uri)]
               [cmd (fold-right
                     (lambda (x acc)
                       (and acc
                            (match x
                              [filename (cons filename acc)]
                              [(filename ,re)
                               (if (pregexp-match re (path-last filename))
                                   (cons filename acc)
                                   #f)]
                              [,_ (cons x acc)])))
                     '()
                     ls)])
          (when cmd       ; skip the file if the filename regexp fails
            (run-command cmd))))))
  )
