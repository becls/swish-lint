;;; Copyright 2021 Beckman Coulter, Inc.
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

#!chezscheme
(library (indent)
  (export
   fold-indent
   has-prop?
   indent
   indent-tokens
   token
   token-bfp
   token-efp
   token-err
   token-length
   token-props
   token-raw
   token-type
   token-value
   tokenize
   )
  (import
   (chezscheme)
   (swish imports)
   )
  (define $yield
    (make-process-parameter
     (lambda args (errorf 'yield "invalid context"))))

  (define (yield x) (($yield) x))

  (define-syntax generator
    (syntax-rules ()
      [(_ b1 b2 ...)
       (let ()
         (define (wrap return)
           (lambda (x)
             (set! return (call/1cc
                           (lambda (resume-here)
                             (set! proc resume-here)
                             (return x))))))
         (define (proc return)
           (parameterize ([$yield (wrap return)])
             b1 b2 ...))
         (lambda () (call/cc proc)))]))

  (define-record-type token
    (nongenerative)
    (fields
     (immutable type)
     (immutable value)
     (immutable raw)
     (immutable bfp)
     (immutable efp)
     (immutable err)
     (immutable props)
     ))

  (define-enumeration prop-element
    (
     block-comment
     char
     comment
     datum-comment
     leading-whitespace
     left-margin-whitespace
     line-comment
     number
     right-column-whitespace
     string
     trailing-whitespace
     with-code-whitespace
     )
    token-prop)

  (define empty-props (token-prop))

  (define delims
    '("[^\\S\\n]+"               ; contiguous whitespace, not newlines
      "\\s"                      ; other whitespace cases
      ";+[^\\n]*"                ; line comments
      "#\\|"                     ; lblock-comment
      "\\|#"                     ; rblock-comment
      ))

  (define my-regexp
    (re (join delims #\|)))

  (define (open-token-string text)
    (define (build-token* type value bfp efp err props)
      (make-token type value value bfp efp err props))
    (define (build-token type value bfp efp err props)
      (make-token type value (substring text bfp efp) bfp efp err props))
    (define (as-token str start end err)
      (and
       (fx< start end)
       (let ([first (string-ref str start)]
             [at-least-two? (> (- end start) 1)])
         (cond
          [(eq? first #\newline)
           (build-token 'eol #f start end err empty-props)]
          [(char-whitespace? first)
           (build-token 'ws (- end start) start end err empty-props)]
          [(eq? first #\;)
           (build-token* 'line-comment (substring str start end) start end err (token-prop comment line-comment))]
          [(and at-least-two?
                (eq? first #\#)
                (eq? (string-ref str (fx+ start 1)) #\|))
           (build-token 'lblock-comment #f start end err empty-props)]
          [(and at-least-two?
                (eq? first #\|)
                (eq? (string-ref str (fx+ start 1)) #\#))
           (build-token 'rblock-comment #f start end err empty-props)]
          [else
           (build-token* 'text (substring str start end) start end err empty-props)]))))
    (define (gen-ext-tokens start end err)
      (unless (fx= start end)
        (let ([c (string-ref text start)])
          (cond
           [(eq? c #\newline)           ; newline
            (let ([next (fx+ start 1)])
              (yield (build-token 'eol #f start next err empty-props))
              (gen-ext-tokens next end err))]
           [(eq? c #\;)                 ; line comments
            (let lp ([next (fx+ start 1)])
              (cond
               [(or (fx>= next end)
                    (eq? (string-ref text next) #\newline))
                (yield
                 (build-token* 'line-comment
                   (substring text start next)
                   start
                   next
                   err
                   (token-prop comment line-comment)))
                (gen-ext-tokens next end err)]
               [else
                (lp (fx+ next 1))]))]
           [(and (eq? c #\#)            ; lblock-comment
                 (let ([next (fx+ start 1)])
                   (and (fx< next end)
                        (eq? (string-ref text next) #\|)
                        (fx+ next 1)))) =>
            (lambda (next)
              (yield (build-token 'lblock-comment #f start next err empty-props))
              (gen-ext-tokens next end err))]
           [(and (eq? c #\|)            ; rblock-comment
                 (let ([next (fx+ start 1)])
                   (and (fx< next end)
                        (eq? (string-ref text next) #\#)
                        (fx+ next 1)))) =>
            (lambda (next)
              (yield (build-token 'rblock-comment #f start next err empty-props))
              (gen-ext-tokens next end err))]
           [(char-whitespace? c) ; contiguous whitespace, not newlines
            (let lp ([next (fx+ start 1)])
              (cond
               [(or (fx>= next end)
                    (let ([c2 (string-ref text next)])
                      (or (eq? c2 #\newline)
                          (not (char-whitespace? c2)))))
                (yield (build-token 'ws (fx- next start) start next err empty-props))
                (gen-ext-tokens next end err)]
               [else
                (lp (fx+ next 1))]))]
           [else
            (match (pregexp-match-positions my-regexp text start end)
              [((,bfp . ,efp))
               ;; Something occurred before the regexp
               (cond [(as-token text start bfp err) => yield])
               ;; Regexp found something
               (cond [(as-token text bfp efp err) => yield])
               (gen-ext-tokens efp end err)]
              [,_
               ;; Something occurred that was not processed by read-token,
               ;; and was not caught by the regular expressions.
               (cond [(as-token text start end err) => yield])])]))))
    (define (gen-tokens)
      (let ([ip (open-input-string text)]
            [end (string-length text)])
        (when (and (fx> end 2)
                   (eq? (string-ref text 0) #\#)
                   (eq? (string-ref text 1) #\!)
                   (let ([x (string-ref text 2)])
                     (or (eq? x #\space)
                         (eq? x #\/))))
          ;; Advance the port, attempt to parse the entire line as
          ;; extended tokens and continue.
          (get-line ip)
          (gen-ext-tokens 0 (port-position ip) #f))
        (let lp ([prior-efp (port-position ip)])
          (unless (= prior-efp end)
            (match (try (let-values ([(type value bfp efp) (read-token ip)])
                          (unless (= prior-efp bfp)
                            ;; Something occurred before the read token, read extended tokens first.
                            (gen-ext-tokens prior-efp bfp #f))
                          (unless (= bfp efp)
                            (yield (build-token type value bfp efp #f
                                     (cond
                                      [(eq? type 'atomic)
                                       (cond
                                        [(string? value) (token-prop string)]
                                        [(number? value) (token-prop number)]
                                        [(char? value) (token-prop char)]
                                        [else empty-props])]
                                      [else
                                       empty-props]))))
                          efp))
              [`(catch ,r ,err)
               (let ([s (exit-reason->english r)])
                 (cond
                  [(starts-with? s "Exception in read-token: unexpected end-of-file")
                   ;; Rewind the port, attempt to parse the entire
                   ;; line as extended tokens and continue.
                   (set-port-position! ip prior-efp)
                   (get-line ip)
                   (let ([efp (port-position ip)])
                     (gen-ext-tokens prior-efp efp err)
                     (lp efp))]
                  [else
                   ;; Advance the port to the next whitespace
                   ;; character, attempt to parse the failed
                   ;; characters as extended tokens and continue.
                   (let lp ()
                     (let ([c (get-char ip)])
                       (if (or (eof-object? c) (char-whitespace? c))
                           (unget-char ip c)
                           (lp))))
                   (let ([efp (port-position ip)])
                     (gen-ext-tokens prior-efp efp err)
                     (lp efp))]))]
              [,efp (lp efp)])))))
    (generator
     (gen-tokens)
     (yield (eof-object))))

  (define (read-extended-token tg)
    (tg))

  (define (token-length t)
    (string-length (token-raw t)))

  (define (token-open? t)
    (memq (token-type t) '(lparen lbrack record-brack vfxnparen vfxparen vnparen vparen vu8nparen vu8paren)))

  (define (token-close? t)
    (memq (token-type t) '(rparen rbrack)))

  (define (parse text)
    (let ([tp (open-token-string text)])
      (let lp ()
        (let ([t (read-extended-token tp)])
          (if (eof-object? t)
              '()
              (cons t (lp)))))))

  (define (make-token-port ls)
    (let ([ls ls])
      (lambda (op)
        (match op
          [get-token
           (match ls
             [() (eof-object)]
             [(,x . ,rest)
              (set! ls rest)
              x])]
          [peek-token
           (match ls
             [() (eof-object)]
             [(,x . ,_) x])]
          [make-shallow-copy
           (make-token-port ls)]))))

  (define (get-token tp)
    (tp 'get-token))

  (define (peek-token tp)
    (tp 'peek-token))

  (define (copy-token-input-port tp)
    (tp 'make-shallow-copy))

  (define (make-token-output-port)
    (let ([ls '()]
          [column 0])
      (lambda (op)
        (match op
          [#(put ,t)
           (set! ls (cons t ls))
           (set! column
             (if (eq? (token-type t) 'eol)
                 0
                 (+ column (token-length t))))]
          [get-column column]
          [get-all
           (let ([result (reverse ls)])
             (set! ls '())
             (set! column 0)
             result)]))))

  (define (put-token tp x)
    (tp `#(put ,x)))

  (define (get-output-tokens tp)
    (tp 'get-all))

  (define (get-column tp)
    (tp 'get-column))

  (define (add-prop* props prop)
    (enum-set-union props prop))

  (define (add-prop ls prop)
    (map
     (lambda (t)
       (make-token
        (token-type t)
        (token-value t)
        (token-raw t)
        (token-bfp t)
        (token-efp t)
        (token-err t)
        (add-prop* (token-props t) prop)))
     ls))

  (define (has-prop? t prop)
    (enum-set-member? prop (token-props t)))

  (define-syntax token-cond
    (syntax-rules (eof)
      [(_ $t $type [eof $eof-expr ...] clauses ...)
       (and (identifier? #'$t) (identifier? #'$type))
       (cond
        [(eof-object? $t) $eof-expr ...]
        [else
         (let ([$type (token-type $t)])
           (cond clauses ...))])]))

  (define (tokens-until* tp keep? pred)
    (let lp ([acc '()])
      (let ([t (peek-token tp)])
        (token-cond t type
          [eof (reverse acc)]
          [(pred type)
           (reverse
            (if keep?
                (cons (get-token tp) acc)
                acc))]
          [else
           (lp (cons (get-token tp) acc))]))))

  (define (tokens-until tp ttype)
    (tokens-until* tp #t
      (lambda (type) (equal? type ttype))))

  (define (line-tokens tp)
    (tokens-until tp 'eol))

  (define (space-tokens tp)
    (tokens-until* tp #f
      (lambda (type)
        (not (eq? type 'ws)))))

  (define (block-tokens tp)
    (let lp ([depth 1] [acc '()])
      (cond
       [(= depth 0) (reverse acc)]
       [else
        (let ([t (get-token tp)])
          (token-cond t type
            [eof (reverse acc)]
            [(eq? type 'lblock-comment)
             (lp (+ depth 1) (cons t acc))]
            [(eq? type 'rblock-comment)
             (lp (- depth 1) (cons t acc))]
            [else
             (lp depth (cons t acc))]))])))

  (define (expr-tokens tp)
    (let lp ([armed? #f] [depth 0] [acc '()])
      (cond
       [(and armed? (= depth 0)) (reverse acc)]
       [else
        (let ([t (get-token tp)])
          (token-cond t type
            [eof (reverse acc)]
            [(eq? type 'line-comment)
             (let ([comment (line-tokens tp)])
               (lp armed? depth
                 (fold-right cons (cons t acc) (reverse comment))))]
            [(token-open? t)
             (lp #t (+ depth 1) (cons t acc))]
            [(token-close? t)
             (lp armed? (- depth 1) (cons t acc))]
            [(eq? type 'quote)
             (lp armed? depth (cons t acc))]
            [(or (eq? type 'eol)
                 (eq? type 'ws))
             (lp armed? depth (cons t acc))]
            [else
             (lp #t depth (cons t acc))]))])))

  (define (mark tp)
    (let ([t (get-token tp)])
      (token-cond t type
        [eof '()]
        [(and (eq? type 'quote) (eq? (token-value t) 'datum-comment))
         (let* ([tokens (mark (make-token-port (expr-tokens tp)))]
                [expr (cons t tokens)])
           (append
            (add-prop expr (token-prop comment datum-comment))
            (mark tp)))]
        [(eq? type 'lblock-comment)
         (let ([comment (cons t (block-tokens tp))])
           (append
            (add-prop comment (token-prop comment block-comment))
            (mark tp)))]
        [else
         (cons t (mark tp))])))

  (define (count-leading-semi s limit)
    (let ([s-len (string-length s)])
      (let lp ([n 0])
        (cond
         [(and (fx< n limit)
               (fx< n s-len)
               (eq? (string-ref s n) #\;))
          (lp (fx+ n 1))]
         [else n]))))

  (define (mark-whitespace tp)
    (let lp ([start-of-line? #t])
      (let ([t (get-token tp)])
        (token-cond t type
          [eof '()]
          [(eq? type 'eol)
           (cons t (lp #t))]
          [(eq? type 'ws)
           (let* ([spaces (cons t (space-tokens tp))]
                  [next (peek-token tp)]
                  [spaces
                   (token-cond next type
                     [eof spaces]
                     [(eq? type 'eol)
                      (add-prop spaces (token-prop trailing-whitespace))]
                     [(eq? type 'line-comment)
                      (match (count-leading-semi (token-raw next) 3)
                        [3 (add-prop spaces (token-prop left-margin-whitespace))]
                        [2 (add-prop spaces (token-prop with-code-whitespace))]
                        [1 (add-prop spaces (token-prop right-column-whitespace))])]
                     [start-of-line?
                      (add-prop spaces (token-prop leading-whitespace))]
                     [else spaces])])
             (append spaces (lp start-of-line?)))]
          [else
           (cons t (lp #f))]))))

  (define (scheme-no-indent t aol? indent)
    0)

  (define (scheme-normal-indent t aol? indent)
    (if aol?
        (+ (token-length t) 1)
        0))

  (define (scheme-nice-indent t aol? indent)
    (if aol? 1 0))

  (define (scheme-special-indent count t aol? indent)
    (if (and aol? (<= count 0))
        (+ (token-length t) 1)
        1))

  (define-syntax define-indent-table
    (syntax-rules ()
      [(_ name [ids n] ...)
       (define name
         (let ([ht (make-eq-hashtable)])
           (for-each (lambda (id) (eq-hashtable-set! ht id n)) 'ids)
           ...
           ht))]))

  (define-indent-table indent-table
    [(and
      if
      or
      = < > <= >= + - * /
      fx= fx< fx> fx<= fx>= fx+ fx- fx* fx/
      fl= fl< fl> fl<= fl>= fl+ fl- fl* fl/
      cfl= cfl+ cfl- cfl* cfl/
      quotient remainder modulo max min
      fxquotient fxremainder fxmodulo fxmax fxmin
      logand logior logor logxor
      fxlogand fxlogior fxlogor fxlogxor)
     scheme-normal-indent]
    [(do syntax-case) 2]
    [(begin delay dynamic-wind) 0]
    [(case
         lambda
       let
       let*
       letrec
       let-values
       let*-values
       let-syntax
       letrec-syntax
       syntax-rules
       library
       call-with-input-file
       with-input-from-file
       with-input-from-port
       call-with-output-file
       with-output-to-file
       with-output-to-port)
     1]
    ;; Manual updates to style
    [(call-with-values) 0]
    )

  (define (emit-whitespace op n)
    (when (> n 0)
      (let ([raw (make-string n #\space)])
        (put-token op
          (make-token 'ws raw raw #f #f #f empty-props)))))

  (define (emit-tokens op ls)
    (match ls
      [() (void)]
      [(,first . ,rest)
       (put-token op first)
       (emit-tokens op rest)]))

  (define (lookup-indent name)
    (let ([name (match (pregexp-match "#[23]%(.+)" name)
                  [(,_ ,name) name]
                  [#f name])])
      (eq-hashtable-ref indent-table (string->symbol name) #f)))

  (define (indent-code tokens)
    (let* ([tokens (mark-whitespace (make-token-port tokens))]
           [ip (make-token-port tokens)]
           [op (make-token-output-port)])
      (define-tuple <level> indent lin subform-count)

      (define default-lin -1)

      (define (subform-indent t aol? indent)
        (token-cond t type
          [eof (values 0 default-lin)]
          ;; Because tokens are treated differently than in other
          ;; environments, we need to make sure they provide no
          ;; additional indentation during subform-indent.
          [(memq type '(lparen lbrack rparen rbrack quote record-brack vfxnparen vfxparen vnparen vparen vu8nparen vu8paren box))
           (values 0 default-lin)]
          [(or (has-prop? t 'number)
               (has-prop? t 'string)
               (has-prop? t 'char))
           (values 0 -2)]
          [else
           (let ([x (lookup-indent (token-raw t))])
             (cond
              [(not x)
               (values (scheme-nice-indent t aol? indent) default-lin)]
              [(integer? x)
               (values (scheme-special-indent x t aol? indent) x)]
              [else
               (values (x t aol? indent) default-lin)]))]))

      (define (increment-subform-count ls)
        (match ls
          [() ls]
          [(,first . ,rest)
           (cons (<level> copy* first [subform-count (+ subform-count 1)])
             rest)]))

      (define (start-new-line rlevels)
        (match rlevels
          [() (s0 0 rlevels)]
          [(`(<level> ,indent) . ,_)
           (s0 indent rlevels)]))

      (define (s0 indent rlevels)
        ;; start of line; emit leading whitespace
        (let ([t (get-token ip)])
          (token-cond t type
            [eof (void)]
            [(eq? type 'eol)
             (put-token op t)
             (start-new-line rlevels)]
            [(eq? type 'ws)
             (s0
              (if (has-prop? t 'with-code-whitespace)
                  indent
                  (+ indent (token-length t)))
              rlevels)]
            [else
             (let ([indent
                    (match rlevels
                      [() indent]
                      [(`(<level> ,indent ,lin ,subform-count) . ,_)
                       (if (< subform-count lin)
                           (+ indent 2)
                           indent)])])
               (emit-whitespace op indent)
               (s1 t indent rlevels))])))

      (define (s1 t indent rlevels)
        ;; line cont'd
        (token-cond t type
          [eof (void)]
          [(eq? type 'eol)
           (put-token op t)
           (start-new-line rlevels)]
          [(has-prop? t 'line-comment)
           (put-token op t)
           (s1 (get-token ip) indent rlevels)]
          [(eq? type 'lblock-comment)
           (put-token op t)
           (emit-tokens op (block-tokens ip))
           (s1 (get-token ip) indent rlevels)]
          [(token-open? t)
           (put-token op t)
           (let* ([indent (+ indent (token-length t))]
                  [next (get-token ip)]
                  [aol? (atom-on-line? next ip)])
             (let-values ([(delta lin) (subform-indent next aol? indent)])
               (s1 next indent
                 (cons (<level> make
                         [indent (+ indent delta)]
                         [lin lin]
                         [subform-count -1])
                   rlevels))))]
          [(token-close? t)
           (put-token op t)
           ;; When the parens are unbalanced, this code continues on
           ;; allowing upcoming subforms to indent according to the
           ;; current form's position.
           (s1 (get-token ip) (+ indent 1)
             (increment-subform-count
              (match rlevels
                [() '()]                ; unbalanced parens
                [(,_ . ,rest) rest])))]
          [(eq? type 'ws)
           (put-token op t)
           (s1 (get-token ip) (+ indent (token-length t)) rlevels)]
          [else
           (put-token op t)
           (s1 (get-token ip)
             (+ indent (token-length t))
             (increment-subform-count rlevels))]))

      (define (atom-on-line? first ip)
        (let ([ip (copy-token-input-port ip)])
          (define (s0 t)
            (token-cond t type
              [eof #f]
              [(eq? type 'eol) #f]
              [(memq type '(lparen lbrack))
               (s1 (get-token ip))]
              [(and (or (eq? type 'atomic)
                        (eq? type 'quote))
                    (not (has-prop? t 'line-comment))
                    (not (has-prop? t 'block-comment)))
               #t]
              [else (s0 (get-token ip))]))
          (define (s1 t)
            (token-cond t type
              [eof #f]
              [(eq? type 'eol) #f]
              [(eq? type 'ws) (s1 (get-token ip))]
              [(memq type '(rparen rbrack)) #t]
              [(and (or (eq? type 'atomic)
                        (eq? type 'quote))
                    (not (has-prop? t 'line-comment))
                    (not (has-prop? t 'block-comment)))
               #t]
              [else (s1 (get-token ip))]))
          (token-cond first type
            [eof #f]
            [(eq? type 'eol) #f]
            [else (s0 (get-token ip))])))

      (start-new-line '())
      (get-output-tokens op)))

  (define middle-anchor 40)
  (define max-fill-col 70)            ; Default fill-column from Emacs

  (define-tuple <marker>
    anchor-min
    anchor-max
    )

  (define (build-comment-markers tokens)
    (define ip (make-token-port tokens))
    (define ht (make-eq-hashtable))
    (let lp ([line 1] [code-end 0] [line-comment? #f] [line-comment-length 0])
      (let ([t (get-token ip)])
        (token-cond t type
          [eof ht]
          [(eq? type 'eol)
           (when line-comment?
             (eq-hashtable-set! ht line
               (<marker> make
                 [anchor-min (+ code-end 1)]
                 [anchor-max (max (+ code-end 1)
                                  (- max-fill-col line-comment-length))])))
           (lp (+ line 1) 0 #f 0)]
          [(has-prop? t 'line-comment)
           (lp line code-end #t (+ (token-length t) line-comment-length))]
          [(and (eq? type 'ws)
                (has-prop? t 'right-column-whitespace))
           (lp line code-end line-comment? line-comment-length)]
          [else
           (lp line (+ code-end (token-length t)) line-comment? line-comment-length)]))))

  (define (build-anchors markers)
    (define ht (make-eq-hashtable))
    (define lines (vector->list (vector-sort < (hashtable-keys markers))))

    (define (minmax marker)
      (match marker
        [#f (values 0 max-fill-col)]
        [`(<marker> ,anchor-min ,anchor-max)
         (values anchor-min anchor-max)]))

    (define (look-ahead line curr-min curr-max)
      (match (eq-hashtable-ref markers line #f)
        [#f (values curr-min curr-max)]
        [`(<marker> ,anchor-min ,anchor-max)
         (if (and (<= curr-min anchor-max) (>= curr-max anchor-min))
             (look-ahead (+ line 1)
               (max curr-min anchor-min)
               (min curr-max anchor-max))
             (values curr-min curr-max))]))

    (let lp ([lines lines]
             [prev-line (most-negative-fixnum)]
             [prev-anchor middle-anchor])
      (match lines
        [() ht]
        [(,line . ,rest)
         (let*-values ([(curr-min curr-max)
                        (minmax (eq-hashtable-ref markers line #f))]
                       [(curr-min curr-max)
                        (look-ahead (+ line 1) curr-min curr-max)])
           (let* ([anchor
                   (cond
                    [(and (= line (+ prev-line 1))
                          (< prev-anchor max-fill-col)
                          (<= curr-min prev-anchor curr-max))
                     prev-anchor]
                    [else middle-anchor])]
                  [anchor
                   (cond
                    [(<= curr-min anchor curr-max) anchor]
                    [(< anchor curr-min) curr-min]
                    [else curr-max])])
             (eq-hashtable-set! ht line anchor)
             (lp rest line anchor)))])))

  (define (indent-comments tokens)
    (let* ([tokens (mark-whitespace (make-token-port tokens))]
           [markers (build-comment-markers tokens)]
           [anchors (build-anchors markers)]
           [ip (make-token-port tokens)]
           [op (make-token-output-port)])
      (let lp ([line 1] [prior #f])
        (define (align-middle line t)
          (let* ([anchor (eq-hashtable-ref anchors line middle-anchor)]
                 [spaces (max (- anchor (get-column op)) 1)])
            (emit-whitespace op spaces)
            (put-token op t)))

        (let ([t (get-token ip)])
          (token-cond t type
            [eof (void)]
            [(eq? type 'eol)
             (put-token op t)
             (lp (+ line 1) #f)]
            [(has-prop? t 'block-comment)
             (put-token op t)
             (lp line t)]
            [(has-prop? t 'right-column-whitespace)
             ;; Don't actually write right-column whitespace yet.
             (lp line t)]
            [(= (count-leading-semi (token-raw t) 2) 1)
             (cond
              [(or (not prior)
                   (and prior (has-prop? prior 'right-column-whitespace)))
               (align-middle line t)
               (lp line t)]
              [else
               (put-token op t)
               (lp line t)])]
            [else
             (put-token op t)
             (lp line t)])))

      (get-output-tokens op)))

  (define (tokenize text)
    (let* ([tokens (parse text)]
           [tokens (mark (make-token-port tokens))])
      tokens))

  (define (indent-tokens tokens)
    (let* ([tokens (indent-code tokens)]
           [tokens (indent-comments tokens)])
      tokens))

  (define (display-tokens ls op)
    (for-each
     (lambda (t)
       (display (token-raw t) op))
     ls))

  (define (tokens->string ls)
    (let ([op (open-output-string)])
      (display-tokens ls op)
      (get-output-string op)))

  (define (indent text)
    (tokens->string (indent-tokens (tokenize text))))

  (define (fold-indent text init proc)
    (let ([src-port (open-input-string text)]
          [dst-port (open-input-string (indent text))])
      (let lp ([line 1] [acc init])
        (if (eof-object? (peek-char src-port))
            acc
            (let ([src-line (get-line src-port)]
                  [dst-line (get-line dst-port)])
              (lp (+ line 1) (proc line src-line dst-line acc)))))))

  (record-writer (csv7:record-type-descriptor (token-prop))
    (lambda (r p wr)
      (fprintf p "#<token-prop {~{~a~^,~}}>" (enum-set->list r))))
  )
