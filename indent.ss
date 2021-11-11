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

(library (indent)
  (export
   <token>
   fold-indent
   has-prop?
   indent
   indent-tokens
   token-length
   tokenize
   )
  (import
   (chezscheme)
   (swish imports)
   )
  (define delims
    '("\\("
      "\\)"
      "\\["
      "\\]"
      "[^\\S\\n]+"               ; contiguous whitespace, not newlines
      "\\s"                      ; other whitespace cases
      "#'"
      "#`"
      "#,@?"
      "'"
      "`"
      ",@?"
      "\""
      "\\\\"
      ";{1,3}"                          ; line comments
      "#;"
      "#\\\\"
      "#\\|"
      "\\|#"
      ))

  (define my-regexp
    (re (join delims #\|)))

  (define (fold-tokens text init proc)
    (define (as-token str start end char-token?)
      (cond
       [(= start end) #f]
       [(and char-token? (= (+ start 1) end)) (string-ref str start)]
       [else (substring str start end)]))
    (define end (string-length text))
    (let lp ([start 0] [acc init])
      (match (pregexp-match-positions my-regexp text start)
        [((,bfp . ,efp))
         (let* ([acc
                 (cond
                  [(as-token text start bfp #f) =>
                   (lambda (t) (proc acc t start bfp))]
                  [else acc])]
                [acc
                 (cond
                  [(as-token text bfp efp #t) =>
                   (lambda (t) (proc acc t bfp efp))]
                  [else acc])])
           (lp efp acc))]
        [,_
         (cond
          [(and (> (- end start) 0)
                (as-token text start end #t)) =>
           (lambda (t) (proc acc t start end))]
          [else acc])])))

  (define-tuple <token>
    name                    ; char | string
    group-id                ; defaults to bfp, add-prop smears the set
    bfp
    efp
    props
    )

  (define props-ht (ht:make symbol-hash eq? symbol?))

  (define (token-length x)
    (cond
     [(char? x) 1]
     [(string? x) (string-length x)]
     [(<token> is? x) (token-length (<token> name x))]))

  (define (name-whitespace? name)
    (or (and (char? name)
             (char-whitespace? name)
             (not (eq? name #\newline)))
        (and (string? name)
             (name-whitespace? (string-ref name 0)))))

  (define (name-line-comment? name)
    (or (eq? name #\;)
        (and (string? name) (eq? (string-ref name 0) #\;))))

  (define (name-block-comment? name)
    (and (string? name) (string=? name "#|")))

  (define (parse input)
    (reverse
     (fold-tokens input '()
       (lambda (acc token bfp efp)
         (cons (<token> make
                 [name token]
                 [group-id bfp]
                 [bfp bfp]
                 [efp efp]
                 [props props-ht])
           acc)))))

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
          [#(put ,x)
           (set! ls (cons x ls))
           (set! column
             (if (eq? (<token> name x) #\newline)
                 0
                 (+ column (token-length x))))]
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
    (ht:set props prop #t))

  (define (add-prop ls prop)
    (let ([id (<token> group-id (car ls))])
      (map
       (lambda (token)
         (<token> copy* token
           [group-id id]
           [props (add-prop* props prop)]))
       ls)))

  (define (has-prop? t prop)
    (ht:ref (<token> props t) prop #f))

  (define-syntax token-cond
    (syntax-rules (eof)
      [(_ $t $name [eof $eof-expr ...] clauses ...)
       (and (identifier? #'$t) (identifier? #'$name))
       (cond
        [(eof-object? $t) $eof-expr ...]
        [else
         (let ([$name (<token> name $t)])
           (cond clauses ...))])]))

  (define (tokens-until* tp keep? pred)
    (let lp ([acc '()])
      (let ([t (peek-token tp)])
        (token-cond t name
          [eof (reverse acc)]
          [(pred name)
           (reverse
            (if keep?
                (cons (get-token tp) acc)
                acc))]
          [else
           (lp (cons (get-token tp) acc))]))))

  (define (tokens-until tp tname)
    (tokens-until* tp #t
      (lambda (name) (equal? name tname))))

  (define (line-tokens tp)
    (tokens-until tp #\newline))

  (define (space-tokens tp)
    (tokens-until* tp #f
      (lambda (name)
        (not (name-whitespace? name)))))

  (define (block-tokens tp)
    (let lp ([depth 1] [acc '()])
      (cond
       [(= depth 0) (reverse acc)]
       [else
        (let ([t (get-token tp)])
          (token-cond t name
            [eof (reverse acc)]
            [(equal? name "#|")
             (lp (+ depth 1) (cons t acc))]
            [(equal? name "|#")
             (lp (- depth 1) (cons t acc))]
            [else
             (lp depth (cons t acc))]))])))

  (define (string-tokens tp)
    (let ([t (get-token tp)])
      (token-cond t name
        [eof '()]
        [(eq? name #\")
         (cons t '())]
        [(eq? name #\\)
         (let ([next (get-token tp)])
           (if (eof-object? next)
               (cons t '())
               (cons* t next (string-tokens tp))))]
        [(equal? name "#\\")
         (let ([next (get-token tp)])
           (if (eof-object? next)
               (cons t '())
               (cons* t next (string-tokens tp))))]
        [else
         (cons t (string-tokens tp))])))

  (define (expr-tokens tp)
    (let lp ([armed? #f] [depth 0] [acc '()])
      (cond
       [(and armed? (= depth 0)) (reverse acc)]
       [else
        (let ([t (get-token tp)])
          (token-cond t name
            [eof (reverse acc)]
            [(name-line-comment? name)
             (let ([comment (line-tokens tp)])
               (lp armed? depth
                 (fold-right cons (cons t acc) (reverse comment))))]
            [(or (eq? name #\() (eq? name #\[))
             (lp #t (+ depth 1) (cons t acc))]
            [(or (eq? name #\)) (eq? name #\]))
             (lp armed? (- depth 1) (cons t acc))]
            [(or (eq? name #\')
                 (eq? name #\`)
                 (eq? name #\,)
                 (equal? name ",@")
                 (equal? name "#'")
                 (equal? name "#`")
                 (equal? name "#,")
                 (equal? name "#,@"))
             (lp armed? depth (cons t acc))]
            [(or (eq? name #\newline)
                 (name-whitespace? name))
             (lp armed? depth (cons t acc))]
            [(eq? name #\")
             (let ([string (string-tokens tp)])
               (lp #t depth
                 (fold-right cons (cons t acc) (reverse string))))]
            [else
             (lp #t depth (cons t acc))]))])))

  (define (mark tp)
    (let ([t (get-token tp)])
      (token-cond t name
        [eof '()]
        [(name-line-comment? name)
         (let ([comment (cons t (line-tokens tp))])
           (append
            (add-prop (add-prop comment 'comment) 'line-comment)
            (mark tp)))]
        [(equal? name "#;")
         (let* ([tokens (assemble (mark (make-token-port (expr-tokens tp))))]
                [expr (cons t tokens)])
           (append
            (add-prop (add-prop expr 'comment) 'datum-comment)
            (mark tp)))]
        [(equal? name "#|")
         (let ([comment (cons t (block-tokens tp))])
           (append
            (add-prop (add-prop comment 'comment) 'block-comment)
            (mark tp)))]
        [(equal? name "#\\")
         (let ([next (get-token tp)])
           (if (eof-object? next)
               (cons t '())
               (cons* t next (mark tp))))]
        [(eq? name #\")
         (let ([string (cons t (string-tokens tp))])
           (append
            (add-prop string 'string)
            (mark tp)))]
        [(and (string? name) (string->number name))
         (append (add-prop (list t) 'number) (mark tp))]
        [else
         (cons t (mark tp))])))

  (define (mark-whitespace tp)
    (let lp ([start-of-line? #t])
      (let ([t (get-token tp)])
        (token-cond t name
          [eof '()]
          [(eq? name #\newline)
           (cons t (lp #t))]
          [(name-whitespace? name)
           (let* ([spaces (cons t (space-tokens tp))]
                  [next (peek-token tp)]
                  [spaces
                   (token-cond next name
                     [eof spaces]
                     [(eq? name #\newline)
                      (add-prop spaces 'trailing-whitespace)]
                     [(and (string? name) (starts-with? name ";;;"))
                      (add-prop spaces 'left-margin-whitespace)]
                     [(and (string? name) (starts-with? name ";;"))
                      (add-prop spaces 'with-code-whitespace)]
                     [(eq? name #\;)
                      (add-prop spaces 'right-column-whitespace)]
                     [start-of-line?
                      (add-prop spaces 'leading-whitespace)]
                     [else spaces])])
             (append spaces (lp start-of-line?)))]
          [else
           (cons t (lp #f))]))))

  (define (assemble tokens)
    ;; Assemble individual tokens into strings, characters, () and [].
    (let lp ([tokens tokens])
      (match tokens
        [() '()]
        [(,(open <= `(<token> [name #\(]))
          ,(close <= `(<token> [name #\)] ,efp)) . ,rest)
         (cons
          (<token> copy open
            [name "()"]
            [efp efp])
          (lp rest))]
        [(,(open <= `(<token> [name #\[]))
          ,(close <= `(<token> [name #\]] ,efp)) . ,rest)
         (cons
          (<token> copy open
            [name "[]"]
            [efp efp])
          (lp rest))]
        [(,(pre <= `(<token> [name "#\\"] ,props))
          ,(tok <= `(<token> ,name ,efp ,@props)) . ,rest)
         (cons
          (<token> copy pre
            [name (format "#\\~a" name)]
            [efp efp]
            [props (add-prop* props 'char)])
          (lp rest))]
        [(,(pre <= `(<token> ,name ,group-id ,bfp)) . ,rest)
         (guard (has-prop? pre 'string))
         (let ([op (open-output-string)])
           (display name op)
           (let ([rest
                  (let lp ([ls rest])
                    (match ls
                      [(,(tok <= `(<token> ,name ,@group-id)) . ,rest)
                       (guard (has-prop? tok 'string))
                       (display name op)
                       (lp rest)]
                      [,_ ls]))])
             (cons
              (let ([name (get-output-string op)])
                (<token> copy pre
                  [name name]
                  [efp (+ bfp (string-length name))]))
              (lp rest))))]
        [(,tok . ,rest)
         (cons tok (lp rest))])))

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
           ;; Because tokens are treated differently than in other
           ;; environments, we need to make sure they provide no
           ;; additional indentation during subform-indent.
           (for-each
            (lambda (name)
              (eq-hashtable-set! ht (string->symbol name) scheme-no-indent))
            '(",@" "#'" "#`" "#," "#,@" "#" "#vu8" "#vfx"))
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
      (put-token op
        (<token> make
          [name (if (= n 1)
                    #\space
                    (make-string n #\space))]
          [group-id #f]
          [bfp #f]
          [efp #f]
          [props props-ht]))))

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
        (token-cond t name
          [eof (values 0 default-lin)]
          [(string? name)
           (cond
            [(or (has-prop? t 'number)
                 (has-prop? t 'string)
                 (has-prop? t 'char))
             (values 0 -2)]
            [else
             (let ([x (lookup-indent name)])
               (cond
                [(not x)
                 (values (scheme-nice-indent t aol? indent) default-lin)]
                [(integer? x)
                 (values (scheme-special-indent x t aol? indent) x)]
                [else
                 (values (x t aol? indent) default-lin)]))])]
          [else
           (values 0 default-lin)]))

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
          (token-cond t name
            [eof (void)]
            [(eq? name #\newline)
             (put-token op t)
             (start-new-line rlevels)]
            [(name-whitespace? name)
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
        (token-cond t name
          [eof (void)]
          [(eq? name #\newline)
           (put-token op t)
           (start-new-line rlevels)]
          [(has-prop? t 'line-comment)
           (put-token op t)
           (s1 (get-token ip) indent rlevels)]
          [(name-block-comment? name)
           (put-token op t)
           (emit-tokens op (block-tokens ip))
           (s1 (get-token ip) indent rlevels)]
          [(or (eq? name #\() (eq? name #\[))
           (put-token op t)
           (let* ([indent (+ indent 1)]
                  [next (get-token ip)]
                  [aol? (atom-on-line? ip)])
             (let-values ([(delta lin) (subform-indent next aol? indent)])
               (s1 next indent
                 (cons (<level> make
                         [indent (+ indent delta)]
                         [lin lin]
                         [subform-count -1])
                   rlevels))))]
          [(or (eq? name #\)) (eq? name #\]))
           (put-token op t)
           ;; When the parens are unbalanced, this code continues on
           ;; allowing upcoming subforms to indent according to the
           ;; current form's position.
           (s1 (get-token ip) (+ indent 1)
             (increment-subform-count
              (match rlevels
                [() '()]                ; unbalanced parens
                [(,_ . ,rest) rest])))]
          [(name-whitespace? name)
           (put-token op t)
           (s1 (get-token ip) (+ indent (token-length t)) rlevels)]
          [else
           (put-token op t)
           (s1 (get-token ip)
             (+ indent (token-length t))
             (increment-subform-count rlevels))]))

      (define (atom-on-line? ip)
        (let ([ip (copy-token-input-port ip)])
          (let lp ([t (get-token ip)])
            (token-cond t name
              [eof #f]
              [(eq? name #\newline) #f]
              [(and (string? name)
                    (not (name-whitespace? name))
                    (not (has-prop? t 'line-comment))
                    (not (has-prop? t 'block-comment)))
               #t]
              [else (lp (get-token ip))]))))

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
        (token-cond t name
          [eof ht]
          [(eq? name #\newline)
           (when line-comment?
             (eq-hashtable-set! ht line
               (<marker> make
                 [anchor-min (+ code-end 1)]
                 [anchor-max (max (+ code-end 1)
                                  (- max-fill-col line-comment-length))])))
           (lp (+ line 1) 0 #f 0)]
          [(has-prop? t 'line-comment)
           (lp line code-end #t (+ (token-length t) line-comment-length))]
          [(and (name-whitespace? name)
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
          (token-cond t name
            [eof (void)]
            [(eq? name #\newline)
             (put-token op t)
             (lp (+ line 1) #f)]
            [(has-prop? t 'block-comment)
             (put-token op t)
             (lp line t)]
            [(has-prop? t 'right-column-whitespace)
             ;; Don't actually write right-column whitespace yet.
             (lp line t)]
            [(eq? name #\;)
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
           [tokens (mark (make-token-port tokens))]
           [tokens (assemble tokens)])
      tokens))

  (define (indent-tokens tokens)
    (let* ([tokens (indent-code tokens)]
           [tokens (indent-comments tokens)])
      tokens))

  (define (display-tokens ls op)
    (for-each
     (lambda (t)
       (display (<token> name t) op))
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
  )
