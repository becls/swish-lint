(library (config)
  (export
   config:load-project
   config:load-user
   config:user-dir
   make-optional-passes
   )
  (import
   (checkers)
   (chezscheme)
   (config-params)
   (swish imports)
   (trace)
   )
  (define (load-config who path process-expr)
    (when (file-exists? path)
      (trace-expr `(loading ,path))
      (match (try
              (let ([ip (open-file-to-read path)])
                (on-exit (close-port ip)
                  (let lp ()
                    (let ([x (read ip)])
                      (if (eof-object? x)
                          '()
                          (cons x (lp))))))))
        [`(catch ,reason)
         (trace-expr `(,who => ,(exit-reason->english reason)))]
        [,exprs
         (for-each
          (lambda (expr)
            (match (try (process-expr expr))
              [`(catch ,reason)
               (trace-expr `(,who => ,(exit-reason->english reason)))]
              [,_ (void)]))
          exprs)])))

  (define (config:load-project path)
    (load-config 'config:load-project path
      (lambda (expr)
        (match expr
          [(definition-keywords . ,ls)
           (unless (for-all string? ls)
             (throw "definition-keywords must all be strings"))
           (config:definition-keywords ls)]
          [,_
           (trace-expr `(config:load-project ignoring ,expr))]))))

  (define (config:user-dir)
    (cond
     [(getenv "XDG_CONFIG_HOME")]
     [(getenv "HOME") =>
      (lambda (home) (path-combine home ".config"))]
     [else #f]))

  (define (config:load-user)
    (cond
     [(config:user-dir) =>
      (lambda (path)
        (let ([fn (path-combine path "swish" "swish-lint.ss")])
          (load-config 'config:load-user fn
            (lambda (expr)
              (match expr
                [(find-files . ,(ls <= (,cmd . ,args)))
                 (unless (for-all string? ls)
                   (throw "find-files must all be strings"))
                 (config:find-files ls)]
                [(optional-checkers . ,ls)
                 (optional-checkers (append (optional-checkers) (make-optional-passes ls)))]
                [,_
                 (trace-expr `(config:load-user ignoring ,expr))])))))]))

  (define (make-optional-passes ls)
    (let lp ([ls ls] [acc '()])
      (match ls
        [() (reverse acc)]
        [((external . ,(ls <= (,cmd . ,args))) . ,rest)
         (guard
          (for-all
           (lambda (x)
             (match x
               [filename #t]
               [(filename ,re) (guard (string? re)) #t]
               [,_ (string? x)]))
           ls))
         (lp rest (cons (make-external-checker ls) acc))]
        [((regexp ,type ,regexp) . ,rest)
         (guard (member type '("info" "warning" "error")))
         (lp rest
           (cons (make-regexp-checker (string->symbol type) regexp) acc))]
        [(,clause . ,_)
         (errorf 'make-optional-passes "invalid clause: ~a" clause)])))
  )
