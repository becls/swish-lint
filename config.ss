(library (config)
  (export
   config-output-port
   config:load-project
   config:load-user
   config:user-dir
   make-optional-passes
   output-env
   )
  (import
   (checkers)
   (chezscheme)
   (config-params)
   (swish imports)
   )
  (define config-output-port
    (make-parameter
     (make-custom-textual-output-port "bit-sink port"
       (lambda (str start n) n) #f #f #f)))

  (define (output-env)
    (cond
     [(getenv "XDG_CONFIG_HOME") =>
      (lambda (home)
        (fprintf (config-output-port) "XDG_CONFIG_HOME: ~a\n" home))]
     [(getenv "HOME") =>
      (lambda (home)
        (fprintf (config-output-port) "HOME: ~a\n" home))]
     [else
      (fprintf (config-output-port) "Neither HOME nor XDG_CONFIG_HOME are set\n")]))

  (define (load-config who path process-expr)
    (cond
     [(file-exists? path)
      (fprintf (config-output-port) "Found ~a configuration: ~a\n" who path)
      (match (try
              (let ([ip (open-file-to-read path)])
                (on-exit (close-port ip)
                  (let lp ()
                    (let ([x (read ip)])
                      (if (eof-object? x)
                          '()
                          (cons x (lp))))))))
        [`(catch ,reason)
         (fprintf (config-output-port) "~a: ~a\n" who (exit-reason->english reason))]
        [,exprs
         (for-each
          (lambda (expr)
            (match (try (process-expr expr))
              [`(catch ,reason)
               (fprintf (config-output-port) "~a: ~a\n" who (exit-reason->english reason))]
              [,_ (void)]))
          exprs)])]
     [else
      (fprintf (config-output-port) "~:(~a~) configuration not found: ~a\n" who path)]))

  (define (config:load-project path)
    (let ([fn (path-combine path ".swish" "swish-lint.ss")])
      (load-config 'project fn
        (lambda (expr)
          (match expr
            [(definition-keywords . ,ls)
             (unless (for-all string? ls)
               (throw "definition-keywords must all be strings"))
             (config:definition-keywords ls)]
            [,_
             (fprintf (config-output-port) "project: ignoring ~a\n" expr)])))))

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
          (load-config 'user fn
            (lambda (expr)
              (match expr
                [(find-files . ,(ls <= (,cmd . ,args)))
                 (unless (for-all string? ls)
                   (throw "find-files must all be strings"))
                 (config:find-files ls)]
                [(optional-checkers . ,ls)
                 (optional-checkers (append (optional-checkers) (make-optional-passes ls)))]
                [,_
                 (fprintf (config-output-port) "user: ignoring ~a\n" expr)])))))]))

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
