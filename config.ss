(library (config)
  (export
   config:load-project
   )
  (import
   (chezscheme)
   (config-params)
   (swish imports)
   (trace)
   )
  (define (config:load-project path)
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
         (trace-expr `(config:load-project => ,(exit-reason->english reason)))]
        [,exprs
         (for-each
          (lambda (expr)
            (match (try (process-project expr))
              [`(catch ,reason)
               (trace-expr `(config:load-project => ,(exit-reason->english reason)))]
              [,_ (void)]))
          exprs)])))

  (define (process-project expr)
    (match expr
      [(definition-keywords . ,ls)
       (unless (for-all string? ls)
         (throw "definition-keywords must all be strings"))
       (config:definition-keywords ls)]
      [,_
       (trace-expr `(config:load-project ignoring ,expr))]))
  )
