(library (doc)
  (export
   doc:get-text
   doc:get-value-near
   doc:start
   doc:start&link
   doc:updated
   )
  (import
   (chezscheme)
   (cursor)
   (read)
   (swish imports)
   (trace)
   )

  (define-state-tuple <document> cursor worker-pid on-changed)

  (define (init on-changed)
    `#(ok ,(<document> make
             [cursor #f]
             [worker-pid #f]
             [on-changed on-changed])))

  (define (terminate reason state) 'ok)

  (define (handle-call msg from state)
    (match msg
      [get-text
       `#(reply ,(cursor->string ($state cursor)) ,state)]
      [#(get-value-near ,line1 ,char1)
       (trace-time `(get-value-near ,line1 ,char1)
         (let* ([cursor (cursor:goto-line! ($state cursor) (fx- line1 1))]
                [text (line-str (cursor-line cursor))])
           (match (try
                   (let-values ([(type value bfp efp)
                                 (read-token-near/col text char1)])
                     (and (eq? type 'atomic)
                          (match value
                            [,_ (guard (symbol? value))
                             (get-symbol-name value)]
                            [($primitive ,value)
                             (get-symbol-name value)]
                            [($primitive ,_ ,value)
                             (get-symbol-name value)]
                            [,_ #f]))))
             [`(catch ,reason)
              (trace-expr
               `(get-value-near ,line1 ,char1 => ,(exit-reason->english reason)))
              `#(reply #f ,state)]
             ["" `#(reply #f ,state)]
             [,result
              `#(reply ,result ,state)])))]))

  (define (handle-cast msg state)
    (match msg
      [#(updated ,change ,skip-delay?)
       (let ([pid ($state worker-pid)])
         (when pid (kill pid 'cancelled)))
       (let ([cursor
              (cond
               [(not change) (string->cursor "")]
               [(string? change) (string->cursor change)]
               [else (lsp:change-content ($state cursor) change)])])
         (cond
          [($state on-changed) =>
           (lambda (on-changed)
             (let ([pid (on-changed change (cursor:copy cursor) skip-delay?)])
               (monitor pid)
               `#(no-reply
                  ,($state copy
                     [cursor cursor]
                     [worker-pid pid]))))]
          [else
           `#(no-reply ($state copy [cursor cursor]))]))]))

  (define (handle-info msg state)
    (match msg
      [`(DOWN ,_ ,pid ,reason)
       (cond
        [(eq? pid ($state worker-pid))
         (unless (eq? reason 'normal)
           (trace-expr `(doc-worker ,(exit-reason->english reason))))
         `#(no-reply ,($state copy [worker-pid #f]))]
        [else
         `#(no-reply ,state)])]))

  (define (doc:start&link on-changed)
    (gen-server:start&link #f on-changed))

  (define (doc:start on-changed)
    (gen-server:start #f on-changed))

  (define (doc:get-text who)
    (gen-server:call who 'get-text))

  (define (doc:get-value-near who line1 char1)
    (gen-server:call who `#(get-value-near ,line1 ,char1)))

  (define (doc:updated who change skip-delay?)
    (gen-server:cast who `#(updated ,change ,skip-delay?)))
  )
