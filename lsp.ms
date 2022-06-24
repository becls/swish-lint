(import
 (json)
 (lsp)
 (testing pipe)
 (testing rpc)
 (tower)
 )

(define (capture-progress)
  (define-tuple <progress> token kind message percentage)
  (rpc-client:set-event-handler 'rpc 'progress
    (lambda (event)
      (and (equal? (json:ref event 'method #f) "$/progress")
           (let* ([params (json:get event 'params)]
                  [value (json:get params 'value)])
             (<progress> make
               [token (json:get params 'token)]
               [kind (json:get value 'kind)]
               [message (json:ref value 'message #f)]
               [percentage (json:ref value 'percentage #f)]))))
    self)
  (lambda (token)
    (receive
     [`(<progress> ,@token [kind "begin"] ,percentage)
      (let lp ([prior percentage])
        (receive
         [`(<progress> ,@token [kind "end"]) 'ok]
         [`(<progress> ,@token [kind "report"] ,percentage)
          (assert (>= percentage prior))
          (lp percentage)]))])))

(define (start-lsp-server path)
  (define-values (ip1 op1) (make-pipe "pipe1"))
  (define-values (ip2 op2) (make-pipe "pipe2"))
  (define-values (tip top) (make-pipe "trace pipe"))

  (log-file ":memory:")
  (match-let*
   ([,ip1 (binary->utf8 ip1)]
    [,op1 (binary->utf8 op1)]
    [,ip2 (binary->utf8 ip2)]
    [,op2 (binary->utf8 op2)]
    [#(ok ,_) (supervisor:start&link 'main-sup 'one-for-all 0 1
                (append
                 (make-swish-sup-spec (list swish-event-logger))
                 (tower:sup-spec 0)
                 (lsp:sup-spec #f ip1 op2 #t)))]
    [#(ok ,rpc) (rpc-client:start&link 'rpc
                  (lambda (msg)
                    ;;(json:write (trace-output-port) msg 0)
                    (lsp:write-msg op1 msg)
                    (flush-output-port op1)))]
    [,rlp (spawn&link
           (lambda ()
             (lsp:read-loop ip2
               (lambda (msg)
                 ;;(json:write (trace-output-port) msg 0)
                 (rpc-client:message rpc msg)))))])
   (define wait-for-progress (capture-progress))
   (rpc-client:call rpc
     (json:make-object
      [jsonrpc "2.0"]
      [method "initialize"]
      [params
       (json:make-object
        [rootUri
         (cond
          [(and path (format "file://~a" path))]
          [else #\nul])]
        [capabilities (json:make-object)])]))
   (rpc-client:notify rpc
     (json:make-object
      [jsonrpc "2.0"]
      [method "initialized"]
      [params (json:make-object)]))
   (when path
     (wait-for-progress "enumerate-directories"))
   rpc))

(define (stop-lsp-server rpc)
  (rpc-client:call rpc
    (json:make-object
     [jsonrpc "2.0"]
     [method "shutdown"]
     [params (json:make-object)]))

  (when (whereis 'log-db)
    (transaction 'log-db 'ok))
  (cond
   [(whereis 'main-sup) =>
    (lambda (pid)
      (define os (open-output-string))
      (unlink pid)
      (monitor pid)
      (parameterize ([console-error-port os])
        (kill pid 'shutdown)
        (receive (after 15000
                   (display (get-output-string os))
                   (throw 'timeout-in-stop))
          [`(DOWN ,_ ,@pid ,_) 'ok])))]
   [else 'ok]))

(isolate-mat no-root-folder ()
  (let ([rpc (start-lsp-server #f)])
    (on-exit (stop-lsp-server rpc)
      'ok)))

(isolate-mat smoke ()
  (let ([rpc (start-lsp-server (get-real-path "."))])
    (on-exit (stop-lsp-server rpc)
      'ok)))
