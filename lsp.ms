(import
 (json)
 (lsp)
 (software-info)
 (testing pipe)
 (testing rpc)
 (tower)
 )

(software-info:install)

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

(define (did-open uri text)
  (rpc-client:notify 'rpc
    (json:make-object
     [jsonrpc "2.0"]
     [method "textDocument/didOpen"]
     [params
      (json:make-object
       [textDocument
        (json:make-object
         [uri uri]
         [text text])])])))

(define (did-change uri text)
  (rpc-client:notify 'rpc
    (json:make-object
     [jsonrpc "2.0"]
     [method "textDocument/didChange"]
     [params
      (json:make-object
       [textDocument
        (json:make-object
         [uri uri])]
       [contentChanges
        (list
         (json:make-object
          [text text]))])])))

(define (did-save uri text)
  (rpc-client:notify 'rpc
    (json:make-object
     [jsonrpc "2.0"]
     [method "textDocument/didSave"]
     [params
      (json:make-object
       [textDocument
        (json:make-object
         [uri uri])]
       [text text])])))

(define (did-close uri)
  (rpc-client:notify 'rpc
    (json:make-object
     [jsonrpc "2.0"]
     [method "textDocument/didClose"]
     [params
      (json:make-object
       [textDocument
        (json:make-object
         [uri uri])])])))

(define (get-completions uri line0 char0)
  (rpc-client:call 'rpc
    (json:make-object
     [jsonrpc "2.0"]
     [method "textDocument/completion"]
     [params
      (json:make-object
       [textDocument
        (json:make-object
         [uri uri])]
       [position
        (json:make-object
         [line line0]
         [character char0])])])))

(isolate-mat no-root-folder ()
  (let ([rpc (start-lsp-server #f)])
    (on-exit (stop-lsp-server rpc)
      'ok)))

(isolate-mat smoke ()
  (let ([rpc (start-lsp-server (get-real-path "."))])
    (on-exit (stop-lsp-server rpc)
      'ok)))

(isolate-mat completions ()
  (define (extract-labels ls)
    (map (lambda (obj) (json:ref obj 'label #f)) ls))

  (define (all uri str expect)
    (did-change uri str)
    (let ([max (string-length str)])
      (let lp ([n 0] [expect expect])
        (cond
         [(> n max) (assert (null? expect))]
         [else
          (let ([actual (extract-labels (get-completions uri 0 n))])
            (match expect
              [() (throw `#(not-enough-expected actual: ,actual))]
              [(,exp . ,rest)
               (unless (equal? actual exp)
                 (printf "~a: ~a\n" actual exp)
                 (throw `#(failed ,actual ,exp)))
               (lp (+ n 1) rest)]))]))))

  ;; TODO: This needs to be more robust in the presence of stray files
  ;; in the testing directory. Possibly need to create a separate
  ;; directory and write code into it instead of using swish-lint's
  ;; directory.
  (let ([rpc (start-lsp-server (get-real-path "."))])
    (on-exit (stop-lsp-server rpc)
      (assert (null? (get-completions "file:///tmp/does-not-exist.ss" 0 0)))
      (let ([uri "file:///tmp/foo.ss"])
        (did-open uri "")
        (all uri "1" '(() ()))
        (all uri "()" '(() () ()))
        (all uri "(1)" '(() () () ()))
        (all uri "(z)" '(() () ("zero?") ()))
        (all uri "(ze)" '(() () ("zero?") ("zero?") ()))
        (all uri "(zed)" '(() () () () () ()))
        (all uri "(#)" '(() () () ()))
        (all uri "(#%)" '(() () () () ()))
        (all uri "(#%z)" '(() () ("zero?") ("zero?") ("zero?") ()))
        (all uri "(#%ze)" '(() () ("zero?") ("zero?") ("zero?") ("zero?") ()))
        (all uri "(#%zed)" '(() () () () () () () ()))
        (all uri "(#2%z)" '(() () ("zero?") ("zero?") ("zero?") ("zero?") ()))
        (all uri "(#2%ze)" '(() () ("zero?") ("zero?") ("zero?") ("zero?") ("zero?") ()))
        (all uri "(#2%zed)" '(() () () () () () () () ()))

        ;; Completions should not include the identifier under the
        ;; cursor when it's the only reference.
        ;;
        ;; Completions don't include the current non-keyword
        ;; identifiers until the file is saved.
        (match-let*
         ([,_ (did-save uri "(let ([zyx")]
          [() (extract-labels (get-completions uri 0 10))]
          [,_ (did-save uri "(let ([zyx 12]) (+ z 12))")]
          [("zyx" "zero?") (extract-labels (get-completions uri 0 20))]
          [,_ (did-save uri "(let ([zyx 12]) (+ zyx 12))")]
          [("zyx") (extract-labels (get-completions uri 0 22))])
         'ok)
        (did-close uri)
        'ok))))
