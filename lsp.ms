(import
 (json)
 (lsp)
 (software-info)
 (testing pipe)
 (testing rpc)
 (tower)
 )

(software-info:install)

(define-syntax code
  (syntax-rules ()
    [(_ str ...)
     (ct:join #\newline str ...)]))

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

(define (capture-test-sync)
  (let ([me self])
    (spawn
     (lambda ()
       (event-mgr:add-handler
        (lambda (event)
          (match event
            [(test-sync . ,_) (send me event)]
            [,_ #f])))
       (receive))))
  (lambda (uri)
    (receive [(test-sync . ,@uri) 'ok])))

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

(define (get-definitions uri line0 char0)
  (rpc-client:call 'rpc
    (json:make-object
     [jsonrpc "2.0"]
     [method "textDocument/definition"]
     [params
      (json:make-object
       [textDocument
        (json:make-object
         [uri uri])]
       [position
        (json:make-object
         [line line0]
         [character char0])])])))

(define (get-highlights uri line0 char0)
  (rpc-client:call 'rpc
    (json:make-object
     [jsonrpc "2.0"]
     [method "textDocument/documentHighlight"]
     [params
      (json:make-object
       [textDocument
        (json:make-object
         [uri uri])]
       [position
        (json:make-object
         [line line0]
         [character char0])])])))

(define (get-references uri line0 char0)
  (rpc-client:call 'rpc
    (json:make-object
     [jsonrpc "2.0"]
     [method "textDocument/references"]
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

(define (extract-range r)
  (list
   'from
   (json:get r '(start line))
   (json:get r '(start character))
   'to
   (json:get r '(end line))
   (json:get r '(end character))))

(isolate-mat defns/refs ()
  (define (extract-ranges ls)
    (map
     (lambda (obj)
       (list* (json:ref obj 'uri #f)
         (extract-range (json:get obj 'range))))
     ls))
  (define (get-defns uri line0 char0)
    (extract-ranges (get-definitions uri line0 char0)))
  (define (get-refs uri line0 char0)
    (extract-ranges (get-references uri line0 char0)))
  (define (get-local-refs uri line0 char0)
    ;; For sanity, slam the uri into the extracted ranges
    (map
     (lambda (ls)
       (assert (eq? (car ls) #f))
       (cons uri (cdr ls)))
     (extract-ranges (get-highlights uri line0 char0))))

  (define (check-range range f)
    ;; All calls f within the range should be the same. Calls outside
    ;; the range should be different.
    (match range
      [(,uri from ,sl ,sc to ,el ,ec)
       (let ([r (f uri sl sc)])
         (when (not (null? r))
           (assert (not (equal? r (f uri sl (- sc 1))))))
         (do ([c (+ sc 1) (+ c 1)]) ((= c ec))
           (assert (equal? r (f uri sl c))))
         (when (not (null? r))
           (assert (not (equal? r (f uri sl (+ ec 1))))))
         r)]))

  (let ([rpc (start-lsp-server (get-real-path "."))])
    (on-exit (stop-lsp-server rpc)
      (define wait-for-test-sync (capture-test-sync))

      (assert (null? (get-defns "file:///tmp/does-not-exist.ss" 0 0)))
      (assert (null? (get-local-refs "file:///tmp/does-not-exist.ss" 0 0)))
      (assert (null? (get-refs "file:///tmp/does-not-exist.ss" 0 0)))
      (let* ([uri1 "file:///tmp/file1.ss"]
             [uri2 "file:///tmp/file2.ss"]
             [uris (list uri1 uri2)])
        (for-each
         (lambda (uri) (did-open uri ""))
         uris)
        (for-each wait-for-test-sync uris)
        (did-change uri1
          (code
           "(define foo 12)"
           "(define bar 13)"
           "(define baz (+ foo bar))"
           "(define #{g0 irkaa9foht6pk4yacg2xmr1pt-0} 14)"
           "(define |nonstandard symbol| 15)"
           ))
        (did-change uri2
          (code
           "(let ([sum (+ foo bar baz)])"
           "  sum)"
           "(let ([prod (* #{g0 irkaa9foht6pk4yacg2xmr1pt-0} |nonstandard symbol|)])"
           "  prod)"
           ))
        (for-each wait-for-test-sync uris)

        (match-let*
         ([,foo-defn `(,uri1 from 0 8 to 0 11)]
          [,bar-defn `(,uri1 from 1 8 to 1 11)]
          [,baz-defn `(,uri1 from 2 8 to 2 11)]
          [,foo-ref1 `(,uri1 from 2 15 to 2 18)]
          [,bar-ref1 `(,uri1 from 2 19 to 2 22)]
          [,gen-defn `(,uri1 from 3 8 to 3 41)]
          [,nss-defn `(,uri1 from 4 8 to 4 28)]

          [,sum-ref1 `(,uri2 from 0 7 to 0 10)]
          [,foo-ref2 `(,uri2 from 0 14 to 0 17)]
          [,bar-ref2 `(,uri2 from 0 18 to 0 21)]
          [,baz-ref2 `(,uri2 from 0 22 to 0 25)]
          [,sum-ref2 `(,uri2 from 1 2 to 1 5)]

          [,prod-ref1 `(,uri2 from 2 7 to 2 11)]
          [,gen-ref1  `(,uri2 from 2 15 to 2 48)]
          [,nss-ref1  `(,uri2 from 2 49 to 2 69)]
          [,prod-ref2 `(,uri2 from 3 2 to 3 6)]

          ;; foo
          [(,@foo-defn) (check-range foo-defn get-defns)]
          [(,@foo-defn ,@foo-ref1) (check-range foo-defn get-local-refs)]
          [(,@foo-ref2) (check-range foo-ref2 get-local-refs)]
          [(,@foo-defn ,@foo-ref1 ,@foo-ref2) (check-range foo-defn get-refs)]

          ;; 12
          [() (get-defns uri1 0 12)]

          ;; bar
          [(,@bar-defn) (check-range bar-defn get-defns)]
          [(,@bar-defn ,@bar-ref1) (check-range bar-defn get-local-refs)]
          [(,@bar-ref2) (check-range bar-ref2 get-local-refs)]
          [(,@bar-defn ,@bar-ref1 ,@bar-ref2) (check-range bar-defn get-refs)]

          ;; 13
          [() (get-defns uri1 1 12)]

          ;; baz
          [(,@baz-defn) (check-range baz-defn get-defns)]
          [(,@baz-defn) (check-range baz-defn get-local-refs)]
          [(,@baz-ref2) (check-range baz-ref2 get-local-refs)]
          [(,@baz-defn ,@baz-ref2) (check-range baz-defn get-refs)]

          [(,@foo-defn) (check-range foo-ref1 get-defns)]
          [(,@bar-defn) (check-range bar-ref1 get-defns)]

          ;; gensym
          [(,@gen-defn) (check-range gen-defn get-defns)]
          [(,@gen-defn) (check-range gen-defn get-local-refs)]
          [(,@gen-ref1) (check-range gen-ref1 get-local-refs)]
          [(,@gen-defn ,@gen-ref1) (check-range gen-defn get-refs)]

          ;; nonstandard symbol
          [(,@nss-defn) (check-range nss-defn get-defns)]
          [(,@nss-defn) (check-range nss-defn get-local-refs)]
          [(,@nss-ref1) (check-range nss-ref1 get-local-refs)]
          [(,@nss-defn ,@nss-ref1) (check-range nss-defn get-refs)]

          [() (check-range sum-ref1 get-defns)] ; currently let bindings are not defns
          [() (check-range sum-ref2 get-defns)] ; currently let bindings are not defns
          [(,@foo-defn) (check-range foo-ref2 get-defns)]
          [(,@bar-defn) (check-range bar-ref2 get-defns)]
          [(,@baz-defn) (check-range baz-ref2 get-defns)]
          [() (check-range prod-ref1 get-defns)] ; currently let bindings are not defns
          [() (check-range prod-ref2 get-defns)] ; currently let bindings are not defns
          [(,@gen-defn) (check-range gen-ref1 get-defns)]
          [(,@nss-defn) (check-range nss-ref1 get-defns)])
         'ok)

        (did-change uri1
          (code
           "(define-syntax (define-thing x)"
           "  (syntax-case x ()"
           "   [(_) #'(begin 'thing)]))"
           ""
           "x"
           ))
        (wait-for-test-sync uri1)
        (match-let*
         ([,define-thing-defn `(,uri1 from 0 16 to 0 28)]
          [,x-defn `(,uri1 from 0 29 to 0 30)]
          [(,@define-thing-defn) (check-range define-thing-defn get-defns)]
          [(,@x-defn) (check-range x-defn get-defns)]
          ;; The current code incorrectly determines (define-thing x)
          ;; is the definition of `x`. A future implementation may
          ;; treat `x` as a local definition, and the following test
          ;; result would change.
          [(,@x-defn) (get-defns uri1 4 0)])
         'ok)

        ;; If we ever decide to treat marks as definitions, the test
        ;; results here would change.
        (did-change uri1
          (code "(list #0=0 #1=1 #1# #0#)"))
        (wait-for-test-sync uri1)
        (match-let*
         ([() (get-defns uri1 0 6)]
          [() (get-defns uri1 0 11)]
          [() (get-defns uri1 0 16)]
          [() (get-defns uri1 0 20)])
         'ok)

        (for-each did-close uris)
        'ok))))
