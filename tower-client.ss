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

#!chezscheme
(library (tower-client)
  (export
   abs-path->uri
   tower-client:call
   tower-client:get-completions
   tower-client:get-definitions
   tower-client:get-local-references
   tower-client:get-references
   tower-client:log
   tower-client:reset-directory
   tower-client:shutdown-server
   tower-client:start&link
   tower-client:update-keywords
   tower-client:update-references
   uri->abs-path
   )
  (import
   (chezscheme)
   (swish imports)
   )

  (define (uri->abs-path uri)
    (match (pregexp-match (re "file://(.*)") uri)
      [(,_ ,filename)
       (meta-cond
        [windows?
         (pregexp-replace* (re "\\\\")
           (match (pregexp-match (re "/([A-Za-z])%3[Aa](.*)") filename)
             [(,_ ,drive ,filename)
              (string-append (string-upcase drive) ":" filename)]
             [,_
              filename])
           "/")]
        [else filename])]
      [,_ (throw `#(unhandled-uri ,uri))]))

  (define (abs-path->uri path)
    (meta-cond
     [windows?
      (string-append "file:///"
        (string-downcase (substring path 0 1))
        "%3A"
        (substring path 2 (string-length path)))]
     [else
      (string-append "file://" path)]))

  (define (tower-client:start&link)
    (define-state-tuple <tower-client>
      ws
      id
      id-map                            ; id -> from
      sync-token
      )
    (define (try-connect state)
      (match (try (ws:connect "localhost" 51342 "/tower" self))
        [`(catch ,_) #f]
        [,ws
         (sync ws state)
         ws]))
    (define (do-connect state)
      (cond
       [($state ws)]
       [(try-connect state)]
       [else
        ;; Explicitly generating the executable's name here. If this
        ;; code is running via Swish script, we want to make sure to
        ;; fire up the Tower correctly.
        (let ([cmd (path-combine (path-parent (app:path)) "swish-lint")])
          (fprintf (trace-output-port) "Launching ~a\n" cmd)
          (spawn-os-process-detached cmd '("--tower"))
          (let lp ([n 1])
            (receive (after 200 'ok))
            (fprintf (trace-output-port) "Reconnecting...\n")
            (cond
             [(try-connect state)]
             [(< n 10) (lp (+ n 1))]
             [else (throw 'unable-to-connect)])))]))
    (define (sync ws state)
      (cond
       [($state sync-token) =>
        (lambda (sync-token)
          (ws:send ws
            (json:object->bytevector
             (json:make-object
              [method "synchronize"]
              [params
               (json:make-object
                [sync-token sync-token])]))))]))
    (define (rpc-reply x)
      (cond
       [(json:ref x 'error #f) =>
        (lambda (err) (make-fault (json:ref err 'message #f)))]
       [else
        (let ([res (json:ref x 'result #!bwp)])
          (when (eq? res #!bwp)
            (throw `#(unhandled-message ,x)))
          res)]))
    (define (init)
      `#(ok ,(<tower-client> make
               [ws #f]
               [id 0]
               [id-map (ht:make equal-hash eq? fixnum?)]
               [sync-token #f])))
    (define (terminate reason state)
      (ws:close ($state ws)))
    (define (handle-call msg from state)
      (match msg
        [#(call ,msg)
         (match (try (do-connect state))
           [`(catch ,reason ,err)
            `#(reply ,err ,state)]
           [,ws
            (let ([id (+ ($state id) 1)])
              (json:set! msg 'id id)
              (ws:send ws (json:object->bytevector msg))
              `#(no-reply
                 ,($state copy
                    [ws ws]
                    [id id]
                    [id-map (ht:set ($state id-map) id from)])))])]))
    (define (handle-cast msg state)
      (match msg
        [#(cast ,msg)
         (match (try (do-connect state))
           [`(catch ,_)
            `#(no-reply ,state)]
           [,ws
            (ws:send ws (json:object->bytevector msg))
            `#(no-reply ,($state copy [ws ws]))])]))
    (define (handle-info msg state)
      (match msg
        [#(ws:message ,ws ,message)
         (let ([msg (if (string? message)
                        (json:string->object message)
                        (json:bytevector->object message))])
           (match (json:ref msg 'id #f)
             [#f
              (match (json:ref msg 'method #f)
                ["synchronize"
                 `#(no-reply
                    ,($state copy
                       [sync-token (json:ref msg '(params sync-token) #f)]))]
                [,_
                 (fprintf (console-error-port) "Unhandled Tower event\n")
                 (json:write (console-error-port) msg 0)
                 (newline (trace-output-port))
                 (flush-output-port (trace-output-port))
                 `#(no-reply ,state)])]
             [,id
              (match (ht:ref ($state id-map) id #f)
                [#f
                 (fprintf (console-error-port) "Unexpected message\n")
                 (json:write (console-error-port) msg 0)
                 (newline (trace-output-port))
                 (flush-output-port (trace-output-port))
                 `#(no-reply ,state)]
                [,from
                 (let ([reply (rpc-reply msg)]
                       [state ($state copy
                                [id-map (ht:delete ($state id-map) id)])])
                   (gen-server:reply from reply)
                   `#(no-reply ,state))])]))]
        [#(ws:closed ,ws ,code ,reason)
         `#(no-reply
            ,(cond
              [(eq? ws ($state ws))
               (ht:fold ($state id-map)
                 (lambda (k v acc)
                   (gen-server:reply v (make-fault 'disconnected))
                   acc)
                 (void))
               ($state copy
                 [ws #f]
                 [id 0]
                 [id-map (ht:make equal-hash eq? fixnum?)])]
              [else state]))]
        [#(ws:init ,ws)
         `#(no-reply ,state)]))
    (gen-server:start&link 'tower-client))

  (define (tower-client:call msg)
    (gen-server:call 'tower-client `#(call ,msg)))

  (define (tower-client:cast msg)
    (gen-server:cast 'tower-client `#(cast ,msg)))

  (define (tower-client:get-completions filename line char prefix)
    (tower-client:call
     (json:make-object
      [method "get-completions"]
      [params
       (json:make-object
        [filename filename]
        [line line]
        [char char]
        [prefix prefix])])))

  (define (tower-client:get-definitions filename name)
    (tower-client:call
     (json:make-object
      [method "get-definitions"]
      [params
       (json:make-object
        [filename filename]
        [name name])])))

  (define (tower-client:get-references filename name)
    (tower-client:call
     (json:make-object
      [method "get-references"]
      [params
       (json:make-object
        [filename filename]
        [name name])])))

  (define (tower-client:get-local-references filename name)
    (tower-client:call
     (json:make-object
      [method "get-local-references"]
      [params
       (json:make-object
        [filename filename]
        [name name])])))

  (define (tower-client:log msg)
    (tower-client:cast
     (json:make-object
      [method "log"]
      [params
       (json:make-object
        [timestamp (erlang:now)]
        [message msg])])))

  (define (tower-client:reset-directory dir)
    (tower-client:call
     (json:make-object
      [method "reset-directory"]
      [params
       (json:make-object
        [directory dir])])))

  (define (tower-client:update-keywords keywords)
    ;; Keywords is a list of JSON objects containing: keyword, meta
    (tower-client:call
     (json:make-object
      [method "update-keywords"]
      [params
       (json:make-object
        [keywords keywords])])))

  (define (tower-client:update-references filename refs)
    ;; Refs is a list of JSON objects containing: name, line, char, meta
    (tower-client:call
     (json:make-object
      [method "update-references"]
      [params
       (json:make-object
        [filename filename]
        [references refs])])))

  (define (tower-client:shutdown-server)
    (tower-client:call
     (json:make-object
      [method "shutdown"]
      [params (json:make-object)])))
  )
