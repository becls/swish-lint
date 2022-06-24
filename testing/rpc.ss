;;; Copyright 2022 Beckman Coulter, Inc.
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
(library (testing rpc)
  (export
   rpc-client:call
   rpc-client:message
   rpc-client:notify
   rpc-client:send
   rpc-client:set-event-handler
   rpc-client:start&link
   )
  (import
   (chezscheme)
   (swish imports)
   )

  (define (rpc-reply x)
    (cond
     [(json:ref x 'error #f) =>
      (lambda (err) (make-fault (json:ref err 'message #f)))]
     [else
      (let ([res (json:ref x 'result #!bwp)])
        (when (eq? res #!bwp)
          (throw `#(missing-result ,x)))
        res)]))

  (define (rpc-client:start&link name write-msg)
    (define-state-tuple <rpc-client>
      id
      id-map                           ; id -> (lambda (id reply) ...)
      event-handlers                   ; (<event-handler> ...)
      )
    (define-tuple <event-handler> type pred pid mon)
    (define (init)
      `#(ok ,(<rpc-client> make
               [id 0]
               [id-map (ht:make values eq? fixnum?)]
               [event-handlers '()])))
    (define (terminate reason state) 'ok)

    (define (handle-call msg from state)
      (match msg
        [#(send ,msg ,callback)
         (let ([id (fx+ ($state id) 1)])
           (json:set! msg 'id id)
           (write-msg msg)
           `#(reply ok
               ,($state copy
                  [id id]
                  [id-map (ht:set ($state id-map) id callback)])))]
        [#(call ,msg)
         (let ([callback (lambda (id reply) (gen-server:reply from reply))]
               [id (fx+ ($state id) 1)])
           (json:set! msg 'id id)
           (write-msg msg)
           `#(no-reply
              ,($state copy
                 [id id]
                 [id-map (ht:set ($state id-map) id callback)])))]
        [#(notify ,msg)
         (write-msg msg)
         `#(reply ok ,state)]
        [#(set-event-handler ,type ,pred ,pid)
         `#(reply ok
             ,($state copy*
                [event-handlers
                 (cons (<event-handler> make
                         [type type]
                         [pred pred]
                         [pid pid]
                         [mon (monitor pid)])
                   (remove-type type event-handlers))]))]))
    (define (handle-cast msg state) (match msg))
    (define (handle-info msg state)
      (match msg
        [#(message ,msg)
         (match (json:ref msg 'id #f)
           [#f
            (call-event-handler msg ($state event-handlers))
            `#(no-reply ,state)]
           [,id
            (match (ht:ref ($state id-map) id #f)
              [#f
               (fprintf (console-error-port) "Unexpected message\n")
               (json:write (console-error-port) msg 0)
               (newline (console-error-port))
               (flush-output-port (console-error-port))
               `#(no-reply ,state)]
              [,callback
               (let ([reply (rpc-reply msg)]
                     [state ($state copy
                              [id-map (ht:delete ($state id-map) id)])])
                 (callback id reply)
                 `#(no-reply ,state))])])]
        [`(DOWN ,mon ,pid ,reason)
         `#(no-reply
            ,($state copy*
               [event-handlers (remove-mon mon event-handlers)]))]))
    (define (call-event-handler event event-handlers)
      (match event-handlers
        [() #f]
        [(`(<event-handler> ,pred ,pid) . ,rest)
         (cond
          [(pred event) => (lambda (value) (send pid value))]
          [else (call-event-handler event rest)])]))
    (define (remove-type type event-handlers)
      (match event-handlers
        [() '()]
        [(`(<event-handler> [type ,@type] ,mon) . ,rest)
         (demonitor&flush mon)
         rest]
        [(,first . ,rest) (cons first (remove-type type rest))]))
    (define (remove-mon mon event-handlers)
      (match event-handlers
        [() '()]
        [(`(<event-handler> [mon ,@mon]) . ,rest) rest]
        [(,first . ,rest) (cons first (remove-mon mon rest))]))
    (gen-server:start&link name))

  (define (rpc-client:send who msg callback)
    (gen-server:call who `#(send ,msg ,callback)))

  (define (rpc-client:call who msg)
    (gen-server:call who `#(call ,msg) 'infinity))

  (define (rpc-client:notify who msg)
    (gen-server:call who `#(notify ,msg)))

  (define (rpc-client:set-event-handler who type pred pid)
    (gen-server:call who `#(set-event-handler ,type ,pred ,pid)))

  (define (rpc-client:message who msg)
    (send who `#(message ,msg)))
  )
