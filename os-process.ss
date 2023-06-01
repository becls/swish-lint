;;; Copyright 2023 Beckman Coulter, Inc.
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

(library (os-process)
  (export
   os-process:start&link
   os-process:stop
   os-process:write
   )
  (import
   (chezscheme)
   (swish imports)
   )

  (define (os-process:start&link cmd args type get put get-trace)
    (gen-server:start&link #f cmd args type get put get-trace))

  (define (os-process:write who data)
    (gen-server:call who `#(write ,data) 'infinity))

  (define (os-process:stop who exit-status)
    (gen-server:call who `#(stop ,exit-status)))

  (define-state-tuple <os-process-state>
    os-pid
    ip
    op
    ep
    put                                 ; (lambda (op data) ...)
    exit-status
    reader                              ; pid
    tracer                              ; pid
    )

  (define (init cmd args type get put get-trace)
    (define me self)
    (let*-values
        ([(op ip ep os-pid) (spawn-os-process cmd args me)]
         [(ip op ep)
          (match type
            [binary (values ip op ep)]
            [utf8 (values (binary->utf8 ip)
                    (binary->utf8 op)
                    (binary->utf8 ep))])])
      (process-trap-exit #t)
      (unless put
        (force-close-output-port op))
      `#(ok
         ,(<os-process-state> make
            [os-pid os-pid]
            [ip ip]
            [op op]
            [ep ep]
            [put put]
            [exit-status 1]
            [reader (spawn&link (lambda () (get ip me)))]
            [tracer (spawn&link (lambda () (get-trace ep me)))]
            ))))

  (define (terminate reason state)
    ($state open [os-pid ip op ep exit-status reader tracer])
    (when os-pid
      (osi_kill* os-pid exit-status))
    (when (memq reason '(normal shutdown))
      (kill reader 'shutdown)
      (kill tracer 'shutdown))
    (try (close-input-port ip))
    (try (close-input-port ep))
    (force-close-output-port op))

  (define (handle-call msg from state)
    (match msg
      [#(write ,data)
       ($state open [op put])
       (put op data)
       (flush-output-port op)
       `#(reply ok ,state)]
      [#(stop ,exit-status)
       `#(stop normal normal ,($state copy [exit-status exit-status]))]))

  (define (handle-cast msg state) (match msg))

  (define (handle-info msg state)
    (match msg
      [`(EXIT ,from ,reason)
       `#(stop ,reason ,state)]
      [#(process-terminated ,os-pid ,exit-status ,term-signal)
       `#(stop
          ,(if (zero? term-signal)
               'normal
               msg)
          ,($state copy [os-pid #f] [exit-status exit-status]))]))
  )
