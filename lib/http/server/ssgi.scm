
(define-module http.server.ssgi
  (use gauche.net)
  (use srfi-13)
  (export run))
(select-module http.server.ssgi)

(debug-print-width #f)
(define *max-request-size* 131072)
(define *timeout* 300)

(define (run app . env)
  (let ([env (get-optional env #f)])
    (if (eq? env #f)
        (let ([env (hash-table 'equal?
                               '("SERVER_PORT"       . 8088)
                               '("SERVER_NAME"       . "localhost")
                               '("SCRIPT_NAME"       . "")
                               '("REMOTE_ADDR"       . "")
                               '("ssgi.version"      . "0.0001")
                               '("ssgi.errors"       . "")
                               '("ssgi.url_scheme"   . "http")
                               '("ssgi.run_once"     . #f)
                               '("ssgi.multithread"  . #f)
                               '("ssgi.multiprocess" . #f)
                               '("ssgi.streaming"    . #t)
                               '("ssgi.nonblocking"  . #f)
                               '("ssgix.input.input.buffered" . #t)
                               '("ssgix.io" . ""))])
          (accept-loop app env))
        (accept-loop app env))))

(define (accept-loop app env)
  (let ([sock (make-server-socket 
               'inet (hash-table-get env "SERVER_PORT") :reuse-addr? #t)]
        [sigpipe (sys-sigset SIGPIPE)]
        [proc_req_count 0]
        [max_reqs_per_child 0])
    (set-signal-handler! sigpipe #f)
    (while (or (equal? max_reqs_per_child 0)
               (< proc_req_count max_reqs_per_child))
      (let* ([accepted (socket-accept sock)])
        (socket-setsockopt accepted SOL_TCP TCP_NODELAY 1)
        (+ proc_req_count 1)
        (handle-response accepted 
                         (run-app app (handle-connection accepted env)))
        (socket-close accepted)))))

(define (handle-connection sock env)
  (parse-header env 
                (do-timeout 
                 (lambda () 
                   (read-block *max-request-size* (socket-input-port sock))))))

(define (parse-header env header)
  (let* ([lines (string-split (x->string header) "\r\n")]
        [reqs (string-split (string-incomplete->complete (car lines)) " ")]
        [ver-match (rxmatch #/(\d)\.(\d)/ (caddr reqs))]
        [uri-match (rxmatch #/(^[^?]*)(?:\?(.*))?$/ (cadr reqs))])
    (hash-table-put! env "REQUEST_METHOD" (car reqs))
    (hash-table-put! env "SERVER_PROTOCOL" #`"HTTP/,(ver-match 1).,(ver-match 2)")
    (hash-table-put! env "REQUEST_URI" (cadr reqs))
    (hash-table-put! env "PATH_INFO" (uri-match 1))
    (hash-table-put! env "QUERY_STRING" (uri-match 2))
    (hash-table-put! env "SCRIPT_NAME" "")
    (import-headers (cdr lines) env)
    #?=(hash-table-keys env)
    #?=(hash-table-values env)
    env))

(define (import-headers lines env)
  (let* ([token #/[^\]\[\x00-\x1f\x7f()<>@,\;:\\\"\/?={} \t]+/]
        [header-token #/^([^\]\[\x00-\x1f\x7f()<>@,\;:\\\"\/?={} \t]+): ?/])
    (if (string-null? (string-incomplete->complete (car lines)))
        env
        (let ([k (rxmatch token (string-incomplete->complete (car lines)))]
              [header (regexp-replace header-token  (string-incomplete->complete (car lines)) "")])
        (hash-table-put! env (k) header)
        (import-headers (cdr lines) env)))))

(define (run-app app env)
  (app env))
 
(define (handle-response sock res)
  (let ([port (socket-output-port sock)]
        [gmtime (sys-asctime (sys-gmtime (sys-time)))])
    (do-timeout 
     (lambda () 
       (display (string-append "HTTP/1.0 200 OK\r\n"
                               "Date: " gmtime
                               "Server: http.servere.ssgi\r\n\r\n"
                               "<h1>It works.</h1>"
                               ) port)))))

(define (do-timeout proc)
  (let ([wait_until (+ *timeout* (sys-time))])
    (with-signal-handlers ((SIGALRM #f))
      (lambda () 
        (sys-alarm *timeout*)
        (let ([res (guard 
                       (var ([eq? var EINTR] 
                             (let ([left (- wait_until (sys-time))])
                               (if (<= left 0)
                                   #f
                                   (sys-alarm (+ left 0.1))))))
                     (proc))])
          (sys-alarm 0) 
          res)))))

    