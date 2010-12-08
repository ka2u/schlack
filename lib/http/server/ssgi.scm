
(define-module http.server.ssgi
  (use gauche.net)
  (use srfi-13)
  (export run))
(select-module http.server.ssgi)

(debug-print-width #f)
(define *max-request-size* 131072)
(define *timeout* 300)

(define (run app . args)
  (let-optionals* args ((host 0)
                        (port 8088)
                        (timeout 300)
                        (server_software "http.server.ssgi")
                        (server_ready (lambda ()))
                        (max_reqs_per_child 100))
    (accept-loop app (list host port timeout server_software server_ready max_reqs_per_child))))

(define (accept-loop app args)
  (let ([sock (make-server-socket 
               'inet (cadr args) :reuse-addr? #t)]
        [sigpipe (sys-sigset SIGPIPE)]
        [proc_req_count 0]
        [max_reqs_per_child 0])
    (set-signal-handler! sigpipe #f)
    (while (or (equal? max_reqs_per_child 0)
               (< proc_req_count max_reqs_per_child))
      (let* ([accepted (socket-accept sock)]
             [env (hash-table 'equal?
                              '("SERVER_PORT"       . (cadr args))
                              '("SERVER_NAME"       . (car args))
                              '("SCRIPT_NAME"       . "")
                              '("REMOTE_ADDR"       . 
                                (sockaddr-name (socket-getpeername accepted)))
                              '("ssgi.version"      . '(1 . 1))
                              '("ssgi.errors"       . (current-error-port))
                              '("ssgi.url_scheme"   . "http")
                              '("ssgi.run_once"     . #f)
                              '("ssgi.multithread"  . #f)
                              '("ssgi.multiprocess" . #f)
                              '("ssgi.streaming"    . #t)
                              '("ssgi.nonblocking"  . #f)
                              '("ssgix.input.buffered" . #t)
                              '("ssgix.io" . accepted))])
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
          (if (hash-table-exists? env (k))
              (hash-table-put! env (k) 
                                (string-append (hash-table-get env (k)) ", " header))
              (hash-table-put! env (k) header))
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

    