(define-module schlack.handler.standalone
  (use gauche.net)
  (use srfi-13)
  (use rfc.uri)
  (use rfc.822)
  (export server-run
          parse-header
          run-app))
(select-module schlack.handler.standalone)

(debug-print-width #f)
(define env (hash-table 'equal?
  '(SERVER_PORT . 8080)
  '(SERVER_NAME . 0)
  '(SCRIPT_NAME . "")
  '(REMOTE_ADDR . "")
  '(ssgi.version        . '(1 . 1))
  '(ssgi.errors         . (current-error-port))
  '(ssgi.url_scheme     . "http")
  '(ssgi.run_once       . #f)
  '(ssgi.multithread    . #f)
  '(ssgi.multiprocess   . #f)
  '(ssgi.streaming      . #t)
  '(ssgi.nonblocking    . #f)
  '(ssgi.input.buffered . #t)
  '(ssgi.io             . "")
  ))

(define *status-code-map*
  (hash-table 'eqv?
              '(100 . "Continue")
              '(101 . "Switching Protocols")
              '(200 . "OK")
              '(201 . "Created")
              '(202 . "Accepted")
              '(203 . "Non-Authoritative Information")
              '(204 . "No Content")
              '(205 . "Reset Content")
              '(206 . "Partial Content")
              '(300 . "Multiple Choices")
              '(301 . "Moved Permanently")
              '(302 . "Found")
              '(303 . "See Other")
              '(304 . "Not Modified")
              '(305 . "Use Proxy")
              '(306 . "(Unused)")
              '(307 . "Temporary Redirect")
              '(400 . "Bad Request")
              '(401 . "Unauthorized")
              '(402 . "Payment Required")
              '(403 . "Forbidden")
              '(404 . "Not Found")
              '(405 . "Method Not Allowed")
              '(406 . "Not Acceptable")
              '(407 . "Proxy Authentication Required")
              '(408 . "Request Timeout")
              '(409 . "Conflict")
              '(410 . "Gone")
              '(411 . "Length Required")
              '(412 . "Precondition Failed")
              '(413 . "Request Entity Too Large")
              '(414 . "Request-URI Too Long")
              '(415 . "Unsupported Media Type")
              '(416 . "Requested Range Not Satisfiable")
              '(417 . "Expectation Failed")
              '(500 . "Internal Server Error")
              '(501 . "Not Implemented")
              '(502 . "Bad Gateway")
              '(503 . "Service Unavailable")
              '(504 . "Gateway Timeout")
              '(505 . "HTTP Version Not Supported")
              ))


(define (env-put! key val)
  (hash-table-put! env key val))

(define (env-get key)
  (hash-table-get env key))

(define (server-run app)
  (let ([sock (make-server-socket 'inet 8088 :reuse-addr? #t)])
    (while #t
      (let ([accepted (socket-accept sock)])
        (parse-header accepted)
        (respond accepted (run-app app accepted))
        (socket-close accepted)))))

(define (parse-header sock)
  (guard (e [else (rerspond/ng sock 500)])
    (let* ([line (read-line (socket-input-port sock))])
      (rxmatch-case line
        [test eof-object? (lambda ())]
        [#/^(GET|HEAD|POST)\s+(\S+)\s+HTTP\/(\d+)\.(\d+)$/ (meth abs-path major minor)
            (receive (auth path q frag) (uri-decompose-hierarchical abs-path)
              (let1 path (uri-decode-string path :cgi-decode #t)
                (env-put! "REQUEST_METHOD" meth)
                (env-put! "SERVER_PROTOCOL" #`"HTTP/,|major|.,|minor|")
                (env-put! "REQUEST_URI" abs-path)
                (env-put! "PATH_INFO" path)
                (env-put! "QUERY_STRING" q)
                (env-put! "SCRIPT_NAME" "")
                (parse-leftover-header 
                 (rfc822-read-headers (socket-input-port sock)))
                ))]))))

(define (parse-leftover-header lis)
    (unless (null? lis)
      (let ([set (car lis)])
        (env-put! (car set) (cadr set))
        (parse-leftover-header (cdr lis)))))


(define (run-app app sock)
  (guard (e [else (rerspond/ng sock 500)])
    (eval `(,app ,env) (interaction-environment))))

(define (respond sock response)
  (guard (e [else (rerspond/ng sock 500)])
    (let ([port (socket-output-port sock)]
          [code (car response)]
          [header (cadr response)]
          [body (caddr response)])
      (define (p x) (display x port))
      (define (crlf) (display "\r\n" port))
      (p (env-get "SERVER_PROTOCOL")) (p " ") 
      (p code) (p (hash-table-get *status-code-map* code)) (crlf)
      (respond-header header (lambda (x) (p x)) (lambda () (crlf)))
      (p "Date: ") (p (sys-asctime (sys-gmtime (sys-time)))) ; added \r\n
      (p "Server: schlack.handler.standalone")(crlf)(crlf)
      (p body)
      (flush port))))

(define (respond-header header p crlf)
  (unless (null? header)
    (let ([elem (car header)])
      (p (car elem)) (p ": ") (p (cadr elem)) (crlf)
      (respond-header (cdr header) (lambda (x) (p x)) (lambda () (crlf))))))
    

(define (respond/ng sock code)
  (let ([body "Internal Server Error"])
    (respond sock `(500 
                    (("Content-Type" "text/html") 
                     ("Content-Length" ,(string-length body)))
                    ,body))))