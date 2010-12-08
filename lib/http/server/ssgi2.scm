(define-module http.server.ssgi2
  (use gauche.net)
  (use srfi-13)
  (use rfc.uri)
  (use rfc.822)
  (export run
          parse-header
          run-app))
(select-module http.server.ssgi2)

(debug-print-width #f)
(define env (hash-table 'equal?
  '("SERVER_PORT" . 8080)
  '("SERVER_NAME" . 0)
  '("SCRIPT_NAME" . "")
  '("REMOTE_ADDR" . "")
  '("ssgi.version"        . '(1 . 1))
  '("ssgi.errors"         . (current-error-port))
  '("ssgi.url_scheme"     . "http")
  '("ssgi.run_once"       . #f)
  '("ssgi.multithread"    . #f)
  '("ssgi.multiprocess"   . #f)
  '("ssgi.streaming"      . #t)
  '("ssgi.nonblocking"    . #f)
  '("ssgi.input.buffered" . #t)
  '("ssgi.io"             . "")))

(define (env-put! key val)
  (hash-table-put! env key val))

(define (env-get key)
  (hash-table-get env key))

(define (run app)
  (let ([sock (make-server-socket 'inet 8088 :reuse-addr? #t)])
    (while #t
      (let ([accepted (socket-accept sock)])
        (parse-header accepted)
        (run-app app)
        (respond accepted)
        (socket-close accepted)))))

(define (parse-header sock)
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
              (parse-left-header 
               (rfc822-read-headers (socket-input-port sock)))
            ))])))

(define (parse-left-header lis)
  (let ([set (car lis)])
    (unless (null? (cdr lis))
    (env-put! (car set) (cadr set))
    (parse-left-header (cdr lis)))))


(define (run-app app)
  (app))

(define (respond sock)
  (let ([port (socket-output-port sock)])
    (define (p x) (display x port))
    (define (crlf) (display "\r\n" port))
    (p "HTTP/1.0 200 OK")(crlf)
    (p "Date: ") (p (sys-asctime (sys-gmtime (sys-time)))) ; added \r\n
    (p "Server: http.server.ssgi2")(crlf)(crlf)
    (p "<h1>It works.</h1>")
    ))
