(define-module schlack.runner
  (use gauche.parseopt)
  (use schlack.handler.standalone)
  (export run))
(select-module schlack.runner)

(define (run args)
  (let* ([options (parse-args args)]
         [handler (hash-table-get options "server")]
        [app (load-ssgi (hash-table-get options "app"))]
        [server (make (guess handler))])
        (server-run server app)))

(define (parse-args args)
  (let-args (cdr args)
      ((app    "a|app=s")
       (host   "o|host=s")
       (port   "p|port=i")
       (server "s|server=s" "standalone")
       (socket "S|socket=s")
       (listen "l|listen=s")
       (daemonize "D|daemonize")
       (env    "e|env=s")
       (reload "r|reload")
       (Reload "R|Reload=s")
       (loader "L|loader=s")
       (access-log "access-log=s")
       (help    "h|help")
       (version "v|version")
       . restargs
       )
    (let ([opt (make-hash-table 'equal?)])
      (hash-table-put! opt "app" app)
      (hash-table-put! opt "host" host)
      (hash-table-put! opt "port" port)
      (hash-table-put! opt "server" server)
      (hash-table-put! opt "socket" socket)
      (hash-table-put! opt "listen" listen)
      opt)))

    
(define (load-ssgi name)
  (let ([inport (open-input-file name)])
    (read inport)))

    
(define (guess server)
  (cond 
   (else <standalone>)))