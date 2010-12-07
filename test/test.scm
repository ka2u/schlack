#!/opt/gauche/bin/gosh

(add-load-path "/home/kaz/dev/schlack/lib")
(use http.server.ssgi)

(run (lambda (env) "app process") ())
