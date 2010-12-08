#!/opt/gauche/bin/gosh

(add-load-path "/home/kaz/dev/schlack/lib")
(use http.server.ssgi2)

(run (lambda () "app process"))
