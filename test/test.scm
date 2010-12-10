#!/opt/gauche/bin/gosh

(add-load-path "/home/kaz/dev/schlack/lib")
(use schlack.handler.standalone)

(server-run (lambda () "app process"))
