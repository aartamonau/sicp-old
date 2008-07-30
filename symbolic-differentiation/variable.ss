(module variable scheme
  (provide variable? same-variable?)

  (define (variable? x) (symbol? x))
  (define (same-variable? x y) (eq? x y)))