(module huffman (except-in scheme symbols)
  (provide encode decode)

  (define (make-leaf symbol weight)
    (list 'leaf symbol weight))

  (define (leaf? object)
    (eq? (car object) 'leaf))

  (define (symbol-leaf x) (cadr x))

  (define (weight-leaf x) (caddr x))

  (define (make-code-tree left right)
    (list left
          right
          (append (symbols left)
                  (symbols right))
          (+ (weight left)
             (weight right))))

  (define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))

  (define (left-branch tree) (car tree))

  (define (right-branch tree) (cadr tree))

  (define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))

  (define (encode message tree)
    (define (choose-branch symbol tree)
      (if (eqv? (memv symbol (symbols (right-branch tree))) false)
          (cons 0 (left-branch tree))
          (cons 1 (right-branch tree))))
    (define (encode-symbol symbol tree)
      (if (leaf? tree)
          (if (eqv? symbol (symbol-leaf tree))
              '()
              (error "Symbol cannot be encoded using specified tree: " (symbol->string symbol)))
          (let ((direction (choose-branch symbol tree)))
            (cons (car direction) (encode-symbol symbol (cdr direction))))))
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))

  (define (decode bits tree)
    (define (choose-branch bit tree)
      (cond ((= bit 0) (left-branch tree))
            ((= bit 1) (right-branch tree))
            (else (error "[choose-branch] Bad bit: " bit))))
    (define (decode-1 bits current-branch)
      (if (null? bits)
          '()
          (let ((next-branch
                 (choose-branch (car bits) current-branch)))
            (if (leaf? next-branch)
                (cons (symbol-leaf next-branch)
                      (decode-1 (cdr bits) tree))
                (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))

  (define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set)
                      (adjoin-set x (cdr set))))))

  (define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
          (cons (make-leaf (car pair)
                           (cdr pair))
                (make-leaf (cdr pairs)))))))