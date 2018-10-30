#lang peg

(provide (all-defined-out));
(provide parser);
(struct seq (a b) #:transparent);
(struct non-deterministic-choice (a b) #:transparent);
(struct repetition (a) #:transparent);
(struct atomic (a) #:transparent);

(define (parser str)
  (peg pdl str));


_ < [ \t\n]*;
EOI < ! . ;
pdl <- _ v:(non-deterministic) _ EOI -> v;
non-deterministic <- v1:(seq) _ v2:((~'U' _ seq _)*) -> (foldl (lambda (a b) (non-deterministic-choice b a)) v1 v2) ;
seq <- v1:(repetition) _ v2:((~[;] _ repetition)*) -> (foldl (lambda (a b) (seq b a)) v1 v2) ;
repetition <- v:(atomic) _ a:('*'?) _ -> (if (not (null? a)) (repetition v) v);
atomic <- v:[a-zA-Z] -> (atomic v);


test <- v:('a'?) -> v;