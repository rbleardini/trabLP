#lang racket


(require "pdl.rkt")
(struct graph-struct (l) #:transparent)
(struct node (name) #:transparent)
(struct edge (origin destination label) #:transparent)
;(struct failure (estado) #:transparent)

(define test-graph (graph-struct (list (edge (node "A") (node "B") "a") (edge (node "A") (node "A") "b")
                                       (edge (node "B") (node "A") "a") (edge (node "B") (node "B") "a")
                                       (edge (node "A") (node "C") "a") (edge (node "A") (node "C") "b")
                                       (edge (node "B") (node "C") "b") (edge (node "C") (node "D") "c"))))

;(define grafo '((A B a)(A A b) (B A a)(B B a) (A C a)(A C b))) ;Origem Destino Aresta
;(define prog  '("A" (a : a U a : b)))

(define (next-world world atomic graph [lst '()])
      (if (null? (graph-struct-l graph))
          lst
          (let*
              ([first-transition (car (graph-struct-l graph))]
               [origin (edge-origin first-transition)]
               [destination (node-name (edge-destination first-transition))]
               [label (edge-label first-transition)])
            (if (and
                 (equal? world (node-name origin))
                 (equal? (atomic-a atomic) label))
                (next-world world atomic (graph-struct (cdr (graph-struct-l graph))) (cons destination lst))
                (next-world world atomic (graph-struct (cdr (graph-struct-l graph))) lst)))))

(define (graph-vertices g)
  (match g
    [(graph-struct l) (remove-duplicates (apply append (map (lambda (e) (list (node-name (edge-origin e)) (node-name (edge-destination e)))) l)))]))

(define (sublist l1 l2)
  (if (null? l1)
      #t
      (and (member (car l1) l2) (sublist (cdr l1) l2))))

(define (next-world* world pdl graph)
  (match pdl
    [(? atomic? a)
     (let ([new (next-world world pdl graph)])
           new)]
    [(non-deterministic-choice a b)
     (let ([new-a (next-world* world a graph)]
           [new-b (next-world* world b graph)])
                   (append new-a new-b))]
           
    [(seq a b)
     (let ([new-a (next-world* world a graph)]
           [new-f (lambda (world) (next-world* world b graph))])
       (apply append (map new-f new-a)))]
    [(repetition a) (let rep ([l (list world)]
                              [world world])
                      (let ([new-worlds (next-world* world a graph)])
                        (if (sublist new-worlds l)
                            l
                            (apply append (map (lambda (w) (rep (cons w (append new-worlds l)) w)) new-worlds)))))]))

(define program (seq
                 (atomic "a")
                 (seq
                  (non-deterministic-choice (atomic "a") (atomic "a"))
                  (atomic "b"))))


(define (evaluate pdl graph world)
  (sublist (graph-vertices graph) (remove-duplicates (next-world* world pdl graph))))




   
;;(-> pdl graph world (list) bool)
#|(define (evaluate-front pdl graph world)
  (null? (evaluate pdl graph world)))
|#
;;(-> pdl graph world (list) (list worlds))
#|
(define (evaluate pdl graph world [visited '()])
  (match pdl
    [(? atomic? a) (]
    [(non-deterministic-choice a b) (and
                                     (evaluate a graph world visited)
                                     (evalueate b graph world visited))]
    [(seq a b) (]
|#


