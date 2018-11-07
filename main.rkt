#lang racket


(require "pdl.rkt")
(struct graph-struct (l) #:transparent)
(struct node (name) #:transparent)
(struct edge (origin destination label) #:transparent)


(define test-graph (graph-struct (list (edge (node "A") (node "B") "a") (edge (node "A") (node "A") "c")
                                       (edge (node "C") (node "B") "c") (edge (node "B") (node "C") "c")
                                       (edge (node "C") (node "A") "b"))))

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

(define (next-world++ world atomic graph [lst-vertices '()] [lst-arestas '()])
  (if (null? (graph-struct-l graph))
      (values lst-vertices lst-arestas) 
      (let*
          ([first-transition (car (graph-struct-l graph))]
           [origin (edge-origin first-transition)]
           [destination (node-name (edge-destination first-transition))]
           [label (edge-label first-transition)])
        (if (and
             (equal? world (node-name origin))
             (equal? (atomic-a atomic) label))
            (next-world++ world atomic (graph-struct (cdr (graph-struct-l graph))) (cons destination lst-vertices) (cons first-transition lst-arestas))
            (next-world++ world atomic (graph-struct (cdr (graph-struct-l graph))) lst-vertices lst-arestas)))))


(define (graph-vertices g)
  (match g
    [(graph-struct l) (remove-duplicates (apply append (map (lambda (e) (list (node-name (edge-origin e)) (node-name (edge-destination e)))) l)))]))

(define (graph-arestas g)
  (match g
    [(graph-struct l) l]))

(define (sublist l1 l2)
  (if (null? l1)
      #t
      (and (member (car l1) l2) (sublist (cdr l1) l2))))


;(world pdl graph . -> . (values mundos arestas))
;; proposições atomicas deve retornar (values mundos arestas)
;; dentro de cada match, todo let deve virar um let-values
;; para pegar a lista de mundos e de arestas
;; ai vai retornar as duas(talvez concatenando)
;; Esse é o roteiro final, o racket contra-ataca
;; não existe tentativa, faça ou não-faça(sua nota depende disso
;; então faça)

;; tudo é possivel, o impossivel só demora mais tempo ou é mais caro
;; exceto o problema da parada.

(define (graph-walker world pdl graph)
  (match pdl
    [(? atomic? a)
     (next-world++ world pdl graph)]
    [(non-deterministic-choice a b)
     (let-values ([(new-a new-arestas-a) (graph-walker world a graph)]
                  [(new-b new-arestas-b) (graph-walker world b graph)])
       (values (append new-a new-b) (append new-arestas-a new-arestas-b)))]
           
    [(seq a b)
     (let*-values ([(new-a new-arestas-a) (graph-walker world a graph)]
                   [(new-f) (lambda (world) (graph-walker world b graph))]
                   [(worlds-and-arestas)
                    (map
                     (lambda (world)
                       (let-values ([(l k) (new-f world)]) (list l k)))
                     new-a)]
                   [(worlds) (map car worlds-and-arestas)]
                   [(arestas) (map cadr worlds-and-arestas)])
       (values (append new-a (apply append worlds)) (append new-arestas-a (apply append arestas))))]
    [(repetition a) (let rep ([l (list world)]
                              [k (list)]
                              [world world])
                      (let-values ([(new-worlds arestas) (graph-walker world a graph)])
                        (if (and
                             (sublist new-worlds l)
                             (sublist arestas k))
                            (values l k)
                            (let* ([worlds-and-arestas (map
                              (lambda (w)
                                (let-values ([(a b) (rep
                                 (cons w (append new-worlds l)) (append arestas k) w)]) (list a b)))
                              new-worlds)]
                                  [worlds (map car worlds-and-arestas)]
                                  [arestas (map cadr worlds-and-arestas)])
                            (values (apply append worlds) (apply append arestas))))))]))

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


(define (evaluate pdl world [graph test-graph])
  (let-values ([(l k) (graph-walker world pdl graph)])
    (let ([l (cons world l)])
      (if
       (and
        (sublist (graph-vertices graph) (remove-duplicates l)) ;andei por todos os vertices?
        (not (null? (next-world* world pdl graph))) ;deu errado o pdl???
        (sublist (graph-arestas graph) (remove-duplicates k))) ; andei por todas as arestas?
       #t
       (last l)))))

