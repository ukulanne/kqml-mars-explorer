;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anne Summers           ;;
;; ukulanne@gmail.com     ;;
;; Mars Explorer          ;;
;; November 30, 2003      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; KQML Mars Explorer
;; Copyright (C) (2003-2018)  Anne Summers

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; More kqml info can be found at:
;; https://www.csee.umbc.edu/csee/research/kqml/

;; Originally developed for Dr Scheme
;; It can be run on Dr Racket with Language set to Pretty Big.

(define (display-msg obj) (display obj)(newline))

;; Frame
(define frame (instantiate frame% ("Mars Explorer") (width 500) (height 500)))
;; Make the drawing area
(define canvas (instantiate canvas% (frame)))
;; Get the canvas's drawing context
(define dc (send canvas get-dc))

;; Make some pens and brushes
(define no-pen (instantiate pen% ("BLACK" 1 'transparent)))
(define no-brush (instantiate brush% ("BLACK" 'transparent)))
(define blue-brush (instantiate brush% ("BLUE" 'solid)))
(define yellow-brush (instantiate brush% ("YELLOW" 'solid)))
(define red-pen (instantiate pen% ("RED" 2 'solid)))
(define yellow-pen (instantiate pen% ("YELLOW" 2 'solid)))

;; Draw the different objects required
(define (draw-craft dc x y amount)
  (send dc set-pen red-pen) 
  (send dc set-brush blue-brush) 
  (send dc draw-rectangle x y 40 40)) 

;; A rover and a explorer are the same although
;; the latter is painted completely in blue
;; (out is red if we have full amount of mineral)
;; and the former is outliined in yellow

(define (draw-explorer dc x y max amount)
  (send dc set-pen 
        (cond 
          ((zero? max) yellow-pen) 
          ((zero? amount) red-pen)
          (else no-pen)))
  (send dc set-brush blue-brush) 
  (send dc draw-ellipse x y 10 10)) 
  
(define (draw-obstacle dc x y)
  (send dc set-pen red-pen) 
  (send dc set-brush blue-brush) 
  (send dc draw-rectangle x y 10 10)) 

(define (draw-mineral dc x y id)
  (send dc set-pen no-pen)
  (send dc set-brush yellow-brush)
  (send dc draw-ellipse x y 8 8))

;; Object properties
(define craft #f) ;; list '(x y amount)
(define explorers #f) ;;list of list of the form '(x y full max amount)
;;(define rovers #f) ;; list of lists of the form '(x y flag)
(define minerals #f) ;;list of list of the form '(x y amount)
(define obstacles '()) ;; list of list of the form '(x y)
(define msg '()) ;; list of message of the form '(s-expression)
(define kqml-flag #f)

;; Each kqml message is an expression to be evaluated.
;; Basically, we fetch it from the msg list and evaluate them
;; No further parsing is required of the message; 
;; 'tis treated as a normal Scheme language expression
;; This functions returns different result depending on the arguments received

(define (kqml type type-arg . args)
  (cond
    ;; We found a mineral. Go get 'em!
    ((and (equal? type 'message) (equal? type-arg "mineral-found"))
     (list "goto" (second args) (third args)))
    ;; The mineral mound non e mai piu.
    ((and (equal? type 'message) (equal? type-arg "mineral-exhausted")) "exhausted")
    ;; Return to the main starship
    ((and (equal? type 'message) (equal? type-arg "mission-completed")) (list "return"))
    (else #f)))

(define (main start flag type)
  
  (define (first)
    (set! craft '(0 0 55))
    (set! explorers '((0 20 0 0) (10 20 15 15) (20 20 45 45) (30 20 10 10)))
    (set! obstacles '((103 16) (401 200) (210 150)(250 301)
                      (100 15) (205 300) (500 100)(100 204)
                      (100 115) (400 100) (20 200)(400 200)
                      ))
    (set! minerals '((100 230 35) (200 160 25) (300 100 30)
                     (110 330 15) (90 105 15) (0 109 45)
                     (410 430 15) (190 305 15) (499 109 35)
                     (450 230 15) (290 305 15) (199 49 55)
                     (410 430 15) (10 405 5) (499 10 35)
                     (210 305 15) (190 305 15) (499 110 35)
                     (325 205 10) (405 0 15) (200 200 5)
                     (480 0 15) (390 425 15) (400 109 15))))
  
  
  
  ;; Get new coordinates for explorer. 
  ;; Usually we try to advance linealy five points in Y
  ;; However if the special flag is given we change also X
  ;; we also need to check if those coords are inside the canvas
  ;; if not we need to recalculate
  ;; If the explorer is full of mineral then we return to 
  ;; the starship at 0, 0
  
  (define (get-new-coords craft explorer)
    (define (new-coord-for-target-coords coords target-coords)
      (let* ((x (car coords))
            (y (cadr coords))
            (xx (car target-coords))
            (yy (cadr target-coords))
            (sign-x (if (> x xx) - +))
            (sign-y (if (> y yy) - +))
            (final-x #f)
            (final-y #f))
        (set! final-x (apply sign-x (list x 5)))
        (set! final-y (apply sign-y (list y 5)))
        (list final-x final-y)))
    
    (define (get-coords-from-msg coords)
      (call-with-current-continuation
       (lambda (break)
         (if (or (null? msg) (not kqml-flag))
             (break coords))
      (let ((msg (eval (car msg))))
      (cond 
        ((equal? (car msg) "mission-completed") (list 0 0))
        ((equal? (car msg) "goto") (new-coord-for-target-coords coords (cdr msg)))
        (else coords))))))
      
     
    (let ((x (car explorer))
          (y (cadr explorer))
          (xx #f)
          (yy #f)
          (craft-full (<= (third craft) 0))
          (full (and (<= (fourth explorer) 0) (not(zero? (third explorer))))))
  
      ;; It explorer is full then we return to the craft
      ;; moving from x,y to the origin in steps of 5 in each axis
      ;; we reach 0 in either of the axis thus mantaining that point
      ;; until the other coordinate reaches 0
      
      (if (or full craft-full)
          (begin
            (set! xx (if (<= x 5) 0 (- x 5)))
            (set! yy (if (<= y 5) 0 (- y 5)))
            (list xx yy))

          ;; Normal movement is defined as an aleatory change of coordinates
          ;; either by adding 5 or 0 to x or y (or both).
          ;; Very stupid since it can basically be around the same point forever
          
          ;; Once the new coords are calculated we need to check if there is a msg in the 
          ;; queu capable of changing the coordinates 
          
          (let* ((sign (if (zero? (random 2)) + -))
                 (step-x (if (zero? (random 2)) 0 5))
                 (step-y (if (zero? (random 2)) 0 5))
                 (x (apply sign (list x step-x)))
                 (y (apply sign (list y step-y))))
            (get-coords-from-msg 
             (list
              (cond 
                ((<= x 0) 0)
                ((>= x 500) 500)
                (else x))
              (cond 
                ((<= y 0) 0)
                ((>= y 500) 500)
                (else y)))))
    )))
            
  ;; Check if we are now inside our main starship
  (define (craft? coords)(and (zero? (car coords)) (zero? (cadr coords))))
  
  ;; Check if this explorer is a rover, i.e. cannot carry minerals to the craft
  (define (rover? explorer)(and (zero? (third explorer)) (zero? fourth explorer)))
  
  ;;Is there an obstacle in the new coordinates?
  (define (obstacle? obstacles coords)(member coords obstacles))
  
  ;; Check if X,Y are present inside list if so return this element
  (define (mineral? ls coords)
    (define (helper ls)
      (cond 
        ((null? ls) #f)
        ((equal? coords (list (caar ls) (cadar ls)))(append (list (car coords) (cadr coords)) (cddar ls)))
        (else (helper (cdr ls)))))
    (helper ls))
     
  (define (leave-mineral-on-craft kraft explorer)
    (if (zero? (fourth explorer))
        (begin
          (set! craft (list (car kraft)(cadr kraft) (- (third kraft) (third explorer))))
          (list (car explorer) (cadr explorer) (third explorer) (third explorer)))
        explorer))
          
  (define (grab-mineral explorer my-minerals coords)
    ;; Change the new state of the mineral if amount is zero we get rid of the mound.
    
    (define (change-minerals minerals the-mineral new-mineral ms)
      (cond 
        ((null? minerals) (reverse ms))
        ((and (equal? the-mineral (car minerals)) (> (caddar minerals) 0))
         (change-minerals (cdr minerals) the-mineral new-mineral (cons new-mineral ms)))
        ((and (equal? the-mineral (car minerals)) (zero? (caddar minerals) ))
         (change-minerals (cdr minerals) the-mineral new-mineral ms))
        (else (change-minerals (cdr minerals) the-mineral new-mineral (cons (car minerals) ms)))))
    
    ;; We move to the place of a mineral set so we take as much as we can
    (let* ((the-mineral (mineral? my-minerals coords)) ;; we need to know which is the mineral set found!
            (x-max (third explorer))
            (x-amount (fourth explorer)) ;;amount has the free space 
            (remain (- (third the-mineral) x-amount)) ;; what remains on the mount need to be checked 
            (new-mineral (list (car the-mineral) (cadr the-mineral) remain)))
      (display-msg (list "mineral-remaining" the-mineral x-amount remain))
      
      ;; We substract the minerals to be loaded inside the explorer
      (set! minerals (change-minerals my-minerals the-mineral new-mineral '()))
      
      ;; We communicate that we found a mound if there is still something remaining
      (if (> remain 0)
          (begin 
            (set! msg (cons `(kqml 'message "mineral-found" 'coords ,(car coords) ,(cadr coords)) 
                          msg))  
            (if kqml-flag
                (display-msg (car msg))))
          
          ;; if the mineral from the mound is exhausted then we tell our buddies not to come here anymore
          (begin 
            (set! msg '()) 
             (if kqml-flag
                 (display-msg `(kqml 'message "mineral-exhausted" 'coords ,(car coords) ,(cadr coords))))))
      
      ;; The new explorer data    
      (list (car explorer) (cadr explorer) (third explorer) 0)))
  
  ;; Get the new explorer movement
  ;; in any case it should return the new state of the explorer
  ;; i.e. positions and how many mineral already collected
  ;; Grab-mineral also decide the state of the mineral sets
  
   (define (fetch-explorer-movement explorer . flag)
     (let ((coords (get-new-coords craft explorer)))
       (cond 
         ((craft? coords)(leave-mineral-on-craft craft explorer));; check if we want to deliver mineral
         ((obstacle? obstacles coords) (fetch-explorer-movement explorer #t))
         ((mineral? minerals coords) (grab-mineral explorer minerals coords))
         (else (append coords (cddr explorer))))))
  
  ;;Grab the new action for each explorer 
  (define (next) 
    (set! explorers (map fetch-explorer-movement explorers)))
  ;; (display-msg (cons "explorer-locations " explorers))
  (if flag
      (begin 
        (set! kqml-flag
              (if (or (equal? type 'kqml) (equal? type 'kqml-rover))#t #f))
        (first) 
        (if (not (equal? type 'kqml-rover)) (set! explorers (cdr explorers)))
        (send frame show #t))
        
      (next)) 
  
  ;; We clear the canvas to get rid of old objects
  
  (send dc clear)
  
   ;; After calculations draw objects  
  
  (apply draw-craft (cons dc craft))
  (for-each (lambda (x)(apply draw-explorer (cons dc x))) explorers)
  (for-each (lambda (m)(apply draw-mineral (cons dc m))) minerals)
  (for-each (lambda (r)(apply draw-obstacle (cons dc r))) obstacles)
  
  ;; We sleep the action for a fraction of time so we can see what's happening on the canvas!
  
   (sleep/yield 0.01)
  
  ;;(display explorers)
  
  ;; We check how many iterations we have made 
  ;; or if the main starship is full.
  
  (if (or (zero? start) (<= (third craft) 0)) ;; needs to be checked to see all crafts return :)
      (display-msg "(end)")
      (main (- start 1) #f type)))

;; With rovers and messages
;;(main 4000 #t 'kqml-rover)

;; With kqml messages but no rovers
;;(main 4000 #t 'kqml)

;; No interaction between them Everything on your own
;;(main 4000 #t #f)