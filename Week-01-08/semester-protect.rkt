
;Given Code

(require 2htdp/image)
(require 2htdp/universe)
;constants
(define W 800) ; width of the scene
(define H 600) ; height of the scene
(define GROUND 560) ; leaves stop falling at this y
(define SIZE 40) ; a new leaf has size (+ 10 (random SIZE))
(define BACKGROUND
(place-image (rectangle W (- H GROUND) "solid" "darkolivegreen")
(/ W 2) (+ GROUND (/ (- H GROUND) 2))
(rectangle W H "solid" "lightblue")))


(define-struct leaf (size color pos))
; A leaf is a structure:
; (make-leaf Number String Posn)
; interpretation: A leaf's size, color and position.
; size is a Number and represents the size of the leaf.
; color is a String and represents the color of the leaf.
; pos is a Posn and represents the location of the leaf.

;constructors
(define L1 (make-leaf 16 "red"    (make-posn 150 80)))
(define L2 (make-leaf 30 "orange" (make-posn 300 150)))
(define L3 (make-leaf 50 "yellow" (make-posn 450 558)))
(define L4 (make-leaf 24 "brown"  (make-posn 600 GROUND)))

;selector
(leaf-size  L1) ;number 16

;predicate
(leaf? L1) ;#true

; A LOL is a list of leaves.
(define LOL (list L1 L2 L3 L4))


; Contract: drawLeaf : leaf → Image
; Purpose: Draws one leaf as a solid ellipse using the leaf's size and color.

; Examples:
(check-expect(drawLeaf L1)(ellipse 32 16 "solid" "red"))
(check-expect(drawLeaf L2)(ellipse 60 30 "solid" "orange"))

; Template:
; 
; (define (drawLeaf l)
;   (... (leaf-size l)(leaf-color l)))


; Function:
(define (drawLeaf l) ; Define a function that takes one leaf.
  (ellipse
   (* 2 (leaf-size l)) ; Set the width to two times the leaf size.
   (leaf-size l) ; Set the height to the leaf size.
   "solid" ; Draw a solid ellipse.
   (leaf-color l))) ; Use the leaf's color.



; Contract: drawLeaves : LOL → Image
; Purpose: Draws all leaves on the background at their respective positions.

; Examples:
(check-expect (drawLeaves empty) BACKGROUND) ;when list comes empty ,function should give BACKGROUND
(check-expect (drawLeaves (list L1)) (place-image (drawLeaf L1) 150 80 BACKGROUND))

; Template:
; 
; (define (drawLeaves lol)
;   (cond
;     [(empty? lol) ...]
;     [else (... (first lol) (rest lol))]))


; Function:
(define (drawLeaves lol) ; Define a function that takes a list of leaves.
  (cond
    [(empty? lol) BACKGROUND] ; If the list is empty, return the background.
    [else
     (place-image
      (drawLeaf (first lol)) ; Draw the first leaf.
      (posn-x (leaf-pos (first lol))) ; Get the x position of the first leaf.
      (posn-y (leaf-pos (first lol))) ; Get the y position of the first leaf.
      (drawLeaves (rest lol)))])) ; Draw the remaining leaves.




;contract : countOnGround : LOL ---> Number
;purpose: to count the leaves whose y is equal to ground

;examples
(check-expect (countOnGround (list L1 L4 L4)) 2)

;templates
; (define (countOnGround alist2)
;   (cond
;     ((empty? alist2) ...)
;     ((= (posn-y (leaf-pos(first alist2))) GROUND) ...)
;     (else (... (countOnGround (rest alist2))))))



;function
(define (countOnGround alist2) ; Define a function that takes a list of leaves.
  (cond
    [(empty? alist2) 0] ; If the list is empty, return 0.
    [(= (posn-y (leaf-pos (first alist2))) GROUND)
     (+ 1 (countOnGround (rest alist2)))] ; If the leaf is on the ground, add 1 and check the rest.
    [else
     (countOnGround (rest alist2))])) ; Otherwise, check the rest of the list.
(countOnGround LOL) ; Count the leaves on the ground in LOL.



; Contract: draw-f : LOL --> Image
; Purpose: to write the number of the leaves on the ground at top-left with font size 28

; Examples

(check-expect (draw-f LOL)(overlay/align "left" "top"(text "Leaves on the ground: 1" 28 "black")
    (drawLeaves LOL)))

; Templates
; 
; (define (draw-f alist3)
; 
;   (... (countOnGround alist3) ...
; 
;        (drawLeaves alist3)))


; Function

(define (draw-f alist3) ; Define a function that takes a list of leaves.
  (overlay/align "left" "top" ; Place the text at the top-left corner.
   (text
    (string-append "Leaves on the ground: "
                   (number->string (countOnGround alist3))) ; Convert the count to text.
    28 "black") ; Set the text size and color.
   (drawLeaves alist3))) ; Draw all leaves.

(draw-f LOL) ; Call the function with LOL.


;contract: fallSpeed : number --> number
;purpose: to measure how many pixels a leaf falls in one tick with its size

;example

(check-expect (fallSpeed 25) 3)
(check-expect (fallSpeed 48) 2)
(check-expect (fallSpeed 15) 4)

;template
; (define (fallSpeed size)
;   (cond
;     ((< size 20) ...)
;     ((and (>= size 20) (< size 40)) ...)
;     ((>= size 40) ...)))
; 


;function

(define (fallSpeed size) ; Define a function that takes the leaf size.
  (cond
    ((< size 20) 4) ; If size is less than 20, return 4.
    ((and (>= size 20) (< size 40)) 3) ; If size is between 20 and 40, return 3.
    ((>= size 40) 2))) ; If size is 40 or more, return 2.

(fallSpeed 58) ; Call the function with size 58.




;contract: moveLeaf : leaf-->leaf
;purpose: to move a leaf down with its speed and stopping when it reaches the ground

;examples

(check-expect (moveLeaf L1) (make-leaf 16 "red" (make-posn 150 84)))
(check-expect (moveLeaf L3) (make-leaf 50 "yellow" (make-posn 450 GROUND)))

;template
; (define (moveLeaf aleaf)
;   (cond
;     ((>= (+ ( posn-y (leaf-pos aleaf)) (fallSpeed (leaf-size aleaf))) GROUND) ...)
;     (else ...)))
; 



;function

(define (moveLeaf aleaf) ; Define a function that takes one leaf.
  (cond
    ((>= (+ (posn-y (leaf-pos aleaf))
            (fallSpeed (leaf-size aleaf))) GROUND)
     ; If the leaf reaches the ground, keep it on the ground.
     (make-leaf
      (leaf-size aleaf) ; Keep the same size.
      (leaf-color aleaf) ; Keep the same color.
      (make-posn
       (posn-x (leaf-pos aleaf)) ; Keep the same x position.
       GROUND))) ; Set the y position to the ground.

    (else
     (make-leaf
      (leaf-size aleaf) ; Keep the same size.
      (leaf-color aleaf) ; Keep the same color.
      (make-posn
       (posn-x (leaf-pos aleaf)) ; Keep the same x position.
       (+ (posn-y (leaf-pos aleaf))
          (fallSpeed (leaf-size aleaf))))))) ; Move the leaf down.
)

(moveLeaf L4) ; Move leaf L4.




;contract: tick-f: LOL--> LOL
;purpose: to move every leaf in the list

;examples
(check-expect (tick-f (list L1)) (list (make-leaf 16 "red" (make-posn 150 84))))
(check-expect (tick-f empty) empty)
 
 

;template
; (define (tick-f lol)
; (cond
;  ((empty? lol) ...)
;  (else (cons ... (first lol) ... (tick-f (rest lol))))))



;function

(define (tick-f lol) ; Define a function that takes a list of leaves.
  (cond
    ((empty? lol) '()) ; If the list is empty, return an empty list.
    (else
     (cons
      (moveLeaf (first lol)) ; Move the first leaf.
      (tick-f (rest lol)))))) ; Move the remaining leaves.

(tick-f LOL) ; Apply the function to LOL.




;contract: key-f : LOL KeyEvent --> LOL
;purpose : to update the list of leaves depending on key event

;examples

(check-expect (key-f LOL "c") '())
(check-expect (key-f LOL "m") (cons
 (make-leaf 16 "red" (make-posn 150 80))
 (cons (make-leaf 30 "orange" (make-posn 300 150)) (cons (make-leaf 50 "yellow" (make-posn 450 558)) (cons (make-leaf 24 "brown" (make-posn 600 560)) '())))))


(check-expect (length (key-f (list L1 L2) "r")) 3)
(check-expect (length (key-f (list L1 L2) "y")) 3)
(check-random (key-f empty "r")
              (list (make-leaf (+ 10 (random SIZE)) "red"
                               (make-posn (random W) 0))))

;template
; (define (key-f alol ke)
;   (cond
;     ((key=? ke "r") ...)
;     ((key=? ke "o") ...)
;     ((key=? ke "y") ...)
;     ((key=? ke "b") ...)
;     ((key=? ke "c") ...)
;     (else ...)))
; 
; 

;function

(define (key-f alol ke) ; Define a function that takes a list and a key.
  (cond
    ((key=? ke "r") ; If the key is r, create a red leaf.
     (cons (make-leaf (+ 10 (random SIZE)) "red"
                      (make-posn (random W) 0)) alol))

    ((key=? ke "o") ; If the key is o, create an orange leaf.
     (cons (make-leaf (+ 10 (random SIZE)) "orange"
                      (make-posn (random W) 0)) alol))

    ((key=? ke "y") ; If the key is y, create a yellow leaf.
     (cons (make-leaf (+ 10 (random SIZE)) "yellow"
                      (make-posn (random W) 0)) alol))

    ((key=? ke "b") ; If the key is b, create a brown leaf.
     (cons (make-leaf (+ 10 (random SIZE)) "brown"
                      (make-posn (random W) 0)) alol))

    ((key=? ke "c") '()) ; If the key is c, return an empty list.

    (else alol))) ; For other keys, keep the same list.

(key-f LOL "m") ; Call the function with LOL and the key m.

 


;contract: isClicked? leaf number number --> boolean
;purpose : to check whether a given point is on a leaf

;examples
(check-expect (isClicked? L1 160 85) #true)
(check-expect (isClicked? L1 170 80) #false)

;template
; (define (isClicked? bleaf x y)
;   (cond
;     ((and (<= (abs (- x (posn-x (leaf-pos bleaf)))) (leaf-size bleaf)) (<= (abs (- y (posn-y (leaf-pos bleaf)))) (/ (leaf-size bleaf) 2 ))) ...)
;     (else ...)))
; 

;function

(define (isClicked? bleaf x y) ; Define a function that takes a leaf and click coordinates.
  (cond
    ((and
      (<= (abs (- x (posn-x (leaf-pos bleaf))))
          (leaf-size bleaf))
      ; Check if the x distance is within the leaf width.

      (<= (abs (- y (posn-y (leaf-pos bleaf))))
          (/ (leaf-size bleaf) 2)))
      ; Check if the y distance is within the leaf height.

     #true) ; If both conditions are true, return true.

    (else #false))) ; Otherwise, return false.

(isClicked? L1 160 85) ; Check if the click is on L1.
(isClicked? L2 170 80) ; Check if the click is on L2.




;contract: mouse-f : LOL number number MouseEvent --> LOL
;purpose: to remove the first leaf under the mouse when the mouse button is pressed otherwise keep the list unchanged

;examples

(check-expect (mouse-f (list L1 L2) 300 150 "button-down") (list L1))
(check-expect (mouse-f (list L1 L2) 0 0 "button-down") (list L1 L2))


;template

; (define (mouse-f aLOL x y me)
;   (cond
;     ((empty? aLOL) ...)
;     ((not (mouse=? me "button-down")) ...)
;     ((isClicked? (first aLOL) x y)...)
;     (else ...)))
;    




;function


(define (mouse-f aLOL x y me) ; Define a function that takes a list, mouse position and mouse event.
  (cond
    ((empty? aLOL) '()) ; If the list is empty, return an empty list.

    ((not (mouse=? me "button-down")) aLOL)
    ; If the mouse button is not pressed, return the same list.

    ((isClicked? (first aLOL) x y) (rest aLOL))
    ; If the first leaf is clicked, remove it from the list.

    (else
     (cons (first aLOL) (mouse-f (rest aLOL) x y me)))))
     ; Otherwise, keep the first leaf and check the remaining leaves.

(mouse-f LOL 0 0 "button-down") ; Call the function with LOL.


; Contract: main : LOL -> LOL

(define (main lol) ; Define the main function with a list of leaves.
  (big-bang lol ; Start the program with the given list.
    (on-tick tick-f) ; Update the leaves at each tick.
    (on-key key-f) ; Handle keyboard events.
    (on-mouse mouse-f) ; Handle mouse events.
    (to-draw draw-f))) ; Draw the leaves and the text.

(main (list L1 L2 L3 L4)) ; Start the program with four leaves.
