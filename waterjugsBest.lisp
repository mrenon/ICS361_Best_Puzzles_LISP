;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; Name: Mauricio Renon                                   Date: 9/15/14
;;; Course: ICS361        Assignment #3 waterjug problem
;;; File: waterjugsBest.lisp


;;; Source: LazyToad.com/lti/ai/hw1-1.html

(load "bestSearch.lisp") ;loads the best search algorithm


;;; Global variable of moves
(setq *moves*
      '(fill-first fill-second pour-first-second
        pour-second-first empty-first empty-second))

; This returns the quantity in the first jug
; it takes the state and gives you the first parameter in the list
(defun first-jug (state) (car state))

; This returns the quantity in the second jug
; it tkaes the state 
(defun second-jug (state) (cadr state))

; This returns the state of the two jugs
(defun mk-state (f s) (list f s))

; This checks whether a given state is a goal
; goal is to get 4 in the second jug
(defun goalp (state)
    (eq (first-jug state) 2))

; This returns all possible states that can be derived
; from a given state
(defun new-states (state)
    (remove-null
	(list
	    (fill-first state)
	    (fill-second state)
	    (pour-first-second state)
	    (pour-second-first state)
	    (empty-first state)
	    (empty-second state))))

; This removes all the null states
(defun remove-null (x)
    (cond
	((null x) nil)
	((null (car x)) (remove-null (cdr x)))
	((cons (car x) (remove-null (cdr x))))))

; This returns the state when the first jug is filled (first jug can hold 3)
(defun fill-first (state)
    (cond
	((< (first-jug state) 3) (mk-state 3 (second-jug state)))))) ; Checks to see if the first jug state is less than 3, if it is, change the state to 3.

; This returns the staet when the second just is filled (second can hold 5)
(defun fill-second (state)
    (cond
	((< (second-jug state) 5) (mk-state (first-jug state) 5)))) ; Checks to see if the second jug state is less than 5, if it is, change the state to 5.

; This returns the staet when quantity in first
; is poured into the second jug
(defun pour-first-second (state)
    (let (   (f (first-jug state)) ;set f to the first jug
	     (s (second-jug state))) ;set s to second jug
	(cond
	    ((zerop f) nil)		; Cant pour nothing
	    ((= s 5) nil)		; Second full
	    ((<= (+ f s) 5)		; Empty first into second
		(mk-state 0 (+ f s)))
	    (t				; Fill second from first
		(mk-state (- (+ f s) 5) 5)))))

;returns the steate when second jug is poured to first
(defun pour-second-first (state)
    (let (   (f (first-jug state))
	     (s (second-jug state)))
	(cond
	    ((zerop s) nil)		; Cant pour nothing
	    ((= f 3) nil)		; First full	    
	    ((<= (+ f s) 3)		; Empty second into first
		(mk-state (+ f s) 0))	    
	    (t				; Fill first from second
		(mk-state 3 (- (+ f s) 3))))))

; Checks to see if the first jug has anything greater than 0, if it is,
; change the first jug to zero to empty it
(defun empty-first (state)
    (cond
	((> (first-jug state) 0) (mk-state 0 (second-jug state)))))

; Checks to see if the second jug has anything greater than 0, if it is,
; change the first jug to zero to empty it
(defun empty-second (state)
    (cond
	((> (second-jug state) 0) (mk-state (first-jug state) 0))))
