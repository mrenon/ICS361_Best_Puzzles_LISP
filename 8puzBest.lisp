;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; Name: Mauricio Renon                                   Date: 9/15/14
;;; Course: ICS361        Assignment #3 8 Puzzle
;;; File: 8puzBest.lisp

; Source: http://lazytoad.com/lti/ai/hw1-3.html

;;; The 8 puzzle

;;; State is a list
;;; 
;;; (1 2 3 4 0 8 7 6 5)

;;; Best-First Search with a Depth Limit
;;; Example Usage: 
;;; CL_USER> (best *start* 100000)

;;; Check if start state is Solvable for the goal
;;; Example Usage:
;;; CL_USER> (solvable '(1 2 3 4 5 6 7 8 0))

(defvar *start* '(1 2 3 ; the start state of the grid
		  4 0 8
		  7 6 5))

(defvar *goal* '(1 2 3 ; the goal state of the grid
		 4 5 6   
		 7 8 0))

;;; Define adjacencies
(defvar *adj*
    '((0 1 3)
      (1 0 4 2)
      (2 1 5) 
      (3 0 4 6)
      (4 1 3 5 7)
      (5 2 4 8)
      (6 3 7)
      (7 4 6 8)
      (8 5 7)))

(defvar iteration 1)


(defun goalp (state) ; checks to see if the state os the goal
    (equal state  *goal*))

(defun transpose (state i j)
    (transpose1 state j i (nth i state) (nth j state)))

(defun transpose1 (state i j ival jval)
    (cond
	((null state) nil)
	((zerop i)
	    (cons ival
		(transpose1 (cdr state) (- i 1) (- j 1) ival jval)))
	((zerop j)
	    (cons jval
		(transpose1 (cdr state) (- i 1) (- j 1) ival jval)))
	(t
	    (cons (car state)
		(transpose1 (cdr state) (- i 1) (- j 1) ival jval)))))

(defun loc-of (num state)
    (cond
	((null state) 0)
	((eq (car state) num) 0)
	((+ 1 (loc-of num (cdr state))))))

(defun space-at (state)
    (loc-of 0 state))

(defun new-states (state)
    (let ((zloc (space-at state)))
	(mapcar #'(lambda (toloc)
		      (transpose state zloc toloc))
	    (cdr (assoc zloc *adj*)))))


;;; The value of a state is 3/4 based in how similar that state
;;; is to the goal state, and 1/4 based on whether tiles adjacent
;;; in the goal state are also adjacent in the current state.

(defun heur-value (state)
    (+
	(* 3 (similarity state *goal*))
	(adj-value state *goal*)))

;;; similarity is the number of tiles in the same position in two states
(defun similarity (s1 s2)
    (cond
	((or (null s1) (null s2)) 0)
	((equal (car s1) (car s2)) 
	    (+ 1 (similarity (cdr s1) (cdr s2))))
	((similarity (cdr s1) (cdr s2)))))

(defun adj-num (num state)
    (mapcar
	#'(lambda (n) (nth n state))
	(cdr (assoc (loc-of num state) *adj*))))

(defun number-common (l1 l2)
    (cond
	((null l1) 0)
	((null l2) 0)
	((member (car l1) l2)
	    (+ 1 (number-common (cdr l1) l2)))
	((number-common (cdr l1) l2))))

;;; adj-value is the number of tile adjacencies common between thw
;;; two states
(defun adj-value (s1 s2)
    (apply #'+ 
	(mapcar
	    #'(lambda (num)
		  (number-common (adj-num num s1) (adj-num num s2)))
	    '(1 2 3 4 5 6 7 8))))








;;; Breadth first search with state limit

;;; A node is a list of (hval state parent gradparent ...)
(defun hval-of (node) (car node))
(defun state-of (node) (cadr node))
(defun path-of (node) (cdr node))
(defun depth-of (node) (length (cddr node)))

(defvar *visited* nil)
(defvar *heur-mult* 2)

(defun best (state limit) ;best first search given a state and depth
    (let ((nodes 0) ;setting node to 0
	     (expanded 0) ;setting expanded to 0
	     (branches 0) ; setting branches to 0
	     (limit limit) ;setting limit to 0
	     (open (list (list (heur-value state) state))))

	(setf *visited* nil) ; sets the visited states to nil
	
	(loop
	    (cond ((null open) ; checks if the open list is null
		      (print (list 'nodes nodes expanded branches)) 
		      (return (list 'no 'solution 'found))))
	    
	    (incf nodes) ; increments nodes
	    
	    (cond ((goalp (state-of (car open))) 
		      (print (list 'length 'of 'soln (depth-of (car open))))
		                (terpri)
              			(princ "length of open = ") (princ (list-length open))
              			(terpri)
              			(princ "length of closed = ") (princ (list-length *visited*))
              			(terpri)
                      (print "Goal found. Solution path =")
		      (return (path-of (car open)))
                     (print (list 'length 'of 'opennode (depth-of (open))))))

            (format t "~&ITERATION #~A" iteration)
            (setq iteration (1+ iteration))
            
            (print "open=") (princ (car *visited*))
      

	    (cond ((> nodes limit)
		      (print (list 'nodes nodes expanded branches))
		      (return (list 'closest 'was (car open)))))
	    
	    (let ((children (new-states (state-of (car open)))))
		(incf expanded)
		(setf branches (+ (length children) branches))
		(setf open (combine-queue children (car open) (cdr open)))))))

;;; This function takes the new children of the current node, the
;;; current node, and the rest of the queue and builds new nodes for
;;; those child states that have not been visited.

;;; Note that the SORT is overkill, since we only need the best
;;; state in front, but the program is shorter if we use sort

;;; Note: we use (*HEUR-MULT* X HEUR - DEPTH) as the value of a node...
;;; this makes for for shorter (but not necessarily optimal) paths.

(defun combine-queue (new-states node queue)
    (push (state-of node) *visited*)
    (dolist (state new-states)
	(if (not (member state *visited* :test #'equal))
	    (push (cons (- (* *heur-mult* (heur-value state)) (depth-of node))
		      (cons state (cdr node)))
		queue)))
    (sort queue #'> :key #'car))


;; Given a start state, this function checks if the goal state '(1 2 3 4 5 6 7 8 0)
;;; can be reached.
(defun solvable (start)
  (print "start state: ") (princ start)
  (terpri)
  (print "goal state: ") (princ *goal*)
  (terpri)
  (print "can this start state reach this goal state: ")
        (setq temp start)
  (setf inversion 0)
        (setf temp (remove 0 temp))
        (setf i 0)
  (loop while (< i (length temp)) do 
               (setf j (1+ i))
    (loop while (< j (length temp)) do 
                      (if (> (nth i temp) (nth j temp)) (incf inversion))
                      (incf j))
                (incf i)
                (setf j 1))
  (if (oddp inversion) '(false) '(true)))