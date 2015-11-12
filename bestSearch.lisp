;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; Name: Mauricio Renon                                   Date: 9/15/14
;;; Course: ICS361        Assignment #3 best search
;;; File: bestSearch.lisp

;;; source: http://www.cs.unm.edu/~luger/ai-final/code/LISP.best.html

;;;; Author: George F. Luger and William A. Stubblefield

;;; This is one of the example programs from the textbook:
;;;
;;; Artificial Intelligence: 
;;; Structures and strategies for complex problem solving
;;;
;;; by George F. Luger and William A. Stubblefield
;;;
;;; Corrections by Christopher E. Davis (chris2d@cs.unm.edu)
;;; 
;;;
;;; These programs are copyrighted by Benjamin/Cummings Publishers.
;;;
;;; We offer them for use, free of charge, for educational purposes only.
;;;
;;; Disclaimer: These programs are provided with no warranty whatsoever as to
;;; their correctness, reliability, or any other property.  We have written 
;;; them for specific educational purposes, and have made no effort
;;; to produce commercial quality computer programs.  Please do not expect 
;;; more of them then we have intended.
;;;
;;; This code has been tested with CMU Common Lisp CVS release-19a
;;; 19a-release-20040728 and appears to function as intended.

;;; this file contains the best-first search algorithm from chapter 7.

;;; This .lisp file is used together with the FWGCbest.lisp and waterjugsBest.lisp
;;; Once these have been defined, evaluate:
;;;  For fwgc:
;;;  (heuristic '(e e e e))
;;;  (setq *moves* '(farmer-takes-self farmer-takes-wolf farmer-takes-goat farmer-takes-cabbage))
;;;  (run-best '(e e e e) '(w w w w))

;;;  For water jug:
;;;  (heuristic '(0 0))
;;;  (run-best '(0 0) '(4 0))

(defvar iteration 1) ;variable to keep track of the number of iterations
                     ;this variable doesn't reset if you do "run-best" on FWGC and waterjugs right after another 

(defvar *goal* nil) ;create the goal variable set to null
(defvar *open* nil) ;create the open list 
(defvar *closed* nil) ;create a closed list
(defvar *moves* nil) ;create the moves that can be used by the problem


;;; insert-by-weight will add new child states to an ordered list of 
;;; states-to-try.  
(defun insert-by-weight (children sorted-list)
  (cond ((null children) sorted-list)
        (t (insert (car children) ;get the first element in the children list and insert throught the insert function
           (insert-by-weight (cdr children) sorted-list)))))

(defun insert (item sorted-list)
  (cond ((null sorted-list) (list item))
        ((< (get-weight item) (get-weight (car sorted-list)))
         (cons item sorted-list))
        (t (cons (car sorted-list) (insert item (cdr sorted-list))))))


;;; run-best is a simple top-level "calling" function to run best-first-search
(defun run-best (start goal)
  (setq *goal* goal)
  (setq *open* (list (build-record start nil 0 (heuristic start))))
  (setq *closed* nil)
  (best-first))

;;; These functions handle the creation and access of (state parent) 
;;; pairs.
(defun build-record (state parent depth weight) 
  (list state parent depth weight))

(defun get-state (state-tuple) (nth 0 state-tuple)) ;gets the 0th tuple

(defun get-parent (state-tuple) (nth 1 state-tuple)) ;gets the 1st tuple

(defun get-depth (state-tuple) (nth 2 state-tuple))

(defun get-weight (state-tuple) (nth 3 state-tuple))

(defun retrieve-by-state (state list)
  (cond ((null list) nil)
        ((equal state (get-state (car list))) (car list))
        (t (retrieve-by-state state (cdr list)))))


;; best-first defines the actual best-first search algorithm
;;; it uses "global" open and closed lists.

(defun best-first ()
  (terpri)
  (terpri)
  (format t "~&ITERATION #~A" iteration) ;prints the iteration number
  (terpri)
  (princ "open = ") (princ (car *open*)) ;prints the first item on the open list
  (terpri)
  (cond ((null *open*) nil) ;checks for null on open 
        (t (let ((state (car *open*))) ;goes in open and takes the first element and makes it state
             (setq *closed* (cons state *closed*))
             (cond ((equal (get-state state) *goal*)
              (terpri)
              (princ "length of open = ") (princ (list-length *open*)) ;print out the length of the open list
              (terpri)
              (princ "length of closed = ") (princ (list-length *closed*)) ;print out the length of the closed list
              (terpri)
              (print "Goal reached. Solution path:") (reverse (build-solution *goal*))) ;print out the solution path in reverse order
                   (t (setq *open*                                                      ;so it can display start to goal 
                            (insert-by-weight 
                                    (generate-descendants (get-state state)
                                                          (1+ (get-depth state))
                                                          *moves*)
                                    (cdr *open*)))
                      (setq iteration (1+ iteration)) ;increment interation
                      (best-first)
                      ))))))

;;; generate-descendants produces all the descendants of a state
(defun generate-descendants (state depth moves)
  (cond ((null moves) nil) ;checks to see if move is null
        (t (let ((child (funcall (car moves) state))
                 (rest (generate-descendants state depth (cdr moves))))
             (cond ((null child) rest) ;checks to see if there are no children
                   ((retrieve-by-state child rest) rest)
                   ((retrieve-by-state child *open*) rest)
                   ((retrieve-by-state child *closed*) rest)
                   (t (cons (build-record child state depth 
                                          (+ depth (heuristic child))) 
                            rest)))))))


;;; this function will return the solution
(defun build-solution (state)
  (cond ((null state) nil)
        (t (cons state (build-solution 
                        (get-parent 
                         (retrieve-by-state state *closed*)))))))

;;; gives the heuristic value of the state
(defun heuristic (state)
  (heuristic-eval state *goal*))

;;; does the calculation of state and the goal for the heuristic value
(defun heuristic-eval (state goal)
  (cond ((null state) 0)
        ((equal (car state) (car goal)) 
        (heuristic-eval (cdr state) (cdr goal)))
        (t (1+ (heuristic-eval (cdr state) (cdr goal))))))

