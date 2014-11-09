
;;; nQueens.lisp
;;;
;;; Solves the classic n-queens problem by finding (and displaying) every way that
;;; n queens can be placed on an n-by-n chess board such that no two queens attack
;;; one another. This requires that each queen be placed on a unique row, column, and
;;; diagonal.

;;; States are reprented by a list of integers of the form (c1 c2 ... ck)
;;; where k <= n, and n represents the number of rows, columns, and queens.
;;; This choice of representation ensures that exactly one queen is placed in each
;;; row. Furthermore, if the list is a permuation of the integers 1 through k, then
;;; exactly one queen is also placed in each column. All that is left to check are
;;; the diagonals.
;;;
;;; This representation is also used to partially define states, by replacing some
;;; columns with nil. Thus (1 3 NIL 2 4) might indicate the partial-state on a
;;; 5-by-5 board:
;;;
;;; Q . . . .
;;; . . Q . .
;;; . . . . .
;;; . Q . . .
;;; . . . Q .
;;;


;;; The function safe-p returns true if there does not exist a pair of queens in the
;;; list that attack each other diagonally. Two pieces at locations (r, c) and (r', c').
;;; Letting delta_r = r - r' and delta_c = c - c', two pieces are on the same diagonal
;;; if delta_r = delta_c or delta_r = - delta_c. More generally, if |delta_r| = |delta_c|.
;;;
(defun abs-eq (x y)
  "Returns T if the arguments have the same absolute value."
  (eql (abs x) (abs y)))

(defun not-abs-eq (x y)
  "Returns T if the arguments do not have the same absolute value."
  (not (abs-eq x y)))

(defun safe-state-p (queens)
  "Returns true if no pair of queens in the current state are in the same diagonal."
  (cond ((null queens) t)                                         ; a null state is safe
        ((not (first queens)) (safe-state-p (rest queens)))
        (t (and (safe-queen-p (first queens) 1 (rest queens))     ; is the first queen safe from the rest
                (safe-state-p (rest queens))))))

(defun safe-queen-p (col row-diff col-list)
  "Returns true if a queen placed in column col is not in the same diagonal as any queen in col-list.
   row-diff represents the difference in rows between the first queen in the col-list and the
   queen indicated by col."

  (cond ((null col-list) t)  ; An empty col-list is safe                                  
        (t (and 
           (cond ((first col-list)                               ; If a queen is placed in the next row
                   (not-abs-eq (- col (first col-list)) row-diff))
                  (t t))                                          ; If the next row is empty.
            (safe-queen-p col (+ row-diff 1) (rest col-list))))))


;(safe-state-p '(1 3 5))
;(safe-state-p '(1 3 2))
;(safe-state-p '(1 NIL 5))
;(safe-state-p '(1 NIL 3))

;;;
;;; Profile commands

(defvar *nodes-expanded*)

(defun log-expand-node ()
  (incf *nodes-expanded*))

(defun eureka (state)
  (list (first state) *nodes-expanded*))


(defun eureka (state)
  (display-queens (first state)))

;;;
;;; Depth first search
;;;

;;; The function tree search is taken from Norvig's PAIP:
(defun tree-search (states goal-p successors combiner)
  "Find a state that satisfies goal-p.  Start with states,
   and search according to successors and combiner."
  ; (format t "~&;; states: ~a" states)
  (cond ((endp states) nil)                              ; Dead end.
        ((funcall goal-p (first states))                 ; Eureka!
         (append (list (eureka (first states)))
                 (tree-search (rest states)
                              goal-p successors
                              combiner)))
        (t (tree-search                                  ; Keep looking.
            (funcall combiner
                     (funcall successors (first states))
                     (rest states))
            goal-p successors combiner))))

;;; Using a combiner of append we implement a 
;;; depth-first-search

(defun depth-first-search (start goal-p successors)
  "Search by expanding the deepest active state."
  (tree-search (list start) goal-p successors #'append))
 
;;;
;;; A state is now redefined to consist of a list of two lists (queens domain). 
;;; queens is a list of columns, for example, (2 4 5 8) represents a chess board
;;; that contains four queens on the[row, col] squares [1, 2], [2, 4], [3, 5], and [4, 8]. 
;;; domain represents a list of available columns in which future queens may be placed.
;;;
;;; Successors to the current space are obtained by appending the list queens with an 
;;; element of domain (without replacement).


(defun post-cons (x l)
  (append l (list x)))

(defun fill-next-row (state)
  (log-expand-node)
  (new-state-aux (first state) '() (second state)))

(defun new-state-aux (queens used-domain new-domain)
  (cond ((endp new-domain) nil)
        (t (let ((next (first new-domain))
                 (rest (rest new-domain)))
             (cons (list (post-cons next queens) (append used-domain rest))
                   (new-state-aux queens  (post-cons next used-domain) rest))))))

;;; (fill-next-row '(() (1 2 3 4 5 6)))

;;; A goal is achieved if the domain is empty and no two queens in queens attack
;;; each other.

(defun finished-p (state)
  (and (null (second state))
       (safe-state-p (first state))))

(defun sequence (i n)
 "Generates a list of consecutive integers, i through n: '(i i+1 ... n)"
  (cond ((> i n) nil)
        (t (cons i (sequence (+ i 1) n)))))

; (sequence 1 8) => (1 2 3 4 5 6 7 8)

(defun n-queens (n)
  (setf *nodes-expanded* 0)
  (depth-first-search (list '()  (sequence 1 n)) #'finished-p #'fill-next-row))


;;;;;;;;;;
;;;
;;; Now to implement backtracking.
;;;
;;;;;;;;;;

(defun backtracking-search (states goal-p impossible-p successors combiner)
  "Find a state that satisfies goal-p.  Start with states,
   and search according to successors and combiner."
  ;(format t "~&;; states: ~a" states)
  (cond ((endp states) nil)                              ; Dead end.
        ((funcall goal-p (first states))                 ; Eureka!
         (append (list (eureka (first states))) 
               (backtracking-search (rest states)
                                    goal-p impossible-p
                                    successors combiner)))
        ((funcall impossible-p (first states))           ; Skip an impossible intermediate state.
         (backtracking-search (rest states) 
                              goal-p impossible-p successors 
                              combiner))
        (t (backtracking-search                          ; Keep looking.
            (funcall combiner
                     (funcall successors (first states))
                     (rest states))
            goal-p impossible-p successors combiner))))


(defun depth-first-backtracking (start goal-p impossible-p successors)
  "Search by expanding the deepest active state."
  (setf *nodes-expanded* 0)
  (backtracking-search (list start) goal-p impossible-p successors #'append))

(defun n-queens-impossible-p (state)
  (not (safe-state-p (first state))))

(defun n-queens-backtracking (n)
  (depth-first-backtracking (list '() (sequence 1 n))
                            #'finished-p 
                            #'n-queens-impossible-p
                            #'fill-next-row))
 
;;;;;;;;;;;
;;;
;;; Now to implement forward checking
;;;
;;;;;;;;;;


(defun forward-checking-search (states 
                                goal-p 
                                impossible-p 
                                check-failure-p
                                successors combiner)
  "Find a state that satisfies goal-p.  Start with states,
   and search according to successors and combiner."
  ; (format t "~&;; states: ~a" states)
  (cond ((endp states) nil)                              ; Dead end.
        ((funcall goal-p (first states)) 
         (append (list  (eureka (first states))) ; Eureka!
                 (forward-checking-search (rest states)
                                          goal-p impossible-p 
                                          check-failure-p successors 
                                          combiner)))
        ((funcall impossible-p (first states))           ; Skip an impossible intermediate state.
         (forward-checking-search (rest states) 
                                  goal-p impossible-p 
                                  check-failure-p successors 
                                  combiner))
        ((funcall check-failure-p (first states))
         (forward-checking-search (rest states) 
                                  goal-p impossible-p 
                                  check-failure-p successors 
                                  combiner))
        (t (forward-checking-search                      ; Keep looking.
            (funcall combiner
                     (funcall successors (first states))
                     (rest states))
            goal-p impossible-p 
            check-failure-p successors combiner))))

(defun depth-first-forward-checking (state
                                     goal-p
                                     impossible-p
                                     check-failure-p
                                     successors)  
  (setf *nodes-expanded* 0)
  (forward-checking-search (list state)
                           goal-p
                           impossible-p
                           check-failure-p
                           successors
                           #'append))


(defun k-nils (k)
  (cond ((<= k 0) nil)
        (t (append (list nil) (k-nils (- k 1))))))

(defun nqcheck-aux (k queens prev next)
  (cond ((endp next) nil)
        (t (or (safe-state-p (append queens (k-nils k) (list (first next))))
               (nqcheck-aux k queens (append prev (list (first next))) (rest next))))))

(defun n-queens-check (k state)
  "Function used for forward checking the n-queens CSP. Returns T if there exists
   a legal value for the kth queen that follows the current state."
  (cond ((>= k (length (second state))) t)      ; A sentinal. If k exceeds the 
                                                ; cardinality of the domain,  
                                                ; the value should be NIL.
        (t (and (nqcheck-aux k (first state) '() (second state))
                (n-queens-check (+ k 1) state)))))



(defun n-queens-forward-failure (state)
  (not (n-queens-check 0 state)))

(defun n-queens-forward-checking (n)
  (depth-first-forward-checking (list '() (sequence 1 n))
                                #'finished-p 
                                #'n-queens-impossible-p
                                #'n-queens-forward-failure
                                #'fill-next-row))

;; Some graphics

(defun display-queens (queens)
  (format t "~&~A~&~&" queens)
  (mapcar #'(lambda (x) 
              (display-queen-row x (length queens))) queens)
  (format t "~& ~&"))

(defun display-queen-row (col maxcol)
  (loop as i from 1 to (- col 1)
        do (format t ". "))
  (format t "Q ")
  (loop as i from (+ col 1) to maxcol
        do (format t ". "))
  (format t "~&"))

