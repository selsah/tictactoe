(defparameter *board* (list 'board 0 0 0 0 0 0 0 0 0))

;converts binary symbol into eith an "x" or "o" to be printed
(defun convert-to-letter (number)
	   (cond ((equal 10 number) "x")
		 ((equal 1 number) "o")
		 (t " ")))

;prints a single line
(defun print-line (a b c)
	   (format t "~&   ~A | ~A | ~A" 
		  (convert-to-letter a)
		  (convert-to-letter b)
		  (convert-to-letter c)))

;prints board
(defun print-board (board)
	   (print-line (second board) (third board) (fourth board))
	   (format t "~& -------------")
	   (print-line (fifth board) (sixth board) (seventh board))
	   (format t "~& -------------")
	   (print-line (eighth board) (ninth board) (tenth board))
	   (format t "~%~%~%"))


;takes in a number, symbol, board and makes move as long as space is available and input is valid
(defun make-move (number symbol boar)
	   (and (equal (nth number boar) 0)
		(and (> number 0) (< number 10))
		(setf (nth number boar) symbol)))

;player makes a move...retruns true if move was succesfully executed otherwise prints a statement
 (defun player-move ()
   (cond ((equal (winner *board*) 3)
	  '(Computer loveeee))
	 ((full-board *board*)
	  '(Tie game))
	 (t (format t "~&What's your next move? ")
	    (let ((move (read)))
	      (cond ((not (null (make-move move 10 *board*)))
		     (make-move move 10 *board*)
		     (print-board *board*)
		     (computer-move))
		    (t (format t "~& not a valid move")
		       (player-move)))))))

;checks to see if board is full
(defun full-board (board)
	   (cond ((not (member 0 board)) t)
		 (t nil)))

;returns true if move was successfully made, otherwise prints statement
(defun computer-move ()
	  (cond ((equal (winner *board*) 30) nil)
		((full-board *board*) nil)
		(t (let ((guess (+ 1 (random 9))))
		     (cond ((or (equal (block-or-win (return-triplet (find-two *board*))) 2)
				(equal (block-or-win (return-triplet (find-two *board*))) 20))
			    (format t "My Move:")			    
			    (make-move (block-win) 1 *board*)
			    (print-board *board*)
			    (player-move))
			   ((null (make-move guess 1 *board*))
			    (computer-move))		    
			   (t (format t "My Move:")
			      (make-move guess 1 *board*)
			      (print-board *board*)
			      (player-move)))))))

;returns true if x or o is a winner otherwise returns nil
(defun winner (*board*)
	   (let ((win (mapcar #'(lambda (triplet) 
				  (let ((sum (+ (nth (first triplet) *board*)
						(nth (second triplet) *board*)
						(nth (third triplet) *board*))))
				    (cond ((equal sum 30) 30)
					  ((equal sum 3) 3)
					  (t nil))))
			      *triplets*)))
	     (cond ((member 3 win) 3)
		   ((member 30 win) 30)
		   (t nil))))

;sets board back to beginning state new board
(defun new-board ()
  (setf *board* '(board 0 0 0 0 0 0 0 0 0)))


(defun play-game ()
  (format t "~%~%")
  (print-board *board*)
  (if (yes-or-no-p "Would you like to go first")
      (player-move)
      (computer-move))
  (cond ((equal (winner *board*) 3)
	 '(I win I computer))
	((equal (winner *board*) 30)
	 '(You win))
	(t '(Tie game))))

(defparameter *triplets* '((1 2 3) (4 5 6) (7 8 9)
			   (1 4 7) (2 5 8) (3 6 9)
			   (1 5 9) (3 5 7)))

;checks entire board to see if opponent has two in a row...return a list containing all positions that do
(defun find-two (board)
  (mapcar #'(lambda (triplet)
	      (let ((sum (+ (nth (first triplet) board)
			    (nth (second triplet) board)
			    (nth (third triplet) board))))
		(cond ((equal sum 20) triplet)
		      ((equal sum 2) triplet)
		      (t nil)))) *triplets*))

;returns triplet if there is one from a list of items...useful for find-two function
(defun return-triplet (list)
	   (find-if #'(lambda (item)
			(cond ((not (null item)) item)
			      (t nil)))
		    list))

;finds empty space on the board...useful for finding the empty space to block when opponent is about to win.
(defun block-space (list)
	   (find-if #'(lambda (item)
			(cond ((equal (nth item *board*) 0) item)
			      (t nil)))
		    list))

;makes use of three functions that were defined to return the space to be blocked when opponent is about to win 
(defun block-win ()
	   (block-space (return-triplet (find-two *board*))))

;returns the result of checking to see what the sum of a three number list is ... returns two or twenty letting us know 
;two would mean go for win in this game...twenty block opponent win
(defun block-or-win (list)
	   (cond ((null list) nil)
		 (t 
		  (let ((sum (+ (nth (first list) *board*)
			 (nth (second list) *board*)
			 (nth (third list) *board*))))
		    (cond ((equal sum 2) 2)
			  ((equal sum 20) 20)			  
			  (t nil))))))
