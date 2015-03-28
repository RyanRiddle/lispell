; Spellcheck program in lisp inspired by Peter Norvig's spellcheck algorithm
; Ryan Riddle (http://github.com/RyanRiddle

(defparameter *dictionary* '("cheese" "bread" "turkey" "mustard" "water" "sandwich"))
(defparameter *alphabet* "abcdefghijklmnopqrstuvwxyz")

(defun add (word)
    (push word *dictionary*))
	
(defun check (word)
    (member word *dictionary* :test #'equal))
	 
(defun remove-at (pos seq)
	(concatenate
		'string
		(subseq seq 0 pos)
		(subseq seq (+ pos 1) (length seq))))

(defun insert-at (ch pos str)
	(concatenate 'string 
		(subseq str 0 pos)
		(list ch)
		(subseq str pos (length str))))

(defun replace-at (ch pos seq)
	(concatenate 'string
		(subseq seq 0 pos)
		(list ch)
		(subseq seq (+ pos 1) (length seq))))

; there's got to be better way to swap two values in a sequence
(defun transpose-at (pos1 pos2 seq)
	(let ((tmp (elt seq pos1)))
		(replace-at tmp pos2
			(replace-at (elt seq pos2) pos1 seq))))

(defun gen-deletes (word)
    (let ((deletes nil))
		(dotimes (i (length word))
			(push (remove-at i word) deletes))
		deletes))
		
(defun gen-inserts (word)
    (let ((inserts nil))
		(loop for ch across *alphabet* do
			(loop for i from 0 to (length word) do
				(push (insert-at ch i word) inserts)))
		inserts))

(defun gen-replaces (word)
	(let ((replaces nil))
		(loop for ch across *alphabet* do
			(dotimes (i (length word))
				(push (replace-at ch i word) replaces)))
		replaces))

(defun gen-transposes (word)
	(let ((transposes))
		(loop for i from 0 to (- (length word) 2) do
			(loop for j from (+ i 1) to (- (length word) 1) do
				(push (transpose-at i j word) transposes)))
		transposes))
		
(defun suggest (word)
	(if (check word)
		nil
		(let ((possibilities (append (gen-inserts word) (gen-deletes word) (gen-replaces word) (gen-transposes word))))
			(remove-duplicates
				(remove-if-not (lambda (x) (check x)) possibilities)
				 :test #'equal))))
