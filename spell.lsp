; Spellcheck program in lisp inspired by Peter Norvig's spellcheck algorithm
; Ryan Riddle (http://github.com/RyanRiddle

(defparameter *dictionary* nil)
(defparameter *misspellings* nil)
(defparameter *alphabet* "abcdefghijklmnopqrstuvwxyz")

(defun add (word)
    (push word *dictionary*))

(defun add-misspelling (incorrect correct)
	(let ((entry (assoc incorrect *misspellings* :test #'equal)))
		(if entry 
			(push correct (cadr entry))
			(push `(,incorrect (,correct)) *misspellings*))))

(defun read-dict (dict)
	(print "Reading dictionary...")
	(with-open-file (s dict)
        (do ((l (read-line s) (read-line s nil 'eof)))
            ((eq l 'eof) "Done.")
        (add l))))
	
(defun check (word)
    (if (member word *dictionary* :test #'equal)
		t
		nil))
	 
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

(defun transpose-at (pos1 pos2 seq)
	(let ((word (coerce seq 'list)))
		(rotatef (nth pos1 word) (nth pos2 word))
		(coerce word 'string)))

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

(defun build-misspellings ()
	(dolist (word *dictionary*)
		(let* ((del-dist1 (remove-duplicates (gen-deletes word) :test #'equal))
				(del-dist2 (apply #'append (mapcar #'gen-deletes del-dist1)))
				(deletes (remove-duplicates (append del-dist1 del-dist2) :test #'equal)))
			(dolist (misspelling deletes)
				(add-misspelling misspelling word)))))
		
(defun suggest (word)
	(if (check word)
		nil
		(cadr (assoc word *misspellings* :test #'equal))))
