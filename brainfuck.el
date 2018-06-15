;; . putchar(*ptr)
;; , *ptr = getchar()
;; [ while(*ptr) {
;; ] }

(defmacro bfmemory ()
  "Totally unhygienic macro to get the memory"
  `(nth 1 state))

(defmacro bfindex ()
  "Totally unhygienic macro to get the memory"
  `(car state))

(defun brainfuck--read-char ()
  (let ((next-char (char-after)))
    (goto-char (+ 1 (point)))
    (if next-char
        (char-to-string next-char)
      nil)))

(defun brainfuck--right (state)
  (let ((index (bfindex))
        (memory (bfmemory)))
    (if (= (+ index 1) (length memory))
        (setq memory (append memory (list 0))))
    (setcar state (+ index 1))
    (setcar (nthcdr 1 state) memory))
  state)

(defun brainfuck--left (state)
  (let ((index (bfindex)))
    (if (= index 0)
        (error "Invalid shift beyond 0")
      (setcar state (- index 1))))
  state)

(defun brainfuck--set (state new-value)
  (print (format "Setting %s to %s" state new-value))
  (setcar (nthcdr (bfindex)
                  (bfmemory))
          new-value)
  state)

(defun brainfuck--add (state value)
  (brainfuck--set state
                  (+ (nth (bfindex)
                          (bfmemory))
                     value)))

(defun brainfuck--minus (state)
  (brainfuck--add state -1))

(defun brainfuck--plus (state)
  (brainfuck--add state 1))

(defun brainfuck--output (state)
  
  )

(defun brainfuck--input (state)
  )

(defun brainfuck--eval (next-char state)
  (cond 
    ((equal next-char ">") (brainfuck--right state))
    ((equal next-char "<") (brainfuck--left state))
    ((equal next-char "+") (brainfuck--plus state))
    ((equal next-char "-") (brainfuck--minus state))
    ((equal next-char ".") (print "output"))
    ((equal next-char ",") (print "input"))
    ((equal next-char "[") (print "cond-start"))
    ((equal next-char "]") (print "cond-end"))
    (t (print "missed!"))))

(defun brainfuck--empty-state ()
  (list 0 (list 0)))

(defun brainfuck-interpret ()
  "Interpret the code at point with a blank memory state"
  (interactive)
  (let ((state (brainfuck--empty-state))
        (next-char (brainfuck--read-char)))
    (while next-char
      (brainfuck--eval next-char state)
      (setq next-char (brainfuck--read-char)))
    state))

(fmakunbound 'bfmemory)
(fmakunbound 'bfindex)
