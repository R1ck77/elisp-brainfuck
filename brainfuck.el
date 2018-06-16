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
  (setcar (nthcdr (bfindex)
                  (bfmemory))
          new-value)
  state)

(defun brainfuck--get (state)
  (nth (bfindex) (bfmemory)))

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
  (print (char-to-string (brainfuck--get state))))

(defun brainfuck--input (state)
  (brainfuck--set state (string-to-char (read-input "value: "))))

(defun brainfuck--matching-parenthesis ()
  (goto-char (- point 1))
  )

(defun brainfuck--cond (state)
  (if (not (zerop (brainfuck--get state)))
      (brainfuck--goto-matching-parenthesis)
      )
  state)

(defun brainfuck--eval (next-char state)
  (cond 
    ((equal next-char ">") (brainfuck--right state))
    ((equal next-char "<") (brainfuck--left state))
    ((equal next-char "+") (brainfuck--plus state))
    ((equal next-char "-") (brainfuck--minus state))
    ((equal next-char ".") (brainfuck--output state))
    ((equal next-char ",") (brainfuck--input state))
    ((equal next-char "[") state)
    ((equal next-char "]") (brainfuck--cond state))
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
