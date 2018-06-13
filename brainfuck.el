;; > ptr++
;; < ptr--
;; + ++(*ptr)
;; - --(*ptr)
;; . putchar(*ptr)
;; , *ptr = getchar()
;; [ while(*ptr) {
;; ] }

(defun brainfuck--read-char ()
  (let ((next-char (char-after)))
    (goto-char (+ 1 (point)))
    (if next-char
        (char-to-string next-char)
      nil)))

(defun brainfuck--eval (next-char state)
  (print (format "Checking '%s'" next-char))
  (cond 
    ((equal next-char ">") (print "move+"))
    ((equal next-char "<") (print "move-"))
    ((equal next-char "+") (print "value+"))
    ((equal next-char "-") (print "value-"))
    ((equal next-char ".") (print "output"))
    ((equal next-char ",") (print "input"))
    ((equal next-char "[") (print "cond-start"))
    ((equal next-char "]") (print "cond-end"))
    (t (print "missed!"))))

(defun brainfuck-interpret ()
  "Interpret the code at point with a blank memory state"
  (interactive)
  (let ((state '(0 (0)))
        (next-char (brainfuck--read-char)))
    (while next-char
      (brainfuck--eval next-char state)
      (setq next-char (brainfuck--read-char)))))
