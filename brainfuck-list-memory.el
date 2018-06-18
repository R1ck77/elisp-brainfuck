(defvar brainfuck--state nil
  "The state of the memory in the current buffer")
(make-variable-buffer-local 'brainfuck--state)

(defmacro bfmemory ()
  "Totally unhygienic macro to get the memory"
  `(nth 1 brainfuck--state))

(defmacro bfindex ()
  "Totally unhygienic macro to get the memory"
  `(car brainfuck--state))

(defun brainfuck--empty-state ()
  (list 0 (list 0)))

(defun brainfuck--init ()
  (setq brainfuck--state (brainfuck--empty-state)))

(defun brainfuck--set (new-value)
  (setcar (nthcdr (bfindex)
                  (bfmemory))
          new-value))

(defun brainfuck--get ()
  (nth (bfindex) (bfmemory)))

(defun brainfuck--right ()
  (let ((index (bfindex))
        (memory (bfmemory)))
    (if (= (+ index 1) (length memory))
        (setq memory (append memory (list 0))))
    (setcar brainfuck--state (+ index 1))
    (setcar (nthcdr 1 brainfuck--state) memory)))

(defun brainfuck--left ()
  (let ((index (bfindex)))
    (if (= index 0)
        (error "Invalid shift beyond 0")
      (setcar brainfuck--state (- index 1)))))

(defun brainfuck--print-memory ()
  (print brainfuck--state))

(fmakunbound 'bfmemory)
(fmakunbound 'bfindex)

(provide 'brainfuck-list-memory)
