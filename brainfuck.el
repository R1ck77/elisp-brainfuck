(defvar brainfuck-mode-hook nil
  "*List of functions to call when entering brainfuck mode")

(defvar brainfuck-interpret-delay 0.1
  "*Delay between brainfuck symbols evaluation")

(defmacro bfmemory ()
  "Totally unhygienic macro to get the memory"
  `(nth 1 state))

(defmacro bfindex ()
  "Totally unhygienic macro to get the memory"
  `(car state))

(defun brainfuck--mark-next ()
  "Mark th next char, while cleaning all other highlights"
  (remove-text-properties (point-min) (point-max) '(font-lock-face))
  (let ((here (point)))
    (put-text-property here (min (+ here 1) (point-max)) 'font-lock-face font-lock-warning-face)))

(defun brainfuck--read-char ()
  (let ((next-char (char-after)))
    (brainfuck--mark-next)
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

(defun brainfuck--read-backward ()
  (backward-char)
  (char-after))

(defun brainfuck--char-to-score (char)
  (cond
   ((= char ?\[) 1)
   ((= char ?\]) -1)
   (t 0)))

(defun brainfuck--backward-until-balanced ()
  (let ((score (brainfuck--char-to-score
                (brainfuck--read-backward))))
    (while (/= score 0)
      (let ((new-char (brainfuck--read-backward)))
              (setq score (+ score
                     (brainfuck--char-to-score new-char)))))))

(defun brainfuck--cond (state)
  (if (not (zerop (brainfuck--get state)))
      (brainfuck--backward-until-balanced))
  state)

(defun brainfuck--eval (next-char state)
  (let ((is-valid-char t))
    (cond 
     ((equal next-char ">") (brainfuck--right state))
     ((equal next-char "<") (brainfuck--left state))
     ((equal next-char "+") (brainfuck--plus state))
     ((equal next-char "-") (brainfuck--minus state))
     ((equal next-char ".") (brainfuck--output state))
     ((equal next-char ",") (brainfuck--input state))
     ((equal next-char "[") state)
     ((equal next-char "]") (brainfuck--cond state))
     (t (progn
          (setq is-valid-char nil)
          state)))    
    (if is-valid-char
      (let ((here (point)))
        (goto-char (point-min))
        (sit-for brainfuck-interpret-delay)        
        (goto-char here))))
  state)

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

(defvar brainfuck-mode-map nil
  "Keymap for brainfuck major mode")
(if brainfuck-mode-map
    nil
  (setq brainfuck-mode-map (copy-keymap text-mode-map))
  (define-key brainfuck-mode-map "\C-x\C-e" 'brainfuck-interpret))

(defun brainfuck-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'brainfuck-mode)
  (use-local-map brainfuck-mode-map)
  (setq mode-name "BrainFuck")
  (run-hooks 'brainfuck-mode-hook))

(provide 'brainfuck)
