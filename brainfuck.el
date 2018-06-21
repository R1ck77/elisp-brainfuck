;;; eval
;;;    (setq load-path (cons (file-name-directory (buffer-file-name)) load-path))
;;; before this module to add this directory to the emacs load path

;;; (require 'brainfuck-list-memory)
(require 'brainfuck-buffer-memory)

(defvar brainfuck-mode-hook nil
  "*List of functions to call when entering brainfuck mode")

(defvar brainfuck-interpret-delay 0.1
  "*Delay between brainfuck symbols evaluation")

;;; Hook utility function
(defun brainfuck-find-file-hook ()
  "Brainfuck mode for .bf files"
  (when (string= "bf" (file-name-extension (buffer-name)))
    (brainfuck-mode)))

(add-hook 'find-file-hook 'brainfuck-find-file-hook)

(defun brainfuck--mark-next ()
  "Mark th next char, while cleaning all other highlights"
  (remove-text-properties (point-min) (point-max) '(font-lock-face))
  (let ((here (point)))
    (put-text-property here (min (+ here 1) (point-max)) 'font-lock-face 'bold)))

(defun brainfuck--read-char ()
  "Read and mark the next character, return nil if none can be found"
  (let ((next-char (char-after)))
    (brainfuck--mark-next)
    (goto-char (+ 1 (point)))
    (if next-char
        (char-to-string next-char)
      nil)))

(defun brainfuck--add (value)
  "Add value to the current memory cell"
  (brainfuck--set (+ (brainfuck--get)
                     value)))

(defun brainfuck--minus ()
  "- operator"
  (brainfuck--add -1))

(defun brainfuck--plus ()
  "+ operator"
  (brainfuck--add 1))

(defun brainfuck--output ()
  ". operator

Print the current cell value in the *Messages* buffer"
  (print (char-to-string (brainfuck--get))))

(defun brainfuck--input ()
  ", operator

Read the value of the current cell from the minibuffer.

The first character of the input is converted to a char and used as a value"
  (brainfuck--set (string-to-char (read-input "value: "))))

(defun brainfuck--read-backward ()
  "Read the current char, moving backward"
  (backward-char)
  (char-after))

(defun brainfuck--read-forward ()
  "Read the current char, moving forward"
  (forward-char)
  (char-before))

(defun brainfuck--char-to-score (char)
  "Used for square bracket balancing: score [ and ] only in a complementary way"
  (cond
   ((= char ?\[) 1)
   ((= char ?\]) -1)
   (t 0)))

(defun brainfuck--backward-until-balanced ()
  "Move backward in the program until the current ] is balanced"
  (let ((score (brainfuck--char-to-score
                (brainfuck--read-backward))))
    (while (/= score 0)
      (let ((new-char (brainfuck--read-backward)))
              (setq score (+ score
                             (brainfuck--char-to-score new-char)))))))

(defun brainfuck--forward-until-balanced ()
  "Move forward in the program until the current [ is balanced"
  (let ((score (brainfuck--char-to-score (char-before))))
    (while (/= score 0)
      (let ((new-char (brainfuck--read-forward)))
        (setq score (+ score
                       (brainfuck--char-to-score new-char)))))))

(defun brainfuck--cond-forth ()
  "[ operator"
  (if (zerop (brainfuck--get))
      (brainfuck--forward-until-balanced)))

(defun brainfuck--cond-back ()
  "] operator"
  (if (not (zerop (brainfuck--get)))
      (brainfuck--backward-until-balanced)))

(defun brainfuck--eval (next-char)
  (let ((is-valid-char t))
    (cond 
     ((equal next-char ">") (brainfuck--right))
     ((equal next-char "<") (brainfuck--left))
     ((equal next-char "+") (brainfuck--plus))
     ((equal next-char "-") (brainfuck--minus))
     ((equal next-char ".") (brainfuck--output))
     ((equal next-char ",") (brainfuck--input))
     ((equal next-char "[") (brainfuck--cond-forth))
     ((equal next-char "]") (brainfuck--cond-back))
     (t (progn
          (setq is-valid-char nil))))    
    (if is-valid-char
        (let ((here (point)))
          (recenter-top-bottom)
          (sit-for brainfuck-interpret-delay)))))

(defun brainfuck-interpret ()
  "Interpret the code at point with a blank memory state"
  (interactive)
  (brainfuck--init)
  (let ((next-char (brainfuck--read-char)))
    (while next-char
      (brainfuck--eval next-char)
      (setq next-char (brainfuck--read-char)))
    (brainfuck--print-memory)))

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
