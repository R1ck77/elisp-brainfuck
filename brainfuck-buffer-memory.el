(defvar brainfuck--memory-buffer nil
  "Memory buffer to hold the buffer evaluation")
(make-variable-buffer-local 'brainfuck--memory-buffer)

(defvar brainfuck--current-line-cache nil
  "Current line, cached for performance reasons")
(make-variable-buffer-local 'brainfuck--current-line-cache)

(defvar brainfuck--current-value-cache nil
  "Current value, cachied for performance reasons")
(make-variable-buffer-local 'brainfuck--current-value-cache)

(defconst brainfuck--buffer-memory-name-template "*BrainFuckMemory%s*"
  "Template for the name of the interpreter's memory")

(defconst brainfuck--address-format "0x%.4x "
  "Format for the memory address, including the separator")

(defconst brainfuck--value-format "%d"
  "Format for the value")

(defconst brainfuck--line-regexp "0x\\([0-9af]+\\) \\([0-9]+\\)"
  "Regular expression complementing the address and value format strings")

(defun brainfuck--mark-line ()
  (with-current-buffer brainfuck--memory-buffer
    (put-text-property (line-beginning-position) (line-end-position)
                       'font-lock-face 'bold)))

(defun brainfuck--unmark-line ()
  (with-current-buffer brainfuck--memory-buffer
      (remove-text-properties (line-beginning-position) (line-end-position)
                          '(font-lock-face))))

(defun brainfuck--get-line ()
  (with-current-buffer brainfuck--memory-buffer
    brainfuck--current-line-cache))

(defun brainfuck--set-line (new-line)
  (with-current-buffer brainfuck--memory-buffer
    (setq brainfuck--current-line-cache new-line)))

(defun brainfuck--set-value (new-value)
  (with-current-buffer brainfuck--memory-buffer
    (setq brainfuck--current-value-cache new-value)))

(defun brainfuck--get-value ()
  (with-current-buffer brainfuck--memory-buffer
    brainfuck--current-value-cache))

(defun brainfuck--memory-buffer-name (&optional index)
  "Get a name for a memory buffer"
  (format brainfuck--buffer-memory-name-template (if index index "")))

(defun brainfuck--create-empty-buffer ()
  "Create a new buffer for the interpreter's memory.

Get the default name (no numeric index) or an indexed name
that's not used already, returns the buffer"
  (let ((default-name (brainfuck--memory-buffer-name)))
    (if (not (get-buffer default-name))
        (get-buffer-create default-name)
      (let* ((index 0)
             (indexed-name (brainfuck--memory-buffer-name index)))
        (while (get-buffer indexed-name)
          (setq index (+ index 1))
          (setq indexed-name (brainfuck--memory-buffer-name index)))
        (get-buffer-create indexed-name)))))

(defun brainfuck--address-representation (address)
  "Format an address"
  (format brainfuck--address-format address))

(defun brainfuck--insert-memory-address (address)
  "Insert a correctly formatted address with value separator"
  (insert (brainfuck--address-representation address)))

(defun brainfuck--value-representation (value)
  "Format an value"
  (format brainfuck--value-format value))

(defun brainfuck--insert-value (value)
  "Insert a correctly formatted value"
  (insert (brainfuck--value-representation value)))

(defun brainfuck--insert-line (address value)
  "Insert a line with the specified address and value"
  (brainfuck--unmark-line)
  (brainfuck--insert-memory-address address)
  (brainfuck--insert-value value)
  (beginning-of-line)
  (brainfuck--mark-line)
  (brainfuck--set-line address)
  (brainfuck--set-value value))

(defun brainfuck--insert-first-line ()
  "Insert the first line in the buffer"
  (brainfuck--insert-line 0 0))

(defun brainfuck--empty-state ()
  (let ((buffer (brainfuck--create-empty-buffer)))
    (setq brainfuck--memory-buffer buffer)
    (with-current-buffer brainfuck--memory-buffer
      (font-lock-mode)
      (setq brainfuck--memory-buffer buffer)
      (brainfuck--insert-first-line))    
    (display-buffer brainfuck--memory-buffer)))

(defun brainfuck--init ()
  (brainfuck--empty-state))

(defun brainfuck--clear-line ()
  (delete-region (line-beginning-position)
                 (line-end-position)))

(defun brainfuck--set (new-value)
  (with-current-buffer brainfuck--memory-buffer
    (brainfuck--clear-line)
    (brainfuck--insert-line (brainfuck--get-line) new-value))
  (brainfuck--set-value new-value))

(defun brainfuck--get ()
  (brainfuck--get-value))

(defun brainfuck--right ()
  (brainfuck--unmark-line)
  (with-current-buffer brainfuck--memory-buffer
    (end-of-line)
    (if (not (zerop (forward-line)))
        (progn
          (insert "\n")
          (brainfuck--insert-line (+ 1 (brainfuck--get-line)) 0))
      (brainfuck--update-cache-with-line-content)))
  (brainfuck--mark-line))

(defun brainfuck--parse-line ()  
  (save-excursion
    (save-match-data
      (if (search-forward-regexp brainfuck--line-regexp (line-end-position) t)
          (list (string-to-number (buffer-substring (match-beginning 1) (match-end 1)) 16)
                (string-to-number (buffer-substring (match-beginning 2) (match-end 2))))
        (error (concat "invalid line \"" (substring-no-properties (buffer-substring (line-beginning-position) (line-end-position))) "\""))))))

(defun brainfuck--update-cache-with-line-content ()
  (let ((line-content (brainfuck--parse-line)))
    (brainfuck--set-line (nth 0 line-content))
    (brainfuck--set-value (nth 1 line-content))))

(defun brainfuck--previous-line ()
  (beginning-of-line)
  (let ((result (forward-line -1)))
    (if (= -1 result)
        (error "Negative memory address"))))

(defun brainfuck--left ()
  (with-current-buffer brainfuck--memory-buffer
    (brainfuck--unmark-line)
    (brainfuck--previous-line)
    (beginning-of-line)
    (brainfuck--mark-line)
    (brainfuck--update-cache-with-line-content)))

(defmacro comment (&rest args))

(defun brainfuck--print-memory ()
  (comment "do nothing"))

(fmakunbound 'comment)

(provide 'brainfuck-buffer-memory)
