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

;;; (defun forward-line-create ()
;;;   (if (not (zerop (forward-line)))
;;;       (insert "\n")))
;;; 
;;; (defun test-next-line ()
;;;   (forward-line-create)
;;;   (if (= (line-beginning-position) (line-end-position))
;;;       (insert "new stuff!")
;;;     (goto-char (line-end-position))))

(defun brainfuck--value-representation (value)
  "Format an value"
  (format brainfuck--value-format value))

(defun brainfuck--insert-value (value)
  "Insert a correctly formatted value"
  (insert (brainfuck--value-representation value)))

(defun brainfuck--insert-line (address value)
  "Insert a line with the specified address and value"
  (brainfuck--insert-memory-address address)
  (brainfuck--insert-value value)
  (beginning-of-line)  
  (setq brainfuck--current-line-cache address)
  (setq brainfuck--current-value-cache value))

(defun brainfuck--insert-first-line ()
  "Insert the first line in the buffer"
  (brainfuck--insert-line 0 0))

(defun brainfuck--empty-state ()
  (let ((buffer (brainfuck--create-empty-buffer)))
    (with-current-buffer buffer
      (brainfuck--insert-first-line))    
    (setq brainfuck--memory-buffer buffer)
    (display-buffer buffer)))

(defun brainfuck--init ()
  (display-buffer)
  (brainfuck--empty-state))

(defun brainfuck--clear-line ()
  (delete-region (line-beginning-position)
                 (line-end-position)))

(defun brainfuck--set (new-value)
  (with-current-buffer brainfuck--memory-buffer
    (brainfuck--clear-line)
    (brainfuck--insert-line brainfuck--current-line-cache new-value))
  (setq brainfuck--current-value-cache new-value))

(defun brainfuck--get ()
  brainfuck--current-value-cache)

(defun brainfuck--right ()

  )

(defun brainfuck--parse-line ()  
  (search-forward-regexp brainfuck--line-regexp (line-end-position) t)  
  ;;; HIC SUNT LEONES → parse the matches
  )

(defun brainfuck--update-cache-with-line-content ()
  (let ((line-content (brainfuck--parse-line)))
    (setq brainfuck--current-line-cache (nth 0 line-content))
    (setq brainfuck--current-value-cache (nth 1 line-content))))

(defun brainfuck--previous-line ()
  (let ((result (forward-line -1)))
    (if (= -1 result)
        (error "Negative memory address"))))

(defun brainfuck--left ()
  (with-current-buffer brainfuck--memory-buffer
    (brainfuck--previous-line)
    (brainfuck--update-cache-with-line-content)))

(defmacro comment (&rest args))

(defun brainfuck--print-memory ()
  (comment do nothing)
  )

(fmakunbound 'comment)

(provide 'brainfuck-buffer-memory)
