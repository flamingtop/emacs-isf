
; incrementally search files in specified folder

;; which project dir to search from
;; by default it's the current folder where emacs is opened
(defvar isf-dir (file-truename default-directory))

;; command to set project dir
(defun isf-set-dir (dir)
  (interactive "DProject directory:")
  (setq isf-dir (file-truename dir)))

;; output buffer
(defvar isf-buffer-name "*isf-files*")
;; sub-process
(defvar isf-process-name "isf-prociess")
;; mini-buffer prompt
(defvar isf-prompt "Search: ")

(defun isf-debounce (func &optional delay)
  ;; takes a function and returns a debouncable version of it
  (let*
      ((callee
        (cond
         ((and
           (not (symbolp func)) (functionp func)) `',func)
         ((boundp func) (symbol-value func))
         (t `',func)))
       (delay (if (not delay) 0.100 delay))
       (timer (intern (concat "timer-" (symbol-name func)))))
      
    (progn
      (set timer nil)
      `(lambda
         (&rest args)
         (progn
           (if
               (and (vectorp ,timer) (not (aref ,timer 0)))
               (cancel-timer ,timer))
           (setq
            ,timer
            (run-at-time
             ,delay nil
             (lambda
               (params)
               (apply ,callee params))
             args)))))))

(defun isf-on-key (callback)
  ;; general keyboard event handler
  (let
      ((keys ""))
    (catch 'break
      (while t
        (progn
          (let
              ;; timeout neccessary?
              ((char (read-char-exclusive (concat isf-prompt keys))))
               (cond

                 ;; ESC
                ((= char ?\e)  (throw 'break nil))

                ;; BACKSPACE
                ((= char ?\d) 
                 (if (> (length keys) 0)
                     (setq keys (substring keys 0 (1- (length keys))))))

                ;; ENTER
                ((= char ?\r)  (progn
                                (funcall callback keys)
                                (throw 'break nil)))

                (t (unless (or
                            (equal '(control) (event-modifiers char))
                            (equal '(super) (event-modifiers char))
                            (equal '(meta) (event-modifiers char)))
                     (setq keys (concat keys (byte-to-string char)))))))

          (if
              (> (length keys) 0)
              (progn
                (message (concat isf-prompt keys))
                (funcall callback keys))))))))


(defun isf-find-files (pattern)
  (catch 'break

    ;; capture empty input
    (if
        (equal 0 (length (s-trim pattern)))
        (throw 'break 'empty-pattern))
    
    ;; empty process raw output
    (setq raw-output "")

    ;; always get rid of the old process first
    (if
        (get-process isf-process-name)
        (delete-process (get-process isf-process-name)))

    ;; put a tail * into the search pattern
    (unless
        (equal "*" (substring pattern (1- (length pattern))))
        (setq pattern (concat pattern "*")))

    ;; fire up the process
    (start-process
        isf-process-name
        nil
        "find" isf-dir "\(" "-name" "*.git*" "\)" "-prune" "-o" "-name" pattern "-print")

    ;; show searching indicator 
    ;; (setq isf-searching t)
    ;; (isf-searching)

    ;; collect process output
    (set-process-filter
        (get-process isf-process-name)
        (lambda (process output)
          (setq raw-output (concat raw-output output))))

    ;; monitor process progress
    (set-process-sentinel
        (get-process isf-process-name)
        (funcall
         (lambda()
           `(lambda (process event)
              (if
                  (equal event "finished\n")
                  (progn
                   ;; (setq isf-searching nil)
                    (funcall
                     'isf-render-files
                     (if
                         (> (length raw-output) 0)
                         (split-string (s-trim raw-output) "\n")
                       nil))))))))
nil))

(defun isf-find-files-terminal-renderer () 
  `(lambda (button) (find-file-other-window (button-get button 'path))))

(defun isf-find-files-gui-renderer ()
  `(lambda (button)
     (delete-window
      (get-buffer-window isf-buffer-name))
     (with-selected-frame
         (make-frame
          '((width . 1800)
            (height . 60)
            (minibuffer . t)))
       (find-file-existing (button-get button 'path))
       (split-window-vertically (floor (* (/ 2.0 3.0) (frame-height))))
       (set-window-buffer
        (select-window (window-in-direction 'below))
        isf-buffer-name))))

(defun isf-render-files (files)
  "display found files in a temporary buffer"
  (let   
      ((button-action
        (if (display-graphic-p) (isf-find-files-terminal-renderer)
          (isf-find-files-terminal-renderer))))

    (with-current-buffer (get-buffer-create isf-buffer-name)
      (fundamental-mode)
      (if buffer-read-only
          (toggle-read-only))
      (erase-buffer)
      (dolist (path files)
        (button-put
         (insert-button

          ;; catch the error
          (condition-case ex
              (substring path (length isf-dir))
            ('args-out-of-range (message-box path)))

          'action button-action
          'mouse-action button-action
          'keymap '(keymap (mouse-1 . push-button) (13 . push-button)))
         'path
         path)
        (insert "\n"))
      (help-mode))

    (progn
      (if
          (get-buffer-window isf-buffer-name)
          (select-window (get-buffer-window isf-buffer-name))
        (select-window (split-window-below (floor (* (/ 2.0 3.0) (window-height))))))
      (switch-to-buffer isf-buffer-name)
      (goto-char 1))))




;; incrementally search files
(defun isf ()
  (interactive)
  (isf-on-key (isf-debounce 'isf-find-files)))















;; TODO
;; show searching folder 

;; - determine longest / shortest common sub-string
;; - highlight only the last two segments of the returned path
;; - check if dir is set if not read from minibuffer with auto completion

;; (condition-case ex
;;  (error 'error)
;;  ('erro (message-box "ERROR"))
;;  ('hello (message-box "BLAH"))
;;  (message-box "LAST CASE"))



;; (setq isf-searching-current-bar "-")
;; (setq isf-searching nil)

;; (defun isf-searching ()
;;   (if isf-searching
;;       (run-at-time 0.010 nil 
;;                    (lambda ()
;;                      (cond
;;                       ((equal isf-searching-current-bar "-")
;;                        (setq isf-searching-current-bar "\\"))
;;                       ((equal isf-searching-current-bar "\\")
;;                        (setq isf-searching-current-bar "|"))
;;                       ((equal isf-searching-current-bar "|")
;;                        (setq isf-searching-current-bar "/"))
;;                       ((equal isf-searching-current-bar "/")
;;                        (setq isf-searching-current-bar "-")))
;;                      (message isf-searching-current-bar)
;;                      (isf-searching)))))




