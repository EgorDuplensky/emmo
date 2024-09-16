;; emmo.el --- basic editing commands for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.
;; Maintainer: egor.duplensky@gmail.com
;; Keywords: motions
;; Package: emmo
;;; Commentary:
;;;  Emacs motions
;;; Code:
;;; Todo:
;;; - support numberic argument

(require 'expand-region)
(eval-when-compile (require 'cl-lib))

(defun select-region-between-points (point1 point2)
  "Select the region between POINT1 and POINT2."
  (interactive "nEnter first point: \nnEnter second point: ")
  (goto-char point1)
  (push-mark point2 nil t)
  (setq mark-active t))

(defun emmo-skip-whitespaces-forward (stop-after &optional n)
  (interactive)
  "Search forward, but keep point at the beginning of the match."
  (when (re-search-forward "[^[:space:]\n]" nil t n)
    (when (not stop-after)
      (goto-char (match-beginning 0))
      )
    )
  )

(defun emmo-skip-whitespaces-backward (stop-after &optional n)
  (interactive)
  "Search backward, but keep point at the beginning of the match."
  (when (re-search-backward "[^[:space:]\n]" nil t n)
    (when (not stop-after)
      (goto-char (match-end 0))
      )
    )
  )

(defun emmo-find-opening-char (char closing-char count)
  (message "looking for %c or %c. count : %d" char closing-char count)
  (let ((regexp (format "[%c%c]" char closing-char)))
    (re-search-backward regexp nil t)
    (setq matched-char (char-after (match-beginning 0)))
    (if (eq matched-char closing-char)
        (setq count (+ count 1))
      (setq count (- count 1)))
    (when (not (eq count 0))
      (emmo-find-opening-char char closing-char count)
      )
    )
  )

(defun emmo-mark-around* (yank? search-forward-char)
  "Works like vim's ci command. Takes a char, like ( or \" and
kills the first ancestor semantic unit starting with that char."
  (let* ((expand-region-fast-keys-enabled nil)
         (expand-region-smart-cursor nil)
         (char (or search-forward-char
                   (read-char
                    (if yank?
                        "Yank around, starting with:"
                      "Change around, starting with:"))))
         (starting-point (point))
         (closing-char (emmo-matching-pair-char char))
         )
    (emmo-find-opening-char char closing-char 1)
    )
  )

(defun emmo-find-openining-round ()
  (interactive)
    (emmo-mark-around* nil ?\( )
    )

(defun mark-around* (yank? search-forward-char)
  "Works like vim's ci command. Takes a char, like ( or \" and
kills the first ancestor semantic unit starting with that char."
  (let* ((expand-region-fast-keys-enabled nil)
         (expand-region-smart-cursor nil)
         (char (or search-forward-char
                   (char-to-string
                    (read-char
                     (if yank?
                         "Yank around, starting with:"
                       "Change around, starting with:")))))
         (q-char (regexp-quote char))
         (starting-point (point)))
    (when search-forward-char
      (search-forward char nil))
    (cl-flet ((message (&rest args) nil))
      (when (looking-at q-char)
        (er/expand-region 1))
      (while (and (not (= (point) (point-min)))
                  (not (looking-at q-char)))
        (er/expand-region 1))
      (if (not (looking-at q-char))
          (if search-forward-char
              (error "Couldn't find any expansion starting with %S" char)
            (goto-char starting-point)
            (setq mark-active nil)
            (change-around* yank? char))
        (if yank?
            (progn
              (copy-region-as-kill (region-beginning) (region-end))
              (ci--flash-region (region-beginning) (region-end))
              (goto-char starting-point))
          (select-region-between-points (region-beginning) (region-end)))))))

(defun copy-to-char (arg char &optional interactive)
  "Kill up to and including ARGth occurrence of CHAR.
When run interactively, the argument INTERACTIVE is non-nil.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
See also `zap-up-to-char'.
If called interactively, do a case sensitive search if CHAR
is an upper-case character."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		             (read-char-from-minibuffer "Copy to char: "
						                        nil 'read-char-history)
                     t))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
	    (setq char (or (aref translation-table-for-input char) char))))
  (let ((case-fold-search (if (and interactive (char-uppercase-p char))
                              nil
                            case-fold-search)))
    (let ((beg (point)))
      (kill-ring-save (point) (search-forward (char-to-string char) nil nil arg))
      (my/flash-region beg (point))
      (goto-char beg))
    ))

(defun copy-up-to-char (arg char &optional interactive start-point)
  "Kill up to, but not including ARGth occurrence of CHAR.
When run interactively, the argument INTERACTIVE is non-nil.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point.
If called interactively, do a case sensitive search if CHAR
is an upper-case character."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		             (read-char-from-minibuffer "Copy up to char: "
						                        nil 'read-char-history)
                     t))
  (let ((direction (if (>= arg 0) 1 -1))
        (case-fold-search (if (and interactive (char-uppercase-p char))
                              nil
                            case-fold-search)))
    (let ((beg (point)))
      (kill-ring-save (point)
		              (progn
		                (forward-char direction)
		                (unwind-protect
		                    (search-forward (char-to-string char) nil nil arg)
		                  (backward-char direction))
		                (point)))
      (my/flash-region beg (point))
      (goto-char beg)
      )
    ))

(defalias 'kill-up-to-char 'zap-up-to-char)
(defalias 'kill-to-char 'zap-to-char)

(defun emmo-mark-around-whitespace ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'whitespace)))
    (when bounds
      (select-region-between-points (car bounds) (cdr bounds))
      )
    )
  )

;; todo: currenly marks inside word
(defun emmo-mark-around-word (&optional n)
  "Mark the around word at the current point."
  (interactive)
  (unless n (setq n 1))
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (goto-char (car bounds))
      (emmo-skip-whitespaces-backward nil)
      ;; (forward-word)
      (let ((beg (point)))
        (forward-word n)
        (select-region-between-points beg (point))
        )
      )
    )
  )

(defun emmo-mark-around-symbol (&optional n)
  "Mark the around symbol at the current point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (goto-char (car bounds))
      (emmo-skip-whitespaces-backward nil)
      (let ((beg (point)))
        (forward-symbol n)
        (select-region-between-points beg (point))
        )
      )
    )
  )

(defun emmo-mark-around-line (&optional n)
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'line)))
    (when bounds
      (goto-char (car bounds))
      (let ((beg (point)))
        (forward-line n)
        (select-region-between-points beg (point))
        )
      )
    )
  )

(defun emmo-mark-around-paragraph (&optional n)
  "Mark around paragraph at point N times."
  (interactive "p")
  (mark-paragraph n)
  )

(defun emmo-mark-around-round-bracket (&optional n)
  "Mark the text inside (including) round brackets at point N times."
  (interactive)
  (mark-around* nil "(")
  )

(defun emmo-mark-around-square-bracket (&optional n)
  "Kill the function at point."
  (interactive)
  (mark-around* nil "[")
  )

(defun emmo-mark-around-curly-bracket (&optional n)
  "Kill the function at point."
  (interactive)
  (mark-around* nil "{")
  )

(defun emmo-mark-around-angle-bracket (&optional n)
  "Kill the function at point."
  (interactive)
  (mark-around* nil "<")
  )

(defun emmo-mark-around-single-quote (&optional n)
  "Kill the function at point."
  (interactive)
  (mark-around* nil "-")
  )

(defun emmo-mark-around-double-quote (&optional n)
  "Kill the function at point."
  (interactive)
  (mark-around* nil "\"")
  )

(defun emmo-mark-around-function (&optional n)
  "Kill the function at point."
  (interactive)
  (mark-defun)
  )

(defun emmo-mark-around-buffer (&optional n)
  "Kill the function at point."
  (interactive)
  (mark-whole-buffer)
  )

(defun emmo-mark-if-statement (&optional n)
  (interactive)
  (treesit-parser-create 'cpp)
  (let* ((node (treesit-node-at (point)))
         (if-node (treesit-parent-until node
                                        (lambda (&optional n)
                                          (equal (treesit-node-type n) "if_statement")))))
    (if if-node
        (progn
          (goto-char (treesit-node-start if-node))
          (set-mark (treesit-node-end if-node))
          (message "If statement marked."))
      (message "Not on an if statement."))))

(defun emmo-mark-inside-argument (&optional n)
  "Mark the N-th function parameter_declaration.
Under the cursor using the Tree-sitter library.
If N is not provided, mark the first parameter."
  (interactive "p")
  (treesit-parser-create 'cpp)
  (let* ((node (treesit-node-at (point)))
         (parameter-node nil)
         (func-node nil)
         (argument-nodes '()))

    ;; Check if already inside a parameter declaration
    (setq parameter-node (treesit-parent-until node
                                               (lambda (&optional a-node)
                                                 (equal (treesit-node-type a-node) "parameter_declaration"))))

    ;; If a `parameter_declaration` node is found, mark this node and exit
    (if parameter-node
        (progn
          (goto-char (treesit-node-start parameter-node))
          (set-mark (treesit-node-end parameter-node)))
      ;; Find the encompassing function node
      (setq func-node (treesit-parent-until node
                                            (lambda (&optional a-node)
                                              (member (treesit-node-type a-node)
                                                      '("function_definition" "function_declaration" "method_definition")))))

      ;; Recursive function to collect parameter_declaration nodes
      (defun collect-argument-nodes (node)
        "Recursively collect all parameter_declaration nodes."
        (when (member (treesit-node-type node) '("parameter_declaration"))
          (push node argument-nodes))
        (mapcar #'collect-argument-nodes (treesit-node-children node)))

      ;; Collect all parameter_declaration nodes within function node context
      (when func-node
        (collect-argument-nodes func-node)
        (setq argument-nodes (reverse argument-nodes)))  ; Reverse to maintain order

      ;; Mark the N-th argument node if it exists
      (let ((argument-node (nth (1- n) argument-nodes)))  ; 1-indexed to 0-indexed
        (if argument-node
            (progn
              (goto-char (treesit-node-start argument-node))
              (set-mark (treesit-node-end argument-node))
              (message "Marked parameter %d." n))
          (message "Parameter %d not found." n)))
      )
    )
  )

(defgroup emmo-emotions nil
  "Display informations of the current line."
  :group 'tools
  :group 'convenience
  :group 'emotions
  )

(defconst emmo-actions-key-alist
  '((mark      . "m")
    (copy      . "w")
    (kill      . "k")
    (delete    . "d")
    (duplicate . "x") ;; xerox?
    (comment   . "c")
    (indent    . "i")
    (go        . "g")
    (surround  . "s")
    )
  "Alist mapping each action to its corresponding character key.")

(defconst emmo-objects-key-alist
  '((word           . "w")
    (symbol         . "s")
    (line           . "l")
    (paragraph      . "p")
    (argument       . "a")
    (function       . "d") ;; defun
    (white-space    . "SPC")
    ;;(till         . "t")
    ;;(find         . "f")
    ;;(till-backward . "T")
    ;;(find-forward  . "F")
    (round-bracket  . "(")
    (square-bracket . "[")
    (curly-bracket  . "{")
    (angle-bracket  . "<")
    (single-quote   . "'")
    (double-quote   . "\"")
    (buffer         . "b")
    )
  "Alist mapping each object to its corresponding character key.")

(defconst emmo-scopes-key-alist
  '((inside . "i")
    (around . "a")
    (beg    . "b")
    (end    . "e")
    )
  "Alist mapping each scope to its corresponding character key.")

(defun emmo-flash-region (start end)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (overlay-put overlay 'priority 100)
    (run-with-timer 0.1 nil 'delete-overlay overlay)))

(defun emmo-kill-ring-flash-save (beg end)
  (kill-ring-save (point) (mark))
  (emmo-flash-region (point) (mark))
  )

(defun emmo-matching-pair-char (char)
  (cond
   ((eq char ?{) ?})
   ((eq char ?}) ?{)
   ((eq char ?[) ?])
   ((eq char ?]) ?[)
   ((eq char ?\() ?\))
   ((eq char ?\)) ?\()
   ((eq char ?<) ?>)
   ((eq char ?>) ?<)
   ((eq char ?\`) ?\`)
   ((eq char ?\') ?\')
   ((eq char ?\") ?\")
   )
  )

(defun emmo-mark-around-object (object &optional n)
  "Call the appropriate function based on the value of OBJECT."
  (interactive
   (list (completing-read "Select object type: "
                          '("word" "line" "paragraph" "function" "round-bracket" "square-bracket" "curly-bracket" "angle-bracket" "single-quote" "double-quote"))))
  (let ((func-name (intern (concat "emmo-mark-around-" (symbol-name object)))))
    (if (fboundp func-name)
        (funcall func-name n)
      (error "Undefined funtion: %s" (concat "emmo-mark-around-" (symbol-name object))))))

(defun emmo-act (action scope object &optional n)
  (interactive "p")
  (let ((cur (point))
        (current-line (line-number-at-pos)))
    (emmo-mark-around-object object n)
    (cond
     ;; inside
     ((eq scope 'inside)
      (cond
       ;; ((member object '(word symbol))
       ;;  ;; do nothing for now, word and symbol need special processing
       ;;  (ignore))
       ((member object '(round-bracket square-bracket curly-bracket angle-bracket single-quote double-quote))
        ;; do nothing for now, word and symbol need special processing
        (progn
          (emmo-skip-whitespaces-forward t)
          (exchange-point-and-mark)
          (emmo-skip-whitespaces-backward t)
          (exchange-point-and-mark)
          ))
       ;; default
       (t (progn
            (emmo-skip-whitespaces-forward nil)
            (exchange-point-and-mark)
            (emmo-skip-whitespaces-backward nil)
            (exchange-point-and-mark)
            ))
       )
      )
     ;; beggining
     ((eq scope 'beg)
      (emmo-skip-whitespaces-forward nil)
      (set-mark cur)
      (exchange-point-and-mark)
      )
     ;; end
     ((eq scope 'end)
      (exchange-point-and-mark)
      (emmo-skip-whitespaces-backward nil)
      (exchange-point-and-mark)
      (goto-char cur)
      )
     ;; around
     ((eq scope 'around) t)
     )
    (cond
     ;; copy
     ((eq action 'copy)
      (emmo-kill-ring-flash-save (point) (mark))
      (goto-char cur))
     ;; delete
     ((eq action 'delete)
      (delete-region (point) (mark)))
     ;; kill
     ((eq action 'kill)
      (kill-region (point) (mark)))
     ;; duplicate / xerox
     ((eq action 'duplicate)
      (exchange-point-and-mark)
      (duplicate-dwim)
      (deactivate-mark)
      )
     ;; comment
     ((eq action 'comment)
      (comment-or-uncomment-region-or-line))
     ;; indent
     ((eq action 'indent)
      (indent-region (point) (mark))
      (goto-line current-line)
      (indent-for-tab-command)
      )
     ;; go
     ((eq action 'go)
      (exchange-point-and-mark)
      (keyboard-escape-quit)
      )
     ;; surround
     ((eq action 'surround)
      (let ((mark-point (mark)))
        (keyboard-escape-quit)
        (let ((char (read-char)))
          (insert-char char)
          (goto-char (+ mark-point 1))
          (insert-char (emmo-matching-pair-char char))
          (goto-char (+ cur 1))
          ))
      )
     ;; mark
     ((eq action 'mark) t)
     )
    )
  )

(defvar read-only-keymap-mode-map (make-sparse-keymap))

(define-minor-mode read-only-keymap-mode
  "Special-Keymap mode activates the keymap of `special-mode'."
  :init-value nil
  :keymap read-only-keymap-mode-map)


;; Define the macro to set keybindings
(defmacro bind-em-action (action scope object)
  `(let ((action-char (cdr (assoc ,action emmo-actions-key-alist)))
         (scope-char (cdr (assoc ,scope emmo-scopes-key-alist)))
         (object-char (cdr (assoc ,object emmo-objects-key-alist))))
     (when (and action-char scope-char object-char)
       (global-set-key (kbd (concat "C-c " action-char scope-char object-char))
                       (lambda (&optional n) (interactive "p") (unless n (setq n 1)) (emmo-act ,action ,scope ,object n))
                       )
       ;; (define-key read-only-keymap-mode-map (kbd (concat action-char scope-char object-char))
       ;;             (lambda (&optional n) (interactive "p") (unless n (setq n 1)) (emmo-act ,action ,scope ,object n))
       ;;             )
       )
     )
  )

(macroexpand '(bind-em-action 'copy 'inside 'word))

;; Define the key binding using the macro
(defmacro emmo-define-act (action scope object)
  `(lambda (&optional n)
     (interactive "p")
     (unless n (setq n 1))
     (emmo-act ,action ,scope ,object n)))

;; (defun emmo-copy-inside-word ()
;;   (interactive)
;;   (emmo-act 'copy 'inside 'word)
;; )

; Set keybindings dynamically for each combination of action, scope, and object
(defun emmo-init ()
  (interactive)
  (dolist (action-entry emmo-actions-key-alist)
    (dolist (scope-entry emmo-scopes-key-alist)
      (dolist (object-entry emmo-objects-key-alist)
        (let ((action (car action-entry))
              (scope (car scope-entry))
              (object (car object-entry)))
          (bind-em-action action scope object)))))

  ;; @todo manually define extra keybindings
  ;; should be covered after new scopes (begin and end) are added
  (global-set-key (kbd "C-c w t") 'copy-up-to-char)
  (global-set-key (kbd "C-c w f") 'copy-to-char)
  (global-set-key (kbd "C-c k t") 'kill-up-to-char)
  (global-set-key (kbd "C-c k f") 'kill-to-char)
  (global-set-key (kbd "C-c w w") (emmo-define-act 'copy 'inside 'word))
  (global-set-key (kbd "C-c w s") (emmo-define-act 'copy 'inside 'symbol))
  (global-set-key (kbd "C-c w l") (emmo-define-act 'copy 'around 'line))
  (global-set-key (kbd "C-c w p") (emmo-define-act 'copy 'around 'paragraph))
  (global-set-key (kbd "C-c k w") (emmo-define-act 'kill 'inside 'word))
  (global-set-key (kbd "C-c k s") (emmo-define-act 'kill 'inside 'symbol))
  (global-set-key (kbd "C-c k l") (emmo-define-act 'kill 'around 'line))
  (global-set-key (kbd "C-c k p") (emmo-define-act 'kill 'around 'paragraph))
  (global-set-key (kbd "C-c k k") (emmo-define-act 'kill 'around 'line))
  (global-set-key (kbd "C-c d w") (emmo-define-act 'delete 'inside 'word))
  (global-set-key (kbd "C-c d s") (emmo-define-act 'delete 'inside 'symbol))
  (global-set-key (kbd "C-c d p") (emmo-define-act 'delete 'around 'paragraph))
  (global-set-key (kbd "C-c d d") (emmo-define-act 'delete 'around 'line))
  (global-set-key (kbd "C-c c p") (emmo-define-act 'comment 'inside 'paragraph))
  (global-set-key (kbd "C-c c c") (emmo-define-act 'comment 'inside 'paragraph))
  (global-set-key (kbd "C-c i p") (emmo-define-act 'indent 'inside 'paragraph))
  (global-set-key (kbd "C-c i f") (emmo-define-act 'indent 'inside 'function))
  ;; (global-set-key (kbd "C-c i b") (emmo-define-act 'indent 'around 'buffer))
  (global-set-key (kbd "C-c x x") 'duplicate-dwim)
  (global-set-key (kbd "C-c x p") (emmo-define-act 'duplicate 'around 'paragraph))
  )

(provide 'emmo)
;;; emmo.el ends here
