(defpackage :lem-vi-sexp
  (:use :cl
        :lem)
  (:import-from :lem-vi-mode
                :*normal-keymap*
                :*insert-keymap*
                :*inner-text-objects-keymap*
                :*outer-text-objects-keymap*
                :*operator-keymap*
                :change-state
                :insert
                :define-text-object-command)
  (:import-from :lem-vi-mode/core
                :make-range
                :range-beginning
                :range-end)
  (:import-from :lem-vi-mode/commands
                :vi-move-to-matching-item
                :vi-indent
                :vi-a-double-quote
                :vi-inner-double-quote
                :vi-a-word
                :vi-inner-word
                :vi-a-paren
                :vi-inner-paren)
  (:import-from :lem-vi-mode/commands/utils
                :bolp)
  (:import-from :lem-paredit-mode
                :paredit-insert-paren
                :paredit-close-parenthesis
                :paredit-insert-doublequote
                :paredit-wrap-round
                :paredit-forward
                :paredit-backward
                :paredit-raise
                :paredit-slurp
                :paredit-barf
                :paredit-splice
                :paredit-backward-delete
                :paredit-forward-delete)
  (:export :vi-sexp
           :add-vi-sexp-mapping
           :vi-sexp-splice
           :vi-sexp-wrap-round
           :vi-sexp-wrap-round-after
           :vi-sexp-round-head-wrap-list
           :vi-sexp-round-tail-wrap-list
           :vi-sexp-move-to-next-bracket
           :vi-sexp-move-to-prev-bracket
           :vi-sexp-backward
           :vi-sexp-forward
           :vi-sexp-indent-toplevel
           :vi-sexp-raise-form
           :vi-sexp-raise
           :vi-sexp-insert-head
           :vi-sexp-insert-tail
           :vi-sexp-barf
           :vi-sexp-slurp
           :vi-a-paren
           :vi-inner-paren
           :vi-sexp-a-toplevel-form
           :vi-sexp-inner-toplevel-form
           :vi-sexp-an-element
           :vi-sexp-inner-element
           :vi-a-double-quote
           :vi-inner-double-quote
           :vi-sexp-insert-paren
           :vi-sexp-close-parenthesis
           :vi-sexp-insert-doublequote
           :vi-sexp-backward-delete
           :vi-sexp-forward-delete))
(in-package :lem-vi-sexp)

(declaim (ftype (function ()) add-vi-sexp-mapping))

(define-minor-mode vi-sexp
    (:name "vi-sexp"
     :keymap *vi-sexp-keymap*)
  (add-vi-sexp-mapping))

(define-command vi-sexp-move-to-prev-bracket (n) ("p")
  (dotimes (i n)
    (lem:backward-up-list (current-point))))

(define-command vi-sexp-move-to-next-bracket (&optional (n 1)) ("p")
  (dotimes (i n)
    (when (eql (character-at (current-point)) #\))
      (character-offset (current-point) 1))
    (lem:forward-up-list (current-point)))
  (character-offset (current-point) -1))

(define-command vi-sexp-backward (&optional (n 1)) ("p")
  (paredit-backward n))

(define-command vi-sexp-forward (&optional (n 1)) ("p")
  (paredit-forward n))

(define-command vi-sexp-splice (&optional (n 1)) ("p")
  (dotimes (i n)
    (paredit-splice)))

(defun vi-sexp-wrap-round-at (insert-at)
  (with-point ((p (current-point)))
    (unless (bolp p)
      (let ((c (character-at p -1)))
        (when (syntax-closed-paren-char-p c)
          (vi-move-to-matching-item))
        (cond
          ((or (syntax-space-char-p c)
               (syntax-open-paren-char-p c))
           nil)
          (t (form-offset p -1)))))
    (move-point (current-point) p))
  (paredit-wrap-round)
  (ecase insert-at
    (:before
     (insert-character (current-point) #\Space)
     (backward-char))
    (:after
     (forward-sexp)))
  (change-state 'insert))

(define-command vi-sexp-wrap-round () ()
  (vi-sexp-wrap-round-at :before))

(define-command vi-sexp-wrap-round-after () ()
  (vi-sexp-wrap-round-at :after))

(define-command vi-sexp-round-head-wrap-list () ()
  (backward-up-list (current-point))
  (vi-sexp-wrap-round))

(define-command vi-sexp-round-tail-wrap-list () ()
  (forward-up-list (current-point))
  (vi-sexp-wrap-round-after))

(define-text-object-command vi-sexp-an-element () () ()
  (if (member (character-at (current-point)) '(#\( #\)))
      (vi-a-paren 1)
      (vi-a-word 1)))

(define-text-object-command vi-sexp-inner-element () () ()
  (if (member (character-at (current-point)) '(#\( #\)))
      (vi-inner-paren 1)
      (vi-inner-word 1)))

(define-text-object-command vi-sexp-a-toplevel-form () () ()
  (with-point ((beg (current-point)))
    (handler-case
        (loop (backward-up-list beg))
      (editor-error ()))
    (when (char= (character-at beg) #\()
      (with-point ((end beg))
        (form-offset end 1)
        (make-range beg end)))))

(define-text-object-command vi-sexp-inner-toplevel-form () () ()
  (let ((range (vi-sexp-a-toplevel-form)))
    (when range
      (character-offset (range-beginning range) 1)
      (character-offset (range-end range) -1)
      range)))

(define-command vi-sexp-indent-toplevel () ()
  (when (eq 'vi-indent (command-name (this-command)))
    (vi-sexp-a-toplevel-form)))

(define-command vi-sexp-insert-head () ()
  (backward-up-list (current-point))
  (forward-char)
  (insert-character (current-point) #\Space)
  (backward-char)
  (change-state 'insert))

(define-command vi-sexp-insert-tail () ()
  (forward-up-list (current-point))
  (change-state 'insert))

(define-command vi-sexp-raise-form (&optional (n 1)) ("p")
  (unless (syntax-open-paren-char-p (character-at (current-point)))
    (backward-up-list (current-point)))
  (dotimes (i n)
    (paredit-raise))
  (with-point ((end (current-point)))
    (scan-lists end 1 0 t)
    (indent-points (current-point) end)))

(define-command vi-sexp-raise (&optional (n 1)) ("p")
  (dotimes (i n)
    (paredit-raise)))

(define-command vi-sexp-barf (&optional (n 1)) ("p")
  (dotimes (i n)
    (paredit-barf)))

(define-command vi-sexp-slurp (&optional (n 1)) ("p")
  (dotimes (i n)
    (paredit-slurp)))

(define-command vi-sexp-insert-paren () ()
  (paredit-insert-paren))

(define-command vi-sexp-close-parenthesis () ()
  (paredit-close-parenthesis))

(define-command vi-sexp-insert-doublequote () ()
  (paredit-insert-doublequote))

(define-command vi-sexp-backward-delete (&optional (n 1)) ("p")
  (paredit-backward-delete n))

(define-command vi-sexp-forward-delete (&optional (n 1)) ("p")
  (paredit-forward-delete n))

(defun add-vi-sexp-mapping ()
  (define-key *normal-keymap* "Leader @" 'vi-sexp-splice)
  (define-key *normal-keymap* "Leader w" 'vi-sexp-wrap-round)
  (define-key *normal-keymap* "Leader W" 'vi-sexp-wrap-round-after)
  (define-key *normal-keymap* "Leader i" 'vi-sexp-round-head-wrap-list)
  (define-key *normal-keymap* "Leader I" 'vi-sexp-round-tail-wrap-list)
  (define-key *normal-keymap* "(" 'vi-sexp-move-to-prev-bracket)
  (define-key *normal-keymap* ")" 'vi-sexp-move-to-next-bracket)
  (define-key *normal-keymap* "M-b" 'vi-sexp-backward)
  (define-key *normal-keymap* "M-w" 'vi-sexp-forward)
  (define-key *operator-keymap* "-" 'vi-sexp-indent-toplevel)
  (define-key *normal-keymap* "Leader o" 'vi-sexp-raise-form)
  (define-key *normal-keymap* "Leader O" 'vi-sexp-raise)
  (define-key *normal-keymap* "Leader h" 'vi-sexp-insert-head)
  (define-key *normal-keymap* "Leader l" 'vi-sexp-insert-tail)
  (define-key *normal-keymap* "M-H" 'vi-sexp-barf)
  (define-key *normal-keymap* "M-L" 'vi-sexp-slurp)
  (define-key *insert-keymap* "(" 'vi-sexp-insert-paren)
  (define-key *insert-keymap* ")" 'vi-sexp-close-parenthesis)
  (define-key *insert-keymap* "\"" 'vi-sexp-insert-doublequote)
  (define-key *insert-keymap* 'delete-previous-char 'vi-sexp-backward-delete)
  (define-key *insert-keymap* 'delete-next-char 'vi-sexp-forward-delete)
  (define-key *outer-text-objects-keymap* "f" 'vi-a-paren)
  (define-key *inner-text-objects-keymap* "f" 'vi-inner-paren)
  (define-key *outer-text-objects-keymap* "F" 'vi-sexp-a-toplevel-form)
  (define-key *inner-text-objects-keymap* "F" 'vi-sexp-inner-toplevel-form)
  (define-key *outer-text-objects-keymap* "e" 'vi-sexp-an-element)
  (define-key *inner-text-objects-keymap* "e" 'vi-sexp-inner-element)
  (define-key *outer-text-objects-keymap* "s" 'vi-a-double-quote)
  (define-key *inner-text-objects-keymap* "s" 'vi-inner-double-quote)
  (values))
