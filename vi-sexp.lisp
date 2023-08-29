(defpackage :lem-vi-sexp
  (:use :cl
        :lem)
  (:import-from :lem-vi-mode
                :*normal-keymap*
                :*insert-keymap*
                :change-state
                :insert
                :define-text-object-command)
  (:import-from :lem-vi-mode/core
                :make-range
                :range-beginning
                :range-end)
  (:import-from :lem-vi-mode/states
                :*inner-text-objects-keymap*
                :*outer-text-objects-keymap*)
  (:import-from :lem-vi-mode/commands
                :vi-move-to-matching-paren
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
                :paredit-splice)
  (:export :vi-sexp
           :add-vi-sexp-mapping))
(in-package :lem-vi-sexp)

(declaim (ftype (function ()) add-vi-sexp-mapping))

(define-minor-mode vi-sexp
    (:name "vi-sexp"
     :keymap *vi-sexp-keymap*)
  (add-vi-sexp-mapping))

(define-command vi-sexp-move-to-prev-bracket (n) ("p")
  (dotimes (i n)
    (lem:backward-up-list (current-point))))

(define-command vi-sexp-move-to-next-bracket (n) ("p")
  (dotimes (i n)
    (lem:forward-up-list (current-point))))

(defun vi-sexp-wrap-round-at (insert-at)
  (with-point ((p (current-point)))
    (unless (bolp p)
      (let ((c (character-at p -1)))
        (when (syntax-closed-paren-char-p c)
          (vi-move-to-matching-paren))
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

(define-text-object-command vi-sexp-an-element () ()
  (if (member (character-at (current-point)) '(#\( #\)))
      (vi-a-paren 1)
      (vi-a-word 1)))

(define-text-object-command vi-sexp-inner-element () ()
  (if (member (character-at (current-point)) '(#\( #\)))
      (vi-inner-paren 1)
      (vi-inner-word 1)))

(define-text-object-command vi-sexp-a-toplevel-form () ()
  (with-point ((beg (current-point)))
    (handler-case
        (loop (backward-up-list beg))
      (editor-error ()))
    (when (char= (character-at beg) #\()
      (with-point ((end beg))
        (form-offset end 1)
        (make-range beg end)))))

(define-text-object-command vi-sexp-inner-toplevel-form () ()
  (let ((range (vi-sexp-a-toplevel-form)))
    (when range
      (character-offset (range-beginning range) 1)
      (character-offset (range-end range) -1)
      range)))

(define-command vi-sexp-indent-toplevel () ()
  (let ((range (vi-sexp-a-toplevel-form)))
    (when range
      (indent-points (range-beginning range)
                     (range-end range)))))

(define-command vi-sexp-insert-head () ()
  (backward-up-list (current-point))
  (forward-char)
  (insert-character (current-point) #\Space)
  (backward-char)
  (change-state 'insert))

(define-command vi-sexp-insert-tail () ()
  (forward-up-list (current-point))
  (change-state 'insert))

(define-command vi-sexp-raise-form () ()
  (backward-up-list (current-point))
  (paredit-raise))

(defun add-vi-sexp-mapping ()
  (define-key *normal-keymap* "Space @" 'paredit-splice)
  (define-key *normal-keymap* "Space w" 'vi-sexp-wrap-round)
  (define-key *normal-keymap* "Space W" 'vi-sexp-wrap-round-after)
  (define-key *normal-keymap* "Space i" 'vi-sexp-round-head-wrap-list)
  (define-key *normal-keymap* "Space I" 'vi-sexp-round-tail-wrap-list)
  (define-key *normal-keymap* "(" 'vi-sexp-move-to-prev-bracket)
  (define-key *normal-keymap* ")" 'vi-sexp-move-to-next-bracket)
  (define-key *normal-keymap* "M-b" 'paredit-backward)
  (define-key *normal-keymap* "M-w" 'paredit-forward)
  (define-key *normal-keymap* "= -" 'vi-sexp-indent-toplevel)
  (define-key *normal-keymap* "Space o" 'vi-sexp-raise-form)
  (define-key *normal-keymap* "Space O" 'paredit-raise)
  (define-key *normal-keymap* "Space h" 'vi-sexp-insert-head)
  (define-key *normal-keymap* "Space l" 'vi-sexp-insert-tail)
  (define-key *normal-keymap* "M-S-h" 'paredit-barf)
  (define-key *normal-keymap* "M-S-l" 'paredit-slurp)
  (define-key *insert-keymap* "(" 'paredit-insert-paren)
  (define-key *insert-keymap* ")" 'paredit-close-parenthesis)
  (define-key *insert-keymap* "\"" 'paredit-insert-doublequote)
  (define-key *outer-text-objects-keymap* "f" 'vi-a-paren)
  (define-key *inner-text-objects-keymap* "f" 'vi-inner-paren)
  (define-key *outer-text-objects-keymap* "F" 'vi-sexp-a-toplevel-form)
  (define-key *inner-text-objects-keymap* "F" 'vi-sexp-inner-toplevel-form)
  (define-key *outer-text-objects-keymap* "e" 'vi-sexp-an-element)
  (define-key *inner-text-objects-keymap* "e" 'vi-sexp-inner-element)
  (define-key *outer-text-objects-keymap* "s" 'vi-a-double-quote)
  (define-key *inner-text-objects-keymap* "s" 'vi-inner-double-quote)
  (values))
