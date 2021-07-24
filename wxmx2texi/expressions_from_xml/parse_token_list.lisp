;; parse_token_list.lisp -- supply list of tokens to parser to construct Maxima expression
;; copyright 2021 by Robert Dodier
;; I release this work under terms of the GNU General Public License

;; It is assumed that the list of tokens IS NOT terminated by $;
;; therefore $; is appended to the list.

(defparameter *token-list* nil)

(let
  ((f (symbol-function 'peek-one-token))
   (g (symbol-function 'scan-one-token)))
  (defun peek-one-token ()
    (if *token-list*
      (car *token-list*)
      (funcall f)))
  (defun scan-one-token ()
    (if *token-list*
      (pop *token-list*)
      (funcall g))))

(defmfun $parse_token_list (l)
  (let ((*token-list* (append (cdr l) (list '$\;))))
    (with-input-from-string (s "0") ;; token "0" just shows there is input present; it isn't used
      (mread s))))

;; Spurious line info shows up in the values returned by this implementation.
#+nil (defmfun $parse_token_list (l)
  (let ((*token-list* (append (cdr l) (list '$\;))))
    (do
      ((input (parse '$any 0.)
              (parse '$any 0.)))
      (nil)
      (case (first-c)
        ((|$;| |$$|)
         (return (list (mheader (pop-c)) nil input)))
        (t (parse-bug-err '$parse_token_list))))))
