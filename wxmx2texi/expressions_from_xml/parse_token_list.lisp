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
    (mread *standard-input* nil))) ;; doesn't actually use *STANDARD-INPUT*, but otherwise complains
