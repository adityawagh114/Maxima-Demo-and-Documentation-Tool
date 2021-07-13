;;This a a modified version of the pacakge mathml.lisp

(declaim (sb-ext:muffle-conditions cl:warning))


(in-package :maxima)
;; wmathml-printing
;; Created by David Drysdale (DMD), December 2002/January 2003
;;
;; closely based on the original TeX conversion code in mactex.lisp, 
;; for which the following credits apply:
;;   (c) copyright 1987, Richard J. Fateman
;;   small corrections and additions: Andrey Grozin, 2001
;;   additional additions: Judah Milgram (JM), September 2001
;;   additional corrections: Barton Willis (BLW), October 2001

;; Usage: wmathml(d8,"/tmp/foo.xml"); wmathml(d10,"/tmp/foo.xml"); ..
;; to append lines d8 and d10 to the wmathml file.  If given only
;; one argument the result goes to standard output.

;; Method:

;; Producing wmathml from a macsyma internal expression is done by
;; a reversal of the parsing process.  Fundamentally, a
;; traversal of the expression tree is produced by the program,
;; with appropriate substitutions and recognition of the
;; infix / prefix / postfix / matchfix relations on symbols. Various
;; changes are made to this so that wmathml will like the results.

;;  Instructions:
;; in macsyma, type wmathml(<expression>);  or wmathml(<label>); or
;; wmathml(<expr-or-label>, <file-name>);  In the case of a label,
;; an equation-number will also be produced.
;; in case a file-name is supplied, the output will be sent
;; (perhaps appended) to that file.

(macsyma-module wmathml)

(declare-top (special lop rop ccol $gcprint texport $labels $inchar vaxima-main-dir))

;; top level command the result of converting the expression x.

(defmspec $wmathml(l) ;; mexplabel, and optional filename
  ;;if filename supplied but 'nil' then return a string
  (let ((args (cdr l)))
    (cond ((and (cdr args) (null (cadr args)))
	   (let ((*standard-output* (make-string-output-stream)))
	     (apply 'wmathml1  args)
	     (get-output-stream-string *standard-output*)
	     )
	   )
	  (t (apply 'wmathml1  args)))))
      

(defun wmathml1 (mexplabel &optional filename ) ;; mexplabel, and optional filename
  (prog (mexp  texport $gcprint ccol x y itsalabel tmpport)
	;; $gcprint = nil turns gc messages off
	(setq ccol 1)
	(cond ((null mexplabel)
	       (displa " No eqn given to wmathml")
	       (return nil)))
	;; collect the file-name, if any, and open a port if needed
	(setq texport (cond((null filename) *standard-output* ); t= output to terminal
			   (t
			     (open (string (stripdollar filename))
				   :direction :output
				   :if-exists :append
				   :if-does-not-exist :create))))
	;; go back and analyze the first arg more thoroughly now.
	;; do a normal evaluation of the expression in macsyma
	(setq mexp (meval mexplabel))
	(cond ((member mexplabel $labels :test #'eq); leave it if it is a label
	       (setq mexplabel (intern (format nil "(~a)" (stripdollar mexplabel))))
	       (setq itsalabel t))
	      (t (setq mexplabel nil)));flush it otherwise

	;; maybe it is a function?
	(cond((symbolp (setq x mexp)) ;;exclude strings, numbers
	      (setq x ($verbify x))
	      (cond ((setq y (mget x 'mexpr))
		     (setq mexp (list '(mdefine) (cons (list x) (cdadr y)) (caddr y))))
		    ((setq y (mget x 'mmacro))
		     (setq mexp (list '(mdefmacro) (cons (list x) (cdadr y)) (caddr y))))
		    ((setq y (mget x 'aexpr))
		     (setq mexp (list '(mdefine) (cons (list x 'array) (cdadr y)) (caddr y)))))))
	(cond ((and (null (atom mexp))
		    (member (caar mexp) '(mdefine mdefmacro) :test #'eq))
	       (format texport "<pre>~%" ) 
	       (cond (mexplabel (format texport "~a " mexplabel)))
               ;; need to get rid of "<" signs
               (setq tmpport (make-string-output-stream))
               (mgrind mexp tmpport)
               (close tmpport)
               (format texport "~a" 
                       (string-substitute "&lt;" #\< (get-output-stream-string tmpport)))
	       (format texport ";~%</pre>"))

	      ((and itsalabel ;; but is it a user-command-label?
                  (char= (char (string $inchar) 1) (char (string mexplabel) 1)))
	       ;; aha, this is a C-line: do the grinding:
	       (format texport "<pre>~%~a " mexplabel)
               ;; need to get rid of "<" signs
               (setq tmpport (make-string-output-stream))
               (mgrind mexp tmpport)
               (close tmpport)
               (format texport "~a" 
                       (string-substitute "&lt;" #\< (get-output-stream-string tmpport)))
	       (format texport ";~%</pre>"))

	      (t ; display the expression for wmathml now:
		 (myprinc "" texport)
		 (mapc #'(lambda (x) (myprinc x texport))
		       ;;initially the left and right contexts are
		       ;; empty lists, and there are implicit parens
		       ;; around the whole expression
		       (wmathml mexp nil nil 'mparen 'mparen))
		 (cond (mexplabel
			(format texport "<mspace width=\"verythickmathspace\"/> <mtext>~a</mtext> " mexplabel)))
		 (format texport "")))
	(cond(filename(terpri texport); and drain port if not terminal
		      (close texport)))
	(return mexplabel)))

(defun wmathml (x l r lop rop)
	;; x is the expression of interest; l is the list of strings to its
	;; left, r to its right. lop and rop are the operators on the left
	;; and right of x in the tree, and will determine if parens must
	;; be inserted
	(setq x (nformat x))
	(cond ((atom x) (wmathml-atom x l r))
	      ((or (<= (wmathml-lbp (caar x)) (wmathml-rbp lop)) 
                   (> (wmathml-lbp rop) (wmathml-rbp (caar x))))
	       (wmathml-paren x l r))
	      ;; special check needed because macsyma notates arrays peculiarly
	      ((member 'array (cdar x) :test #'eq) (wmathml-array x l r))
	      ;; dispatch for object-oriented wmathml-ifiying
	      ((get (caar x) 'wmathml) (funcall (get (caar x) 'wmathml) x l r))
	      (t (wmathml-function x l r nil))))

(defun string-substitute (newstring oldchar x &aux matchpos)
  (setq matchpos (position oldchar x))
  (if (null matchpos) x
    (concatenate 'string 
                 (subseq x 0 matchpos)
                 newstring
                 (string-substitute newstring oldchar (subseq x (1+ matchpos))))))

;;; NOTE that we try to include spaces after closing tags (e.g. "</mwhatever> ")
;;; so that the line breaking algorithm in myprinc has some spaces where it
;;; can choose to line break.

;;; First we have the functions which are called directly by wmathml and its
;;; descendents

(defun wmathml-atom (x l r) 
  (append l
	  (list (cond ((numberp x) (wmathmlnumformat x))
                      ((stringp x) (format nil "<mtext>~a</mtext>" x))
		      ((and (symbolp x) (get x 'wmathmlword)))
		      (t (wmathml-stripdollar x))))
	  r))

(defun wmathmlnumformat(atom)
  (let (r firstpart exponent)
    (cond ((integerp atom)
           (strcat "<mn>" (format nil "~d" atom) "</mn> "))
	  (t
	   (setq r (explode atom))
	   (setq exponent (member 'e r :test #'string-equal));; is it ddd.ddde+EE
	   (cond ((null exponent)
                  (strcat "<mn>" (format nil "~a" (implode (exploden atom))) "</mn> "))
		 (t
		  (setq firstpart
			(nreverse (cdr (member 'e (reverse r) :test #'string-equal))))
		  (strcat 
                   "<mrow><mn>"
                   (apply #'strcat firstpart)
                   "</mn><mo>&times;</mo> <msup><mn>10</mn><mn>"
                   (apply #'strcat (cdr exponent))
                   "</mn></msup> </mrow> ")
                  ))))))

(defun wmathml-stripdollar(sym)
  (or (symbolp sym) 
      (return-from wmathml-stripdollar sym))
  (let* ((pname (maybe-invert-string-case (string-left-trim '(#\$) (symbol-name sym))))
	 (l (length pname))
	 (begin-sub
	  (loop for i downfrom (1- l)
		 when (not (digit-char-p (aref pname i)))
		 do (return (1+ i)))))
    (cond ((< begin-sub l) ;; need to do subscripting
           (strcat "<msub><mi>" 
                   (subseq pname 0 begin-sub)
                   "</mi> <mn>" 
                   (subseq pname begin-sub l)
                   "</mn></msub> "))
          (t ;; no subscripting needed
           (strcat "<mi>" pname "</mi> ")))))

(defun wmathml-paren (x l r)
  (wmathml x (append l '("<mfenced separators=\"\">")) (cons "</mfenced> " r) 'mparen 'mparen))

(defun wmathml-array (x l r)
  (let ((f))
    (if (eq 'mqapply (caar x))
	(setq f (cadr x)
	      x (cdr x)
	      l (wmathml f (append l (list "<mfenced separators=\",\">")) 
                        (list "</mfenced> ") 'mparen 'mparen))
      (setq f (caar x)
	    l (wmathml (wmathmlword f) (append l '("<msub><mrow>")) nil lop 'mfunction)))
    (setq
     r (nconc (wmathml-list (cdr x) nil (list "</mrow></msub> ") "<mo>,</mo>") r))
    (nconc l (list "</mrow><mrow>") r  )))

;; set up a list , separated by symbols (, * ...)  and then tack on the
;; ending item (e.g. "]" or perhaps ")"
(defun wmathml-list (x l r sym)
  (if (null x) r
      (do ((nl))
	  ((null (cdr x))
	   (setq nl (nconc nl (wmathml (car x)  l r 'mparen 'mparen)))
	   nl)
	  (setq nl (nconc nl (wmathml (car x)  l (list sym) 'mparen 'mparen))
		  x (cdr x)
		  l nil))))

;; we could patch this so sin x rather than sin(x), but instead we made sin a prefix
;; operator
(defun wmathml-function (x l r op) op
  (setq l (wmathml (wmathmlword (caar x)) l nil 'mparen 'mparen)
        r (wmathml (cons '(mprogn) (cdr x)) nil r 'mparen 'mparen))
  (nconc l r))

;;; Now we have functions which are called via property lists

(defun wmathml-prefix (x l r)
  (wmathml (cadr x) (append l (wmathmlsym (caar x))) r (caar x) rop))

(defun wmathml-infix (x l r)
  ;; check for 2 args
  (if (or (null (cddr x)) (cdddr x)) (wna-err (caar x)))
  (setq l (wmathml (cadr x) l nil lop (caar x)))
  (wmathml (caddr x) (append l (wmathmlsym (caar x))) r (caar x) rop))

(defun wmathml-postfix (x l r)
  (wmathml (cadr x) l (append (wmathmlsym (caar x)) r) lop (caar x)))

(defun wmathml-nary (x l r)
  (let* ((op (caar x)) (sym (wmathmlsym op)) (y (cdr x)) (ext-lop lop) (ext-rop rop))
    (cond ((null y)       (wmathml-function x l r t)) ; this should not happen
          ((null (cdr y)) (wmathml-function x l r t)) ; this should not happen, too
          (t (do ((nl) (lop ext-lop op) (rop op (if (null (cdr y)) ext-rop op)))
                 ((null (cdr y)) (setq nl (nconc nl (wmathml (car y)  l r lop rop))) nl)
	         (setq nl (nconc nl (wmathml (car y)  l (list sym)   lop rop))
		       y (cdr y)
		       l nil))))))

(defun wmathml-nofix (x l r) (wmathml (caar x) l r (caar x) rop))

(defun wmathml-matchfix (x l r)
  (setq l (append l (car (wmathmlsym (caar x))))
	;; car of wmathmlsym of a matchfix operator is the lead op
	r (append (cdr (wmathmlsym (caar x))) r)
	;; cdr is the trailing op
	x (wmathml-list (cdr x) nil r "<mo>,</mo>")) 
  (append l x))

(defun wmathmlsym (x) (or (get x 'wmathmlsym) (get x 'strsym)(get x 'dissym)
		      (stripdollar x)))

(defun wmathmlword (x)(or (get x 'wmathmlword) (stripdollar x)))

(defprop bigfloat wmathml-bigfloat wmathml)

(defun wmathml-bigfloat (x l r) 
  (let ((formatted (fpformat x)))
    (if (or (find '|b| formatted) (find '|B| formatted))
      (let*
        ((spell-out-expt
           (append
             '("<mrow><msub><mn>")
             (apply #'append
                    (mapcar
                     #'(lambda (e) (if (or (eq e '|b|) (eq e '|B|))
                                       '("</mn><mi>B</mi></msub>"
                                         "<mo>&times;</mo>"
                                         "<msup><mn>10</mn><mn>")
                                       (list e)))
                      formatted))
             '("</mn></msup></mrow>"))))
        (append l spell-out-expt r))
      (append l formatted r))))

(defprop mprog "<mi>block</mi><mspace width=\"mediummathspace\"/> " wmathmlword) 
(defprop %erf "<mi>erf</mi> " wmathmlword)
(defprop $erf "<mi>erf</mi> " wmathmlword) ;; etc for multicharacter names
(defprop $true  "<mi>true</mi> "  wmathmlword)
(defprop $false "<mi>false</mi> " wmathmlword)

(defprop mprogn wmathml-matchfix wmathml) ;; mprogn is (<progstmnt>, ...)
(defprop mprogn (("<mfenced separators=\"\">") "</mfenced> ") wmathmlsym)

(defprop mlist wmathml-matchfix wmathml)
(defprop mlist (("<mfenced separators=\"\" open=\"[\" close=\"]\">")"</mfenced> ") wmathmlsym)

;;absolute value
(defprop mabs wmathml-matchfix wmathml)
(defprop mabs (("<mfenced separators=\"\" open=\"|\" close=\"|\">")"</mfenced> ") wmathmlsym) 

(defprop mqapply wmathml-mqapply wmathml)

(defun wmathml-mqapply (x l r)
  (setq l (wmathml (cadr x) l (list "(" ) lop 'mfunction)
	r (wmathml-list (cddr x) nil (cons ")" r) "<mo>,</mo>"))
  (append l r));; fixed 9/24/87 RJF

(defprop $%i "<mi>&ImaginaryI;</mi> " wmathmlword)
(defprop $%pi "<mi>&pi;</mi> " wmathmlword)
(defprop $%e "<mi>&ExponentialE;</mi> " wmathmlword)
(defprop $inf "<mi>&infin;</mi> " wmathmlword)
(defprop $minf "<mi>-&infin;</mi> " wmathmlword)
(defprop %laplace "<mo>&Laplacetrf;</mo>" wmathmlword)
(defprop $alpha "<mi>&alpha;</mi> " wmathmlword)
(defprop $beta "<mi>&beta;</mi> " wmathmlword)
(defprop $gamma "<mi>&gamma;</mi> " wmathmlword)
(defprop %gamma "<mi>&Gamma;</mi> " wmathmlword)
(defprop $delta "<mi>&delta;</mi> " wmathmlword)
(defprop $epsilon "<mi>&epsilon;</mi> " wmathmlword)
(defprop $zeta "<mi>&zeta;</mi> " wmathmlword)
(defprop $eta "<mi>&eta;</mi> " wmathmlword)
(defprop $theta "<mi>&theta;</mi> " wmathmlword)
(defprop $iota "<mi>&iota;</mi> " wmathmlword)
(defprop $kappa "<mi>&kappa;</mi> " wmathmlword)
;(defprop $lambda "<mi>&lambda;</mi> " wmathmlword)
(defprop $mu "<mi>&mu;</mi> " wmathmlword)
(defprop $nu "<mi>&nu;</mi> " wmathmlword)
(defprop $xi "<mi>&xi;</mi> " wmathmlword)
(defprop $pi "<mi>&pi;</mi> " wmathmlword)
(defprop $rho "<mi>&rho;</mi> " wmathmlword)
(defprop $sigma "<mi>&sigma;</mi> " wmathmlword)
(defprop $tau "<mi>&tau;</mi> " wmathmlword)
(defprop $upsilon "<mi>&upsilon;</mi> " wmathmlword)
(defprop $phi "<mi>&phi;</mi> " wmathmlword)
(defprop $chi "<mi>&chi;</mi> " wmathmlword)
(defprop $psi "<mi>&psi;</mi> " wmathmlword)
(defprop $omega "<mi>&omega;</mi> " wmathmlword)

(defprop mquote wmathml-prefix wmathml)
(defprop mquote ("<mo>'</mo>") wmathmlsym)
(defprop mquote 201. wmathml-rbp)

(defprop msetq wmathml-infix wmathml)
(defprop msetq ("<mo>:</mo>") wmathmlsym)
(defprop msetq 180. wmathml-rbp)
(defprop msetq 20. wmathml-rbp)

(defprop mset wmathml-infix wmathml)
(defprop mset ("<mo>::</mo>") wmathmlsym)
(defprop mset 180. wmathml-lbp)
(defprop mset 20. wmathml-rbp)

(defprop mdefine wmathml-infix wmathml)
(defprop mdefine ("<mo>:=</mo>") wmathmlsym)
(defprop mdefine 180. wmathml-lbp)
(defprop mdefine 20. wmathml-rbp)

(defprop mdefmacro wmathml-infix wmathml)
(defprop mdefmacro ("<mo>::=</mo>") wmathmlsym)
(defprop mdefmacro 180. wmathml-lbp)
(defprop mdefmacro 20. wmathml-rbp)

(defprop marrow wmathml-infix wmathml)
(defprop marrow ("<mo>&rightarrow;</mo>") wmathmlsym)
(defprop marrow 25 wmathml-lbp)
(defprop marrow 25 wmathml-rbp)

(defprop mfactorial wmathml-postfix wmathml)
(defprop mfactorial ("<mo>!</mo>") wmathmlsym)
(defprop mfactorial 160. wmathml-lbp)

(defprop mexpt wmathml-mexpt wmathml)
(defprop mexpt 140. wmathml-lbp)
(defprop mexpt 139. wmathml-rbp)

(defprop %sum 110. wmathml-rbp)  ;; added by BLW, 1 Oct 2001
(defprop %product 115. wmathml-rbp) ;; added by BLW, 1 Oct 2001

;; insert left-angle-brackets for mncexpt. a^<n> is how a^^n looks.
(defun wmathml-mexpt (x l r)
  (let((nc (eq (caar x) 'mncexpt))); true if a^^b rather than a^b
     ;; here is where we have to check for f(x)^b to be displayed
     ;; as f^b(x), as is the case for sin(x)^2 .
     ;; which should be sin^2 x rather than (sin x)^2 or (sin(x))^2.
     ;; yet we must not display (a+b)^2 as +^2(a,b)...
     ;; or (sin(x))^(-1) as sin^(-1)x, which would be arcsine x
     (cond ;; this whole clause
	   ;; should be deleted if this hack is unwanted and/or the
	   ;; time it takes is of concern.
	   ;; it shouldn't be too expensive.
	   ((and (eq (caar x) 'mexpt) ; don't do this hack for mncexpt
		 (let*
		  ((fx (cadr x)); this is f(x)
		   (f (and (not (atom fx)) (atom (caar fx)) (caar fx))) ; this is f [or nil]
		   (bascdr (and f (cdr fx))) ; this is (x) [maybe (x,y..), or nil]
		   (expon (caddr x)) ;; this is the exponent
		   (doit (and
			  f ; there is such a function
			  (member (get-first-char f) '(#\% #\$) :test #'char=) ;; insist it is a % or $ function
                          (not (member f '(%sum %product %derivative %integrate %at
                                         %lsum %limit) :test #'eq)) ;; what else? what a hack...
			  (or (and (atom expon) (not (numberp expon))) ; f(x)^y is ok
			      (and (atom expon) (numberp expon) (> expon 0))))))
			      ; f(x)^3 is ok, but not f(x)^-1, which could
			      ; inverse of f, if written f^-1 x
			      ; what else? f(x)^(1/2) is sqrt(f(x)), ??
		  (cond (doit
			(setq l (wmathml `((mexpt) ,f ,expon) l nil 'mparen 'mparen))
			(if (and (null (cdr bascdr))
				 (eq (get f 'wmathml) 'wmathml-prefix))
			    (setq r (wmathml (car bascdr) nil r f 'mparen))
			  (setq r (wmathml (cons '(mprogn) bascdr) nil r 'mparen 'mparen))))
		        (t nil))))) ; won't doit. fall through
      (t (setq l (wmathml (cadr x) (append l '("<msup><mrow>")) nil lop (caar x))
	       r (if (mmminusp (setq x (nformat (caddr x))))
		    ;; the change in base-line makes parens unnecessary
		    (if nc
			(wmathml (cadr x) '("</mrow> <mfenced separators=\"\" open=\"<\" close=\">\"> -")(cons "</mfenced></msup> " r) 'mparen 'mparen)
			(wmathml (cadr x) '("</mrow> <mfenced separators=\"\"> -")(cons "</mfenced></msup> " r) 'mparen 'mparen))
		    (if nc
			(wmathml x (list "</mrow> <mfenced separators=\"\" open=\"<\" close=\">\">")(cons "</mfenced></msup>" r) 'mparen 'mparen)
			(if (and (numberp x) (< x 10))
			    (wmathml x (list "</mrow> ")(cons "</msup> " r) 'mparen 'mparen)
			    (wmathml x (list "</mrow> <mrow>")(cons "</mrow></msup> " r) 'mparen 'mparen))
			)))))
      (append l r)))

(defprop mncexpt wmathml-mexpt wmathml)

(defprop mncexpt 135. wmathml-lbp)
(defprop mncexpt 134. wmathml-rbp)

(defprop mnctimes wmathml-nary wmathml)
(defprop mnctimes "<mi>&ctdot;</mi> " wmathmlsym)
(defprop mnctimes 110. wmathml-lbp)
(defprop mnctimes 109. wmathml-rbp)

(defprop mtimes wmathml-nary wmathml)
(defprop mtimes "<mspace width=\"thinmathspace\"/>" wmathmlsym)
(defprop mtimes 120. wmathml-lbp)
(defprop mtimes 120. wmathml-rbp)

(defprop %sqrt wmathml-sqrt wmathml)

(defun wmathml-sqrt(x l r)
  ;; format as \\sqrt { } assuming implicit parens for sqr grouping
  (wmathml (cadr x) (append l  '("<msqrt>")) (append '("</msqrt>") r) 'mparen 'mparen))

;; macsyma doesn't know about cube (or nth) roots,
;; but if it did, this is what it would look like.
(defprop $cubrt wmathml-cubrt wmathml)

(defun wmathml-cubrt (x l r)
  (wmathml (cadr x) (append l  '("<mroot><mrow>")) (append '("</mrow>3</mroot>") r) 'mparen 'mparen))

(defprop mquotient wmathml-mquotient wmathml)
(defprop mquotient ("<mo>/</mo>") wmathmlsym)
(defprop mquotient 122. wmathml-lbp) ;;dunno about this
(defprop mquotient 123. wmathml-rbp)

(defun wmathml-mquotient (x l r)
  (if (or (null (cddr x)) (cdddr x)) (wna-err (caar x)))
  (setq l (wmathml (cadr x) (append l '("<mfrac><mrow>")) nil 'mparen 'mparen)
	r (wmathml (caddr x) (list "</mrow> <mrow>") (append '("</mrow></mfrac> ")r) 'mparen 'mparen))
  (append l r))

(defprop $matrix wmathml-matrix wmathml)

(defun wmathml-matrix(x l r) ;;matrix looks like ((mmatrix)((mlist) a b) ...)
  (append l `("<mfenced separators=\"\" open=\"(\" close=\")\"><mtable>")
	 (mapcan #'(lambda(y)
			  (wmathml-list (cdr y) (list "<mtr><mtd>") (list "</mtd></mtr> ") "</mtd><mtd>"))
		 (cdr x))
	 '("</mtable></mfenced> ") r))

;; macsyma sum or prod is over integer range, not  low <= index <= high
;; wmathml is lots more flexible .. but

(defprop %sum wmathml-sum wmathml)
(defprop %lsum wmathml-lsum wmathml)
(defprop %product wmathml-sum wmathml)

;; easily extended to union, intersect, otherops

(defun wmathml-lsum(x l r)
  (let ((op (cond ((eq (caar x) '%lsum) "<mrow><munder><mo>&sum;</mo> <mrow>")
		  ;; extend here
		  ))
	;; gotta be one of those above 
	(s1 (wmathml (cadr x) nil nil 'mparen rop));; summand
	(index ;; "index = lowerlimit"
	       (wmathml `((min simp) , (caddr x), (cadddr x))  nil nil 'mparen 'mparen)))
       (append l `( ,op ,@index "</mrow></munder> <mrow>" ,@s1 "</mrow></mrow> ") r)))

(defun wmathml-sum(x l r)
  (let ((op (cond ((eq (caar x) '%sum) "<mrow><munderover><mo>&sum;</mo><mrow>")
		  ((eq (caar x) '%product) "<mrow><munderover><mo>&prod;</mo><mrow>")
		  ;; extend here
		  ))
	;; gotta be one of those above
	(s1 (wmathml (cadr x) nil nil 'mparen rop));; summand
	(index ;; "index = lowerlimit"
	       (wmathml `((mequal simp) ,(caddr x),(cadddr x)) nil nil 'mparen 'mparen))
	(toplim (wmathml (car(cddddr x)) nil nil 'mparen 'mparen)))
       (append l `( ,op ,@index "</mrow> <mrow>" ,@toplim "</mrow></munderover> <mrow>" ,@s1 "</mrow></mrow> ") r)))

(defprop %integrate wmathml-int wmathml)

(defun wmathml-int (x l r)
  (let ((s1 (wmathml (cadr x) nil nil 'mparen 'mparen));;integrand delims / & d
	(var (wmathml (caddr x) nil nil 'mparen rop))) ;; variable
       (cond((= (length x) 3)
	     (append l `("<mrow><mo>&int;</mo><mrow>" ,@s1 "</mrow> <mspace width=\"mediummathspace\"/> <mrow><mo>&DifferentialD;</mo><mi>" ,@var "</mi></mrow></mrow> ") r))
	    (t ;; presumably length 5
	       (let ((low (wmathml (nth 3 x) nil nil 'mparen 'mparen))
		     ;; 1st item is 0
		     (hi (wmathml (nth 4 x) nil nil 'mparen 'mparen)))
		    (append l `("<mrow><munderover><mo>&int;</mo> <mrow>" ,@low "</mrow> <mrow>" ,@hi "</mrow> </munderover> <mrow>" ,@s1 "</mrow> <mspace width=\"mediummathspace\"/> <mrow><mo>&DifferentialD;</mo><mi>" ,@var "</mi> </mrow></mrow> ") r))))))

(defprop %limit wmathml-limit wmathml)

(defprop mrarr wmathml-infix wmathml)
(defprop mrarr ("<mo>&rarr;</mo> ") wmathmlsym)
(defprop mrarr 80. wmathml-lbp)
(defprop mrarr 80. wmathml-rbp)

(defun wmathml-limit(x l r) ;; ignoring direction, last optional arg to limit
  (let ((s1 (wmathml (second x) nil nil 'mparen rop));; limitfunction
	(subfun ;; the thing underneath "limit"
         (wmathml `((mrarr simp) ,(third x) ,(fourth x)) nil nil 'mparen 'mparen)))
       (append l `("<munder><mo>lim</mo><mrow>" ,@subfun "</mrow> </munder> <mrow>" ,@s1 "</mrow>") r)))

(defprop %at wmathml-at wmathml)

;; e.g.  at(diff(f(x)),x=a)
(defun wmathml-at (x l r)
  (let ((s1 (wmathml (cadr x) nil nil lop rop))
	(sub (wmathml (caddr x) nil nil 'mparen 'mparen)))
       (append l '("<msub><mfenced separators=\"\" open=\"\" close=\"|\">") s1  '("</mfenced> <mrow>") sub '("</mrow> </msub> ") r)))

;;binomial coefficients

(defprop %binomial wmathml-choose wmathml)

(defun wmathml-choose (x l r)
  `(,@l
    "<mfenced separators=\"\" open=\"(\" close=\")\"><mtable><mtr><mtd>"
    ,@(wmathml (cadr x) nil nil 'mparen 'mparen)
    "</mtd></mtr> <mtr><mtd>"
    ,@(wmathml (caddr x) nil nil 'mparen 'mparen)
    "</mtd></mtr> </mtable></mfenced> "
    ,@r))


(defprop rat wmathml-rat wmathml)
(defprop rat 120. wmathml-lbp)
(defprop rat 121. wmathml-rbp)
(defun wmathml-rat(x l r) (wmathml-mquotient x l r))

(defprop mplus wmathml-mplus wmathml)
(defprop mplus 100. wmathml-lbp)
(defprop mplus 100. wmathml-rbp)

(defun wmathml-mplus (x l r)
 ;(declare (fixnum w))
 (cond ((member 'trunc (car x) :test #'eq)
	(setq r (cons "<mo>+</mo><mtext>&ctdot;</mtext> " r))))
 (cond ((null (cddr x))
	(if (null (cdr x))
	    (wmathml-function x l r t)
	    (wmathml (cadr x) (cons "<mo>+</mo>" l) r 'mplus rop)))
       (t (setq l (wmathml (cadr x) l nil lop 'mplus)
		x (cddr x))
	  (do ((nl l)  (dissym))
	      ((null (cdr x))
	       (if (mmminusp (car x)) (setq l (cadar x) dissym (list "<mo>-</mo> "))
		   (setq l (car x) dissym (list "<mo>+</mo> ")))
	       (setq r (wmathml l dissym r 'mplus rop))
	       (append nl r))
	      (if (mmminusp (car x)) (setq l (cadar x) dissym (list "<mo>-</mo> "))
		  (setq l (car x) dissym (list "<mo>+</mo> ")))
	      (setq nl (append nl (wmathml l dissym nil 'mplus 'mplus))
		    x (cdr x))))))

(defprop mminus wmathml-prefix wmathml)
(defprop mminus ("<mo>-</mo>") wmathmlsym)
(defprop mminus 100. wmathml-rbp)
(defprop mminus 100. wmathml-lbp)

(defprop min wmathml-infix wmathml)
(defprop min ("<mo>&isin;</mo> ") wmathmlsym)
(defprop min 80. wmathml-lbp)
(defprop min 80. wmathml-rbp)

(defprop mequal wmathml-infix wmathml)
(defprop mequal ("<mo>=</mo> ") wmathmlsym)
(defprop mequal 80. wmathml-lbp)
(defprop mequal 80. wmathml-rbp)

(defprop mnotequal wmathml-infix wmathml)
(defprop mnotequal 80. wmathml-lbp)
(defprop mnotequal 80. wmathml-rbp)

(defprop mgreaterp wmathml-infix wmathml)
(defprop mgreaterp ("<mo>&gt;</mo> ") wmathmlsym)
(defprop mgreaterp 80. wmathml-lbp)
(defprop mgreaterp 80. wmathml-rbp)

(defprop mgeqp wmathml-infix wmathml)
(defprop mgeqp ("<mo>&ge;</mo> ") wmathmlsym)
(defprop mgeqp 80. wmathml-lbp)
(defprop mgeqp 80. wmathml-rbp)

(defprop mlessp wmathml-infix wmathml)
(defprop mlessp ("<mo>&lt;</mo> ") wmathmlsym)
(defprop mlessp 80. wmathml-lbp)
(defprop mlessp 80. wmathml-rbp)

(defprop mleqp wmathml-infix wmathml)
(defprop mleqp ("<mo>&le;</mo> ") wmathmlsym)
(defprop mleqp 80. wmathml-lbp)
(defprop mleqp 80. wmathml-rbp)

(defprop mnot wmathml-prefix wmathml)
(defprop mnot ("<mo>&not;</mo> ") wmathmlsym)
(defprop mnot 70. wmathml-rbp)

(defprop mand wmathml-nary wmathml)
(defprop mand ("<mo>&and;</mo> ") wmathmlsym)
(defprop mand 60. wmathml-lbp)
(defprop mand 60. wmathml-rbp)

(defprop mor wmathml-nary wmathml)
(defprop mor ("<mo>&or;</mo> ") wmathmlsym)

;; make sin(x) display as sin x , but sin(x+y) as sin(x+y)
;; etc

(defun wmathml-setup (x)
  (let((a (car x))
       (b (cadr x)))
      (setf (get a 'wmathml) 'wmathml-prefix)
      (setf (get a 'wmathmlword) b)  ;This means "sin" will always be roman
      (setf (get a 'wmathmlsym) (list b))
      (setf (get a 'wmathml-rbp) 130)))

(mapc #'wmathml-setup
  '(
     (%acos "<mi>arccos</mi> ")
     (%asin "<mi>arcsin</mi> ")
     (%atan "<mi>arctan</mi> ")
     (%arg "<mi>arg</mi> ")
     (%cos "<mi>cos</mi> ")
     (%cosh "<mi>cosh</mi> ")
     (%cot "<mi>cot</mi> ")
     (%coth "<mi>coth</mi> ")
     (%csc "<mi>cosec</mi> ")
     (%deg "<mi>deg</mi> ")
     (%determinant "<mi>det</mi> ")
     (%dim "<mi>dim</mi> ")
     (%exp "<mi>exp</mi> ")
     (%gcd "<mi>gcd</mi> ")
     (%hom "<mi>hom</mi> ")
     (%inf "<mi>&infin;</mi> ") 
     (%ker "<mi>ker</mi> ")
     (%lg "<mi>lg</mi> ")
     ;;(%limit "<mi>lim</mi> ")
     (%liminf "<mi>lim inf</mi> ")
     (%limsup "<mi>lim sup</mi> ")
     (%ln "<mi>ln</mi> ")
     (%log "<mi>log</mi> ")
     (%max "<mi>max</mi> ")
     (%min "<mi>min</mi> ")
     ; Latex's "Pr" ... ?
     (%sec "<mi>sec</mi> ")
     (%sech "<mi>sech</mi> ")
     (%sin "<mi>sin</mi> ")
     (%sinh "<mi>sinh</mi> ")
     (%sup "<mi>sup</mi> ")
     (%tan "<mi>tan</mi> ")
     (%tanh "<mi>tanh</mi> ")
    ;; (%erf "{\\rm erf}") this would tend to set erf(x) as erf x. Unusual
     ;(%laplace "{\\cal L}")
     )) ;; etc

(defprop mor wmathml-nary wmathml)
(defprop mor 50. wmathml-lbp)
(defprop mor 50. wmathml-rbp)

(defprop mcond wmathml-mcond wmathml)
(defprop mcond 25. wmathml-lbp)
(defprop mcond 25. wmathml-rbp)

(defprop %derivative wmathml-derivative wmathml)

(defun wmathml-derivative (x l r)
  (wmathml (wmathml-d x "&DifferentialD;") l r lop rop ))

(defun wmathml-d(x dsym) ;dsym should be "&DifferentialD;" or "&PartialD;"
  ;; format the macsyma derivative form so it looks
  ;; sort of like a quotient times the deriva-dand.
  (let*
   ((arg (cadr x)) ;; the function being differentiated
    (difflist (cddr x)) ;; list of derivs e.g. (x 1 y 2)
    (ords (odds difflist 0)) ;; e.g. (1 2)
    (vars (odds difflist 1)) ;; e.g. (x y)
    (numer `((mexpt) ,dsym ((mplus) ,@ords))) ; d^n numerator
    (denom (cons '(mtimes)
		 (mapcan #'(lambda(b e)
				  `(,dsym ,(simplifya `((mexpt) ,b ,e) nil)))
			 vars ords))))
   `((mtimes)
     ((mquotient) ,(simplifya numer nil) ,denom)
     ,arg)))

(defmfun $wmathml1 (x) (reduce #'strcat (wmathml x nil nil 'mparen 'mparen)))


(defun wmathml-mcond (x l r)
  (append l
    (wmathml (cadr x) '("<mi>if</mi> <mspace width=\"mediummathspace\"/>")
      '("<mspace width=\"mediummathspace\"/> <mi>then</mi><mspace width=\"mediummathspace\"/> ") 'mparen 'mparen)
    (if (eql (fifth x) '$false)
      (wmathml (caddr x) nil r 'mcond rop)
      (append (wmathml (caddr x) nil nil 'mparen 'mparen)
        (wmathml (fifth x) '("<mspace width=\"mediummathspace\"/> <mi>else</mi><mspace width=\"mediummathspace\"/> ") r 'mcond rop)))))

(defprop mdo wmathml-mdo wmathml)
(defprop mdo 30. wmathml-lbp)
(defprop mdo 30. wmathml-rbp)
(defprop mdoin wmathml-mdoin wmathml)
(defprop mdoin 30. wmathml-rbp)

(defun wmathml-lbp(x)(cond((get x 'wmathml-lbp))(t(lbp x))))
(defun wmathml-rbp(x)(cond((get x 'wmathml-rbp))(t(lbp x))))

;; these aren't quite right

(defun wmathml-mdo (x l r)
  (wmathml-list (wmathmlmdo x) l r "<mspace width=\"mediummathspace\"/> "))

(defun wmathml-mdoin (x l r)
  (wmathml-list (wmathmlmdoin x) l r "<mspace width=\"mediummathspace\"/> "))

(defun wmathmlmdo (x)
   (nconc (cond ((second x) `("<mi>for</mi> " ,(second x))))
	 (cond ((equal 1 (third x)) nil)
	       ((third x)  `("<mi>from</mi> " ,(third x))))
	 (cond ((equal 1 (fourth x)) nil)
	       ((fourth x) `("<mi>step</mi> " ,(fourth x)))
	       ((fifth x)  `("<mi>next</mi> " ,(fifth x))))
	 (cond ((sixth x)  `("<mi>thru</mi> " ,(sixth x))))
	 (cond ((null (seventh x)) nil)
	       ((eq 'mnot (caar (seventh x)))
		`("<mi>while</mi> " ,(cadr (seventh x))))
	       (t `("<mi>unless</mi> " ,(seventh x))))
	 `("<mi>do</mi> " ,(eighth x))))

(defun wmathmlmdoin (x)
  (nconc `("<mi>for</mi>" ,(second x) "<mi>in</mi> " ,(third x))
	 (cond ((sixth x) `("<mi>thru</mi> " ,(sixth x))))
	 (cond ((null (seventh x)) nil)
	       ((eq 'mnot (caar (seventh x)))
		`("<mi>while</mi> " ,(cadr (seventh x))))
	       (t `("<mi>unless</mi> " ,(seventh x))))
	 `("<mi>do</mi> " ,(eighth x))))

;; Undone and trickier:
;; Maybe do some special hacking for standard notations for
;; hypergeometric fns, alternative summation notations  0<=n<=inf, etc.
