;; expressions_from_xml.lisp -- construct Maxima expressions from XML
;; copyright 2021 by Robert Dodier
;; I release this work under terms of the GNU General Public License

(ql:quickload :plump)

(defun $expressions_from_xml (x)
  (cond
    ((plump:textual-node-p x)
     (plump:text x))
    ((plump:nesting-node-p x)
     (let*
       ((op (mfuncall '$parse_string (plump:tag-name x)))
        (attribute-keys (loop for k being the hash-keys of (plump:attributes x) collect k))
        (attribute-values (mapcar #'(lambda (a) (plump:get-attribute x a)) attribute-keys))
        (children-list (coerce (plump:child-elements x) 'list))
        (children-exprs (mapcar '$expressions_from_xml children-list)))
       (if children-exprs
         `((mqapply) ((,op) ,@attribute-values) ,@children-exprs)
         `((mqapply) ((,op) ,@attribute-values) ,($expressions_from_xml (plump:first-child x))))))
     (t ;; Not sure what to do here.
       x)))

  (defun $read_xml (f)
  (plump:first-child (plump:parse (string f))))
