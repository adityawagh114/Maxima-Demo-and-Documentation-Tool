;; attribute defines the name and the value  

(defclass attribute ()
  ((name :accessor attribute-name)
  (value :accessor attribute-value)))
  
(defclass line()
((mathData :accessor line-mathData)))

(defclass input()
  ((attributes 
    :accessor input-attributes
    :initform (map-into (make-array 0 :fill-pointer 0 :adjustable t) (lambda () (make-instance 'attribute))))

    (lines 
    :accessor input-lines
    :initform (map-into (make-array 0 :fill-pointer 0 :adjustable t) (lambda () (make-instance 'line))))))


(defclass editor()
 ((attributes 
    :accessor editor-attributes
    :initform (map-into (make-array 0 :fill-pointer 0 :adjustable t) (lambda () (make-instance 'attribute))))

   (lines 
    :accessor editor-lines
    :initform (map-into (make-array 0 :fill-pointer 0 :adjustable t) (lambda () (make-instance 'line))))))    

 
(defclass cell()
  ((attributes 
    :accessor cell-attributes
    :initform (map-into (make-array 0 :fill-pointer 0 :adjustable t) (lambda () (make-instance 'attribute))))

    (editors 
    :accessor cell-editors
    :initform (map-into (make-array 0 :fill-pointer 0 :adjustable t) (lambda () (make-instance 'editor))))

    (inputs 
    :accessor cell-inputs
    :initform (map-into (make-array 0 :fill-pointer 0 :adjustable t) (lambda () (make-instance 'input)))))) 


(defclass document ()
  ((attributes
    :accessor document-attributes
    :initform (map-into (make-array 0 :fill-pointer 0 :adjustable t) (lambda () (make-instance 'attribute))))
   (cells
    :accessor document-cells
    :initform (map-into (make-array 0 :fill-pointer 0 :adjustable t) (lambda () (make-instance 'cell))))))