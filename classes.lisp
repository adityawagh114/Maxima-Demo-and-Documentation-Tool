;; Here Classes are created to store the properties of the XML. Data of the parsed XML is stored here.

;; attribute defines the name and the value  
(defclass attribute ()
  ((name :accessor attribute-name)
   (value :accessor attribute-value)))
  


;;   The line contains the mathData(various equations/definations) 
(defclass line()
  ((mathData :accessor line-mathData)))



;; input has its attributes and lines. Dynamic arrays of both the properties are created. 
(defclass input()
  ((attributes 
    :accessor input-attributes
    :initform (map-into (make-array 0 :fill-pointer 0 :adjustable t) (lambda () (make-instance 'attribute))))

    (lines 
    :accessor input-lines
    :initform (map-into (make-array 0 :fill-pointer 0 :adjustable t) (lambda () (make-instance 'line))))))



;; editor has its attributes and lines. Dynamic arrays of both the properties are created.
(defclass editor()
 ((attributes 
    :accessor editor-attributes
    :initform (map-into (make-array 0 :fill-pointer 0 :adjustable t) (lambda () (make-instance 'attribute))))

   (lines 
    :accessor editor-lines
    :initform (map-into (make-array 0 :fill-pointer 0 :adjustable t) (lambda () (make-instance 'line))))))    



;;   A cell contains attributes,editors as well as inputs. Dynamic arrays of the properties are created.
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



;; The documnet acts as the root node of the XML. It conatins its attributes and cells. 
(defclass document ()
  ((attributes
    :accessor document-attributes
    :initform (map-into (make-array 0 :fill-pointer 0 :adjustable t) (lambda () (make-instance 'attribute))))
   (cells
    :accessor document-cells
    :initform (map-into (make-array 0 :fill-pointer 0 :adjustable t) (lambda () (make-instance 'cell))))))
