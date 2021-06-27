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


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;                                                                                                            ;;
    ;;                                                                                                            ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;plump xml parser has been used https://shinmera.github.io/plump/ 
(ql:quickload :plump)

;; add the location of the content.xml to test
(defparameter root (plump:parse #P"d:/Users/ADITYA SANDEEP WAGH/portacle/content.xml"))

 ;; (plump:CHILD-ELEMENTS root) will return vector of childrens of the root
(setf document_array (plump:CHILD-ELEMENTS root))

;; the first element of the vector will be the node of the document 
(setf document (aref document_array 0) )

;; cells is a vector of all the cell in the document
(setf cells (plump:child-elements document))



;; class of document type
(setf document_object (make-instance 'document))

;; (plump:attributes document) will return a map containing  key and value pairs of the attributes
(setf attributes_map (plump:attributes document) )

;; vector to store values of attributes of document
(setf document_attribute_array  (make-array 0 :fill-pointer 0 :adjustable t) )

;; vector to store cells of the document
(setf document_cell_array  (make-array 0 :fill-pointer 0 :adjustable t) )

(maphash #'(lambda (key value) 
             
             ;;store key value in the structure attribute
             (setf item (make-instance 'attribute))
             
             (setf (attribute-name item) key)
  
             (setf (attribute-value item) value)
            ;; appending each item to document_attribute_array
             (vector-push-extend item document_attribute_array))  attributes_map)

;; storing the document_attribute_array in document_object
(setf (document-attributes document_object) document_attribute_array)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; looping the cells array
(dotimes (i  (length cells))

 
  (setf cell (aref cells i ))
  
    ;;cell_object is a instance of cell object
    (setf cell_object (make-instance 'cell))

    ;; array to store attributes of the cell
    (setf cell_attribute_array  (make-array 0 :fill-pointer 0 :adjustable t) )
    


  (setf attributes_map (plump:attributes cell) )

  (maphash #'(lambda (key value) 
               
               ;;store key value in the structure attributes

             (setf item (make-instance 'attribute))
             
             (setf (attribute-name item) key)
             

             (setf (attribute-value item) value)
           
            ;; appending each item to cell_attribute_array
             (vector-push-extend item cell_attribute_array) ) attributes_map)

         ;; storing the cell_attribute_array in cell_object
         (setf (cell-attributes cell_object) cell_attribute_array)



  
            ;; child of cell, it contains editor or input+editor      
          (setf child (plump:CHILD-ELEMENTS cell))

          ;; the first_child will contain the first child of the cell and it will be used to know whether the cell has editor or input as first child
          (setf first_child (aref child 0) ) 
  


  
   ;; (plump:tag-name first_child) it will return the tag of the XML first_child
  (if (string-equal "input" (plump:tag-name first_child))
      (  ;; if it is of type input
        
         progn
           
           ;; this is editor which is inside input 
          (setf editor (aref child 1) ) 

            ;;object of type input 
          (setf input_object (make-instance 'input))
            ;;input object array to store in cell_object
          (setf input_object_array  (make-array 0 :fill-pointer 0 :adjustable t) )
            


          ;; storing the attributes of the editor which is inside input 
          (setf attributes_map (plump:attributes editor) )
          (setf input_attribute_array  (make-array 0 :fill-pointer 0 :adjustable t) )

          (maphash #'(lambda (key value) 
                     
              ;;store key value in the structure attributes
             (setf item (make-instance 'attribute))
             (setf (attribute-name item) key)
             (setf (attribute-value item) value)

             (vector-push-extend item input_attribute_array)) attributes_map) 
        
            ;; input_attribute_array will be stored in input_object
            (setf (input-attributes input_object) input_attribute_array)




            ;; lines if a vector of all the line in the editor
            (setf lines (plump:CHILD-ELEMENTS editor)) 
        
                ;;to stores all the line of the input
            (setf input_lines_array  (make-array 0 :fill-pointer 0 :adjustable t) )

              ;; looping through the lines
            (dotimes (i  (length lines))

              (setf mathString (aref lines i ))

                  ;; store mathString in input_lines_array
                (vector-push-extend (plump:text mathstring) input_lines_array))


            ;; input_lines_array is stored in input_object
            (setf (input-lines input_object) input_lines_array)

             ;; input_object is stored in input_object_array and further it will be stored in the cell_object
            (vector-push-extend input_object input_object_array)
            (setf (cell-inputs cell_object) input_object_array)
        

        )
      ( 
        ;; else (the cell only contains the editor)
        progn

          
          ;; everthing done for editor is similar to input above
         (setf editor first_child)
         (setf editor_object (make-instance 'editor))
         (setf editor_object_array  (make-array 0 :fill-pointer 0 :adjustable t) )



         (setf editor_attribute_array  (make-array 0 :fill-pointer 0 :adjustable t) )

         (setf attributes_map (plump:attributes editor) )

         (maphash #'(lambda (key value) 
                    
                    ;;store key value in the structure attributes
             (setf item (make-instance 'attribute))
            
             (setf (attribute-name item) key)
            

             (setf (attribute-value item) value)

             (vector-push-extend item editor_attribute_array) ) attributes_map) 



        (setf (editor-attributes editor_object) editor_attribute_array)
   
        (setf lines (plump:CHILD-ELEMENTS editor))

         (setf editor_lines_array  (make-array 0 :fill-pointer 0 :adjustable t) )


        (dotimes (i  (length lines))

          (setf mathString (aref lines i ))

              (vector-push-extend (plump:text mathstring) editor_lines_array) )  


        (setf (editor-lines editor_object) editor_lines_array)

        (vector-push-extend editor_object editor_object_array)
        (setf (cell-editors cell_object) editor_object_array)  ) ) 




    ;; the cell_object will be stored in document_cell_array
  (vector-push-extend cell_object document_cell_array) )

;; document_cell_array will stored in document_object
(setf (document-cells document_object) document_cell_array)










