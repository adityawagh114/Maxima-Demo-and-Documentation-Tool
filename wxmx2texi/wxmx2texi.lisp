(declaim (sb-ext:muffle-conditions cl:warning))
(in-package :maxima)
;;plump xml parser has been used https://shinmera.github.io/plump/ 

(ql:quickload :zip)


(defun $get_folder_name(src)
   
   (pathname-name src)
  
  )


(defun $get_temp(str)
   
      (defparameter file_location  (uiop:tmpize-pathname (pathname  str)) )

      (defparameter foldername (pathname-name (pathname file_location ) ))
      ;; (print foldername)

      (delete-file (pathname file_location) )

      (defparameter folder_location ( concatenate 'string str foldername "/"))

      ;; (print folder_location)
      (ensure-directories-exist (pathname folder_location))

         foldername
  )

(defun $change_output_location(output_folder temp_folder_name folder_name)

      (defparameter output_folder ( concatenate 'string output_folder temp_folder_name "/" folder_name ".texi" ))
    
       
output_folder
 )





;;function to rename .wxmx from .zip and then unzip it and return the location of content.xml
(defun $unzipfile(src output_folder temp_folder_name folder_name)
   
   (setf newsrc (subseq src 0 (- (length src  ) 5 )))
  ;;  (setf dest (concatenate 'string newsrc "/"))
   (setf dest (concatenate 'string output_folder temp_folder_name "/" folder_name "/" ))

   (setf newsrc   (concatenate 'string newsrc ".zip" ))

   (rename-file (pathname src) (pathname newsrc) )
   (ensure-directories-exist (pathname dest) )

   (zip:unzip (pathname newsrc) (pathname dest) ) 

   (setf rename_newsrc (subseq newsrc 0 (- (length newsrc  ) 4 )))
   (setf rename_newsrc   (concatenate 'string rename_newsrc ".wxmx" ))
   (rename-file (pathname newsrc) (pathname rename_newsrc) )

   (setf dest (concatenate 'string dest "content.xml")))






;;pass cell index and this will return the lines one by one the .mac will know the total number of lines and it will convert 




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
    :initform (map-into (make-array 0 :fill-pointer 0 :adjustable t) (lambda () (make-instance 'input))))

    (output_string :accessor cell-output_string)
      
    )) 



;; The documnet acts as the root node of the XML. It conatins its attributes and cells. 
(defclass document ()
  ((attributes
    :accessor document-attributes
    :initform (map-into (make-array 0 :fill-pointer 0 :adjustable t) (lambda () (make-instance 'attribute))))
   (cells
    :accessor document-cells
    :initform (map-into (make-array 0 :fill-pointer 0 :adjustable t) (lambda () (make-instance 'cell))))))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                                                                                           ;;
                                                                                                           ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;function to parse the xml (pass the file location as argument. Example  (xmlparser "d:/Users/ADITYA SANDEEP WAGH/portacle/content.xml"))
(defun $xmlparser (file_location olocation folder_location)
  

    (defparameter str (string ""))

  (setf strvector  (make-array 0 :fill-pointer 0 :adjustable t) )


  

  ;; add the location of the content.xml to test

  
  (defparameter root (plump:parse (pathname file_location) )  )

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



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
    ;; (print (length child))

    (if (= 3 (length child))
    (progn

         (setf output_node (aref child 2) )

          (defparameter output_str (plump:serialize output_node nil ) )      
            ( setf (cell-output_string cell_object) output_str) 

            ;;  (print (cell-output_string cell_object)) 
              
    )
    (
     setf (cell-output_string cell_object) "NotDefined") 


    
    )
    
    
 


    
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
           (vector-push-extend (plump:text mathstring) strvector)
           (setf str (concatenate 'string str  ",  " (plump:text mathstring) ))

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

             (setf str (concatenate 'string str ",  " (plump:text mathstring)  ))
                        (vector-push-extend (plump:text mathstring) strvector)

           (vector-push-extend (plump:text mathstring) editor_lines_array) 
           
           
           )  


         (setf (editor-lines editor_object) editor_lines_array)

         (vector-push-extend editor_object editor_object_array)
         (setf (cell-editors cell_object) editor_object_array)  ) ) 

 


    ;; the cell_object will be stored in document_cell_array
    (vector-push-extend cell_object document_cell_array) )

  ;; document_cell_array will stored in document_object
  (setf (document-cells document_object) document_cell_array)
  (setf str (concatenate 'string str "]"   ))
  (setf str (subseq str 1))
  (setf str (concatenate 'string "[" str ))
;;   (print (length strvector))
(vector-push-extend "Wx-endofloop" strvector)
  (printobject document_object olocation folder_location)


  strvector)


(defun transform_string (str)
  
  (defparameter newstr  (make-array 0
                                        :element-type 'character
                                        :fill-pointer 0
                                        :adjustable t))

  (dotimes (i  (length str))
    
        (if (or (char= #\@ (char str i)) (char= #\{ (char str i))  (char= #\} (char str i))   )

            (vector-push-extend #\@ newstr) )
        
        (vector-push-extend ( char str i) newstr)  )

    newstr
    )













(defun modify_output(str)

   (setf str   (concatenate 'string  "<outputs>" str))
   (setf str   (concatenate 'string   str "</outputs>"))
str

)







;; function to print the parsed objects (pass the object as argument). Example  (printobject (xmlparser "d:/Users/ADITYA SANDEEP WAGH/portacle/content.xml"))
(defun printobject (document_obj texi_location folder_location)
    
         (setf image_number 1)
(with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  (format texinfo_string 
  
  " \\input texinfo ~%~%@node Top, Cell1,(dir),(dir)~%@top~%"))   

;; (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
;;   (format texinfo_string "@unnumbered "))




;; (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
;;   (format texinfo_string "Element tag Name = wxMaximaDocument  ~%@unnumberedsec Attributes of wxMaximaDocument-->   "))


  ;; (format t " Element tag Name = wxMaximaDocument")

  ;; (format t "Attributes of wxMaximaDocument-->")

  (setf attribute_array (document-attributes document_obj))

  (dotimes (i  (length attribute_array))
    
    (setf single_attribute (aref attribute_array i ))
  ;;   (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  ;; (format texinfo_string "~%@subsection Attribute name = ~a    Attribute value = ~a " (attribute-name single_attribute) (attribute-value single_attribute)  ) )
    
    ;; (format t "~% Attribute name = ~a    Attribute value = ~a " (attribute-name single_attribute) (attribute-value single_attribute))



    )
  
(with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  (format texinfo_string "~% ~% ~% ~% ~%  "))
  ;; (format t "~% ~% ~% ~% ~% ")
  (setf cell_array (document-cells document_obj))


;; (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
;;   (format texinfo_string "~%@unnumberedsec No of Cells--> ~d~%~%" (length cell_array)))

   (setq startindex 1)
  (setq endindex (length cell_array) )



;; (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
;;   (format texinfo_string "
;;      @end example "))



  ;; (format t "~% No of Cells--> ~d~%~%" (length cell_array))

  (dotimes (i (length cell_array))

    (setf cellobject (aref cell_array i ))






   (if (string/=	(cell-output_string cellobject) "NotDefined" )   
 (setf final_output (modify_output (cell-output_string cellobject) )))
;; (setf final_output (cell-output_string cellobject ))


;; (print (cell-output_string cellobject ))

;; (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
;;   (format texinfo_string "this is final output --> ~a" final_output))




    
   (if (= startindex 1) 
 (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  (format texinfo_string "@node Cell~a,Cell~a,Top,Top~%~%" startindex  ( + startindex 1) )))   

           (if (= startindex endindex) 
    (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
      (format texinfo_string "@node Cell~a, ,Cell~a,Top~%~%
       " startindex ( - startindex 1) )))

        (if (and (< startindex endindex) (> startindex 1) )        
      (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  (format texinfo_string "@node Cell~a,Cell~a,Cell~a,Top~%
    ~%" startindex (+ startindex 1) (- startindex 1) )))
      (setq startindex (+ startindex 1))
      
      
      
      




 

;; (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
;;   (format texinfo_string "
;;     @node Cell~a,Cell~a,Cell~a,Top~%
;;     @example
  
  
;;    " (startindex) (startindex+1) (startindex-1) ))




    ;;print cell attributes
;; (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
;;   (format texinfo_string "@unnumberedsec Attributes of cell--> "))


    ;; (format t " Attributes of cell-->")

    (setf attribute_array (cell-attributes cellobject))
    (setq type_number 0)

       (if (= 2 (length attribute_array))
             (
               progn
                  
                (setf first_attribute (aref attribute_array 0 ))
                (setf second_attribute (aref attribute_array 1 ))

                
                (if (and (string= (attribute-value first_attribute) "section" ) (string= (attribute-value second_attribute) "2" )    ) 
                      (
                        progn
                        (setq type_number 1)
   
                            (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
                             (format texinfo_string "~%@chapter ")) 
                      
                      )
                
                
                
                )
                 (if (and (string= (attribute-value first_attribute) "subsection" ) (string= (attribute-value second_attribute) "3" )    ) 
                      (
                        progn
                        (setq type_number 2)
                         (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
                             (format texinfo_string "~%@section "))

                      )
                
                
                
                
                )
                 (if (and (string= (attribute-value first_attribute) "subsection" ) (string= (attribute-value second_attribute) "4" )    ) 
                        (
                        progn
                        (setq type_number 3)
                         (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
                             (format texinfo_string "~%@subsection "))

                      )
                
                
                
                
                )
                 (if (and (string= (attribute-value first_attribute) "subsection" ) (string= (attribute-value second_attribute) "5" )    ) 
                        (
                        progn
                        (setq type_number 4)
                          (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
                             (format texinfo_string "~%@subsubsection "))

                      )
                
                
                
                
                )
                 (if (and (string= (attribute-value first_attribute) "subsection" ) (string= (attribute-value second_attribute) "6" )    ) 
                           (
                        progn
                        (setq type_number 5)
                              (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
                             (format texinfo_string "~%@subsubsection "))

                      )
                
                
                
                
                )
                     
 



             )
             (
             progn
                (setf first_attribute (aref attribute_array 0 ))

 
                (if (string= (attribute-value first_attribute) "text" ) 
                       (
                        progn
                        (setq type_number 6)
  
                            ;;do nothing
                      )
                
                
                
                
                )
                (if (string= (attribute-value first_attribute) "code" ) 
                        (
                        progn
                        (setq type_number 7)
                        

                      )
                
                
                
                
                )





             )
       
       
       
       )


  ;;   (dotimes (i  (length attribute_array))
      
      ;; (setf single_attribute (aref attribute_array i ))

  ;;     (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  ;; (format texinfo_string "~%@subsection Attribute name = ~a    Attribute value = ~a " (attribute-name single_attribute) (attribute-value single_attribute)))
      
  ;;     ;; (format t "~% Attribute name = ~a    Attribute value = ~a " (attribute-name single_attribute) (attribute-value single_attribute))



  ;;     )

    ;;type of cell     

  ;;   (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  ;; (format texinfo_string "~%~%@unnumberedsec Cell type--> "))  
    ;; (format t "~%~% Cell type-->")
    (setf editor_array (cell-editors cellobject))
    (setf input_array (cell-inputs cellobject))
    




    (if( = (length editor_array) 1)
       (  ;;of type editor
        progn


  ;;   (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  ;; (format texinfo_string " Editor ~%"))  

        ;; (format t " Editor ~%")
        

  ;;   (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  ;; (format texinfo_string "@unnumberedsec Attributes of editor-->"))  


        ;; (format t " Attributes of editor-->")

        (setf editorobject (aref  editor_array 0))

        

        (setf attribute_array (editor-attributes editorobject))

        (dotimes (i  (length attribute_array))
          
          (setf single_attribute (aref attribute_array i ))


  ;;          (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  ;; (format texinfo_string "~%@subsection Attribute name = ~a    Attribute value = ~a " (attribute-name single_attribute) (attribute-value single_attribute)))  
          
          ;; (format t "~% Attribute name = ~a    Attribute value = ~a " (attribute-name single_attribute) (attribute-value single_attribute))



          )       

        
        (setf linesarray (editor-lines editorobject))
  ;;           (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  ;; (format texinfo_string "~%~% @unnumberedsec No of lines = ~d ~%@unnumberedsubsec Lines-->~%" (length linesarray)))  
        
  ;;       ;; (format t "~%~% No of lines = ~d ~% Lines-->~%" (length linesarray))

  ;;  (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  ;; (format texinfo_string "@example ~%"))  
        

        (dotimes (i  (length linesarray))


                (setf strline (aref linesarray i))

                (setf strline (transform_string strline))

                (if (or (search "wxplot2d" strline)  (search "wxplot3d" strline)  (search "wxdraw2d" strline) (search "wxdraw3d" strline)  )
                    (
                        progn
                         
                       ;;add image

                        
                         

                       (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
                       (format texinfo_string  "~%@ifnotinfo~%@image{~a/image~a,10cm}~%@end ifnotinfo~%" folder_location image_number ))  
                             
                             (setf image_number (+ 1 image_number))

                    )
                    (
                      progn
                         
                            (if (= 7 type_number )
                            (progn

                       (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
                       (format texinfo_string  "@c ~a ~%" strline))                             
                            
                            
                            )
                            (progn
                            
                       (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
                       (format texinfo_string  " ~a ~%" strline))  
                            )
                            )
                      
                      
                    )
                
                
                )

          ;; (format t "  ~a ~%" (aref linesarray i))
          


          )

  ;;   (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  ;; (format texinfo_string "@end example ~%"))         



        



        )
       (  
        progn

  ;;   (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  ;; (format texinfo_string "Input ~%"))  

        
        ;; (format t " Input ~%")


  ;;           (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  ;; (format texinfo_string "@unnumberedsec Attributes of editor inside input-->"))  

        ;; (format t " Attributes of editor inside input-->")

        (setf inputobject (aref  input_array 0))

        

        (setf attribute_array (input-attributes inputobject))

        (dotimes (i  (length attribute_array))
          
          (setf single_attribute (aref attribute_array i ))


  ;;           (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  ;; (format texinfo_string "~%@subsection Attribute name = ~a    Attribute value = ~a " (attribute-name single_attribute) (attribute-value single_attribute)))  
          
          ;; (format t "~% Attribute name = ~a    Attribute value = ~a " (attribute-name single_attribute) (attribute-value single_attribute))



          )       

        
        (setf linesarray (input-lines inputobject))
  ;;                (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  ;; (format texinfo_string "~%~%@unnumberedsec No of lines = ~d ~%@unnumberedsubsec Lines-->~%" (length linesarray)))  
        
  ;;       ;; (format t "~%~% No of lines = ~d ~% Lines-->~%" (length linesarray))


  ;;   (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  ;; (format texinfo_string "@example ~%"))         


        (dotimes (i  (length linesarray))
          
          ;; (format t "  ~a ~%" (aref linesarray i))

                 (setf strline (aref linesarray i))

                (setf strline (transform_string strline))

                (if (or (search "wxplot2d" strline)  (search "wxplot3d" strline)  (search "wxdraw2d" strline) (search "wxdraw3d" strline)  )
                    (
                        progn
                         
                       ;;add image

                        
                         

                       (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
                       (format texinfo_string  "~%@ifnotinfo~%@image{~a/image~a,10cm}~%@end ifnotinfo~%" folder_location image_number ))  
                             
                             (setf image_number (+ 1 image_number))

                    )
                    (
                      progn
                         

                      
                       (if (= 7 type_number )
                            (progn

                       (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
                       (format texinfo_string  "@c ~a ~%" strline))                             
                            
                            
                            )
                            (progn
                            
                       (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
                       (format texinfo_string  " ~a ~%" strline))  
                            )
                            ) 
                    )
                
                
                )

          )


  ;;   (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  ;; (format texinfo_string "@end example ~%"))         





        )
       
       )
    
        (if (= type_number 7)
        (progn


                    (if (string/=	(cell-output_string cellobject) "NotDefined" )
                      (progn
          (defparameter maxima_string (mfuncall '$display_output_xml1 final_output))
          (defparameter tex_string (mfuncall '$display_output_xml2 final_output))


         (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create) 
                             (format texinfo_string "~%@c Maxima expression:-~%  @c ~a" maxima_string)  ) 
         

         (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create) 
                             (format texinfo_string "~%@c Simplified 2D:- ~%  " )  )

                               (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
                             (format texinfo_string "~%@ifinfo~%@example ~%"))

                        (mfuncall '$display_output_xml3 final_output texi_location )     

         
                    (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create) 
                             (format texinfo_string "~%@end example ~%@end ifinfo")) 
                             
         (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create) 
                             (format texinfo_string "~%@iftex~%@tex~%$$~a$$~%@end tex~%@end iftex" tex_string))
                             
                             
                             ))


                             
                              ))
                             




           (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  (format texinfo_string "  ~% ~% ~% ~%"   ))  

    ;; (format t "~% ~% ~% ~%")

;; (with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
;;   (format texinfo_string "
;;        @end example~% "))



    )

(with-open-file (texinfo_string texi_location :direction :output :if-exists :append :if-does-not-exist :create)
  (format texinfo_string "@bye~% "))

  )
