;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

(in-package :THEMATIC-SUBSTRATE)

(defun lisp-to-xml (expr &optional (stream t) 
                         &key
                         (use-attributes-p t) 
                         top-level-attributes
                         indentation-p 
                         newlines-p 
                         ascii-p)
                         
  (labels ((do-it (expr i)
             
             (when indentation-p
               (dotimes (i (* i 2))
                 (format stream " ")))
             
             (typecase expr
               (cons 
                (format stream 
                        (if newlines-p 
                            "<LIST>~%"
                          "<LIST>"))
                (mapc #'(lambda (x) (do-it x (1+ i)))
                      expr)
                
                (when indentation-p
                  (dotimes (i (* i 2))
                    (format stream " ")))
                
                (format stream 
                        (if newlines-p 
                            "</LIST>~%"
                          "</LIST>")))
               
               (null
                (format stream 
                        (if newlines-p 
                            "<NULL/>~%"
                          "<NULL/>")))

               (symbol 
                (cond ((eq expr t)
                       (format stream 
                               (if newlines-p 
                                   "<TRUE/>~%"
                                 "<TRUE/>")))
                      (t
                       (let ((type
                              (if (or (char= (elt (symbol-name expr) 0) #\?)
                                      (char= (elt (symbol-name expr) 0) #\$))
                                  'variable
                                'symbol)))

                         (if use-attributes-p 
                             (format stream 
                                     (if ascii-p 
                                         (if newlines-p 
                                             "<~A VALUE=\"~A\"/>~%" 
                                           "<~A VALUE=\"~A\"/>")
                                       (if newlines-p 
                                           "<~A VALUE=\"~S\"/>~%" 
                                         "<~A VALUE=\"~S\"/>"))
                                     type
                                     expr
                                     type)
                           
                           (format stream 
                                   (if ascii-p 
                                       (if newlines-p 
                                           "<~A>~A</~A>~%" 
                                         "<~A>~A</~A>")
                                     (if newlines-p 
                                         "<~A>~S</~A>~%"
                                       "<~A>~S</~A>"))
                                   type
                                   expr
                                   type))))))
               
               (otherwise 
                (let ((type (type-of expr)))
                  (if use-attributes-p 
                      (format stream 
                              (if ascii-p 
                                  (if newlines-p 
                                      "<~A VALUE=\"~A\"/>~%" 
                                    "<~A VALUE=\"~A\"/>")
                                (if newlines-p 
                                    "<~A VALUE=\"~S\"/>~%" 
                                  "<~A VALUE=\"~S\"/>"))
                              type
                              expr
                              type)
                    
                    (format stream 
                            (if ascii-p 
                                (if newlines-p 
                                    "<~A>~A</~A>~%" 
                                  "<~A>~A</~A>")
                              (if newlines-p 
                                  "<~A>~S</~A>~%"
                                "<~A>~S</~A>"))
                            type
                            expr
                            type)))))))
               
               
    (if top-level-attributes
        (format stream
                (if newlines-p "<XML ~A>~%" "<XML ~A>") top-level-attributes)
      (format stream (if newlines-p "<XML>~%" "<XML>")))
      
    (do-it expr 1)
    (format stream "</XML>~%")))

#|     
(defun xml-to-lisp (stream)
  )
|# 

