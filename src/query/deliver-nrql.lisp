;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: CL-USER; Base: 10 -*-

(in-package :CL-USER)

(defun shell-command (string)
  (format t "~A~%" string)
  (sys:call-system-showing-output 
   (format nil " ~A" string)))

  
(defun deliver-nrql (&optional racer-tests)

  (let* ((version 
          (substitute #\- #\.
                      (format nil "~A" (get-nrql-version))))
         (nrql-name (format nil "nRQL-~A" version))
         (nrql-path (format nil "/home/mi.wessel/~A" nrql-name))
         (query-path (format nil "/home/mi.wessel/lispworks/work/dlmaps/query")))

    (create-stubs)

    ;(run-nrql-tests)

    (when racer-tests
      (load "racer:test-tboxes"))

    (shell-command (format nil "rm -rf ~A" nrql-path))

    (shell-command (format nil "cp -R -L ~A ~A" query-path nrql-path))
    

    (shell-command (format nil "cp ~A/nrql-sysdcl.lisp ~A/sysdcl-archive/nrql-sysdcl-~A.lisp" 
                           query-path 
                           query-path 
                           version))

    (shell-command (format nil "cp ~A/nrql-sysdcl.lisp ~A/nrql-sysdcl-~A.sys" query-path nrql-path version))


    (shell-command (format nil "rm ~A/*.fasl" nrql-path))
    (shell-command (format nil "rm ~A/*.ufsl" nrql-path))
    (shell-command (format nil "rm ~A/*.bak" nrql-path))
    (shell-command (concatenate 'string (format nil "rm ~A" nrql-path)
                                "/*~"))
    (shell-command (format nil "rm -rf ~A/alt" nrql-path))


    (dolist (file 
             (sort (copy-list
                    (directory (format nil "~A/*.lisp" nrql-path)))
                   #'>
                   :key #'(lambda (x) ( length (pathname-name x)))))

      (let* ((oname (pathname-name file))
             (pos (position-if #'(lambda (x) (<= 48 (char-code x) 57)) oname))
             (name2 (concatenate 'string 
                                 (if pos (subseq oname 0 pos)
                                   oname)
                                 (format nil "-~A" version)))
             (oname (format nil "~A" oname)))

        (shell-command (format nil "mv ~A/~A.lisp ~A/~A.lisp" nrql-path oname nrql-path name2))

        (shell-command (format nil "sed 's/~A/~A/' < ~A/nrql-sysdcl-~A.sys > ~A/nrql-sysdcl-neu-~A.sys" 
                               
                               oname name2
                               nrql-path
                               version
                               nrql-path
                               version))

        (shell-command (format nil "mv ~A/nrql-sysdcl-neu-~A.sys ~A/nrql-sysdcl-~A.sys" 
                               nrql-path version
                               nrql-path version
                               ))))


    (shell-command (format nil "mv ~A/nrql-sysdcl-~A.sys ~A/nrql-sysdcl-~A.lisp" nrql-path version nrql-path version))
    
    (shell-command (format nil "cp ~A/nrql-sysdcl-~A.lisp ~A/nrql-sysdcl.lisp" nrql-path version nrql-path))

    ;;;
    ;;; Umlaute entfernen
    ;;;

    (dolist (file (directory (format nil "~A/*.lisp" nrql-path)))
      (let* ((oname (pathname-name file)))

        (shell-command (format nil "sed 's/Ä/Ae/g;s/Ö/Oe/g;s/Ü/Ue/g;s/ß/ss/g;s/ü/ue/g;s/ö/oe/g;s/ä/ae/g' < ~A/~A.lisp > ~A/~A.lisp.neu"
                               nrql-path oname
                               nrql-path oname))

        (shell-command (format nil "mv ~A/~A.lisp.neu ~A/~A.lisp" 
                               nrql-path oname
                               nrql-path oname
                               ))))


    ;;;
    ;;; Unix 2 Mac 
    ;;;


    (dolist (file (directory (format nil "~A/*.lisp" nrql-path)))
      (let* ((oname (pathname-name file)))

        (shell-command (format nil "tr '\\n' '\\r' < ~A/~A.lisp > ~A/~A.lisp.neu"
                               nrql-path oname
                               nrql-path oname))

        (shell-command (format nil "mv ~A/~A.lisp.neu ~A/~A.lisp" 
                               nrql-path oname
                               nrql-path oname
                               ))))

    ;;; 
    
    (shell-command (format nil "rm ~A/*.bak" nrql-path))

    (shell-command (format nil "rm ~A/nrql-dev-sysdcl-~A.lisp" nrql-path version nrql-path version))

    (shell-command (format nil "rm /home/mi.wessel/~A.tar.gz" nrql-name))

    
    (shell-command (format nil "tar -chvzf /home/mi.wessel/~A.tar.gz /home/mi.wessel/~A" nrql-name nrql-name))

    (shell-command (format nil "cp /home/mi.wessel/~A.tar.gz /home/mi.wessel/Desktop/nRQL-Archive/" nrql-name))

    
    ))


