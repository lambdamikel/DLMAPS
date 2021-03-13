(PROCLAIM
   '(OPTIMIZE
      (SAFETY 0)		; Run time error checking level
      (SPEED 3)			; Speed of the compiled code
      (COMPILATION-SPEED 0)	; Speed of compilation
      (SPACE 0)			; Space of both intermidiate files and object
      #+(or ALLEGRO LISPWORKS) (DEBUG 0)
      ))
(load "FaCT.lsp")
(compile-file "FaCT.lsp")
(load "Utilities/process-data.lsp")
(compile-file "Utilities/process-data.lsp")
(load "RandGen/Hustadt/genmodal.lsp")
(load "RandGen/Hustadt/hustadt-test.lsp")
(compile-file "RandGen/Hustadt/genmodal.lsp")
(compile-file "RandGen/Hustadt/hustadt-test.lsp")
(load "RandGen/Sebastiani/values_generators.lsp")
(load "RandGen/Sebastiani/lang-con.lsp")
(load "RandGen/Sebastiani/random_gen.lsp")
(load "RandGen/Sebastiani/tableaux_test.lsp")
(compile-file "RandGen/Sebastiani/lang-con.lsp")
(compile-file "RandGen/Sebastiani/random_gen.lsp")
(compile-file "RandGen/Sebastiani/tableaux_test.lsp")
