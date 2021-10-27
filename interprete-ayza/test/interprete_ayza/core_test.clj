(ns interprete-ayza.core-test
  (:require [clojure.test :refer :all]
            [interprete-ayza.core :refer :all]))

;(deftest a-test
;  (testing "FIXME, I fail."
 ;   (is (= 0 1))))

(deftest palabra-reservada?-test 
    (testing "Prueba de la funcion: palabra-reservada?"
        (is (= false (palabra-reservada? 'ASIGNAR)))
        (is (= true (palabra-reservada? "CALL")))
        (is (= true (palabra-reservada? "IF")))
        (is (= false (palabra-reservada? 'PALABRA)))
        (is (= false (palabra-reservada? "PALABRA")))
        (is (= false (palabra-reservada? "Y")))
    )   
)

(deftest identificador?-test
    (testing "Prueba de la funcion: identificador?"
        (is (= false (identificador? 2)))
        (is (= false (identificador? '2)))
        (is (= false (identificador? "2")))
        (is (= false (identificador? "CALL")))
        (is (= false (identificador? 'CALL)))       
        (is (= true (identificador? "V2")))
        (is (= true (identificador? 'V2)))
        (is (= true (identificador? "ASD2")))        
        (is (= true (identificador? 'ASD2)))
        (is (= true (identificador? "Y")))        
    )   
)

(deftest cadena?-test 
     (testing "Prueba de la funcion: cadena?"
        (is (= false (cadena? "Hola")))
        (is (= false (cadena? "'Hola")))
        (is (= false (cadena? 2)))
        (is (= false (cadena? 'a)))
        (is (= false (cadena? "Hola'")))
        (is (= true (cadena? "'Hola'")))
        (is (= true (cadena? "'Y='")))     
     )   
)

(deftest ya-declarado-localmente?-test
     (testing "Prueba de la funcion: ya-declarado-localmente?"
        (is (= true (ya-declarado-localmente? 'Y '[ [0] [[X VAR 0] [Y VAR 1]] ])))
        (is (= false (ya-declarado-localmente? 'Z '[ [0] [[X VAR 0] [Y VAR 1]] ])))
        (is (= false (ya-declarado-localmente? 'Y '[[0 3 5] [[X VAR 0] [Y VAR 1] [INICIAR PROCEDURE 1] [Y CONST 2] [ASIGNAR PROCEDURE 2]]])))
        (is (= true (ya-declarado-localmente? 'Y '[[0 3 5] [[X VAR 0] [Y VAR 1] [INICIAR PROCEDURE 1] [Y CONST 2] [ASIGNAR PROCEDURE 2] [Y CONST 6]]])))
     )                 
)

(deftest dump-test
    (testing "Prueba de la funcion: dump"
        (is (= (dump '[[PFM 0] [PFI 2] MUL [PFI 1] ADD NEG]) (print 0 ['PFM 0] 1 ['PFI 2] 2 'MUL 3 ['PFI 1] 4 'ADD 5 'NEG)))
        (is (= (dump '[HLT]) (print 0 'HLT)))
        (is (= (dump nil) (print 0 nil)))
    )
)

(deftest fixup-test
    (testing "Prueba de la funcion: fixup"
        (is (= (fixup ['WRITELN (list 'END (symbol ".")) [] :error [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] 1) ['WRITELN  (list 'END (symbol ".")) [] :error [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] ))
        (is (= (fixup ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] 1) ['WRITELN (list 'END '.) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP 4] [CAL 1] RET]]))
        (is (= (fixup ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP 4] [CAL 1] RET [PFM 2] OUT NL RET]] 0) ['WRITELN (list 'END '.) [] :sin-errores [[0 3] []] 6 '[[JMP 8] [JMP 4] [CAL 1] RET [PFM 2] OUT NL RET]]))
    )
)

(deftest generar-operador-relacional-test
    (testing "Prueba de la funcion: generar-operador-relacional"
        (is (= (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :error [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '=) ['WRITELN (list 'END (symbol ".")) [] :error [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]]))
        (is (= (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '+) ['WRITELN (list 'END '.) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]]))
        (is (= (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '=) ['WRITELN (list 'END '.) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET EQ]]))
        (is (= (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '>=) ['WRITELN (list 'END '.) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET GTE]]))
    )
)

(deftest generar-signo-test
    (testing "Prueba de la funcion: generar-signo"
        (is (= (generar-signo ['nil () [] :error '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '-) ['nil () [] :error '[[0] [[X VAR 0]]] 1 '[MUL ADD]]))
        (is (= (generar-signo ['nil () [] :error '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '+) ['nil () [] :error '[[0] [[X VAR 0]]] 1 '[MUL ADD]]))
        (is (= (generar-signo ['nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '+) ['nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]]))
        (is (= (generar-signo ['nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '*) ['nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]]))
        (is (= (generar-signo ['nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '-) ['nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD NEG]]))
    )
)

(deftest buscar-coincidencias-test
    (testing "Prueba de la funcion: buscar-coincidencias" 
        (is (= (buscar-coincidencias '[nil () [CALL X] :sin-errores [[0 3] [[X VAR 0] [Y VAR 1] [A PROCEDURE 1] [X VAR 2] [Y VAR 3] [B PROCEDURE 2]]] 6 [[JMP ?] [JMP 4] [CAL 1] RET]]) (list '[X VAR 0] '[X VAR 2])))
    )
)

(deftest generar-test
    (testing "Prueba de la funcion: generar"
        (is (= (generar '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]] 'HLT) ['nil () '[VAR X] :sin-errores [[0] []] 0 '[[JMP ?] HLT]]))
    )
)

(deftest procesar-signo-unario-test
    (testing    
        (is (= (procesar-signo-unario ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X ':=] :error '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]
                ) 
                ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END '. ) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X ':=] :error [[0] [['X 'VAR 0] ['Y 'VAR 1]]] 2 []]
            )
        )
        (is (= (procesar-signo-unario [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X ':=] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]
                ) 
               [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END '.) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X ':=] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]
            )
        )
        (is (= 
               (procesar-signo-unario ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X  ':=] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]
               ) 
               [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END '.) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X ':= '+] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]        
            )
        )
        (is (= 
                (procesar-signo-unario ['- (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X ':=] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]
                )
                [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END '.) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X ':= '-] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]
            )
        )    
    )
)

(deftest declaracion-var-test
    (testing "Prueba de la funcion: declaracion-var"
        (is (=
                (declaracion-var ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X ':= 7 (symbol ";") 'Y ':= 12 (symbol ";") 'END '.) [] :error [[0] []] 0 '[[JMP ?]]]
                )
                ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X ':= 7 (symbol ";") 'Y ':= 12 (symbol ";") 'END '.) [] :error '[[0] []] 0 '[[JMP ?]]]
            )
        )
        (is (=
                (declaracion-var ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X ':= 7 (symbol ";") 'Y ':= 12 (symbol ";") 'END (symbol ".")) [] :sin-errores [[0] []] 0 '[[JMP ?]]]
                )
                ['BEGIN (list 'X ':= 7 (symbol ";") 'Y ':= 12 (symbol ";") 'END '.) ['VAR 'X (symbol ",") 'Y (symbol ";")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 '[[JMP ?]]]
            )
        )
    )
)

(deftest inicializar-contexto-local-test    
    (testing "Prueba de la funcion: inicializar-contexto-local"
        (is (=
                (inicializar-contexto-local '[nil () [] :error [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]])
                ['nil () [] :error '[[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 '[[JMP ?]]]            
            )
        )
        (is (=
                (inicializar-contexto-local '[nil () [] :sin-errores [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]])
                ['nil () [] :sin-errores '[[0 3] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 '[[JMP ?]]]
            )
        )
    )
)

(deftest a-mayusculas-salvo-strings-test
    (testing "Prueba de la funcion: a-mayusculas-salvo-strings"
        (is (=  
                (a-mayusculas-salvo-strings "  const Y = 2;"
                )
                "  CONST Y = 2;"
            )
        )
        (is (=
                (a-mayusculas-salvo-strings "  writeln ('Se ingresa un valor, se muestra su doble.');"
                )
                "  WRITELN ('Se ingresa un valor, se muestra su doble.');"
            )
        )
        (is (=
                (a-mayusculas-salvo-strings "  write ('Y=');"
                )
                "  WRITE ('Y=');"
            )
        ) 
        (is (=
                (a-mayusculas-salvo-strings "  readln (Y);"
                )
                "  READLN (Y);"
            )
        )                
    )
)

(deftest aplicar-aritmetico-test
    (testing "Prueba de la funcion: aplicar-aritmetico"
        (is
            (=
                (aplicar-aritmetico + [1 2])
                [3]
            )        
        )
        (is
            (=
                (aplicar-aritmetico - [1 4 1])
                [1 3]
            )        
        )
        (is
            (=
                (aplicar-aritmetico / [1 2 4])
                [1 0]
            )        
        )
        (is
            (=
                (aplicar-aritmetico + nil)
                nil
            )        
        )
        (is
            (=
                (aplicar-aritmetico + [])
                []
            )        
        )
        (is
            (=
                (aplicar-aritmetico + [1])
                [1]
            )        
        )
        (is
            (=
                (aplicar-aritmetico 'hola [1 2 4])
                [1 2 4]
            )        
        )
        (is
            (=
                (aplicar-aritmetico count [1 2 4])
                [1 2 4]
            )        
        )
        (is
            (=
                (aplicar-aritmetico + '[a b c])
                '[a b c]
            )        
        )
        
    )
)

(deftest aplicar-relacional-test
    (testing "Prueba de la funcion: aplicar-relacional"
        (is
            (=
                (aplicar-relacional > [7 5])
                [1]
            )
        )
        (is
            (=
                (aplicar-relacional > [4 7 5]) 
                [4 1]
            )
        )
        (is
            (=
                (aplicar-relacional = [4 7 5]) 
                [4 0]
            )
        )
        (is
            (=
                (aplicar-relacional not= [4 7 5])
                [4 1]
            )
        )
        (is
            (=
                (aplicar-relacional < [4 7 5])
                [4 0]
            )
        )
        (is
            (=
                (aplicar-relacional <= [4 6 6])
                [4 1]
            )
        )
        (is
            (=
                (aplicar-relacional <= '[a b c])
                '[a b c]
            )
        )
    )
)

(deftest termino-test
    (testing "Prueba de la funcion: termino"
        (is
            (=
                (termino ['X (list '* 2 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []]
                ) 
                ['X (list '* 2 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []]
            )
        )
        (is
            (=
                (termino ['X (list '* 2 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []]
                ) 
                ['END (list '.) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=") 'X '* 2] :sin-errores '[[0] [[X VAR 0]]] 1 '[[PFM 0] [PFI 2] MUL]]
            )
        )

    )
)

(deftest cargar-var-en-tabla-test
    (testing "Prueba de la funcion: cargar-var-en-tabla"
       (is 
          (= 
            '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]] 
            (cargar-var-en-tabla '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]]
            )
          )  
       ) 
       (is 
          (= 
             '[nil () [VAR X] :sin-errores [[0] [[X VAR 0]]] 1 [[JMP ?]]]
             (cargar-var-en-tabla '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]]
             ) 
           )
       ) 
       (is 
          (= 
             '[nil () [VAR X Y] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 [[JMP ?]]] 
             (cargar-var-en-tabla '[nil () [VAR X , Y] :sin-errores [[0] [[X VAR 0]]] 1 [[JMP ?]]]
             ) 
           )
       )
    )
)

(deftest expresion-test
    (testing "Prueba de la funcion: expresion"
       (is 
           (=                             
                (expresion ['- (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []]
                )
                ['- (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error [[0] '[[X VAR 0]]] 1 []]
           )
       )
       (is 
           (=
                (expresion ['+ (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []]
                )
                ['END (list (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=") '+ (symbol "(") 'X '* 2 '+ 1 (symbol ")")] :sin-errores '[[0] [[X VAR 0]]] 1 '[[PFM 0] [PFI 2] MUL [PFI 1] ADD]]
           )           
       )
       (is 
           (=
                (expresion ['- (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []]
                )
                ['END (list '.) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=") '- (symbol "(") 'X '* 2 '+ 1 (symbol ")")] :sin-errores '[[0] [[X VAR 0]]] 1 '[[PFM 0] [PFI 2] MUL [PFI 1] ADD NEG]]
           )           
       )
     )
)

(deftest interpretar-test
  (testing "Prueba de funcion: interpretar POP PMI PFI"
    (is (= ['[1] 2 [] []] (interpretar '[[PFI 1] [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[1 1] 4 [] []] (interpretar '[[PFI 1] [POP 0] [PFM 0] [POP 1] RHLT] '[0 0] 0 [] [])))
    )
    (testing "Prueba de funcion: interpretar ADD SUB MUL DIV"
    (is (= ['[1] 3 [] []] (interpretar '[[PFI 1] [POP 0] ADD RHLT] '[0] 0 [] [])))
    (is (= ['[6] 4 [] []] (interpretar '[[PFI 3] [PFI 2] MUL [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[-6] 4 [] []] (interpretar '[[PFI 3] [PFI -2] MUL [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[1] 4 [] []] (interpretar '[[PFI 3] [PFI 2] SUB [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[-1] 4 [] []] (interpretar '[[PFI 2] [PFI 3] SUB [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[0] 4 [] []] (interpretar '[[PFI 2] [PFI 3] DIV [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[2] 4 [] []] (interpretar '[[PFI 4] [PFI 2] DIV [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[-2] 4 [] []] (interpretar '[[PFI -4] [PFI 2] DIV [POP 0] RHLT] '[0] 0 [] [])))
    )
    (testing "Prueba de funcion: interpretar ADD SUB MUL DIV"
    (is (= ['[1] 4 [] []] (interpretar '[[PFI 3] [PFI 3] EQ [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[0] 4 [] []] (interpretar '[[PFI 3] [PFI 3] NEQ [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[1] 4 [] []] (interpretar '[[PFI 3] [PFI 4] NEQ [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[0] 4 [] []] (interpretar '[[PFI 3] [PFI 4] GT [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[0] 4 [] []] (interpretar '[[PFI 4] [PFI 4] GT [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[1] 4 [] []] (interpretar '[[PFI 5] [PFI 4] GT [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[1] 4 [] []] (interpretar '[[PFI 5] [PFI 4] GTE [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[1] 4 [] []] (interpretar '[[PFI 5] [PFI 5] GTE [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[0] 4 [] []] (interpretar '[[PFI 4] [PFI 5] GTE [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[1] 4 [] []] (interpretar '[[PFI 3] [PFI 4] LT [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[0] 4 [] []] (interpretar '[[PFI 4] [PFI 4] LT [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[0] 4 [] []] (interpretar '[[PFI 5] [PFI 4] LT [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[0] 4 [] []] (interpretar '[[PFI 5] [PFI 4] LTE [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[1] 4 [] []] (interpretar '[[PFI 5] [PFI 5] LTE [POP 0] RHLT] '[0] 0 [] [])))
    (is (= ['[1] 4 [] []] (interpretar '[[PFI 4] [PFI 5] LTE [POP 0] RHLT] '[0] 0 [] [])))
    )
    (testing "Prueba de funcion: interpretar NEG ODD"
      (is (= ['[0] 2 [-1] []] (interpretar '[[PFI 1] NEG RHLT] '[0] 0 [] [])))
      (is (= ['[0] 2 [1] []] (interpretar '[[PFI -1] NEG RHLT] '[0] 0 [] [])))
      (is (= ['[0] 2 [1] []] (interpretar '[[PFI -1] ODD RHLT] '[0] 0 [] [])))
      (is (= ['[0] 2 [0] []] (interpretar '[[PFI 2] ODD RHLT] '[0] 0 [] [])))
      (is (= ['[0] 2 [0] []] (interpretar '[[PFI 0] ODD RHLT] '[0] 0 [] [])))
    )
    (testing "Prueba de funcion: interpretar RET CAL JC JMP"
      (is (= ['[0] 4 [] []] (interpretar '[[JMP 4] ADD SUM RES RHLT] '[0] 0 [] [])))
      (is (= ['[0] 4 [] []] (interpretar '[[JC 4] ADD SUM RES RHLT] '[0] 0 [1] [])))
      (is (= ['[0] 1 [] []] (interpretar '[[JC 4] RHLT] '[0] 0 [0] [])))
      (is (= ['[0] 4 [] [1]] (interpretar '[[CAL 4] ADD SUM RES RHLT] '[0] 0 [] [])))
      (is (= ['[0] 4 [] []] (interpretar '[[RET 4] ADD SUM RES RHLT] '[0] 0 [] [4])))
      )
)
; -------------> ^ está ok 