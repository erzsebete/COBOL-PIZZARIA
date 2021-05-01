      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pizaria_Ramalho.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  DATA-SISTEMA.
         05 ANO                       PIC 9(04)  VALUES ZEROS.
         05 MES                       PIC 9(02)  VALUES ZEROS.
         05 DIA                       PIC 9(02)  VALUES ZEROS.
       01  HORA-SISTEMA.
         05 HORA                      PIC 9(02)  VALUES ZEROS.
         05 MINUTO                    PIC 9(02)  VALUES ZEROS.

        77 PEDIDO                     PIC 9(3)   VALUES 0.

        77 TIPO-PIZZA                 PIC X.
           88 VALIDAR-TIPO-PIZZA                 VALUES "1" THRU "3".

        77 INGREDIENTE                PIC X(02).
           88 VALIDAR-INGREDIENTE                VALUES "01" THRU "10".

        77 TEMP-NOME-CLIENTE          PIC X(30).
        77 NOME-CLIENTE               PIC X(30).
           88 VALIDAR-NOME-CLIENTE               VALUES "A" THRU "Z".

        77 CONTATO-CLIENTE            PIC X(8).
           88 VALIDAR-CONTATO-CLIENTE            VALUES "0" THRU "9".

        77 QT-INGR                    PIC X.
           88 VALIDAR-QT-INGR                    VALUES "1" THRU "5".
        77 AUX-INGR                   PIC 9      VALUES 0.

        77 LINHA                      PIC 99     VALUE 18.
        77 LINHATOTAL                 PIC 99     VALUE 0.

        77 TOTAL                      PIC 999V99 VALUES ZEROS.
        77 SAIDA-TOTAL                PIC ZZ9.99.

        77 IVA                        PIC 9V99   VALUES ZEROS.
        77 SAIDA-IVA                  PIC 9.99.

        77 TOTAL_FINAL                PIC 999V99 VALUES ZEROS.
        77 SAIDA-TOTAL_FINAL          PIC ZZ9.99.

        77 RESPOSTA                   PIC X      VALUES SPACE.
           88 VALIDAR-RESPOSTA                   VALUES "S","s","N","n".

       SCREEN SECTION.
        01 CLS BLANK SCREEN.
        01 CABECALHO.
         05 LINE 1  COL 01 VALUE "Pizaria Ramalho, GestPedidosBeta-1"
           FOREGROUND-COLOR 2 HIGHLIGHT.
         05 LINE 2  COL 01 VALUE "Pizas e Derivados, Lda."
           FOREGROUND-COLOR 2 HIGHLIGHT.
         05 LINE 3  COL 01 VALUE
           "___________________________________________________________"
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 LINE 3 COL 60 VALUE
           "________________________________"
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 LINE 5  COL 01 VALUE "Nß Pedido:"
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 LINE 6  COL 01 VALUE "     Data:"
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 LINE 5  COL 50 VALUE "Cliente:"
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 LINE 6  COL 50 VALUE "Contato: "
           FOREGROUND-COLOR 6 HIGHLIGHT.

         05 LINE 8  COL 01 VALUE "[1] Pequena [2] MÇdia [3] Grande"
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 LINE 10  COL 01 VALUE "Tipo de Piza Pretendido:"
           FOREGROUND-COLOR 6 HIGHLIGHT.

         05 LINE 12 COL 01  VALUE "        Nß Ingredientes: "
           FOREGROUND-COLOR 6 HIGHLIGHT.

         05 LINE 14 COL 01 VALUE
           "___________________________________________________________"
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 LINE 14 COL 60 VALUE
           "________________________________"
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 LINE 15 COL 01 VALUE
           "C¢d. Ingrediente            Ingrediente               Preáo"
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 LINE 16  COL 01 VALUE
           "___________________________________________________________"
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 LINE 16 COL 60 VALUE
           "________________________________"
           FOREGROUND-COLOR 6 HIGHLIGHT.


           PROCEDURE DIVISION.
       INICIO.

           DISPLAY CLS.
           DISPLAY CABECALHO.
           ADD 1 TO PEDIDO.
           DISPLAY PEDIDO AT 0514.

           ACCEPT DATA-SISTEMA FROM DATE YYYYMMDD.
           DISPLAY FUNCTION CONCATENATE(DIA,"/",MES,"/",ANO) AT 0614.

           MOVE 18 TO LINHA.
           MOVE 0 TO LINHATOTAL.
           MOVE 0 TO TOTAL.
           MOVE 0 TO AUX-INGR.


       NOME_CLIENTE.
           PERFORM WITH TEST AFTER UNTIL (VALIDAR-NOME-CLIENTE)
           ACCEPT TEMP-NOME-CLIENTE AT 0560
           MOVE FUNCTION UPPER-CASE (TEMP-NOME-CLIENTE) TO NOME-CLIENTE
               IF (NOT VALIDAR-NOME-CLIENTE) THEN
                   DISPLAY "Insira apenas letras."
                   FOREGROUND-COLOR 4 HIGHLIGHT              AT 0590
               ELSE
                   DISPLAY " " ERASE EOL                     AT 0590
               END-IF
           END-PERFORM.

       CONTATO_CLIENTE.
           PERFORM WITH TEST AFTER UNTIL (VALIDAR-CONTATO-CLIENTE)
           ACCEPT CONTATO-CLIENTE                            AT 0660
              IF (NOT VALIDAR-CONTATO-CLIENTE) THEN
                   DISPLAY "Insira apenas algarismos."
                   FOREGROUND-COLOR 4 HIGHLIGHT              AT 0690
               ELSE
                   DISPLAY " " ERASE EOL                     AT 0690
               END-IF
           END-PERFORM.

       TIPO_PIZZA.
           PERFORM WITH TEST AFTER UNTIL (VALIDAR-TIPO-PIZZA)
           ACCEPT TIPO-PIZZA                                 AT 1026
               IF (NOT VALIDAR-TIPO-PIZZA) THEN
                   DISPLAY "Escolha 1, 2 ou 3."
                   FOREGROUND-COLOR 4 HIGHLIGHT              AT 1050
               ELSE
                   DISPLAY " " ERASE EOL                     AT 1050
               END-IF
           END-PERFORM.


           EVALUATE (TIPO-PIZZA)

           WHEN "1"
           DISPLAY "- PEQUENA" AT 1028
           ADD 3 TO TOTAL

           WHEN "2"
           DISPLAY "- MêDIA"   AT 1028
           ADD 4 TO TOTAL

           WHEN "3"
           DISPLAY "- GRANDE"  AT 1028
           ADD 5 TO TOTAL.

       QT_INGREDIENTES.

           PERFORM WITH TEST AFTER UNTIL (VALIDAR-QT-INGR)
           ACCEPT QT-INGR                                    AT 1226
               IF (NOT VALIDAR-QT-INGR) THEN
                   DISPLAY "Insira 0 a 5 ingredientes."
                   FOREGROUND-COLOR 4 HIGHLIGHT              AT 1250
               ELSE
                   DISPLAY " " ERASE EOL                     AT 1250
               END-IF
           END-PERFORM.


       INSERIR_INGREDIENTES.
           PERFORM UNTIL FUNCTION NUMVAL(QT-INGR) = AUX-INGR
            ACCEPT INGREDIENTE AT LINE LINHA COL 05







      *--------------------^     NéO CONSIGO FAZER:
      *----------PERFORM WITH TEST AFTER UNTIL (VALIDAR-INGREDIENTE)



           EVALUATE (INGREDIENTE)

           WHEN "1"
           DISPLAY "FIAMBRE"                  AT LINE LINHA COL 30
           DISPLAY "0.5"                      AT LINE LINHA COL 56
           ADD 0.5 TO TOTAL

           WHEN "2"
           DISPLAY "ATUM"                     AT LINE LINHA COL 30
           DISPLAY "0.7"                      AT LINE LINHA COL 56
           ADD 0.7 TO TOTAL

           WHEN "3"
           DISPLAY "ANCHOVAS"                 AT LINE LINHA COL 30
           DISPLAY "0.4"                      AT LINE LINHA COL 56
           ADD 0.4 TO TOTAL

           WHEN "4"
           DISPLAY "CAMARéO"                  AT LINE LINHA COL 30
           DISPLAY "0.8"                      AT LINE LINHA COL 56
           ADD 0.8 TO TOTAL

           WHEN "5"
           DISPLAY "BACON"                    AT LINE LINHA COL 30
           DISPLAY "0.9"                      AT LINE LINHA COL 56
           ADD 0.9 TO TOTAL

           WHEN "6"
           DISPLAY "BANANA"                   AT LINE LINHA COL 30
           DISPLAY "0.3"                      AT LINE LINHA COL 56
           ADD 0.3 TO TOTAL

           WHEN "7"
           DISPLAY "ANANÜS"                   AT LINE LINHA COL 30
           DISPLAY "0.4"                      AT LINE LINHA COL 56
           ADD 0.4 TO TOTAL

           WHEN "8"
           DISPLAY "AZEITONAS"                AT LINE LINHA COL 30
           DISPLAY "0.3"                      AT LINE LINHA COL 56
           ADD 0.3 TO TOTAL

           WHEN "9"
           DISPLAY "COGUMELOS"                AT LINE LINHA COL 30
           DISPLAY "0.6"                      AT LINE LINHA COL 56
           ADD 0.6 TO TOTAL

           WHEN "10"
           DISPLAY "MILHO"                    AT LINE LINHA COL 30
           DISPLAY "0.5"                      AT LINE LINHA COL 56
           ADD 0.5 TO TOTAL

           END-EVALUATE

           ADD 1 TO LINHA
           ADD 1 TO AUX-INGR

           END-PERFORM.


           COMPUTE IVA= TOTAL * 0.23.
           MOVE IVA TO SAIDA-IVA.
           COMPUTE TOTAL_FINAL = TOTAL + IVA.
           MOVE TOTAL_FINAL TO SAIDA-TOTAL_FINAL.



           MOVE TOTAL TO SAIDA-TOTAL.
           MOVE LINHA TO LINHATOTAL.
           ADD 1 TO LINHATOTAL.

           DISPLAY
           "___________________________________________________________"
           AT LINE LINHATOTAL COL 01.
           DISPLAY "________________________________"
           AT LINE LINHATOTAL COL 60.
           ADD 1 TO LINHATOTAL.

           DISPLAY FUNCTION CONCATENATE
           ("                                 TOTAL INGREDIENTES:   ",
           QT-INGR)                           AT LINE LINHATOTAL COL 01.
           ADD 1 TO LINHATOTAL.

           DISPLAY FUNCTION CONCATENATE
           ("                                       TIPO DE PIZA:   ",
           TIPO-PIZZA)                        AT LINE LINHATOTAL COL 01.
           ADD 1 TO LINHATOTAL.

           DISPLAY FUNCTION CONCATENATE
           ("                                            A PAGAR: ",
           SAIDA-TOTAL," EUR")                AT LINE LINHATOTAL COL 01.
           ADD 1 TO LINHATOTAL.

           DISPLAY FUNCTION CONCATENATE
           ("                                        I.V.A (23%):   ",
           SAIDA-IVA," EUR")                  AT LINE LINHATOTAL COL 01.
           ADD 1 TO LINHATOTAL.

           DISPLAY FUNCTION CONCATENATE
           ("                                              FINAL: ",
           SAIDA-TOTAL_FINAL," EUR") AT LINE LINHATOTAL COL 01.
           ADD 2 TO LINHATOTAL.


        NOVO_PEDIDO.
           DISPLAY
            "                                        NOVO PEDIDO?"
           AT LINE LINHATOTAL COL 01.

           PERFORM WITH TEST AFTER UNTIL (VALIDAR-RESPOSTA)
               ACCEPT RESPOSTA AT LINE LINHATOTAL COL 60

               IF (NOT VALIDAR-RESPOSTA) THEN
                   DISPLAY "Indique: S ou N" FOREGROUND-COLOR 4
                   HIGHLIGHT AT LINE LINHATOTAL COL 66
               ELSE
                   DISPLAY " " ERASE EOL AT LINE LINHATOTAL COL 66
               END-IF
           END-PERFORM.

           IF FUNCTION UPPER-CASE(RESPOSTA)="S" THEN
                GO INICIO
           END-IF.
           DISPLAY "PROGRAMA TERMINADO" FOREGROUND-COLOR 5 HIGHLIGHT
           AT LINE LINHATOTAL COL 66

           ACCEPT OMITTED AT LINE LINHATOTAL COL 88.
           STOP RUN.
       END PROGRAM Pizaria_Ramalho.
