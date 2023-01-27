       IDENTIFICATION DIVISION.
       PROGRAM-ID. 04UNISYS.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 DATA-INICIAL.
         03 DIA-INICIAL      PIC 9(02).
         03 MES-INICIAL      PIC 9(02).
         03 ANO-INICIAL      PIC 9(04).

       01 DATA-FINAL.
         03 DIA-FINAL        PIC 9(02).
         03 MES-FINAL        PIC 9(02).
         03 ANO-FINAL        PIC 9(04).

       01 DIA1               PIC 9(02).
       01 DIA2               PIC 9(02).

       01 DIVIDE-ANO1.
         03 DIV1-POR-4       PIC 9(04).
         03 DIV1-POR-100     PIC 9(04).
         03 DIV1-POR-400     PIC 9(04).
      *armazena o resto das divisoes do ano1
       01 RESTO-ANO1.
         03 R1-4             PIC 9(04).
         03 R1-100           PIC 9(04).
         03 R1-400           PIC 9(04).

      *armazena o resultado da divisao do ano2 por 4, 100 e 400
       01 DIVIDE-ANO2.
         03 DIV2-POR-4       PIC 9(04).
         03 DIV2-POR-100     PIC 9(04).
         03 DIV2-POR-400     PIC 9(04).
      *armazena o resto das divisoes do ano2
       01 RESTO-ANO2.
         03 R2-4             PIC 9(04).
         03 R2-100           PIC 9(04).
         03 R2-400           PIC 9(04).

      *diferença de dias
       01 DIFERENCA-DIAS     PIC 9(05).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            ADD 01   TO DIA-INICIAL.
            ADD 01   TO MES-INICIAL.
            ADD 2021 TO ANO-INICIAL.

            ADD 27   TO DIA-FINAL.
            ADD 01   TO MES-FINAL.
            ADD 2025 TO ANO-FINAL.
      *JANEIRO
            IF (MES-INICIAL = 01)OR(MES-FINAL = 01)
              ADD 31 TO DIA1
              ADD 31 TO DIA2
            END-IF
      *FEVEREIRO
            IF (MES-INICIAL = 02)OR(MES-FINAL = 02)

             DISPLAY "QUANTIDADE DIAS: " DIA1
             DISPLAY "QUANTIDADE DIAS: " DIA2

              DIVIDE ANO-INICIAL BY 4    GIVING DIV1-POR-4
               REMAINDER R1-4
              DIVIDE ANO-INICIAL BY 100  GIVING DIV1-POR-100
               REMAINDER R1-100
              DIVIDE ANO-INICIAL BY 400  GIVING DIV1-POR-400
               REMAINDER R1-400

              DIVIDE ANO-FINAL   BY 4    GIVING DIV2-POR-4
               REMAINDER R2-4
              DIVIDE ANO-FINAL   BY 100  GIVING DIV2-POR-100
               REMAINDER R2-100
              DIVIDE ANO-FINAL   BY 400  GIVING DIV2-POR-400
               REMAINDER R2-400

              IF (R1-4 = 0)AND(R1-100 IS NOT = 0)OR(R1-400 = 0)
               DISPLAY "Ano1 " ANO-INICIAL " eh bissexto"
                 ADD 29  TO DIA1

              ELSE
                 ADD 28  TO DIA1

              END-IF
      *    Verifica SEGUNDO ANO
              IF (R2-4 = 0)AND(R2-100 IS NOT = 0)OR(R2-400 = 0)
               DISPLAY "Ano2 " ANO-FINAL " eh bissexto"

                 ADD 29  TO DIA2
              ELSE

                ADD 28   TO DIA2
              END-IF

             DISPLAY "QUANTIDADE DIAS: " DIA1
             DISPLAY "QUANTIDADE DIAS: " DIA2

            END-IF
      *MARÇO
            IF (MES-INICIAL = 03)OR(MES-FINAL = 03)
              ADD 31 TO DIA1
              ADD 31 TO DIA2
            END-IF
      *ABRIL
            IF (MES-INICIAL = 04)OR(MES-FINAL = 04)
              ADD 30 TO DIA1
              ADD 30 TO DIA2
            END-IF
      *MAIO
            IF (MES-INICIAL = 05)OR(MES-FINAL = 05)
              ADD 31 TO DIA1
              ADD 31 TO DIA2
            END-IF
      *JUNHO
            IF (MES-INICIAL = 06)OR(MES-FINAL = 06)
              ADD 30 TO DIA1
              ADD 30 TO DIA2
            END-IF
      *JULHO
            IF (MES-INICIAL = 07)OR(MES-FINAL = 07)
              ADD 31 TO DIA1
              ADD 31 TO DIA2
            END-IF
      *AGOSTO
            IF (MES-INICIAL = 08)OR(MES-FINAL = 08)
              ADD 31 TO DIA1
              ADD 31 TO DIA2
            END-IF
      *SETEMBRO
            IF (MES-INICIAL = 09)OR(MES-FINAL = 09)
              ADD 30 TO DIA1
              ADD 30 TO DIA2
            END-IF
      *OUTUBRO
            IF (MES-INICIAL = 10)OR(MES-FINAL = 10)
              ADD 31 TO DIA1
              ADD 31 TO DIA2
            END-IF
      *NOVEMBRO
            IF (MES-INICIAL = 11)OR(MES-FINAL = 11)
              ADD 30 TO DIA1
              ADD 30 TO DIA2
            END-IF
      *DEZEMBRO
            IF (MES-INICIAL = 12)OR(MES-FINAL = 12)
              ADD 31 TO DIA1
              ADD 31 TO DIA2
            END-IF

            COMPUTE DIFERENCA-DIAS =(ANO-FINAL * 365)+(MES-FINAL * DIA2)
                                     + DIA-FINAL - (ANO-INICIAL * 365)-
                                     (MES-INICIAL * DIA1)- DIA-INICIAL.

            DISPLAY "A diferenca entre as datas eh: " DIFERENCA-DIAS.

            STOP RUN.
       END PROGRAM 04UNISYS.
