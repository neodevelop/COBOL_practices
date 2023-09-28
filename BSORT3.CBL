       IDENTIFICATION DIVISION.
       PROGRAM-ID.BSORT3.
       AUTHOR.JJRZ.
       DATE-WRITTEN. 16/08/2000.
       DATE-COMPILED. 16/08/2000.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM.
       OBJECT-COMPUTER. IBM.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   SELECT ENTRADA ASSIGN TO "ENTRADA".
	   SELECT SALIDA  ASSIGN TO "SALIDA".
	   SELECT ORDEN   ASSIGN TO "ORDEN".
	   SELECT REPORTE ASSIGN TO "REPORTE".
       DATA DIVISION.
       FILE SECTION.
       FD ENTRADA.
       01 REGISTRO.
           03 NOMBRE PIC X(30).
	   03 DEPTO  PIC X(15).
	   03 SUELDO PIC 9(07).
       FD SALIDA.
       01 REG-SAL.
           03 NOMBRE-S PIC X(30).
           03 DEPTO-S PIC X(15).
	   03 SUELDO-S PIC 9(07).
       SD ORDEN.
       01 REG-SOR.
           03 NOMBRE-SS PIC X(30).
           03 DEPTO-SS PIC X(15).
	   03 SUELDO-SS PIC 9(07).
       FD REPORTE.
       01 LINEA PIC X(80).
       WORKING-STORAGE SECTION.
       01 ENCABEZADO1.
	   03 FILLER PIC X(28) VALUE SPACES.
	   03 FILLER PIC X(23) VALUE "EMPLEADOS DE LA EMPRESA".
	   03 FILLER PIC X(29) VALUE SPACES.
       01 ENCABEZADO2.
	   02 FILLER PIC X(05) VALUE SPACES.
	   02 FILLER PIC X(06) VALUE "NOMBRE".
           02 FILLER PIC X(26) VALUE SPACES.
	   02 FILLER PIC X(12) VALUE "DEPARTAMENTO".
           02 FILLER PIC X(14) VALUE SPACES.
	   02 FILLER PIC X(06) VALUE "SUELDO".
	   02 FILLER PIC X(11) VALUE SPACES.
       01 LINEA-R.
	   03 FILLER PIC X(05) VALUE SPACES.
	   03 NOMBRE-R PIC X(30).
           03 DEPTO-R PIC X(15).
           03 FILLER PIC X(05) VALUE SPACES.
           03 SUELDO-R PIC $(06)9.
       01 LINEA-R1.
           03 FILLER PIC X(19) VALUE SPACES.
           03 FILLER PIC X(27) VALUE "EL SUBTOTAL EN EL AREA DE  ".
	   03 DEPTO-R1 PIC X(15).
           03 FILLER PIC X(09) VALUE "  ES DE  ".
           03 SUELDO-R1 PIC $(08)9.
       01 LINEA-R2.
         03 FILLER PIC X(37) VALUE SPACES.
         03 FILLER PIC X(31) VALUE "EL TOTAL DE LOS SUELDOS ES DE  ". 
         03 TOTAL-R2 PIC $(10)9.
       01 LINEA-R3 PIC X(80) VALUE ALL "==".
       01 FECHA.
           03 AA PIC 99.
           03 MM PIC 99.
           03 DD PIC 99.
       01 LINFECHA.
           03 FILLER PIC X(45) VALUE SPACES.
           03 DDF PIC 9(02).
           03 FILLER PIC X(03) VALUE " - ".
           03 MMF PIC X(10).
           03 FILLER PIC X(03) VALUE " - ".
           03 AAF PIC 9(02).
       01 VARS.
           03 SW PIC 9.
           03 SW1 PIC 9.
           03 SISTEMAS PIC 9(08).
           03 INFORMAT PIC 9(08).
           03 CAPTURA PIC 9(08).
           03 TOTAL PIC 9(10).

       PROCEDURE DIVISION.
       00-INICIO.
	   SORT ORDEN
		ON ASCENDING KEY DEPTO-SS
		   DESCENDING KEY NOMBRE-SS
		USING ENTRADA
		GIVING SALIDA.

       01-ABRE.
	   OPEN INPUT SALIDA
	   OPEN OUTPUT REPORTE
	   PERFORM 700-FECHA
           WRITE LINEA FROM LINFECHA AFTER ADVANCING 2 LINES
           WRITE LINEA FROM LINEA-R3
           MOVE ZEROS TO VARS
           WRITE LINEA FROM ENCABEZADO1 AFTER ADVANCING 2 LINES
           WRITE LINEA FROM LINEA-R3
           WRITE LINEA FROM ENCABEZADO2
           WRITE LINEA FROM LINEA-R3
           PERFORM 02-LECTURA
           PERFORM 03-PROCESO UNTIL SW=1
           MOVE "SISTEMAS" TO DEPTO-R1
           MOVE SISTEMAS TO SUELDO-R1
           WRITE LINEA FROM LINEA-R1 
           WRITE LINEA FROM LINEA-R3
           COMPUTE TOTAL=SISTEMAS+INFORMAT+CAPTURA
           MOVE TOTAL TO TOTAL-R2
           WRITE LINEA FROM LINEA-R3
           WRITE LINEA FROM LINEA-R2
	   CLOSE SALIDA
	   CLOSE REPORTE
           STOP RUN.

       700-FECHA.
           ACCEPT FECHA FROM DATE
           MOVE DD TO DDF
           MOVE AA TO AAF
           EVALUATE MM
               WHEN 01
                   MOVE "ENERO" TO MMF
               WHEN 02
                   MOVE "FEBRERO" TO MMF
               WHEN 03
                   MOVE "MARZO" TO MMF
               WHEN 04
                   MOVE "ABRIL" TO MMF
               WHEN 05
                   MOVE "MAYO" TO MMF
               WHEN 06
                   MOVE "JUNIO" TO MMF
               WHEN 07
                   MOVE "JULIO" TO MMF
               WHEN 08
                   MOVE "AGOSTO" TO MMF
               WHEN 09
                   MOVE "SEPTIEMBRE" TO MMF
               WHEN 10
                   MOVE "OCTUBRE" TO MMF
               WHEN 11
                   MOVE "NOVIEMBRE" TO MMF
               WHEN 12
                   MOVE "DICIEMBRE" TO MMF
           END-EVALUATE.

       02-LECTURA.
           READ SALIDA AT END MOVE 1 TO SW.

       03-PROCESO.
           IF DEPTO-S="CAPTURA"
	       COMPUTE CAPTURA=SUELDO-S+CAPTURA
	       MOVE NOMBRE-S TO NOMBRE-R
	       MOVE DEPTO-S TO DEPTO-R
	       MOVE SUELDO-S TO SUELDO-R
               WRITE LINEA FROM LINEA-R
           ELSE
               MOVE "CAPTURA" TO DEPTO-R1
               MOVE CAPTURA TO SUELDO-R1
               IF SW1=0
                   WRITE LINEA FROM LINEA-R1
                   WRITE LINEA FROM LINEA-R3
                   WRITE LINEA FROM ENCABEZADO2
                   WRITE LINEA FROM LINEA-R3
                   MOVE 1 TO SW1
               END-IF
               IF DEPTO-S="INFORMATICA"
		   COMPUTE INFORMAT=INFORMAT+SUELDO-S
                   MOVE NOMBRE-S TO NOMBRE-R
                   MOVE DEPTO-S TO DEPTO-R
		   MOVE SUELDO-S TO SUELDO-R
                   WRITE LINEA FROM LINEA-R
               ELSE
                   MOVE "INFORMATICA" TO DEPTO-R1
                   MOVE INFORMAT TO SUELDO-R1
                   IF SW1=1
                       WRITE LINEA FROM LINEA-R1
                       WRITE LINEA FROM LINEA-R3
                       WRITE LINEA FROM ENCABEZADO2
                       WRITE LINEA FROM LINEA-R3
                       MOVE 2 TO SW1
                   END-IF
                   IF DEPTO-S="SISTEMAS"
		       COMPUTE SISTEMAS=SISTEMAS+SUELDO-S
                       MOVE NOMBRE-S TO NOMBRE-R
                       MOVE DEPTO-S TO DEPTO-R
		       MOVE SUELDO-S TO SUELDO-R
                       WRITE LINEA FROM LINEA-R
                   END-IF
               END-IF
	   END-IF
	   PERFORM 02-LECTURA.
