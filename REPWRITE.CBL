       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPWRITE.
       AUTHOR. ULISES.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       OBJECT-COMPUTER. HP.
       SOURCE-COMPUTER. HP.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	    SELECT SAL	ASSIGN TO "SAL".
	    SELECT REPO ASSIGN TO "REPO".
       DATA DIVISION.
       FILE SECTION.
       FD SAL.
       01 REGISTRO.
	   02 IDD  PIC X(5).
	   02 APAT PIC X(12).
	   02 AMAT PIC X(12).
	   02 NOM  PIC X(12).
	   02 EDAD PIC 99.
	   02 SEX  PIC X.
	   02 TEL PIC 9(8).
	   02 ESP  PIC X(16).
       FD REPO
	    REPORT IS REPOR.
	    01 LINEA PIC X(80).

       WORKING-STORAGE SECTION.
       01 W-ALFAS.
	    03 W-OPCION 	PIC X VALUE SPACES.
       01 CUENTAS		PIC 999 VALUE ZEROS.
       REPORT SECTION.
       RD REPOR
	    CONTROLS ARE FINAL DIRECCION DEPTO
	    PAGE LIMIT IS 55 LINES
	    HEADING 2
	    FIRST DETAIL 5
	    LAST DETAIL 30
	    FOOTING 52.
       01 TYPE IS PH.
	    03 LINE 2.
		05 COLUMN 31 PIC X(19) VALUE "REPORTE DE DOCTORES".
		05 COLUMN 55 PIC X(06) VALUE "PAGINA".
		05 COLUMN 65 PIC ZZ SOURCE PAGE-COUNTER.
	    03 LINE 3.
		05 COLUMN 10 PIC X(03) VALUE "RFC".
		05 COLUMN 28 PIC X(09) VALUE "DIRECCION".
		05 COLUMN 49 PIC X(12) VALUE "DEPARTAMENTO".
		05 COLUMN 71 PIC X(06) VALUE "SUELDO".
	    03 LINE 4.
		05 COLUMN 1 PIC X(80) VALUE ALL "*".

       01 LINDET TYPE IS DE.
	    03 LINE PLUS 1.
		05 COLUMN 7  PIC X(10) SOURCE RFC.
		05 COLUMN 23 PIC X(20) SOURCE DIRECCION.
		05 COLUMN 49 PIC X(15) SOURCE DEPTO.
		05 COLUMN 70 PIC $9999 SOURCE SUELDO.

       01 TYPE IS CF DEPTO.
	    03 LINE NUMBER IS PLUS 1.
		05 COLUMN 30 PIC X(21) VALUE "TOTAL DE DEPARTAMENTO".
		05 TOTDIR COLUMN 55 PIC $$$,$$$.99
		    SUM SUELDO RESET ON DEPTO.

       01 TYPE IS CF DIRECCION.
	    03 LINE NUMBER IS PLUS 1.
		05 COLUMN 30 PIC X(15) VALUE "TOTAL DIRECCION".
		05 TOTDIR COLUMN 55 PIC $$$,$$$.99
		    SUM SUELDO RESET ON DIRECCION.

       01 TYPE IS CF FINAL.
	    03 LINE PLUS 1.
		05 COLUMN 30 PIC X(15) VALUE "GRAN TOTAL".
		05 COLUMN 55 PIC $$,$$$,$$$.99 SUM SUELDO.

       01 TYPE IS PF.
	    03 LINE 53.
		05 COLUMN 1 PIC X(80) VALUE ALL"*".
	    03 LINE 54.
		05 COLUMN 5 PIC X(20) VALUE "LIC. JOSE JUAN REYES".

       PROCEDURE DIVISION.
       000-RUT-INICIO.
	    PERFORM 100-INICIO
	    PERFORM 200-PROCESO UNTIL W-OPCION='S'
	    PERFORM 500-FINAL
	    STOP RUN.

       100-INICIO.
	    OPEN INPUT SAL OUTPUT REPO
	    PERFORM 300-LECTURA
	    INITIATE REPOR.

       200-PROCESO.
	    GENERATE LINDET
	    PERFORM 300-LECTURA.

       300-LECTURA.
	    READ SAL AT END MOVE "S" TO W-OPCION.

       500-FINAL.
	    TERMINATE REPOR
	    CLOSE SAL REPO
	    STOP RUN.
