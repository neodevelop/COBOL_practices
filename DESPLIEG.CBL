      *JOSE JUAN REYES ZU�IGA
       IDENTIFICATION DIVISION.
       PROGRAM-ID.DESPLIEG.
       AUTHOR.ZU�IGA.
       DATE-WRITTEN.14-08-00.
       DATE-COMPILED.14-08-00.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.IBM.
       OBJECT-COMPUTER.IBM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 VARS.
	   03 X PIC 999.
	   03 Y PIC 999.
	   03 Z PIC 99.
	   03 CONT PIC 99.
       01 OPC PIC X.
       PROCEDURE DIVISION.
       000-INICIO.
	   DISPLAY ERASE.
	   MOVE 10 TO CONT.
	   PERFORM 100-DESPLIEGA UNTIL Z>40
	   STOP RUN.

       100-DESPLIEGA.
	   DISPLAY Z LINE Y COL X.
	   ADD 1 TO Z.
	   ADD 1 TO Y.
	   IF Z=CONT
	   THEN
	     ADD 10 TO CONT
	     ADD 4 TO X
	     MOVE ZEROS TO Y
	   END-IF
