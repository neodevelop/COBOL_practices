      *JOSE JUAN REYES ZU�IGA
       IDENTIFICATION DIVISION.
       PROGRAM-ID.OPERARIF.
       AUTHOR.ZU�IGA.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.IBM.
       OBJECT-COMPUTER.IBM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUMS.
	   03 VARE1 PIC ZZZ.ZZ.
	   03 VARE2 PIC ZZZ.ZZ.
	   03 RESE PIC -ZZZZ.ZZ.
       01 VARS.
	   03 VAR1 PIC 999V99 VALUE ZEROS.
	   03 VAR2 PIC 999V99 VALUE ZEROS.
	   03 RES PIC -9999V99 VALUE ZEROS.
	   03 OPC1 PIC 9 VALUE ZEROS.
       01 OPCS.
	   03 OPC2 PIC X.
       PROCEDURE DIVISION.
       000-INICIO.
	   PERFORM 100-PROCESO UNTIL OPC2="N" OR "n"
	   STOP RUN.

       100-PROCESO.
	   DISPLAY ERASE.
	   DISPLAY "MENU DE OPCIONES" AT 0510.
	   DISPLAY "1.SUMA" AT 0710.
	   DISPLAY "2.RESTA" AT 0910.
	   DISPLAY "3.MULTIPLICACION" AT 1110.
	   DISPLAY "4.DIVISION" AT 1310.
	   DISPLAY "ELIJA SU OPCION:" AT 1510.
	   ACCEPT OPC1 AT 1710.
	   IF OPC1=1
	   THEN
	     DISPLAY ERASE
	     DISPLAY "DAME EL PRIMER NUMERO" AT 0510
	     ACCEPT VARE1 AT 0610
	     DISPLAY "DAME EL SEGUNDO NUMERO" AT 0810
	     ACCEPT VARE2 AT 0910
	     PERFORM 050-MOVER1
	     COMPUTE RES=VAR1 + VAR2
	     PERFORM 075-MOVER2
	     DISPLAY VARE1 AT 1110
	     DISPLAY "+" AT 1113
	     DISPLAY VARE2 AT 1114
	     DISPLAY "=" AT 1117
	     DISPLAY RESE AT 1118
	     ELSE
	     IF OPC1=2
	     THEN
	       DISPLAY ERASE
	       DISPLAY "DAME EL PRIMER NUMERO" AT 0510
	       ACCEPT VARE1 AT 0610
	       DISPLAY "DAME EL SEGUNDO NUMERO" AT 0810
	       ACCEPT VARE2 AT 0910
	       PERFORM 050-MOVER1
	       COMPUTE RES=VAR1 - VAR2
	       PERFORM 075-MOVER2
	       DISPLAY VARE1 AT 1110
	       DISPLAY "-" AT 1113
	       DISPLAY VARE2 AT 1114
	       DISPLAY "=" AT 1117
	       DISPLAY RESE AT 1118
	       ELSE
	       IF OPC1=3
	       THEN
		 DISPLAY ERASE
		 DISPLAY "DAME EL PRIMER NUMERO" AT 0510
		 ACCEPT VARE1 AT 0610
		 DISPLAY "DAME EL SEGUNDO NUMERO" AT 0810
		 ACCEPT VARE2 AT 0910
		 PERFORM 050-MOVER1
		 COMPUTE RES=VAR1 * VAR2
		 PERFORM 075-MOVER2
		 DISPLAY VARE1 AT 1110
		 DISPLAY "*" AT 1113
		 DISPLAY VARE2 AT 1114
		 DISPLAY "=" AT 1117
		 DISPLAY RESE AT 1118
		 ELSE
		 IF OPC1=4
		 THEN
		   DISPLAY ERASE
		   DISPLAY "DAME EL DIVIDENDO" AT 0510
		   ACCEPT VARE1 AT 0610
		   DISPLAY "DAME EL DIVISOR" AT 0810
		   ACCEPT VARE2 AT 0910
		   PERFORM 050-MOVER1
		   COMPUTE RES=VAR1 / VAR2
		   PERFORM 075-MOVER2
		   DISPLAY VARE1 AT 1110
		   DISPLAY "*" AT 1113
		   DISPLAY VARE2 AT 1114
		   DISPLAY "=" AT 1117
		   DISPLAY RESE AT 1118
		   ELSE
		     DISPLAY ERASE
		     DISPLAY "ERROR VERIFIQUE SU OPCION" AT 3010
		 END-IF
	       END-IF
	     END-IF
	   END-IF

	   DISPLAY "DESEA HACER OTRA OPERACION S/N" AT 3110.
	   ACCEPT OPC2 AT 3210.

	   050-MOVER1.
	     MOVE VARE1 TO VAR1.
	     MOVE VARE2 TO VAR2.
	   075-MOVER2.
	     MOVE RES TO RESE.
