       special-names.
	   crt status is key-status.
       working-storage section.
       01 set-bit-pairs 	      pic 9(02) comp-x value 1.
       01 adis-key-control.
	    03 adis-key-setting       pic 9(02) comp-x.
	    03 filler		      pic x value "2".
	    03 first-adis-key	      pic 9(02) comp-x.
	    03 number-of-adis-keys    pic 9(02) comp-x.
       01 key-status.
	    03 key-type 	pic x.
	    03 key-code-1	pic 9(02)     comp-x.
	    03 key-code-2	pic 9(02)     comp-x.
       01 data-item		pic x.
       01 espera		pic x.
       01 X			PIC 99 VALUE ZEROS.
       01 Y			PIC 99 VALUE ZEROS.
       procedure division.
	   display erase
	   display "hola" AT 0305
	   MOVE 04 TO X
	   display "PATY" AT 0405 with background-color is 6
	   DISPLAY "SELENE" AT 0505 with background-color is 0
	   DISPLAY "ESPERANZA" AT 0605 with background-color is 0
	   DISPLAY "JUANA    " AT 0705 with background-color is 0
	   move 3 to adis-key-setting
	   move 5 to first-adis-key
	   move 2 to number-of-adis-keys
	   call x"af" using set-bit-pairs adis-key-control
	   PERFORM UNTIL key-type = 0 and key-code-1 = 48
	   accept data-item at 0101 with NO-ECHO
	   if key-type = "2"
	    evaluate key-code-1
	     when 5
	       SUBTRACT  1 FROM X
	       IF X < 4
		 MOVE 7 TO X
	       END-IF
	       display x at 2050
	       if x = 4
		 display "PATY" at 0405 with background-color is 6
		 display "SELENE" at 0505 with background-color is 0
	       else
		 if x = 5
		  display "SELENE" at 0505 with background-color is 6
		  display "ESPERANZA" at 0605 WITH BACKGROUND-COLOR IS 0
		 else
		   if x = 6
		      display "ESPERANZA"  at 0605
				WITH BACKGROUND-COLOR IS 6
		      display "JUANA" AT 0705
				WITH BACKGROUND-COLOR IS 0
		   else
		      if x=7
			display "PATY"	at 0405
			    WITH BACKGROUND-COLOR 0
			display "JUANA" at 0705
			  with BACKGROUND-COLOR 6
		      end-if
			   end-if
			 end-if
		      end-if
	     when 6
	       ADD 1 TO X
	       IF X > 7
		 MOVE 4 TO X
	       END-IF
	       display x at 2050
	       if x = 4
		 display "PATY" at 0405 with background-color is 6
		 display "JUANA" at 0705 with background-color is 0
	       else
		 if x = 5
		  display "SELENE" at 0505 with background-color is 6
		  display "PATY" at 0405 WITH BACKGROUND-COLOR IS 0
		 else
		   if x = 6
		      display "SELENE"	 at 0505
				WITH BACKGROUND-COLOR IS 0
		      display "ESPERANZA" AT 0605
				WITH BACKGROUND-COLOR IS 6
		   else
		      if x=7
			display "ESPERANZA"  at 0605
			    WITH BACKGROUND-COLOR 0
			display "JUANA" at 0705
			  with BACKGROUND-COLOR 6
		      end-if
			   end-if
			 end-if
		      end-if
		  when 48
		      display "tecleaste enter"
		  when other
		      display key-code-1
		      display "ninguna2"
	       end-evaluate
	     else
	       if key-type = "0"
		    if key-code-1 = 48
			display "tecleaste enter"
			IF X = 4
			   DISPLAY "TECLEASTE EN PATY " AT 2010
			ELSE
			   IF X = 5
			      DISPLAY "TECLEASTE EN SELENE" AT 2010
			   ELSE
			      IF X = 6
			       DISPLAY "TECLEASTE EN ESPERANZA" AT 2010
			      ELSE
				DISPLAY "TECLEASTE EN JUANA" AT 2010
			      END-IF
			   END-IF
			END-IF
		    end-if
	       end-if
	     end-if
	   end-perform
	   ACCEPT ESPERA
	   stop run.
