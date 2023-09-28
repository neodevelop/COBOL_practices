      $set noosvs mf novsc2

       IDENTIFICATION DIVISION.
       special-names.
	 crt status is key-status.
       data division.
       working-storage section.
       01 use-mouse-function pic 9(02) comp-x value 64.
       01 usage-parameter pic 9(02) comp-x.

       01 enable-mouse-function pic 9(02) comp-x value 66.
       01 enable-parameter pic 9(02) comp-x.
       01 get-mouse-details pic 9(02) comp-x value 67.
       01 mouse-details.
	  03 mouse-x-position pic 9(04) comp-x.
	  03 mouse-y-position pic 9(04) comp-x.
	  03 mouse-status pic 9(04) comp-x.

       01 data-item pic x.

       01 adis-key-control.
	  03 adis-key-setting pic 9(02) comp-x.
	  03 filler pic x value "2".
	  03 first-adis-key pic 9(02) comp-x.
	  03 number-of-adis-keys pic 9(02) comp-x.

       01 set-bit-pairs pic 9(02) comp-x value 1.
       01 key-status.
	  03 key-type pic x.
	  03 key-code-1 pic 9(02) comp-x.
	  03 key-code-2 pic 9(02) comp-x.
       01 espera pic x.
       procedure division.
	   display erase
	   display "a" at 0505
	   move 1 to usage-parameter
	   call x"af" using use-mouse-function usage-parameter
	   move 3 to adis-key-setting
	   move 27 to first-adis-key
	   move 2 to number-of-adis-keys
	   call x"af" using set-bit-pairs adis-key-control
	   accept data-item at 0101
	   if key-type = "2" and key-code-1 =27
	       display "the mouse terminated accept "
	       call x"af" using get-mouse-details mouse-details
	       display "mouse-x-position is " at line 3 column 1
	       display mouse-x-position at line 3 column 22
	       display "mouse-y-position is " at line 4 column 1
	       display mouse-y-position at line 4 column 22
	       if mouse-x-position = 4 and mouse-y-position = 4
		  call "LLAMADA"
	       end-if
	   else
	       display "no pase por aqui".
	   move 1 to usage-parameter
	   call x"af" using use-mouse-function usage-parameter.
	   display "a" at 0350
	   display key-type at 0352
	   display "b" at 0450
	   display key-code-1 at 0452
	   accept espera
	   stop run.
