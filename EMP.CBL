       IDENTIFICATION DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "C:\COBOL\BIN\EMP.wks".
       SCREEN SECTION.
       COPY "C:\COBOL\BIN\EMP.ss".
       PROCEDURE DIVISION.
           DISPLAY G-EMP.
           ACCEPT G-EMP.
           STOP RUN.
