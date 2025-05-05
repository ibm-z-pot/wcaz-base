      *****************************************************************
      * COBOL Java interoperability PROGA -> PROGB -> PROGC
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGC.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01  WORK-FIELDS.
           05  WS-BYE-MSG    PIC X(32)    VALUE 'Goodbye from COBOL'.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05  PROGB-MESSAGE PIC X(32).
      *
      *
       PROCEDURE DIVISION.
           MOVE WS-BYE-MSG         TO PROGB-MESSAGE.
      *
           EXEC CICS RETURN END-EXEC.
