      *****************************************************************
      * COBOL Java interoperability PROGA -> PROGB -> PROGC
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGB.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01  WORK-FIELDS.
           05  WS-HELLO-MSG  PIC X(32)    VALUE 'Hello from COBOL'.
           05  WS-BYE-MSG    PIC X(32)    VALUE 'Goodbye from COBOL'.
       01  CA.
           05  PROGC-MESSAGE PIC X(32).
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05  PROGA-MESSAGE PIC X(32).
      *
      *
       PROCEDURE DIVISION.
           MOVE WS-HELLO-MSG       TO PROGC-MESSAGE.
           DISPLAY 'PROGB  request= ' PROGC-MESSAGE.
           EXEC CICS LINK
                     PROGRAM('PROGC')
                     COMMAREA(CA)
           END-EXEC.
           DISPLAY 'PROGC response= ' PROGC-MESSAGE.
           MOVE WS-BYE-MSG         TO PROGA-MESSAGE.
      *
           EXEC CICS RETURN END-EXEC.
