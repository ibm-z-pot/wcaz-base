      *****************************************************************
      * COBOL Java interoperability PROGA -> PROGB -> PROGC
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGA.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01  WORK-FIELDS.
           05  WS-MESSAGE    PIC X(32)    VALUE 'Hello from COBOL'.
       01  CA.
           05  CA-MESSAGE    PIC X(32).
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05  FILLER              PIC X OCCURS 1 TO 24576
                                    DEPENDING ON EIBCALEN.
      *
       PROCEDURE DIVISION.
           MOVE WS-MESSAGE TO CA-MESSAGE.
           DISPLAY 'PROGA  request= ' CA-MESSAGE.
           EXEC CICS LINK
                     PROGRAM('PROGB')
                     COMMAREA(CA)
           END-EXEC.
           DISPLAY 'PROGB response= ' CA-MESSAGE.
      *
           EXEC CICS RETURN END-EXEC.
