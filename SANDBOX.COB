
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SALESRPT.
       AUTHOR.        A. VAZQUEZ.
       DATE-WRITTEN.  07/13/2026.

      ****************************************************************
      *                                                              *
      *  PROGRAM NAME:      SALESRPT.                                *
      *                                                              *
      *  PURPOSE:                                                    *
      *                                                              *
      *                                                              *
      *                                                              *
      *  INPUT:                                                      *
      *                                                              *
      *  OUTPUT:                                                     *
      *                                                              *
      *                                                              *
      *  ENTRY/EXIT CONDS:                                           *
      *                                                              *
      *  NOTES:                                                      *
      *                                                              *
      *                                                              *
      ****************************************************************

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT SALE-FILE
               ASSIGN TO SALEFILE
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-SALE-FILE-STATUS.

           SELECT REPORT-FILE
               ASSIGN TO REPORTFL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-REPORT-FILE-STATUS.


       DATA DIVISION.
       FILE SECTION.

       FD  SALE-FILE.
       01  SALE-RECORD.
           05 IN-BRANCH-NME           PIC X(20).
           05 IN-BROKER-NME           PIC X(25).
           05 IN-DEP-AMT              PIC 9(9)V99.
           05 FILLER                  PIC X(24).

       FD  REPORT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS.

       01  REPORT-RECORD.
           05 OUT-BRANCH-NME          PIC X(20).
           05 FILLER                  PIC X(8).
           05 OUT-BROKER-NME          PIC X(25).
           05 FILLER                  PIC X(9).
           05 OUT-SALE-REC            PIC X(14).
           05 FILLER                  PIC X(56).


       WORKING-STORAGE SECTION.

       01 WS-FORMAT                   PIC $$$,$$$,$$9.99 VALUE ZERO.

       01  WS-SALE-FILE-STATUS        PIC XX.
           88 SALE-FILE-OK            VALUE '00'.

       01  WS-REPORT-FILE-STATUS      PIC XX.
           88 REPORT-FILE-OK          VALUE '00'.

      ** FILE READ STATUS FLAG AND 88 VALUES **
       01  WS-END-OF-FILE             PIC X VALUE 'N'.
           88 END-OF-FILE             VALUE 'Y'.
           88 NOT-END-OF-FILE         VALUE 'N'.


       LOCAL-STORAGE SECTION.


       PROCEDURE DIVISION.

      *************************************************************
      ** MAIN                                                     *
      *************************************************************
       000-MAIN.
           DISPLAY 'SALESRPT STARTING... V2'.

           PERFORM 100-OPEN-SALE-FILE.

           PERFORM 110-OPEN-REPORT-FILE.

           PERFORM 200-READ-SALE-FILE UNTIL END-OF-FILE.

           PERFORM 300-CLOSE-SALE-FILE.

           PERFORM 310-CLOSE-REPORT-FILE.

           STOP RUN.


       100-OPEN-SALE-FILE.
           DISPLAY 'OPENING SALE FILE...'.

           OPEN INPUT SALE-FILE.

           IF NOT SALE-FILE-OK
              DISPLAY 'FATAL: SALE FILE OPEN ERROR: '
                   WS-SALE-FILE-STATUS
              STOP RUN
           END-IF.

       110-OPEN-REPORT-FILE.
           DISPLAY 'OPENING REPORT FILE...'.

           OPEN OUTPUT REPORT-FILE.

           IF NOT REPORT-FILE-OK
              DISPLAY 'FATAL: REPORT FILE OPEN ERROR: '
                   WS-REPORT-FILE-STATUS
              STOP RUN
           END-IF.


       200-READ-SALE-FILE.
           READ SALE-FILE
               AT END
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   PERFORM 210-PROCESS-SALES-RECORD

           END-READ.


       210-PROCESS-SALES-RECORD.

      ** Log record **
           MOVE IN-DEP-AMT TO WS-FORMAT.
           DISPLAY
              IN-BRANCH-NME SPACE
              IN-BROKER-NME SPACE
              WS-FORMAT.

      ** save record **
           MOVE SPACES TO REPORT-RECORD.

           MOVE IN-BRANCH-NME TO OUT-BRANCH-NME.
           MOVE IN-BROKER-NME TO OUT-BROKER-NME.
           MOVE IN-DEP-AMT    TO WS-FORMAT.
           MOVE WS-FORMAT     TO OUT-SALE-REC.

           WRITE REPORT-RECORD

           IF NOT REPORT-FILE-OK
               DISPLAY 'FATAL: REPORT FILE WRITE ERROR: '
                   WS-REPORT-FILE-STATUS
               STOP RUN
           END-IF.


       300-CLOSE-SALE-FILE.
           DISPLAY 'CLOSING SALES FILE...'.

           CLOSE SALE-FILE

           IF NOT SALE-FILE-OK
               DISPLAY 'FATAL: SALE FILE CLOSE ERROR: '
                   WS-SALE-FILE-STATUS
              STOP RUN
           END-IF.

       310-CLOSE-REPORT-FILE.
           DISPLAY 'CLOSING REPORT FILE...'.

           CLOSE REPORT-FILE

           IF NOT REPORT-FILE-OK
               DISPLAY 'FATAL: REPORT FILE CLOSE ERROR '
                   WS-REPORT-FILE-STATUS
              STOP RUN
           END-IF.


       END PROGRAM SALESRPT.

