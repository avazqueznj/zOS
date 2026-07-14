
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


       DATA DIVISION.
       FILE SECTION.

       FD  SALE-FILE.
       01  SALE-RECORD.
           05 IN-BRANCH-NME           PIC X(20).
           05 IN-BROKER-NME           PIC X(25).
           05 IN-DEP-AMT              PIC 9(9)V99.
           05 FILLER                  PIC X(24).


       WORKING-STORAGE SECTION.

       01 WS-FORMAT                   PIC $$$,$$$,$$9.99 VALUE ZERO.

       01  WS-SALE-FILE-STATUS        PIC XX.
           88 SALE-FILE-OK            VALUE '00'.

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

           PERFORM 200-READ-SALE-FILE UNTIL END-OF-FILE.

           PERFORM 300-CLOSE-SALE-FILE.

           STOP RUN.


       100-OPEN-SALE-FILE.
           DISPLAY 'OPENING SALE FILE...'.

           OPEN INPUT SALE-FILE.

           IF NOT SALE-FILE-OK
              DISPLAY 'FATAL: SALE FILE OPEN ERROR: '
                   WS-SALE-FILE-STATUS
              STOP RUN
           END-IF.



       200-READ-SALE-FILE.
           READ SALE-FILE
               AT END
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   PERFORM 210-DISPLAY-SALES-RECORD

           END-READ.


       210-DISPLAY-SALES-RECORD.
           MOVE IN-DEP-AMT TO WS-FORMAT.

           DISPLAY
              IN-BRANCH-NME SPACE
              IN-BROKER-NME SPACE
              WS-FORMAT.


       300-CLOSE-SALE-FILE.
           DISPLAY 'CLOSING FILE...'.

           CLOSE SALE-FILE

           IF NOT SALE-FILE-OK
               DISPLAY 'FATAL: SALE FILE CLOSE ERROR: '
                   WS-SALE-FILE-STATUS
              STOP RUN
           END-IF.


       END PROGRAM SALESRPT.