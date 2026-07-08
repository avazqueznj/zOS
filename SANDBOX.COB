

       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ADDTWO.
       AUTHOR.        A. VAZQUEZ.
       DATE-WRITTEN.  07/07/2026.

      ****************************************************************
      *                                                              *
      *  PROGRAM NAME:      ADDTWO.                                  *
      *                                                              *
      *  PURPOSE:           EMMA deliverable for assignment 3        *
      *                     Compute and display addition of          *
      *                     two numbers                              *
      *                                                              *
      *  INPUT:                                                      *
      *                                                              *
      *  OUTPUT:            Will DISPLAY result of adding 2 numbers   *
      *                                                              *
      *                                                              *
      *  I/O:                                                        *
      *                                                              *
      *                                                              *
      *  ENTRY/EXIT CONDS:  NONE.                                    *
      *                                                              *
      *  NOTES:                                                      *
      *                                                              *
      *                                                              *
      ****************************************************************
       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 WS-NUM1         PIC 9(5)       VALUE ZERO.
       01 WS-NUM2         PIC 9(5)       VALUE ZERO.
       01 WS-SUM          PIC 9(6)V99    VALUE ZERO.

       01 WS-FORMAT       PIC ZZ,ZZ9.99  VALUE ZERO.

       LOCAL-STORAGE SECTION.

       PROCEDURE DIVISION.

      *************************************************************
      ** MAIN                                                     *
      *************************************************************
       000-MAIN.

           PERFORM 100-ADDTWO.
           PERFORM 110-DISPLAY-RESULTS.

           STOP RUN.

      *************************************************************
      ** Add 2 numbers                                            *
      *************************************************************
       100-ADDTWO.

           MOVE 123 TO WS-NUM1.
           MOVE 345 TO WS-NUM2.
           COMPUTE WS-SUM = WS-NUM1 + WS-NUM2.

      *************************************************************
      ** Display results                                          *
      *************************************************************
       110-DISPLAY-RESULTS.

           DISPLAY "The sum of ".

           MOVE WS-NUM1 TO WS-FORMAT.
           DISPLAY WS-FORMAT.

           DISPLAY " +  ".

           MOVE WS-NUM2 TO WS-FORMAT.
           DISPLAY WS-FORMAT.

           MOVE WS-SUM TO WS-FORMAT.
           DISPLAY "----------------".
           DISPLAY WS-FORMAT.



       END PROGRAM ADDTWO.

