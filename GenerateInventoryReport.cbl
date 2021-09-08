        IDENTIFICATION DIVISION.
        PROGRAM-ID. InventoryReportCreator.
        AUTHOR. Daniel Lengler.
      * David Tang and Naimul Raman's Project 3 was used as a base     
      
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT INVENT-FILE-IN
                ASSIGN TO "C:\temp\INVENT6.TXT"
                ORGANIZATION IS INDEXED
                ACCESS MODE SEQUENTIAL
                RECORD KEY IS PART-NUMBER-IN-PK.
           SELECT INVENT-REPORT-OUT
                ASSIGN TO "C:\temp\INVREPRT.TXT"
                ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INVENT-FILE-IN.
       01  INVENTORY-RECORD-IN.
           COPY IndexedInventRecordStr.cpy IN copy-lib.
       
       FD  INVENT-REPORT-OUT.
       01  INVENT-RECORD-OUT    PIC X(55).
      
       WORKING-STORAGE SECTION.
       01  DAY-NAMES.
           05  FILLER  PIC X(3) VALUE "MON".
           05  FILLER  PIC X(3) VALUE "TUE".
           05  FILLER  PIC X(3) VALUE "WED".
           05  FILLER  PIC X(3) VALUE "THR".
           05  FILLER  PIC X(3) VALUE "FRI".
           05  FILLER  PIC X(3) VALUE "SAT".
           05  FILLER  PIC X(3) VALUE "SUN".
       
       01  DAY-TABLE REDEFINES DAY-NAMES.
           05 DAY-NAME OCCURS 7 TIMES PIC X(3).
       
       01  DATE-IN.
           10 YEAR-IN  PIC 9(2).
           10 MONTH-IN PIC 9(2).
           10 DAY-IN   PIC 9(2).
       
       01  DAY-OF-WEEK-INT PIC 9(1).    
           
       01  INV-TITLE.
           05  FILLER            PIC X(7) VALUE SPACES.
           05  FILLER            PIC X(16)  VALUE "INVENTORY REPORT".
           05  FILLER            PIC X(4) VALUE SPACES.
           05  DAY-WEEK          PIC X(3).
           05  FILLER            PIC X VALUE SPACES.
           05  DAY-YEAR          PIC 9(2).
           05  FILLER            PIC X VALUE SPACES.
           05  DAY-MONTH         PIC 9(2).
           05  FILLER            PIC X VALUE SPACES.
           05  DAY-DAY           PIC 9(2).
           05  FILLER            PIC X VALUE SPACES.
           
       01  INVENT-RECORD-DETAIL.
           05  PART-NUMBER-OUT   PIC ZZZZZZ9.
           05  FILLER            PIC X(4) VALUE SPACES.
           05  PART-NAME-OUT     PIC X(20).
           05  FILLER            PIC X(4) VALUE SPACES.
           05  QTY-ON-HAND-OUT   PIC ZZZ9.
           05  FILLER            PIC X(2)  VALUE SPACES.
           05  STOCK-VALUE-OUT-F   PIC $,$$$,$$9.99.

       01  INV-COLUMN-HEADER.
           05  FILLER  PIC X(10)  VALUE   "PARTNUMBER".
           05  FILLER  PIC X(4)   VALUE   SPACES.
           05  FILLER  PIC X(8)   VALUE   "PARTNAME".
           05  FILLER  PIC X(13)   VALUE   SPACES.
           05  FILLER  PIC X(8)   VALUE   "QUANTITY".
           05  FILLER  PIC X(2)   VALUE   SPACES.
           05  FILLER  PIC X(5)    VALUE   "VALUE".
           
       01  AUDIT-TRAIL.
           05  FILLER           PIC X(11) VALUE "TOTAL VALUE".
           05  FILLER           PIC X(2)  VALUE  SPACES.
           05  TOT-INV-VAL-F    PIC $$$,$$$,$$9.99 VALUE ZERO.
           05  FILLER           PIC X(2)  VALUE SPACES.
           05  FILLER           PIC X(4)  VALUE "READ".
           05  FILLER           PIC X(2)  VALUE SPACES.
           05  READ-COUNTER-F   PIC Z(3)9  VALUE ZERO.
           05  FILLER           PIC X(2)  VALUE SPACES.
           05  FILLER           PIC X(7)  VALUE "WRITTEN".
           05  FILLER           PIC X(2)  VALUE SPACES.
           05  WRITE-COUNTER-F  PIC Z(3)9  VALUE ZERO.

       01  SUMMARY-DATA.
           05  TOTAL-INV-VALUE  PIC 9(8)V99 VALUE ZEROS.               
           05  READ-COUNTER     PIC 9(4)  VALUE ZERO.
           05  WRITE-COUNTER    PIC 9(4)  VALUE ZERO.
           05  STOCK-VALUE-OUT  PIC 9(6)V99 VALUE ZEROS.                
           
       01  FLAGS-AND-COUNTERS.
           05  INVENT-EOF-FLAG     PIC X(3) VALUE "NO".


       PROCEDURE DIVISION.
       100-PRODUCE-INVENTORY-REPORT.
           PERFORM 201-INIT-INVENTORY-REPORT.
           PERFORM 202-PRODUCE-INV-DETAIL-RECORD
               UNTIL INVENT-EOF-FLAG = "YES".
           PERFORM 203-TERM-INVENTORY-REPORT.
           EXIT PROGRAM.

       201-INIT-INVENTORY-REPORT.
           PERFORM 301-OPEN-INV-FILES.
           PERFORM 304-READ-INV-RECORD.
           PERFORM 302-WRITE-COL-HEADERS.

       202-PRODUCE-INV-DETAIL-RECORD.
           PERFORM 303-CALCULATE-INV-VALUE.
           PERFORM 305-CALCULATE-TOTAL-INV-VALUE.                   
           PERFORM 306-WRITE-INVENTORY-DETAIL.
           PERFORM 304-READ-INV-RECORD.

       203-TERM-INVENTORY-REPORT.
           PERFORM 307-WRITE-AUDIT-TRAIL.
           PERFORM 308-CLOSE-INV-FILES.

       301-OPEN-INV-FILES.
           OPEN INPUT  INVENT-FILE-IN
                OUTPUT INVENT-REPORT-OUT.

       302-WRITE-COL-HEADERS.
           ACCEPT DATE-IN FROM DATE.
           MOVE YEAR-IN TO DAY-YEAR.
           MOVE MONTH-IN TO DAY-MONTH.
           MOVE DAY-IN TO DAY-DAY.
           ACCEPT DAY-OF-WEEK-INT FROM DAY-OF-WEEK.
           MOVE DAY-NAME(DAY-OF-WEEK-INT) TO DAY-WEEK.
           
           WRITE INVENT-RECORD-OUT FROM INV-TITLE.
           WRITE INVENT-RECORD-OUT FROM INV-COLUMN-HEADER.  

       303-CALCULATE-INV-VALUE.
       MULTIPLY QTY-ON-HAND  BY UNIT-PRICE-IN
               GIVING  STOCK-VALUE-OUT.
           MOVE STOCK-VALUE-OUT TO STOCK-VALUE-OUT-F.

       304-READ-INV-RECORD.
           READ INVENT-FILE-IN
              AT END  MOVE "YES" TO INVENT-EOF-FLAG
                NOT AT END ADD 1 TO READ-COUNTER.

       305-CALCULATE-TOTAL-INV-VALUE.
           ADD  STOCK-VALUE-OUT TO TOTAL-INV-VALUE.

       306-WRITE-INVENTORY-DETAIL.
           MOVE PART-NUMBER-IN-PK TO PART-NUMBER-OUT.
           MOVE PART-NAME         TO PART-NAME-OUT.
           MOVE QTY-ON-HAND       TO QTY-ON-HAND-OUT.
           IF WRITE-COUNTER NOT EQUAL 0
               AND FUNCTION MOD (WRITE-COUNTER, 10) = 0
                   WRITE INVENT-RECORD-OUT FROM INV-COLUMN-HEADER
                       AFTER ADVANCING PAGE
           END-IF.
           WRITE INVENT-RECORD-OUT FROM INVENT-RECORD-DETAIL.
           ADD 1 TO WRITE-COUNTER.

       307-WRITE-AUDIT-TRAIL.
           MOVE TOTAL-INV-VALUE TO TOT-INV-VAL-F.
           MOVE READ-COUNTER TO READ-COUNTER-F.
           MOVE WRITE-COUNTER TO WRITE-COUNTER-F.
           WRITE INVENT-RECORD-OUT FROM AUDIT-TRAIL.
       
       308-CLOSE-INV-FILES.
           CLOSE INVENT-FILE-IN INVENT-REPORT-OUT.