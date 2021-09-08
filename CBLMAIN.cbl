      ******************************************************************
   	  * This program is to:
      *      - convert Master Inventory Sequential File
      *        to Indexed Sequencial File.
      *
      *      - convert Supplier Sequential File
      *        to Supplier Indexed File.
      *
      *      - call routine to create Inventory Report File  
   	  *
      *      - call routine to create Re-order Report File
      *
      ******************************************************************
      * M A I N      M O D U L E *
      ******************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 CBLMAIN.
       AUTHOR.                     Maycon Morais.
       DATE-WRITTEN.               APRIL 01 2020.
	   DATE-COMPILED.              SEP 02 2021.

       ENVIRONMENT                       DIVISION.
       INPUT-OUTPUT                      SECTION.
       
       FILE-CONTROL.
       SELECT  INVENT-FILE-IN            
               ASSIGN TO "C:\temp\INVENT.TXT"
               ORGANIZATION LINE SEQUENTIAL.
       
       SELECT  SUPPLIERS-FILE-IN
               ASSIGN TO "C:\temp\SUPPLIERS4.TXT"
               ORGANIZATION LINE SEQUENTIAL.
               
       SELECT INVENT6
               ASSIGN TO "C:\temp\INVENT6.TXT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PART-NUMBER-IN-PK.
               
       SELECT SUPPLIERI
               ASSIGN TO "C:\temp\SUPPLIERI.TXT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS SUPPLIER-CODE-OUT-PK.

       DATA                              DIVISION.
       FILE                              SECTION.
       FD  INVENT-FILE-IN.
       01  INVENTORY-RECORD-IN.
           COPY 'InventRecordStr'        IN copy-lib.
       
       FD  SUPPLIERS-FILE-IN.
       01  SUPPLIER-RECORD-IN.
           COPY 'SupplierStr'            IN copy-lib.
       
       FD  INVENT6.    
       01  INVENTORY-RECORD-OUT.
           COPY 'IndexedInventRecordStr' IN copy-lib.
       
       FD  SUPPLIERI.
       01  SUPPLIER-RECORD-OUT.
           COPY 'IndexedSupplierStr'     IN copy-lib.
           
       WORKING-STORAGE SECTION.
       01  FLAGS-AND-COUNTERS.
           05  INVENT-EOF-FLAG           PIC X(03) VALUE "NO".
           05  SUPPLIERS-EOF-FLAG        PIC X(03) VALUE "NO".
           05  READ-OK-SWITCH            PIC X(03) VALUE "NO".
           05  CONT-TRANSACTION-FLAG     PIC X VALUE "Y".
           
       01  TRANSACTION-DETAILS.
           05  TRANSACTION-TYPE-WS       PIC X VALUE "+".
               88  INCREASE VALUE "R".
               88  DECREASE VALUE "S".
           05  TRANSACTION-AMOUNT-WS     PIC 9(3) VALUE 0.
           
       01  ROUTINE-CREATE-REPORTS.
           05 WS-GENERATE-INV            PIC X(23)
           VALUE 'GenerateInventoryReport'.
           05 WS-GENERATE-REORDER        PIC X(07) 
           VALUE 'reorder'.
           
       SCREEN SECTION.
       01  TRANSACTION-ENTRY-SCREEN.
         05 VALUE "==MAYCON==" LINE 1.
           05 VALUE "==TRANSACTION ENTRY==" LINE 2.
           05 VALUE "PRESS TAB TO NAVIGATE, ENTER TO SUBMIT" LINE 3.
           05 VALUE "TRANSACTION TYPE (R/S): " LINE 4.
           05 TRANSACTION-TYPE-SCR PIC X TO TRANSACTION-TYPE-WS.        
           05 VALUE "INVENTORY NUMBER (5 DIGITS): " LINE 5.
           05 PART-NUMBER-SCR PIC 9(5) TO PART-NUMBER-IN-PK.            
           05 VALUE "TRANSACTION AMOUNT (3 DIGITS): " LINE 6.
           05 TRANSACTION-AMOUNT-SCR PIC 9(3) TO TRANSACTION-AMOUNT-WS.

       01 TRANSACTION-INVENTORY-ITEM.
           05 VALUE "ITEM UPDATED: " LINE 8.
           05 VALUE "ITEM NUM  - ITEM               - QTT" LINE 9.

       01  TEXT-PROMPTS.
           05  TRANSACTION-CONTINUE-PROMPT 
               VALUE "Enter another transaction? (Y/N): "  LINE 12.
       
       PROCEDURE DIVISION.
       100-PRODUCE-INVENTORY-REPORT.
           PERFORM 201-INIT-INVENTORY-REPORT.
           
           PERFORM 202-TRANSFER-INV-RECORD
               UNTIL INVENT-EOF-FLAG = "YES".
           
           PERFORM 203-TRANSFER-SUPPLY-RECORD
               UNTIL SUPPLIERS-EOF-FLAG = "YES".
           
           PERFORM 205-CLOSE-INV-FILES.
           PERFORM 204-RECORD-INV-TRANSACT.
           
      * PARAGRAPH TO CALL ROUTINE TO CREATE INVENTORY REPORT    
           PERFORM 206-CALL-INVENT-REPORT.
      * PARAGRAPH TO CALL ROUTINE TO CREATE RE-ORDER REPORT    
           PERFORM 207-CALL-REORDER-REPORT.
           
           STOP RUN.
       
       201-INIT-INVENTORY-REPORT.
           PERFORM 301-OPEN-INV-FILES.
           PERFORM 302-READ-INV-RECORD.
           PERFORM 303-READ-SUPPLY-RECORD.
           
       202-TRANSFER-INV-RECORD.
           PERFORM 304-WRITE-INV-RECORD-INDEXED.
           PERFORM 302-READ-INV-RECORD.
       
       203-TRANSFER-SUPPLY-RECORD.
           PERFORM 305-WRITE-SUP-RECORD-INDEXED.
           PERFORM 303-READ-SUPPLY-RECORD.
       
       204-RECORD-INV-TRANSACT.
           PERFORM 307-OPEN-INV-FILE-IO.
           PERFORM 306-ENTER-TRANSACTION
               UNTIL CONT-TRANSACTION-FLAG = "n" or "N".
           PERFORM 308-CLOSE-INV-FILE.
       
       205-CLOSE-INV-FILES.
           CLOSE INVENT-FILE-IN SUPPLIERS-FILE-IN INVENT6
               SUPPLIERI.
           
       206-CALL-INVENT-REPORT.
           CALL WS-GENERATE-INV.
       
       207-CALL-REORDER-REPORT.
           CALL WS-GENERATE-REORDER.
           
       301-OPEN-INV-FILES.
           OPEN INPUT  INVENT-FILE-IN SUPPLIERS-FILE-IN
               OUTPUT  INVENT6 SUPPLIERI.
       
       302-READ-INV-RECORD.
           READ INVENT-FILE-IN
              AT END  
           MOVE "YES"                    TO INVENT-EOF-FLAG.
       
       303-READ-SUPPLY-RECORD.
           READ SUPPLIERS-FILE-IN
              AT END  
           MOVE "YES"                    TO SUPPLIERS-EOF-FLAG.
       
       304-WRITE-INV-RECORD-INDEXED.
           WRITE INVENTORY-RECORD-OUT FROM INVENTORY-RECORD-IN
               INVALID KEY PERFORM 701-INVALID-INV-KEY
           END-WRITE.
           
       305-WRITE-SUP-RECORD-INDEXED.    
           WRITE SUPPLIER-RECORD-OUT FROM SUPPLIER-RECORD-IN            
               INVALID KEY PERFORM 702-INVALID-SUPPLY-KEY
           END-WRITE.
           
       306-ENTER-TRANSACTION.
           MOVE "NO"                     TO READ-OK-SWITCH.
           DISPLAY " " WITH BLANK SCREEN.
           DISPLAY TRANSACTION-ENTRY-SCREEN.
           ACCEPT TRANSACTION-TYPE-SCR.
           ACCEPT PART-NUMBER-SCR.
           ACCEPT TRANSACTION-AMOUNT-SCR.
           
           READ INVENT6
               INVALID KEY
                   PERFORM 701-INVALID-INV-KEY
               NOT INVALID KEY
                   MOVE "YES" TO READ-OK-SWITCH
           END-READ.
           
           IF READ-OK-SWITCH = "YES"
              EVALUATE TRUE
                 WHEN INCREASE
                      ADD TRANSACTION-AMOUNT-WS TO QTY-ON-HAND       
                      REWRITE INVENTORY-RECORD-OUT
                      PERFORM 309-DISPLAY-ITEM
                 WHEN DECREASE
                      IF TRANSACTION-AMOUNT-WS > QTY-ON-HAND         
                         PERFORM 
                         703-SUBTRACTION-ERROR-BALANCE-NEGATIVE
                      ELSE
                         SUBTRACT TRANSACTION-AMOUNT-WS FROM
                         QTY-ON-HAND
                         REWRITE INVENTORY-RECORD-OUT
                         PERFORM 309-DISPLAY-ITEM
                      END-IF
              END-EVALUATE
              END-IF
           
           DISPLAY TRANSACTION-CONTINUE-PROMPT.
           ACCEPT CONT-TRANSACTION-FLAG.
           
       307-OPEN-INV-FILE-IO.
           OPEN I-O INVENT6.
           
       308-CLOSE-INV-FILE.
           CLOSE INVENT6.
       
       309-DISPLAY-ITEM.
           DISPLAY TRANSACTION-INVENTORY-ITEM.
           DISPLAY PART-NUMBER-IN-PK LINE 10 COLUMN 2.
           DISPLAY PART-NAME LINE 10 COLUMN 13.
           DISPLAY QTY-ON-HAND LINE 10 COLUMN 34.
           
       701-INVALID-INV-KEY.
           DISPLAY "INVALID INV KEY ERROR".    
           
       702-INVALID-SUPPLY-KEY.
           DISPLAY "INVALID SUPPLY KEY ERROR". 
       
       703-SUBTRACTION-ERROR-BALANCE-NEGATIVE.
           DISPLAY "Insufficient inventory. Transaction canceled."
           LINE 7 COLUMN 2.
           