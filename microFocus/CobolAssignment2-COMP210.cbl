       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOL-ASSIGNMENT-TWO.
       AUTHOR. RéAL ORTELLI.
   
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F01-INPUT-FILE ASSIGN TO 'ASST2.DAT'
                                 ORGANIZATION IS LINE SEQUENTIAL.
           SELECT F02-PRINT-FILE ASSIGN TO 'ASST2.OUT'
                                 ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       
       FD F01-INPUT-FILE
          RECORD CONTAINS 35 CHARACTERS
          DATA RECORD IS F01-INPUT-IN.
          
       01 F01-INPUT-IN.
          05 F01-IN-PART-NAME             PIC X(20).
          05 F01-IN-BEGINNING             PIC 9(3).
          05 F01-IN-AMOUNT-RECEIVED       PIC 9(3).
          05 F01-IN-AMOUNT-SHIPPED        PIC 9(3).
          05 F01-IN-UNIT-PRICE            PIC 9999V99.
          
       FD F02-PRINT-FILE
          RECORD CONTAINS 83 CHARACTERS
          DATA RECORD IS F02-PRINT-OUT-LINE.
       
       01 F02-PRINT-OUT-LINE           PIC X(83).
       
       WORKING-STORAGE SECTION.
       
       01 W01-END-OF-FILE-SWITCH       PIC X(2) VALUE SPACES.
       01 W01-ASTERISK-ONE             PIC X(2) VALUE '*'.
       01 W01-ASTERISK-TWO             PIC X(2) VALUE '**'.
       01 W01-BEGINNING                PIC 9(3).
       01 W01-AMOUNT-RECEIVED          PIC 9(3).
       01 W01-AMOUNT-SHIPPED           PIC 9(3).
       01 W01-ENDING                   PIC 9(3).
       01 W01-UNIT-PRICE               PIC 9999V99.
       01 W01-ACCUMULATED-TOTAL        PIC 999999V99.
       01 W01-LINE-TOTAL               PIC 99999V99.
       
       01 W02-ASSIGNMENT-HEADING.
          05                           PIC X(24) VALUE SPACES.
          05                           PIC X(25) VALUE 'Real Ortelli Assignment 2'.
       
       01 W03-CATEGORY-HEADING.
          05                           PIC X(9) VALUE 'Part Name'.
          05                           PIC X(14) VALUE SPACES.
          05                           PIC X(9) VALUE 'Beginning'.
          05                           PIC X(4) VALUE SPACES.
          05                           PIC X(4) VALUE 'Recd'.
          05                           PIC X(4) VALUE SPACES.
          05                           PIC X(7) VALUE 'Shipped'.
          05                           PIC X(3) VALUE SPACES.
          05                           PIC X(6) VALUE 'Ending'.
          05                           PIC X(4) VALUE SPACES.
          05                           PIC X(5) VALUE 'Price'.
          05                           PIC X(7) VALUE SPACES.
          05                           PIC X(5) VALUE 'Total'.
          05                           PIC X(2) VALUE SPACES.
       
       01 W04-END-OF-REPORT.
          05                           PIC X(28) VALUE 'Total Value of all inventory'.
          05                           PIC X(43) VALUE SPACES.
          05   W04-INVENTORY-VALUE     PIC $$$$,$$$.99.
          05                           PIC X(2) VALUE SPACES.
          
       01 W05-INPUT-DATA.
          05   W05-PART-NAME           PIC X(20) VALUE SPACES.
          05                           PIC X(6) VALUE SPACES.
          05   W05-BEGINNING           PIC Z(3).
          05                           PIC X(8) VALUE SPACES.
          05   W05-RECD                PIC Z(3).
          05                           PIC X(6) VALUE SPACES.
          05   W05-SHIPPED             PIC Z(3).
          05                           PIC X(7) VALUE SPACES.
          05   W05-ENDING              PIC Z(3).
          05                           PIC X(4) VALUE SPACES.
          05   W05-PRICE               PIC ZZZ.99.
          05                           PIC X(4) VALUE SPACES.
          05   W05-TOTAL               PIC ZZZZZ.99.
          05   W05-MISC-PRINT          PIC X(2) VALUE SPACES.
          
       01 W06-DASH-LINE.
          05                           PIC X(70) VALUE SPACES.
          05                           PIC X(11) VALUE '-----------'.
          05                           PIC X(2) VALUE SPACES.

       PROCEDURE DIVISION.

           PERFORM 100-OPEN-FILES
           PERFORM 200-WRITE-HEADING-LINES
           PERFORM 300-PROCESS-RECORDS
               UNTIL W01-END-OF-FILE-SWITCH = 'NO'
           PERFORM 400-PRINT-TOTALS
           PERFORM 500-CLOSE-FILES
           
           STOP RUN
           .
       
       100-OPEN-FILES.
           OPEN INPUT F01-INPUT-FILE
                OUTPUT F02-PRINT-FILE
           READ F01-INPUT-FILE
                AT END MOVE 'NO' TO W01-END-OF-FILE-SWITCH
           END-READ
           .
           
       200-WRITE-HEADING-LINES.
           MOVE W02-ASSIGNMENT-HEADING TO F02-PRINT-OUT-LINE
           WRITE F02-PRINT-OUT-LINE
           
           MOVE SPACES TO F02-PRINT-OUT-LINE
           WRITE F02-PRINT-OUT-LINE
           
           MOVE W03-CATEGORY-HEADING TO F02-PRINT-OUT-LINE
           WRITE F02-PRINT-OUT-LINE
           .
           
       300-PROCESS-RECORDS.
           MOVE F01-IN-PART-NAME TO W05-PART-NAME
           MOVE F01-IN-BEGINNING TO W01-BEGINNING
           MOVE F01-IN-AMOUNT-RECEIVED TO W01-AMOUNT-RECEIVED
           MOVE F01-IN-AMOUNT-SHIPPED TO W01-AMOUNT-SHIPPED
           MOVE F01-IN-UNIT-PRICE TO W01-UNIT-PRICE
           
           PERFORM 310-DO-CALCULATIONS
           
           MOVE W01-LINE-TOTAL TO W05-TOTAL
           MOVE W01-UNIT-PRICE TO W05-PRICE
           MOVE W01-AMOUNT-RECEIVED TO  W05-RECD
           MOVE W01-AMOUNT-SHIPPED TO W05-SHIPPED
           MOVE W01-BEGINNING TO W05-BEGINNING
           MOVE W01-ENDING TO W05-ENDING
           
           MOVE W05-INPUT-DATA TO F02-PRINT-OUT-LINE
           WRITE F02-PRINT-OUT-LINE
           
           READ F01-INPUT-FILE
               AT END MOVE 'NO' TO W01-END-OF-FILE-SWITCH
           END-READ
           .
           
       310-DO-CALCULATIONS.
           COMPUTE W01-ENDING ROUNDED = W01-BEGINNING + W01-AMOUNT-RECEIVED - W01-AMOUNT-SHIPPED
           COMPUTE W01-LINE-TOTAL ROUNDED = W01-UNIT-PRICE * W01-ENDING
           COMPUTE W01-ACCUMULATED-TOTAL ROUNDED = W01-ACCUMULATED-TOTAL + W01-LINE-TOTAL

           EVALUATE W01-LINE-TOTAL
               WHEN > 50000.00
                   MOVE W01-ASTERISK-TWO TO W05-MISC-PRINT
               WHEN >= 40000.00
                   MOVE W01-ASTERISK-ONE TO W05-MISC-PRINT
               WHEN OTHER
                   MOVE SPACES TO W05-MISC-PRINT
           END-EVALUATE
           .
       
       400-PRINT-TOTALS.
           MOVE W06-DASH-LINE TO F02-PRINT-OUT-LINE
           WRITE F02-PRINT-OUT-LINE
           
           MOVE W01-ACCUMULATED-TOTAL TO W04-INVENTORY-VALUE
           
           MOVE W04-END-OF-REPORT TO F02-PRINT-OUT-LINE
           WRITE F02-PRINT-OUT-LINE
           .
           
       500-CLOSE-FILES.
           CLOSE F01-INPUT-FILE
                 F02-PRINT-FILE
           .