       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGNMENT-ONE.
       AUTHOR. RÈAL ORTELLI.
   
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F01-EMPLOYEE-FILE ASSIGN TO 'ASST1.DAT'
                                 ORGANIZATION IS LINE SEQUENTIAL.
           SELECT F02-PRINT-FILE ASSIGN TO 'ASST1.OUT'
                                 ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       
       FD F01-EMPLOYEE-FILE
          RECORD CONTAINS 30 CHARACTERS
          DATA RECORD IS F01-EMPLOYEE-IN.
          
       01 F01-EMPLOYEE-IN.
          05 F01-EMP-NAME              PIC X(18).
          05 F01-EMP-SSN               PIC 9(9).
          05 F01-EMP-GROSSPAY          PIC 9(3).
          
       FD F02-PRINT-FILE
          RECORD CONTAINS 71 CHARACTERS
          DATA RECORD IS F02-PRINT-OUT-LINE.
       
       01 F02-PRINT-OUT-LINE           PIC X(71).
       
       WORKING-STORAGE SECTION.
       
       01 W01-END-OF-FILE-SWITCH       PIC X(2) VALUE SPACES.
       01 W01-ONE-HUNDRED              PIC 9(3) VALUE 100.
       01 W01-FIFTY                    PIC 9(2) VALUE 50.
       01 W01-TWENTY                   PIC 9(2) VALUE 20.
       01 W01-TEN                      PIC 9(2) VALUE 10.
       01 W01-FIVE                     PIC 9(2) VALUE 5.
       01 W01-ACCUMLATION              PIC 9(5).
       
       01 W02-ASSIGNMENT-HEADING.
          05                           PIC X(22) VALUE SPACES.
          05                           PIC X(25) VALUE 'Real Ortelli Assignment 1'.
       
       01 W03-EMPLOYEE-NAME-HEADING.
          05                           PIC X(2) VALUE SPACES.
          05                           PIC X(13) VALUE 'EMPLOYEE NAME'.
          05                           PIC X(20) VALUE SPACES.
          05                           PIC X(4) VALUE '$100'.
          05                           PIC X(3) VALUE SPACES.
          05                           PIC X(3) VALUE '$50'.
          05                           PIC X(2) VALUE SPACES.
          05                           PIC X(3) VALUE '$20'.
          05                           PIC X(2) VALUE SPACES.
          05                           PIC X(3) VALUE '$10'.
          05                           PIC X(3) VALUE SPACES.
          05                           PIC X(2) VALUE '$5'.
          05                           PIC X(3) VALUE SPACES.
          05                           PIC X(2) VALUE '$1'.
          05                           PIC X(3) VALUE SPACES.
          05                           PIC X(3) VALUE 'PAY'.
       
       01 W04-END-OF-REPORT.
          05                           PIC X(13) VALUE 'End of Report'.
          
       01 W05-EMPLOYEE-DATA.
          05                           PIC X(2) VALUE SPACES.
          05   W05-EMPLOYEE-NAME       PIC X(18) VALUE SPACES.
          05                           PIC X(2) VALUE SPACES.
          05   W05-SSN-NUMBER          PIC 9(9).
          05                           PIC X(7) VALUE SPACES.
          05   W05-100-DOLLARS         PIC 9.
          05                           PIC X(5) VALUE SPACES.
          05   W05-50-DOLLARS          PIC 9.
          05                           PIC X(4) VALUE SPACES.
          05   W05-20-DOLLARS          PIC 9.
          05                           PIC X(4) VALUE SPACES.
          05   W05-10-DOLLARS          PIC 9.
          05                           PIC X(4) VALUE SPACES.
          05   W05-5-DOLLARS           PIC 9.
          05                           PIC X(4) VALUE SPACES.
          05   W05-1-DOLLAR            PIC 9.
          05                           PIC X(3) VALUE SPACES.
          05   W05-PAY-AMOUNT          PIC 9(3).

       PROCEDURE DIVISION.

           PERFORM 100-OPEN-FILES
           PERFORM 200-WRITE-HEADING-LINES
           PERFORM 300-PROCESS-RECORDS
               UNTIL W01-END-OF-FILE-SWITCH = 'NO'
           PERFORM 400-WRITE-FOOTER
           PERFORM 500-CLOSE-FILES
           
           STOP RUN
           .
           
       100-OPEN-FILES.
           OPEN INPUT F01-EMPLOYEE-FILE
                OUTPUT F02-PRINT-FILE
           READ F01-EMPLOYEE-FILE
                AT END MOVE 'NO' TO W01-END-OF-FILE-SWITCH
           END-READ
           .
           
       200-WRITE-HEADING-LINES.
           MOVE W02-ASSIGNMENT-HEADING TO F02-PRINT-OUT-LINE
           WRITE F02-PRINT-OUT-LINE
           
           MOVE W03-EMPLOYEE-NAME-HEADING TO F02-PRINT-OUT-LINE
           WRITE F02-PRINT-OUT-LINE
           .
           
       300-PROCESS-RECORDS.
           MOVE F01-EMP-NAME TO W05-EMPLOYEE-NAME
           MOVE F01-EMP-SSN TO W05-SSN-NUMBER
           MOVE F01-EMP-GROSSPAY TO W05-PAY-AMOUNT
           
           PERFORM 310-DO-CALCULATIONS
           
           MOVE W05-EMPLOYEE-DATA TO F02-PRINT-OUT-LINE
           WRITE F02-PRINT-OUT-LINE
           
           READ F01-EMPLOYEE-FILE
               AT END MOVE 'NO' TO W01-END-OF-FILE-SWITCH
           END-READ
           .
           
       310-DO-CALCULATIONS.
            
            COMPUTE W01-ACCUMLATION = F01-EMP-GROSSPAY
            COMPUTE W05-100-DOLLARS = W01-ACCUMLATION / W01-ONE-HUNDRED
            COMPUTE W01-ACCUMLATION = W05-100-DOLLARS * W01-ONE-HUNDRED - W01-ACCUMLATION
            COMPUTE W05-50-DOLLARS = W01-ACCUMLATION / W01-FIFTY
            COMPUTE W01-ACCUMLATION = W05-50-DOLLARS * W01-FIFTY - W01-ACCUMLATION
            COMPUTE W05-20-DOLLARS = W01-ACCUMLATION / W01-TWENTY
            COMPUTE W01-ACCUMLATION = W05-20-DOLLARS * W01-TWENTY - W01-ACCUMLATION
            COMPUTE W05-10-DOLLARS = W01-ACCUMLATION / W01-TEN
            COMPUTE W01-ACCUMLATION = W05-10-DOLLARS * W01-TEN - W01-ACCUMLATION
            COMPUTE W05-5-DOLLARS = W01-ACCUMLATION / W01-FIVE
            COMPUTE W01-ACCUMLATION = W05-5-DOLLARS * W01-FIVE - W01-ACCUMLATION
            COMPUTE W05-1-DOLLAR = W01-ACCUMLATION
           .
           
       400-WRITE-FOOTER.
           MOVE W04-END-OF-REPORT TO F02-PRINT-OUT-LINE
           WRITE F02-PRINT-OUT-LINE
           .
           
       500-CLOSE-FILES.
           CLOSE F01-EMPLOYEE-FILE
                 F02-PRINT-FILE
           .
