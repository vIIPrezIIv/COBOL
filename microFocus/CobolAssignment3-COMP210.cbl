       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOL-ASSIGNMENT-THREE.
       AUTHOR. RéAL ORTELLI.
   
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F01-PAYROLL-FILE ASSIGN TO 'PAYROLLRECORDS.DAT'
                                 ORGANIZATION IS LINE SEQUENTIAL.
           SELECT F02-VALID-FILE ASSIGN TO 'VALIDRECORDS.DAT'
                                 ORGANIZATION IS LINE SEQUENTIAL.
           SELECT F03-ERROR-FILE ASSIGN TO 'ERRORFILE.DAT'
                                 ORGANIZATION IS LINE SEQUENTIAL.                      

       DATA DIVISION.
       FILE SECTION.
       
       FD F01-PAYROLL-FILE
          RECORD CONTAINS 78 CHARACTERS
          DATA RECORD IS F01-PAYROLL-IN.
          
       01 F01-PAYROLL-IN.
          05 F01-IN-SOCIAL-SECURITY-NUMBER        PIC X(9).
          05 F01-IN-FULL-NAME.
             10 F01-IN-LAST-NAME                  PIC X(14).
             10 F01-IN-FIRST-NAME                 PIC X(12).
             10 F01-IN-INITIAL                    PIC X.
          05 F01-IN-HOURLY-RATE                   PIC 999V99.
          05 F01-IN-HOURS-WORKED                  PIC 999V99.
             88 F01-IN-HOURS-THRU                 VALUE 1 THRU 999.
          05 F01-IN-HOURS-WORKED-X
             REDEFINES F01-IN-HOURS-WORKED        PIC X(5).
          05 F01-IN-SALARY                        PIC X.
          05 F01-IN-DEPENDENTS                    PIC 99.
             88 F01-IN-DEPENDENTS-TYPE            VALUE 1 THRU 10.
          05 F01-IN-DEPENDENTS-X
             REDEFINES F01-IN-DEPENDENTS          PIC XX. 
          05 F01-IN-TAX-STATUS                    PIC 9.
          05 F01-IN-INSURANCE                     PIC X.
             88 F01-IN-INSURANCE-TYPE             VALUES 'A' 'B' 'C' 'Z'.
          05 F01-IN-YTD-INFO.
             10 F01-IN-YTD-EARNINGS               PIC 999999V99.
             10 F01-IN-YTD-TAXES                  PIC 99999V99.
             10 F01-IN-YTD-FICA                   PIC 9999V99.
             10 F01-IN-YTD-INSURANCE              PIC 9999V99.
          
       FD F02-VALID-FILE
          RECORD CONTAINS 100 CHARACTERS
          DATA RECORD IS F02-VALID-OUT-LINE.
       
       01 F02-VALID-OUT-LINE           PIC X(100).
       
       FD F03-ERROR-FILE
          RECORD CONTAINS 100 CHARACTERS
          DATA RECORD IS F03-ERROR-OUT-LINE.
       
       01 F03-ERROR-OUT-LINE           PIC X(100).
       
       WORKING-STORAGE SECTION.
       
       01 W01-END-OF-FILE-SWITCH       PIC X(3) VALUE SPACES.
          88 NO-DATA-REMAINS           VALUE 'NO'.
       01 W01-VALID-RECORD-SWITCH      PIC X(3).
          88 W01-VALID-RECORD          VALUE 'YES'.
       
       01 W02-ERROR-HEADING.
          05                           PIC X(28) VALUE SPACES.
          05 W02-PAYROLL               PIC X(18) VALUE 'Payroll Error File'.
          05                           PIC X(48) VALUE SPACES.
       
       01 W03-ERROR-HEADING-TWO.
          05 W03-SSN                   PIC X(3) VALUE 'SSN'.
          05                           PIC X(12) VALUE SPACES.
          05 W03-LAST-NAME             PIC X(9) VALUE 'Last Name'.
          05                           PIC X(7) VALUE SPACES.
          05 W03-OFFENDING-FIELD-DATA  PIC X(20) VALUE 'Offending Field Data'.
          05                           PIC X(5) VALUE SPACES.
          05 W03-ERROR                 PIC X(5) VALUE 'Error'.
          05                           PIC X(33) VALUE SPACES.
          
       01 W04-WRITE-ERROR.
          05 W04-SSN                   PIC 9(9).
          05                           PIC X(6) VALUE SPACES.
          05 W04-LAST-NAME             PIC X(14).
          05                           PIC X(2) VALUE SPACES.
          05 W04-OFFENDING-FIELD-DATA  PIC X(15).
          05                           PIC X(10) VALUE SPACES.
          05 W04-ERROR                 PIC X(38).
          
       01 W05-REPORT-HEADING.
          05 W05-REPORT                PIC X(33) VALUE 'Report Produced By Real Ortelli'.
          05                           PIC X(61) VALUE SPACES.
          
       01 W06-ERROR-MESSAGES.
          05 W06-SSN-ERR-MSG               PIC X(38)
                       VALUE 'Social Security Number cannot be blank'.
          05 W06-DEPEND-NOT-NUMERIC-ERR    PIC X(26)
                       VALUE 'Dependents must be numeric'.
          05 W06-DEPEND-OVER10-ERR         PIC X(29)
                       VALUE 'Dependents must be 10 or less'.
          05 W06-HOURS-NOT-NUMERIC-ERR     PIC X(28)
                       VALUE 'Hours worked must be numeric'.
          05 W06-SALARY-OVER-40-ERR-MSG    PIC X(34)
                       VALUE 'No overtime for salaried employees'.
          05 W06-INSURANCE-ERR             PIC X(30)
                       VALUE 'Insurance must be A, B, C or Z'.
                       
       PROCEDURE DIVISION.

           PERFORM 100-OPEN-FILES
           PERFORM 200-WRITE-HEADING-LINES
           PERFORM 300-READ-RECORD
           PERFORM 400-PROCESS-RECORDS
               UNTIL NO-DATA-REMAINS
           PERFORM 500-WRITE-FOOTER
           PERFORM 600-CLOSE-FILES
        
           STOP RUN
           .
           
      *This procedure opens the files.  
       100-OPEN-FILES.
           OPEN INPUT F01-PAYROLL-FILE
                OUTPUT F02-VALID-FILE
                       F03-ERROR-FILE
           .
      *Prints headings for error output file.     
       200-WRITE-HEADING-LINES.
           MOVE W02-ERROR-HEADING TO F03-ERROR-OUT-LINE
           WRITE F03-ERROR-OUT-LINE
           
           MOVE SPACES TO F03-ERROR-OUT-LINE
           WRITE F03-ERROR-OUT-LINE
           
           MOVE W03-ERROR-HEADING-TWO TO F03-ERROR-OUT-LINE
           WRITE F03-ERROR-OUT-LINE
           .
      *Reads a record.     
       300-READ-RECORD.
           READ F01-PAYROLL-FILE
                AT END MOVE 'NO' TO W01-END-OF-FILE-SWITCH
           END-READ
           .
      *Processes the records and validates them then calls the valid output file write.     
       400-PROCESS-RECORDS.
           MOVE 'YES' TO W01-VALID-RECORD-SWITCH
           
           PERFORM 410-VALIDATE-SSN
           PERFORM 420-VALIDATE-HOURS-WORKED
           PERFORM 430-VALIDATE-DEPENDENTS
           PERFORM 440-VALIDATE-INSURANCE
           
           PERFORM 401-WRITE-RECORD
           
           PERFORM 300-READ-RECORD
           .
      *Validates the Social Security Number.     
       410-VALIDATE-SSN.
           IF F01-IN-SOCIAL-SECURITY-NUMBER = SPACES
              MOVE W06-SSN-ERR-MSG TO W04-ERROR
              MOVE SPACES TO W04-OFFENDING-FIELD-DATA
              PERFORM 402-WRITE-ERROR
           END-IF
              .
      *Validates the hours worked.       
       420-VALIDATE-HOURS-WORKED.
           IF F01-IN-HOURS-WORKED NOT NUMERIC
              MOVE W06-HOURS-NOT-NUMERIC-ERR TO W04-ERROR
              MOVE F01-IN-HOURS-WORKED-X TO W04-OFFENDING-FIELD-DATA
              PERFORM 402-WRITE-ERROR
           ELSE
              IF F01-IN-SALARY = 'S'
                 IF F01-IN-HOURS-WORKED > 40
                    MOVE W06-SALARY-OVER-40-ERR-MSG TO W04-ERROR
                    MOVE F01-IN-HOURS-WORKED-X TO W04-OFFENDING-FIELD-DATA
                    PERFORM 402-WRITE-ERROR
                 END-IF
              END-IF
           END-IF
              .
      *Validates the dependents.        
       430-VALIDATE-DEPENDENTS.
           IF F01-IN-DEPENDENTS NOT NUMERIC
              MOVE W06-DEPEND-NOT-NUMERIC-ERR TO W04-ERROR
              MOVE F01-IN-DEPENDENTS-X TO W04-OFFENDING-FIELD-DATA
              PERFORM 402-WRITE-ERROR
           ELSE
              IF F01-IN-DEPENDENTS > 10
                 MOVE W06-DEPEND-OVER10-ERR TO W04-ERROR
                 MOVE F01-IN-DEPENDENTS TO W04-OFFENDING-FIELD-DATA
                 PERFORM 402-WRITE-ERROR
              END-IF
           END-IF
              .
      *Validates the insurance.        
       440-VALIDATE-INSURANCE.
           IF NOT F01-IN-INSURANCE-TYPE
              MOVE W06-INSURANCE-ERR TO W04-ERROR
              MOVE F01-IN-INSURANCE TO W04-OFFENDING-FIELD-DATA
              PERFORM 402-WRITE-ERROR
           END-IF
           .
      *Writes the valid output record.    
       401-WRITE-RECORD.
           IF W01-VALID-RECORD
              MOVE F01-PAYROLL-IN TO F02-VALID-OUT-LINE
              WRITE F02-VALID-OUT-LINE
           ELSE
              MOVE SPACES TO F03-ERROR-OUT-LINE
              WRITE F03-ERROR-OUT-LINE
           END-IF
           .
      *Writes the errors tot he error output file.     
       402-WRITE-ERROR.
           MOVE 'NO' TO W01-VALID-RECORD-SWITCH 
           
           MOVE F01-IN-SOCIAL-SECURITY-NUMBER TO W04-SSN
           MOVE F01-IN-LAST-NAME TO W04-LAST-NAME
           
           MOVE W04-WRITE-ERROR TO F03-ERROR-OUT-LINE
           WRITE F03-ERROR-OUT-LINE
           .
      *Writes the footer tot he error output file.      
       500-WRITE-FOOTER.
           MOVE W05-REPORT-HEADING TO F03-ERROR-OUT-LINE
           WRITE F03-ERROR-OUT-LINE
               AFTER ADVANCING 2 LINES
           .
            
      *This procedure closes the files. 
       600-CLOSE-FILES.
           CLOSE F01-PAYROLL-FILE
                 F02-VALID-FILE
                 F03-ERROR-FILE
           .
           
        
           
           