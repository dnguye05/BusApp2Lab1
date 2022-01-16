      ******************************************************************
      *Author: David Nguyen
      *Date: January 14, 2022
      *Purpose: lab1
      ******************************************************************
       IDENTIFICATION DIVISION.
           PROGRAM-ID. lab1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'EMPL_INPUT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO 'EMPL_OUTPUT'.
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD.
       01  INPUT-RECORD PIC X(80).
       FD  OUTPUT-FILE
               LABEL RECORDS ARE OMITTED.
       01  OUTPUT-RECORD PIC X(110).
       WORKING-STORAGE SECTION.
       01  INPUT-DATA.
           03 I-NAME PIC X(20).
           03 I-MAJOR PIC X(4).
           03 I-YEAR PIC X(4).
           03 I-LOAN.
               05 I-LOAN-WHOLE PIC 9(5).
               05 I-LOAN-DECIMAL PIC P9(2).
           03 I-PAID OCCURS 4 TIMES.
               05 I-PAID-WHOLE PIC 9(4).
               05 I-PAID-DECIMAL PIC P9(2).
       01  FORMAT-DATA.
           03 F-LOAN PIC 9(5)V9(2).
           03 F-PAID-ARRAY OCCURS 4 TIMES.
               05 F-PAID PIC 9(4)V9(2).
           03 F-TOTAL-PAID PIC 9(5)V9(2).
           03 F-BALANCE PIC S9(5)V9(2).
       01  OUTPUT-HEADING.
           03 FILLER PIC X(4) VALUES 'NAME'.
           03 FILLER PIC X(16) VALUES SPACES.
           03 FILLER PIC X(6) VALUES 'DEGREE'.
           03 FILLER PIC X(4) VALUES SPACES.
           03 FILLER PIC X(4) VALUES 'YEAR'.
           03 FILLER PIC X(3) VALUES SPACES.
           03 FILLER PIC X(4) VALUES 'LOAN'.
           03 FILLER PIC X(11) VALUES SPACES.
           03 FILLER PIC X(4) VALUES 'PAID'.
       01  OUTPUT-DATA1.
           03 O-NAME PIC X(20).
           03 O-MAJOR PIC X(4).
           03 FILLER PIC X(6) VALUE SPACES.
           03 O-YEAR PIC X(4).
           03 FILLER PIC X(3) VALUE SPACES.
           03 O-LOAN PIC ZZZZ9.99.
           03 FILLER PIC X(4) VALUES SPACES.
           03 O-PAID-1 PIC ZZZ9.99.
       01  OUTPUT-DATA2.
           03 FILLER PIC X(49) VALUE SPACES.
           03 O-PAID-OTHER PIC ZZZ9.99.
       01  OUTPUT-FOOTER1.
           03 FILLER PIC X(31) VALUE SPACES.
           03 FILLER PIC X(10) VALUE 'TOTAL PAID'.
           03 FILLER PIC X(5) VALUE SPACES.
           03 O-TOTAL-PAID PIC $$$,$$9.99.
           03 FILLER PIC X(11) VALUE SPACES.
           03 FILLER PIC X(7) VALUE 'BALANCE'.
           03 FILLER PIC X(5) VALUE SPACES.
           03 O-BALANCE PIC $$$,$$9.99.
       01  OUTPUT-FOOTER2.
           03 FILLER PIC X(31) VALUE SPACES.
           03 FILLER PIC X(10) VALUE 'TOTAL PAID'.
           03 FILLER PIC X(5) VALUE SPACES.
           03 O-TOTAL-PAID2 PIC $$$,$$9.99.
           03 FILLER PIC X(2) VALUE SPACES.
           03 FILLER PIC X(17) VALUE 'OVERPAID BALANCE'.
           03 FILLER PIC X(4) VALUE SPACES.
           03 O-BALANCE2.
               05 O-BALANCE2-NUM PIC $$$,$$9.99.
               05 O-BALANCE2-SYMBOL PIC X(3) VALUE '-**'.
       01  MISC.
           03 EOF-I PIC 9 VALUE 0.
           03 SUB PIC 99.
           03 RECORD-COUNT PIC 99 VALUE 0.
       PROCEDURE DIVISION.
       000-MAINLINE.
           OPEN INPUT INPUT-FILE
               OUTPUT OUTPUT-FILE
           PERFORM 500-READ-INPUT.
           PERFORM 100-PRINT-HEADING.
           PERFORM 200-LOOP-FILE
               UNTIL EOF-I = 1;
           CLOSE INPUT-FILE
               OUTPUT-FILE.
           STOP RUN.
       100-PRINT-HEADING.
           IF RECORD-COUNT = 0 THEN
               WRITE OUTPUT-RECORD FROM OUTPUT-HEADING
           ELSE
               MOVE SPACES TO OUTPUT-RECORD
               WRITE OUTPUT-RECORD
                   AFTER ADVANCING PAGE
               WRITE OUTPUT-RECORD FROM OUTPUT-HEADING
                   AFTER ADVANCING 1 LINE
               MOVE 0 TO RECORD-COUNT
           END-IF.
           MOVE SPACES TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD
               AFTER ADVANCING 1 LINE.
       200-LOOP-FILE.
           PERFORM 300-PRINT-INFO.
           PERFORM 500-READ-INPUT.
       300-PRINT-INFO.
           IF RECORD-COUNT >= 8 THEN
               PERFORM 100-PRINT-HEADING
           END-IF.
           MOVE I-NAME TO O-NAME.
           MOVE I-MAJOR TO O-MAJOR.
           MOVE I-YEAR TO O-YEAR.
           MOVE I-LOAN TO F-LOAN.
           MOVE F-LOAN TO O-LOAN.
           MOVE I-PAID(1) TO F-PAID(1).
           MOVE F-PAID(1) TO O-PAID-1.
           WRITE OUTPUT-RECORD FROM OUTPUT-DATA1
               AFTER ADVANCING 1 LINE.
      *OUTPUT OTHER PAID AMOUNT
           PERFORM VARYING SUB FROM 2 BY 1
               UNTIL SUB > 4
               MOVE I-PAID(SUB) TO F-PAID(SUB)
               MOVE F-PAID(SUB) TO O-PAID-OTHER
               WRITE OUTPUT-RECORD FROM OUTPUT-DATA2
                   AFTER ADVANCING 1 LINE
           END-PERFORM.
           PERFORM 350-COMPUTE-FOOTER.
           ADD 1 TO RECORD-COUNT.
       350-COMPUTE-FOOTER.
           MOVE 0 TO F-TOTAL-PAID.
           PERFORM VARYING SUB FROM 1 BY 1
               UNTIL SUB > 4
               COMPUTE F-TOTAL-PAID = F-TOTAL-PAID + F-PAID(SUB)
           END-PERFORM.
           MOVE F-TOTAL-PAID TO O-TOTAL-PAID.
           COMPUTE F-BALANCE = F-LOAN - F-TOTAL-PAID.
           IF F-BALANCE >= 0 THEN
               MOVE F-BALANCE TO O-BALANCE
               WRITE OUTPUT-RECORD FROM OUTPUT-FOOTER1
                   AFTER ADVANCING 1 LINE
               MOVE SPACES TO OUTPUT-RECORD
               WRITE OUTPUT-RECORD
                   AFTER ADVANCING 1 LINE
           ELSE
               MOVE F-TOTAL-PAID TO O-TOTAL-PAID2
               MOVE F-BALANCE TO O-BALANCE2-NUM
               WRITE OUTPUT-RECORD FROM OUTPUT-FOOTER2
                   AFTER ADVANCING 1 LINE
               MOVE SPACES TO OUTPUT-RECORD
               WRITE OUTPUT-RECORD
                   AFTER ADVANCING 1 LINE
           END-IF.
           MOVE 0 TO F-BALANCE.
       500-READ-INPUT.
           READ INPUT-FILE INTO INPUT-DATA
               AT END MOVE 1 TO EOF-I.
       END PROGRAM lab1.
