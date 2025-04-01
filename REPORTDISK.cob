       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORTDISK.
      ******************************************************************
      * Author: ALEXANDRE PEDRO
      * Company: XPTO
      * Date: 31/03/2025
      * Purpose: RECORD THE MOVIES FROM MOVIES.DAT TO MOVIES.TXT
      ******************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT       IS COMMA.

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *       SELECT MOVIES ASSIGN TO "./Data/MOVIES.DAT"
             SELECT MOVIES ASSIGN TO "C:\Cobol\Project\Data\MOVIES.DAT"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS MOVIES-STATUS
             RECORD KEY IS MOVIES-KEY.

      *       SELECT REPORT-MOVIES ASSIGN TO './Data/MOVIES.TXT'
             SELECT REPORT-MOVIES ASSIGN TO
               "C:\Cobol\Project\Data\MOVIES.TXT"
             ORGANIZATION IS SEQUENTIAL
             FILE STATUS IS REPORT-MOVIES-STATUS.

       DATA                    DIVISION.
       FILE                    SECTION.
      *---------------------------- FILE DESCRIPTION
       FD MOVIES.
       01 MOVIES-REG.
            05 MOVIES-KEY            PIC 9(005).
            05 MOVIES-TITLE          PIC X(030).
            05 MOVIES-GENRE          PIC X(008).
            05 MOVIES-DURATION       PIC 9(003).
            05 MOVIES-DISTRIBUTOR    PIC X(015).
            05 MOVIES-RATING         PIC 9(002).

      *---------------------------- FILE OUTPUT
       FD REPORT-MOVIES.
       01 REPORT-MOVIES-REG.
            05 REPORT-MOVIES-DATA    PIC X(063).


       WORKING-STORAGE         SECTION.
      *---------------------------- DATA ENTRY VARIABLES
       77 WRK-KEY                    PIC X(001).

      *---------------------------- DISPLAY VARIABLES
       01 WRK-LINE                   PIC 9(002) VALUE 01.

      *---------------------------- ERROR MESSAGES
       77 WRK-ERROR-MSG              PIC X(030) VALUE SPACES.

       01   WRK-MSGS.
            05 WRK-MSG-CORRUPTED     PIC X(030) VALUE
               "CORRUPTED FILE".
            05 WRK-MSG-NOTFOUND      PIC X(030) VALUE
               "ENTITY NOT FOUND".
            05 WRK-MSG-OPEN          PIC X(030) VALUE
               "ERROR WHILE OPENING FILE".
            05 WRK-MSG-PATH          PIC X(030) VALUE
               "ERROR ON FILE PATH".
            05 WRK-MSG-PRESSKEY      PIC X(030) VALUE
               "PRESS ANY KEY".
            05 WRK-MSG-READRECORDS   PIC X(030) VALUE
               "READ RECORDS ".
            05 WRK-MSG-WRITTENREC    PIC X(030) VALUE
               "WRITTEN RECORDS".
            05 WRK-MSG-UNKNOWN       PIC X(030) VALUE
               "UNKNOWN ERROR".

      *---------------------------- FILE
       77 MOVIES-STATUS              PIC 9(002) VALUE ZEROS.
       77 REPORT-MOVIES-STATUS       PIC 9(002) VALUE ZEROS.
       77 WRK-WRITTEN-RECORDS        PIC 9(005) VALUE ZEROS.
       77 WRK-REGQTY                 PIC 9(005) VALUE ZEROS.

       LINKAGE                 SECTION.
      *---------------------------- LINKAGE VARIABLES
       01 LNK-TITLE.
            05 LNK-SCREEN-TITLE      PIC X(020).
            05 LNK-MODULE-TITLE      PIC X(026).

       SCREEN                  SECTION.
      *---------------------------- ERROR SCREEN
       01 ERROR-SCREEN.
            05 MSG-ERROR.
                10 LINE 16 COLUMN 01 ERASE EOL
                   BACKGROUND-COLOR 3.
                10 LINE 16 COLUMN 10 PIC X(030)
                   BACKGROUND-COLOR 3
                   USING WRK-ERROR-MSG.
                10 COLUMN PLUS 2     PIC X(001)
                   BACKGROUND-COLOR 3
                   USING WRK-KEY.

      *---------------------------- SCREEN LAYOUT
       01 CLEANER-SCREEN.
            05 CLEAN-SCREEN.
               10 BLANK SCREEN.
               10 LINE 01 COLUMN 01  PIC X(020) ERASE EOL
                    BACKGROUND-COLOR 3.
               10 LINE 01 COLUMN 15  PIC X(020)
                    BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
                    FROM LNK-SCREEN-TITLE.
               10 LINE 02 COLUMN 01  PIC X(025) ERASE EOL
                    BACKGROUND-COLOR 1.
               10 LINE 02 COLUMN 14  PIC X(026)
                    BACKGROUND-COLOR 1 FOREGROUND-COLOR 6
                    FROM LNK-MODULE-TITLE.

       PROCEDURE               DIVISION USING LNK-TITLE.

       0100-MAIN               SECTION.
            PERFORM 0200-INITIALIZE.
            PERFORM 0300-PROCESS.
            PERFORM 0400-FINALIZE.
            GOBACK.

       0200-INITIALIZE         SECTION.
            OPEN I-O MOVIES.
            PERFORM 0210-VERIFICATION.

       0210-VERIFICATION       SECTION.
       0201-VERIFICATION-MOVIES.
            EVALUATE MOVIES-STATUS
              WHEN 0
                CONTINUE
              WHEN 30
                MOVE WRK-MSG-PATH TO WRK-ERROR-MSG
              WHEN 35
                MOVE WRK-MSG-OPEN TO WRK-ERROR-MSG
              WHEN 42
                MOVE WRK-MSG-CORRUPTED TO WRK-ERROR-MSG
              WHEN OTHER
                MOVE WRK-MSG-UNKNOWN TO WRK-ERROR-MSG
            END-EVALUATE.

            IF WRK-ERROR-MSG NOT EQUAL SPACES
               PERFORM 9000-MANAGE-ERROR
            END-IF.

       0300-PROCESS            SECTION.
            MOVE SPACES TO MOVIES-TITLE MOVIES-GENRE MOVIES-DISTRIBUTOR
               WRK-KEY.
            MOVE ZEROS TO MOVIES-KEY MOVIES-DURATION MOVIES-RATING
               WRK-REGQTY WRK-WRITTEN-RECORDS.

            DISPLAY CLEANER-SCREEN.
            PERFORM 0310-REPORT.

       0310-REPORT             SECTION.
            READ MOVIES
               INVALID KEY
                   MOVE WRK-MSG-NOTFOUND TO WRK-ERROR-MSG
               NOT INVALID KEY
                   OPEN OUTPUT REPORT-MOVIES
                   PERFORM 0320-REPORT-PROCESS
                   CLOSE REPORT-MOVIES
            END-READ.
            IF WRK-ERROR-MSG NOT EQUAL SPACES
                PERFORM 9000-MANAGE-ERROR
            ELSE
               PERFORM 0330-REPORT-STATISTICS
            END-IF.

            MOVE SPACES TO WRK-KEY.

       0320-REPORT-PROCESS     SECTION.
            PERFORM UNTIL MOVIES-STATUS = 10
               ADD 1 TO WRK-REGQTY
               MOVE MOVIES-REG TO REPORT-MOVIES-REG

               WRITE REPORT-MOVIES-REG
               IF REPORT-MOVIES-STATUS = 0
                   ADD 1 TO WRK-WRITTEN-RECORDS
               END-IF

               READ MOVIES NEXT
                   AT END
                       MOVE 10 TO MOVIES-STATUS
               END-READ
            END-PERFORM.

       0330-REPORT-STATISTICS  SECTION.
            MOVE 17 TO WRK-LINE.
            DISPLAY WRK-MSG-READRECORDS LINE WRK-LINE COLUMN 10
               BACKGROUND-COLOR 3.
            DISPLAY WRK-REGQTY          LINE WRK-LINE COLUMN 26
               BACKGROUND-COLOR 3.

            ADD 1 TO WRK-LINE.
            DISPLAY WRK-MSG-WRITTENREC  LINE WRK-LINE COLUMN 10
               BACKGROUND-COLOR 3.
            DISPLAY WRK-WRITTEN-RECORDS LINE WRK-LINE COLUMN 26
               BACKGROUND-COLOR 3.

            ADD 1 TO WRK-LINE.
            DISPLAY WRK-MSG-PRESSKEY    LINE WRK-LINE COLUMN 10
               BACKGROUND-COLOR 3.
            ACCEPT WRK-KEY              LINE WRK-LINE COLUMN 26
               BACKGROUND-COLOR 3.

       0400-FINALIZE           SECTION.
            CLOSE MOVIES.
            GOBACK.

       9000-MANAGE-ERROR       SECTION.
            ACCEPT ERROR-SCREEN.
            MOVE SPACES TO WRK-ERROR-MSG.
