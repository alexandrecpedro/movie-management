       IDENTIFICATION          DIVISION.
       PROGRAM-ID. MENU.
      ******************************************************************
      * Author: ALEXANDRE PEDRO
      * Company: XPTO
      * Date: 21/03/2025
      * Purpose: MOVIES MANAGEMENT SYSTEM
      ******************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT       IS COMMA.

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *      SELECT MOVIES ASSIGN TO "./Data/MOVIES.DAT"
            SELECT MOVIES ASSIGN TO "C:\Cobol\Project\Data\MOVIES.DAT"
            ORGANIZATION IS INDEXED
            ACCESS MODE IS SEQUENTIAL
            FILE STATUS IS MOVIES-STATUS
            RECORD KEY IS MOVIES-KEY.

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

       WORKING-STORAGE         SECTION.
      *---------------------------- DATA ENTRY VARIABLES
       77 WRK-KEY                    PIC X(001) VALUE SPACE.
       77 WRK-OPTION                 PIC X(001) VALUE SPACE.

      *---------------------------- ERROR MESSAGES
       77 WRK-ERROR-MSG              PIC X(030) VALUE SPACES.

       01   WRK-MSGS.
            05 WRK-MSG-CORRUPTED     PIC X(030) VALUE
               "CORRUPTED FILE".
            05 WRK-MSG-OPEN          PIC X(030) VALUE
               "ERROR WHILE OPENING FILE".
            05 WRK-MSG-OPTION        PIC X(030) VALUE
               "INVALID OPTION! TRY AGAIN".
            05 WRK-MSG-PATH          PIC X(030) VALUE
               "ERROR ON FILE PATH".

      *---------------------------- FILE
       77 MOVIES-STATUS              PIC 9(002) VALUE ZEROS.

      *---------------------------- TITLES
       01 WRK-TITLE.
            05 WRK-SCREEN-TITLE      PIC X(020) VALUE "MOVIE SYSTEM".
            05 WRK-MODULE-TITLE      PIC X(026) VALUE SPACES.

       SCREEN                  SECTION.
      *---------------------------- SCREEN LAYOUT
       01 CLEANER-SCREEN.
            05 CLEAN-SCREEN.
                10 BLANK SCREEN.
                10 LINE 01 COLUMN 01 PIC X(020) ERASE EOL
                    BACKGROUND-COLOR 3.
                10 LINE 01 COLUMN 15 PIC X(020)
                    BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
                    FROM WRK-SCREEN-TITLE.
                10 LINE 02 COLUMN 01 PIC X(025) ERASE EOL
                    BACKGROUND-COLOR 1.
                10 LINE 02 COLUMN 14 PIC X(026)
                    BACKGROUND-COLOR 1 FOREGROUND-COLOR 6
                    FROM WRK-MODULE-TITLE.

      *---------------------------- MENU
       01 SHOW-MENU.
            05 LINE 07 COLUMN 15 VALUE "1 - REGISTER".
            05 LINE 08 COLUMN 15 VALUE "2 - SEARCH".
            05 LINE 09 COLUMN 15 VALUE "3 - UPDATE".
            05 LINE 10 COLUMN 15 VALUE "4 - DELETE".
            05 LINE 11 COLUMN 15 VALUE "5 - REPORT ON SCREEN".
            05 LINE 12 COLUMN 15 VALUE "6 - REPORT ON DISK".
            05 LINE 13 COLUMN 15 VALUE "X - EXIT".
            05 LINE 15 COLUMN 15 VALUE "OPTION...: ".
            05 LINE 15 COLUMN 26     PIC X(001) USING WRK-OPTION.

      *---------------------------- ERROR SCREEN
       01 ERROR-SCREEN.
            05 MSG-ERROR.
                10 LINE 18 COLUMN 01 ERASE EOL
                   BACKGROUND-COLOR 3.
                10 LINE 18 COLUMN 10 PIC X(030)
                   BACKGROUND-COLOR 3
                   USING WRK-ERROR-MSG.
                10 COLUMN PLUS 2     PIC X(001)
                   BACKGROUND-COLOR 3
                   USING WRK-KEY.

       PROCEDURE               DIVISION.

       0100-MAIN               SECTION.
            PERFORM 0200-INITIALIZE.
            PERFORM 0300-PROCESS UNTIL WRK-OPTION = "X" OR "x".
            PERFORM 0400-FINALIZE.
            STOP RUN.

       0200-INITIALIZE         SECTION.
            OPEN I-O MOVIES.
            PERFORM 0210-VERIFICATION.

       0210-VERIFICATION.
            EVALUATE MOVIES-STATUS
              WHEN 30
                MOVE WRK-MSG-PATH TO WRK-ERROR-MSG
              WHEN 35
                OPEN OUTPUT MOVIES
                CLOSE MOVIES
                OPEN I-O MOVIES
                CONTINUE
              WHEN 42
                MOVE WRK-MSG-CORRUPTED TO WRK-ERROR-MSG
              WHEN OTHER
                CONTINUE
            END-EVALUATE.

            IF WRK-ERROR-MSG NOT EQUAL SPACES
               PERFORM 9000-MANAGE-ERROR
            END-IF.

       0220-DISPLAY-MENU.
            DISPLAY CLEANER-SCREEN.
            ACCEPT SHOW-MENU.

       0300-PROCESS            SECTION.
            EVALUATE WRK-OPTION
              WHEN 1
                 MOVE "MODULE - INSERT " TO WRK-MODULE-TITLE
                 CALL "INSERT" USING WRK-TITLE
              WHEN 2
                 MOVE "MODULE - QUERY " TO WRK-MODULE-TITLE
                 CALL "QUERY" USING WRK-TITLE
              WHEN 3
                 MOVE "MODULE - UPDATE " TO WRK-MODULE-TITLE
                 CALL "UPDATE" USING WRK-TITLE
              WHEN 4
                 MOVE "MODULE - DELETE " TO WRK-MODULE-TITLE
                 CALL "DELETE" USING WRK-TITLE
              WHEN 5
                 MOVE "MODULE - REPORT ON SCREEN " TO WRK-MODULE-TITLE
                 CALL "REPORTSCREEN" USING WRK-TITLE
              WHEN 6
                 MOVE "MODULE - REPORT ON DISK " TO WRK-MODULE-TITLE
                 CALL "REPORTDISK" USING WRK-TITLE
              WHEN OTHER
                 IF WRK-OPTION = "X" OR "x"
                     CONTINUE
                 END-IF
                 MOVE WRK-MSG-OPTION TO WRK-ERROR-MSG
                 PERFORM 9000-MANAGE-ERROR
            END-EVALUATE.

            MOVE SPACE TO WRK-OPTION WRK-KEY WRK-MODULE-TITLE.
            MOVE SPACES TO MOVIES-TITLE MOVIES-GENRE MOVIES-DISTRIBUTOR.
            MOVE ZEROS TO MOVIES-KEY MOVIES-DURATION MOVIES-RATING.

            PERFORM 0220-DISPLAY-MENU.

       0400-FINALIZE           SECTION.
            CLOSE MOVIES.
            GOBACK.

       9000-MANAGE-ERROR       SECTION.
            ACCEPT ERROR-SCREEN.
            MOVE SPACES TO WRK-ERROR-MSG.
