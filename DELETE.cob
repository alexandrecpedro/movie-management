       IDENTIFICATION          DIVISION.
       PROGRAM-ID. DELETE.
      ******************************************************************
      * Author: ALEXANDRE PEDRO
      * Company: XPTO
      * Date: 24/03/2025
      * Purpose: DELETE MOVIES FROM MOVIES.DAT
      ******************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT       IS COMMA.
       
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
             SELECT MOVIES ASSIGN TO "./Data/MOVIES.DAT"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS RANDOM
             FILE STATUS IS MOVIES-STATUS
             RECORD KEY IS MOVIES-KEY.

       DATA                    DIVISION.
       FILE                    SECTION.
      *---------------------------- FILE DESCRIPTION 
       FD MOVIES.
       01 MOVIES-REG.
            05 MOVIES-KEY           PIC 9(005).
            05 MOVIES-TITLE         PIC X(050).
            05 MOVIES-GENRE         PIC X(030).
            05 MOVIES-DURATION      PIC 9(003).
            05 MOVIES-DISTRIBUTOR   PIC X(040).
            05 MOVIES-RATING        PIC 9(002).


       WORKING-STORAGE         SECTION.
      *---------------------------- DATA ENTRY VARIABLES
      77 WRK-KEY                    PIC X(001).
      
      *---------------------------- FILE
       77 MOVIES-STATUS             PIC 9(02) VALUE ZEROS.

      *---------------------------- ERROR MESSAGES
       77 WRK-ERROR-MSG             PIC X(030) VALUE SPACES.

       01   WRK-MSGS.
            05  WRK-MSG-CORRUPTED   PIC X(030) VALUE
            "CORRUPTED FILE".
            05  WRK-MSG-NOTDELETED   PIC X(030) VALUE
            "ERROR WHILE DELETING ENTITY".
            05  WRK-MSG-NOTFOUND    PIC X(030) VALUE
            "ENTITY NOT FOUND".
            05  WRK-MSG-OPEN        PIC X(030) VALUE
            "ERROR WHILE OPENING FILE".
            05  WRK-MSG-PATH        PIC X(030) VALUE
            "ERROR ON FILE PATH".
       
       LINKAGE                 SECTION.
      *---------------------------- LINKAGE VARIABLES
       01 LNK-TITLE.
            05 LNK-SCREEN-TITLE     PIC X(020).
            05 LNK-MODULE-TITLE     PIC X(016).

       SCREEN                  SECTION.
      *---------------------------- SCREEN LAYOUT
       01 CLEANER-SCREEN.
            05 CLEAN-SCREEN.
               10 BLANK SCREEN.
               10 LINE 01 COLUMN 01 PIC X(020) ERASE EOL
                    BACKGROUND-COLOR 3.
               10 LINE 01 COLUMN 15 PIC X(020)
                    BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
                    FROM LNK-SCREEN-TITLE.
               10 LINE 02 COLUMN 01 PIC X(025) ERASE EOL
                    BACKGROUND-COLOR 1.
               10 LINE 02 COLUMN 14 PIC X(015)
                    BACKGROUND-COLOR 1 FOREGROUND-COLOR 4
                    FROM LNK-MODULE-TITLE.

      *---------------------------- MOVIE DATA SCREEN
       01 ENTITY-DATA-SCREEN.
            05 QUERY-KEY FOREGROUND-COLOR 2.
                10 LINE 10 COLUMN 10 VALUE "ID: ".
                10 COLUMN PLUS 2    PIC 9(005) USING MOVIES-KEY
                   BLANK WHEN ZEROS.
            05 ENTITY-DATA.
                10 LINE 11 COLUMN 10 VALUE "TITLE: ".
                10 COLUMN PLUS 2    PIC X(050) USING MOVIES-TITLE
                   BLANK WHEN SPACES.
                10 LINE 12 COLUMN 10 VALUE "GENRE: ".
                10 COLUMN PLUS 2    PIC X(030) USING MOVIES-GENRE
                   BLANK WHEN SPACES.
                10 LINE 13 COLUMN 10 VALUE "DURATION: ".
                10 COLUMN PLUS 2    PIC 9(003) USING MOVIES-DURATION
                   BLANK WHEN ZEROS.
                10 LINE 14 COLUMN 10 VALUE "DISTRIBUTOR: ".
                10 COLUMN PLUS 2    PIC X(040) USING MOVIES-DISTRIBUTOR
                   BLANK WHEN SPACES.
                10 LINE 15 COLUMN 10 VALUE "RATING: ".
                10 COLUMN PLUS 2    PIC 9(002) USING MOVIES-RATING
                   BLANK WHEN ZEROS.

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
       0220-VERIFICATION-MOVIES.
            EVALUATE MOVIES-STATUS
              WHEN 30
                MOVE WRK-MSG-PATH TO WRK-ERROR-MSG
                PERFORM 9000-MANAGE-ERROR
              WHEN 35
                MOVE WRK-MSG-OPEN TO WRK-ERROR-MSG
                PERFORM 9000-MANAGE-ERROR
              WHEN 42
                MOVE WRK-MSG-CORRUPTED TO WRK-ERROR-MSG
                PERFORM 9000-MANAGE-ERROR
            END-EVALUATE.

       0300-PROCESS            SECTION.
            DISPLAY CLEANER-SCREEN.
            DISPLAY ENTITY-DATA-SCREEN.
            ACCEPT QUERY-KEY.
            PERFORM 0310-READ THRU 0320-DELETE.

       0310-READ.
            READ MOVIES
               INVALID KEY
                   MOVE WRK-MSG-NOTFOUND TO WRK-ERROR-MSG
               NOT INVALID KEY
                   MOVE "DO YOU WANT TO PROCEED (Y/N)?" TO WRK-ERROR-MSG
                   DISPLAY ENTITY-DATA  
            END-READ.
            PERFORM 9000-MANAGE-ERROR.
       
       0320-DELETE.
            IF WRK-KEY = 'Y' AND MOVIES-STATUS = 0
               DELETE MOVIES
                   INVALID KEY
                       MOVE WRK-MSG-NOTDELETED TO WRK-ERROR-MSG
                       PERFORM 9000-MANAGE-ERROR
               END-DELETE
            END-IF.

       0400-FINALIZE           SECTION.
           CLOSE MOVIES.
           GOBACK.
       
       9000-MANAGE-ERROR       SECTION.
            ACCEPT ERROR-SCREEN.
