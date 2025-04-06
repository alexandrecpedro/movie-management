       IDENTIFICATION          DIVISION.
       PROGRAM-ID. REGISTER.
      ******************************************************************
      * Author: ALEXANDRE PEDRO
      * Company: XPTO
      * Date: 24/03/2025
      * Purpose: REGISTER MOVIES INTO MOVIES.DAT
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
             ACCESS MODE IS DYNAMIC
             FILE STATUS IS MOVIES-STATUS
             RECORD KEY IS MOVIES-KEY.

       DATA                    DIVISION.
       FILE                    SECTION.
      *---------------------------- FILE DESCRIPTION
       FD MOVIES.
       01 MOVIES-REG.
            05 MOVIES-KEY             PIC 9(005).
            05 MOVIES-TITLE           PIC X(030).
            05 MOVIES-GENRE           PIC X(008).
            05 MOVIES-DURATION        PIC 9(003).
            05 MOVIES-DISTRIBUTOR     PIC X(015).
            05 MOVIES-RATING          PIC 9(002).


       WORKING-STORAGE         SECTION.
      *---------------------------- DATA ENTRY VARIABLES
       77 WRK-KEY                     PIC X(001).

      *---------------------------- DISPLAY VARIABLES
       77 WRK-INSTRUCTIONS            PIC X(040) VALUE SPACES.
       77 WRK-LINE                    PIC 9(002) VALUE 01.

      *---------------------------- ERROR MESSAGES
       77 WRK-ERROR-MSG               PIC X(040) VALUE SPACES.

       01 WRK-MSGS.
            05 WRK-MSG-CONTINUE       PIC X(040) VALUE
               "DO YOU WANT TO REGISTER ANOTHER (Y/N)?".
            05 WRK-MSG-CORRUPTED      PIC X(040) VALUE
               "CORRUPTED FILE".
            05 WRK-MSG-DATA           PIC X(040) VALUE
               "ENTER ENTITY DATA. THEN PRESS ENTER".
            05 WRK-MSG-DUPLICATED     PIC X(040) VALUE
               "ENTITY ALREADY EXISTS".
            05 WRK-MSG-INFO           PIC X(040) VALUE
               "ENTER AN ID AND PRESS ENTER".
            05 WRK-MSG-OPEN           PIC X(040) VALUE
               "ERROR WHILE OPENING FILE".
            05 WRK-MSG-PATH           PIC X(040) VALUE
               "ERROR ON FILE PATH".
            05 WRK-MSG-PROCEED        PIC X(040) VALUE
               "DO YOU WANT TO PROCEED (Y/N)?".
            05 WRK-MSG-REGISTERED     PIC X(040) VALUE
               "REGISTERED ENTITY".
            05 WRK-MSG-NOTREGISTERED  PIC X(040) VALUE
               "ERROR WHILE REGISTERING ENTITY".
            05 WRK-MSG-UNKNOWN        PIC X(040) VALUE
               "UNKNOWN ERROR".

      *---------------------------- FILE
       77 MOVIES-STATUS               PIC 9(002) VALUE ZEROS.

      *---------------------------- FLAG
       77 WRK-EXIT-FLAG               PIC X(001).

       LINKAGE                 SECTION.
      *---------------------------- LINKAGE VARIABLES
       01 LNK-TITLE.
            05 LNK-SCREEN-TITLE       PIC X(020).
            05 LNK-MODULE-TITLE       PIC X(026).

       SCREEN                  SECTION.
      *---------------------------- ERROR SCREEN
       01 ERROR-SCREEN.
            05 MSG-ERROR.
                10 LINE 18 COLUMN 01 ERASE EOL
                   BACKGROUND-COLOR 3.
                10 LINE 18 COLUMN 10  PIC X(040)
                   BACKGROUND-COLOR 3
                   USING WRK-ERROR-MSG.
                10 COLUMN PLUS 2      PIC X(001)
                   BACKGROUND-COLOR 3
                   USING WRK-KEY.

      *---------------------------- MOVIE DATA SCREEN
       01 ENTITY-DATA-SCREEN.
            05 INFO-ENTITY.
                10 LINE 05 COLUMN 10  PIC X(040)
                   USING WRK-INSTRUCTIONS.
            05 QUERY-KEY FOREGROUND-COLOR 2.
                10 LINE 10 COLUMN 10 VALUE "ID: ".
                10 COLUMN PLUS 2      PIC 9(005) USING MOVIES-KEY
                   BLANK WHEN ZEROS.
            05 ENTITY-DATA.
                10 LINE 11 COLUMN 10 VALUE "TITLE: ".
                10 COLUMN PLUS 2      PIC X(030) USING MOVIES-TITLE.
                10 LINE 12 COLUMN 10 VALUE "GENRE: ".
                10 COLUMN PLUS 2      PIC X(008) USING MOVIES-GENRE.
                10 LINE 13 COLUMN 10 VALUE "DURATION: ".
                10 COLUMN PLUS 2      PIC 9(003) USING MOVIES-DURATION
                   BLANK WHEN ZEROS.
                10 LINE 14 COLUMN 10 VALUE "DISTRIBUTOR: ".
                10 COLUMN PLUS 2      PIC X(015)
                   USING MOVIES-DISTRIBUTOR.
                10 LINE 15 COLUMN 10 VALUE "RATING: ".
                10 COLUMN PLUS 2      PIC 9(002) USING MOVIES-RATING
                   BLANK WHEN ZEROS.

      *---------------------------- SCREEN LAYOUT
       01 CLEANER-SCREEN.
            05 CLEAN-SCREEN.
               10 BLANK SCREEN.
               10 LINE 01 COLUMN 01   PIC X(020) ERASE EOL
                    BACKGROUND-COLOR 3.
               10 LINE 01 COLUMN 15   PIC X(020)
                    BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
                    FROM LNK-SCREEN-TITLE.
               10 LINE 02 COLUMN 01   PIC X(025) ERASE EOL
                    BACKGROUND-COLOR 1.
               10 LINE 02 COLUMN 14   PIC X(026)
                    BACKGROUND-COLOR 1 FOREGROUND-COLOR 6
                    FROM LNK-MODULE-TITLE.

       PROCEDURE               DIVISION USING LNK-TITLE.

       0100-MAIN               SECTION.
            PERFORM 0200-INITIALIZE.
            PERFORM 0300-PROCESS UNTIL WRK-EXIT-FLAG = "Y".
            PERFORM 0400-FINALIZE.
            GOBACK.

       0200-INITIALIZE         SECTION.
            OPEN I-O MOVIES.
            PERFORM 0210-VERIFICATION.

       0210-VERIFICATION.
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

       0220-DISPLAY-SCREEN     SECTION.
            MOVE SPACES TO MOVIES-TITLE MOVIES-GENRE MOVIES-DISTRIBUTOR
               WRK-ERROR-MSG WRK-EXIT-FLAG WRK-KEY.
            MOVE ZEROS TO MOVIES-KEY MOVIES-DURATION MOVIES-RATING.

            DISPLAY CLEANER-SCREEN.
            MOVE WRK-MSG-INFO TO WRK-INSTRUCTIONS.
            DISPLAY ENTITY-DATA-SCREEN.

       0300-PROCESS            SECTION.
            PERFORM 0220-DISPLAY-SCREEN.
            ACCEPT QUERY-KEY.
            PERFORM 0310-READ.
            PERFORM 0220-DISPLAY-SCREEN.

            MOVE 18 TO WRK-LINE.
            DISPLAY WRK-MSG-CONTINUE LINE WRK-LINE COLUMN 10
               BACKGROUND-COLOR 3.
            ACCEPT WRK-KEY LINE WRK-LINE COLUMN 51
               BACKGROUND-COLOR 3.

            IF WRK-KEY = "Y" OR WRK-KEY = "y"
                MOVE "N" TO WRK-EXIT-FLAG
            ELSE
                MOVE "Y" TO WRK-EXIT-FLAG
            END-IF.

       0310-READ               SECTION.
            READ MOVIES
               INVALID KEY
                   MOVE WRK-MSG-DATA TO WRK-INSTRUCTIONS
                   DISPLAY INFO-ENTITY
                   ACCEPT ENTITY-DATA
                   PERFORM 0320-CONFIRM-WRITE
               NOT INVALID KEY
                   MOVE WRK-MSG-DUPLICATED TO WRK-ERROR-MSG
                   PERFORM 9000-MANAGE-ERROR.

       0320-CONFIRM-WRITE      SECTION.
            MOVE 18 TO WRK-LINE.
            DISPLAY WRK-MSG-PROCEED LINE WRK-LINE COLUMN 10
               BACKGROUND-COLOR 3.
            ACCEPT WRK-KEY LINE WRK-LINE COLUMN 51
               BACKGROUND-COLOR 3.

            IF WRK-KEY = "Y" OR WRK-KEY = "y"
               MOVE SPACES TO WRK-KEY
               PERFORM 0330-WRITE
            ELSE
               DISPLAY WRK-MSG-NOTREGISTERED LINE WRK-LINE COLUMN 10
                   BACKGROUND-COLOR 3
            END-IF.

       0330-WRITE              SECTION.
            WRITE MOVIES-REG
               INVALID KEY
                   MOVE WRK-MSG-NOTREGISTERED TO WRK-ERROR-MSG
               NOT INVALID KEY
                   MOVE WRK-MSG-REGISTERED TO WRK-ERROR-MSG.

            PERFORM 9000-MANAGE-ERROR.

       0400-FINALIZE           SECTION.
            CLOSE MOVIES.
            GOBACK.

       9000-MANAGE-ERROR       SECTION.
            MOVE SPACES TO WRK-KEY.
            ACCEPT ERROR-SCREEN.
            MOVE SPACES TO WRK-ERROR-MSG.
            MOVE SPACES TO WRK-KEY WRK-INSTRUCTIONS.
