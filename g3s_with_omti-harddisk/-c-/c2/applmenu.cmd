SET TALK OFF
STORE t TO xapplsw
DO WHILE xapplsw
  ERASE
  @ 1,20 SAY '**** DBASE II APPLICATION MENU ****'
  @ 3,0 SAY '0.  EXIT MENU'
  @ 3,40 SAY '1.  Run DB-SQZ5 on a .CMD file'
  @ 4,0 SAY '2.  Run dUTIL on a .CMD file'
  @ 4,40 SAY '3.  Run fastBASE Utility on .CMD file'
  @ 5,0 SAY '4.  Run COMPDB on a .CMD file'
  @ 5,40 SAY '5.  Run UNCOMPDB on a .CMD file'
  @ 6,0 SAY '6.  Run DBSQUASH on a .CMD file'
  @ 6,40 SAY '7.  Run RECOVER on a .DBF file'
  @ 7,0 SAY '8.  Run DBCLINIC on a .DBF file'
  @ 7,40 SAY '9.  Run ZIP for screen generation'
  @ 8,0 SAY 'A.  Run dFASTEST'
  @ 8,40 SAY 'B.  Run dSORT on a .DBF file'
  @ 9,0 SAY 'C.  Run MAIL.CMD'
  @ 9,40 SAY 'D.  Run UNIQUE on a .DBF file'
  @ 10,0 SAY 'E.  Run DECODE on a "sq" .CMD file'
  @ 10,40 SAY 'F.  Run RECODE on a "usq" .CMD file'
  @ 11,0 SAY 'G.  Run fastBASE .CMD file generator'
  @ 11,40 SAY 'H.  Run WordStar'
  @ 16,28 SAY '**** ENTER OPTION ****'
  WAIT TO xoption
  STORE !(xoption) TO xoption
  DO CASE
    CASE xoption= '0'
      STORE f TO xapplsw
    CASE xoption= '1'
      ERASE
      CLEAR
      DO db-sqz5
    CASE xoption= '2'
      ERASE
      CLEAR
      SET CONSOLE OFF
      QUIT TO 'DUTIL0'
    CASE xoption= '3'
      ERASE
      CLEAR
      SET CONSOLE OFF
      QUIT TO 'FBUT'
    CASE xoption= '4'
      ERASE
      CLEAR
      SET CONSOLE OFF
      QUIT TO 'COMPDB'
    CASE xoption= '5'
      ERASE
      CLEAR
      SET CONSOLE OFF
      QUIT TO 'UNCOMPDB'
    CASE xoption= '6'
      ERASE
      CLEAR
      SET CONSOLE OFF
      QUIT TO 'MBASIC DBSQUASH'
    CASE xoption= '7'
      ERASE
      CLEAR
      SET CONSOLE OFF
      QUIT TO 'RECOVER'
    CASE xoption= '8'
      ERASE
      CLEAR
      SET CONSOLE OFF
      QUIT TO 'MBASIC DBCLINIC'
    CASE xoption= '9'
      ERASE
      CLEAR
      SET CONSOLE OFF
      QUIT TO 'ZIP'
    CASE xoption= 'A'
      ERASE
      CLEAR
      DO dfastest
    CASE xoption= 'B'
      ERASE
      CLEAR
      SET CONSOLE OFF
      QUIT TO 'DSORT'
    CASE xoption= 'C'
      ERASE
      CLEAR
      DO mail
    CASE xoption= 'D'
      ERASE
      CLEAR
      DO unique
    CASE xoption= 'E'
      DO WHILE t
        ERASE
        STORE '?' TO which
        @ 12,20 SAY 'Run DEC8 or DEC16 ?'
        @ 13,25 SAY '(A) DEC8'
        @ 14,25 SAY '(B) DEC16'
        @ 16,20 SAY '---> ' GET which
        READ
        IF !(which)= 'A'
          CLEAR
          SET CONSOLE OFF
          QUIT TO 'DEC8'
        ELSE
          IF !(which)= 'B'
            CLEAR
            SET CONSOLE OFF
            QUIT TO 'DEC16R8'
          ENDIF !(which)= 'B'
          LOOP
        ENDIF !(which)= 'A'
      ENDDO WHILE t
    CASE xoption= 'F'
      ERASE
      CLEAR
      SET CONSOLE OFF
      QUIT TO 'RECODE'
    CASE xoption= 'G'
      ERASE
      CLEAR
      SET CONSOLE OFF
      QUIT TO 'FBASMAIN'
    CASE xoption= 'H'
      ERASE
      CLEAR
      SET CONSOLE OFF
      QUIT TO 'WS0'
    OTHERWISE
      @ 22,27 SAY '**** INVALID OPTION ****' +chr(7)
      WAIT
      @ 22,0
  ENDCASE
ENDDO WHILE xapplsw
RETURN
