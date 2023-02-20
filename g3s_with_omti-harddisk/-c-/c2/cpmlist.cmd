
* Program..: CPMLIST.CMD
* Author...: Your Name
* Date.....: 10/07/93
* Notice...: Copyright 1993, All Rights Reserved
* Reserved.: pagenum, line, pagehdg, col:hdg, condition,
*            lastrec
*
SET TALK OFF
SET BELL OFF
SET MARGIN TO 1
STORE 1 TO pagenum
STORE 254 TO line
STORE "CP/M Roundtable File-List" TO pagehdg
STORE (80-LEN(pagehdg))/2 TO col:hdg
*
* ---Open the datafile and print the report.
USE CPMLIBS
ERASE
@ 2, 0 SAY pagehdg
@ 2,72 SAY DATE()
@ 3, 0 SAY "========================================"
@ 3,40 SAY "========================================"
STORE " " TO select
@ 5,0 SAY "Output to the screen or printer? [S/P] ";
      GET select PICTURE "!"
READ
DO CASE
   CASE select = "S"
      ERASE
      STORE 22 TO pagelen
   CASE select = "P"
      SET FORMAT TO PRINT
      STORE 70 TO pagelen
   OTHERWISE
      ERASE
      SET BELL ON
      SET TALK ON
      RETURN
ENDCASE
* ---Enter FOR <expression> for the report, such as,
* ---STORE "STATE = 'CA'" TO condition
STORE " " TO condition
DO WHILE .NOT. EOF
   IF line > pagelen
      IF select = "S"
         ERASE
      ELSE
         EJECT
      ENDIF
      @ 0,0 SAY "PAGE NO."
      @ 0,9 SAY STR(pagenum,3)
      @ 2,col:hdg SAY pagehdg
      *
      * ---Generate column headings.
      @ 4,  0 SAY "Nummer"
      @ 4, 11 SAY "Name"
      @ 4, 26 SAY "Datum"
      @ 4, 35 SAY "Beschreibung"
      @ 5,  0 SAY "========"
      @ 5, 11 SAY "============"
      @ 5, 26 SAY "======"
      @ 5, 35 SAY "========================================"
      STORE  pagenum+1 TO pagenum
      STORE 7 TO line
   ENDIF
   * ---Test to see if the condition exists.
   IF condition <> " "
      IF .NOT. (&condition)
         SKIP
         LOOP
      ENDIF
   ENDIF
   *
   * ---Print detail line.
   @ line,  0 SAY $(filenum,1,  8)
   @ line, 11 SAY $(name,1, 12)
   @ line, 26 SAY $(date,1,  6)
   @ line, 35 SAY $(descript,1, 40)
   STORE line+1 TO line
   SKIP
ENDDO
@ line+1,0 SAY " "
SET FORMAT TO SCREEN
RELEASE ALL
SET TALK ON
SET BELL ON
RETURN
* EOF: CPMLIST.CMD
��������Ѿ���߳��׋�������������
�����բ
���������Ҿ���������ߋ�����ߞ��А�ߌ��������Ѣ
�����߫�ߖ���
��߫�ߌ���������
�������������߳��׋��������
�������׋�������ӌ������������
����߬����ߋ����ٖ���Ԣ���ٲ��������������������ߤ߫�ߋ����ٖ����
�������������߫�ߌ���������
�
����ש��ז���������߫�ߖ���
�
�
������������Ѿ���߳��׌�������������
�����߫�ߖ���
��߫�ߌ���������
�������������߳��׌��������
�������׌�