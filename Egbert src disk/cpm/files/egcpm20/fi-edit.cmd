
* Program..: FI-EDIT.CMD
* Author...: Your Name
* Date.....: 00/00/00
* Notice...: Copyright 1900, All Rights Reserved
*
DO WHILE T
   STORE " " TO editchoice
   @ 12,0 SAY "COMMAND: (E)dit, (D)elete, (U)ndelete, "+;
              "(C)ontinue, (P)osition ";
          GET editchoice PICTURE "!"
   READ NOUPDATE
   CLEAR GETS
   @ 12,0 SAY clearline
   DO CASE
      CASE editchoice = " "
      * ---Exit.
         RETURN
      CASE editchoice = "P"
      * ---(P)osition
         STORE "X" TO poschoice
         DO WHILE poschoice <> " "
            DO FI-posn
         ENDDO
      CASE editchoice = "D"
      * ---(D)elete
         DELETE
         @ 1,55 SAY "DELETED"
      CASE editchoice = "U"
      * ---(U)ndelete
         RECALL
         @ 1,55 SAY "       "
      CASE editchoice = "E"
      * ---(E)dit
         @ 12,0 SAY "Press <control-W> to exit"
         IF # <> 0
            DO FI-some
            READ
         ENDIF
      CASE editchoice = "C"
      * ---(C)ontinue to the next record.
         STORE # TO lastrecord
         CONTINUE
         * ---Check for END-OF-FILE.
         IF .NOT. EOF
            DO FI-gets
            CLEAR GETS
         ELSE
            * ---EOF encountered.
            GOTO lastrecord
            @ 12,0 SAY clearline
            @ 12,0 SAY "END-OF-FILE encountered"
            STORE " " TO select
            @ 13,0 SAY "Strike any key to continue...";
                   GET select
            READ NOUPDATE
            @ 12,0 SAY clearline
            @ 13,0 SAY clearline
         ENDIF
   ENDCASE
ENDDO
* EOF: FI-EDIT.CMD
�����ő���
�
������������߫�ߜ������
�
�
���������
���ߺ���ߢ��ߐ������
�
����������߰��
�
���������������Һ������ߚ��������߫�ߐ������
����������߫��ِ������
����������߰�
���߯���������ߢԐ������
���߾���������ߢԆ�������
���߻���������ߢԻ�����
���߱���������߼����������Ƣ��׻��������������������������ߤ�߾��߭�����߭��������
��բ
����ߨ����߫�
����߬��������߫�ߚ����������
����߿