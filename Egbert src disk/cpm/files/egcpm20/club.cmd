
* Program..: CLUB.CMD
* Author...: Your Name
* Date.....: 00/00/00
* Notice...: Copyright 1900, All Rights Reserved
* Notes....: 
* Reserved.: select, selectnum
*
SET TALK OFF
SET BELL OFF
SET COLON OFF

DO WHILE T

ERASE
@  1, 0 SAY "========================================"
@  1,40 SAY "========================================"
@  2, 0 SAY "||"
@  2,33 SAY "M E N C L U B"
@  2,78 SAY "||"
@  3, 0 SAY "========================================"
@  3,40 SAY "========================================"
@  4, 0 SAY "||"
@  4,78 SAY "||"
@  5, 0 SAY "||"
@  5,78 SAY "||"
@  6, 0 SAY "||"
@  6,78 SAY "||"
@  7, 0 SAY "||"
@  7,78 SAY "||"
@  8, 0 SAY "||"
@  8,78 SAY "||"
@  9, 0 SAY "||"
@  9,78 SAY "||"
@ 10, 0 SAY "||"
@ 10,78 SAY "||"
@ 11, 0 SAY "||"
@ 11,78 SAY "||"
@ 12, 0 SAY "||"
@ 12,78 SAY "||"
@ 13, 0 SAY "========================================"
@ 13,40 SAY "========================================"
@  5,33 SAY " 0. exit"
@  6,33 SAY " 1. suchen"
@  7,33 SAY " 2. editieren"
@  8,33 SAY " 3. erweitern"
@  9,33 SAY " 4. listen"
@ 10,33 SAY " 5. ende"
STORE  6 TO selectnum
DO WHILE selectnum < 0 .OR. selectnum >  5
   STORE " " TO select
   @ 13,33 SAY " select : : "
   @ 13,42 GET select PICTURE "#"
   READ
   STORE VAL(select) TO selectnum
ENDDO

DO CASE
   CASE selectnum= 0
      SET COLON ON
      SET BELL ON
      SET TALK ON
      CLEAR
      RETURN
   CASE selectnum= 1
   *  DO suchen
   CASE selectnum= 2
   *  DO editieren
   CASE selectnum= 3
   *  DO erweitern
   CASE selectnum= 4
   *  DO listen
   CASE selectnum= 5
   *  DO ende
ENDCASE

ENDDO T
* EOF: CLUB.CMD
�ߜ���������ݬ�
����߰�
�
����������߫��ِ������
����������߰�
���߯���������ߢԐ������
���߾���������ߢԆ�������
���߻���������ߢԻ�����
���߱���������߼����������Ƣ��׻��������������������������ߤ�߾��߭�����߭��������
���߱���������ߢ
���߭���������ߌ������ߌ���������
��բ
�����߫���߰���
�����߽���߰���
�����߼����߰���
�
����ߨ����߫�
�
��