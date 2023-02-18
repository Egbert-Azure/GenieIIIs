
* Program..: FI-FRAME.CMD
* Author...: Your Name
* Date.....: 00/00/00
* Notice...: Copyright 1900, All Rights Reserved
*
@  2, 0 SAY "----------------------------------------"
@  2,40 SAY "----------------------------------------"
@  4, 0 SAY "Date......:"
@  5, 0 SAY "Input.....:"
@  6, 0 SAY "Output....:"
@  7, 0 SAY "Grund.....:"
@  8, 0 SAY "Club......:"
@ 11, 0 SAY "----------------------------------------"
@ 11,40 SAY "----------------------------------------"
RETURN
* EOF: FI-FRAME.CMD
@ 12,0 SAY "EMPTY DATA FILE"
         @ 13,0 SAY "Strike any key to continue...";
                GET select
         READ NOUPDATE
      ELSE
         * ---The data file contains records.
         DO FI-gets
         CLEAR GETS
         DO FI-edit
      ENDIF
      USE
   CASE selectnum= 4
   *  DO pack
      DO FI-pack
ENDCASE

ENDDO T
* EOF: FI-MAIN.CMD
ﬂﬂ∂πﬂ‹ﬂ¬ﬂœ¢
ã§ﬂﬂﬂﬂﬂﬂﬂﬂﬂ’ﬂ“““´óöﬂõûãûﬂôñìöﬂñåﬂöíèãÜ—¢
ã§ﬂﬂﬂﬂﬂﬂﬂﬂﬂ¨´∞≠∫ﬂ›ﬂ›ﬂ´∞ﬂåöìöúã¢
ã§ﬂﬂﬂﬂﬂﬂﬂﬂﬂøﬂ¢‘¨´≠◊ìûåãìñëö‘Œ”Õ÷‘§”œﬂ¨æ¶