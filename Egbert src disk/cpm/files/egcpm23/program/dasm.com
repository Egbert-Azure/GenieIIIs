��	��

Interaktiver Z80-Disassembler

$ �v"*�'|�(F#̈́*�'�*�'"�'�#"�'�F!U�{͘�
8*�'#�8�0#"�'�_ !�^#V���*�'� ~�#��� �#~2�'��  >	�)&#~�(��*�':�'�� -39BQZ�������-?Pvz��������5?y������%�@��%�b��%�v��%��v���%��v����%�|��%���%~�8�L��%�b��*�'�@��%�b��*�'#~� 8�[0��>'�)&:�'���%����~�
8��>H�)&��%�0�)&��%�#~��>H�)&���%�#~��>H�)&���>A�)&��%�+*�'��#~_ *�'������(��%�v��*�'�	�(��%���(��%��	���������>H�)&>L�)&��(��%������(��%���*�'�	�(��%#F"�'!,�{͘�hX�#"�'F!s�{͘���h�*�h�!#�e#�eY�#"�'F!,	�{͘�(�h�*�h##�eͱ���@��%ͼ��%�ͼ���%ͼ��{�X~!�(!�̀��%#����%�b��*�'#����%#����*�'�@��%ͼ���	�(��%���	������ͼ���(��%ͼ����	����(��%#�����Z##F!,��{͘�x�� :�'*�'���(*�'++"�'��##ͱ��++���b���>C�)&���%~�>Iʗ>D�)&~���%>R�)&��%��	�����*�'�v���(��%�v��*�'��	����(��%*�'+"�'*�'+ʹ&DB	 �[>,�)&>	�)&>;�)&>(�)&>)�)&��>A�)&~�8!0�i~�)&#~�.�)&�~��NZZ.NCC.POPEP.M.~!n���R�i~�)&�>H�)&>L�)&�~�A�o�$�BCDEHLMA~!�~!���i~�)&#~�)&BCDEHLSPBCDEHLAFBCDEIXSPBCDEIYSP~��0�)&>I�)&{�)&�>'�)&��'�)&��)&��)&���d$�>0�)&��d$~�)&#����ͼ>+�)&~��>H�)&�#^#V��c#����7�:�'��(���"�:�'������z�>0(z�(��{�d$>H�)&��{�
8����0��� EX DE,HL� EX AF,AF'�� EXX�� EX (SP),HL�' DAA�� DI�� EI�v HALT�/ CPL�? CCF�  NOP�7 SCF�� JP (HL)� RLCA� RLA� RRCA� RRA�� RET�� LD SP,HL��ADC A,��ADD A,��AND ��CP ��OR ��SBC A,��SUB ��XOR �DEC �INC �	ADD HL,�DEC �INC �
LD A,�LD ��POP ��PUSH ��RET ��RST �@	LD �
LD ��ADD A,��ADC A,��AND ��CP ��OR ��SUB ��SBC A,��XOR ��IN A,��OUT � JR �DJNZ �JR �LD ��JP ��CALL �:LD A,�*LD HL,�"LD �2LD ��CALL ��JP ��  ��  ��  ��� RLC �RRC �RL �RR � SLA �(SRA �8SRL �@BIT ��RES ��SET � *��EX (SP),��JP ��LD SP,��POP ��PUSH �	ADD �#INC �+DEC �4INC �5DEC ��ADD A,��ADC A,��SUB ��SBC A,��AND ��XOR ��OR ��CP �FLD �p LD �!!LD �""LD �*#LD �6$LD ��%  � *  ��*�p*  �D NEG�E RETN�F IM 0�V IM 1�^ IM 2�G LD I,A�M RETI�O LD R,A�W LD A,I�_ LD A,R�g RRD�o RLD�BSBC HL,�JADC HL,�@&IN �AOUT (C),�'LD�'CP�'IN��'OUT��'OT�C(LD �K)LD � *  ��*1:(ͭ&
� DASM Version 1.6                                                        �
 ��&��&:] �!8Pͭ&
Es werden alle Dateien gelesen von  !] ~�)&#���ͭ&
Einlesen beendet
 	� ͭ&
Hilfe mit <H>, Statistiken mit <?>
 ��&�2�'2�'2�'2�'1:(�~&:�'G�(� �GO�ɯ2�'2�'ͭ&
Kommandofehler!
 ��
;� A�B�CDFE�#FHWIoK�#L�O$Py-0R�W�U!"XZh?� ͭ&
� Befehlsliste von DASM �

� ;adr,text  � Kommentar einfuegen       � ;adr       � Kommentar anzeigen
� ;          � Kommentartabelle zeigen   � ;adr,      � Kommentar loeschen
� A          � Code/Datenbereiche suchen � B          � Symboltabelle erstellen
� C          � Kontrolltabelle anzeigen, Startadresse kann angegeben werden
� Cadr,kenn  � Kontrolleintrag B=DEFB H=Hexbyte W=DEFW S=DEFS E=END I=Instruktion
� D          � Speicherauszug anzeigen   � D=nn       � Bytezahl fuer D setzen
� DS         � Symboltabelle anzeigen    � DS.symb    � Speicherauszug ab Symbol
� Eadr,.symb � Symbol eintragen          � K.symbol   � Symbol loeschen
� Fwort,adr  � Hexwort ab <adr> suchen   � F          � Suche fortsetzen
� L          � Disassemblieren           � L=n        � Zeilenzahl fuer L setzen
� O          � Aktuellen Offset zeigen   � Oadr       � Neuen Offset setzen
� Pvon,bis   � Programmvorspann erzeugen � -          � Bildschirmanzeige aus
� Rdatei     � Datei lesen - .COM, .CTL=Kontrolle, .SYM=Symbole, .DOC=Kommentare
� Rdatei.ALL � Lese .CTL, .SYM, .DOC, und .COM auf einmal
� Wdatei     � Datei speichern: .ASM .CTL .SYM .DOC
� Wdatei.ALL � Speichere .ASM, .CTL, .SYM und .DOC auf einmal
� Uadr       � Adresse der Kommentartabelle setzen
� X          � DASM erneut starten und bisher erarbeitete Daten loeschen
� Z          � END in die .ASM-Datei schreiben
� ?          � Statistische Informationen ueber das Programm anzeigen
 ��
ͭ&Neustart von DASM (J/N)?  �l�J��
ͭ&
DASM wird erneut gestartet ...
 ��
� �_���%��!�'~���
�$� (�,��
#��>2�'ʹ&	ORG	 ��z��{�d$>H�)&��%��$���
�! /##~���
++~�#~�8	+~�#~����#F�#~+�A8H#~�+(B�-(>��F>2�'#~�)&�ʹ&	 = 	 ��~���+~�d$>H�)&��%�2�'##~�i#�-'þ����>2�'�'!�'~#� ���
*�'��*�'"�'�!�'~�ʣ�Sʛ�=�~�,�w�$�*�'"�'��(� (�,��
#��$�"�'�*�'�"�'*�'�"�'���
ͭ&
Adr   00   02   04   06    08   0A   0C   0E    ASCII-Text
----------------------------------------------------------------
 *�'�-'��[�r$���z$~�d$#}��z$}�}��z$}� ��~� 8�8> �)&#}�(����%"�':�'�:�'�0���
#�$#�"�'��,�k� �k���
��
��!�'~�. �!$�#0*�'! /^#V#~�����r$�>=�)&F>�O#~�)&#�A�z$����-'���%��
:�'=2�'���%>2�'�ͭ&
Der aktuelle Offset ist  *�'�r$��%��
!�'~�(��$���
�"�'�:�'�2\ 2h !�':�'� ��
�: ~�@2\ ##] �l�l�~�(#�.(�~���.#��> ��>2�'�2�'>2�'�2�'�2�'2�'�2�':�'2�'2�'!�'~�(N�,(>� (:�= !#�${���
2�'2�'~#�,(� (���
��
�$�"�'��(� (�,��
#�$�"�'�2�'�-':�'�(	:�'=��
*�':�'�:�'���
:�'�(3*�'�*�'~�m'8##�~#�m'0�+�[��*�'�>B�-�"�'�>I�-*�'|�(F*�'��)!8=##F#~�;+ $"�'.7>2�'�����#~�\ ��%��)& ���* "�'̓��%*�'���+8###~�I(W�E�h�#^#V�*�'��v"�"�'�2�'��S�$�Bʈ�Hʂ�W�T�)&ͭ&: Unzulaessiger CTL-Eintrag
 ��
�)�2�'��:�'/o:�'/g#>2�'ʹ&DS	 |��}�d$>H�)&��%�"�'�>2�'ʹ&DW	 *�'�*�'^#V���%*�'##"�'���%��2�'>�2�'�2�'<2�'ʹ&DB	 *�'�*�':�'�~(�
8� 8����>H�)&:�'�2�'�0�)&#�*�'#"�'�:�'� :�'��i*�'��c#�|:�'�2�'��|>,�)&Ü>'�)&:�'<2�'~�)&~�'�)&#�*�'#"�'�:�'� :�'�(3�*�'��c#�0(:�'<2�'�#0�8+~#� (~� 8��>'�)&��>'�)&��%�!�'~���
�$�� (�,��
#�$���
BK�! /#~+�8 ~�8~�w#~�w+##~���
#�o> �g�ͭ&
Anfang der Scratch-Area:  ! )�r$ͭ&
Ende der Scratch-Area:    * . �r$ͭ&

.COM-Anfang:  0100
.COM-Ende:     *�'�r$ͭ&

SYMTBL:   ! /�r$*�'�r$��%ͭ&PC:       *�'�r$��%ͭ&OFFSET:   *�'�r$��%*�'|�(ͭ&COMNTS:   *�'�r$*�'�r$��%ͩ����
ͭ&CTLTBL:   ! )�r$~#�##< �+++�r$��%ͭ&
Die Symbolkommentare sind A :�'�>N >U�)&>S�)&��%!�'~�(+�$�"�'*�'"�'��(� (�,��
#�$���
*�'"�'*�'��-'*�'~#"�'� �~� ����[+�r$���z$�:�'/_:�'/W��v">2�'ʹ&END
 �2�':�'���
>ͮ%�S%�2�'ͭ&Die .ASM-Datei wurde geschlossen.
 ��
�::�'�!e z'�$'«��ø!�'͟ͭ&
Die .DOC-Datei wird eingelesen ...
 �!�'͟ͭ&Die .SYM-Datei wird eingelesen ...
 �!�'͟ͭ&Die .CTL-Datei wird eingelesen ...
 ͈ !�'͟ͭ&Die .COM-Datei wird eingelesen ...
 ���e �2h �l!e �'�$' �ø!e �'�$'����
ʹ$*�'|�5ڦ ��� \ � � �� ���ͭ&Der letzte Block wurde in den Speicher gelesen bei  �r$ͭ&
Der letzte Block endet an der relativen Adresse  �["�'�r$��%� � �!e �'�$' Â !e �'�$'��
�øͭ&Achtung: Das wuerde den Disassembler ueberschreiben, weil der
Offset zu klein gewaehlt wurde:  ���! /�-"�'##6 �͗!*�'�-"�'6�#6��ʹ$��$���	(��(��
(��cs#r#�# ��$�	(	�(w#��p��0  � �����:8��0�))))�o���$�����::�'�!e z'�$'��!�'͟ͭ&
Die .DOC-Datei wird gespeichert ...
 ͌!�'͟ͭ&Die .SYM-Datei wird gespeichert ...
 �	!�'͟ͭ&Die .CTL-Datei wird gespeichert ...
 �Q ͭ&Soll eine neue .ASM-Datei angelegt werden (J/N)?  �l�J��
!}'͟>2�'�2�'�2%ͭ&Speichern von .ASM ist moeglich.
Benutzen Sie das <Z>-Kommando oder <E>, um die Datei zu schliessen.
 ��
!e �'�$'�Z�	��
! /�2%^#V#~#G��5�>> ͮ%~ͮ%#�>ͮ%>
ͮ%�>ͮ%�S%�z�U$ͮ%z�Y$ͮ%{�U$ͮ%{�Y$î%!e �'�$'�K !e �'�$'(!e }'�$'��
�{͌��
͗!*�'##6 *�'�ͭ&Es kann keine andere Datei geoeffnet werden, solange .ASM offen ist!
 ��
ͭ&Vorzeitiges Dateiende!
 ��
��!�'~����$���� (�,��
#~�>��
�����+8###�Ѿ��2�'��ډ:�'�I(�W(�H(�B(�S(�E(�(��
##w��! �~#�+<(	 �����ͩ��
:�'�K��
�E(�H(�B(�W(	�I(�S��
��! )~#�##< �T]###��+~}� �|� ��s#r#:�'w�! )~#�+<7�#z�+� {���###���! )�-'^#V#z�< 	��%ͩ��
���c#8����%~�)&#�>:�)&���z$�z$��r$>=�)&> �)&�~�)&�z$�z$��#��Q ��
�2%! )^#V#z�<(�>>,ͮ%~ͮ%>ͮ%>
ͮ%#�>ͮ%�S%�͈ øʹ$! )��$  �($�,(�:8��0�))))�o���$�s#r#��$w#�6�#6��͗!!�'~��K!�A #~�U(����
2�'��
�$#��F!� (�,��
��)!8�s!��~���
*�'s#r#� �#w� �#"�'6�#6��p��
*�'~#�+<7�#z�+ {��##~�i#�,!�)!*�'�-'^#V#z�<��
��r$�>;�)&F#~�)&���%#�~#�<�+�T]##~�i# ~#O�<A ��++"�'��*�'|��ͭ&Kommando ignoriert!
Benutzen Sie <Unnnn>, um DASM mitzuteilen, da~ <nnnn> die Startadresse der Kommentartabelle sein soll.
 ��
!�'~���
�.��
�$�|�58"�'"�'6�#6���
ͭ&Nein! Adresse ist kleiner als  ! 5. �r$��%��
��>2�'*�'��c#ګ"H�~�+(�-(# ����>:�)&���(��%�2�'�z$�z$*�'�r$>2�'�����!�'6L#z�H${�H$!�'�#8s#r����*�'T]x��o|� g"�'##6 ++DM*�'{� z�(���s#r#p�#w����! /��##�~#�(,�8 O�$'(
8�~#�i��++�ѷ�O�$'0�H�$'(0��++"�'��7�! /~#� ~�(#~�7�#�o0�$�#~#G��7�!�'�$~� (�,��
#~�.��
���c#8$���%��ͭ& wurde geloescht
 �+++��#���!$�x�8� bk#	6 A����"��
!�'~�.��
�!$�#��
��#��
DM##~#�i�*�'{� z�(�`i"�'##6 �# �~#�+(��-(��08�:8��A8�[8���
+�����U$w#��Y$w#���
8��0���U$�)&��Y$�)&|�d$}�d$> �)&~�.(   ~�0��:8�A��G���0�))))�o�#��!$��#��
^#V�~��! "�'�2| \ � �<�ͭ&Datei nicht gefunden!
 ����*�'|� ��\ � ���!�  ~#"�'���8ͭ&Vorzeitiges Dateiende!
 ��
>w��\ � \ � <��
�2| !� "�'��\ � � 4\ � <�ͭ&Fehler beim Schliessen der Datei!
 ��
ͭ&Fehler beim Speichern!
 �*�'w,"�'�����\ � � �!� "�'����*�'|�(��F��(#~�)&�!  "�'"�'>�)&>
�)&>
�)&>*&���� ���}������ �����>p�>q�>������� �&�"�&� &�&&�� �&�"�&�#&�&&_�:�'�� �:�'!�'�{Į%{�
 
:�'�(!�'5�������%�'>P
� !�'^ #6!�'#~���%�a8��{0��_w�2�'2�'�~�)&#~� �#��>P2�'>2�'>2�'2�'!�'"�'! "�'"�'"�'! O"�'!��"�'! �"�'"�'ɯ2/! /"�'>2�'!��" )>I2�(! �"�'"�'6�#6����# ������ �(1� �(��%��
ͭ&
Abbrechen (J/N)?  �l�J�  ��
�������
�� ��?�ALLASMCOMCTLSYMDOC.                                                                                                                                                                                                                                                                                                                                                                                   