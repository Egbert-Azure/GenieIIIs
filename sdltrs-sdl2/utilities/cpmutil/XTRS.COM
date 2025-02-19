òXTRS - controls miscellaneous functions of the XTRS emulator

Usage:	XTRS action [parameters]

Where:	action is one of the following:
	   BOOT - reboot emulator
	   CHANGE - signal disk change
	   DEBUG - enter debugger
	   EXIT - end emulator
	   HIGHSPEED - high speed (autodelay off)
	   MOUNT [-L] hostfilename disknum - mount disk
	   NORMALSPEED - normal speed (autodelay on)
	   REPORT - report status
	   SYSTEM command - execute command on host
	   UMOUNT disknum - umount disk
	hostfilename is the name of the virtual disk file on the host
	   (converted to uppercase by default), use -L to convert to
	   lowercase and [ to toggle case of next character.
	command is the command (and parameters) to execute on the host
	   (converted to lowercase by default), use [ to toggle case.
	   Note output from command is NOT displayed in the XTRS window.
	disknum is disk drive number (between 0 and 3 inclusive).
	-L converts hostfilename to lowercase.$
$ERROR: Expecting hostfilename
$ERROR: Expecting disknum
$ERROR: Expecting command
$ERROR: Extra parameter: $ERROR: Bad action
$ERROR: Bad disknum
$ERROR: Bad option "x"
$HOST COMMAND: $HOST COMMAND failed: $MOUNT failed - does hostfilename exist?
$UMOUNT failed
$SYSTEM command returned non-zero status
$** disk change **
$Graphics ..: $Radio Shack
$Micro Labs
$Speed .....: $Normal
$High
$Model .....: $Internal Error: bad variable in command skeleton
$                                                                                                                                         rm -f %d;test -f %h && ln -s %h %d rm -f %d                                                                                                                                                                                                                                                                                                                                               1�:� �(F!� ͅ(>~�9�B�	�C�	�D�$	�E�,	�H�4	�M�:	�N�y	�R�	�S��	�U�
I	� 	� �  >��		> �	� �		>��		>��		��		�q�N
��
�ͱͅ��
/���#ͅ�!d8́
ͽ
��/x��8�	ͩ�		͋	ͮ	��	�		n	� �M�� �(_� �	� �P	� >�y�(	^	� �g	� �'	� >
�}�(	C	� �5	� ��q>ͭ�dͱ��
 �!dͽ
��Wx��`�		�q��
/���#ͅ�!d[́
ͽ
��Cx��L�	~���-�#~�f
#~���  �ͅ��9�L(2�r	� �	>ͭ�����%(
w�# ������d(�h(|	� �		�/�i����i�������	� ��͎�	� �����~��� �#�> ���	� �	�	� �		� �	]	� �	�0	� �͎�	� �	���	�	� �	���	�	� �	���		�	� �		LIBHEX ����_� �������� 0>.�r���?�?�?�?��
8��0��͗O�͏��͏�r�͗�r�|ͬ}ͬ�������ͻ>:�r> �ry� 
> �r�r~ͬ#�> �r��y� 
> �r�r~́#�>�r>
�ry� �����������>R�r>E�r>G�r>S�r>:�r��A͇��ͬ> �r�́�B͇����ͻ��D͇����ͻ�H͇ͻ>�r>
�r���������� � �� =� �����LIBHFN  2�����~�(	� (#��> ���:���E��������~�(�[ #~�(�B#�> ������� (6.#�> w���:���E��� (w#����LIBSTR �A��[�� ��a��{�� �� ��~�(�0w#����~�(�0w#���~��#�w��#��{�ͅɷ�~��� (�#�~��� �#������~�(#�6$�	� �6 ����LIBXTRS Unable to obtain HOST error message
$** normal speed **
$** high speed **
$  �(��0��1��2��3��4��5��<���z�(	� �� ��~ ��C	6
#6$� 	� ���	� ��>�}��(�0_��4>P������:��(�2�: �(D���������>�y�(�2 >2�>!    ��	� �����>�y� �2 >2�>!   ��	� ��08,�40(����6d#6i#6s#6k#��Ms#(�0w#�6-#�w#6 �ɯ�