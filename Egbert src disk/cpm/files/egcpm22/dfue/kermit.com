  �$�x��(CP4MIT.ASM (8)  6-Feb-85$!  9";41}4+�)� <2�4* 7|��fc�|��o*7��|��o�B7�/�)�x�,�)�	�,�)�	1}4�7�2�"2�":�4�@2++ʹ"�!E��(��<+�)�xBYE$�CONNECT$%	DIRECTORY$ ERASE$�!EXIT$FINISH$�GET$%HELP$LOG$LOGOUT$9RECEIVE$%SEND$�SET$<SHOW$
STATUS$%
TRANSMIT$�VERSION$�
BYE to host (LOGOUT) and exit to CP/M
CONNECT to host on selected port
ERASE a CP/M file
EXIT to CP/M
FINISH running Kermit on the host
HELP by giving this message
DIRECTORY of current used Micro-disk
LOG the terminal sessions to a file
LOGOUT the host
RECEIVE file from host
SEND file to host
SET a parameter
SHOW the parameters
STATUS of Kermit
TRANSMIT file to host (in connect state)
VERSION of Kermit running$��(�E�x�W7���(�E7*;4����(E�)�x>\ �#Þ��(!\ �4 �i7>2�4�xG!��(��	BAUD-RATE$�	BLOCK-CHECK-TYPE$�DEBUG$�DEFAULT-DISK$PESCAPE$�	FILE-MODE$)IBM$
LOCAL-ECHO$�LOGGING$�PARITY$nPORT$�	PRINTER$�TACTRAP$�TIMER$�VT52-EMULATION$t	WARNING$�
BAUD-RATE
BLOCK-CHECK-TYPE for error detection
DEBUG message control
DEFAULT-DISK to receive data
ESCAPE character during CONNECT
FILE-MODE for outgoing files
IBM mode: parity and turn around handling
LOCAL-ECHO (half-duplex)
LOGGING of terminal sessions
PARITY for communication line
PORT to communicate on
PRINTER copy control
TAC interface support
TIMER control
VT52-EMULATION
WARNING for filename conflicts$�!��	2�7�x1-CHARACTER-CHECKSUM$112-CHARACTER-CHECKSUM$223-CHARACTER-CRC-CCITT$33
1-CHARACTER-CHECKSUM
2-CHARACTER-CHECKSUM
3-CHARACTER-CRC-CCITT$\ >�#�[:\ ��e:�42�4� :�4=_� �x�!���(��PAD-CHAR$�PADDING$�
PAD-CHAR
PADDING$��(�x��(�x��	2�4�x��(E0�)� 2y7�x��	2�7�x��	2~7�x��	27�x��	2�4�x��	2�7�x��	2�7��>�> 2�7x2�72�4�x8!W��	2�7�xASCII$BINARY$DEFAULT$  
ASCII	BINARY	DEFAULT$}!���	2�7�xEVEN$  MARK$NONE$ODD$		SPACE$
EVEN	MARK	NONE	ODD	SPACE$�!	��	������h0�)� 2�7:�72�7�x	CHARACTER$OFF$  ON$
ON to enable TAC trap
OFF to disable TAC trap
CHARACTER to enable TAC trap and specify intercept character$:x7�����	2x7�x*n7|���*p7��(���(�"z7��Z7�x*r7|���*t7��(���(�"|7�]7�xOFF$  ON$
OFF	ON$�	!�	��(2�5��(:�5�V#F#"�5#��	N��
##��	!
7ɷ*�5�
?Not found$��(�'7�1
�x��(�'7�1
�x:�4��>
�1�)�x
:x7��Ą
͕
ͳ
Ϳ
��
��
��
*r7|��*n7|��<�u͊͖����1�):�7���1�):x7������1�):�7�1��)�1��)�1�)�1�):�7���1�):�7��2�):~7��2�):�4����#2�)^2�):�7i2��)n2��)s2��)y2�	�)}2�)�2�):|7!�2���q*r7��	�9!�2�q�2�):z7!�2���q*n7V#F#"�5#�XN��n##�R!�2�q*�5��)>2�):�7_� S2�).2�)��(�)�2�):�4����(�)!�^#V#z�ʿ��)�)�ë*7��)�)�x� �!�"i(  �2�)�1:�7��)_� �)�1��)�1�)�-�)�xCP4PKT.ASM (6)  22-Nov-84$�4>�#Þ��Z26�6$�9�07�4�)>12�4�26>R͋Þ�]�9�2�4!  "�4"�42�42�4�67!  ��(>R2�4�37*�4��(:�4�D��~�F �I�~�R¹��:�4�F�~�/̀�~�C��-:�4����2�4�-̀�x�A��'-̀�x:�4���M+�0�I<2�4>12�4̀�R�S�>:�42�4�2�4:6͕:6!�4�Y!�4�26>Y͋�I:�42�4>F2�4��E�O��I:�4��Zk+�0�I<2�4̀�R�S¥:�4��zM+�0�I<2�4:�4=�?G:6��O�e�2�4!�4�26>Y͋�I��Z¿:�4����+�0�Iͳ��F��͌�O͕��I:�42�4ͣ>D2�4:�4�Zȯ2�4��B�	͌�O�26>Y͋�I>C2�4��E�I��I:�4��%�+�0�I<2�4̀�R�D͌�O:�4��K�+�0�Iͳ�͕:�42�4:6���I�2�426O:�4��zO>26y2�4>Y͋�I��F:�4���k+�0�Iͳ��Z��͌�O͕:6�»:�4�D����*���2�4:�42�4ͣ>F2�4��+�0�I�E�I��I>\ �#�x��(��!�,�)�x�9�2�42�42�4!  "�4"�4�67!  ��(>12�4>S2�4�37*�4��(:�4�D�E��.�F�P�B�.�Z�[��.�S�tͮ:�4�F�.�/̀�.�B�ͭ�.�C-:�4�ʑ�-̀�x�A¥'-̀�x'-̀�x:�4����+�0�I<2�4>12�4:�72�4!�4�26:�426>S͋�I̀�!)�Y�͌�͕:6!�4�Y:�42�4�2�4:�42�4>F2�4�")��N�7�e:�4<�?G:6���2�4>F2�4��E�I��I:�4��S�+�0�I<2�4�2�4!�4"�4!] "�4  x��|>.*�4w#"�4x��*�4~�#"�4�!�k*�4w#"�4�ky26*�4>$w�07�4�):�426>F͋�I̀�!)�Y��͌�͕:�42�4�2�4�p��2�4>D2�4����I>Z2�4��N��e:�4<�?G:6�����E�I��I:�4��(�+�0�I<2�4!�4"�4!x5"�4*�4~#"�4O:�4�y�l:�7�y�l:�4��iy��i�s>�2�4y�*�4w#"�4:�4��::�426:�426>D͋�I̀�!)�Y��͌�:6͕:�42�4�2�4:6���:�4�Z��2�4�X��2�4:�4���>Z2�4��p��2�4����I>Z2�4��N��e:�4<�?G:6�����E�I��I:�4���+�0�I<2�4:�426�26:�4��>>D2�4>26>Z͋�I̀�!)�Y͌�͕:�42�4�2�4\ � :�4�Zʅ��!څ�")�2�4>F2�4�>B2�4��N¢�e:�4<�?G:6���U�E�I��I:�4����+�0�I<2�4:�426�26>B͋�I̀�!)�Y��͌�͕:�42�4�2�4>C2�4��N��e:�4<�?G:6�����E�I��I:�7� w#:�7� w#:�7� w#:�7�@�w#:�7� w#:�7w#6Y:�7��P:�4w#:�7w#>�2 6>2�7>#2�7>&2�4>12�4~� 2�7: 6��##~� 2�7: 6��#~�@�2�7: 6��#~� 2�7: 6��#~2�7: 6��#> 2�4~�N��� ���Y��:�7���>�2�4��2�4>�2�4: 6��#~G:�7��2�4�2�5!�4"�4:�7G :�4��:�4O!�55�)!�45��j*�i*�4~#"�4 ��;�:�5=2�5~#"�4��]~#"�4!�55W怳_z���]��]z�@��*�4w#"�4��+�0�:�7O:�4�1G:�7��2�5!x5"�4 :�5=�x�)2�5:�4=��2�4÷�A)ï÷x��)�|*�4~#"�4W�_��:�4���:�5��m=2�5*�4:�4w#"�4 z�W� ���:�4��:�4��z��`!�55*�4q#"�4z�`���I:�7��8��G*�4:�4Wz�@�2�4x�)~#��3>2�5!�55*�4q#"�4:�5�@�*�4�w#"�4Ë*�4+"�4!�44x�)>��!�4"�4!] "�4�2�52�52\  p#<���*�4~�a���_#�.��"�4!e "�4:�52�5>	2�5����"�4*�4w#"�4:�5<2�5���2�5*�4~#"�4���.��*�4~�a��_��#"�4*�4w#"�4:�5<2�5���*�46$�07�4�):�7���\ � ����/-�0:�5��R:�52�5 P<�	�^�=_!\ 6&��!\ ~�w#�l\ � ����ʩx�ʒ{�ʢ�_{�	�_:�5_��_!,�)��5!]  ~#� ��³x���>.�!e ó>$�5�)�&*���)�+�0���(�2�4>12�4:�4��	E,�)�x<2�4�26>26!�46F>G͋� ̀���Y�x�E����x��(�E�x�x�2�4>12�4:�4��]y,�)�<2�4�26>26!�46L>G͋�V̀�N�Y�)�E�N��26!�4>w#:�4�1G:6�#�w#  O:6� w#�O> �G:6w#�O> �G:6���=26~#�O> �Gÿ:�4�2���y����?� w#�6 �!�4͕�KBz�� w#x�Gy��� w#y�?� w#:7��1�6 �?7!�4���:�7w#�w�7:�72�5:�5=��Y2�5:�7̓(_�	7�A!�4~��z̓(_�	7:�7��v̓(_�	7#�\�7�)ͧ�!)� À�� �!)�ʑ2�4O:�4�1Gy�#�26 � �!)�ʑ262�4�O> �G:6� 26� �!)�ʑ2�52�4�O> �G:62�5!�4"�4:�5��2�5� �!)�ʑ*�4w#"�4�O> �G��� �!)�ʑ� 2�5:�4�2�s�Gy����?G:�5�ʜ�e�*�46 !�4͕KBz�W:�5��C� �!)�ʑ� 2�5x�Gy��G:�5��C� �!)�ʑ� Gy�?��C*�46 :�5�)!5"�4�-�!)�­���-�!)���!5"�4*�4w#"�4G��ڧ:�7�»:�7���:�4�S���-�����*�4+6 �7:7��#�<7!5��!5"�4�)*�4~#"�4��)���*v7"�4�7�7��PG:�7�x�K����)�7�7��v��s��n�ʏ��v�@2�4���:�4��5*�4+"�4|��5�e����'7�x��  ~����_�O !�		�{�O!�	~�#V�_#~�W�#Ú���  �!�1B�Rc�s���
���ƍ���  �#�2$F�W6e�tH���Z�Ӿl���~����-7>A2�4�!+�):6O !�4	>$w�4�)���-7��)�+�`7�7�7�7�>A2�4��e:�426�26>N͋�I��67*�4#"�4��(�����/�0������97��)�*7�:�4G:6��<�?2�4*�4#"�4ɯ2�426>Y͋�I�<2�4:�4=�?G:6��O�eͣ�~���_�7�#��CP4TT.ASM  (4)  13-Jan-85$>\ �#�x��(�")���,�)�xM.�)��(%/�)��(Q/�)>2�4�2�4�V� ��3:4��	�O2�3�3*�4:�4G�f�A)Ò*�4:�4G~#��U�ʉ�ʏ�
ʒ:�3��U2�3�U24"�4x2�4y2�4:�42�3!�3"34�V:�4�©:�3=��2�3�7*34~#"34̓(_�	7:�7���̓(_�	7:�7�ʩ{�_�7�7é�2�5�V���:�5��3���m/�+��x\ � �2�4���([-�)��(-�)��(�-�)�H7:�4����V���x�J�7�7��g2�4�7���V��V������_:x7�:�4�ʑ�S�V{�>2�4�V�T7��V:�4�����7:~7��7�7!�44~�e�V6 ɯ2�4�V!�4\  �i7��)�@!�4>��w/0���)0�)!�45�
��[*��:�4��*�4s#"�4�>̓(_�	7�j*�@>̓(_�	7�B-�)��*�@!�4~�w��,�)\ � �2�4��¨{�Y�d>2�4��Zʊ�A���L��A*l7_ ��7�)�2�4�>̓(_�	7>/̓(_�	7>K̓(_�	7Å�¹{�2�4>2�4ɯ2�4{�O:�4G�7�c7��7��)_2�5:y7����7{̓(_�	7�7:�7��){�_�7�)�7���G���_�C�#�-�)�K7:�4��,��S�1�1
�)�)��;�R�F:�4��F�Ýx�?:�4��Y�0�):�4��f�0�)�0�)�N7T1�)��(f1�)��x�0�̓(_�7�	7�7�):y7�°x̓(_�7�	7�7�)x�_�P��:~7�2~7�):�4���x�_�R��>�2�4/0�)�)�Q��>�2�40�)�)x�Q7�)�7�)CP4CPM.ASM (3)  8-Jul-84$\ >�#�) �A :\ � �5 �2\ !] 6?#�: :\ ��K :�4�@2.�)�-�)>2�7͐!��!�j �� � ��{ � �� !] ͼ >.ͯ ͼ �(!!�75�ğ ��� �a ͭ >:ͯ í ͭ > ���_� ����~�ͯ #¼ �����)���6��):�4=_� �x�2�"2�":\ ��� =_� �f7�.�):\ ��!� <�@2$.$.�)���(+.�)�� #\ � :}   o��?! ��:~ 2~ :} 2} �B!o:~ g�	:�7�O	/�o���	�w!ͪ Ã!����	ڃ!ͭ ͭ ���(>kͯ �� ##~2�7#~2�7##^#V�"�7�>\ �#�x\ � <��!,�)�x\ � :.�)�xCP4WLD.ASM (3) 27-Jul-84$����� � �2h :�"��%"2�"!\ �" �i7\ � �B"=2�"!�"\  �i7\ � � �:�"��R"=2�"��="��s"�w":�"2\ :�"<2�"> 2h 2} ����7�o"�����O !� 	\  �i7�             CP4CMD.ASM (5)  6-Feb-85$��"�3!  9"�3�"�3�!�3"34"54�2�32�3>�2�3�)�a(*�3�!�3"54>�2�3*�3�*�3�!�3"34"54�2�32�3>�2�3�)�a(*�3�2�3��'�ʔ#���#��?%��?%��=%��A#�2�)��"�3 �n'��#���q#� �2�3*34+"34"54!�35�G#�?ʃ#��'7x*�3��)!�36 *�3w#"�3�G#�n'����¼#� �2�3*34+"34"54!�35Ô#�?��#�3�)�)�a(*54>$w*34+"34�3�)�2�3��"��'7�)"�3�"�3F#"74*54"94x��*74^#{����$�n'���$��?�I$�2�3!�35*�3��)�)�a(*54>$w*34+"34�3�)��"�¸$�2�3����%�w$� *34+"34"54!�35����	$*34+���~�$ʓ$#�w#�:�3<2�3�~$:�3<2�3�> w#"34"54����) � �����$���%��$"3�)��"�� #^#V{�)�a��$�{��$�_V#��	$ {���$� "74*94"54�$�K{�� F#�*74~�O��%�*94�)�F#�~#�a�8%�{�8%�_���"%> 224�".4 6 #"04�6 #<��O%�n'��e&��?%:24��A'*54#"54:34��%�2�3>?�$'��X&�2�3*34+"34"54!�35{2�5�	��%=*04��%6?#<è%*.4�� ���N&!!6�G'!16�G'*.4�� ����%!16�G'!6!16:�5�	��%= ���%#��%y��)&��&:�5�	�&>.�_'F#��)&� ��%�_'��%y2�5>$�_'*54�	� *34+"34!�35:�5���"� ��"{��0'��0'�)�.&{��0'�
�0'	 *.4	"04	�X%�:£&{��0'*04+~�@"04+w �X%�*��&:24� �A'{��0'�&��&*04>?w#"04{���&�X%�!�0'g:�7�|�'�%�0'�(�0'�)�0'�,�0'�/�0'�:�$'�@�0'�[�$'�^�0'�_�0'�|�0'�a�$'�{�$'�_*04w#"04�X%:�3��!)/3	� �R3�;'�� �<_w#�U'���*34w#"34!�34����:�3���'*54~#"54� ʉ'�	':�3��p'>�2�3> ����'��2�3������'�?ʻ'�ʻ'�
ʻ'���*54+"54�������:�3��](!�34� *34w#"34���'�� (�7�a(!�3"34!�36 ��"��(��1(�$7:�3==�� (� ��'2�3�!7*34++"34��"�?�U(��U(��M(�
�M(���'�'7:�3���">�2�3�](����*�3��)�CP4UTL.ASM (5)  13-Jan-85$��!�7N !�(	�â(ì(ß(ñ(û(��(���(����(����(���(����(���(���:y7� ��(�-�):y7�@_� �>�#��(�$+�)�x>�#Þ�����	��(��|���(�}�:_� !+	� �###�ɯ2�42C6242D62h 2j 2| \ � �:4�����:C6��l)*A6"�4� 	"A6=2C6>2�4����):D6��~)24�2�4����*�7"A6"�4�� \ � 2D6�±)!C64:�7�ʷ)*�4� Ä)��*�I)��*:C6�P)�2h 2j 2| \ � ���8*#\ � *} |��C*+"} *�7�� !\ � ���**�7� >��*#�*"�4"A6{2�4�2C6��*�)\ � �2h 2j 2| \ � ���*�7"�4� "A6{2�4�2C6�)�!C64:�7��|*5���!C64:�7��|*͔*Ò**A6"�4� "A6>2�4��)��*�7"A6"�4*�4�!� "�4� \ � ���*!C65*��*�):�4����**�4=��*6#��*�j*�!):C6���*͔*�!)\ � �)� � Kermit-80 v4.05 $Kermit-80  x:>$
$
?Unrecognized command$
?Not confirmed$?Unable to receive initiate
$?Unable to receive file name
$?Unable to receive end of file
$?Unable to receive data
$?Disk full
$?Unable to receive an acknowledgement from the host
$
?Unable to find file
$?Unable to rename file$
?Disk full$
?Unable to tell host that the session is finished$
?Unable to tell host to logout$
?Kermit has not been configured for a target system$
?Consistency check on configuration failed$
?Error writing to log file
$Completed$Failed$%Renaming file to $
[Closing the log file]$
[Connected to remote host.  Type $C to return;
 type $? for command list]
$
[Connection closed, back at micro]$Control-$ (Not implemented)
$Interrupted$		    Directory for drive x:
$
		Drive $  has $K bytes free
$
File(s) erased$

[Transmitting file to host:
 1. Type any character to send a line.
 2. Type RETURN to terminate the line and to get the next line (go back to 1.)
    (You may send other characters before RETURN.),
   or type $R to send the same line again,
   or type $C to abort transmission.]
$
[Transmission done. Connected normally to remote host,
 type $Sending...$Receiving...$Warning: eighth bit cannot be sent$
For help, type ? at any point in a command$
[Logging suspended]
$
[Logging resumed]
$
Type the new escape character:  $
Type the new TAC intercept character:  $
R Send the same line again$
Q  Suspend logging
R  Resume logging$
?  This message
C  Close the connection
0  (zero) Transmit a NULL
P  Toggle printer on/off
S  Status of the connection$
Typing another $ will send it to the host

Command>$
Transmitting a file$
Local echo$ on$ off$
VT52 emulation$
File Mode$ default$ ASCII$ binary$
IBM flag$
File warning$
Printer copy$
Logging is$ suspended$
Escape char: $
Block check type: $-character$
Parity: $none$mark$space$odd$even$
Port in use is: $
Current baud rate is: $indeterminate (not SET)$Timer$
Current TACTrap Status/Intercept Character:  $
?Program error:  Invalid COMND call$
?Ambiguous$
?Illegal CP/M file specification$
?Wild-cards not allowed in file specification$ Confirm with carriage return$a                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               �  �7Ë8Û8å8ò8Ì8Õ8Í8ú8�19�'9�"9�79�9��8��8��8��8��8�9�9÷7��7��7��7��7��7��7��7��7��7��7��8�8�J9�9          %����      ^
    ##1 @�      CP4SYS.ASM (12)  6-Feb-85$� }2�7P9�@9�9�@9`9�@9�9�@9�=9!:"�7>@2�7� ����$�_�E9{�����9�@9��@9[͕8�9�@9g9�@9:7���9�@9�:�7�0�U8:\ ��18=�68� _.� �!� +~w�D8�>8^#V��� �*�7#  ��k8W+}��|8z�f8��b8�i`:�7��)=8����� �� ���� ���� ��� �ͥ8�²8��� ����9�@9�|�_�͕8�}�_Õ8��8��8��8:�@9��8��8��8��8	��8��8��8��8��8
:�@9Õ8 ͕8Õ8�9�@9�9�@9M9	� �###����
$configured for $ with $]

Number of packets:
Number of retries:
File name:$


RPack:

SPack:$Commodore 128 CP/M 3.0$ADM31A$  
$  $Y$ =$$  
$  $  $    $$   $   $  $  Y$ T$                                                                                                               