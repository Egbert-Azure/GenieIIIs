�\
Export CP/M file(s) to the host operating system.

Usage:	EXPORT [-H][-L][-T][-V] cpmfileref [hostfilename]

Where:	cpmfileref is the local file to export
	   (use "?" and "*" to specify multiple files).
	hostfilename is the name of the file to create on the host
	   (converted to uppercase by default), use -L to convert
	   to lowercase and [ to toggle case of next character.
	-H switches emulator to high speed and restores speed when done.
	-L converts hostfilename to lowercase.
	-T export text file (CR LF becomes LF, stop at SUB character).
	-V (verbose) display "r" for block read, "w" for block written.
$Invalid option "x"
$ERROR: Must specify cpmfileref
$Sorry, no files to export
$WARNING: Multiple matching files - ignoring specified hostfilename
$ -> $ERROR: Unable to open CP/M file
$ERROR: Unable to read CP/M file
$ERROR: Unable to close CP/M file
$ERROR: Unable to open HOST file: $ERROR: Unable to write HOST file: $ERROR: Unable to close HOST file: $WARNING: No sub character found in text file
$                                                                                                                                                                                                                                                                                                                                        1Z:� ���!� Ͳ
��͇(d\ �(	Ͳ
(l��	d~���-�#~͟#~���  �Ͳ
��f
�H(�L(�T(�V( 2�z	� ��>2�>��	�>2�>2��	� 	� �  :�Ĭ�x�(x�(�	� �D:�(!  �=�N(:�(�	� !  ~�(�=�\ !�
�N��:�đ��!� \ ͗	ͻ
	� !ͻ
w	� �M�Ͳ�:�(ͭ͗�	
�	͞�	�� !� �f A����=�<�!���  ͎ 1����(�� 7�����͎ �
�2>�� :�������� �	� �=��\ �     ��:�'�'�'�'�'� O!� 	# ���\ � ��֯�]  �����\ � ��[��	� �=����r�	\ � ������� ��8	� �=�y� � �f���\ � �Z	� �=�!A��1�S��}�F���H �[��:w�	�������Fw#x�(��!�����>���!�G����[��4���F�:�(� ��LIBCFN ��#~+�:(�	~�f
�@##�_	�_	��~�(� (#�����~�( � (�.(�*(�f
#�~��� �#�.��> ͒	�#>?͒	�������(�@w#6:#��	�� (6.#��	> w������ (w#����LIBHFN  2�	����~�(	� (#��> ���:�	��r
�
������~�(�[ #~�(�o
#�> �����E
�� (6.#�E
> w���:�	��r
��� (w#����LIBSTR �A��[�� ��a��{�� �� ��~�(�]
w#����~�(�]
w#���~��#�w��#�ͨ
�Ͳ
ɷ�~��� (�#�~��� �#������~�(#�6$�	� �6 ����LIBXTRS Unable to obtain HOST error message
$** normal speed **
$** high speed **
$  �(��0��1��2��3��4��5��<���z�(	� �� ��~ �@�p	6
#6$� 	� ���
	� ��>�C}��(�0_��4>P������:,�(�2,:-�(D���������>�Cy�(�2->2,>!    �C	� �����>�Cy� �2->2,>!   �C	� ��08,�40(����6d#6i#6s#6k#��zs#(�]
w#�6-#�w#6 �ɯ�