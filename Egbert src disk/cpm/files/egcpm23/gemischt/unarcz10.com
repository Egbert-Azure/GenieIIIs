��Z3ENV   �   �COMCM�EXEOBJOV?REL?RLINTSYSBADLBRARCARK?Q??Z?                UNARCZ   ZCPR3 Archive File Extractor   Version 1.0
Usage:
  {dir:}arcfile{.typ} {dir:}{afn.aft} {{/}N|P|C}
Examples:
  B0>A3:SAVE.ARK *.*  ; List all files in CP/M archive SAVE in A3
  B0>A0:SAVE.ARC *.*  ; List all files in MS-DOS archive SAVE in A0
  A0>SAVE             ; Same as above
  A0>SAVE *.* N       ; Same as above (no screen pauses)
  A0>SAVE *.DOC       ; List just .DOC files
  A0>SAVE READ.ME     ; Typeout the file READ.ME
  A0>SAVE READ.ME /N  ; Typeout the file READ.ME (no screen pauses)
  A0>SAVE C3:         ; Extract all files to directory C3
  A3>SAVE B:*.DOC     ; Extract .DOC files to directory B3
  A0>SAVE 1:READ.ME   ; Extract file READ.ME to directory A1
  A0>SAVE PRN.DAT P   ; Print the file PRN.DAT (no formatting)
  A0>SAVE *.* C       ; Check validity of all files in archive

Copyright (C) 1986, 1987 by Robert A. Freed �		� �sE�v1! ��ͭͲ! E���(	���� F���(!�S�r ���i	�^	 !p���!  �*|� ��(i=�O�x:��  1  ����� ����=�	0�!�  ͌6 �ͩ����د22�2�� � #8: !�2�>� �E�����ͯ�*	) ~2-#~2.!� :� ��*O Ͷ��*�/�*> �� -Ͷ�('> �� !Ͷ�(�/ #~�(�N(�P 2+�C 2,:2�24:� 
 �� 2/:i 20!H ͉>:w#> w:\ �  � <�@2G:y 21!m ͉>:w#> w> !l A��6 � bk6?��!e � 6A#6R#6K2*!] ��*��a	Wʟ�!L � �w=22:� :0!/�( ß<W:0�8 ß!\ :� ڟ��{͙ !*�s(�6 !g 6C�28��Lͬ:��k :� � ###~<25��k�=(:�!����!d����(!���!���:�!�y��Tå�*-~� <ɯ�\ �{> ͋A̈́> �� <�̈́A� <�� � ��� ��� ��(��â����!p~5� #�+w�7���*2,��"2~����� ��͞�{\ �� ���ß#|���x� �{ja��j8��!2�w<�0z���w�\ $�{� *} �8�"} !��!| 4�b�� �����!p���c!B�!6�(�!8� >$� �. x�8�> w�~ �#�?(��#� ��:G!,�k= Gw2+:O:<W:1�0:A� �(\qͬ>�2A,=�ҟ_��A2lYͬ� ѻ�� ��_� ! |͘�} �90y�O��q%w$, �ɱ�^	�!>�(�?(�(��#��=2A�!6 >?���:A��G:b�
�ҟo& �~͘!~wO#:�w $:��k(�8�0w:+� G!�-N���J9(6 ͞͓(���ͣ(�_ͯ{� �Y�͓͓�ʟ2�:b�0M���(��8�	�����0��:A<ȷ�*x�Rv��!z��x���z��̓!�6 ��ß ^�͙
��O���(z�W��� ����ٯo��? 
����8�7�&0$$~$F /����� ����
! uT]���!�6d�0��O!8�>U(=85!c6(	6l�_> ��8��/>2�!  "��!�>	(�!	�!��"�"��S)2�x�2
͙
�C�ů���͂�< �����  �ڕ
�͸ #~���~�� `i͸~�8
rW+~s_#��#�~�+��͂�#~������%($~rW+~s_#��� ��*��d�#"����  �͸��+s#r#�w�}-� |%�(
�(	� ��4�T])�!�5#~#F!��?(M����<�8�2�|=��!�N6	+w+~�(G�w���G������! �,͜
!"���!��������������X 7�lgx�G ��  ob	���,�L}�Z8�90��)��))))lo& 	T])))))))))))|�g�͸�~�� H����)	^#Vz� ��eˤ�͸�~�(�#��r+s������_!��8+~��g�������2���g�j�j�j�j���  *}. �ckC�hhhHxG� >�y�	�����(�O�o|&�$fo�:���*}�o��R(�DM*z�B"z0*|+"|��B����:��(Nbk	6#}� ��(
��e�e����!� �͞͓�=��ß����:0����:1_ � �����:,��:+� C��:�<��!=8͆�x� ���	(�8��0�ͯ�ͣ�!)4(�:� ��k(�2ß�^��� ͣ��#x� ��*|�#"��p�!#��z�!��!�B � ���y�3�e��͑:t� ������H�3!��[�m��y�[!�^���#͑���['�X96 &� !���_=(� ��T� (�͆�ͯ��>͆>
͆!45�> ��w����ͣ(�������� �<24�ͯ�����N����ͯ���= > ͆� ��ͯ�(�� (>=͆�x�>.(�(w#���*z:|�� ���?lg:5=_ /�_T*!"!��m6k#����!�:b� �8	(	�(		�	8	(	�����u���y�����`i����)�j��J�)�j�)�j�)�j��J�)�j�)�j������x���(>e=�R��B�0����s6%#�:t��s6 #�*t�)))|��8�O !�			���6 #��?�P0q�*v|�a(
�8p� >�)))��͌6:#��?��w#����[x�*'"'��\Sr�o�cz��'�`�w#�   	���^#V#~#f�_ ���W_
7����0�w#�0����|��� �ٱ(�w#��~#� (�+�z�(���� |�8�e. )�8�,��^#V#N#F���w#�� q#���a��{����Z80 required!$aborted! CP/M version 2 or higher required Not enough memory Ambiguous archive file name Cannot find archive file Invalid archive file format Warning: Bad archive file header, bytes skipped = 00000 No matching file(s) in archive Invalid archive file directory Invalid output directory Archive File = A00: FILENAME.ARC Output Directory = A00: Checking archive... Cannot extract file (need newer version of UNARC?) Replace existing output file (y/n)?  Disk full Directory full Cannot close output file Incompatible crunched file format Typeout line limit exceeded Warning: Extracted file has incorrect  CRC length [more]        	 ???JanFebMarAprMayJunJulAugSepOctNovDecUnpacked Packed SqueezedCrunchedSquashedUnknown!Name========  =Length  Disk  =Method= Ver =Stored Saved ===Date== =Time=  CRC=         ====  =======  ====               =======  ===                    ====
Total                                                                                                                  