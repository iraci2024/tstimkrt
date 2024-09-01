@echo off
cls

SET Flags=-c -6 -O2 -Ve -X- -pr -a8 -b -d -k- -vi -tWM -r -RT- -DBZ_NO_STDIO

del bzlib.obj compress.obj decompress.obj blocksort.obj huffman.obj

bcc32.exe %Flags% bzlib.c         
bcc32.exe %Flags% compress.c      
bcc32.exe %Flags% decompress.c    
bcc32.exe %Flags% blocksort.c     
bcc32.exe %Flags% huffman.c       
