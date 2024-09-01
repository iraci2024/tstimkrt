@echo off
cls
SET Flags=-c -O2 -tWM

del deflate.o inflate.o inftrees.o infback.o inffast.o trees.o 
compress.o adler32.o crc32.o

bcc64.exe %Flags% adler32.c
bcc64.exe %Flags% deflate.c
bcc64.exe %Flags% infback.c
bcc64.exe %Flags% inffast.c
bcc64.exe %Flags% inflate.c
bcc64.exe %Flags% inftrees.c
bcc64.exe %Flags% trees.c
bcc64.exe %Flags% crc32.c
bcc64.exe %Flags% compress.c
pause