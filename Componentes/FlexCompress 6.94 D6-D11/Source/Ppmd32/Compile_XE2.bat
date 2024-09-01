@echo off

SET Flags=-c -6 -O2 -V?- -X- -pr -a8 -b -d -k- -vi -tWM -r -RT- -DFOR_DELPHI

del ppmd.obj

bcc32.exe %Flags% ppmd.cpp
