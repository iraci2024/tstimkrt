@echo off
cls

echo Copy obj-files from Delphi LIB dir:

set LibsDir="C:\Program Files (x86)\Embarcadero\Studio\22.0\lib\win32\release"
set obj=deflate.obj inflate.obj inftrees.obj infback.obj inffast.obj trees.obj compress.obj adler32.obj crc32.obj
del %obj%
for %%I in (%obj%) do copy %LibsDir%\%%I .
