call "C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" amd64

cd masmx64
call bld_ml64.bat
cd ..

del deflate.obj inflate.obj inftrees.obj infback.obj inffast.obj trees.obj compress.obj adler32.obj crc32.obj

cl /c /I"masmx64" /Zi /nologo /W3 /WX- /O2 /Ob1 /D "ZLIB_WINAPI" /D "_CRT_NONSTDC_NO_DEPRECATE" /D "_CRT_SECURE_NO_DEPRECATE" /D "_CRT_NONSTDC_NO_WARNINGS" /D "ASMV" /D "ASMINF" /D "WIN64" /U"ASMV" /U"ASMINF" /GF /Gm- /MD /GS- /Gy /fp:precise /Zc:wchar_t /Zc:forScope /Gd /errorReport:queue deflate.c inflate.c inftrees.c infback.c inffast.c trees.c compress.c adler32.c crc32.c