call "C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" amd64

del blocksort.obj bzlib.obj compress.obj decompress.obj huffman.obj

rem cl /nologo -W3 -O2 -Ob2 -Oy- /GS- /Gy /Gd /MT -Zi /c /DBZ_NO_STDIO -D_FILE_OFFSET_BITS=64 blocksort.c bzlib.c compress.c decompress.c huffman.c
rem cl /c /Zi /nologo /W3 /WX- /O2 /Gm- /MD /GS- /Gy /fp:precise /Zc:wchar_t /Zc:forScope /Gz /TC -DZ_FILE_OFFSET_BITS=64 /DBZ_NO_STDIO blocksort.c bzlib.c compress.c decompress.c huffman.c

cl /c /Zi /nologo /W3 /WX- /O2 /Ob2 /Oy- /D "WIN64" /D "NDEBUG" /D "_WINDOWS" /D "BZ_NO_STDIO" /D "_VC80_UPGRADE=0x0600" /D "_WINDLL" /GF /Gm- /EHsc /MT /Zp1 /GS- /Gy /fp:precise /Zc:wchar_t /Zc:forScope /Gd /errorReport:queue blocksort.c bzlib.c compress.c decompress.c huffman.c


