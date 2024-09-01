call "C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" amd64


del PPMd.obj

rem cl /c /Zi /nologo /W3 /WX- /O2 /Ob1 /D "_CRT_NONSTDC_NO_DEPRECATE" /D "_CRT_SECURE_NO_DEPRECATE" /D "_CRT_NONSTDC_NO_WARNINGS"  /D "WIN64" /GF /Gm- /MD /GS- /Gy /fp:precise /Zc:wchar_t /Zc:forScope /Gd /errorReport:queue  PPMd.cpp

cl /Zi /nologo /Zp1 /GS- /Gy /Gd PPMD.cpp