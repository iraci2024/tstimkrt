//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("vclFlexCompressb4.res");
USEPACKAGE("vcl40.bpi");
USEUNIT("FXCZlib.pas");
USEUNIT("FXCBZip2.pas");
USEUNIT("FXCCipher.pas");
USEUNIT("FXCConst.pas");
USEUNIT("FXCDecConst.pas");
USEUNIT("FXCDecUtil.pas");
USEUNIT("FXCExcept.pas");
USEUNIT("FXCFolderDialog.pas");
USEUNIT("FXCStrFunc.pas");
USEUNIT("FlexCompress.pas");
USERES("FlexCompress.dcr");
USEPACKAGE("vclx40.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
