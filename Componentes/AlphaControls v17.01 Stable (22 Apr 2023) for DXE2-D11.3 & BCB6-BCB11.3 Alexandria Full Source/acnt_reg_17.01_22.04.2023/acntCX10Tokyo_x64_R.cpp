//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("sPopupClndr.pas", Spopupclndr, sPopupCalendar);
USEFORMNS("acMagn.pas", Acmagn, acMagnForm);
USEFORMNS("acPathDialog.pas", Acpathdialog, PathDialogForm);
USEFORMNS("acThumbForm.pas", Acthumbform, MagnifierOwner);
USEFORMNS("sColorDialog.pas", Scolordialog, sColorDialogForm);
USEFORMNS("sCalcUnit.pas", Scalcunit, sCalcForm);
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------


#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
	return 1;
}
//---------------------------------------------------------------------------
