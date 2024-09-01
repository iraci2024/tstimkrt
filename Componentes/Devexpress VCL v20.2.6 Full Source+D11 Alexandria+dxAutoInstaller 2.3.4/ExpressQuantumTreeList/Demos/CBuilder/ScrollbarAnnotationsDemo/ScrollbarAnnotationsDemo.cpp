//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORM("ScrollbarAnnotationsDemoMain.cpp", ScrollbarAnnotationsDemoMainForm);
USEFORM("ScrollbarAnnotationsDemoData.cpp", ScrollbarAnnotationsDemoDataDM);
USEFORM("CustomAnnotationSettings.cpp", frmCustomAnnotationSettings);
USEFORM("..\Common\DemoBasicMain.cpp", DemoBasicMainForm);
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\Common\DemoRating.cpp", DemoRatingForm);
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->CreateForm(__classid(TScrollbarAnnotationsDemoDataDM), &ScrollbarAnnotationsDemoDataDM);
		Application->CreateForm(__classid(TScrollbarAnnotationsDemoMainForm), &ScrollbarAnnotationsDemoMainForm);
		Application->Run();
	}
	catch (Exception &exception)
	{
		Application->ShowException(&exception);
	}
	catch (...)
	{
		try
		{
			throw Exception("");
		}
		catch (Exception &exception)
		{
			Application->ShowException(&exception);
		}
	}
	return 0;
}
//---------------------------------------------------------------------------
