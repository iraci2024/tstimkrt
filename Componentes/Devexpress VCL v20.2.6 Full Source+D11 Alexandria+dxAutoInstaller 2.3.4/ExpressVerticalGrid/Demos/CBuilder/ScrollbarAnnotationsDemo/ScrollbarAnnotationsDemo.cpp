//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORM("..\Common\DemoBasicMain.cpp", DemoBasicMainForm);
USEFORM("..\Common\DemoBasicAbout.cpp", DemoBasicAboutForm);
USEFORM("..\Common\CarsData.cpp", dmCars);
USEFORM("..\Common\DemoRating.cpp", DemoRatingForm);
USEFORM("CustomAnnotationSettings.cpp", frmCustomAnnotationSettings);
USEFORM("ScrollbarAnnotationsDemoMain.cpp", ScrollbarAnnotationsDemoMainForm);
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->CreateForm(__classid(TdmCars), &dmCars);
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
