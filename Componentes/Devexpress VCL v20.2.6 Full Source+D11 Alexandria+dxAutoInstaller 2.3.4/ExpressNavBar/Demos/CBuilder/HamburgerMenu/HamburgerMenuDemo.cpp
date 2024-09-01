//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("HamburgerMenuDemo.res");
USEFORM("HamburgerMenuDemoMain.cpp", frmHamburgerMenuDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
				 Application->CreateForm(__classid(TfrmHamburgerMenuDemo), &frmHamburgerMenuDemo);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
