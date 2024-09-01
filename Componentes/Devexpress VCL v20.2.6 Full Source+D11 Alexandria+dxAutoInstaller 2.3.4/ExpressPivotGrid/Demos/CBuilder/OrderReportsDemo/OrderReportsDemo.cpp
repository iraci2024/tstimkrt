//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("OrderReportsDemo.res");
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\Common\DemoBasicDM.cpp", dmOrders); /* TDataModule: File Type */
USEFORM("..\Common\DemoBasicMain.cpp", frmDemoBasicMain);
USEFORM("OrderReportsMain.cpp", frmOrderReport);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TdmOrders), &dmOrders);
                 Application->CreateForm(__classid(TfrmOrderReport), &frmOrderReport);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
