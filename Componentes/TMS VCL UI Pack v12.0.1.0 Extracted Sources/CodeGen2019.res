        ��  ��                  �  4   ��
 C O D E G E N 2 0 1 9       0         unit %0:s;

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, AdvToolBar, AdvToolBarStylers, AdvPreviewMenu,
  AdvPreviewMenuStylers, AdvShapeButton, AdvOfficeStatusBar,
  AdvOfficeStatusBarStylers, %4:s;

type
  T%1:s = class(%2:s)
    AdvToolBarPager1: TAdvToolBarPager;
    AdvPage1: TAdvPage;
    AdvToolBarOfficeStyler1: TAdvToolBarOfficeStyler;
    AdvToolBar1: TAdvToolBar;
    AdvPreviewMenuOfficeStyler1: TAdvPreviewMenuOfficeStyler;
    AdvOfficeStatusBar1: TAdvOfficeStatusBar;
    AdvOfficeStatusBarOfficeStyler1: TAdvOfficeStatusBarOfficeStyler;
    AdvQuickAccessToolBar1: TAdvQuickAccessToolBar;
    AdvShapeButton1: TAdvShapeButton;
    AdvFormStyler1: TAdvFormStyler;
    %3:s: T%3:s;
    procedure AdvFormStyler1ApplyStyle(Sender: TObject; AComponent: TComponent;
      var Allow: Boolean);
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  end;

var
  %1:s: T%1:s;

implementation

{$R *.dfm}

procedure T%1:s.AdvFormStyler1ApplyStyle(Sender: TObject;
  AComponent: TComponent; var Allow: Boolean);
begin
  %3:s.AppMenu1.SetAppStyle(AdvFormStyler1.Style,AdvFormStyler1.AppColor);
end;

end.
  