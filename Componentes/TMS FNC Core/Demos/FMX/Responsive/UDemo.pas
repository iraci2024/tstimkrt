unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  System.ImageList, FMX.ImgList, FMX.StdCtrls, FMX.TMSFNCCustomComponent,
  FMX.TMSFNCStateManager, FMX.TMSFNCResponsiveManager,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TForm20 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    TMSFNCResponsiveManager1: TTMSFNCResponsiveManager;
    ImageList1: TImageList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    StyleBook1: TStyleBook;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form20: TForm20;

implementation

{$R *.fmx}

end.
