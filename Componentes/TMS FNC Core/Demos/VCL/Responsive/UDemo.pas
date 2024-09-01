unit UDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.ImageList, Vcl.ImgList,
  VCL.TMSFNCCustomComponent, VCL.TMSFNCStateManager,
  VCL.TMSFNCResponsiveManager, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.BaseImageCollection, Vcl.ImageCollection, Vcl.VirtualImageList;

type
  TForm20 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    TMSFNCResponsiveManager1: TTMSFNCResponsiveManager;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    VirtualImageList1: TVirtualImageList;
    ImageCollection1: TImageCollection;
    VirtualImageList2: TVirtualImageList;
    procedure TMSFNCResponsiveManager1AfterLoadState(Sender: TObject;
      AState: TTMSFNCStateManagerItem);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form20: TForm20;

implementation

{$R *.dfm}

procedure TForm20.TMSFNCResponsiveManager1AfterLoadState(Sender: TObject;
  AState: TTMSFNCStateManagerItem);
begin
  if AState.Index = 0 then
  begin
    Button1.Images := VirtualImageList2;
    Button2.Images := VirtualImageList2;
    Button3.Images := VirtualImageList2;
    Button4.Images := VirtualImageList2;
  end
  else
  begin
    Button1.Images := VirtualImageList1;
    Button2.Images := VirtualImageList1;
    Button3.Images := VirtualImageList1;
    Button4.Images := VirtualImageList1;
  end;
end;

end.
