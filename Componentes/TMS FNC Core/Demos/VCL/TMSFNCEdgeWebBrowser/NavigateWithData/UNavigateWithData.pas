unit UNavigateWithData;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCL.TMSFNCTypes, VCL.TMSFNCUtils,
  VCL.TMSFNCGraphics, VCL.TMSFNCGraphicsTypes, Vcl.StdCtrls, Vcl.Grids,
  VCL.TMSFNCCustomControl, VCL.TMSFNCWebBrowser, VCL.TMSFNCEdgeWebBrowser;

type
  TForm2 = class(TForm)
    TMSFNCEdgeWebBrowser1: TTMSFNCEdgeWebBrowser;
    URIEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    HeaderGrid: TStringGrid;
    Label3: TLabel;
    BodyMemo: TMemo;
    NavigateBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure NavigateBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  HeaderGrid.Cells[0, 0] := 'Name';
  HeaderGrid.Cells[1, 0] := 'Value';
  HeaderGrid.Cells[0, 1] := 'Content-Type';
  HeaderGrid.Cells[1, 1] := 'application/x-www-form-urlencoded';
end;

procedure TForm2.NavigateBtnClick(Sender: TObject);
var
  headers: TStringList;
  I: Integer;
begin
  headers := TStringList.Create;
  try
    for I := 1 to HeaderGrid.RowCount - 1 do
    begin
      if Trim(HeaderGrid.Cells[0, I]) <> '' then
        headers.AddPair(Trim(HeaderGrid.Cells[0, I]), Trim(HeaderGrid.Cells[1, I]));
    end;
    TMSFNCEdgeWebBrowser1.NavigateWithData(URIEdit.Text, 'POST', BodyMemo.Text, headers);
  finally
    headers.Free;
  end;
end;

end.
