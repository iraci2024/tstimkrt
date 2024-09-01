unit UNavigatePost;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TMSFNCTypes, FMX.TMSFNCUtils, FMX.TMSFNCGraphics, FMX.TMSFNCGraphicsTypes,
  FMX.TMSFNCCustomControl, FMX.TMSFNCWebBrowser, FMX.TMSFNCEdgeWebBrowser,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.Edit,
  FMX.ScrollBox, FMX.Memo, System.Rtti, FMX.Grid.Style, FMX.Grid;

type
  TForm2 = class(TForm)
    TMSFNCEdgeWebBrowser1: TTMSFNCEdgeWebBrowser;
    BodyMemo: TMemo;
    NavigateBtn: TButton;
    URIEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    HeaderGrid: TStringGrid;
    Label3: TLabel;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
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

{$R *.fmx}

procedure TForm2.NavigateBtnClick(Sender: TObject);
var
  headers: TStringList;
  I: Integer;
begin
  headers := TStringList.Create;
  try
    for I := 0 to HeaderGrid.RowCount - 1 do
    begin
      if Trim(HeaderGrid.Cells[0, I]) <> '' then
        headers.AddPair(Trim(HeaderGrid.Cells[0, I]), Trim(HeaderGrid.Cells[1, I]));
    end;
    TMSFNCEdgeWebBrowser1.NavigateWithData(URIEdit.Text, 'POST', BodyMemo.Text, headers);
  finally
    headers.Free;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  HeaderGrid.Cells[0, 0] := 'Content-Type';
  HeaderGrid.Cells[1, 0] := 'application/x-www-form-urlencoded';

  ReportMemoryLeaksOnShutdown := True;
end;

end.
