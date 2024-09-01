unit UDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, Vcl.DBCtrls, VCL.TMSFNCCustomComponent,
  VCL.TMSFNCDataBinding, Data.DB, Datasnap.DBClient, Vcl.ExtCtrls, Grids;

type
  THTMLPaintBox = class(TPaintBox)
  private
    FHTML: string;
    procedure SetHTML(const Value: string);
  protected
    procedure Paint; override;
  published
    property HTML: string read FHTML write SetHTML;
  end;

  TStringGridEx = class(TStringGrid, ITMSFNCDataBinderGrid)
  protected
    procedure DataBeginUpdate;
    procedure DataEndUpdate;
    procedure SetDataColumnCount(AValue: Integer);
    procedure SetDataRowCount(AValue: Integer);
    procedure ClearData;
    function GetDataRowCount: Integer;
    procedure SetDataValue(AColumn, ARow: Integer; AValue: string);
    procedure SetDataHeader(AColumn: Integer; AValue: string);
    procedure DataInsertRow(AInsertPosition: Integer);
  end;

  TForm130 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    TMSFNCDataBinder1: TTMSFNCDataBinder;
    DBNavigator1: TDBNavigator;
    ListBox1: TListBox;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form130: TForm130;

implementation

{$R *.dfm}

uses
  VCL.TMSFNCGraphics, VCL.TMSFNCGraphicsTypes;

procedure TForm130.Button1Click(Sender: TObject);
begin
  TMSFNCDataBinder1.Active := not TMSFNCDataBinder1.Active;
  if TMSFNCDataBinder1.Active then
    Button1.Caption := 'Deactivate'
  else
    Button1.Caption := 'Activate';
end;

procedure TForm130.Button2Click(Sender: TObject);
begin
  TMSFNCDataBinder1.StartEditor;
end;

procedure TForm130.FormCreate(Sender: TObject);
var
  s: TStringGridEx;
  p: THTMLPaintBox;
begin
  s := TStringGridEx.Create(Self);
  s.Parent := Self;
  s.Name := 'StringGrid1';
  s.Align := alClient;
  s.Margins.Left := 10;
  s.Margins.Right := 10;
  s.Margins.Bottom := 10;
  s.Margins.Top := Image1.Height + 20;
  s.AlignWithMargins := True;
  s.FixedRows := 1;

  p := THTMLPaintBox.Create(Self);
  p.Parent := Self;
  p.Name := 'HTMLPaintBox1';

  p.Left := Image1.Left + Image1.Width + 20;
  p.Top := Image1.Top;
  p.Width := Width - p.Left - 20;
  p.Height := Image1.Height;

  TMSFNCDataBinder1.ConnectSingleHTMLTemplate(p, DataSource1, 'HTML', '<b>Name: <#COMMON_NAME></b><br/><font size="10"><#NOTES></font>');
  TMSFNCDataBinder1.ConnectSingle(Image1, DataSource1, 'Picture', 'Graphic');
  TMSFNCDataBinder1.ConnectList(ListBox1, DataSource1, 'Items', 'Species Name');
  TMSFNCDataBinder1.ConnectGrid(s, DataSource1);

  TMSFNCDataBinder1.Active := True;
  Button1.Caption := 'Deactivate';
end;

{ THTMLPaintBox }

procedure THTMLPaintBox.Paint;
var
  g: TTMSFNCGraphics;
begin
  inherited;
  g := TTMSFNCGraphics.Create(Canvas);
  try
    g.DrawText(ClientRect, HTML, True, gtaLeading, gtaLeading);
  finally
    g.Free;
  end;
end;

procedure THTMLPaintBox.SetHTML(const Value: string);
begin
  FHTML := Value;
  Invalidate;
end;

{ TStringGridEx }

procedure TStringGridEx.ClearData;
begin
  ColCount := 0;
  RowCount := FixedRows + 1;
end;

procedure TStringGridEx.DataBeginUpdate;
begin

end;

procedure TStringGridEx.DataEndUpdate;
begin

end;

procedure TStringGridEx.DataInsertRow(AInsertPosition: Integer);
begin

end;

function TStringGridEx.GetDataRowCount: Integer;
begin
  Result := RowCount - FixedRows;
end;

procedure TStringGridEx.SetDataColumnCount(AValue: Integer);
begin
  ColCount := AValue;
end;

procedure TStringGridEx.SetDataHeader(AColumn: Integer; AValue: string);
begin
  Cells[AColumn, 0] := AValue;
end;

procedure TStringGridEx.SetDataRowCount(AValue: Integer);
begin
  RowCount := AValue + FixedRows;
end;

procedure TStringGridEx.SetDataValue(AColumn, ARow: Integer; AValue: string);
begin
  Cells[AColumn, ARow + FixedRows] := AValue;
end;

end.
