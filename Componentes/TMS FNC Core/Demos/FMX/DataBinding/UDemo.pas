unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Forms,
  FMX.Types, FMX.Controls, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  FMX.Layouts, FMX.ListBox, FMX.TMSFNCCustomComponent, FMX.TMSFNCDataBinding,
  System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid, Data.DB,
  Datasnap.DBClient, Data.Bind.Controls, Data.Bind.Components,
  Data.Bind.DBScope, Fmx.Bind.Navigator;

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
    function DefinePresentationName: string; override;
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
    TMSFNCDataBinder1: TTMSFNCDataBinder;
    ListBox1: TListBox;
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    BindNavigator1: TBindNavigator;
    BindSourceDB1: TBindSourceDB;
    ImageControl1: TImageControl;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form130: TForm130;

implementation

{$R *.fmx}

uses
  FMX.TMSFNCGraphics, FMX.TMSFNCGraphicsTypes, FMX.Graphics;

procedure TForm130.Button1Click(Sender: TObject);
begin
  TMSFNCDataBinder1.Active := not TMSFNCDataBinder1.Active;
  if TMSFNCDataBinder1.Active then
    Button1.Text := 'Deactivate'
  else
    Button1.Text := 'Activate';
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
  s.Align := TAlignLayout.Client;
  s.Margins.Left := 10;
  s.Margins.Right := 10;
  s.Margins.Bottom := 10;
  s.Margins.Top := ImageControl1.Height + 20;

  p := THTMLPaintBox.Create(Self);
  p.Parent := Self;
  p.Name := 'HTMLPaintBox1';

  p.Position.X := ImageControl1.Position.X + ImageControl1.Width + 20;
  p.Position.Y := ImageControl1.Position.Y;
  p.Width := Width - p.Position.X - 20;
  p.Height := ImageControl1.Height;

  TMSFNCDataBinder1.ConnectSingleHTMLTemplate(p, DataSource1, 'HTML', '<b>Name: <#COMMON_NAME></b><br/><font size="10"><#NOTES></font>');
  TMSFNCDataBinder1.ConnectSingle(ImageControl1, DataSource1, 'Bitmap', 'Graphic');
  TMSFNCDataBinder1.ConnectList(ListBox1, DataSource1, 'Items', 'Species Name');
  TMSFNCDataBinder1.ConnectGrid(s, DataSource1);

  TMSFNCDataBinder1.Active := True;
  Button1.Text := 'Deactivate';
end;

{ THTMLPaintBox }

procedure THTMLPaintBox.Paint;
var
  g: TTMSFNCGraphics;
begin
  inherited;
  g := TTMSFNCGraphics.Create(Canvas);
  try
    g.DrawText(LocalRect, HTML, True, gtaLeading, gtaLeading);
  finally
    g.Free;
  end;
end;

procedure THTMLPaintBox.SetHTML(const Value: string);
begin
  FHTML := Value;
  Repaint;
end;

{ TStringGridEx }

function TStringGridEx.DefinePresentationName: string;
begin
  Result := 'Grid-' + GetPresentationSuffix;
end;


procedure TStringGridEx.ClearData;
begin
  ClearColumns;
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
  Result := RowCount;
end;

procedure TStringGridEx.SetDataColumnCount(AValue: Integer);
var
  I: Integer;
  sc: TStringColumn;
begin
  for I := 0 to AValue - 1 do
  begin
    sc := TStringColumn.Create(Self);
    AddObject(sc);
  end;
end;

procedure TStringGridEx.SetDataHeader(AColumn: Integer; AValue: string);
begin
  Columns[AColumn].Header := AValue;
end;

procedure TStringGridEx.SetDataRowCount(AValue: Integer);
begin
  RowCount := AValue;
end;

procedure TStringGridEx.SetDataValue(AColumn, ARow: Integer; AValue: string);
begin
  Cells[AColumn, ARow] := AValue;
end;

end.
