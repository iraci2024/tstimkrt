unit UDemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, DBCtrls, Grids, LCLTMSFNCDataBinding, db, BufDataset, memds;

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

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    DataSource1: TDataSource;
    DBNavigator1: TDBNavigator;
    ListBox1: TListBox;
    MemDataset1: TMemDataset;
    Panel1: TPanel;
    TMSFNCDataBinder1: TTMSFNCDataBinder;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

uses
  LCLTMSFNCGraphicsTypes, LCLTMSFNCGraphics, Math, DateUtils;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TMSFNCDataBinder1.Active := not TMSFNCDataBinder1.Active;
  if TMSFNCDataBinder1.Active then
    Button1.Caption := 'Deactivate'
  else
    Button1.Caption := 'Activate';
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TMSFNCDataBinder1.StartEditor;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  s: TStringGridEx;
  p: THTMLPaintBox;
const
  FirstNames: array[0 .. 19] of string = ('John', 'Sarah', 'Fred', 'Beth',
    'Eric', 'Tina', 'Thomas', 'Judy', 'Robert', 'Angela', 'Tim', 'Traci',
    'David', 'Paula', 'Bruce', 'Jessica', 'Richard', 'Carla', 'James',
    'Mary');
  LastNames: array[0 .. 11] of string = ('Parker', 'Johnson', 'Jones',
    'Thompson', 'Smith', 'Baker', 'Wallace', 'Harper', 'Parson', 'Edwards',
    'Mandel', 'Stone');

  Employers: array[0 .. 14] of string = ('Microsoft', 'Walmart', 'ExxonMobil', 'Volkswagen', 'Toyota',
    'Apple', 'BP', 'Daimler', 'General Motors', 'Total', 'General Electric', 'Allianz', 'Foxconn', 'Verizon', 'Costco');

var
  idx: Integer;
  sf: TStringField;
begin
  MemDataset1.FieldDefs.Add('ID', ftInteger);
  MemDataset1.FieldDefs.Add('Name', ftString, 255);
  MemDataset1.FieldDefs.Add('Birthday', ftDateTime);
  MemDataset1.FieldDefs.Add('Salary', ftCurrency);
  MemDataset1.FieldDefs.Add('Employed', ftBoolean);
  MemDataset1.FieldDefs.Add('Employer', ftstring, 255);

  for idx := 0 to MemDataset1.FieldDefs.Count - 1 do
    MemDataset1.FieldDefs[idx].CreateField(MemDataset1);

  MemDataset1.Open;
  MemDataset1.DisableControls;
  try
   for idx := 0 to 99 do
   begin
     MemDataset1.Append;
     MemDataset1.FieldByName('ID').AsInteger := idx;
     MemDataset1.FieldByName('Name').AsString := FirstNames[Random(20)] + ' ' + LastNames[Random(12)];
     MemDataset1.FieldByName('Birthday').AsDateTime := Int(IncDay(Now,  RandomRange(-10000, 10000)));
     MemDataset1.FieldByName('Salary').AsFloat := 20000.0 + Random(600) * 100;
     MemDataset1.FieldByName('Employed').AsBoolean := Boolean(Random(2));
     MemDataset1.FieldByName('Employer').AsString := Employers[Random(15)];
     MemDataset1.Post;
   end;
   MemDataset1.First;
  finally
   MemDataset1.EnableControls;
  end;

  MemDataSet1.Active := True;

  s := TStringGridEx.Create(Self);
  s.Parent := Self;
  s.Name := 'StringGrid1';
  s.Align := alClient;
  s.BorderSpacing.Left := 10;
  s.BorderSpacing.Right := 10;
  s.BorderSpacing.Bottom := 10;
  s.BorderSpacing.Top := ListBox1.Height + 20;
  s.FixedRows := 1;

  p := THTMLPaintBox.Create(Self);
  p.Parent := Self;
  p.Name := 'HTMLPaintBox1';

  p.Left := ListBox1.Left + ListBox1.Width + 20;
  p.Top := ListBox1.Top;
  p.Width := Width - p.Left - 20;
  p.Height := ListBox1.Height;

  TMSFNCDataBinder1.ConnectSingleHTMLTemplate(p, DataSource1, 'HTML',
    '<b>Name: <#NAME></b><br/>Employer: <#EMPLOYER><br/>Birthday: <#BIRTHDAY><br/>Salary: <#SALARY>');

  TMSFNCDataBinder1.ConnectList(ListBox1, DataSource1, 'Items', 'Name');
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
    g.DrawText(Bounds(0, 0, Width, Height), HTML, True, gtaLeading, gtaLeading);
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

procedure TStringGridEx.ClearData;
begin
  Clear;;
end;

procedure TStringGridEx.DataBeginUpdate;
begin

end;

procedure TStringGridEx.DataEndUpdate;
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

procedure TStringGridEx.DataInsertRow(AInsertPosition: Integer);
begin

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

