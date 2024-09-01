unit UCookiesDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TMSFNCTypes, FMX.TMSFNCUtils, FMX.TMSFNCGraphics, FMX.TMSFNCGraphicsTypes,
  FMX.TMSFNCCustomControl, FMX.TMSFNCWebBrowser, FMX.TMSFNCEdgeWebBrowser,
  System.Rtti, FMX.Grid.Style, FMX.StdCtrls, FMX.Grid, FMX.ScrollBox, FMX.Edit,
  FMX.Controls.Presentation, FMX.DateTimeCtrls;

type
  TForm2 = class(TForm)
    TMSFNCEdgeWebBrowser1: TTMSFNCEdgeWebBrowser;
    NavigateBtn: TButton;
    URIEdit: TEdit;
    CookieGrid: TStringGrid;
    Label3: TLabel;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    NameLbl: TLabel;
    NameEdit: TEdit;
    DomainLbl: TLabel;
    DomainEdit: TEdit;
    PathLbl: TLabel;
    PathEdit: TEdit;
    AddCookieBtn: TButton;
    DeleteCookieBtn: TButton;
    DeleteAllCookiesBtn: TButton;
    GetCookiesBtn: TButton;
    StringColumn5: TStringColumn;
    Label1: TLabel;
    ValueEdit: TEdit;
    Label2: TLabel;
    ExpirationDate: TDateEdit;
    ExpirationTime: TTimeEdit;
    procedure FormCreate(Sender: TObject);
    procedure NavigateBtnClick(Sender: TObject);
    procedure GetCookiesBtnClick(Sender: TObject);
    procedure TMSFNCEdgeWebBrowser1GetCookies(Sender: TObject;
      ACookies: array of TTMSFNCWebBrowserCookie);
    procedure TMSFNCEdgeWebBrowser1NavigateComplete(Sender: TObject;
      var Params: TTMSFNCCustomWebBrowserNavigateCompleteParams);
    procedure AddCookieBtnClick(Sender: TObject);
    procedure DeleteCookieBtnClick(Sender: TObject);
    procedure DeleteAllCookiesBtnClick(Sender: TObject);
    procedure CookieGridSelectCell(Sender: TObject; const ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure TMSFNCEdgeWebBrowser1Initialized(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    CookieList: array of TTMSFNCWebBrowserCookie;
    procedure FillEdits(AIndex: Integer);
  end;

var
  Form2: TForm2;

implementation

uses
  System.DateUtils;

{$R *.fmx}

procedure TForm2.GetCookiesBtnClick(Sender: TObject);
begin
  TMSFNCEdgeWebBrowser1.GetCookies;
end;

procedure TForm2.NavigateBtnClick(Sender: TObject);
begin
  TMSFNCEdgeWebBrowser1.Navigate(URIEdit.text);
end;

procedure TForm2.TMSFNCEdgeWebBrowser1GetCookies(Sender: TObject; ACookies: array of TTMSFNCWebBrowserCookie);
var
  I, l: Integer;
begin
  l := Length(ACookies);
  CookieGrid.RowCount := l;

  SetLength(CookieList, l);
  for I := 0 to l - 1 do
  begin
    CookieList[I] := ACookies[I];
    CookieGrid.Cells[0, I] := ACookies[I].Name;
    CookieGrid.Cells[1, I] := ACookies[I].Domain;
    CookieGrid.Cells[2, I] := ACookies[I].Path;
    CookieGrid.Cells[3, I] := ACookies[I].Value;
    CookieGrid.Cells[4, I] := DateTimeToStr(ACookies[I].Expires);
  end;

  if l > 0 then
  begin
    CookieGrid.SelectRow(0);
    FillEdits(0);
  end;
end;

procedure TForm2.TMSFNCEdgeWebBrowser1Initialized(Sender: TObject);
begin
  if Pos('http', TMSFNCEdgeWebBrowser1.URL) > 0 then
    URIEdit.Text := TMSFNCEdgeWebBrowser1.URL;
end;

procedure TForm2.TMSFNCEdgeWebBrowser1NavigateComplete(Sender: TObject; var Params: TTMSFNCCustomWebBrowserNavigateCompleteParams);
begin
  TMSFNCEdgeWebBrowser1.GetCookies;
end;

procedure TForm2.AddCookieBtnClick(Sender: TObject);
var
  c: TTMSFNCWebBrowserCookie;
begin
  c.Name := NameEdit.Text;
  c.Domain := DomainEdit.Text;
  c.Path := PathEdit.Text;
  c.Expires := ExpirationDate.Date + ExpirationTime.Time;
  c.Value := ValueEdit.Text;
  c.Secure := true;
  c.SameSite := sstLax;

  TMSFNCEdgeWebBrowser1.AddCookie(c);

  TMSFNCEdgeWebBrowser1.GetCookies;
end;

procedure TForm2.CookieGridSelectCell(Sender: TObject; const ACol, ARow: Integer; var CanSelect: Boolean);
begin
  FillEdits(ARow);
end;

procedure TForm2.DeleteAllCookiesBtnClick(Sender: TObject);
begin
  TMSFNCEdgeWebBrowser1.DeleteAllCookies;

  TMSFNCEdgeWebBrowser1.GetCookies;
end;

procedure TForm2.DeleteCookieBtnClick(Sender: TObject);
begin
  TMSFNCEdgeWebBrowser1.DeleteCookie(NameEdit.Text, DomainEdit.Text, PathEdit.Text);

  TThread.Sleep(100);

  TMSFNCEdgeWebBrowser1.GetCookies;
end;

procedure TForm2.FillEdits(AIndex: Integer);
begin
  if (Length(CookieList) > AIndex) and (AIndex >= 0) then
  begin
    NameEdit.Text := CookieList[AIndex].Name;
    DomainEdit.Text := CookieList[AIndex].Domain;
    PathEdit.Text := CookieList[AIndex].Path;
    ValueEdit.Text := CookieList[AIndex].Value;
    ExpirationDate.Date := CookieList[AIndex].Expires;
    ExpirationTime.Time := CookieList[AIndex].Expires.GetTime;
  end
  else
  begin
    NameEdit.Text := '';
    DomainEdit.Text := '';
    PathEdit.Text := '';
    ValueEdit.Text := '';
    ExpirationDate.Date := Now + 1;
    ExpirationTime.Time := Now;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  ExpirationDate.Date := Now + 1;
  ExpirationTime.Time := Now;
end;

end.
