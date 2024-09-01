unit UEdgeBrowserCookies;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCL.TMSFNCTypes, VCL.TMSFNCUtils,
  VCL.TMSFNCGraphics, VCL.TMSFNCGraphicsTypes, VCL.TMSFNCCustomControl,
  VCL.TMSFNCWebBrowser, VCL.TMSFNCEdgeWebBrowser, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.Grids;

type
  TForm2 = class(TForm)
    URLEdit: TEdit;
    NameEdit: TEdit;
    NavigateBtn: TButton;
    CookieGrid: TStringGrid;
    Label1: TLabel;
    AddCookieBtn: TButton;
    DeleteCookieBtn: TButton;
    DeleteAllCookiesBtn: TButton;
    ExpiresDate: TDateTimePicker;
    DomainEdit: TEdit;
    PathEdit: TEdit;
    ValueEdit: TEdit;
    TMSFNCEdgeWebBrowser1: TTMSFNCEdgeWebBrowser;
    GetCookiesBtn: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure NavigateBtnClick(Sender: TObject);
    procedure GetCookiesBtnClick(Sender: TObject);
    procedure AddCookieBtnClick(Sender: TObject);
    procedure DeleteCookieBtnClick(Sender: TObject);
    procedure DeleteAllCookiesBtnClick(Sender: TObject);
    procedure CookieGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure TMSFNCEdgeWebBrowser1GetCookies(Sender: TObject;
      ACookies: array of TTMSFNCWebBrowserCookie);
    procedure TMSFNCEdgeWebBrowser1NavigateComplete(Sender: TObject;
      var Params: TTMSFNCCustomWebBrowserNavigateCompleteParams);
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

{$R *.dfm}

procedure TForm2.AddCookieBtnClick(Sender: TObject);
var
  c: TTMSFNCWebBrowserCookie;
begin
  c.Name := NameEdit.Text;
  c.Domain := DomainEdit.Text;
  c.Path := PathEdit.Text;
  c.Expires := ExpiresDate.DateTime;
  c.Value := ValueEdit.Text;
  c.Secure := true;
  c.SameSite := sstLax;

  TMSFNCEdgeWebBrowser1.AddCookie(c);

  TMSFNCEdgeWebBrowser1.GetCookies;
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
    ExpiresDate.DateTime := CookieList[AIndex].Expires;
  end
  else
  begin
    NameEdit.Text := '';
    DomainEdit.Text := '';
    PathEdit.Text := '';
    ValueEdit.Text := '';
    ExpiresDate.DateTime := Now + 1;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  ExpiresDate.DateTime := Now + 1;
  CookieGrid.Cells[0, 0] := 'Name';
  CookieGrid.Cells[1, 0] := 'Domain';
  CookieGrid.Cells[2, 0] := 'Path';
  CookieGrid.Cells[3, 0] := 'Value';
  CookieGrid.Cells[4, 0] := 'Expires';
end;

procedure TForm2.GetCookiesBtnClick(Sender: TObject);
begin
  TMSFNCEdgeWebBrowser1.GetCookies;
end;

procedure TForm2.NavigateBtnClick(Sender: TObject);
begin
  TMSFNCEdgeWebBrowser1.Navigate(URLEdit.Text);
end;

procedure TForm2.TMSFNCEdgeWebBrowser1GetCookies(Sender: TObject; ACookies: array of TTMSFNCWebBrowserCookie);
var
  I, l: Integer;
begin
  l := Length(ACookies);
  CookieGrid.RowCount := l + 1;

  SetLength(CookieList, l);
  for I := 0 to l - 1 do
  begin
    CookieList[I] := ACookies[I];
    CookieGrid.Cells[0, I + 1] := ACookies[I].Name;
    CookieGrid.Cells[1, I + 1] := ACookies[I].Domain;
    CookieGrid.Cells[2, I + 1] := ACookies[I].Path;
    CookieGrid.Cells[3, I + 1] := ACookies[I].Value;
    CookieGrid.Cells[4, I + 1] := DateTimeToStr(ACookies[I].Expires);
  end;

  if l > 0 then
  begin
    CookieGrid.Row := 1;
    FillEdits(0);
  end;
end;

procedure TForm2.TMSFNCEdgeWebBrowser1Initialized(Sender: TObject);
begin
  if Pos('http', TMSFNCEdgeWebBrowser1.URL) > 0 then
    URLEdit.Text := TMSFNCEdgeWebBrowser1.URL;
end;

procedure TForm2.TMSFNCEdgeWebBrowser1NavigateComplete(Sender: TObject; var Params: TTMSFNCCustomWebBrowserNavigateCompleteParams);
begin
  TMSFNCEdgeWebBrowser1.GetCookies;
end;

procedure TForm2.CookieGridSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  FillEdits(ARow - 1);
end;

end.
