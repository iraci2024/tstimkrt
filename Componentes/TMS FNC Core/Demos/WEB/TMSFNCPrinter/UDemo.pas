unit UDemo;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, WEBLib.TMSFNCPrinters, WEBLib.TMSFNCGraphics, WEBLib.TMSFNCGraphicsTypes,
  Vcl.Graphics, Vcl.Controls, WEBLib.ExtCtrls, Vcl.StdCtrls, WEBLib.StdCtrls, WEBLIB.TMSFNCTypes,
  VCL.TMSFNCTypes, VCL.TMSFNCUtils, VCL.TMSFNCGraphics, VCL.TMSFNCGraphicsTypes;

type
  TForm1 = class(TWebForm)
    WebButton1: TWebButton;
    WebImageControl1: TWebImageControl;
    procedure WebButton1Click(Sender: TObject);
    procedure WebFormCreate(Sender: TObject);
  private
    { Private declarations }
    bmp : TTMSFNCBitmap;
    procedure DoBitmapChange(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }
procedure TForm1.DoBitmapChange(Sender: TObject);
begin
  WebButton1.Enabled := True;
end;

procedure TForm1.WebButton1Click(Sender: TObject);
begin
  TMSFNCPrinter.BeginDoc;
end;

procedure TForm1.WebFormCreate(Sender: TObject);
begin
  WebButton1.Enabled := False;
  bmp := TTMSFNCBitmap.Create;
  bmp.OnChange := DoBitmapChange;
  bmp.LoadFromFile(WebImageControl1.Picture.FileName);

  TMSFNCPrinter.OnDrawContent :=
  procedure
  begin
    TMSFNCPrinter.Graphics.Font.Color := gcBlue;
    TMSFNCPrinter.Graphics.Font.Size := 40;
    TMSFNCPrinter.Graphics.DrawText(0,20, TMSFNCPrinter.PageWidth, 100, 'Hello', False, gtaCenter, gtaCenter);

    TMSFNCPrinter.Graphics.Fill.Color := gcRed;
    TMSFNCPrinter.Graphics.DrawEllipse(100,200, TMSFNCPrinter.PageWidth - 100, 300);

    TMSFNCPrinter.Graphics.DrawBitmap(50,400, TMSFNCPrinter.PageWidth - 50, TMSFNCPrinter.PageHeight - 50, bmp, True, True);
    TMSFNCPrinter.EndDoc;
    bmp.Free;
  end;
end;

end.