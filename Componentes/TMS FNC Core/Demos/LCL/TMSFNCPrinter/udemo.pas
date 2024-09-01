unit UDemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  PrintersDlgs, LCLTMSFNCPrinters, LCLTMSFNCGraphicsTypes;

type
  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    PrintDialog1: TPrintDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DrawContent;
  private
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if PrintDialog1.Execute then
  begin
    TMSFNCPrinter.BeginDoc;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TMSFNCPrinter.OnDrawContent:=@DrawContent;
end;

procedure TForm1.DrawContent;
begin
  TMSFNCPrinter.Graphics.Font.Color := gcBlue;
  TMSFNCPrinter.Graphics.Font.Size := 40;
  TMSFNCPrinter.Graphics.DrawText(0,20, TMSFNCPrinter.PageWidth, 100, 'Hello', False, gtaCenter, gtaCenter);

  TMSFNCPrinter.Graphics.Fill.Color := gcRed;
  TMSFNCPrinter.Graphics.DrawEllipse(100,200, TMSFNCPrinter.PageWidth - 100, 300);
  TMSFNCPrinter.Graphics.DrawBitmap(50,400, TMSFNCPrinter.PageWidth - 50, TMSFNCPrinter.PageHeight - 50, Image1.Picture, True, True);

  TMSFNCPrinter.EndDoc;
end;

end.

