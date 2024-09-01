unit UDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCL.TMSFNCPrinters, VCL.TMSFNCGraphicsTypes,
  Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.StdCtrls, VCL.TMSFNCTypes;

type
  TForm3 = class(TForm)
    Button1: TButton;
    PrintDialog1: TPrintDialog;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
begin
  if (PrintDialog1.Execute) then
    TMSFNCPrinter.BeginDoc;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  TMSFNCPrinter.OnDrawContent :=
  procedure
  begin
    TMSFNCPrinter.Graphics.Font.Color := gcBlue;
    TMSFNCPrinter.Graphics.Font.Size := 40;
    TMSFNCPrinter.Graphics.DrawText(0,20, TMSFNCPrinter.PageWidth, 100, 'Hello', False, gtaCenter, gtaCenter);

    TMSFNCPrinter.Graphics.Fill.Color := gcRed;
    TMSFNCPrinter.Graphics.DrawEllipse(100,200, TMSFNCPrinter.PageWidth - 100, 300);
    TMSFNCPrinter.Graphics.DrawBitmap(50,400, TMSFNCPrinter.PageWidth - 50, TMSFNCPrinter.PageHeight - 50, Image1.Picture, True, True);
    TMSFNCPrinter.EndDoc;
  end;
end;

end.
