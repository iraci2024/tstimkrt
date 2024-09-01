unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, pngimage, ExtCtrls, LayeredForm, jpeg;

type
  TForm1 = class(TForm)
    LayeredForm1: TLayeredForm;
    Image1: TImage;
    procedure Image1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Image1Click(Sender: TObject);
begin
  if MessageDlg('Close application', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    Close;

end;

end.
