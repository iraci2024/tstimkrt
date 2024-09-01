unit NewNameForm;

interface

uses
{$IFDEF CLR}
  System.ComponentModel,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfmNewName = class(TForm)
    edNewName: TEdit;
    lbCaption: TLabel;
    btOk: TButton;
    btCancel: TButton;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmNewName: TfmNewName;

implementation

{$IFDEF CLR}
{$R *.nfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TfmNewName.FormShow(Sender: TObject);
begin
  edNewName.SetFocus;
end;

end.
