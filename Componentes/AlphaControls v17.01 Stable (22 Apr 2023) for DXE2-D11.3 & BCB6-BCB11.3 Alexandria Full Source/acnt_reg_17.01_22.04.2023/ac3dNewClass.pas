unit ac3dNewClass;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons,
  {$IFDEF DELPHI6UP}Variants, {$ENDIF}
  sBitBtn, sComboBox, sEdit, sSkinProvider, ImgList, acAlphaImageList, sDefaults;


type
  TFormNewThirdClass = class(TForm)
    sEdit1: TsEdit;
    sComboBox1: TsComboBox;
    sBitBtn1: TsBitBtn;
    sBitBtn2: TsBitBtn;
    sSkinProvider1: TsSkinProvider;
    procedure FormCreate(Sender: TObject);
  end;


const
  acThirdCaptions: array [0..ord(acLastSupportedType)] of string = ('Edit', 'Button', 'BitBtn', 'CheckBox', 'ComboBox', 'Grid', 'GroupBox',
    'ListView', 'Panel', 'TreeView', 'wwEdit', 'GridEh', 'VirtualTree', 'PageControl', 'TabControl', 'ToolBar', 'StatusBar', 'SpeedButton',
    'ScrollControl', 'UpDownBtn', {$IFDEF ADDWEBBROWSER}'WebBrowser', {$ENDIF}'ScrollBar', 'StaticText', 'NativePaint');

var
  FormNewThirdClass: TFormNewThirdClass;


implementation

uses acDesignData;

{$R *.dfm}

procedure TFormNewThirdClass.FormCreate(Sender: TObject);
var
  i: integer;
begin
  sEdit1.Text := '';
  sComboBox1.Items.Clear;
  for i := 0 to Length(acThirdCaptions) - 1 do
    sComboBox1.Items.Add(acThirdCaptions[i]);
    
  sComboBox1.ItemIndex := 0;
end;

end.
