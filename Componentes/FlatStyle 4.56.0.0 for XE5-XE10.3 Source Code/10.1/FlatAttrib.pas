unit FlatAttrib;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, FlatUtils, FlatCtrls;

type
  TMaskForm = class(TForm)
    EditText: TFlatMaskEdit;
    EditMask: TFlatEdit;
    OKButton: TFlatButton;
    CancelButton: TFlatButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MaskForm: TMaskForm;

implementation

{$R *.dfm}

end.
