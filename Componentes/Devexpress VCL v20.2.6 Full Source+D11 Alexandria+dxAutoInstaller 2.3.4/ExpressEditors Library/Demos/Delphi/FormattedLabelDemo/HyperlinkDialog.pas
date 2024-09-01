unit HyperlinkDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer,
  cxEdit, Menus, StdCtrls, cxButtons, cxTextEdit, cxMaskEdit, cxSpinEdit, cxButtonEdit, cxLabel, dxForms,
  dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutContainer, cxClasses, dxLayoutControl;

type
  TfmHyperlinkDialog = class(TdxForm)
    btnOk: TcxButton;
    btnCancel: TcxButton;
    edtTextToDisplay: TcxTextEdit;
    edtAddress: TcxButtonEdit;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
  end;

implementation

{$R *.dfm}

end.
