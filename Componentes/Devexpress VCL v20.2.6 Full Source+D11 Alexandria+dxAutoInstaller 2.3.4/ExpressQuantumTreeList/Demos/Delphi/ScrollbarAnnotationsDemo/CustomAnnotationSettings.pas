unit CustomAnnotationSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, Menus, dxLayoutControlAdapters, dxLayoutContainer,
  StdCtrls, cxButtons, cxClasses, dxLayoutControl, dxLayoutcxEditAdapters,
  cxContainer, cxEdit, dxCore, cxTextEdit, cxMaskEdit, cxDropDownEdit,
  dxColorEdit, cxSpinEdit, dxColorDialog, cxColorComboBox, dxCoreGraphics,
  cxTrackBar, cxButtonEdit, ExtCtrls, dxGdiPlusClasses, dxLayoutLookAndFeels, dxScrollbarAnnotations;

type
  TfrmCustomAnnotationSettings = class(TForm)
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1: TdxLayoutControl;
    cxButton2: TcxButton;
    dxLayoutItem2: TdxLayoutItem;
    dxColorDialog1: TdxColorDialog;
    seMinHeight: TcxSpinEdit;
    seMaxHeight: TcxSpinEdit;
    seWidth: TcxSpinEdit;
    seOffset: TcxSpinEdit;
    cbAlignment: TcxComboBox;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    pbColor: TPaintBox;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    cbAnnotationKind: TcxComboBox;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    cxEditStyleController1: TcxEditStyleController;
    procedure cxButton3Click(Sender: TObject);
    procedure pbColorPaint(Sender: TObject);
    procedure cbAnnotationKindPropertiesChange(Sender: TObject);
    procedure cbAlignmentPropertiesChange(Sender: TObject);
    procedure cxButton2Click(Sender: TObject);
    procedure seMinHeightPropertiesEditValueChanged(Sender: TObject);
    procedure seMaxHeightPropertiesEditValueChanged(Sender: TObject);
    procedure seWidthPropertiesEditValueChanged(Sender: TObject);
    procedure seOffsetPropertiesEditValueChanged(Sender: TObject);
  private
    FCustomAnnotations: TdxCustomScrollbarAnnotations;
    procedure AnnotationChanged;
    function GetCurrentAnnotation: TdxCustomScrollbarAnnotation;
    property CurrentAnnotation: TdxCustomScrollbarAnnotation read GetCurrentAnnotation;
  public
    procedure Initialize(AAnnotations: TdxCustomScrollbarAnnotations);
  end;

var
  frmCustomAnnotationSettings: TfrmCustomAnnotationSettings;
  SCustomAnnotationStrs: array [0..2] of string;

implementation

{$R *.dfm}

const
  SAlignment: array [TdxScrollbarAnnotationAlignment] of string = ('saaNear', 'saaCenter', 'saaFar', 'saaClient');

function TfrmCustomAnnotationSettings.GetCurrentAnnotation: TdxCustomScrollbarAnnotation;
var
  AAnnotationIndex: Integer;
begin
  AAnnotationIndex := cbAnnotationKind.ItemIndex;
  Result := FCustomAnnotations[AAnnotationIndex];
end;

procedure TfrmCustomAnnotationSettings.Initialize(AAnnotations: TdxCustomScrollbarAnnotations);
var
  I: Integer;
  AAlignment: TdxScrollbarAnnotationAlignment;
begin
  FCustomAnnotations := AAnnotations;
  AutoSize := True;
  for I := 0 to FCustomAnnotations.Count - 1 do
    cbAnnotationKind.Properties.Items.Add(SCustomAnnotationStrs[I]);
  for AAlignment := Low(TdxScrollbarAnnotationAlignment) to High(TdxScrollbarAnnotationAlignment) do
    cbAlignment.Properties.Items.Add(SAlignment[AAlignment]);
  cbAnnotationKind.ItemIndex := 0;
end;

procedure TfrmCustomAnnotationSettings.AnnotationChanged;
var
  AAnnotation: TdxCustomScrollbarAnnotation;
begin
  AAnnotation := CurrentAnnotation;
  cbAlignment.ItemIndex := Ord(AAnnotation.Alignment);
  seMinHeight.EditValue := AAnnotation.MinHeight;
  seMaxHeight.EditValue := AAnnotation.MaxHeight;
  seWidth.EditValue := AAnnotation.Width;
  seOffset.EditValue := AAnnotation.Offset;
  pbColor.Refresh;
end;

procedure TfrmCustomAnnotationSettings.cbAlignmentPropertiesChange(
  Sender: TObject);
begin
  CurrentAnnotation.Alignment := TdxScrollbarAnnotationAlignment(cbAlignment.ItemIndex);
end;

procedure TfrmCustomAnnotationSettings.cbAnnotationKindPropertiesChange(
  Sender: TObject);
begin
  AnnotationChanged;
end;

procedure TfrmCustomAnnotationSettings.cxButton2Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmCustomAnnotationSettings.cxButton3Click(Sender: TObject);
begin
  dxColorDialog1.Color := CurrentAnnotation.Color;
  if dxColorDialog1.Execute then
  begin
    CurrentAnnotation.Color := dxColorDialog1.Color;
    pbColor.Refresh;
  end;
end;

procedure TfrmCustomAnnotationSettings.pbColorPaint(Sender: TObject);
begin
   cxDrawTransparencyCheckerboard(pbColor.Canvas.Handle, pbColor.ClientRect, 7);
   dxGPPaintCanvas.BeginPaint(pbColor.Canvas.Handle, pbColor.ClientRect);
   try
     dxGPPaintCanvas.FillRectangle(pbColor.ClientRect, CurrentAnnotation.Color);
   finally
     dxGPPaintCanvas.EndPaint;
   end;
end;

procedure TfrmCustomAnnotationSettings.seMaxHeightPropertiesEditValueChanged(
  Sender: TObject);
begin
  CurrentAnnotation.MaxHeight := seMaxHeight.EditValue;
end;

procedure TfrmCustomAnnotationSettings.seMinHeightPropertiesEditValueChanged(
  Sender: TObject);
begin
  CurrentAnnotation.MinHeight := seMinHeight.EditValue;
end;

procedure TfrmCustomAnnotationSettings.seOffsetPropertiesEditValueChanged(
  Sender: TObject);
begin
  CurrentAnnotation.Offset := seOffset.EditValue;
end;

procedure TfrmCustomAnnotationSettings.seWidthPropertiesEditValueChanged(
  Sender: TObject);
begin
  CurrentAnnotation.Width := seWidth.EditValue;
end;

end.
