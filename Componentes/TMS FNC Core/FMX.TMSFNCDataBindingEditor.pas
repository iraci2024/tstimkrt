{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2021                                      }
{            Email : info@tmssoftware.com                            }
{            Web : https://www.tmssoftware.com                       }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit FMX.TMSFNCDataBindingEditor;

{$I FMX.TMSFNCDefines.inc}

{$IFDEF FMXLIB}
{$WARNINGS OFF}
{$HINTS OFF}
{$IF COMPILERVERSION > 30}
{$DEFINE DELPHIBERLIN}
{$IFEND}
{$HINTS ON}
{$WARNINGS ON}
{$ENDIF}

{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$DEFINE LCLWEBLIB}
{$DEFINE VCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF VCLLIB}
{$DEFINE VCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, FMX.TMSFNCCustomComponent, FMX.TMSFNCDataBinding, FMX.TMSFNCCustomControl, FMX.Controls,
  FMX.TMSFNCTypes, FMX.StdCtrls, FMX.Graphics, FMX.ExtCtrls, FMX.Forms, FMX.TMSFNCGraphics,
  FMX.TMSFNCGraphicsTypes, FMX.TMSFNCEditorButton, FMX.TMSFNCEditorPanel, FMX.TMSFNCEditorListView
  {$IFDEF FMXLIB}
  ,FMX.Types, FMX.Memo, FMX.ListBox, FMX.ComboEdit, FMX.Layouts
  {$ENDIF}
  {$IFNDEF LCLLIB}
  {$IFNDEF WEBLIB}
  ,UITypes
  {$ENDIF}
  ,Types
  {$ENDIF}
  ;

resourcestring
  sTMSFNCDataBindingEditorOK = 'OK';
  sTMSFNCDataBindingEditorCancel = 'Cancel';
  sTMSFNCDataBindingEditorEditMode = '&Edit Mode';
  sTMSFNCDataBindingEditorCloseEditMode = '&Close Edit Mode';
  sTMSFNCDataBindingEditorActive = 'Active';
  sTMSFNCDataBindingEditorAdd = '+';
  sTMSFNCDataBindingEditorDelete = '-';

type
  TTMSFNCDataBindingEditor = class;

  {$IFDEF FMXLIB}
  TTMSFNCDataBindingEditorParent = TFmxObject;
  TTMSFNCDataBindingEditorComboBox = class(TComboEdit);
  TTMSFNCDataBindingEditorComboBoxSub = class(TComboEdit);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  TTMSFNCDataBindingEditorParent = TWinControl;
  TTMSFNCDataBindingEditorComboBox = class(TComboBox);
  TTMSFNCDataBindingEditorComboBoxSub = class(TComboBox);
  {$ENDIF}

  TTMSFNCDataBindingTRectFArray = array of TRectF;
  TTMSFNCDataBindingStringArray = array of string;
  TTMSFNCDataBindingTComboArray = array of TTMSFNCDataBindingEditorComboBoxSub;

  TTMSFNCDataBindingEditorBuildItem = record
    BindComponentRect: TRectF;
    BindComponentText: string;
    FieldNameRect: TRectF;
    FieldNameText: string;
    PropertyNameRect: TRectF;
    PropertyNameText: string;
    ColumnsPropertyNameRect: TRectF;
    ColumnsPropertyNameText: string;
    ColumnsSubPropertyNameRect: TRectF;
    ColumnsSubPropertyNameText: string;
    Item: TTMSFNCDataBinderItem;
    SubPropertyNamesRects: TTMSFNCDataBindingTRectFArray;
    SubFieldNamesRects: TTMSFNCDataBindingTRectFArray;
    SubPropertyNamesTexts: TTMSFNCDataBindingStringArray;
    SubFieldNamesTexts: TTMSFNCDataBindingStringArray;
    SubPropertyNamesCombos: TTMSFNCDataBindingTComboArray;
    SubFieldNamesCombos: TTMSFNCDataBindingTComboArray;
  end;

  TTMSFNCDataBindingEditorMode = (dbemView, dbemEdit);

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsDesktop)]
  {$ENDIF}
  TTMSFNCDataBindingEditor = class(TTMSFNCCustomComponent)
  private
    FCachedComponentScreenShot: TTMSFNCBitmap;
    FBlockChange: Boolean;
    FMode: TTMSFNCDataBindingEditorMode;
    FBuildItem: TTMSFNCDataBindingEditorBuildItem;
    FListBox: TTMSFNCEditorList;
    frm: TTMSFNCCustomDesignerForm;
    FPanel, FTopPanel: TTMSFNCEditorPanel;
    FButtonOk, FButtonCancel, FButtonEdit, FButtonDelete, FButtonAdd: TTMSFNCEditorButton;
    {$IFNDEF FMXLIB}
    FVScrlBox: TScrollBox;
    {$ENDIF}
    {$IFDEF FMXLIB}
    FVScrlBox: TVertScrollBox;
    {$ENDIF}
    FPreview: TTMSFNCCustomControl;
    FDataBinder: TTMSFNCDataBinder;
    FCheckBoxActive: TCheckBox;
    FComboBoxBindType: TComboBox;
    FComboboxBindComponent: TComboBox;
    FComboBoxFieldName: TTMSFNCDataBindingEditorComboBox;
    FComboBoxPropertyName: TTMSFNCDataBindingEditorComboBox;
    FComboBoxColumnsPropertyName: TTMSFNCDataBindingEditorComboBox;
    FComboBoxColumnsSubPropertyName: TTMSFNCDataBindingEditorComboBox;
    FComboBoxDataSource: TComboBox;
    FSubAdd: TTMSFNCEditorButton;
    FSubDelete: TTMSFNCEditorButton;
    FObject: TObject;
    {$IFDEF WEBLIB}
    FComponentEvent: TScreenShotEvent;
    {$ENDIF}
  protected
    function GetInstance: NativeUInt; override;
    procedure RegisterRuntimeClasses; override;
    procedure MakeComponentScreenShot;
    {$IFDEF WEBLIB}
    procedure DoScreenShot(Sender: TObject; ABitmap: TBitmap);
    {$ENDIF}
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure BuildEditor(AParent: TTMSFNCDataBindingEditorParent); virtual;
    procedure BuildItem(AUpdate: Boolean); virtual;
    procedure FillControls; virtual;
    procedure FillEditControls(AUpdate: Boolean); virtual;
    procedure DoResizeForm(Sender: TObject);
    procedure DoEdit(Sender: TObject);
    procedure DoAdd(Sender: TObject);
    procedure DoDelete(Sender: TObject);
    procedure DoSubAdd(Sender: TObject);
    procedure DoSubDelete(Sender: TObject);
    procedure DoActiveChanged(Sender: TObject);
    procedure DoBindTypeChanged(Sender: TObject);
    procedure DoDataSourceChanged(Sender: TObject);
    procedure DoListBoxChange(Sender: TObject);
    procedure DoListBoxItemSelectedChange(Sender: TObject; AIndex: Integer; AItem: TTMSFNCEditorListItem; ASelected: Boolean);
    procedure DoBindComponentChanged(Sender: TObject);
    procedure DoPropertyNameChanged(Sender: TObject);
    procedure DoFieldNameChanged(Sender: TObject);
    procedure DoComboSubPropertyChanged(Sender: TObject);
    procedure DoComboSubFieldChanged(Sender: TObject);
    procedure DoColumnsPropertyNameChanged(Sender: TObject);
    procedure DoColumnsSubPropertyNameChanged(Sender: TObject);
    procedure DoAfterDraw(Sender: TObject; AGraphics: TTMSFNCGraphics; ARect: TRectF); virtual;
    {$IFDEF VCLLIB}
    {$HINTS OFF}
    {$IF COMPILERVERSION >= 33}
    procedure DoAfterMonitorDpiChanged(Sender: TObject; OldDPI: Integer; NewDPI: Integer);
    {$ENDIF}
    {$HINTS ON}
    {$ENDIF}
    {$IFNDEF FMXLIB}
    procedure DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    {$ENDIF}
    {$IFDEF FMXLIB}
    procedure DoFormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    {$ENDIF}
    {$IFDEF WEBLIB}
    procedure ApplyCSS(ACSS: string); virtual;
    procedure DoButtonOKClick(Sender: TObject); virtual;
    procedure DoButtonCancelClick(Sender: TObject); virtual;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF WEBLIB}
    procedure Execute(AProc: TModalResultProc = nil);
    {$ELSE}
    function Execute: TModalResult;
    {$ENDIF}
    procedure Assign(Source: TPersistent); override;
  published
    property DataBinder: TTMSFNCDataBinder read FDataBinder write FDataBinder;
    property &Object: TObject read FObject write FObject;
  end;

  TTMSFNCDataBindingEditorForm = class(TTMSFNCCustomDesignerForm)
  {$IFDEF WEBLIB}
  private
    FDataBindingEditor: TTMSFNCDataBindingEditor;
  protected
    procedure Loaded; override;
    procedure LoadDFMValues; override;
  public
    constructor CreateDialogNew(ADataBindingEditor: TTMSFNCDataBindingEditor; ACaption: string); reintroduce; virtual;
  {$ENDIF}
  end;

implementation

uses
  Math, TypInfo, FMX.TMSFNCPersistence, FMX.TMSFNCUtils, SysUtils, FMX.Dialogs
  , FMX.TMSFNCEditorsTools
  {$IFDEF WEBLIB}
  ,WEBLib.WebTools
  {$ENDIF}
  {$IFNDEF FMXMOBILE}
  {$IFDEF DELPHIBERLIN}
  ,FMX.DialogService.Sync
  {$ENDIF}
  {$ENDIF}

  {$IFNDEF LCLWEBLIB}
  ,Data.DB
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,DB, WEBLib.DB
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,DB
  {$ENDIF}
  ;

{ TTMSFNCDataBindingEditor }

function TranslateTextEx(AText: String): String;
begin
  {$IFDEF FMXLIB}
  Result := TranslateText(AText);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  Result := AText;
  {$ENDIF}
end;

procedure TTMSFNCDataBindingEditor.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) then
  begin
    if (AComponent = FDataBinder) then
      FDataBinder := nil;

    if (AComponent = FObject) then
      FObject := nil;
  end;
end;

procedure TTMSFNCDataBindingEditor.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TTMSFNCDataBindingEditor);
end;

procedure TTMSFNCDataBindingEditor.Assign(Source: TPersistent);
begin
end;

procedure TTMSFNCDataBindingEditor.BuildEditor(AParent: TTMSFNCDataBindingEditorParent);
var
  sl: TStringList;
  I: Integer;
begin
  if Assigned(AParent) then
  begin
    FTopPanel := TTMSFNCEditorPanel.Create(AParent);
    {$IFDEF FMXLIB}
    FTopPanel.Align := TAlignLayout.Top;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FTopPanel.Align := alTop;
    FTopPanel.Top := 0;
    {$ENDIF}
    FTopPanel.Parent := AParent;

    FTopPanel.Height := 37;
    {$IFDEF CMNLIB}
    FTopPanel.PanelPosition := eppCenter;
    {$ENDIF}
    {$IFNDEF CMNLIB}
    FTopPanel.PanelPosition := eppTop;
    {$ENDIF}
    SetEditorBackPanelAppearance(FTopPanel);

    FButtonEdit := TTMSFNCEditorButton.Create(FTopPanel);
    FButtonEdit.Parent := FTopPanel;
    FButtonEdit.Text := TranslateTextEx(sTMSFNCDataBindingEditorEditMode);
    {$IFDEF FMXLIB}
    FButtonEdit.Align := TAlignLayout.Right;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonEdit.Align := alRight;
    {$IFDEF VCLWEBLIB}
    FButtonEdit.AlignWithMargins := True;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF LCLLIB}
    FButtonEdit.BorderSpacing.Right := 5;
    FButtonEdit.BorderSpacing.Top := 5;
    FButtonEdit.BorderSpacing.Bottom := 5;
    FButtonEdit.BorderSpacing.Left := 5;
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FButtonEdit.Margins.Right := 5;
    FButtonEdit.Margins.Top := 5;
    FButtonEdit.Margins.Bottom := 5;
    FButtonEdit.Margins.Left := 5;
    {$ENDIF}
    FButtonEdit.OnClick := DoEdit;
    FButtonEdit.Width := 100;
    FButtonEdit.Toggle := True;
    FButtonEdit.Enabled := False;
    SetEditorTabButtonAppearance(FButtonEdit);

    FButtonAdd := TTMSFNCEditorButton.Create(FTopPanel);
    FButtonAdd.Parent := FTopPanel;
    FButtonAdd.Width := 30;
    FButtonAdd.Text := TranslateTextEx(sTMSFNCDataBindingEditorAdd);
    {$IFDEF CMNWEBLIB}
    {$IFDEF VCLWEBLIB}
    FButtonAdd.AlignWithMargins := True;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF LCLLIB}
    FButtonAdd.BorderSpacing.Right := 5;
    FButtonAdd.BorderSpacing.Top := 5;
    FButtonAdd.BorderSpacing.Bottom := 5;
    FButtonAdd.BorderSpacing.Left := 5;
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FButtonAdd.Margins.Right := 5;
    FButtonAdd.Margins.Top := 5;
    FButtonAdd.Margins.Bottom := 5;
    FButtonAdd.Margins.Left := 5;
    {$ENDIF}
    FButtonAdd.OnClick := DoAdd;
    SetEditorIconButtonAppearance(FButtonAdd);
    FButtonAdd.Appearance.Font.Style := [TFontStyle.fsBold];

    FButtonDelete := TTMSFNCEditorButton.Create(FTopPanel);
    FButtonDelete.Parent := FTopPanel;
    FButtonDelete.Width := 30;
    FButtonDelete.Text := TranslateTextEx(sTMSFNCDataBindingEditorDelete);
    {$IFDEF CMNWEBLIB}
    {$IFDEF VCLWEBLIB}
    FButtonDelete.AlignWithMargins := True;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF LCLLIB}
    FButtonDelete.BorderSpacing.Right := 5;
    FButtonDelete.BorderSpacing.Top := 5;
    FButtonDelete.BorderSpacing.Bottom := 5;
    FButtonDelete.BorderSpacing.Left := 0;
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FButtonDelete.Margins.Right := 5;
    FButtonDelete.Margins.Top := 5;
    FButtonDelete.Margins.Bottom := 5;
    FButtonDelete.Margins.Left := 0;
    {$ENDIF}
    FButtonDelete.OnClick := DoDelete;
    SetEditorIconButtonAppearance(FButtonDelete);
    FButtonDelete.Appearance.Font.Style := [TFontStyle.fsBold];

    {$IFDEF FMXLIB}
    FButtonDelete.Index := 1;
    FButtonAdd.Index := 0;
    FButtonDelete.Align := TAlignLayout.Left;
    FButtonAdd.Align := TAlignLayout.Left;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonAdd.Align := alLeft;
    FButtonDelete.Align := alLeft;
    {$ENDIF}

    {$IFDEF FMXLIB}
    FVScrlBox := TVertScrollBox.Create(AParent);
    FVScrlBox.Align := TAlignLayout.Left;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FVScrlBox := TScrollBox.Create(AParent);
    FVScrlBox.Align := alLeft;
    FVScrlBox.Top := 0;
    {$IFDEF LCLLIB}
    FVScrlBox.HorzScrollBar.Visible:= False;
    {$ENDIF}
    {$ENDIF}
    FVScrlBox.Parent := AParent;
    FVScrlBox.Width := 300;
    {$IFDEF CMNWEBLIB}
    FVScrlBox.BorderStyle := bsNone;
    if IsLightTheme then
      FVScrlBox.Color := EDITORSUBBACKCOLORLIGHT
    else
      FVScrlBox.Color := EDITORSUBBACKCOLORDARK;
    {$ENDIF}

    FListBox := TTMSFNCEditorList.Create(FVScrlBox);
    FListBox.Parent := FVScrlBox;
    {$IFDEF FMXLIB}
    FListBox.Align := TAlignLayout.Top;
    FListBox.OnItemsChanged := DoListBoxChange;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FListBox.Align := alTop;
    {$ENDIF}
    FListBox.MultiSelect := False;
    FListBox.DefaultItemHeight := 28;
    SetEditorListAppearance(FListBox);
    FListBox.ItemsReadOnly := True;
    FListBox.OnItemSelectedChanged := {$IFDEF LCLLIB}@{$ENDIF}DoListBoxItemSelectedChange;

    FPreview := TTMSFNCCustomControl.Create(AParent);
    FPreview.Parent := AParent;
    FPreview.ControlAlignment := caClient;
    {$IFDEF WEBLIB}
    FPreview.DisableBackground;
    {$ENDIF}
    if IsLightTheme then
      FPreview.Color := EDITORSUBBACKCOLORLIGHT
    else
      FPreview.Color := EDITORSUBBACKCOLORDARK;
    FPreview.OnAfterDraw := DoAfterDraw;

    FPanel := TTMSFNCEditorPanel.Create(AParent);
    {$IFDEF FMXLIB}
    FPanel.Align := TAlignLayout.Bottom;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FPanel.Align := alBottom;
    {$ENDIF}
    FPanel.Height := 37;
    FPanel.Parent := AParent;
    {$IFDEF CMNLIB}
    FPanel.PanelPosition := eppCenter;
    {$ENDIF}
    {$IFNDEF CMNLIB}
    FPanel.PanelPosition := eppBottom;
    {$ENDIF}
    SetEditorBackPanelAppearance(FPanel);

    FButtonCancel := TTMSFNCEditorButton.Create(FPanel);
    FButtonCancel.ModalResult := mrCancel;
    FButtonCancel.Parent := FPanel;
    FButtonCancel.Text := TranslateTextEx(sTMSFNCDataBindingEditorCancel);
    {$IFDEF FMXLIB}
    FButtonCancel.Align := TAlignLayout.Right;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonCancel.Align := alRight;
    FButtonCancel.Left := 100;
    {$IFDEF VCLWEBLIB}
    FButtonCancel.AlignWithMargins := True;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF LCLLIB}
    FButtonCancel.BorderSpacing.Right := 5;
    FButtonCancel.BorderSpacing.Top := 5;
    FButtonCancel.BorderSpacing.Bottom := 5;
    FButtonCancel.BorderSpacing.Left := 5;
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FButtonCancel.Margins.Right := 5;
    FButtonCancel.Margins.Top := 5;
    FButtonCancel.Margins.Bottom := 5;
    FButtonCancel.Margins.Left := 5;
    {$ENDIF}
    {$IFDEF WEBLIB}
    FButtonCancel.OnClick := DoButtonCancelClick;
    {$ENDIF}
    SetEditorCancelButtonAppearance(FButtonCancel);


    FButtonOK := TTMSFNCEditorButton.Create(FPanel);
    FButtonOK.ModalResult := mrOk;
    FButtonOK.Parent := FPanel;
    FButtonOK.Text := TranslateTextEx(sTMSFNCDataBindingEditorOK);
    {$IFDEF FMXLIB}
    FButtonOk.Align := TAlignLayout.Right;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonOk.Align := alRight;
    FButtonOk.Left := 99;
    {$IFDEF VCLWEBLIB}
    FButtonOK.AlignWithMargins := True;
    {$ENDIF}
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FButtonOK.Margins.Right := 0;
    FButtonOK.Margins.Top := 5;
    FButtonOK.Margins.Bottom := 5;
    FButtonOK.Margins.Left := 5;
    {$ENDIF}
    {$IFDEF LCLLIB}
    FButtonOK.BorderSpacing.Right := 0;
    FButtonOK.BorderSpacing.Top := 5;
    FButtonOK.BorderSpacing.Bottom := 5;
    FButtonOK.BorderSpacing.Left := 5;
    {$ENDIF}
    {$IFDEF WEBLIB}
    FButtonOK.OnClick := DoButtonOKClick;
    {$ENDIF}
    SetEditorOKButtonAppearance(FButtonOk);

    FComboBoxBindType := TComboBox.Create(AParent);
    FComboBoxBindType.Parent := AParent;
    {$IFDEF CMNWEBLIB}
    FComboBoxBindType.Style := csDropDownList;
    {$ENDIF}
    FComboBoxBindType.Items.Add('Single Value');
    FComboBoxBindType.Items.Add('List');
    FComboBoxBindType.Items.Add('Column List');
    FComboBoxBindType.Items.Add('Grid');
    FComboBoxBindType.OnChange := DoBindTypeChanged;

    FComboBoxDataSource := TComboBox.Create(AParent);
    FComboBoxDataSource.Parent := AParent;
    sl := TStringList.Create;
    try
      if Assigned(FDataBinder) then
        FDataBinder.GetDataSources(sl);

      sl.Insert(0, '');
      FComboBoxDataSource.Items.Assign(sl);
    finally
      sl.Free;
    end;
    {$IFDEF CMNWEBLIB}
    FComboBoxDataSource.Style := csDropDownList;
    {$ENDIF}
    FComboBoxDataSource.OnChange := DoDataSourceChanged;

    FCheckBoxActive := TCheckBox.Create(AParent);
    FCheckBoxActive.Parent := AParent;
    {$IFDEF FMXLIB}
    FCheckBoxActive.Text := TranslateTextEx(sTMSFNCDataBindingEditorActive);
    FCheckBoxActive.OnChange := DoActiveChanged;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FCheckBoxActive.Caption := TranslateTextEx(sTMSFNCDataBindingEditorActive);
    FCheckBoxActive.OnClick := DoActiveChanged;
    {$IFDEF WEBLIB}
    if IsLightTheme then
      FCheckBoxActive.Font.Color := EDITORFONTCOLORLIGHT
    else
      FCheckBoxActive.Font.Color := EDITORFONTCOLORDARK;
    {$ENDIF}
    {$ENDIF}
    SetEditorCheckBoxAppearance(FCheckBoxActive);
    FCheckBoxActive.Width := 60;

    FComboBoxFieldName := TTMSFNCDataBindingEditorComboBox.Create(AParent);
    FComboBoxFieldName.OnChange := DoFieldNameChanged;
    FComboBoxPropertyName := TTMSFNCDataBindingEditorComboBox.Create(AParent);
    FComboBoxPropertyName.OnChange := DoPropertyNameChanged;
    FComboBoxColumnsPropertyName := TTMSFNCDataBindingEditorComboBox.Create(AParent);
    FComboBoxColumnsPropertyName.OnChange := DoColumnsPropertyNameChanged;
    FComboBoxColumnsSubPropertyName := TTMSFNCDataBindingEditorComboBox.Create(AParent);
    FComboBoxColumnsSubPropertyName.OnChange := DoColumnsSubPropertyNameChanged;
    FComboBoxPropertyName := TTMSFNCDataBindingEditorComboBox.Create(AParent);
    FComboBoxPropertyName.OnChange := DoPropertyNameChanged;
    FComboboxBindComponent := TComboBox.Create(AParent);
    FComboboxBindComponent.OnChange := DoBindComponentChanged;
    {$IFDEF CMNWEBLIB}
    FComboboxBindComponent.Style := csDropDownList;
    {$ENDIF}

    FSubAdd := TTMSFNCEditorButton.Create(FPreview);
    FSubAdd.Width := 30;
    FSubAdd.Text := TranslateTextEx(sTMSFNCDataBindingEditorAdd);
    FSubAdd.OnClick := DoSubAdd;
    SetEditorIconButtonAppearance(FSubAdd);
    FSubAdd.Appearance.Font.Style := [TFontStyle.fsBold];

    FSubDelete := TTMSFNCEditorButton.Create(FPreview);
    FSubDelete.Width := 30;
    FSubDelete.Text := TranslateTextEx(sTMSFNCDataBindingEditorDelete);
    FSubDelete.OnClick := DoSubDelete;
    SetEditorIconButtonAppearance(FSubDelete);
    FSubDelete.Appearance.Font.Style := [TFontStyle.fsBold];

    FillControls;
    if FListBox.Items.Count > 0 then
    begin
      if Assigned(FObject) then
      begin
        for I := 0 to FListBox.Items.Count - 1 do
        begin
          if TTMSFNCDataBinderItem(FListBox.Items[I].DataObject).&Object = FObject then
          begin
            FListBox.Items[I].Selected := True;
            Break;
          end;
        end;
      end
      else
        FListBox.UnselectAllItems;
    end;
    BuildItem(True);
  end;
end;

procedure TTMSFNCDataBindingEditor.BuildItem(AUpdate: Boolean);
var
  x, xm, y: Single;
  sc: Single;
  r, rt: TRectF;
  str: string;
  it: TTMSFNCDataBinderItem;
  g: TTMSFNCGraphics;
  fa, pa: TTMSFNCDataBindingTRectFArray;
  fb, pb: TTMSFNCDataBindingStringArray;
  fc, pc: TTMSFNCDataBindingTComboArray;
  I: Integer;
  cbo: TTMSFNCDataBindingEditorComboBoxSub;
begin
  FBuildItem.BindComponentRect := EmptyRect;
  FBuildItem.BindComponentText := '';
  FBuildItem.FieldNameRect := EmptyRect;
  FBuildItem.FieldNameText := '';
  FBuildItem.PropertyNameRect := EmptyRect;
  FBuildItem.PropertyNameText := '';
  FBuildItem.ColumnsPropertyNameRect := EmptyRect;
  FBuildItem.ColumnsPropertyNameText := '';
  FBuildItem.ColumnsSubPropertyNameRect := EmptyRect;
  FBuildItem.ColumnsSubPropertyNameText := '';
  if AUpdate then
  begin
    SetLength(fa, 0);
    SetLength(fb, 0);
    SetLength(fc, 0);
    SetLength(pa, 0);
    SetLength(pb, 0);
    SetLength(pc, 0);
    FBuildItem.SubPropertyNamesRects := pa;
    FBuildItem.SubPropertyNamesTexts := pb;
    FBuildItem.SubPropertyNamesCombos := pc;
    FBuildItem.SubFieldNamesRects := fa;
    FBuildItem.SubFieldNamesTexts := fb;
    FBuildItem.SubFieldNamesCombos := fc;
  end;

  FBuildItem.Item := nil;

  if AUpdate then
  begin
    for I := FPreview.ComponentCount - 1 downto 0 do
    begin
      if FPreview.Components[I] is TTMSFNCDataBindingEditorComboBoxSub then
      begin
        cbo := TTMSFNCDataBindingEditorComboBoxSub(FPreview.Components[I]);
        cbo.Free;
      end;
    end;
  end;

  if (FListBox.LastSelectedItemIndex >= 0) and (FListBox.LastSelectedItemIndex <= FListBox.Items.Count - 1) then
  begin
    it := TTMSFNCDataBinderItem(FListBox.Items[FListBox.LastSelectedItemIndex].DataObject);

    {$IFDEF FMXLIB}
    FCheckBoxActive.IsChecked := it.Active;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FCheckBoxActive.Checked := it.Active;
    {$ENDIF}

    FBlockChange := True;
    FComboBoxBindType.ItemIndex := Integer(it.BindType);
    FComboBoxDataSource.ItemIndex := FComboBoxDataSource.Items.IndexOfObject(it.DataSource);
    FBlockChange := False;

    g := TTMSFNCGraphics.CreateBitmapCanvas;
    g.BeginScene;
    try
//      TTMSFNCUtils.SetFontSize(g.Font, 14);
      if Assigned(frm) then
        sc := TTMSFNCUtils.GetDPIScale(frm, 96)
      else
        sc := TTMSFNCUtils.GetDPIScale(Self, 96);

      g.Font.Height := Round(sc * -11);

      if Assigned(it.&Object) then
      begin
        if (it.&Object is TComponent) and ((it.&Object as TComponent).Name <> '') then
          str := 'Bind component: ' + (it.&Object as TComponent).Name
        else
          str := 'Bind component: ' + it.&Object.ClassName
      end
      else
        str := 'Bind component not assigned!';


      x := 20;
      y := 10;

      r := g.CalculateText(str, RectF(0, 0, 400, 10000), True, False);
      rt := RectF(x, y, x + (r.Right - r.Left) + 6, y + (r.Bottom - r.Top) + 6);

      FBuildItem.BindComponentRect := rt;
      FBuildItem.BindComponentText := str;
      FBuildItem.Item := it;

      if it.BindType <> dbbtGrid then
      begin
        y := y + 40;
        x := x + 40;

        if it.FieldName <> '' then
          str := 'Field name: ' + it.FieldName
        else if it.HTMLTemplate <> '' then
          str := 'HTML template: ' + it.HTMLTemplate
        else
          str := 'Field name: ' + '[Not bound]';

        r := g.CalculateText(str, RectF(0, 0, 400, 10000), True, False);
        rt := RectF(x, y, x + (r.Right - r.Left) + 6, y + (r.Bottom - r.Top) + 6);

        FBuildItem.FieldNameRect := rt;
        FBuildItem.FieldNameText := str;

        if it.ColumnsPropertyName <> '' then
          str := 'Columns property name: ' + it.ColumnsPropertyName
        else
          str := 'Columns property name: ' + '[Not bound]';

        r := g.CalculateText(str, RectF(0, 0, 400, 10000), True, False);
        rt := RectF(x, y, x + (r.Right - r.Left) + 6, y + (r.Bottom - r.Top) + 6);

        FBuildItem.ColumnsPropertyNameRect := rt;
        FBuildItem.ColumnsPropertyNameText := str;

        if it.ColumnsSubPropertyName <> '' then
          str := 'Columns sub property name: ' + it.ColumnsSubPropertyName
        else
          str := 'Columns sub property name: ' + '[Not bound]';

        r := g.CalculateText(str, RectF(0, 0, 400, 10000), True, False);
        rt := RectF(rt.Right + 20, y + (rt.Bottom - rt.Top) / 2 - (r.Bottom - r.Top + 6) / 2, rt.Right + 20 + (r.Right - r.Left) + 6,
          y + (rt.Bottom - rt.Top) / 2 + (r.Bottom - r.Top + 6) / 2);

        FBuildItem.ColumnsSubPropertyNameRect := rt;
        FBuildItem.ColumnsSubPropertyNameText := str;

        case it.BindType of
          dbbtSingleValue:
          begin
            if it.PropertyName <> '' then
              str := 'Property name: ' + it.PropertyName
            else
              str := 'Property name: ' + '[Not bound]';
          end;
          dbbtList, dbbtColumnList:
          begin
            if it.PropertyName <> '' then
              str := 'List property name: ' + it.PropertyName
            else
              str := 'List property name: ' + '[Not bound]';
          end;
        end;

        r := g.CalculateText(str, RectF(0, 0, 400, 10000), True, False);

        case it.BindType of
          dbbtSingleValue, dbbtList:
          begin
            rt := FBuildItem.FieldNameRect;
            rt := RectF(rt.Right + 20, y + (rt.Bottom - rt.Top) / 2 - (r.Bottom - r.Top + 6) / 2, rt.Right + 20 + (r.Right - r.Left) + 6,
              y + (rt.Bottom - rt.Top) / 2 + (r.Bottom - r.Top + 6) / 2);

            x := rt.Left;
          end;
          dbbtColumnList:
          begin
            rt := FBuildItem.ColumnsSubPropertyNameRect;
            rt := RectF(x, rt.Bottom + 10, x + (r.Right - r.Left) + 6, rt.Bottom + 10 + (r.Bottom - r.Top) + 6);
          end;
        end;

        FBuildItem.PropertyNameRect := rt;
        FBuildItem.PropertyNameText := str;

        if AUpdate then
        begin
          SetLength(fa, it.SubFieldNames.Count);
          SetLength(fb, it.SubFieldNames.Count);
          SetLength(fc, it.SubFieldNames.Count);
          SetLength(pa, it.SubPropertyNames.Count);
          SetLength(pb, it.SubPropertyNames.Count);
          SetLength(pc, it.SubPropertyNames.Count);
        end
        else
        begin
          fa := FBuildItem.SubFieldNamesRects;
          fb := FBuildItem.SubFieldNamesTexts;
          fc := FBuildItem.SubFieldNamesCombos;
          pa := FBuildItem.SubPropertyNamesRects;
          pb := FBuildItem.SubPropertyNamesTexts;
          pc := FBuildItem.SubPropertyNamesCombos;
        end;

        xm := 0;
        y := rt.Bottom + 50;

        for I := 0 to it.SubFieldNames.Count - 1 do
        begin
          if it.SubFieldNames[I].Value <> '' then
            str := 'Field name: ' + it.SubFieldNames[I].Value
          else if it.SubFieldNames[I].HTMLTemplate <> '' then
            str := 'HTML template: ' + it.SubFieldNames[I].HTMLTemplate
          else
            str := 'Field name: ' + '[Not bound]';

          fb[I] := str;
          r := g.CalculateText(str, RectF(0, 0, 400, 10000), True, False);
          fa[I] := RectF(x, y, x + (r.Right - r.Left) + 6, y + (r.Bottom - r.Top) + 6);

          xm := Max(fa[I].Right - fa[I].Left, xm);

          y := y + (fa[I].Bottom - fa[I].Top) + 10;

          if AUpdate then
            fc[I] := TTMSFNCDataBindingEditorComboBoxSub.Create(FPreview);

          fc[I].Tag := I;
          fc[I].SetBounds(Round(fa[I].Left), Round(fa[I].Top), Round(fa[I].Right - fa[I].Left), Round(fa[I].Bottom - fa[I].Top));
          fc[I].OnChange := DoComboSubFieldChanged;
        end;

        if xm > 0 then
          xm := xm + 20;

        y := rt.Bottom + 50;

        for I := 0 to it.SubPropertyNames.Count - 1 do
        begin
          if it.SubPropertyNames[I].Value <> '' then
            str := 'Property name: ' + it.SubPropertyNames[I].Value
          else
            str := 'Property name: ' + '[Not bound]';

          pb[I] := str;
          r := g.CalculateText(str, RectF(0, 0, 400, 10000), True, False);
          pa[I] := RectF(x + xm, y, x + xm + (r.Right - r.Left) + 6, y + (r.Bottom - r.Top) + 6);
          y := y + (pa[I].Bottom - pa[I].Top) + 10;

          if AUpdate then
            pc[I] := TTMSFNCDataBindingEditorComboBoxSub.Create(FPreview);

          pc[I].Tag := I;
          pc[I].SetBounds(Round(pa[I].Left), Round(pa[I].Top), Round(pa[I].Right - pa[I].Left), Round(pa[I].Bottom - pa[I].Top));
          pc[I].OnChange := DoComboSubPropertyChanged;
        end;

        FBuildItem.SubPropertyNamesRects := pa;
        FBuildItem.SubPropertyNamesTexts := pb;
        FBuildItem.SubPropertyNamesCombos := pc;
        FBuildItem.SubFieldNamesRects := fa;
        FBuildItem.SubFieldNamesTexts := fb;
        FBuildItem.SubFieldNamesCombos := fc;
      end;
    finally
      g.EndScene;
      g.Free;
    end;
  end;

  MakeComponentScreenShot;

  FPreview.Repaint;
end;

{$IFDEF WEBLIB}
procedure TTMSFNCDataBindingEditor.ApplyCSS(ACSS: string);
begin
//  FButtonOK.ElementClassName := 'IDEButton IDEFont';
//  FButtonCancel.ElementClassName := 'IDEButton IDEFont';
//  FButtonEdit.ElementClassName := 'IDEButton IDEFont';
//  FButtonDelete.ElementClassName := 'IDEButton IDEFont';
//  FSubAdd.ElementClassName := 'IDEButton IDEFont';
//  FSubDelete.ElementClassName := 'IDEButton IDEFont';
//
//  FTopPanel.ElementClassName := 'IDEBkg';
//  FPanel.ElementClassName := 'IDEBkg';
end;

procedure TTMSFNCDataBindingEditor.DoButtonOKClick(Sender: TObject);
begin
  if Assigned(frm) then
    frm.ModalResult := mrOK;
end;

procedure TTMSFNCDataBindingEditor.DoButtonCancelClick(Sender: TObject);
begin
  if Assigned(frm) then
    frm.ModalResult := mrCancel;
end;
{$ENDIF}

constructor TTMSFNCDataBindingEditor.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TTMSFNCDataBindingEditor.Destroy;
begin
  if Assigned(FCachedComponentScreenShot) then
  begin
    FCachedComponentScreenShot.Free;
    FCachedComponentScreenShot := nil;
  end;

  inherited;
end;

procedure TTMSFNCDataBindingEditor.DoActiveChanged(Sender: TObject);
begin
  if Assigned(FBuildItem.Item) then
  begin
    {$IFDEF FMXLIB}
    FBuildItem.Item.Active := FCheckBoxActive.IsChecked;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FBuildItem.Item.Active := FCheckBoxActive.Checked;
    {$ENDIF}
    FPreview.Repaint;
  end;
end;

procedure TTMSFNCDataBindingEditor.DoAdd(Sender: TObject);
begin
  if Assigned(FDataBinder) then
    FDataBinder.Items.Add;

  FillControls;
  if FListBox.Items.Count > 0 then
    FListBox.SelectItem(FListBox.Items.Count - 1);

  BuildItem(True);
  FillEditControls(True);
end;

procedure TTMSFNCDataBindingEditor.DoAfterDraw(Sender: TObject;
  AGraphics: TTMSFNCGraphics; ARect: TRectF);
var
  bcr, fnr, pnr, cnr, cnsr, bmpr, r: TRectF;
  bmp: TTMSFNCBitmap;
  x, y: Single;
  I: Integer;
  str: string;
  c: TTMSFNCGraphicsColor;
  sc: Single;

  procedure DrawSplittedText(ATextRect: TRectF; AText: string);
  var
    sl: TStringList;
    rc, rt, rtt: TRectF;
    x: Single;
    ft: TTMSFNCGraphicsColor;
    fk: TTMSFNCGraphicsFillKind;
    K: Integer;
    spc: Single;
  begin
    if TTMSFNCUtils.IsHTML(AText) then
    begin
      AGraphics.DrawText(ATextRect, AText, True, gtaCenter, gtaCenter, gttNone, 0, -1, -1, False);
      Exit;
    end;
    
    ft := AGraphics.Font.Color;
    fk := AGraphics.Fill.Kind;

    sl := TStringList.Create;
    try
      x := ATextRect.Left;
      TTMSFNCUtils.Split(':', AText, sl);
      spc := Round(sc * 6);
      if sl.Count > 0 then
        spc := spc / sl.Count;

      for K := 0 to sl.Count - 1 do
      begin
        rc := AGraphics.CalculateText(sl[K]);
        rt := RectF(x, ATextRect.Top, x + (rc.Right - rc.Left) + spc, ATextRect.Bottom);
        if K > 0 then
        begin
          AGraphics.Fill.Kind := gfkSolid;
          AGraphics.DrawRectangle(RectF(rt.Left, rt.Top, rt.Right + spc, rt.Bottom));
          AGraphics.Font.Color := gcWhite;
        end;
        
        rtt := rt;
        AGraphics.DrawText(rtt, sl[K], True, gtaCenter, gtaCenter, gttNone, 0, -1, -1, False);
        x := rt.Right + spc;
      end;
    finally
      sl.Free;
      AGraphics.Font.Color := ft;
      AGraphics.Fill.Kind := fk;
    end;
  end;
begin
  if Assigned(FBuildItem.Item) then
  begin
    sc := TTMSFNCUtils.GetDPIScale(frm, 96);

    c := EDITORPRIMCOLOR;
//    TTMSFNCUtils.SetFontSize(AGraphics.Font, 14);
    AGraphics.Font.Height := Round(-11 * sc);

    AGraphics.Stroke.Color := c;
    AGraphics.Fill.Color := c;
    AGraphics.Stroke.Kind := gskSolid;
    AGraphics.Fill.Kind := gfkNone;
    AGraphics.Font.Color := c;

    y := 0;

    if FBuildItem.Item.BindType = dbbtGrid then
    begin
      bcr := FBuildItem.BindComponentRect;
      if FMode = dbemView then
      begin
        AGraphics.DrawRectangle(bcr);
        DrawSplittedText(bcr, FBuildItem.BindComponentText);
      end;

      y := bcr.Bottom;
    end
    else
    begin
      if FBuildItem.Item.BindType in [dbbtSingleValue, dbbtList] then
      begin
        bcr := FBuildItem.BindComponentRect;
        fnr := FBuildItem.FieldNameRect;
        pnr := FBuildItem.PropertyNameRect;

        AGraphics.DrawLine(PointF(bcr.Left, bcr.Top + (bcr.Bottom - bcr.Top) / 2), PointF(bcr.Left - Round(sc * 10), bcr.Top + (bcr.Bottom - bcr.Top) / 2));
        AGraphics.DrawLine(PointF(bcr.Left - Round(sc * 10), bcr.Top + (bcr.Bottom - bcr.Top) / 2), PointF(bcr.Left - Round(sc * 10), fnr.Top + (fnr.Bottom - fnr.Top) / 2));
        AGraphics.DrawLine(PointF(bcr.Left - Round(sc * 10), fnr.Top + (fnr.Bottom - fnr.Top) / 2), PointF(fnr.Left, fnr.Top + (fnr.Bottom - fnr.Top) / 2));
        AGraphics.DrawLine(PointF(fnr.Right, fnr.Top + (fnr.Bottom - fnr.Top) / 2), PointF(pnr.Left, pnr.Top + (pnr.Bottom - pnr.Top) / 2));

        if FMode = dbemView then
        begin
          AGraphics.DrawRectangle(bcr);
          AGraphics.DrawRectangle(fnr);
          AGraphics.DrawRectangle(pnr);

          DrawSplittedText(bcr, FBuildItem.BindComponentText);
          DrawSplittedText(fnr, FBuildItem.FieldNameText);
          DrawSplittedText(pnr, FBuildItem.PropertyNameText);
        end;
      end
      else
      begin
        bcr := FBuildItem.BindComponentRect;
        cnr := FBuildItem.ColumnsPropertyNameRect;
        cnsr := FBuildItem.ColumnsSubPropertyNameRect;
        pnr := FBuildItem.PropertyNameRect;

        AGraphics.DrawLine(PointF(bcr.Left, bcr.Top + (bcr.Bottom - bcr.Top) / 2), PointF(bcr.Left - Round(sc * 10), bcr.Top + (bcr.Bottom - bcr.Top) / 2));
        AGraphics.DrawLine(PointF(bcr.Left - Round(sc * 10), bcr.Top + (bcr.Bottom - bcr.Top) / 2), PointF(bcr.Left - Round(sc * 10), cnr.Top + (cnr.Bottom - cnr.Top) / 2));
        AGraphics.DrawLine(PointF(bcr.Left - Round(sc * 10), bcr.Top + (bcr.Bottom - bcr.Top) / 2), PointF(bcr.Left - Round(sc * 10), pnr.Top + (pnr.Bottom - pnr.Top) / 2));
        AGraphics.DrawLine(PointF(bcr.Left - Round(sc * 10), bcr.Top + (bcr.Bottom - bcr.Top) / 2), PointF(bcr.Left - Round(sc * 10), cnsr.Top + (cnsr.Bottom - cnsr.Top) / 2));
        AGraphics.DrawLine(PointF(bcr.Left - Round(sc * 10), cnr.Top + (cnr.Bottom - cnr.Top) / 2), PointF(cnr.Left, cnr.Top + (cnr.Bottom - cnr.Top) / 2));
        AGraphics.DrawLine(PointF(bcr.Left - Round(sc * 10), pnr.Top + (pnr.Bottom - pnr.Top) / 2), PointF(pnr.Left, pnr.Top + (pnr.Bottom - pnr.Top) / 2));
        AGraphics.DrawLine(PointF(cnr.Right, cnr.Top + (cnr.Bottom - cnr.Top) / 2), PointF(cnsr.Left, cnsr.Top + (cnsr.Bottom - cnsr.Top) / 2));

        if FMode = dbemView then
        begin
          AGraphics.DrawRectangle(bcr);
          AGraphics.DrawRectangle(cnr);
          AGraphics.DrawRectangle(cnsr);
          AGraphics.DrawRectangle(pnr);

          DrawSplittedText(bcr, FBuildItem.BindComponentText);
          DrawSplittedText(cnr, FBuildItem.ColumnsPropertyNameText);
          DrawSplittedText(cnsr, FBuildItem.ColumnsSubPropertyNameText);
          DrawSplittedText(pnr, FBuildItem.PropertyNameText);
        end;
      end;

      x := 40;

      for I := 0 to Length(FBuildItem.SubFieldNamesRects) - 1 do
      begin
        r := FBuildItem.SubFieldNamesRects[I];
        str := FBuildItem.SubFieldNamesTexts[I];
        if FMode = dbemView then
        begin
          AGraphics.DrawRectangle(r);
          DrawSplittedText(r, str);
        end;
        y := Max(r.Bottom, y);
        x := Max(r.Right, x);
      end;

      if ((Length(FBuildItem.SubFieldNamesRects) > 0) or (Length(FBuildItem.SubPropertyNamesRects) > 0)) and (FBuildItem.Item.BindType in [dbbtColumnList, dbbtList]) then
      begin
        AGraphics.DrawLine(PointF(pnr.Right, pnr.Top + (pnr.Bottom - pnr.Top) / 2), PointF(pnr.Right + Round(sc * 10), pnr.Top + (pnr.Bottom - pnr.Top) / 2));
        AGraphics.DrawLine(PointF(pnr.Right + Round(sc * 10), pnr.Top + (pnr.Bottom - pnr.Top) / 2), PointF(pnr.Right + Round(sc * 10), pnr.Top + ((pnr.Bottom - pnr.Top) / 2)  + Round(sc * 30)));
      end;

      if Length(FBuildItem.SubFieldNamesRects) > 0 then
      begin
        AGraphics.DrawLine(PointF(r.Left, pnr.Top + ((pnr.Bottom - pnr.Top) / 2) + Round(sc * 30)), PointF(Max(pnr.Right + Round(sc * 10), x), pnr.Top + ((pnr.Bottom - pnr.Top) / 2)  + Round(sc * 30)));
        AGraphics.DrawText(PointF(r.Left, pnr.Bottom + Round(sc * 25)), 'Sub field names');
      end;

      if Length(FBuildItem.SubPropertyNamesRects) > 0 then
        AGraphics.DrawText(PointF(x + Round(sc * 20), pnr.Bottom + Round(sc * 25)), 'Sub property names');

      x := 0;

      for I := 0 to Length(FBuildItem.SubPropertyNamesRects) - 1 do
      begin
        r := FBuildItem.SubPropertyNamesRects[I];
        str := FBuildItem.SubPropertyNamesTexts[I];
        if FMode = dbemView then
        begin
          AGraphics.DrawRectangle(r);
          DrawSplittedText(r, str);
        end;
        y := Max(r.Bottom, y);
        x := Max(r.Right, x);
      end;

      if Length(FBuildItem.SubPropertyNamesRects) > 0 then
      begin
        AGraphics.DrawLine(PointF(pnr.Left, pnr.Top + ((pnr.Bottom - pnr.Top) / 2) + Round(sc * 30)), PointF(pnr.Right + Round(sc * 10), pnr.Top + ((pnr.Bottom - pnr.Top) / 2)  + Round(sc * 30)));
        AGraphics.DrawLine(PointF(pnr.Right + Round(sc * 10), pnr.Top + ((pnr.Bottom - pnr.Top) / 2) + Round(sc * 30)), PointF(x, pnr.Top + ((pnr.Bottom - pnr.Top) / 2)  + Round(sc * 30)));
      end;
    end;

    if y = 0 then
      y := Max(fnr.Bottom, pnr.Bottom);

    y := y + Round(sc * 10);

    {$IFNDEF WEBLIB}
    if Assigned(FBuildItem.Item.&Object) then
    begin
      if FBuildItem.Item.&Object is TControl then
      begin
        bmp := FCachedComponentScreenShot;
        if Assigned(bmp) then
        begin
          bmpr := RectF(Round(sc * 5), y, (ARect.Right - ARect.Left) - Round(sc * 5), (ARect.Bottom - ARect.Top) - Round(sc * 5));
          AGraphics.Stroke.Kind := gskDot;
          AGraphics.DrawRectangle(bmpr);
          TTMSFNCUtils.SetFontSize(AGraphics.Font, Round(sc * 10));
          AGraphics.DrawText(PointF(bmpr.Left + Round(sc * 5), bmpr.Top + Round(sc * 5)), 'Live Preview');
          InflateRectEx(bmpr, Round(sc * -5), Round(sc * -20));
          AGraphics.DrawBitmap(bmpr, bmp);
        end;
      end;
    end;
    {$ENDIF}
  end;
end;

{$IFDEF VCLLIB}
{$HINTS OFF}
{$IF COMPILERVERSION >= 33}
procedure TTMSFNCDataBindingEditor.DoAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
begin
  BuildItem(True);
end;
{$ENDIF}
{$HINTS ON}
{$ENDIF}

procedure TTMSFNCDataBindingEditor.DoBindComponentChanged(Sender: TObject);
begin
  if FBlockChange then
    Exit;

  if Assigned(FBuildItem.Item) then
  begin
    FBuildItem.Item.Active := False;
    if (FComboboxBindComponent.ItemIndex >= 0) and (FComboboxBindComponent.ItemIndex <= FComboboxBindComponent.Items.Count - 1) then
      FBuildItem.Item.&Object := FComboboxBindComponent.Items.Objects[FComboboxBindComponent.ItemIndex];

    BuildItem(False);
    FillEditControls(True);
  end;
end;

procedure TTMSFNCDataBindingEditor.DoBindTypeChanged(Sender: TObject);
begin
  if FBlockChange then
    Exit;

  if Assigned(FBuildItem.Item) then
    FBuildItem.Item.BindType := TTMSFNCDataBinderBindType(FComboBoxBindType.ItemIndex);

  BuildItem(False);
  FillEditControls(False);
end;

procedure TTMSFNCDataBindingEditor.DoDataSourceChanged(Sender: TObject);
var
  obj: TObject;
begin
  if FBlockChange then
    Exit;

  if Assigned(FBuildItem.Item) then
  begin
    if (FComboBoxDataSource.ItemIndex >= 0) and (FComboBoxDataSource.ItemIndex <= FComboBoxDataSource.Items.Count - 1) then
    begin
      obj := FComboBoxDataSource.Items.Objects[FComboBoxDataSource.ItemIndex];
      if obj is TDataSource then
        FBuildItem.Item.DataSource := obj as TDataSource
      else
        FBuildItem.Item.DataSource := nil;
    end;
  end;

  BuildItem(False);
  FillEditControls(True);
end;

procedure TTMSFNCDataBindingEditor.DoDelete(Sender: TObject);
var
  io: Integer;
begin
  if (FListBox.LastSelectedItemIndex >= 0) and (FListBox.LastSelectedItemIndex <= FListBox.Items.Count - 1) and ( FListBox.Items[FListBox.LastSelectedItemIndex].Selected = True) then
  begin
    io := FListBox.LastSelectedItemIndex;
    FDataBinder.Items.Delete(FListBox.LastSelectedItemIndex);
    FillControls;
    if FListBox.Items.Count > 0 then
    begin
      if (io - 1 >= 0) then
        FListBox.SelectItem(io - 1)
      else
        FListBox.SelectItem(io);
    end
    else
      FListBox.UnselectAllItems;

    BuildItem(True);
    FillEditControls(True);
  end;
end;

procedure TTMSFNCDataBindingEditor.DoEdit(Sender: TObject);
begin
  case FMode of
    dbemView: FMode := dbemEdit;
    dbemEdit: FMode := dbemView;
  end;
  FillEditControls(True);
end;

procedure TTMSFNCDataBindingEditor.DoFieldNameChanged(Sender: TObject);
var
  s: string;
begin
  if FBlockChange then
    Exit;

  if Assigned(FBuildItem.Item) then
  begin
    s := FComboBoxFieldName.Text;
    if TTMSFNCUtils.IsHTML(s) then
    begin
      FBuildItem.Item.HTMLTemplate := s;
      FBuildItem.Item.FieldName := '';
    end
    else
    begin
      FBuildItem.Item.HTMLTemplate := '';
      FBuildItem.Item.FieldName := s;
    end;

    BuildItem(False);
    FillEditControls(False);
  end;
end;

{$IFNDEF FMXLIB}
procedure TTMSFNCDataBindingEditor.DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{$ENDIF}
{$IFDEF FMXLIB}
procedure TTMSFNCDataBindingEditor.DoFormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
{$ENDIF}
begin
  inherited;

  if Key = KEY_ESCAPE then
  begin
    frm.Close;
  end
  else if Key = KEY_RETURN then
  begin
    frm.ModalResult := mrOk;
    {$IFNDEF FMXLIB}
    frm.Close;
    {$ENDIF}
    {$IFDEF FMXLIB}
    frm.CloseModal;
    {$ENDIF}
  end;
end;

procedure TTMSFNCDataBindingEditor.DoListBoxChange(Sender: TObject);
begin
  BuildItem(True);
  FillEditControls(True);
end;

procedure TTMSFNCDataBindingEditor.DoListBoxItemSelectedChange(Sender: TObject; AIndex: Integer; AItem: TTMSFNCEditorListItem; ASelected: Boolean);
begin
  if Length(FListBox.GetSelectedItems) > 0 then
    FButtonEdit.Enabled := True
  else
  begin
    FMode := dbemView;
    FButtonEdit.Selected := False;
    FButtonEdit.Enabled := False;
  end;

  if ASelected then
    DoListBoxChange(Sender);
end;

procedure TTMSFNCDataBindingEditor.DoPropertyNameChanged(Sender: TObject);
begin
  if FBlockChange then
    Exit;

  if Assigned(FBuildItem.Item) then
  begin
    FBuildItem.Item.PropertyName := FComboBoxPropertyName.Text;
    BuildItem(False);
    FillEditControls(False);
  end;
end;

procedure TTMSFNCDataBindingEditor.DoColumnsPropertyNameChanged(Sender: TObject);
begin
  if FBlockChange then
    Exit;

  if Assigned(FBuildItem.Item) then
  begin
    FBuildItem.Item.ColumnsPropertyName := FComboBoxColumnsPropertyName.Text;
    BuildItem(False);
    FillEditControls(False);
  end;
end;

procedure TTMSFNCDataBindingEditor.DoColumnsSubPropertyNameChanged(Sender: TObject);
begin
  if FBlockChange then
    Exit;

  if Assigned(FBuildItem.Item) then
  begin
    FBuildItem.Item.ColumnsSubPropertyName := FComboBoxColumnsSubPropertyName.Text;
    BuildItem(False);
    FillEditControls(False);
  end;
end;

procedure TTMSFNCDataBindingEditor.DoComboSubFieldChanged(Sender: TObject);
var
  i: Integer;
  s: string;
begin
  if FBlockChange then
    Exit;

  if Assigned(FBuildItem.Item) and (Sender is TTMSFNCDataBindingEditorComboBoxSub) then
  begin
    i := (Sender as TTMSFNCDataBindingEditorComboBoxSub).Tag;
    if (i >= 0) and (i <= FBuildItem.Item.SubFieldNames.Count - 1) then
    begin
      s := (Sender as TTMSFNCDataBindingEditorComboBoxSub).Text;
      if TTMSFNCUtils.IsHTML(s) then
      begin
        FBuildItem.Item.SubFieldNames[i].HTMLTemplate := s;
        FBuildItem.Item.SubFieldNames[i].Value := '';
      end
      else
      begin
        FBuildItem.Item.SubFieldNames[i].HTMLTemplate := '';
        FBuildItem.Item.SubFieldNames[i].Value := s;
      end;
      BuildItem(False);
      FillEditControls(False);
    end;
  end;
end;

procedure TTMSFNCDataBindingEditor.DoComboSubPropertyChanged(Sender: TObject);
var
  i: Integer;
begin
  if FBlockChange then
    Exit;

  if Assigned(FBuildItem.Item) and (Sender is TTMSFNCDataBindingEditorComboBoxSub) then
  begin
    i := (Sender as TTMSFNCDataBindingEditorComboBoxSub).Tag;
    if (i >= 0) and (i <= FBuildItem.Item.SubPropertyNames.Count - 1) then
    begin
      FBuildItem.Item.SubPropertyNames[(Sender as TTMSFNCDataBindingEditorComboBoxSub).Tag].Value := (Sender as TTMSFNCDataBindingEditorComboBoxSub).Text;
      BuildItem(False);
      FillEditControls(False);
    end;
  end;
end;

procedure TTMSFNCDataBindingEditor.DoResizeForm(Sender: TObject);
begin
  if Assigned(FCheckBoxActive) and Assigned(FComboBoxBindType) then
  begin
    FCheckBoxActive.SetBounds(FPreview.Left + FPreview.Width - FCheckBoxActive.Width - 10, FPreview.Top + 10, FCheckBoxActive.Width, FCheckBoxActive.Height);
    {$IFDEF FMXLIB}
    FComboBoxBindType.SetBounds(FCheckBoxActive.Position.X - FComboBoxBindType.Width - 10, FPreview.Top + 10, FComboBoxBindType.Width, FComboBoxBindType.Height);
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FComboBoxBindType.SetBounds(FCheckBoxActive.Left - FComboBoxBindType.Width - 10, FPreview.Top + 10, FComboBoxBindType.Width, FComboBoxBindType.Height);
    {$ENDIF}

    {$IFDEF FMXLIB}
    FComboBoxDataSource.SetBounds(FComboBoxBindType.Position.X - FComboBoxDataSource.Width - 10, FPreview.Top + 10, FComboBoxDataSource.Width, FComboBoxDataSource.Height);
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FComboBoxDataSource.SetBounds(FComboBoxBindType.Left - FComboBoxDataSource.Width - 10, FPreview.Top + 10, FComboBoxDataSource.Width, FComboBoxDataSource.Height);
    {$ENDIF}
  end;
end;

procedure TTMSFNCDataBindingEditor.DoSubAdd(Sender: TObject);
begin
  if Assigned(FBuildItem.Item) then
  begin
    FBuildItem.item.SubFieldNames.Add;
    FBuildItem.item.SubPropertyNames.Add;

    BuildItem(True);
    FillEditControls(True);
  end;
end;

procedure TTMSFNCDataBindingEditor.DoSubDelete(Sender: TObject);
begin
  if Assigned(FBuildItem.Item) then
  begin
    if (FBuildItem.Item.SubFieldNames.Count > 0) then
      FBuildItem.item.SubFieldNames.Delete(FBuildItem.Item.SubFieldNames.Count - 1);

    if (FBuildItem.Item.SubPropertyNames.Count > 0) then
      FBuildItem.item.SubPropertyNames.Delete(FBuildItem.Item.SubPropertyNames.Count - 1);

    BuildItem(True);
    FillEditControls(True);
  end;
end;

{$IFDEF WEBLIB}
procedure TTMSFNCDataBindingEditor.Execute(AProc: TModalResultProc = nil);
{$ELSE}
function TTMSFNCDataBindingEditor.Execute: TModalResult;
{$ENDIF}
begin
  {$IFDEF WEBLIB}
  frm := TTMSFNCDataBindingEditorForm.CreateDialogNew(Self, 'Database Binding Editor');
  {$ELSE}
  frm := TTMSFNCDataBindingEditorForm.CreateNew(Application);
  frm.Caption := 'Database Binding Editor';
  {$ENDIF}
  frm.Width := 1000;
  {$IFDEF WEBLIB}
  frm.Height := 500;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  frm.Height := 600;
  {$ENDIF}
  {$IFDEF CMNLIB}
  frm.KeyPreview := True;
    {$IFDEF VCLLIB}
    {$HINTS OFF}
    {$IF COMPILERVERSION >= 33}
  frm.OnAfterMonitorDpiChanged := DoAfterMonitorDpiChanged;
    {$ENDIF}
    {$HINTS ON}
    {$ENDIF}
  {$ENDIF}
  frm.OnKeyDown := DoFormKeyDown;
  frm.OnResize := {$IFDEF LCLLIB}@{$ENDIF}DoResizeForm;
  {$IFDEF FMXLIB}
  frm.Position := TFormPosition.ScreenCenter;

  frm.Fill.Kind := TBrushKind.Solid;
  if IsLightTheme then
    frm.Fill.Color := EDITORSUBBACKCOLORLIGHT
  else
    frm.Fill.Color := EDITORSUBBACKCOLORDARK;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  frm.Position := poScreenCenter;
  if IsLightTheme then
    frm.Color := EDITORSUBBACKCOLORLIGHT
  else
    frm.Color := EDITORSUBBACKCOLORDARK;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  BuildEditor(frm);
  frm.ActiveControl := FListBox;
  {$ENDIF}
  TTMSFNCUtils.ScaleForCurrentDPI(frm);
  {$IFNDEF WEBLIB}
  Result := frm.ShowModal;
  frm.Free;
  {$ELSE}
  frm.ShowModal(
  procedure(AResult: TModalResult)
  begin
    frm := nil;
    if Assigned(AProc) then
      AProc(AResult);
  end);
  {$ENDIF}
end;

procedure TTMSFNCDataBindingEditor.FillControls;
var
  I: Integer;
  it: TTMSFNCDataBinderItem;
  li: TTMSFNCEditorListItem;
  s: string;
begin
  if Assigned(FDataBinder) then
  begin
    FListBox.Items.Clear;
    FListBox.BeginUpdate;
    for I := 0 to FDataBinder.Items.Count - 1 do
    begin
      it := FDataBinder.Items[I];

      s := it.Name;

      if Assigned(it.&Object) and (s = '') then
      begin
        if (it.&Object is TComponent) and ((it.&Object as TComponent).Name <> '') then
          s := (it.&Object as TComponent).Name
        else
          s := it.&Object.ClassName;
      end;

      li := FListBox.Items.Add;
      li.Name := s;
      li.DataObject := it;
    end;
    FListBox.EndUpdate;
  end;
end;

procedure TTMSFNCDataBindingEditor.FillEditControls(AUpdate: Boolean);
var
  I: Integer;
  c: TComponent;
  str: string;
  idx: Integer;
  slc: TStringList;
  r: TRectF;
  f: TCustomForm;
  cbo: TTMSFNCDataBindingEditorComboBoxSub;

  procedure FillPropertyNames(obj: TObject; pp: string; cbo: TStrings);
  var
    k,j: Integer;
    a, aa: TTMSFNCDataBinderPropertyList;
    oi, o: TObject;
  begin
    a := TTMSFNCDataBinder.GetPropertyList(obj);
    for K := 0 to Length(a) - 1 do
    begin
      if pp <> '' then
      begin
        if TTMSFNCPersistence.GetPropInfoName(a[K]) = pp then
        begin
          if TTMSFNCPersistence.GetPropInfoType(a[K]) = tkClass then
          begin
            o := GetObjectProp(obj, pp);
            if Assigned(o) then
            begin
              if o is TCollection then
              begin
                oi := (o as TCollection).ItemClass.Create(nil);
                try
                  aa := TTMSFNCDataBinder.GetPropertyList(oi);
                finally
                  oi.Free;
                end;
              end
              else
                aa := TTMSFNCDataBinder.GetPropertyList(o);

              for J := 0 to Length(aa) - 1 do
                cbo.Add(TTMSFNCPersistence.GetPropInfoName(aa[J]));
              Break;
            end;
          end;
        end;
      end
      else
        cbo.Add(TTMSFNCPersistence.GetPropInfoName(a[K]));
    end;
  end;

  procedure FillFieldNames(ds: TDataSource; cbo: TStrings);
  begin
    if Assigned(ds) and Assigned(ds.DataSet) then
      ds.DataSet.GetFieldNames(cbo);
  end;

begin
  FBlockChange := True;
  if FMode = dbemEdit then
  begin
    FComboboxBindComponent.Parent := FPreview;

    if FBuildItem.Item.BindType <> dbbtGrid then
    begin
      FComboBoxPropertyName.Parent := FPreview;
      if FBuildItem.Item.BindType in [dbbtSingleValue, dbbtList] then
        FComboBoxFieldName.Parent := FPreview
      else
        FComboBoxFieldName.Parent := nil;

      if FBuildItem.Item.BindType in [dbbtColumnList] then
      begin
        FComboBoxColumnsPropertyName.Parent := FPreview;
        FComboBoxColumnsSubPropertyName.Parent := FPreview;
      end
      else
      begin
        FComboBoxColumnsPropertyName.Parent := nil;
        FComboBoxColumnsSubPropertyName.Parent := nil;
      end;

      if FBuildItem.Item.BindType in [dbbtList, dbbtColumnList] then
      begin
        for I := 0 to Length(FBuildItem.SubFieldNamesCombos) - 1 do
          FBuildItem.SubFieldNamesCombos[I].Parent := FPreview;

        for I := 0 to Length(FBuildItem.SubPropertyNamesCombos) - 1 do
          FBuildItem.SubPropertyNamesCombos[I].Parent := FPreview;
      end;

      if FBuildItem.Item.BindType in [dbbtList, dbbtColumnList] then
      begin
        FSubAdd.Parent := FPreview;
        if (Length(FBuildItem.SubPropertyNamesRects) > 0) or (Length(FBuildItem.SubFieldNamesRects) > 0) then
          FSubDelete.Parent := FPreview
        else
          FSubDelete.Parent := nil;

        if (Length(FBuildItem.SubPropertyNamesRects) > 0) then
          r := FBuildItem.SubPropertyNamesRects[Length(FBuildItem.SubPropertyNamesRects) - 1]
        else if (Length(FBuildItem.SubFieldNamesRects) > 0) then
          r := FBuildItem.SubFieldNamesRects[Length(FBuildItem.SubFieldNamesRects) - 1]
        else
          r := FBuildItem.PropertyNameRect;

        FSubAdd.SetBounds(Round(r.Right + 20), Round(r.Top),
          Round(FSubAdd.Width), Round(FSubAdd.Height));

        FSubDelete.SetBounds(Round(r.Right + 30 + FSubAdd.Width), Round(r.Top),
          Round(FSubDelete.Width), Round(FSubDelete.Height));
      end;
    end;

    if AUpdate then
    begin
      if Assigned(FComboBoxPropertyName.Parent) then
        FComboBoxPropertyName.Items.Clear;
      if Assigned(FComboBoxColumnsPropertyName.Parent) then
        FComboBoxColumnsPropertyName.Items.Clear;
      if Assigned(FComboBoxColumnsSubPropertyName.Parent) then
        FComboBoxColumnsSubPropertyName.Items.Clear;
      if Assigned(FComboBoxFieldName.Parent) then
        FComboBoxFieldName.Items.Clear;
      if Assigned(FComboboxBindComponent.Parent) then
        FComboboxBindComponent.Items.Clear;

      FButtonEdit.Text := sTMSFNCDataBindingEditorCloseEditMode;

      f := TTMSFNCUtils.GetOwnerForm(Self);

      if Assigned(f) then
      begin
        idx := -1;
        if Assigned(FComboboxBindComponent.Parent) then
          FComboboxBindComponent.Items.Add('');

        for I := 0 to f.ComponentCount - 1 do
        begin
          c := f.Components[I];
          if (c.Name <> '') then
            str := c.Name
          else
            str := c.ClassName;

          if Assigned(FComboboxBindComponent.Parent) then
            FComboboxBindComponent.Items.AddObject(str, c);

          if c = FBuildItem.Item.BindComponent then
            idx := I;
        end;

        if Assigned(FComboboxBindComponent.Parent) then
          FComboboxBindComponent.ItemIndex := idx + 1;
      end;

      slc := TStringList.Create;
      try
        FillPropertyNames(FBuildItem.Item.BindComponent, '', slc);
        slc.Insert(0, '');
        if Assigned(FComboBoxPropertyName.Parent) then
          FComboBoxPropertyName.Items.Assign(slc);

        if Assigned(FComboBoxColumnsPropertyName.Parent) then
          FComboBoxColumnsPropertyName.Items.Assign(slc);

        slc.Clear;

        FillPropertyNames(FBuildItem.Item.BindComponent, FBuildItem.Item.PropertyName, slc);

        slc.Insert(0, '');

        for I := 0 to Length(FBuildItem.SubPropertyNamesCombos) - 1 do
        begin
          if Assigned(FBuildItem.SubPropertyNamesCombos[I].Parent) then
          begin
            FBuildItem.SubPropertyNamesCombos[I].Items.Assign(slc);
            FBuildItem.SubPropertyNamesCombos[I].Text := FBuildItem.Item.SubPropertyNames[I].Value;
          end;
        end;

        slc.Clear;
        FillPropertyNames(FBuildItem.Item.BindComponent, FBuildItem.Item.ColumnsPropertyName, slc);
        slc.Insert(0, '');

        if Assigned(FComboBoxColumnsSubPropertyName.Parent) then
          FComboBoxColumnsSubPropertyName.Items.Assign(slc);

        slc.Clear;
        FillFieldNames(FBuildItem.Item.DataSource, slc);

        for I := 0 to Length(FBuildItem.SubFieldNamesCombos) - 1 do
        begin
          if Assigned(FBuildItem.SubFieldNamesCombos[I].Parent) then
          begin
            FBuildItem.SubFieldNamesCombos[I].Items.Assign(slc);
            if FBuildItem.Item.SubFieldNames[I].HTMLTemplate <> '' then
              FBuildItem.SubFieldNamesCombos[I].Text := FBuildItem.Item.SubFieldNames[I].HTMLTemplate
            else
              FBuildItem.SubFieldNamesCombos[I].Text := FBuildItem.Item.SubFieldNames[I].Value
          end;
        end;

        slc.Insert(0, '');
        if Assigned(FComboBoxFieldName.Parent) then
          FComboBoxFieldName.Items.Assign(slc);
      finally
        slc.free;
      end;
    end;

    if Assigned(FComboBoxColumnsPropertyName.Parent) then
      FComboBoxColumnsPropertyName.Text := FBuildItem.Item.ColumnsPropertyName;

    if Assigned(FComboBoxColumnsSubPropertyName.Parent) then
      FComboBoxColumnsSubPropertyName.Text := FBuildItem.Item.ColumnsSubPropertyName;


    if Assigned(FComboBoxPropertyName.Parent) then
      FComboBoxPropertyName.Text := FBuildItem.Item.PropertyName;

    if Assigned(FComboBoxFieldName.Parent) then
    begin
      if FBuildItem.Item.HTMLTemplate <> '' then
        FComboBoxFieldName.Text := FBuildItem.Item.HTMLTemplate
      else
        FComboBoxFieldName.Text := FBuildItem.Item.FieldName;
    end;

    FComboboxBindComponent.SetBounds(Round(FBuildItem.BindComponentRect.Left), Round(FBuildItem.BindComponentRect.Top),
      Round(FBuildItem.BindComponentRect.Right - FBuildItem.BindComponentRect.Left), Round(FBuildItem.BindComponentRect.Bottom - FBuildItem.BindComponentRect.Top));

    FComboboxFieldName.SetBounds(Round(FBuildItem.FieldNameRect.Left), Round(FBuildItem.FieldNameRect.Top),
      Round(FBuildItem.FieldNameRect.Right - FBuildItem.FieldNameRect.Left), Round(FBuildItem.FieldNameRect.Bottom - FBuildItem.FieldNameRect.Top));

    FComboboxPropertyName.SetBounds(Round(FBuildItem.PropertyNameRect.Left), Round(FBuildItem.PropertyNameRect.Top),
      Round(FBuildItem.PropertyNameRect.Right - FBuildItem.PropertyNameRect.Left), Round(FBuildItem.PropertyNameRect.Bottom - FBuildItem.PropertyNameRect.Top));

    FComboboxColumnsPropertyName.SetBounds(Round(FBuildItem.ColumnsPropertyNameRect.Left), Round(FBuildItem.ColumnsPropertyNameRect.Top),
      Round(FBuildItem.ColumnsPropertyNameRect.Right - FBuildItem.ColumnsPropertyNameRect.Left), Round(FBuildItem.ColumnsPropertyNameRect.Bottom - FBuildItem.ColumnsPropertyNameRect.Top));

    FComboboxColumnsSubPropertyName.SetBounds(Round(FBuildItem.ColumnsSubPropertyNameRect.Left), Round(FBuildItem.ColumnsSubPropertyNameRect.Top),
      Round(FBuildItem.ColumnsSubPropertyNameRect.Right - FBuildItem.ColumnsSubPropertyNameRect.Left), Round(FBuildItem.ColumnsSubPropertyNameRect.Bottom - FBuildItem.ColumnsSubPropertyNameRect.Top));
  end
  else
  begin
    FButtonEdit.Text := sTMSFNCDataBindingEditorEditMode;
    FComboboxBindComponent.Parent := nil;
    FComboBoxFieldName.Parent := nil;
    FComboBoxPropertyName.Parent := nil;
    FComboBoxColumnsPropertyName.Parent := nil;
    FComboBoxColumnsSubPropertyName.Parent := nil;
    FSubAdd.Parent := nil;
    FSubDelete.Parent := nil;

    for I := FPreview.ComponentCount - 1 downto 0 do
    begin
      if FPreview.Components[I] is TTMSFNCDataBindingEditorComboBoxSub then
      begin
        cbo := TTMSFNCDataBindingEditorComboBoxSub(FPreview.Components[I]);
        cbo.Parent := nil;
      end;
    end;
  end;
  FBlockChange := False;
end;

function TTMSFNCDataBindingEditor.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

{$IFDEF WEBLIB}
procedure TTMSFNCDataBindingEditor.DoScreenShot(Sender: TObject; ABitmap: TBitmap);
begin
  if Sender is TCustomControl then
    (Sender as TCustomControl).OnScreenShot := FComponentEvent;

  if Assigned(FPreview) then
    FPreview.Repaint;
end;
{$ENDIF}

procedure TTMSFNCDataBindingEditor.MakeComponentScreenShot;
begin
  if Assigned(FCachedComponentScreenShot) then
  begin
    FCachedComponentScreenShot.Free;
    FCachedComponentScreenShot := nil;
  end;

  if Assigned(FBuildItem.Item) and Assigned(FBuildItem.Item.&Object) and (FBuildItem.Item.&Object is TControl) then
  begin
    {$IFDEF WEBLIB}
    if FBuildItem.Item.&Object is TCustomControl then
    begin
      FComponentEvent := (FBuildItem.Item.&Object as TCustomControl).OnScreenShot;
      (FBuildItem.Item.&Object as TCustomControl).OnScreenShot := DoScreenShot;
    end;
    {$ENDIF}
    FCachedComponentScreenShot := TTMSFNCBitmap((FBuildItem.Item.&Object as TControl).MakeScreenshot);
  end;
end;

{$IFDEF WEBLIB}

{ TTMSFNCDataBindingEditorForm }

constructor TTMSFNCDataBindingEditorForm.CreateDialogNew(ADataBindingEditor: TTMSFNCDataBindingEditor; ACaption: string);
begin
  FDataBindingEditor := ADataBindingEditor;
  inherited CreateDialogNew(ACaption);
end;

procedure TTMSFNCDataBindingEditorForm.LoadDFMValues;
begin
  inherited LoadDFMValues;
  if Assigned(FDataBindingEditor) then
  begin
    FDataBindingEditor.BuildEditor(Self);
    ActiveControl := FDataBindingEditor.FTopPanel;
  end;
end;

procedure TTMSFNCDataBindingEditorForm.Loaded;
var
  css: string;
begin
  inherited;

  css := Application.IDECSS;

  if (css <> '') then
  begin
    AddCSS('IDECSS', css);
    ElementClassName := 'IDEBkg';
    CaptionElement['class'] := 'IDECaption IDEFont';
    if Assigned(FDataBindingEditor) then
      FDataBindingEditor.ApplyCSS(css);
  end;
end;

{$ENDIF}

end.

