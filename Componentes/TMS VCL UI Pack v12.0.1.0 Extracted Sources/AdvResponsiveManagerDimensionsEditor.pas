{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2022                                      }
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

unit AdvResponsiveManagerDimensionsEditor;

{$I TMSDEFS.INC}

{$IFDEF FMXLIB}
{$WARNINGS OFF}
{$HINTS OFF}
{$IF COMPILERVERSION > 30}
{$DEFINE DELPHIBERLIN}
{$IFEND}
{$HINTS ON}
{$WARNINGS ON}
{$ENDIF}

{$IFDEF VCLLIB}
{$DEFINE VCLWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$DEFINE VCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  Registry,
  {$ENDIF}
  Classes, AdvCustomComponent, AdvCustomControl, Controls,
  AdvTypes, StdCtrls, ExtCtrls, Forms, AdvResponsiveManager,
  AdvEditorButton, AdvEditorPanel, AdvEditorListView
  {$IFDEF FMXLIB}
  ,FMX.Types, FMX.Memo, FMX.Edit, FMX.ListBox, FMX.ComboEdit, FMX.Layouts
  {$ENDIF}
  {$IFNDEF LCLLIB}
  ,UITypes, Types
  {$ENDIF}
  {$IFNDEF LCLLIB}
  ,Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First Release

resourcestring
  sAdvResponsiveManagerDimensionsEditorOK = 'OK';
  sAdvResponsiveManagerDimensionsEditorCancel = 'Cancel';
  sAdvResponsiveManagerDimensionsEditorDelete = '&Delete';
  sAdvResponsiveManagerDimensionsEditorAdd = '&Add';
  sAdvResponsiveManagerDimensionsEditorUpdate = '&Update';
  sAdvResponsiveManagerDimensionsEditorNew = '&New';
  sAdvResponsiveManagerDimensionsEditorTitle = 'Title';
  sAdvResponsiveManagerDimensionsEditorWidth = 'Width';
  sAdvResponsiveManagerDimensionsEditorHeight = 'Height';
  sAdvResponsiveManagerDimensionsEditorType = 'Type';

type
  TAdvResponsiveManagerDimensionsEditor = class;

  {$IFDEF FMXLIB}
  TAdvResponsiveManagerDimensionsEditorParent = TFmxObject;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  TAdvResponsiveManagerDimensionsEditorParent = TWinControl;
  {$ENDIF}

  TAdvResponsiveManagerDimensionsEditorType = (edtDesktop, edtPhone, edtTablet);

  TAdvResponsiveManagerDimensionsEditorDimension = class(TPersistent)
  private
    FWidth: Integer;
    FTitle: string;
    FHeight: Integer;
    FDefault: Boolean;
    FType: TAdvResponsiveManagerDimensionsEditorType;
    FResource: Boolean;
  public
    property Resource: Boolean read FResource write FResource;
    constructor Create; virtual;
  published
    property Title: string read FTitle write FTitle;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property &Default: Boolean read FDefault write FDefault default True;
    property &Type: TAdvResponsiveManagerDimensionsEditorType read FType write FType;
  end;

  TAdvResponsiveManagerDimensionsEditorDimensions = class(TObjectList<TAdvResponsiveManagerDimensionsEditorDimension>);

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsDesktop)]
  {$ENDIF}
  TAdvResponsiveManagerDimensionsEditor = class(TAdvCustomComponent)
  private
    frm: TAdvCustomDesignerForm;
    FDimensions: TAdvResponsiveManagerDimensionsEditorDimensions;
    FPanel: TAdvEditorPanel;
    FButtonOk, FButtonNew, FButtonDelete, FButtonAdd: TAdvEditorButton;
    FLblTitle, FLblWidth, FLblHeight, FLblType: TLabel;
    FEdTitle, FEdWidth, FEdHeight: TEdit;
    FCbType: TComboBox;
    FListBox: TAdvEditorList;
    {$IFNDEF FMXLIB}
    FVScrlBox: TScrollBox;
    {$ENDIF}
    {$IFDEF FMXLIB}
    FVScrlBox: TVertScrollBox;
    {$ENDIF}
  protected
    function GetInstance: NativeUInt; override;
    procedure LoadDimensions;
    procedure DoListClick(Sender: TObject; AIndex: Integer; AItem: TAdvEditorListItem; ASelected: Boolean);
    procedure DoDelete(Sender: TObject);
    procedure DoAdd(Sender: TObject);
    procedure DoNew(Sender: TObject);
    procedure DoFormResize(Sender: TObject);
    procedure RegisterRuntimeClasses; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    {$IFDEF WEBLIB}
    procedure DoButtonOKClick(Sender: TObject); virtual;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    procedure DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    {$ENDIF}
    {$IFDEF FMXLIB}
    procedure DoFormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    {$ENDIF}
    procedure BuildEditor(AParent: TAdvResponsiveManagerDimensionsEditorParent); virtual;
    procedure DoListBoxResize(Sender: TObject);
    procedure RealignControls;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF WEBLIB}
    procedure Execute(AProc: TModalResultProc = nil);
    {$ELSE}
    function Execute: TModalResult;
    {$ENDIF}
    procedure Assign(Source: TPersistent); override;
  end;

  TAdvResponsiveManagerDimensionsEditorForm = class(TAdvCustomDesignerForm)
  {$IFDEF WEBLIB}
  private
    FResponsiveManagerEditor: TAdvResponsiveManagerDimensionsEditor;
  protected
    procedure Loaded; override;
    procedure LoadDFMValues; override;
  public
    constructor CreateDialogNew(AResponsiveManagerEditor: TAdvResponsiveManagerDimensionsEditor; ACaption: string); reintroduce; virtual;
  {$ENDIF}
  end;

implementation

uses
  AdvUtils, Graphics, SysUtils, VCL.Dialogs,
  AdvEditorsTools, AdvGraphicsTypes, Math
  {$IFDEF FMXLIB}
  ,JSON
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,JSON
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fpjson
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,WEBLib.WEBTools, WEBLib.JSON
  {$ENDIF}
  ;

{$IFDEF FNCLIB}
{$IFNDEF VCLLIB}
{$R 'AdvResponsiveManagerResources.res'}
{$ENDIF}
{$ELSE}
{$R 'AdvResponsiveManagerResources.res'}
{$ENDIF}

type
  {$IFNDEF WEBLIB}
  TAdvResponsiveManagerDimensionsEditorOpenDialog = TOpenDialog;
  {$ENDIF}
  {$IFDEF WEBLIB}
  TAdvResponsiveManagerDimensionsEditorOpenDialog = TWebOpenDialog;
  {$ENDIF}

{ TAdvResponsiveManagerDimensionsEditor }

function TranslateTextEx(AText: String): String;
begin
  {$IFDEF FMXLIB}
  Result := TranslateText(AText);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  Result := AText;
  {$ENDIF}
end;

procedure TAdvResponsiveManagerDimensionsEditor.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
end;

procedure TAdvResponsiveManagerDimensionsEditor.RealignControls;
begin
  //TODO
//  {$IFDEF FMXLIB}
//  FButtonDelete.Position.Y := FVisualizer.Position.Y + FVisualizer.Height + 5;
//  FButtonDelete.Position.X := FButtonApply.Width + FButtonCombine.Width + 20;
//  {$ENDIF}
//  {$IFDEF CMNWEBLIB}
//  FButtonDelete.Top := FVisualizer.Top + FVisualizer.Height + 5;
//  FButtonDelete.Left := FButtonApply.Width + FButtonCombine.Width + 20;
//  {$ENDIF}

  FListBox.UpdateList;
  FListBox.Height := Max(FListBox.GetListHeight, FVScrlBox.Height);
end;

procedure TAdvResponsiveManagerDimensionsEditor.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TAdvResponsiveManagerDimensionsEditor);
end;

procedure TAdvResponsiveManagerDimensionsEditor.Assign(Source: TPersistent);
begin
end;

procedure TAdvResponsiveManagerDimensionsEditor.BuildEditor(AParent: TAdvResponsiveManagerDimensionsEditorParent);
var
  I: Integer;
  f: string;
  it: TAdvResponsiveManagerDimensionsEditorDimension;
  li: TAdvEditorListItem;
  {$IFDEF MSWINDOWS}
  fn: string;
  RegIniFile: TRegIniFile;
  {$ENDIF}
  sl: TStringList;
begin
  if Assigned(AParent) then
  begin
    {$IFDEF FMXLIB}
    FVScrlBox := TVertScrollBox.Create(AParent);
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FVScrlBox := TScrollBox.Create(AParent);
    FVScrlBox.Top := 0;
    {$IFDEF LCLLIB}
    FVScrlBox.HorzScrollBar.Visible:= False;
    {$ENDIF}
    {$ENDIF}
    FVScrlBox.Parent := AParent;
    {$IFDEF CMNWEBLIB}
    FVScrlBox.BorderStyle := bsNone;
    if IsLightTheme then
      FVScrlBox.Color := EDITORSUBBACKCOLORLIGHT
    else
      FVScrlBox.Color := EDITORSUBBACKCOLORDARK;
    {$ENDIF}
    FVScrlBox.Width := 250;

    FListBox := TAdvEditorList.Create(FVScrlBox);
    FListBox.MultiSelect := False;
    FListBox.CanUnselectItems := False;

    FListBox.Parent := FVScrlBox;
    {$IFDEF FMXLIB}
    FListBox.Align := TAlignLayout.Top;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FListBox.Align := alTop;
    {$ENDIF}
    FListBox.DefaultItemHeight := 28;
    FListBox.ItemsReadOnly := True;
    SetEditorListAppearance(FListBox);
    FListBox.OnItemSelectedChanged := {$IFDEF LCLLIB}@{$ENDIF}DoListClick;

    sl := TStringList.Create;
    try
//      LoadDimensions;

      FListBox.BeginUpdate;
      for I := 0 to FDimensions.Count - 1 do
      begin
        it := FDimensions[I];
        li := FListBox.Items.Add;
        li.Name := it.Title;
        li.DataObject := it;
      end;
      FListBox.EndUpdate;

      {$IFDEF MSWINDOWS}
      sl.Clear;
      RegIniFile := TRegIniFile.Create('AdvResponsiveManagerDimensions');
      try
        RegIniFile.ReadSectionValues('', sl);
        FListBox.BeginUpdate;
        for I := 0 to sl.Count - 1 do
        begin
          f := sl.Names[I];
          fn := sl.Values[f];
          it := TAdvResponsiveManagerDimensionsEditorDimension.Create;
          it.Resource := False;
          it.FromJSON(fn);

          FDimensions.Add(it);
          li := FListBox.Items.Add;
          li.Name := it.Title;

          li.DataObject := it;
        end;
        FListBox.EndUpdate;
      finally
        RegIniFile.Free;
      end;
      {$ENDIF}
    finally
      sl.Free;
    end;

    FListBox.Height := Max(FListBox.GetListHeight, FVScrlBox.Height);

    FListBox.Appearance.Stroke.Kind := gskSolid;
    FListBox.Appearance.Stroke.Color := gcGray;

    {$IFDEF FMXLIB}
    FVScrlBox.Align := TAlignLayout.Left;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FVScrlBox.Align := alLeft;
    {$ENDIF}

    FPanel := TAdvEditorPanel.Create(AParent);
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

    FLblTitle := TLabel.Create(AParent);
    FLblTitle.Parent := AParent;
    {$IFDEF FMXLIB}
    FLblTitle.Position.Y := 15;
    FLblTitle.Position.X := FVScrlBox.Width + 10;
    FLblTitle.Text := TranslateTextEx(sAdvResponsiveManagerDimensionsEditorTitle) + ':';
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FLblTitle.Top := 15;
    FLblTitle.Left := FVScrlBox.Width + 10;
    FLblTitle.Caption := TranslateTextEx(sAdvResponsiveManagerDimensionsEditorTitle) + ':';
    {$ENDIF}
    FLblTitle.AutoSize := True;
    SetEditorLabelAppearance(FLblTitle);

    FLblWidth := TLabel.Create(AParent);
    FLblWidth.Parent := AParent;
    {$IFDEF FMXLIB}
    FLblWidth.Position.Y := FLblTitle.Position.Y + FLblTitle.Height + 20;
    FLblWidth.Position.X := FVScrlBox.Width + 10;
    FLblWidth.Text := TranslateTextEx(sAdvResponsiveManagerDimensionsEditorWidth) + ':';
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FLblWidth.Top := FLblTitle.Top + FLblTitle.Height + 20;
    FLblWidth.Left := FVScrlBox.Width + 10;
    FLblWidth.Caption := TranslateTextEx(sAdvResponsiveManagerDimensionsEditorWidth) + ':';
    {$ENDIF}
    FLblWidth.AutoSize := True;
    SetEditorLabelAppearance(FLblWidth);

    FLblHeight := TLabel.Create(AParent);
    FLblHeight.Parent := AParent;
    {$IFDEF FMXLIB}
    FLblHeight.Position.Y := FLblWidth.Position.Y + FLblWidth.Height + 20;
    FLblHeight.Position.X := FVScrlBox.Width + 10;
    FLblHeight.Text := TranslateTextEx(sAdvResponsiveManagerDimensionsEditorHeight) + ':';
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FLblHeight.Top := FLblWidth.Top + FLblWidth.Height + 20;
    FLblHeight.Left := FVScrlBox.Width + 10;
    FLblHeight.Caption := TranslateTextEx(sAdvResponsiveManagerDimensionsEditorHeight) + ':';
    {$ENDIF}
    FLblHeight.AutoSize := True;
    SetEditorLabelAppearance(FLblHeight);

    FLblType := TLabel.Create(AParent);
    FLblType.Parent := AParent;
    {$IFDEF FMXLIB}
    FLblType.Position.Y := FLblHeight.Position.Y + FLblHeight.Height + 20;
    FLblType.Position.X := FVScrlBox.Width + 10;
    FLblType.Text := TranslateTextEx(sAdvResponsiveManagerDimensionsEditorType) + ':';
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FLblType.Top := FLblHeight.Top + FLblHeight.Height + 20;
    FLblType.Left := FVScrlBox.Width + 10;
    FLblType.Caption := TranslateTextEx(sAdvResponsiveManagerDimensionsEditorType) + ':';
    {$ENDIF}
    FLblType.AutoSize := True;
    SetEditorLabelAppearance(FLblType);

    FEdTitle := TEdit.Create(AParent);
    FEdTitle.Parent := AParent;
    {$IFDEF FMXLIB}
    FEdTitle.Position.Y := 15;
    FEdTitle.Position.X := FVScrlBox.Width + 100;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FEdTitle.Top := 15;
    FEdTitle.Left := FVScrlBox.Width + 100;
    {$ENDIF}
    FEdTitle.Width := 200;

    FEdWidth := TEdit.Create(AParent);
    FEdWidth.Parent := AParent;
    {$IFDEF FMXLIB}
    FEdWidth.FilterChar := '0123456789';
    FEdWidth.Position.Y := FLblTitle.Position.Y + FLblTitle.Height + 20;
    FEdWidth.Position.X := FVScrlBox.Width + 100;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    {$IFDEF VCLLIB}
    FEdWidth.NumbersOnly := True;
    {$ENDIF}
    FEdWidth.Top := FLblTitle.Top + FLblTitle.Height + 20;
    FEdWidth.Left := FVScrlBox.Width + 100;
    {$ENDIF}
    FEdWidth.Width := 200;

    FEdHeight := TEdit.Create(AParent);
    FEdHeight.Parent := AParent;
    {$IFDEF FMXLIB}
    FEdWidth.FilterChar := '0123456789';
    FEdHeight.Position.Y := FLblWidth.Position.Y + FLblWidth.Height + 20;
    FEdHeight.Position.X := FVScrlBox.Width + 100;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    {$IFDEF VCLLIB}
    FEdHeight.NumbersOnly := True;
    {$ENDIF}
    FEdHeight.Top := FLblWidth.Top + FLblWidth.Height + 20;
    FEdHeight.Left := FVScrlBox.Width + 100;
    {$ENDIF}
    FEdHeight.Width := 200;

    FCbType := TComboBox.Create(AParent);
    FCbType.Parent := AParent;
    {$IFDEF FMXLIB}
    FCbType.Position.Y := FLblHeight.Position.Y + FLblHeight.Height + 20;
    FCbType.Position.X := FVScrlBox.Width + 100;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FCbType.Top := FLblHeight.Top + FLblHeight.Height + 20;
    FCbType.Left := FVScrlBox.Width + 100;
    FCbType.Style := csDropDownList;
    {$ENDIF}
    FCbType.Width := 150;

    FCbType.Items.Add('Desktop');
    FCbType.Items.Add('Phone');
    FCbType.Items.Add('Tablet');

    FCbType.ItemIndex := 0;

    FButtonAdd := TAdvEditorButton.Create(AParent);
    FButtonAdd.Parent := AParent;
    FButtonAdd.Text := TranslateTextEx(sAdvResponsiveManagerDimensionsEditorAdd);
    {$IFDEF FMXLIB}
    FButtonAdd.Position.Y := FCbType.Position.Y + FCbType.Height + 20;
    FButtonAdd.Position.X := FCbType.Position.X;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonAdd.Top := FCbType.Top + FCbType.Height + 20;
    FButtonAdd.Left := FCbType.Left;
    {$ENDIF}
    FButtonAdd.OnClick := DoAdd;
    FButtonAdd.Height := 27;
    SetEditorCancelButtonAppearance(FButtonAdd);

    FButtonNew := TAdvEditorButton.Create(AParent);
    FButtonNew.Parent := AParent;
    FButtonNew.Text := TranslateTextEx(sAdvResponsiveManagerDimensionsEditorNew);
    {$IFDEF FMXLIB}
    FButtonNew.Position.Y := FButtonAdd.Position.Y + FButtonAdd.Height + 10;
    FButtonNew.Position.X := FButtonAdd.Position.X;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonNew.Top := FButtonAdd.Top + FButtonAdd.Height + 10;
    FButtonNew.Left := FButtonAdd.Left;
    {$ENDIF}
    FButtonNew.OnClick := DoNew;
    FButtonNew.Height := 27;
    SetEditorCancelButtonAppearance(FButtonNew);


    FButtonDelete := TAdvEditorButton.Create(AParent);
    FButtonDelete.Parent := AParent;
    FButtonDelete.Text := TranslateTextEx(sAdvResponsiveManagerDimensionsEditorDelete);
    {$IFDEF FMXLIB}
    FButtonDelete.Position.Y := FButtonAdd.Position.Y;
    FButtonDelete.Position.X := FButtonAdd.Position.X + FButtonAdd.Width + 10;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonDelete.Top := FButtonAdd.Top;
    FButtonDelete.Left := FButtonAdd.Left + FButtonAdd.Width + 10;
    {$ENDIF}
    FButtonDelete.OnClick := DoDelete;
    FButtonDelete.Enabled := False;
    FButtonDelete.Height := 27;
    SetEditorCancelButtonAppearance(FButtonDelete);

    FButtonOK := TAdvEditorButton.Create(FPanel);
    FButtonOK.ModalResult := mrOk;
//    FButtonOk.Default := True;
    FButtonOK.Parent := FPanel;
    FButtonOK.Text := TranslateTextEx(sAdvResponsiveManagerDimensionsEditorOK);
    {$IFDEF FMXLIB}
    FButtonOk.Align := TAlignLayout.Right;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonOk.Align := alRight;
    {$IFDEF VCLWEBLIB}
    FButtonOK.AlignWithMargins := True;
    {$ENDIF}
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FButtonOK.Margins.Right := 5;
    FButtonOK.Margins.Top := 5;
    FButtonOK.Margins.Bottom := 5;
    FButtonOK.Margins.Left := 5;
    {$ENDIF}
    {$IFDEF LCLLIB}
    FButtonOK.BorderSpacing.Right := 5;
    FButtonOK.BorderSpacing.Top := 5;
    FButtonOK.BorderSpacing.Bottom := 5;
    FButtonOK.BorderSpacing.Left := 5;
    {$ENDIF}
    {$IFDEF WEBLIB}
    FButtonOK.OnClick := DoButtonOKClick;
    {$ENDIF}
    SetEditorOKButtonAppearance(FButtonOk);

  end;
end;

{$IFDEF WEBLIB}

procedure TAdvResponsiveManagerDimensionsEditor.DoButtonOKClick(Sender: TObject);
begin
  if Assigned(frm) then
    frm.ModalResult := mrOK;
end;
{$ENDIF}

constructor TAdvResponsiveManagerDimensionsEditor.Create(AOwner: TComponent);
begin
  inherited;
  FDimensions := TAdvResponsiveManagerDimensionsEditorDimensions.Create;
end;

destructor TAdvResponsiveManagerDimensionsEditor.Destroy;
begin
  FreeAndNil(FDimensions);
  inherited;
end;

procedure TAdvResponsiveManagerDimensionsEditor.DoAdd(Sender: TObject);
var
  it: TAdvResponsiveManagerDimensionsEditorDimension;
  li: TAdvEditorListItem;
  {$IFDEF WEBLIB}
  res: boolean;
  str: string;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  RegIniFile: TRegIniFile;
  {$ENDIF}
  w, h: Integer;
begin
  if FEdTitle.Text = '' then
    raise Exception.Create('Please enter a valid title');

  {$IFDEF MSWINDOWS}
  RegIniFile := TRegIniFile.Create('AdvResponsiveManagerDimensions');
  {$ENDIF}
  try
    FListBox.BeginUpdate;
    if (FListBox.LastSelectedItemIndex >= 0) and (FListBox.LastSelectedItemIndex >= FListBox.Items.Count - 1) then
    begin
      it := TAdvResponsiveManagerDimensionsEditorDimension(FListBox.Items[FListBox.LastSelectedItemIndex].DataObject);
      li := FListBox.Items[FListBox.LastSelectedItemIndex];
    end
    else
    begin
      it := TAdvResponsiveManagerDimensionsEditorDimension.Create;
      FDimensions.Add(it);
      li := FListBox.Items.Add;
    end;

    it.Title := FEdTitle.Text;

    if TryStrToInt(FEdWidth.Text, w) then
      it.Width := w;

    if TryStrToInt(FEdHeight.Text, h) then
      it.Height := h;

    it.&Type := TAdvResponsiveManagerDimensionsEditorType(FCbType.ItemIndex);
    it.Resource := False;

    li.Name := it.Title;
    li.DataObject := it;
    {$IFDEF MSWINDOWS}
    RegIniFile.WriteString('', it.Title, it.ToJSON);
    {$ENDIF}
    FListBox.EndUpdate;

    FListBox.SelectItem(FListBox.Items.Count - 1);
  finally
    {$IFDEF MSWINDOWS}
    RegIniFile.Free;
    {$ENDIF}
  end;
end;

procedure TAdvResponsiveManagerDimensionsEditor.DoDelete(Sender: TObject);
var
  I: Integer;
  it: TAdvResponsiveManagerDimensionsEditorDimension;
  {$IFDEF MSWINDOWS}
  RegIniFile: TRegIniFile;
  {$ENDIF}
  li: Integer;
begin
  {$IFDEF MSWINDOWS}
  RegIniFile := TRegIniFile.Create('AdvResponsiveManagerDimensions');
  {$ENDIF}
  try
    FListBox.BeginUpdate;
    for I := FListBox.Items.Count - 1 downto 0 do
    begin
      if FListBox.Items[I].Selected then
      begin
        it := TAdvResponsiveManagerDimensionsEditorDimension(FListBox.Items[I].DataObject);
        if not it.Resource then
        begin
          FListBox.Items.Delete(I);
          {$IFDEF MSWINDOWS}
          if RegIniFile.ValueExists(it.Title) then
            RegIniFile.DeleteValue(it.Title);
          {$ENDIF}
          FDimensions.Remove(it);
        end;
      end;
    end;
    FListBox.EndUpdate;

    li := FListBox.LastSelectedItemIndex;

    if FListBox.Items.Count > 0 then
    begin
      if li > FListBox.Items.Count - 1 then
        li := FListBox.Items.Count - 1;
    end;

    if (li >= 0) and (li <= FListBox.Items.Count - 1) then
      FListBox.SelectItem(li)
    else
    begin
      FListBox.SelectItem(-1);
      FEdTitle.Text := '';
      FEdWidth.Text := '';
      FButtonDelete.Enabled := False;
      FEdHeight.Text := '';
      FButtonAdd.Text := TranslateTextEx(sAdvResponsiveManagerDimensionsEditorAdd);
      FCbType.ItemIndex := 0;
    end;
  finally
    {$IFDEF MSWINDOWS}
    RegIniFile.Free;
    {$ENDIF}
  end;

  FListBox.Height := Max(FListBox.GetListHeight, FVScrlBox.Height);
end;

{$IFNDEF FMXLIB}
procedure TAdvResponsiveManagerDimensionsEditor.DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{$ENDIF}
{$IFDEF FMXLIB}
procedure TAdvResponsiveManagerDimensionsEditor.DoFormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
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

procedure TAdvResponsiveManagerDimensionsEditor.DoFormResize(Sender: TObject);
begin
  RealignControls;
end;

procedure TAdvResponsiveManagerDimensionsEditor.DoListBoxResize(Sender: TObject);
begin
  RealignControls;
end;

procedure TAdvResponsiveManagerDimensionsEditor.DoListClick(Sender: TObject; AIndex: Integer; AItem: TAdvEditorListItem; ASelected: Boolean);
var
  it: TAdvResponsiveManagerDimensionsEditorDimension;
begin
  if Assigned(AItem.DataObject) then
  begin
    it := TAdvResponsiveManagerDimensionsEditorDimension(AItem.DataObject);
    FEdTitle.Text := it.Title;
    FEdWidth.Text := IntToStr(it.Width);
    FEdHeight.Text := IntToStr(it.Height);
    FCbType.ItemIndex := Integer(it.&Type);
    FButtonAdd.Text := TranslateTextEx(sAdvResponsiveManagerDimensionsEditorUpdate);
    FButtonDelete.Enabled := True;
  end;
end;

procedure TAdvResponsiveManagerDimensionsEditor.DoNew(Sender: TObject);
begin
  FButtonDelete.Enabled := False;
  FButtonAdd.Text := TranslateTextEx(sAdvResponsiveManagerDimensionsEditorAdd);
  FEdTitle.Text := '';
  FEdHeight.Text := '';
  FEdWidth.Text := '';
  FCbType.ItemIndex := 0;
  FListBox.SelectItem(-1);
  FEdTitle.SetFocus;
end;

{$IFDEF WEBLIB}
procedure TAdvResponsiveManagerDimensionsEditor.Execute(AProc: TModalResultProc = nil);
{$ELSE}
function TAdvResponsiveManagerDimensionsEditor.Execute: TModalResult;
{$ENDIF}
begin
  {$IFDEF WEBLIB}
  if Assigned(VSIDE) then
  begin
    asm
      pas["WEBLib.Forms"].VSIDE.setDocument();
    end;
  end;
  frm := TAdvResponsiveManagerDimensionsEditorForm.CreateDialogNew(Self, 'Responsive Manager Dimensions');
  {$ELSE}
  frm := TAdvResponsiveManagerDimensionsEditorForm.CreateNew(Application);
  frm.Caption := 'Responsive Manager Dimensions';
  {$ENDIF}
  frm.Width := 600;
  frm.Height := 400;
  {$IFDEF CMNLIB}
  frm.KeyPreview := True;
  frm.BorderStyle := bsSingle;
  frm.BorderIcons := [TBorderIcon.biSystemMenu];
  {$ENDIF}
  frm.OnKeyDown := DoFormKeyDown;
  {$IFDEF FMXLIB}
  frm.BorderStyle := TFmxFormBorderStyle.Single;
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
  TAdvUtils.ScaleForCurrentDPI(frm);
  frm.OnResize := {$IFDEF LCLLIB}@{$ENDIF}DoFormResize;
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
    if Assigned(VSIDE) then
    begin
      asm
      pas["WEBLib.Forms"].VSIDE.restoreDocument();
      end;
    end;
  end);
  {$ENDIF}
end;

procedure TAdvResponsiveManagerDimensionsEditor.LoadDimensions;
 {$IFNDEF WEBLIB}
var
  r: TResourceStream;
  sl: TStringList;
  j, a, va, sr: TJSONValue;
  I: Integer;
  it: TAdvResponsiveManagerDimensionsEditorDimension;
{$ENDIF}
begin
  {$IFNDEF WEBLIB}
  r := TAdvUtils.GetResourceStream('ADVRESPONSIVEMANAGERDEVICELIST', HInstance);
  if Assigned(r) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromStream(r);
      j := TAdvUtils.ParseJSON(sl.Text);
      if Assigned(j) then
      begin
        try
          a := j['devices'];
          if a is TJSONArray then
          begin
            for I := 0 to a.AsArray.Length - 1 do
            begin
              va := a.AsArray[I];
              if Assigned(va['default']) then
              begin
                it := TAdvResponsiveManagerDimensionsEditorDimension.Create;

                if Assigned(va['type']) then
                begin
                  if va['type'].AsString = 'desktop' then
                    it.&Type := edtDesktop
                  else if va['type'].AsString = 'phone' then
                    it.&Type := edtPhone
                  else if va['type'].AsString = 'tablet' then
                    it.&Type := edtTablet
                end;

                if Assigned(va['title']) then
                  it.Title := va['title'].AsString;

                if Assigned(va['default']) then
                  it.&Default := va['default'].AsBoolean;

                it.Resource := True;

                if Assigned(va['screen']) then
                begin
                  sr := va['screen']['vertical'];
                  if not Assigned(sr) then
                    sr := va['screen']['horizontal'];

                  if Assigned(sr) then
                  begin
                    if Assigned(sr['width']) and Assigned(sr['height']) then
                    begin
                      it.Width := sr['width'].AsInteger;
                      it.Height := sr['height'].AsInteger;
                    end;
                  end;
                end;

                FDimensions.Add(it);
              end;
            end;
          end;
        finally
          j.Free;
        end;
      end;
    finally
      sl.Free;
      r.Free;
    end;
  end;
  {$ENDIF}
end;

function TAdvResponsiveManagerDimensionsEditor.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

{$IFDEF WEBLIB}

{ TAdvResponsiveManagerDimensionsEditorForm }

constructor TAdvResponsiveManagerDimensionsEditorForm.CreateDialogNew(AResponsiveManagerEditor: TAdvResponsiveManagerDimensionsEditor; ACaption: string);
begin
  FResponsiveManagerEditor := AResponsiveManagerEditor;
  inherited CreateDialogNew(ACaption);
end;

procedure TAdvResponsiveManagerDimensionsEditorForm.LoadDFMValues;
begin
  inherited LoadDFMValues;
  if Assigned(FResponsiveManagerEditor) then
  begin
    FResponsiveManagerEditor.BuildEditor(Self);
    ActiveControl := FResponsiveManagerEditor.FListBox;
  end;
end;

procedure TAdvResponsiveManagerDimensionsEditorForm.Loaded;
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
  end;
end;

{$ENDIF}

{ TAdvResponsiveManagerDimensionsEditorDimension }

constructor TAdvResponsiveManagerDimensionsEditorDimension.Create;
begin
  FDefault := True;
end;

end.
