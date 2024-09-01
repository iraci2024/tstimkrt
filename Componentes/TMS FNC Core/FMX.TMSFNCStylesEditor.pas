{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2020 - 2021                               }
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

unit FMX.TMSFNCStylesEditor;

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
  Windows, Registry,
  {$ENDIF}
  Classes, FMX.TMSFNCCustomComponent, FMX.TMSFNCCustomControl, FMX.Controls,
  FMX.TMSFNCTypes, FMX.StdCtrls, FMX.ExtCtrls, FMX.Forms, FMX.TMSFNCGraphics,
  FMX.TMSFNCGraphicsTypes, FMX.TMSFNCStyles,
  FMX.TMSFNCEditorButton, FMX.TMSFNCEditorPanel, FMX.TMSFNCEditorListView
  {$IFDEF FMXLIB}
  ,FMX.Types, FMX.Memo, FMX.ListBox, FMX.ComboEdit, FMX.Layouts
  {$ENDIF}
  {$IFNDEF LCLLIB}
  ,UITypes, Types
  {$ENDIF}
  {$IFNDEF LCLWEBLIB}
  ,Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,contnrs
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First Release
  // v1.0.1.0 : New : Updated design

resourcestring
  sTMSFNCStylesEditorOK = 'OK';
  sTMSFNCStylesEditorCancel = 'Cancel';
  sTMSFNCStylesEditorOpen = '&Open';
  sTMSFNCStylesEditorApply = '&Apply';
  sTMSFNCStylesEditorCombine = '&Combine';
  sTMSFNCStylesEditorDelete = '&Delete';

type
  TTMSFNCStylesEditor = class;

  {$IFDEF FMXLIB}
  TTMSFNCStylesEditorParent = TFmxObject;
  TTMSFNCStylesEditorComboBox = class(TComboEdit);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  TTMSFNCStylesEditorParent = TWinControl;
  TTMSFNCStylesEditorComboBox = class(TComboBox);
  {$ENDIF}

  TTMSFNCStylesListItem = class
  private
    FResource: Boolean;
    FName: string;
    FDisplayName: string;
  public
    constructor Create(AResource: Boolean; AName, ADisplayName: string);
    property Resource: Boolean read FResource;
    property Name: string read FName;
    property DisplayName: string read FDisplayName;
  end;

  {$IFDEF WEBLIB}
  TTMSFNCStylesListItems = class(TObjectList)
  private
    function GetItem(Index: Integer): TTMSFNCStylesListItem;
    procedure SetItem(Index: Integer; const Value: TTMSFNCStylesListItem);
  public
    property Items[Index: Integer]: TTMSFNCStylesListItem read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCStylesListItems = class(TObjectList<TTMSFNCStylesListItem>);
  {$ENDIF}

  {$IFDEF WEBLIB}
  TTMSFNCStylesListItemsRef = class(TList)
  private
    function GetItem(Index: Integer): TTMSFNCStylesListItem;
    procedure SetItem(Index: Integer; const Value: TTMSFNCStylesListItem);
  public
    property Items[Index: Integer]: TTMSFNCStylesListItem read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCStylesListItemsRef = class(TList<TTMSFNCStylesListItem>);
  {$ENDIF}

  TTMSFNCStylesVisualizer = class(TTMSFNCCustomControl)
  private
    FItems: TTMSFNCStylesListItemsRef;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw({%H-}AGraphics: TTMSFNCGraphics; {%H-}ARect: TRectF); override;
    property Items: TTMSFNCStylesListItemsRef read FItems write FItems;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsDesktop)]
  {$ENDIF}
  TTMSFNCStylesEditor = class(TTMSFNCCustomComponent)
  private
    frm: TTMSFNCCustomDesignerForm;
    FItems: TTMSFNCStylesListItems;
    FPanel, FTopPanel, FSidePanel: TTMSFNCEditorPanel;
    FVisualizer: TTMSFNCStylesVisualizer;
    FSelectedStyle: TLabel;
    FButtonOk, FButtonCombine, FButtonDelete, FButtonApply: TTMSFNCEditorButton;
    FButtonOpen: TTMSFNCEditorButton;
    FListBox: TTMSFNCEditorList;
    {$IFNDEF FMXLIB}
    FVScrlBox: TScrollBox;
    {$ENDIF}
    {$IFDEF FMXLIB}
    FVScrlBox: TVertScrollBox;
    {$ENDIF}
    FSplitter: TSplitter;
    FStylesManager: TTMSFNCStylesManager;
    FStylesForm: TCustomForm;
  protected
    function GetInstance: NativeUInt; override;
    procedure DoListClick(Sender: TObject; AIndex: Integer; AItem: TTMSFNCEditorListItem; ASelected: Boolean);
    procedure DoApply(Sender: TObject);
    procedure DoCombine(Sender: TObject);
    procedure DoDelete(Sender: TObject);
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
    procedure BuildEditor(AParent: TTMSFNCStylesEditorParent); virtual;
    procedure DoOpenFile(Sender: TObject); virtual; {$IFDEF WEBLIB}async;{$ENDIF}
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
    property StylesForm: TCustomForm read FStylesForm write FStylesForm;
    procedure Assign(Source: TPersistent); override;
    class procedure LoadStylesFromResources(AStrings: TStrings);
  end;

  TTMSFNCStylesEditorForm = class(TTMSFNCCustomDesignerForm)
  {$IFDEF WEBLIB}
  private
    FStylesEditor: TTMSFNCStylesEditor;
  protected
    procedure Loaded; override;
    procedure LoadDFMValues; override;
  public
    constructor CreateDialogNew(AStylesEditor: TTMSFNCStylesEditor; ACaption: string); reintroduce; virtual;
  {$ENDIF}
  end;

implementation

uses
  FMX.TMSFNCUtils, FMX.Graphics, SysUtils, FMX.Dialogs,
  FMX.TMSFNCEditorsTools, Math;

type
  {$IFNDEF WEBLIB}
  TTMSFNCStylesEditorOpenDialog = TOpenDialog;
  {$ENDIF}
  {$IFDEF WEBLIB}
  TTMSFNCStylesEditorOpenDialog = TWebOpenDialog;
  {$ENDIF}

{$R 'TMSFNCStylesResources.res'}

{ TTMSFNCStylesEditor }

function TranslateTextEx(AText: String): String;
begin
  {$IFDEF FMXLIB}
  Result := TranslateText(AText);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  Result := AText;
  {$ENDIF}
end;

procedure TTMSFNCStylesEditor.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
end;

procedure TTMSFNCStylesEditor.RealignControls;
begin
  {$IFDEF FMXLIB}
  FButtonApply.Position.Y := FVisualizer.Position.Y + FVisualizer.Height + 5;
  FButtonApply.Position.X := 5;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  FButtonApply.Top := FVisualizer.Top + FVisualizer.Height + 5;
  FButtonApply.Left := 5;
  {$ENDIF}

  {$IFDEF FMXLIB}
  FButtonCombine.Position.Y := FVisualizer.Position.Y + FVisualizer.Height + 5;
  FButtonCombine.Position.X := FButtonApply.Width + 10;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  FButtonCombine.Top := FVisualizer.Top + FVisualizer.Height + 5;
  FButtonCombine.Left := FButtonApply.Width + 10;
  {$ENDIF}

  {$IFDEF FMXLIB}
  FButtonDelete.Position.Y := FVisualizer.Position.Y + FVisualizer.Height + 5;
  FButtonDelete.Position.X := FButtonApply.Width + FButtonCombine.Width + 20;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  FButtonDelete.Top := FVisualizer.Top + FVisualizer.Height + 5;
  FButtonDelete.Left := FButtonApply.Width + FButtonCombine.Width + 20;
  {$ENDIF}

  {$IFDEF FMXLIB}
  FSelectedStyle.Position.X := 5;
  FSelectedStyle.Position.Y := 5;
  {$ELSE}
  FSelectedStyle.Left := 5;
  FSelectedStyle.Top := 5;
  {$ENDIF}

  FListBox.UpdateList;
  FListBox.Height := Max(FListBox.GetListHeight, FVScrlBox.Height);
end;

procedure TTMSFNCStylesEditor.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TTMSFNCStylesEditor);
end;

procedure TTMSFNCStylesEditor.Assign(Source: TPersistent);
begin
end;

{$IFDEF MSWINDOWS}
function EnumRCDataProc(hModule: THandle; lpszType, lpszName: PChar; lParam: NativeInt): BOOL; stdcall;
begin
  if Pos('TTMSFNCSTYLE_', lpszName) = 1 then
    TStrings(lParam).Add(lpszName);

  Result := True;
end;
{$ENDIF}

class procedure TTMSFNCStylesEditor.LoadStylesFromResources(AStrings: TStrings);
begin
  {$IFDEF MSWINDOWS}
  {$WARNINGS OFF}
  EnumResourceNames(HInstance, RT_RCDATA, {$IF FPC_FULLVERSION >= 30200}ENUMRESNAMEPROC(@EnumRCDataProc){$ELSE}@EnumRCDataProc{$ENDIF}, NativeInt(AStrings));
  {$WARNINGS ON}
  {$ENDIF}
end;

procedure TTMSFNCStylesEditor.BuildEditor(AParent: TTMSFNCStylesEditorParent);
var
  sl: TStringList;
  I: Integer;
  f: string;
  it: TTMSFNCStylesListItem;
  li: TTMSFNCEditorListItem;
  {$IFDEF MSWINDOWS}
  fn: string;
  RegIniFile: TRegIniFile;
  {$ENDIF}
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
    {$IFDEF CMNLIB}
    FTopPanel.PanelPosition := eppCenter;
    {$ENDIF}
    {$IFNDEF CMNLIB}
    FTopPanel.PanelPosition := eppTop;
    {$ENDIF}
    SetEditorBackPanelAppearance(FTopPanel);

    FButtonOpen := TTMSFNCEditorButton.Create(FTopPanel);
    FButtonOpen.Parent := FTopPanel;
    FButtonOpen.Text := TranslateTextEx(sTMSFNCStylesEditorOpen);
    {$IFDEF FMXLIB}
    FButtonOpen.Align := TAlignLayout.Left;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonOpen.Align := alLeft;
    {$IFDEF VCLWEBLIB}
    FButtonOpen.AlignWithMargins := True;
    {$ENDIF}
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FButtonOpen.Margins.Right := 0;
    FButtonOpen.Margins.Top := 5;
    FButtonOpen.Margins.Bottom := 5;
    FButtonOpen.Margins.Left := 5;
    {$ENDIF}
    {$IFDEF LCLLIB}
    FButtonOpen.BorderSpacing.Right := 0;
    FButtonOpen.BorderSpacing.Top := 5;
    FButtonOpen.BorderSpacing.Bottom := 5;
    FButtonOpen.BorderSpacing.Left := 5;
    {$ENDIF}
    FButtonOpen.OnClick := DoOpenFile;
    SetEditorTabButtonAppearance(FButtonOpen);

    FTopPanel.Height := 37;

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
    FVScrlBox.Width := 200;

    FListBox := TTMSFNCEditorList.Create(FVScrlBox);
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
      LoadStylesFromResources(sl);
      FListBox.BeginUpdate;
      for I := 0 to sl.Count - 1 do
      begin
        f := sl[I];
        it := TTMSFNCStylesListItem.Create(True, f, StringReplace(f, 'TTMSFNCSTYLE_', '', [rfReplaceAll]));
        FItems.Add(it);
        li := FListBox.Items.Add;
        li.Name := it.DisplayName;
        li.DataObject := it;
      end;
      FListBox.EndUpdate;

      {$IFDEF MSWINDOWS}
      sl.Clear;
      RegIniFile := TRegIniFile.Create('TMSFNCStyles');
      try
        RegIniFile.ReadSectionValues('', sl);
        FListBox.BeginUpdate;
        for I := 0 to sl.Count - 1 do
        begin
          f := sl.Names[I];
          fn := sl.Values[f];
          it := TTMSFNCStylesListItem.Create(False, fn, f);
          FItems.Add(it);
          li := FListBox.Items.Add;

          if not FileExists(fn) then
            li.Name := it.DisplayName + ' [File does not exist!]'
          else
            li.Name := it.DisplayName;

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

     FSidePanel := TTMSFNCEditorPanel.Create(AParent);
    {$IFDEF FMXLIB}
    FSidePanel.Align := TAlignLayout.Client;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FSidePanel.Align := alClient;
    {$ENDIF}
    FSidePanel.PanelPosition := eppCenter;
    FSidePanel.Parent := AParent;
    SetEditorBackPanelAppearance(FSidePanel);

    FSelectedStyle := TLabel.Create(FSidePanel);
    FSelectedStyle.Parent := FSidePanel;
    {$IFDEF FMXLIB}
    FSelectedStyle.Text := 'Selected Style(s)';
    FSelectedStyle.Position.X := 0;
    FSelectedStyle.Position.Y := 0;
    {$ELSE}
    FSelectedStyle.Caption := 'Selected Style(s)';
    FSelectedStyle.Left := 5;
    FSelectedStyle.Top := 5;
    {$ENDIF}
    SetEditorLabelAppearance(FSelectedStyle);

    FSplitter := TSplitter.Create(AParent);
    FSplitter.Parent := AParent;
    {$IFDEF CMNWEBLIB}
    if IsLightTheme then
      FSplitter.Color := EDITORMAINBACKCOLORLIGHT
    else
      FSplitter.Color := EDITORMAINBACKCOLORDARK;
    {$ENDIF}

    {$IFDEF FMXLIB}
    FVScrlBox.Align := TAlignLayout.Left;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FVScrlBox.Align := alLeft;
    {$ENDIF}

    FVisualizer := TTMSFNCStylesVisualizer.Create(FSidePanel);
    FVisualizer.Parent := FSidePanel;
    FVisualizer.ControlAlignment := caClient;
    FVisualizer.SetControlMargins(0, FSelectedStyle.Height + 5, 0, 37);
    {$IFDEF FMXLIB}
    FVisualizer.DisableBackground;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    if IsLightTheme then
      FVisualizer.Color := EDITORSUBBACKCOLORLIGHT
    else
      FVisualizer.Color := EDITORSUBBACKCOLORDARK;
    {$ENDIF}

    FButtonApply := TTMSFNCEditorButton.Create(FSidePanel);
    FButtonApply.Parent := FSidePanel;
    FButtonApply.Text := TranslateTextEx(sTMSFNCStylesEditorApply);
    {$IFDEF FMXLIB}
    FButtonApply.Position.Y := 293;
    FButtonApply.Position.X := 5;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonApply.Top := 293;
    FButtonApply.Left := 5;
    {$ENDIF}
    FButtonApply.OnClick := DoApply;
    FButtonApply.Enabled := False;
    FButtonApply.Height := 27;
    SetEditorIconButtonAppearance(FButtonApply);

    FButtonCombine := TTMSFNCEditorButton.Create(FSidePanel);
    {$IFNDEF WEBLIB}
    FButtonCombine.Parent := FSidePanel;
    {$ENDIF}
    FButtonCombine.Text := TranslateTextEx(sTMSFNCStylesEditorCombine);
    {$IFDEF FMXLIB}
    FButtonCombine.Position.Y := 293;
    FButtonCombine.Position.X := FButtonApply.Width + 10;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonCombine.Top := 293;
    FButtonCombine.Left := FButtonApply.Width + 10;
    {$ENDIF}
    FButtonCombine.OnClick := DoCombine;
    FButtonCombine.Enabled := False;
    FButtonCombine.Height := 27;
    SetEditorIconButtonAppearance(FButtonCombine);

    FButtonDelete := TTMSFNCEditorButton.Create(FSidePanel);
    {$IFNDEF WEBLIB}
    FButtonDelete.Parent := FSidePanel;
    {$ENDIF}
    FButtonDelete.Text := TranslateTextEx(sTMSFNCStylesEditorDelete);
    {$IFDEF FMXLIB}
    FButtonDelete.Position.Y := 293;
    FButtonDelete.Position.X := FButtonApply.Width + FButtonCombine.Width + 20;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonDelete.Top := 293;
    FButtonDelete.Left := FButtonApply.Width + FButtonCombine.Width + 20;
    {$ENDIF}
    FButtonDelete.OnClick := DoDelete;
    FButtonDelete.Enabled := False;
    FButtonDelete.Height := 27;
    SetEditorIconButtonAppearance(FButtonDelete);

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

    FButtonOK := TTMSFNCEditorButton.Create(FPanel);
    FButtonOK.ModalResult := mrOk;
//    FButtonOk.Default := True;
    FButtonOK.Parent := FPanel;
    FButtonOK.Text := TranslateTextEx(sTMSFNCStylesEditorOK);
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

    FVisualizer.OnResize := {$IFDEF LCLLIB}@{$ENDIF}DoListBoxResize;
  end;
end;

{$IFDEF WEBLIB}

procedure TTMSFNCStylesEditor.DoButtonOKClick(Sender: TObject);
begin
  if Assigned(frm) then
    frm.ModalResult := mrOK;
end;
{$ENDIF}

constructor TTMSFNCStylesEditor.Create(AOwner: TComponent);
begin
  inherited;
  FStylesManager := TTMSFNCStylesManager.Create(Self);
  FItems := TTMSFNCStylesListItems.Create;
end;

destructor TTMSFNCStylesEditor.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FStylesManager);
  inherited;
end;

procedure TTMSFNCStylesEditor.DoApply(Sender: TObject);
var
  it: TTMSFNCStylesListItem;
  I: Integer;
begin
  if FVisualizer.Items.Count > 0 then
  begin
    for I := 0 to FVisualizer.Items.Count - 1 do
    begin
      it := FVisualizer.Items[I];
      if Assigned(it) then
      begin
        if it.Resource then
        begin
        {$IFNDEF WEBLIB}
          FStylesManager.LoadStyleFromResource(it.Name)
        {$ELSE}
          raise Exception.Create('Implement LoadStyleFromResource');
        {$ENDIF}
        end
        else
          {$IFNDEF WEBLIB}
          FStylesManager.LoadStyleFromFile(it.Name);
          {$ENDIF}
          {$IFDEF WEBLIB}
          FStylesManager.LoadStyleFromText(it.Name);
          {$ENDIF}
      end;
    end;
  end;
end;

procedure TTMSFNCStylesEditor.DoDelete(Sender: TObject);
var
  I: Integer;
  {$IFDEF MSWINDOWS}
  it: TTMSFNCStylesListItem;
  RegIniFile: TRegIniFile;
  {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  RegIniFile := TRegIniFile.Create('TMSFNCStyles');
  try
    for I := 0 to FVisualizer.Items.Count - 1 do
    begin
      it := FVisualizer.Items[I];
      if RegIniFile.ValueExists(it.DisplayName) then
        RegIniFile.DeleteValue(it.DisplayName);
    end;
  finally
    RegIniFile.Free;
  end;
  {$ENDIF}

  FVisualizer.Items.Clear;

  for I := FListBox.Items.Count - 1 downto 0 do
  begin
    if FListBox.Items[I].Selected then
    begin
      if not TTMSFNCStylesListItem(FListBox.Items[I].DataObject).Resource then
        FListBox.Items.Delete(I);
    end;
  end;
  FVisualizer.Repaint;
  FButtonDelete.Enabled := False;
  FButtonApply.Enabled := False;
  FButtonCombine.Enabled := False;

  FListBox.Height := Max(FListBox.GetListHeight, FVScrlBox.Height);
end;

{$IFNDEF FMXLIB}
procedure TTMSFNCStylesEditor.DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{$ENDIF}
{$IFDEF FMXLIB}
procedure TTMSFNCStylesEditor.DoFormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
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

procedure TTMSFNCStylesEditor.DoFormResize(Sender: TObject);
begin
  RealignControls;
end;

procedure TTMSFNCStylesEditor.DoCombine(Sender: TObject);
{$IFNDEF WEBLIB}
var
  arr: TTMSFNCStylesManagerFileArray;
  I: Integer;
  it: TTMSFNCStylesListItem;
  li: TTMSFNCEditorListItem;
  s: string;
  sd: TSaveDialog;
  sl: TStringList;
  fn, fe: string;
{$ENDIF}
begin
  {$IFNDEF WEBLIB}
  if FVisualizer.Items.Count > 0 then
  begin
    for I := 0 to FVisualizer.Items.Count - 1 do
    begin
      it := FVisualizer.Items[I];
      SetLength(arr, Length(arr) + 1);
      if it.Resource then
        arr[Length(arr) - 1] := FStylesManager.GetStyleFromResource(it.Name)
      else
        arr[Length(arr) - 1] := FStylesManager.GetStyleFromFile(it.Name);
    end;

    sl := TStringList.Create;
    try
      s := FStylesManager.CombineStyles(arr);
      sl.Text := s;
      sd := TSaveDialog.Create(Self);
      sd.Filter := 'All (*.*)|*.*|JSON file (*.json)|*.json|Style file (*.style)|*.style';
      sd.DefaultExt := 'style';
      sd.Options := sd.Options + [TOpenOption.ofOverwritePrompt];
      sd.FilterIndex := 3;

      try
        if sd.Execute then
        begin
          fn := sd.FileName;

          fe := Uppercase(ExtractFileExt(sd.FileName));

          if (fe = '') then
          begin
            if sd.FilterIndex = 2 then
              fn := fn + '.json';
            if sd.FilterIndex = 3 then
              fn := fn + '.style';
          end
          else
          begin
            if fe = '.JSON' then
              sd.FilterIndex := 2;
            if fe = '.STYLE' then
              sd.FilterIndex := 3;
          end;

          sl.SaveToFile(fn);

          it := TTMSFNCStylesListItem.Create(False, fn, StringReplace(ExtractFileNameEx(fn), ExtractFileExt(fn), '', [rfReplaceAll]));
          FItems.Add(it);
          li := FListBox.Items.Add;
          li.Name := it.DisplayName;
          li.DataObject := it;
        end;
      finally
        sd.Free;
      end;
    finally
      sl.Free;
    end;
  end;
  {$ELSE}
  raise Exception.Create('Implement Combine');
  {$ENDIF}
end;

procedure TTMSFNCStylesEditor.DoListBoxResize(Sender: TObject);
begin
  RealignControls;
end;

procedure TTMSFNCStylesEditor.DoListClick(Sender: TObject; AIndex: Integer; AItem: TTMSFNCEditorListItem; ASelected: Boolean);
var
  I: Integer;
begin
  FVisualizer.Items.Clear;
  for I := 0 to FListBox.Items.Count - 1 do
  begin
    if FListBox.Items[I].Selected then
      FVisualizer.Items.Add(TTMSFNCStylesListItem(FListBox.Items[I].DataObject));
  end;
  FVisualizer.Repaint;

  FButtonApply.Enabled := FVisualizer.Items.Count >= 1;
  FButtonCombine.Enabled := FVisualizer.Items.Count > 1;
  FButtonDelete.Enabled := FVisualizer.Items.Count >= 1;
end;

procedure TTMSFNCStylesEditor.DoOpenFile(Sender: TObject);
var
  od: TTMSFNCStylesEditorOpenDialog;
  it: TTMSFNCStylesListItem;
  li: TTMSFNCEditorListItem;
  I: Integer;
  {$IFDEF WEBLIB}
  res: boolean;
  str: string;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  RegIniFile: TRegIniFile;
  {$ENDIF}
begin 
  od := TTMSFNCStylesEditorOpenDialog.Create(Self);
  od.Filter := 'All (*.*)|*.*|JSON file (*.json)|*.json|Style file (*.style)|*.style';
  od.FilterIndex := 3;
  od.Options := od.Options + [TOpenOption.ofAllowMultiSelect];
  {$IFDEF MSWINDOWS}
  RegIniFile := TRegIniFile.Create('TMSFNCStyles');
  {$ENDIF}
  try
    {$IFNDEF WEBLIB}
    if od.Execute then
    {$ENDIF}
    {$IFDEF WEBLIB}
    res := await(Boolean, od.Perform);
    if res then
    {$ENDIF}
    begin
      FListBox.BeginUpdate;
      for I := 0 to od.Files.Count - 1 do
      begin
        {$IFDEF WEBLIB}
        str := await(string, od.Files[I].FileAsText);  
        it := TTMSFNCStylesListItem.Create(false, str, StringReplace(od.Files[I].Name, ExtractFileExt(od.Files[I].Name), '', [rfReplaceAll]));
        {$ENDIF}
        {$IFNDEF WEBLIB}
        it := TTMSFNCStylesListItem.Create(False, od.Files[I], StringReplace(ExtractFileNameEx(od.Files[I]), ExtractFileExt(od.Files[I]), '', [rfReplaceAll]));
        {$ENDIF}
        FItems.Add(it);

        li := FListBox.Items.Add;
        li.Name := it.DisplayName;
        li.DataObject := it;
        {$IFDEF MSWINDOWS}
        RegIniFile.WriteString('', it.DisplayName, od.Files[I]);
        {$ENDIF}
      end;
      FListBox.EndUpdate;
    end;
  finally
    {$IFDEF MSWINDOWS}
    RegIniFile.Free;
    {$ENDIF}
    od.Free;
  end;
end;

{$IFDEF WEBLIB}
procedure TTMSFNCStylesEditor.Execute(AProc: TModalResultProc = nil);
{$ELSE}
function TTMSFNCStylesEditor.Execute: TModalResult;
{$ENDIF}
begin
  {$IFDEF WEBLIB}
  if Assigned(VSIDE) then
  begin
    asm
      pas["WEBLib.Forms"].VSIDE.setDocument();
    end;
  end;
  frm := TTMSFNCStylesEditorForm.CreateDialogNew(Self, 'Styles Manager');
  {$ELSE}
  frm := TTMSFNCStylesEditorForm.CreateNew(Application);
  frm.Caption := 'Styles Manager';
  {$ENDIF}
  frm.Width := 600;
  frm.Height := 400;
  {$IFDEF CMNLIB}
  frm.KeyPreview := True;
  {$ENDIF}
  frm.OnKeyDown := DoFormKeyDown;
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
  FStylesManager.StyleForm := StylesForm;
  {$IFNDEF WEBLIB}
  BuildEditor(frm);
  frm.ActiveControl := FTopPanel;
  {$ENDIF}
  TTMSFNCUtils.ScaleForCurrentDPI(frm);
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

function TTMSFNCStylesEditor.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

{ TTMSFNCStylesListItem }

constructor TTMSFNCStylesListItem.Create(AResource: Boolean; AName, ADisplayName: string);
begin
  FName := AName;
  FResource := AResource;
  FDisplayName := ADisplayName;
end;

{$IFDEF WEBLIB}
function TTMSFNCStylesListItems.GetItem(Index: Integer): TTMSFNCStylesListItem;
begin
  Result := TTMSFNCStylesListItem(inherited Items[Index]);
end;

procedure TTMSFNCStylesListItems.SetItem(Index: Integer; const Value: TTMSFNCStylesListItem);
begin
  inherited Items[Index] := Value;
end;

function TTMSFNCStylesListItemsRef.GetItem(Index: Integer): TTMSFNCStylesListItem;
begin
  Result := TTMSFNCStylesListItem(inherited Items[Index]);
end;

procedure TTMSFNCStylesListItemsRef.SetItem(Index: Integer; const Value: TTMSFNCStylesListItem);
begin
  inherited Items[Index] := Value;
end;
{$ENDIF}

{ TTMSFNCStylesVisualizer }

constructor TTMSFNCStylesVisualizer.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TTMSFNCStylesListItemsRef.Create;
end;

destructor TTMSFNCStylesVisualizer.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TTMSFNCStylesVisualizer.Draw(AGraphics: TTMSFNCGraphics;
  ARect: TRectF);
var
  I: Integer;
  it: TTMSFNCStylesListItem;
  x, y: Single;
  r, rt: TRectF;
begin
  inherited;

  AGraphics.Font.Height :=  ScalePaintValue(-11);

  AGraphics.Stroke.Color := EDITORCOUNTERCOLOR;
  AGraphics.Fill.Color := EDITORCOUNTERCOLOR;

  AGraphics.DrawRectangle(RectF(ScalePaintValue(5.0), ScalePaintValue(5.0), ScalePaintValue(10), ScalePaintValue(20)));
  AGraphics.Font.Color := EDITORCOUNTERCOLOR;
  AGraphics.DrawText(PointF(ScalePaintValue(12.0), ScalePaintValue(5.0)), ' = Resource');


  AGraphics.Stroke.Color := EDITORPRIMCOLOR;
  AGraphics.Fill.Color := EDITORPRIMCOLOR;

  AGraphics.DrawRectangle(RectF(ScalePaintValue(80), ScalePaintValue(5.0), ScalePaintValue(85.0), ScalePaintValue(20)));
  AGraphics.Font.Color := EDITORPRIMCOLOR;
  AGraphics.DrawText(PointF(ScalePaintValue(87.0), ScalePaintValue(5.0)), ' = File');

  AGraphics.Stroke.Color := EDITORPRIMCOLOR;

  x := ScalePaintValue(5);
  y := ScalePaintValue(25);
  for I := 0 to FItems.Count - 1 do
  begin
    it := FItems[I];
    r := AGraphics.CalculateText(it.DisplayName);
    rt := RectF(x, y, x + (r.Right - r.Left) + ScalePaintValue(15), y + ScalePaintValue(25));

    if rt.Right + ScalePaintValue(5) > Width then
    begin
      x := ScalePaintValue(5);
      y := y + ScalePaintValue(30);
      rt := RectF(x, y, x + (r.Right - r.Left) + ScalePaintValue(15), y + ScalePaintValue(25));
    end;

    x := rt.Right + ScalePaintValue(5);

    if IsLightTheme then
      AGraphics.Fill.Color := EDITORSUBBACKCOLORLIGHT
    else
      AGraphics.Fill.Color := EDITORSUBBACKCOLORDARK;

    if it.Resource then
      AGraphics.Stroke.Color := EDITORCOUNTERCOLOR
    else
      AGraphics.Stroke.Color := EDITORPRIMCOLOR;

    AGraphics.DrawRectangle(rt);

    AGraphics.Fill.Color := AGraphics.Stroke.Color;
    AGraphics.Font.Color := AGraphics.Stroke.Color;

    AGraphics.DrawRectangle(RectF(rt.Right - ScalePaintValue(5.0), rt.Top, rt.Right, rt.Bottom));

    AGraphics.DrawText(rt, it.DisplayName, False, gtaCenter);
  end;
end;

{$IFDEF WEBLIB}

{ TTMSFNCStylesEditorForm }

constructor TTMSFNCStylesEditorForm.CreateDialogNew(AStylesEditor: TTMSFNCStylesEditor; ACaption: string);
begin
  FStylesEditor := AStylesEditor;
  inherited CreateDialogNew(ACaption);
end;

procedure TTMSFNCStylesEditorForm.LoadDFMValues;
begin
  inherited LoadDFMValues;
  if Assigned(FStylesEditor) then
  begin
    FStylesEditor.BuildEditor(Self);
    ActiveControl := FStylesEditor.FTopPanel;
  end;
end;

procedure TTMSFNCStylesEditorForm.Loaded;
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

end.
