{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2016 - 2020                                 }
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

{$I TMSDEFS.INC}

unit AdvSmoothImageListBoxPicker;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, AdvSmoothEditButton, AdvSmoothImageListBox, AdvStyleIF, GDIPFill
  {$IFDEF DELPHIXE2_LVL}
  , System.UITypes
  {$ENDIF};

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 3; // Build nr.

  // Version history
  // v1.0.0.0 : first release
  // v1.0.1.0 : New : Support for Windows Vista and Windows Seven Style
  // v1.0.2.0 : New : Built-in support for reduced color set for use with terminal servers
  // v1.1.0.0 : New : VCL Styles support
  // v1.1.0.1 : Fix : Added check if the picker is enabled and visible so there is no exception thrown when it can't be focused.
  // v1.2.0.0 : New : Support for Office 2019 styles
  // v1.2.1.0 : New : UIStyle property for TMSStyle
  // v1.2.1.1 : Fixed : UIStyle set to tsCustom after initial Office look
  // v1.2.1.2 : Fixed : Office 2019 colors updated
  //          : Improved : On creation check for enabled AdvFormStyler
  // v1.2.1.3 : Fixed : Dropdown button colors in office 2019 styles
  //          : Fixed : ParentFont kept true on initialization

type
  TAdvSmoothImageListBoxPicker = class;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothImageListBoxPicker = class(TAdvSmoothEditBtn, ITMSStyle)
  private
    { Private declarations }
    FTimer: TTimer;
    FTMSStyle: TTMSStyle;
    FDesignTime: Boolean;
    FLst: TAdvSmoothImageListBox;
    FDeactivating: boolean;
    FDisableHide: boolean;
    LstParent: TForm;
    CancelThisBtnClick : Boolean;
    FHideListBoxAfterSelection: boolean;
    FOnSelectImage: TAdvSmoothImageListBoxItemEvent;
    FImageListLocation: string;
    procedure HideParent;
    procedure InitEvents;
    function GetParentEx: TWinControl;
    procedure SetParentEx(const Value: TWinControl);
    function GetSelectedImageIndex: Integer;
    procedure SetSelectedImageIndex(const Value: Integer);
    procedure SetImageListLocation(const Value: string);
    procedure SetListbox(const Value: TAdvSmoothImageListBox);
    procedure SetUIStyle(const Value: TTMSStyle);
  protected
    function GetVersionNr: Integer; override;
    { Protected declarations }
    procedure BtnClick(Sender: TObject); override;
    procedure lstParentDeactivate(Sender: TObject);
    procedure ListBoxItemSelect(Sender: TObject; itemindex: integer);
    procedure ListBoxKeyPress(Sender: TObject; var Key: Char);
    procedure ListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListNavigate(Sender: TObject; NavigationMode: TAdvSmoothImageListBoxNavigationMode; var allow: Boolean);
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure Loaded; override;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure CreateWnd; override;
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure TimerEvent(Sender: TObject);
//    function GetChildParent : TComponent; override;
//    function GetChildOwner : TComponent; override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure CancelBtnClick;
    destructor Destroy; override;
    procedure DropDown; virtual;
    property Parent: TWinControl read GetParentEx write SetParentEx;
//    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeID: String;
  published
    { Published declarations }
    property TabOrder;
    property TabStop;
    property HideListBoxAfterSelection : boolean read FHideListBoxAfterSelection write FHideListBoxAfterSelection;
    property OnSelectImage: TAdvSmoothImageListBoxItemEvent read FOnSelectImage write FOnSelectImage;
    property ImagesLocation: string read FImageListLocation write SetImageListLocation;
    property SelectedImageIndex: integer read GetSelectedImageIndex write SetSelectedImageIndex;
    property ListBox: TAdvSmoothImageListBox read FLst write SetListbox;
    property UIStyle: TTMSStyle read FTMSStyle write SetUIStyle default tsCustom;
  end;

implementation

{ TAdvSmoothImageListBoxPicker }

procedure TAdvSmoothImageListBoxPicker.DropDown;
var
  LstPos: TPoint;
  r: TRect;
  i: integer;
  
  function Min(a,b: Integer): Integer;
  begin
    if (a > b) then
      Result := b
    else
      Result := a;
  end;

  function GetParentWnd: HWnd;
  var
    Last, P: HWnd;
  begin
    P := GetParent((Owner as TWinControl).Handle);
    Last := P;
    while P <> 0 do
    begin
      Last := P;
      P := GetParent(P);
    end;
    Result := Last;
  end;

begin
  if (Parent is TForm) then
  begin
    if (Parent as TForm).FormStyle = fsStayOnTop then
      LstParent.FormStyle := fsStayOnTop;
  end
  else
    LstParent.FormStyle := fsStayOnTop;

  LstPos.x := -2;
  LstPos.y := Height - 3;
  LstPos := ClientToScreen(LstPos);

  SystemParametersInfo(SPI_GETWORKAREA, 0,@r,0); //account for taskbar...

  if (LstPos.y + FLst.Height > r.Bottom) then
    LstPos.Y := LstPos.Y - FLst.Height - Height + 3;

  if (LstPos.x + FLst.Width > r.right) then
    LstPos.x := LstPos.x - (FLst.Width - Width);

  try
    i := FLst.Items.Find(Text);
    if (Text = '') or (i = -1) then
      FLst.SelectedItemIndex := FLst.Items.FirstItem
    else
      FLst.SelectedItemIndex := i;
  except
    on Exception do
       Text := 'exception';
  end;

  LstParent.Width := 0;
  LstParent.Height := 0;

  LstParent.Show;

  LstParent.Left := LstPos.x;
  LstParent.Top := LstPos.y;
  lstParent.Width := FLst.Width;
  lstParent.Height := FLst.Height;  

  FLst.SetFocus;
  SendMessage(GetParentWnd, WM_NCACTIVATE, 1, 0);
end;

procedure TAdvSmoothImageListBoxPicker.SaveToTheme(FileName: String);
begin

end;

procedure TAdvSmoothImageListBoxPicker.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  ListBox.SetComponentStyle(AStyle);

  if AStyle = tsCustom then
    Exit;

  case AStyle of
    tsOffice2019White:
    begin
      if Flat and Assigned(Parent) then
      begin
        Color := (Parent as TWinControl).Brush.Color;
      end
      else
        Color := clWhite;
      FlatLineColor := $00ABABAB;
      FocusFontColor := $003B3B3B;
      Font.Color := $00444648;
      LabelFont.Color := $00444648;
    end;
    tsOffice2019Gray:
    begin
      if Flat and Assigned(Parent) then
      begin
        Color := (Parent as TWinControl).Brush.Color;
        FocusFontColor := clWhite;
      end
      else
      begin
        Color := $00B8BBBE;
        FocusFontColor := $001E1F20;
      end;
      FlatLineColor := $00808080;
      Font.Color := $00232425;
      LabelFont.Color := clWhite;
    end;
    tsOffice2019Black:
    begin
      if Flat and Assigned(Parent) then
        Color := (Parent as TWinControl).Brush.Color
      else
        Color := $00444444;
      FlatLineColor := $00686868;
      FocusFontColor := clWhite;
      Font.Color := clWhite;
      LabelFont.Color := clWhite;
    end;
    else
    begin
      if Flat and FlatParentColor and Assigned(Parent) then
        Color := (Parent as TWinControl).Brush.Color
      else
        Color := clWindow;
      FlatLineColor := clWindowText;
      Font.Color := clWindowText;
      ErrorColor := clRed;
      ErrorFontColor := clWhite;
      FocusColor := clNone;
      FocusFontColor := clWindowText;
      Font.Color := clWindowText;
      LabelFont.Color := clWindowText;
    end;
  end;

  case AStyle of
    tsOffice2019White:
    begin
      ButtonColor := clWhite;
      ButtonColorHot := $00F2E1D5;
      ButtonColorDown := $00E3BDA3;
      ButtonTextColor := $00444648;
      ButtonTextColorHot := $00232425;
      ButtonTextColorDown := $00232425;
      ButtonBorderColor := $00ABABAB;
    end;
    tsOffice2019Gray:
    begin
      ButtonColor := $00B8BBBE;
      ButtonColorHot := $00969696;
      ButtonColorDown := $00666666;
      ButtonTextColor := $00232425;
      ButtonTextColorHot := $00232425;
      ButtonTextColorDown := $00232425;
      ButtonBorderColor := $00808080;
    end;
    tsOffice2019Black:
    begin
      ButtonColor := $00444444;
      ButtonColorHot := $00686868;
      ButtonColorDown := $00828282;
      ButtonTextColor := clWhite;
      ButtonTextColorHot := clWhite;
      ButtonTextColorDown := clWhite;
      ButtonBorderColor := $00686868;
    end;
    else
    begin
      ButtonColor := clNone;
      ButtonColorHot := clNone;
      ButtonColorDown := clNone;
      ButtonTextColor := clNone;
      ButtonTextColorHot := clNone;
      ButtonTextColorDown := clNone;
      ButtonBorderColor := clNone;
    end;
  end;
  
end;

procedure TAdvSmoothImageListBoxPicker.SetImageListLocation(
  const Value: string);
begin
  if FImageListLocation <> value then
  begin
    FImageListLocation := Value;
    FLst.AddImageLocationsFromFolder(value);
  end;
end;

procedure TAdvSmoothImageListBoxPicker.SetListbox(
  const Value: TAdvSmoothImageListBox);
begin
  FLst.Assign(Value);
end;

procedure TAdvSmoothImageListBoxPicker.BtnClick(Sender: TObject);
begin
  CancelThisBtnClick := False;
  
  inherited;

  if CancelThisBtnClick then
    Exit;

  if FDeactivating then
  begin
    FDeactivating := false;
    Exit;
  end;

  if Assigned(LstParent) then
  begin
    if LstParent.Visible then
    begin
      FDeactivating := true;
      LstParent.Hide;
      Exit;
    end
    else
      DropDown;
  end
  else
    DropDown;
end;

procedure TAdvSmoothImageListBoxPicker.CancelBtnClick;
begin
  CancelThisBtnClick := True;
end;

constructor TAdvSmoothImageListBoxPicker.Create(AOwner: TComponent);
begin
  inherited;
  Text := '';
  LstParent := TForm.Create(Self);
  LstParent.BorderStyle := bsNone;

  LstParent.Width := 0;
  LstParent.Height := 0;

  FLst := TAdvSmoothImageListBox.Create(Self);
  FLst.SetSubComponent(true);
  FLst.Parent := LstParent;
  //FLst.Name := self.Name + 'lst' + inttostr(AOwner.ComponentCount)+'_';
  FLst.TabStop := true;

  LstParent.OnDeactivate := LstParentDeactivate;
  Width := 108;
  FHideListBoxAfterSelection := True;
  Button.Glyph.Handle := LoadBitmap(0, MakeIntResource(OBM_COMBO));
  Button.FocusControl := nil;
  ButtonStyle := bsDropDown;
  SelectedImageIndex := -1;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  FTMSStyle := tsCustom;

  FTimer := TTimer.Create(self);
  FTimer.Enabled := false;
  FTimer.OnTimer := TimerEvent;
  FTimer.Interval := 100;
end;

procedure TAdvSmoothImageListBoxPicker.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
//  Filer.DefineProperty('ListBox',ReadChild, WriteChild, FLst <> Nil);
end;

destructor TAdvSmoothImageListBoxPicker.Destroy;
begin
  FTimer.Free;
  Flst.Free;
  LstParent.Free;
  inherited;
end;

procedure TAdvSmoothImageListBoxPicker.TimerEvent(Sender: TObject);
begin
  FDeactivating := false;
  FTimer.Enabled :=false;
end;

procedure TAdvSmoothImageListBoxPicker.HideParent;
begin
  FDeactivating := false;
  LstParent.Hide;
  try
    if Self.Visible and Self.Enabled then
      SetFocus;
  except
  end;  
end;

procedure TAdvSmoothImageListBoxPicker.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if (key = VK_F4) and not (ssAlt in Shift) and not (ssCtrl in Shift) then
  begin
    if LstParent.Visible then
      HideParent
    else
      BtnClick(Self);
  end;
end;

procedure TAdvSmoothImageListBoxPicker.InitEvents;
begin
  FLst.OnKeyPress := ListBoxKeyPress;
  FLst.OnKeyDown := ListBoxKeyDown;
  FLst.OnItemSelect := ListBoxItemSelect;
  FLst.OnNavigate := ListNavigate;
end;

procedure TAdvSmoothImageListBoxPicker.Loaded;
begin
  inherited;
  InitEvents;
end;

procedure TAdvSmoothImageListBoxPicker.LoadFromTheme(FileName: String);
begin

end;

procedure TAdvSmoothImageListBoxPicker.ListBoxItemSelect(Sender: TObject; Itemindex: integer);
begin
  Text := FLst.Items[itemindex].Caption.Text;
  
  if FHideListBoxAfterSelection and not FDisableHide then
  begin
    HideParent;
  end;
  
  FDisableHide := false;

  if Assigned(FOnSelectImage) then
    FOnSelectImage(Self, itemindex);
end;

procedure TAdvSmoothImageListBoxPicker.ListBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (key in [VK_DOWN, VK_UP, VK_HOME, VK_END, VK_PRIOR, VK_NEXT]) then
  begin
    FDisableHide := true;
  end;

  if Key = VK_F4 then
    HideParent;

  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);
end;

procedure TAdvSmoothImageListBoxPicker.ListBoxKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Assigned(OnKeyPress) then
    OnKeyPress(Self, Key);

  if (Key = char(VK_RETURN)) or (Key = char(VK_SPACE)) then
    ListBoxItemSelect(Sender, FLst.SelectedItemIndex);

  if Key = #27 then
    HideParent;
end;

procedure TAdvSmoothImageListBoxPicker.ListNavigate(Sender: TObject;
  NavigationMode: TAdvSmoothImageListBoxNavigationMode; var allow: Boolean);
begin
  FDisableHide := true;
end;

procedure TAdvSmoothImageListBoxPicker.lstParentDeactivate(Sender: TObject);
begin
  FDeactivating := true;
  (Sender as TForm).Hide;
  FTimer.Enabled := true;
end;

procedure TAdvSmoothImageListBoxPicker.ReadTMSStyle(Reader: TReader);
var
  TempStyleInt: Integer;
begin
  TempStyleInt := Reader.ReadInteger;
  if (FTMSStyle = tsCustom) and (TempStyleInt <> 0) then
    FTMSStyle := tsCustom; //For compiler warning TempStyleInt not used;
end;

procedure TAdvSmoothImageListBoxPicker.WMSetFocus(var Message: TWMSetFocus);
begin
  if EditorEnabled then
    inherited
  else
    Button.SetFocus;
end;

procedure TAdvSmoothImageListBoxPicker.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(0);
end;

procedure TAdvSmoothImageListBoxPicker.Change;
begin
  inherited;
end;

procedure TAdvSmoothImageListBoxPicker.CreateWnd;
begin
  inherited;
  if FDesignTime then
  begin
    SetComponentStyle(GetDefaultStyle(Owner));
    if SetParentFontForStyle(FTMSStyle) then
      ParentFont := true;
    FTMSStyle := tsCustom;
  end;

  InitEvents;
end;

function TAdvSmoothImageListBoxPicker.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothImageListBoxPicker.GetParentEx: TWinControl;
begin
  Result := inherited Parent;
end;

procedure TAdvSmoothImageListBoxPicker.SetParentEx(const Value: TWinControl);
begin
  inherited Parent := Value;
  InitEvents;
end;

function TAdvSmoothImageListBoxPicker.GetSelectedImageIndex: integer;
begin
  Result := FLst.SelectedItemIndex;
end;

function TAdvSmoothImageListBoxPicker.GetThemeID: String;
begin
  Result := ClassName;
end;

procedure TAdvSmoothImageListBoxPicker.SetSelectedImageIndex(const Value: integer);
begin
  FLst.SelectedItemIndex := value;
  if Value = -1 then
    Text := ''
  else
    Text := ExtractFileName(FLst.Items[value].Caption.Text);
end;

procedure TAdvSmoothImageListBoxPicker.SetUIStyle(const Value: TTMSStyle);
begin
  SetComponentStyle(Value);
end;

function TAdvSmoothImageListBoxPicker.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

{ TAdvSmoothImageListBoxPickerListBox }

end.
