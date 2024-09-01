unit sComboEdit;
{$I sDefs.inc}

interface

uses
  Windows, Classes,
  sCustomComboEdit, sConst;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsComboEdit = class(TsCustomComboEdit)
{$IFNDEF NOTFORHELP}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Alignment;
    property ButtonMode;
    property CharCase;
    property CheckOnExit;
    property EditMask;
    property HideSelection;
    property ClickKey;
    property DirectInput;
    property Enabled;
    property Font;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property TextHint;
    property Visible;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;

    property AutoSelect;
    property HelpContext;
    property PasswordChar;
    property Hint;

    property DragCursor;
    property OnEnter;
    property OnExit;
{$ENDIF} // NOTFORHELP
  end;


implementation

uses sGlyphUtils;


constructor TsComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsComboEdit;
  FDefBmpID := iBTN_ELLIPSIS;
end;

end.
