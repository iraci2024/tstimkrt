{===============================================================================
  RzCheckListItemForm Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2021 by Embarcadero Technologies, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzCheckItemEditDlg
    Used by the TRzCheckListEditor dialog when adding/editing list items.


  Modification History
  ------------------------------------------------------------------------------
  7.0.0  (15 March 2021)
    * Removed old conditional defines. All code designed only for RAD Studio 10.4+.
    * Added High-DPI support for all elements in all controls
    * Added improvments and optimizations in code
  ------------------------------------------------------------------------------
  6.5.0  (28 Nov 2020)
    * Removed old conditional defines. All code designed for RAD Studio 10+.
    * Updated all uses clauses to reference complete unit scope names.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Updated form to use custom framing editing controls and HotTrack style
      buttons, radio buttons, and check boxes.
    * Added As Group check box to allow adding/editing groups.
===============================================================================}

{$I RzComps.inc}

unit RzCheckListItemForm;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  System.Classes,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Controls,
  RzEdit,
  RzButton,
  RzLabel, 
  RzRadChk;

type
  TRzCheckItemEditDlg = class(TForm)
    Label1: TRzLabel;
    btnOK: TRzButton;
    btnCancel: TRzButton;
    edtItem: TRzMemo;
    optItem: TRzRadioButton;
    optGroup: TRzRadioButton;
    procedure FormCreate(Sender: TObject);
  private
    procedure SetItem( const Item: string );
    function GetItem: string;
  public
    property Item: string
      read GetItem
      write SetItem;
  end;


implementation

{$R *.dfm}

procedure TRzCheckItemEditDlg.FormCreate(Sender: TObject);
begin
  PopupMode := pmAuto;
end;


procedure TRzCheckItemEditDlg.SetItem( const Item: string );
begin
  edtItem.Text := Item;
  edtItem.SelectAll;
end;


function TRzCheckItemEditDlg.GetItem: string;
begin
  Result := edtItem.Text;
end;


end.
