{===============================================================================
  RzLookFm Unit

  Raize Components - Form Source Unit

  Copyright © 1995-2021 by Embarcadero Technologies, Inc.  All Rights Reserved.


  Forms
  ------------------------------------------------------------------------------
  TRzLookupForm
    Form file used by TRzLookupDialog component.


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
    * No changes.
===============================================================================}

{$I RzComps.inc}

unit RzLookupForm;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  System.SysUtils,
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.Mask,
  RzEdit,
  RzLstBox,
  RzDlgBtn, 
  RzPanel;

type
  TRzLookupForm = class(TForm)
    PnlSelections: TPanel;
    LstSelections: TRzListBox;
    PnlPrompt: TPanel;
    PnlSearch: TPanel;
    EdtSearch: TRzEdit;
    PnlButtons: TRzDialogButtons;
    procedure EdtSearchChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PnlButtonsClickHelp(Sender: TObject);
    procedure EdtSearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    function FindClosest( S: string ): Integer;
  end;


implementation

{$R *.dfm}

procedure TRzLookupForm.FormCreate(Sender: TObject);
begin
  PopupMode := pmAuto;
end;


function TRzLookupForm.FindClosest( S: string ): Integer;
begin
  for Result := 0 to LstSelections.Items.Count - 1 do
    if AnsiCompareText( Copy( LstSelections.Items[ Result ], 1, Length( S ) ), S ) = 0 then
      Exit;
  Result := -1;
end;

procedure TRzLookupForm.EdtSearchChange(Sender: TObject);
var
  I: Integer;
begin
  I := FindClosest( EdtSearch.Text );
  if I <> -1 then
    LstSelections.ItemIndex := I;
end;

procedure TRzLookupForm.FormResize(Sender: TObject);
begin
  EdtSearch.Width := LstSelections.Width;
end;

procedure TRzLookupForm.FormShow(Sender: TObject);
begin
  EdtSearch.SelStart := Length( EdtSearch.Text );
end;

procedure TRzLookupForm.PnlButtonsClickHelp(Sender: TObject);
begin
  Application.HelpContext( PnlButtons.HelpContext );
end;

procedure TRzLookupForm.EdtSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ( Key = vk_Down ) or ( Key = vk_Up ) then
  begin
    if Key = vk_Down then
    begin
      if LstSelections.ItemIndex < LstSelections.Count - 1 then
        LstSelections.ItemIndex := LstSelections.ItemIndex + 1;
    end
    else
    begin
      if LstSelections.ItemIndex > 0 then
        LstSelections.ItemIndex := LstSelections.ItemIndex - 1;
    end;
    Key := 0;
  end;
end;

end.
