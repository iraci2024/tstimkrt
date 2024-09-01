{===============================================================================
  RzToolbarPrefixForm Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2021 by Embarcadero Technologies, Inc.  All Rights Reserved.


  Description
  ------------------------------------------------------------------------------
  This form is used to specify a prefix or suffix to be used when naming new
  TRzToolButton and TRzSpacer components.


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
    * Added support for specifying a suffix as well as a prefix.
===============================================================================}

{$I RzComps.inc}

unit RzToolbarPrefixForm;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Mask,
  RzButton,
  RzEdit,
  RzRadChk,
  RzLabel;

type
  TRzFrmPrefixSuffix = class(TForm)
    LblPrefix: TRzLabel;
    OptPrefix: TRzRadioButton;
    OptSuffix: TRzRadioButton;
    EdtPrefix: TRzEdit;
    BtnOK: TRzButton;
    BtnCancel: TRzButton;
    procedure OptPrefixClick(Sender: TObject);
    procedure OptSuffixClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

procedure TRzFrmPrefixSuffix.FormCreate(Sender: TObject);
begin
  PopupMode := pmAuto;
end;


procedure TRzFrmPrefixSuffix.OptPrefixClick(Sender: TObject);
begin
  LblPrefix.Caption := 'Prefix';
end;


procedure TRzFrmPrefixSuffix.OptSuffixClick(Sender: TObject);
begin
  LblPrefix.Caption := 'Suffix';
end;


end.
