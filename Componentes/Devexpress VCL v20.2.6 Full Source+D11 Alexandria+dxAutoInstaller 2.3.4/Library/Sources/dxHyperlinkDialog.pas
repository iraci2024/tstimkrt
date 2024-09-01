{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ Express Cross Platform Library classes }
{ }
{ Copyright (c) 2000-2021 Developer Express Inc. }
{ ALL RIGHTS RESERVED }
{ }
{ The entire contents of this file is protected by U.S. and }
{ International Copyright Laws. Unauthorized reproduction, }
{ reverse-engineering, and distribution of all or any portion of }
{ the code contained in this file is strictly prohibited and may }
{ result in severe civil and criminal penalties and will be }
{ prosecuted to the maximum extent possible under the law. }
{ }
{ RESTRICTIONS }
{ }
{ THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES }
{ (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE }
{ SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS }
{ LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM }
{ ONLY. }
{ }
{ THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED }
{ FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE }
{ COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE }
{ AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT }
{ AND PERMISSION FROM DEVELOPER EXPRESS INC. }
{ }
{ CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON }
{ ADDITIONAL RESTRICTIONS. }
{ }
{ ******************************************************************** }

unit dxHyperlinkDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, Menus,
  cxContainer, cxEdit, Forms,
  dxLayoutControlAdapters, dxLayoutcxEditAdapters, Dialogs, cxClasses,
  dxLayoutLookAndFeels, dxLayoutContainer,
  cxTextEdit, cxMaskEdit, cxButtonEdit, StdCtrls, cxButtons, Classes, Controls,
  dxLayoutControl;

type

  { TdxHyperlinkDialogForm }

  TdxHyperlinkDialogForm = class(TForm)
    btnCancel: TcxButton;
    btnOK: TcxButton;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    edtAddress: TcxButtonEdit;
    edtTextToDisplay: TcxTextEdit;
    lcbtnCancel: TdxLayoutItem;
    lcbtnOK: TdxLayoutItem;
    lcEditAddress: TdxLayoutItem;
    lcEditTextToDisplay: TdxLayoutItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMainGroup14: TdxLayoutGroup;
    lcMainGroup3: TdxLayoutGroup;
    lcMainSpaceItem1: TdxLayoutEmptySpaceItem;
  strict private
    function GetAddress: string;
    function GetTextToDisplay: string;
    procedure SetAddress(const AValue: string);
    procedure SetTextToDisplay(const AValue: string);
  public
    property Address: string read GetAddress write SetAddress;
    property TextToDisplay: string read GetTextToDisplay write SetTextToDisplay;
  end;

implementation

{$R *.dfm}
{ TdxHyperlinkDialogForm }

function TdxHyperlinkDialogForm.GetAddress: string;
begin
  Result := edtAddress.Text;
end;

function TdxHyperlinkDialogForm.GetTextToDisplay: string;
begin
  Result := edtTextToDisplay.Text;
end;

procedure TdxHyperlinkDialogForm.SetAddress(const AValue: string);
begin
  edtAddress.Text := AValue;
end;

procedure TdxHyperlinkDialogForm.SetTextToDisplay(const AValue: string);
begin
  edtTextToDisplay.Text := AValue;
end;

end.
