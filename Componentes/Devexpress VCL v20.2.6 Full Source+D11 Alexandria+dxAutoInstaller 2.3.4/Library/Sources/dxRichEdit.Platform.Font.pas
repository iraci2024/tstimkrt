{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressRichEditControl }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL }
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

unit dxRichEdit.Platform.Font; // for internal use

interface

{$I cxVer.inc}
{$I dxRichEditControl.inc}

type

  { TdxRichEditControlCompatibility }

  TdxRichEditControlCompatibility = class
  strict private
  const
    FDefaultFontName: string = 'Calibri';
    FDefaultFontSize: Single = 11.0;
    FMergeSuccessiveTables: Boolean = True;
  strict private
    class procedure SetDefaultFontName(const Value: string); static;
    class procedure SetDefaultFontSize(const Value: Single); static;
  public
    class function DefaultDoubleFontSize: Integer;

    class property DefaultFontName: string read FDefaultFontName
      write SetDefaultFontName;
    class property DefaultFontSize: Single read FDefaultFontSize
      write SetDefaultFontSize;
    class property MergeSuccessiveTables: Boolean read FMergeSuccessiveTables
      write FMergeSuccessiveTables;
  end;

implementation

uses
  SysUtils;

{ TdxRichEditControlCompatibility }

class function TdxRichEditControlCompatibility.DefaultDoubleFontSize: Integer;
begin
  Result := Trunc(DefaultFontSize * 2);
end;

class procedure TdxRichEditControlCompatibility.SetDefaultFontName
  (const Value: string);
begin
  if Value = '' then
    raise Exception.Create('Invalid argument: DefaultFontName');
  FDefaultFontName := Value;
end;

class procedure TdxRichEditControlCompatibility.SetDefaultFontSize
  (const Value: Single);
begin
  if Value < 0 then
    raise Exception.Create('Invalid argument: DefaultFontSize');
  FDefaultFontSize := Value;
end;

end.
