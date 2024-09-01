{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{           copyright (c)  2016 - 2021                               }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit LCLTMSFNCPDFLibGeneralDefault;

{$I LCLTMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, Types, LCLTMSFNCPDFCoreLibBase,
  Graphics
  {$IFNDEF LCLWEBLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  ,UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  ;

type
  TTMSFNCGeneralPDFLibFontMetrics = record
    CapHeight: Integer;
    Ascent: Integer;
    Descent: Integer;
    FontBox: TRect;
    ItalicAngle: Integer;
    Fixed: Boolean;
    TrueType: Boolean;
  end;

  TTMSFNCGeneralPDFLibInitializer = class
  public
    constructor Create;
    procedure InitializeFontFallBackList({%H-}AList: TStrings);
    destructor Destroy; override;
  end;

  TTMSFNCGeneralPDFLibFontInitializer = class
  private
    FBase: string;
    FSize: Single;
    FUsedCharArray: TTMSFNCPDFGraphicsLibUsedFontCharArray;
    FCharArray: TTMSFNCPDFGraphicsLibFontCharArray;
    FCharWidths: TTMSFNCPDFGraphicsLibFontCharWidths;
    FMainInitializer: TTMSFNCGeneralPDFLibInitializer;
    FIsFixedWidth: Boolean;
  public
    constructor Create(const AMainInitializer: TTMSFNCGeneralPDFLibInitializer; const ABase: string; const {%H-}AStyle: TFontStyles; const ASize: Single);
    destructor Destroy; override;
    function GetFontMetrics: TTMSFNCGeneralPDFLibFontMetrics;
    function GetCharArray: TTMSFNCPDFGraphicsLibFontCharArray;
    function GetCharWidths: TTMSFNCPDFGraphicsLibFontCharWidths;
    function GetUsedCharArray: TTMSFNCPDFGraphicsLibUsedFontCharArray;
    function GetGlyphIDs: TTMSFNCPDFGraphicsLibUsedFontCharArray;
    function GetUnitsPerEm: Cardinal;
    function GetTTFDataLength: Integer;
    function GetTTFDataCompressedLength: Int64;
    function GetTTFDataCompressed: TStringStream;
    procedure CompressTTFData;
    procedure InitializeCharWidths;
    procedure InitializeFontFile;
    property IsFixedWidth: Boolean read FIsFixedWidth write FIsFixedWidth;
  end;

implementation

uses
  SysUtils;

{ TTMSFNCGeneralPDFLibFontInitializer }

procedure TTMSFNCGeneralPDFLibFontInitializer.CompressTTFData;
begin

end;

constructor TTMSFNCGeneralPDFLibFontInitializer.Create(const AMainInitializer: TTMSFNCGeneralPDFLibInitializer; const ABase: string; const AStyle: TFontStyles; const ASize: Single);
begin
  FMainInitializer := AMainInitializer;
  FBase := ABase;
  FSize := ASize;
  FUsedCharArray := TTMSFNCPDFGraphicsLibUsedFontCharArray.Create;
end;

destructor TTMSFNCGeneralPDFLibFontInitializer.Destroy;
begin
  if Assigned(FUsedCharArray) then
  begin
    FUsedCharArray.Free;
    FUsedCharArray := nil;
  end;
  inherited;
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetCharArray: TTMSFNCPDFGraphicsLibFontCharArray;
begin
  Result := FCharArray;
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetCharWidths: TTMSFNCPDFGraphicsLibFontCharWidths;
begin
  Result := FCharWidths;
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetFontMetrics: TTMSFNCGeneralPDFLibFontMetrics;
begin
  Result.Ascent := 0;
  Result.Descent := 0;
  Result.FontBox := Bounds(0, 0, 0, 0);
  Result.ItalicAngle := 0;
  Result.Fixed := False;
  Result.TrueType := False;
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetGlyphIDs: TTMSFNCPDFGraphicsLibUsedFontCharArray;
begin
  Result := nil;
end;

procedure TTMSFNCGeneralPDFLibFontInitializer.InitializeCharWidths;
begin
end;

procedure TTMSFNCGeneralPDFLibFontInitializer.InitializeFontFile;
begin
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetTTFDataCompressed: TStringStream;
begin
  Result := nil;
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetTTFDataCompressedLength: Int64;
begin
  Result := 0;
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetTTFDataLength: Integer;
begin
  Result := 0;
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetUnitsPerEm: Cardinal;
begin
  Result := 0;
end;

function TTMSFNCGeneralPDFLibFontInitializer.GetUsedCharArray: TTMSFNCPDFGraphicsLibUsedFontCharArray;
begin
  Result := FUsedCharArray;
end;

{ TTMSFNCGeneralPDFLibInitializer }

constructor TTMSFNCGeneralPDFLibInitializer.Create;
begin
end;

destructor TTMSFNCGeneralPDFLibInitializer.Destroy;
begin
  inherited;
end;

procedure TTMSFNCGeneralPDFLibInitializer.InitializeFontFallBackList(
  AList: TStrings);
begin
end;

end.

