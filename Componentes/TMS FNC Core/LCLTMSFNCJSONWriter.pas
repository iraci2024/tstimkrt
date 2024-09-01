{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2017 - 2021                               }
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

unit LCLTMSFNCJSONWriter;

{$I LCLTMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

{$INLINE ON}{$R-}{$Q-}

interface

uses
  {$IFNDEF LCLWEBLIB}
  Generics.Collections,
  {$ENDIF}
  Classes, SysUtils, LCLTMSFNCTypes;

type
  TTMSFNCJSONStreamWriter = class
  private
  var
    FStream: TStream;
    FWriteStream: TStringStream;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    procedure Write(const Value: string);
  end;

  TTMSFNCJSONWriter = class
  public
    type
      ECannotWriteName = class(Exception)
      public
        constructor Create;
      end;
      EMultipleRootNotAllowed = class(Exception)
      public
        constructor Create;
      end;
      EObjectOrArrayExpected = class(Exception)
      public
        constructor Create;
      end;
      EInvalidNesting = class(Exception)
      public
        constructor Create;
      end;
      EMissingValue = class(Exception)
      public
        constructor Create;
      end;
      ETooManyDepthLevels = class(Exception)
      public
        constructor Create;
      end;
      EEmptyJson = class(Exception)
      public
        constructor Create;
      end;
      EEmptyName = class(Exception)
      public
        constructor Create;
      end;
  private
    type
      TTMSFNCJSONScope = (jscEmptyDocument, jscEmptyArray, jscEmptyObject, jscNonEmptyDocument,
        jscNonEmptyArray, jscNonEmptyObject, jscDanglingName);
    const
      MaxStackSize = 255;
  private
    FWriter: TTMSFNCJSONStreamWriter;
    FStack: array[0..MaxStackSize] of TTMSFNCJSONScope;
    FStackSize: integer;
    FIndent: string;
    FSeparator: string;
    FDeferredName: string;
    procedure SetIndentLength(const Value: integer);
    function GetIndentLength: integer;
    function OpenItem(const Empty: TTMSFNCJSONScope; const OpenBracket: string): TTMSFNCJSONWriter;
    function CloseItem(const Empty, NonEmpty: TTMSFNCJSONScope; const CloseBracket: string): TTMSFNCJSONWriter;
    procedure PushScope(const Scope: TTMSFNCJSONScope); inline;
    function PeekScope: TTMSFNCJSONScope; inline;
    procedure ReplaceTop(const Scope: TTMSFNCJSONScope); inline;
    procedure WriteDeferredName; inline;
    procedure InternalWriteString(const Value: string);
    procedure NewLine; inline;
    procedure BeforeName;
    procedure BeforeValue(const Root: boolean);
  public
    constructor Create(const AStream: TStream);
    destructor Destroy; override;
    function WriteBeginArray: TTMSFNCJSONWriter;
    function WriteEndArray: TTMSFNCJSONWriter;
    function WriteBeginObject: TTMSFNCJSONWriter;
    function WriteEndObject: TTMSFNCJSONWriter;
    function WriteName(const Name: string): TTMSFNCJSONWriter;
    function WriteString(const Value: string): TTMSFNCJSONWriter;
    function WriteRawString(const Value: string): TTMSFNCJSONWriter;
    function WriteBoolean(const Value: boolean): TTMSFNCJSONWriter;
    function WriteNull: TTMSFNCJSONWriter;
    function WriteDouble(const Value: double): TTMSFNCJSONWriter;
    function WriteInteger(const Value: Int64): TTMSFNCJSONWriter;
    procedure Close;
    property IndentLength: integer read GetIndentLength write SetIndentLength;
  end;

implementation

uses
  LCLTMSFNCUtils;

{ TTMSFNCJSONWriter }

procedure TTMSFNCJSONWriter.BeforeName;
begin
  case PeekScope of
    TTMSFNCJSONScope.jscNonEmptyObject: FWriter.Write(',');
    TTMSFNCJSONScope.jscEmptyObject: ;
  else
    raise ECannotWriteName.Create;
  end;
  NewLine;
  ReplaceTop(TTMSFNCJSONScope.jscDanglingName);
end;

procedure TTMSFNCJSONWriter.BeforeValue(const Root: boolean);
begin
  case PeekScope of
    TTMSFNCJSONScope.jscNonEmptyDocument:
      raise EMultipleRootNotAllowed.Create;
    TTMSFNCJSONScope.jscEmptyDocument:
      begin
        if not Root then
           raise EObjectOrArrayExpected.Create;
        ReplaceTop(TTMSFNCJSONScope.jscNonEmptyDocument);
      end;
    TTMSFNCJSONScope.jscEmptyArray:
      begin
        ReplaceTop(TTMSFNCJSONScope.jscNonEmptyArray);
        NewLine;
      end;
    TTMSFNCJSONScope.jscNonEmptyArray:
      begin
        FWriter.Write(',');
        NewLine;
      end;
    TTMSFNCJSONScope.jscDanglingName:
      begin
        FWriter.Write(FSeparator);
        ReplaceTop(TTMSFNCJSONScope.jscNonEmptyObject);
      end;
  else
    raise EInvalidNesting.Create;
  end;
end;

function TTMSFNCJSONWriter.CloseItem(const Empty, NonEmpty: TTMSFNCJSONScope;
  const CloseBracket: string): TTMSFNCJSONWriter;
var
  Context: TTMSFNCJSONScope;
begin
  Context := PeekScope;
  if not (Context in [Empty, NonEmpty]) then
    raise EInvalidNesting.Create;
  if FDeferredName <> '' then
    raise EMissingValue.Create;
  Dec(FStackSize);
  if Context = NonEmpty then
    NewLine;
  FWriter.Write(CloseBracket);
  Result := Self;
end;

procedure TTMSFNCJSONWriter.Close;
begin
  if (FStackSize > 1) or ((FStackSize = 1) and (PeekScope <> TTMSFNCJSONScope.jscNonEmptyDocument)) then
    raise EInvalidNesting.Create;
end;

constructor TTMSFNCJSONWriter.Create(const aStream: TStream);
begin
  inherited Create;
  FWriter := TTMSFNCJSONStreamWriter.Create(aStream);
  FSeparator := ':';
  PushScope(TTMSFNCJSONScope.jscEmptyDocument);
end;

destructor TTMSFNCJSONWriter.Destroy;
begin
  FWriter.Free;
  inherited;
end;

procedure TTMSFNCJSONWriter.NewLine;
var
  I: integer;
begin
  if FIndent <> '' then
  begin
    FWriter.Write(#13#10);
    for I := 1 to FStackSize - 1 do
      FWriter.Write(FIndent);
  end;
end;

function TTMSFNCJSONWriter.OpenItem(const Empty: TTMSFNCJSONScope;
  const OpenBracket: string): TTMSFNCJSONWriter;
begin
  BeforeValue(true);
  PushScope(Empty);
  FWriter.Write(OpenBracket);
  Result := Self;
end;

function TTMSFNCJSONWriter.PeekScope: TTMSFNCJSONScope;
begin
  if FStackSize = 0 then
    raise EEmptyJson.Create;
  Result := FStack[FStackSize - 1];
end;

procedure TTMSFNCJSONWriter.PushScope(const Scope: TTMSFNCJSONScope);
begin
  if FStackSize > MaxStackSize then
    raise ETooManyDepthLevels.Create;
  FStack[FStackSize] := Scope;
  Inc(FStackSize);
end;

procedure TTMSFNCJSONWriter.ReplaceTop(const Scope: TTMSFNCJSONScope);
begin
  if FStackSize = 0 then
    raise EEmptyJson.Create;
  FStack[FStackSize - 1] := Scope;
end;

procedure TTMSFNCJSONWriter.SetIndentLength(const Value: integer);
begin
  if Value <= 0 then
  begin
    FIndent := '';
    FSeparator := ':';
  end else
  begin
    FIndent := StringOfChar(#32, Value);
    FSeparator := ': ';
  end;
end;

function TTMSFNCJSONWriter.GetIndentLength: integer;
begin
  Result := Length(FIndent);
end;

procedure TTMSFNCJSONWriter.InternalWriteString(const Value: string);
begin
  FWriter.Write('"');
  FWriter.Write(TTMSFNCUtils.EscapeString(Value));
  FWriter.Write('"');
end;

function TTMSFNCJSONWriter.WriteString(const Value: string): TTMSFNCJSONWriter;
begin
  WriteDeferredName;
  BeforeValue(false);
  InternalWriteString(Value);
  Result := Self;
end;

function TTMSFNCJSONWriter.WriteEndArray: TTMSFNCJSONWriter;
begin
  Result := CloseItem(TTMSFNCJSONScope.jscEmptyArray, TTMSFNCJSONScope.jscNonEmptyArray, ']');
end;

function TTMSFNCJSONWriter.WriteEndObject: TTMSFNCJSONWriter;
begin
  Result := CloseItem(TTMSFNCJSONScope.jscEmptyObject, TTMSFNCJSONScope.jscNonEmptyObject, '}');
end;

function TTMSFNCJSONWriter.WriteBeginArray: TTMSFNCJSONWriter;
begin
  WriteDeferredName;
  Result := OpenItem(TTMSFNCJSONScope.jscEmptyArray, '[');
end;

function TTMSFNCJSONWriter.WriteBeginObject: TTMSFNCJSONWriter;
begin
  WriteDeferredName;
  Result := OpenItem(TTMSFNCJSONScope.jscEmptyObject, '{');
end;

function TTMSFNCJSONWriter.WriteBoolean(const Value: boolean): TTMSFNCJSONWriter;
begin
  WriteDeferredName;
  BeforeValue(false);
  if Value then
    FWriter.Write('true')
  else
    FWriter.Write('false');
    Result := Self;
end;

procedure TTMSFNCJSONWriter.WriteDeferredName;
begin
  if FDeferredName <> '' then
  begin
    BeforeName;
    InternalWriteString(FDeferredName);
    FDeferredName := '';
  end;
end;

function TTMSFNCJSONWriter.WriteDouble(const Value: double): TTMSFNCJSONWriter;
begin
  WriteDeferredName;
  BeforeValue(false);
  FWriter.Write(FloatToStr(Value));
  Result := Self;
end;

function TTMSFNCJSONWriter.WriteName(const Name: string): TTMSFNCJSONWriter;
begin
  if Name = '' then
    raise EEmptyName.Create;
  if FDeferredName <> '' then
    raise EMissingValue.Create;
  if FStackSize = 0 then
    raise EEmptyJson.Create;
  FDeferredName := Name;
  Result := Self;
end;

function TTMSFNCJSONWriter.WriteNull: TTMSFNCJSONWriter;
begin
  WriteDeferredName;
  BeforeValue(false);
  FWriter.Write('null');
  Result := Self;
end;

function TTMSFNCJSONWriter.WriteRawString(const Value: string): TTMSFNCJSONWriter;
begin
  WriteDeferredName;
  BeforeValue(false);
  FWriter.Write('"');
  FWriter.Write(Value);
  FWriter.Write('"');
  Result := Self;
end;

function TTMSFNCJSONWriter.WriteInteger(const Value: Int64): TTMSFNCJSONWriter;
begin
  WriteDeferredName;
  BeforeValue(false);
  FWriter.Write(IntToStr(Value));
  Result := Self;
end;

{ TTMSFNCJSONStreamWriter }

constructor TTMSFNCJSONStreamWriter.Create(aStream: TStream);
begin
  FStream := aStream;
  FWriteStream := TStringStream.Create(''{$IFDEF WEBLIB}, TEncoding.Ansi{$ENDIF});
end;

destructor TTMSFNCJSONStreamWriter.Destroy;
begin
  try
    FWriteStream.Position := 0;
    FStream.CopyFrom(FWritestream, FWriteStream.Size);
  finally
    FWriteStream.Free;
  end;
  inherited;
end;

procedure TTMSFNCJSONStreamWriter.Write(const Value: string);
begin
  FWriteStream.WriteString(Value);
end;

{ TTMSFNCJSONWriter.ECannotWriteName }

constructor TTMSFNCJSONWriter.ECannotWriteName.Create;
begin
  inherited Create('Cannot write name in current Json scope');
end;

{ TTMSFNCJSONWriter.EMultipleRootNotAllowed }

constructor TTMSFNCJSONWriter.EMultipleRootNotAllowed.Create;
begin
  inherited Create('Multiple root values not allowed');
end;

{ TTMSFNCJSONWriter.EObjectOrArrayExpected }

constructor TTMSFNCJSONWriter.EObjectOrArrayExpected.Create;
begin
  inherited Create('Object or array expected as top-level value');
end;

{ TTMSFNCJSONWriter.EInvalidNesting }

constructor TTMSFNCJSONWriter.EInvalidNesting.Create;
begin
  inherited Create('Invalid nesting. Not all arrays/objects were properly closed.');
end;

{ TTMSFNCJSONWriter.EMissingValue }

constructor TTMSFNCJSONWriter.EMissingValue.Create;
begin
  inherited Create('Json value missing');
end;

{ TTMSFNCJSONWriter.ETooManyDepthLevels }

constructor TTMSFNCJSONWriter.ETooManyDepthLevels.Create;
begin
  inherited Create('Maximum level of nested structured reached.');
end;

{ TTMSFNCJSONWriter.EEmptyJson }

constructor TTMSFNCJSONWriter.EEmptyJson.Create;
begin
  inherited Create('Json is still empty. Cannot perform operation.');
end;

{ TTMSFNCJSONWriter.EEmptyName }

constructor TTMSFNCJSONWriter.EEmptyName.Create;
begin
  inherited Create('Cannot write empty name');
end;

end.
