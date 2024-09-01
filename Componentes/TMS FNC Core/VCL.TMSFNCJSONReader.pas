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

unit VCL.TMSFNCJSONReader;

{$I VCL.TMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

{$SCOPEDENUMS ON}
{$INLINE ON}{$R-}{$Q-}

interface

uses
  {$IFNDEF LCLWEBLIB}
  Generics.Collections,
  {$ENDIF}
  Classes, SysUtils, VCL.TMSFNCTypes;

type
  TTMSFNCJSONStreamReader = class
  public
    type
      EInvalidJsonInput = class(Exception)
      public
        constructor Create;
      end;
      EInternalError = class(Exception)
      public
        constructor Create;
      end;
      EEndOfInputReached = class(Exception)
      public
        constructor Create;
      end;
  private
  var
    FStream: TStream;
    FReadStream: TStringStream;
  public
    constructor Create(const aStream: TStream);
    destructor Destroy; override;
    function NextChar: char; inline;
    function PeekChar: char; inline;
    function ReadChar: char; inline;
    procedure Backup(const {%H-}c: char);
    procedure MoveNext(const Count: integer = 1); inline;
    function Eof: boolean; inline;
  end;

  TTMSFNCJSONToken = (jstoBeginObject, jstoEndObject, jstoBeginArray, jstoEndArray,
    jstoName, jstoBoolean, jstoNull, jstoText, jstoNumber, jstoEOF);

  TTMSFNCJSONReader = class
  private
    type
      TTMSFNCJSONState = (jstNone, jstBeginObject, jstEndObject, jstBeginArray, jstEndArray, jstTrue, jstFalse,
        jstNull, jstDoubleQuoted, jstBuffered, jstDoubleQuotedName, jstInt64, jstNumber, jstEOF);
      TTMSFNCJSONNumberState = (jnstNone, jnstSign, jnstDigit, jnstDecimal, jnstFraction, jnstExpE, jnstExpSign, jnstExpDigit);
      TTMSFNCJSONScope = (jscEmptyDocument, jscEmptyArray, jscEmptyObject, jscNonEmptyDocument,
        jscNonEmptyArray, jscNonEmptyObject, jscDanglingName);
  public
    type
      EInvalidStateException = class(Exception)
      public
        constructor Create(const AState: TTMSFNCJSONState);
      end;
      EUnterminatedArray = class(Exception)
      public
        constructor Create;
      end;
      EUnterminatedObject = class(Exception)
      public
        constructor Create;
      end;
      ENameExpected = class(Exception)
      public
        constructor Create;
      end;
      EColonExpected = class(Exception)
      public
        constructor Create;
      end;
      EReaderClosed = class(Exception)
      public
        constructor Create;
      end;
      EMultipleRootNotAllowed = class(Exception)
      public
        constructor Create;
      end;
      EExpectedValue = class(Exception)
      public
        constructor Create;
      end;
      EObjectOrArrayExpected = class(Exception)
      public
        constructor Create;
      end;
      ETooManyDepthLevels = class(Exception)
      public
        constructor Create;
      end;
      EInvalidEscaped = class(Exception)
      public
        constructor Create;
      end;
    const
      MaxNumberBuffer = 255;
      MaxStackSize = 255;
  private
    FReader: TTMSFNCJSONStreamReader;
    FStack: array[0..MaxStackSize] of TTMSFNCJSONScope;
    FStackSize: integer;
    FPeeked: TTMSFNCJSONState;
    FPeekedInt64: Int64;
    FPeekedNumber: array[0..MaxNumberBuffer] of Char;
    FPeekedString: string;
    function NextPeek: TTMSFNCJSONState; inline;
    procedure CheckState(const State: TTMSFNCJSONState); inline;
    procedure SkipChar;
    function IsLiteral(C: Char): boolean;
    function IsDigit(C: Char): boolean; inline;
    function DoPeek: TTMSFNCJSONState;
    procedure PushScope(const Scope: TTMSFNCJSONScope);
    function NextNonWhitespace: Char;
    function ReadChar: Char;
    function PeekKeyword: TTMSFNCJSONState;
    function PeekNumber: TTMSFNCJSONState;
    function InternalReadQuoted(const BuildString: boolean): string;
    function ReadQuoted: string;
    procedure SkipQuoted;
  private
    function SkipWhitespaceUntilEnd: boolean;
  public
    constructor Create(const AStream: TStream);
    destructor Destroy; override;
    procedure ReadBeginArray;
    procedure ReadEndArray;
    procedure ReadBeginObject;
    procedure ReadEndObject;
    function PeekName: string;
    function ReadName: string;
    function ReadString: string;
    function ReadBoolean: boolean;
    function ReadDouble: double;
    function ReadInt64: Int64;
    function ReadInteger: integer;
    procedure SkipValue;
    procedure ReadNull;
    function HasNext: boolean;
    function Peek: TTMSFNCJSONToken;
    function IsNull: boolean;
    function Eof: boolean;
  end;

implementation

uses
  VCL.TMSFNCUtils;

const
  Wspace: Set of byte = [$20, $A, $D, $9, $C];

{$IFNDEF LCLWEBLIB}
const
  FPC_FULLVERSION = 0;
{$ENDIF}

function ArrayOfCharToString(AArray: array of char): string;
{$IFDEF WEBLIB}
var
  I: Integer;
{$ENDIF}
begin
  {$IFDEF WEBLIB}
  Result := '';
  for I := 0 to Length(AArray) - 1 do
  begin
    if AArray[I] = #0 then
      Break;

    Result := Result + AArray[I];
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  Result := string(AArray);
  {$ENDIF}
end;

{ TTMSFNCJSONReader }

constructor TTMSFNCJSONReader.Create(const aStream: TStream);
begin
  inherited Create;
  FReader := TTMSFNCJSONStreamReader.Create(aStream);
  FPeeked := TTMSFNCJSONState.jstNone;
  FStack[0] := TTMSFNCJSONScope.jscEmptyDocument;
  FStackSize := 1;
end;

destructor TTMSFNCJSONReader.Destroy;
begin
  FReader.Free;
  inherited;
end;

function TTMSFNCJSONReader.Eof: boolean;
begin
  Result := (Peek = TTMSFNCJSONToken.jstoEOF);
end;

function TTMSFNCJSONReader.Peek: TTMSFNCJSONToken;
begin
  case NextPeek of
    TTMSFNCJSONState.jstBeginObject:
      Result := TTMSFNCJSONToken.jstoBeginObject;

    TTMSFNCJSONState.jstEndObject:
      Result := TTMSFNCJSONToken.jstoEndObject;

    TTMSFNCJSONState.jstBeginArray:
      Result := TTMSFNCJSONToken.jstoBeginArray;

    TTMSFNCJSONState.jstEndArray:
      Result := TTMSFNCJSONToken.jstoEndArray;

    TTMSFNCJSONState.jstDoubleQuotedName:
      Result := TTMSFNCJSONToken.jstoName;

    TTMSFNCJSONState.jstTrue,
    TTMSFNCJSONState.jstFalse:
      Result := TTMSFNCJSONToken.jstoBoolean;

    TTMSFNCJSONState.jstNull:
      Result := TTMSFNCJSONToken.jstoNull;

    TTMSFNCJSONState.jstDoubleQuoted,
    TTMSFNCJSONState.jstBuffered:
      Result := TTMSFNCJSONToken.jstoText;

    TTMSFNCJSONState.jstInt64,
    TTMSFNCJSONState.jstNumber:
      Result := TTMSFNCJSONToken.jstoNumber;

    TTMSFNCJSONState.jstEOF:
      Result := TTMSFNCJSONToken.jstoEOF;
  else
    Assert(false);
    Result := TTMSFNCJSONToken.jstoEOF;
  end;
end;

function TTMSFNCJSONReader.PeekKeyword: TTMSFNCJSONState;
begin
  case FReader.PeekChar of
    't', 'T':
      begin
        FReader.MoveNext;
        case FReader.NextChar of
          'r', 'R': case FReader.NextChar of
            'u', 'U': case FReader.NextChar of
              'e', 'E':
                if FReader.EOF or not IsLiteral(FReader.PeekChar) then
                  Exit(TTMSFNCJSONState.jstTrue);
            end;
          end;
        end;
      end;
    'f', 'F':
      begin
        FReader.MoveNext;
        case FReader.NextChar of
          'a', 'A': case FReader.NextChar of
            'l', 'L': case FReader.NextChar of
              's', 'S': case FReader.NextChar of
                'e', 'E':
                  if FReader.EOF or not IsLiteral(FReader.PeekChar) then
                    Exit(TTMSFNCJSONState.jstFalse);
              end;
            end;
          end;
        end;
      end;
    'n', 'N':
      begin
        FReader.MoveNext;
        case FReader.NextChar of
          'u', 'U': case FReader.NextChar of
            'l', 'L': case FReader.NextChar of
              'l', 'L':
                if FReader.EOF or not IsLiteral(FReader.PeekChar) then
                  Exit(TTMSFNCJSONState.jstNull);
            end;
          end;
        end;
      end;
  else
    Exit(TTMSFNCJSONState.jstNone);
  end;
  raise EExpectedValue.Create;
end;

function TTMSFNCJSONReader.PeekName: string;
var
  p: Int64;
begin
  p := FReader.FReadStream.Position;
  Result := ReadQuoted;
  FReader.FReadStream.Position := p;
end;

function TTMSFNCJSONReader.PeekNumber: TTMSFNCJSONState;
const
  MinIncompleteInteger = Low(Int64) div 10;
var
  Last: TTMSFNCJSONNumberState;
  Negative: boolean;
  FitsInInt64: boolean;
  Value: Int64;
  NewValue: Int64;
  C: Char;
  BufIndex: integer;
begin
  C := FReader.PeekChar;
  if (C <> '-') and not IsDigit(C) then
    Exit(TTMSFNCJSONState.jstNone);

  Negative := false;
  FitsInInt64 := true;
  Last := TTMSFNCJSONNumberState.jnstNone;
  BufIndex := 0;
  Value := -1;
  repeat
    if BufIndex >= MaxNumberBuffer then
      raise EExpectedValue.Create;
    C := FReader.NextChar;
    FPeekedNumber[BufIndex] := C;
    Inc(BufIndex);
    case C of
      '-':
        if Last = TTMSFNCJSONNumberState.jnstNone then
        begin
          Negative := true;
          Last := TTMSFNCJSONNumberState.jnstSign;
          Continue;
        end
        else
        if Last = TTMSFNCJSONNumberState.jnstExpE then
        begin
          Last := TTMSFNCJSONNumberState.jnstExpSign;
          Continue;
        end
        else
          raise EExpectedValue.Create;
      '+':
        if Last = TTMSFNCJSONNumberState.jnstExpE then
        begin
          Last := TTMSFNCJSONNumberState.jnstExpSign;
          Continue;
        end
        else
          raise EExpectedValue.Create;
      'e', 'E':
        if Last in [TTMSFNCJSONNumberState.jnstDigit, TTMSFNCJSONNumberState.jnstFraction] then
        begin
          Last := TTMSFNCJSONNumberState.jnstExpE;
          Continue;
        end
        else
          raise EExpectedValue.Create;
      '.':
        if Last = TTMSFNCJSONNumberState.jnstDigit then
        begin
          Last := TTMSFNCJSONNumberState.jnstDecimal;
          Continue;
        end
        else
          raise EExpectedValue.Create;
    else
      if not IsDigit(C) then
        if not IsLiteral(C) then
        begin
          FReader.Backup(C);
          Dec(BufIndex);
          Break;
        end
        else
          raise EExpectedValue.Create;

      if Last in [TTMSFNCJSONNumberState.jnstSign, TTMSFNCJSONNumberState.jnstNone] then
      begin
        Value := -(Ord(C) - 48);
        Last := TTMSFNCJSONNumberState.jnstDigit
      end
      else
      if Last = TTMSFNCJSONNumberState.jnstDigit then
      begin
        if Value = 0 then
          raise EExpectedValue.Create;
        NewValue := Value * 10 - (Ord(C) - 48);
        FitsInInt64 := FitsInInt64 and (
            (Value > MinIncompleteInteger)
             or ((Value = MinIncompleteInteger) and (NewValue < Value))
          );
        Value := NewValue;
      end
      else
      if Last = TTMSFNCJSONNumberState.jnstDecimal then
        Last := TTMSFNCJSONNumberState.jnstFraction
      else
      if Last in [TTMSFNCJSONNumberState.jnstExpE, TTMSFNCJSONNumberState.jnstExpSign] then
        Last := TTMSFNCJSONNumberState.jnstExpDigit;
    end;
  until false;

  if (Last = TTMSFNCJSONNumberState.jnstDigit) and FitsInInt64 and ((Value <> Low(Int64)) or Negative) then
  begin
    if Negative then
      FPeekedInt64 := Value
    else
      FPeekedInt64 := -Value;
    Exit(TTMSFNCJSONState.jstInt64);
  end
  else
  if Last in [TTMSFNCJSONNumberState.jnstDigit, TTMSFNCJSONNumberState.jnstFraction, TTMSFNCJSONNumberState.jnstExpDigit] then
  begin
    FPeekedNumber[BufIndex] := #0;
    Exit(TTMSFNCJSONState.jstNumber);
  end
  else
    raise EExpectedValue.Create;
end;

procedure TTMSFNCJSONReader.PushScope(const Scope: TTMSFNCJSONScope);
begin
  if FStackSize > MaxStackSize then
    raise ETooManyDepthLevels.Create;
  FStack[FStackSize] := Scope;
  Inc(FStackSize);
end;

function TTMSFNCJSONReader.HasNext: boolean;
begin
  Result := not (NextPeek in [TTMSFNCJSONState.jstEndObject, TTMSFNCJSONState.jstEndArray]);
end;

function TTMSFNCJSONReader.IsDigit(C: Char): boolean;
begin
  Result := (C <= #255) and CharIsNumber(C);
end;

function TTMSFNCJSONReader.IsLiteral(C: Char): boolean;
begin
  Result := not TTMSFNCUtils.CharInSet(C, TTMSFNCUtils.CreateCharSet('/\;#{}[]:,'' '#13#10#12#9));
end;

function TTMSFNCJSONReader.IsNull: boolean;
begin
  Result := (Peek = TTMSFNCJSONToken.jstoNull);
end;

function TTMSFNCJSONReader.NextNonWhitespace: Char;
var
  s: Char;
  p: Int64;
begin
  p := FReader.FReadStream.Position;
  Result := #0;
  s := ReadChar;
  repeat
    if (s > #32) or not (Ord(s) in Wspace) then
    begin
      FReader.FReadStream.Position := p;
      Exit(s);
    end;

    s := ReadChar;
  until FReader.Eof;
  FReader.FReadStream.Position := p;
end;

function TTMSFNCJSONReader.NextPeek: TTMSFNCJSONState;
begin
  if FPeeked = TTMSFNCJSONState.jstNone then
    FPeeked := DoPeek;
  Result := FPeeked;
end;

procedure TTMSFNCJSONReader.SkipQuoted;
begin
  InternalReadQuoted(false);
end;

procedure TTMSFNCJSONReader.SkipValue;
var
  Count: integer;
begin
  Count := 0;
  repeat
    case NextPeek of
      TTMSFNCJSONState.jstBeginArray:
        begin
          PushScope(TTMSFNCJSONScope.jscEmptyArray);
          Inc(Count);
        end;
      TTMSFNCJSONState.jstBeginObject:
        begin
          PushScope(TTMSFNCJSONScope.jscEmptyObject);
          Inc(Count);
        end;
      TTMSFNCJSONState.jstEndArray, TTMSFNCJSONState.jstEndObject:
        begin
          Dec(FStackSize);
          Dec(Count);
        end;
      TTMSFNCJSONState.jstDoubleQuoted, TTMSFNCJSONState.jstDoubleQuotedName:
        SkipQuoted;
    end;
    FPeeked := TTMSFNCJSONState.jstNone;
  until Count <= 0;
end;

procedure TTMSFNCJSONReader.SkipChar;
begin
  FReader.MoveNext;
  while Ord(FReader.PeekChar) in Wspace do
    FReader.MoveNext;
end;

function TTMSFNCJSONReader.DoPeek: TTMSFNCJSONState;
var
  FPeekStack: TTMSFNCJSONScope;
  C: Char;
begin
  FPeekStack := FStack[FStackSize - 1];
  if FPeekStack = TTMSFNCJSONScope.jscEmptyArray then
    FStack[FStackSize - 1] := TTMSFNCJSONScope.jscNonEmptyArray
  else
  if FPeekStack = TTMSFNCJSONScope.jscNonEmptyArray then
  begin
    C := NextNonWhitespace;
    case C of
      ']':
        begin
          SkipChar;
          FPeeked := TTMSFNCJSONState.jstEndArray;
          Exit(FPeeked);
        end;
      ',':
         SkipChar;
    else
      raise EUnterminatedArray.Create;
    end;
  end
  else
  if FPeekStack in [TTMSFNCJSONScope.jscEmptyObject, TTMSFNCJSONScope.jscNonEmptyObject] then
  begin
    FStack[FStackSize - 1] := TTMSFNCJSONScope.jscDanglingName;
    if FPeekStack = TTMSFNCJSONScope.jscNonEmptyObject then
    begin
      C := NextNonWhitespace;
      case C of
        '}':
          begin
            while Ord(FReader.PeekChar) in Wspace do
              FReader.MoveNext;

            SkipChar;
            FPeeked := TTMSFNCJSONState.jstEndObject;
            Exit(FPeeked);
          end;
        ',': SkipChar;
      else
        raise EUnterminatedObject.Create;
      end;
    end;
    C := NextNonWhitespace;
    case C of
      '"':
        begin
          SkipChar;
          FPeeked := TTMSFNCJSONState.jstDoubleQuotedName;
          Exit(FPeeked);
        end;
      '}':
        if FPeekStack <> TTMSFNCJSONScope.jscNonEmptyObject then
        begin
          SkipChar;
          FPeeked := TTMSFNCJSONState.jstEndObject;
          Exit(FPeeked);
        end else
          raise ENameExpected.Create;
    else
      raise ENameExpected.Create;
    end;
  end
  else
  if FPeekStack = TTMSFNCJSONScope.jscDanglingName then
  begin
    FStack[FStackSize - 1] := TTMSFNCJSONScope.jscNonEmptyObject;
    C := NextNonWhitespace;
    if C = ':' then
      SkipChar
    else
      raise EColonExpected.Create;
  end
  else
  if FPeekStack = TTMSFNCJSONScope.jscEmptyDocument then
    FStack[FStackSize - 1] := TTMSFNCJSONScope.jscNonEmptyDocument
  else
  if FPeekStack = TTMSFNCJSONScope.jscNonEmptyDocument then
  begin
    if SkipWhitespaceUntilEnd then
    begin
      FPeeked := TTMSFNCJSONState.jstEOF;
      Exit(FPeeked);
    end else
      raise EMultipleRootNotAllowed.Create;
  end;

  C := NextNonWhitespace;
  case C of
    ']':
      if FPeekStack = TTMSFNCJSONScope.jscEmptyArray then
      begin
        SkipChar;
        FPeeked := TTMSFNCJSONState.jstEndArray;
        Exit(FPeeked);
      end else
        raise EExpectedValue.Create;
    '"':
      begin
        if FStackSize = 1 then
          raise EObjectOrArrayExpected.Create;
        SkipChar;
        FPeeked := TTMSFNCJSONState.jstDoubleQuoted;
        Exit(FPeeked);
      end;
    '[':
      begin
        SkipChar;
        FPeeked := TTMSFNCJSONState.jstBeginArray;
        Exit(FPeeked);
      end;
    '{':
      begin
        SkipChar;
        FPeeked := TTMSFNCJSONState.jstBeginObject;
        Exit(FPeeked);
      end;
  end;

  if FStackSize = 1 then
    raise EObjectOrArrayExpected.Create;

  Result := PeekKeyword;
  if Result <> TTMSFNCJSONState.jstNone then
    Exit;
  Result := PeekNumber;
  if Result <> TTMSFNCJSONState.jstNone then
    Exit;
  raise EExpectedValue.Create;
end;

function TTMSFNCJSONReader.SkipWhitespaceUntilEnd: boolean;
var
  s: Char;
  p: Int64;
begin
  p := FReader.FReadStream.Position;
  Result := True;
  s := ReadChar;
  repeat
    if (s > #32) or not (Ord(s) in Wspace) then
    begin
      FReader.FReadStream.Position := p;
      Exit(false);
    end;

    s := ReadChar;
  until FReader.Eof;
  FReader.FReadStream.Position := p;
end;

procedure TTMSFNCJSONReader.CheckState(const State: TTMSFNCJSONState);
begin
  if NextPeek <> State then
    raise EInvalidStateException.Create(State);
end;

procedure TTMSFNCJSONReader.ReadBeginArray;
begin
  CheckState(TTMSFNCJSONState.jstBeginArray);
  PushScope(TTMSFNCJSONScope.jscEmptyArray);
  FPeeked := TTMSFNCJSONState.jstNone;
end;

procedure TTMSFNCJSONReader.ReadEndArray;
begin
  CheckState(TTMSFNCJSONState.jstEndArray);
  Dec(FStackSize);
  FPeeked := TTMSFNCJSONState.jstNone;
end;

function TTMSFNCJSONReader.ReadBoolean: boolean;
begin
  case NextPeek of
    TTMSFNCJSONState.jstTrue:
      Result := True;
    TTMSFNCJSONState.jstFalse:
      Result := False;
  else
    raise EInvalidStateException.Create(TTMSFNCJSONState.jstTrue);
  end;
  FPeeked := TTMSFNCJSONState.jstNone;
end;

function TTMSFNCJSONReader.ReadChar: Char;
begin
  Result := FReader.ReadChar;
end;

function TTMSFNCJSONReader.ReadDouble: double;
begin
  case NextPeek of
    TTMSFNCJSONState.jstInt64:
      begin
        FPeeked := TTMSFNCJSONState.jstNone;
        Exit(FPeekedInt64);
      end;
    TTMSFNCJSONState.jstNumber:
      begin
        if TryStrToFloat(ArrayOfCharToString(FPeekedNumber), Result) then
        begin
          FPeeked := TTMSFNCJSONState.jstNone;
          Exit;
        end else
          FPeekedString := ArrayOfCharToString(FPeekedNumber);
      end;
    TTMSFNCJSONState.jstDoubleQuoted:
      FPeekedString := ReadQuoted;
    TTMSFNCJSONState.jstBuffered: ;
  else
    raise EInvalidStateException.Create(TTMSFNCJSONState.jstNumber);
  end;

  FPeeked := TTMSFNCJSONState.jstBuffered;
  Result := StrToFloat(FPeekedString);
  FPeekedString := '';
  FPeeked := TTMSFNCJSONState.jstNone;
end;

function TTMSFNCJSONReader.ReadInt64: Int64;
var
  AsDouble: double;
begin
  case NextPeek of
    TTMSFNCJSONState.jstInt64:
      begin
        FPeeked := TTMSFNCJSONState.jstNone;
        Exit(FPeekedInt64);
      end;
    TTMSFNCJSONState.jstNumber:
      begin
        if TryStrToInt64(ArrayOfCharToString(FPeekedNumber), Result) then
        begin
          FPeeked := TTMSFNCJSONState.jstNone;
          Exit;
        end else
          FPeekedString := ArrayOfCharToString(FPeekedNumber);
      end;
    TTMSFNCJSONState.jstDoubleQuoted:
      begin
        FPeekedString := ReadQuoted;
        if TryStrToInt64(FPeekedString, Result) then
        begin
          FPeeked := TTMSFNCJSONState.jstNone;
          Exit;
        end;
      end;
    TTMSFNCJSONState.jstBuffered: ;
  else
    raise EInvalidStateException.Create(TTMSFNCJSONState.jstInt64);
  end;

  FPeeked := TTMSFNCJSONState.jstBuffered;
  AsDouble := StrToFloat(FPeekedString);
  Result := Round(AsDouble);
  if AsDouble <> Result then
    raise EInvalidStateException.Create(TTMSFNCJSONState.jstInt64);
  FPeekedString := '';
  FPeeked := TTMSFNCJSONState.jstNone;
end;

function TTMSFNCJSONReader.ReadInteger: integer;
var
  AsDouble: double;
begin
  case NextPeek of
    TTMSFNCJSONState.jstInt64:
      begin
        Result := Integer(FPeekedInt64);
        if Result <> FPeekedInt64 then
          raise EInvalidStateException.Create(TTMSFNCJSONState.jstInt64);
        FPeeked := TTMSFNCJSONState.jstNone;
        Exit;
      end;
    TTMSFNCJSONState.jstNumber:
      begin
        if TryStrToInt(ArrayOfCharToString(FPeekedNumber), Result) then
        begin
          FPeeked := TTMSFNCJSONState.jstNone;
          Exit;
        end else
          FPeekedString := ArrayOfCharToString(FPeekedNumber);
      end;
    TTMSFNCJSONState.jstDoubleQuoted:
      begin
        FPeekedString := ReadQuoted;
        if TryStrToInt(FPeekedString, Result) then
        begin
          FPeeked := TTMSFNCJSONState.jstNone;
          Exit;
        end;
      end;
    TTMSFNCJSONState.jstBuffered: ;
  else
    raise EInvalidStateException.Create(TTMSFNCJSONState.jstInt64); // todo
  end;

  FPeeked := TTMSFNCJSONState.jstBuffered;
  AsDouble := StrToFloat(FPeekedString);
  Result := Round(AsDouble);
  if AsDouble <> Result then
    raise EInvalidStateException.Create(TTMSFNCJSONState.jstInt64);
  FPeekedString := '';
  FPeeked := TTMSFNCJSONState.jstNone;
end;

function TTMSFNCJSONReader.ReadName: string;
begin
  CheckState(TTMSFNCJSONState.jstDoubleQuotedName);
  FPeeked := TTMSFNCJSONState.jstNone;
  Result := ReadQuoted;
end;

procedure TTMSFNCJSONReader.ReadNull;
begin
  CheckState(TTMSFNCJSONState.jstNull);
  FPeeked := TTMSFNCJSONState.jstNone;
end;

procedure TTMSFNCJSONReader.ReadBeginObject;
begin
  CheckState(TTMSFNCJSONState.jstBeginObject);
  PushScope(TTMSFNCJSONScope.jscEmptyObject);
  FPeeked := TTMSFNCJSONState.jstNone;
end;

procedure TTMSFNCJSONReader.ReadEndObject;
begin
  CheckState(TTMSFNCJSONState.jstEndObject);
  Dec(FStackSize);
  FPeeked := TTMSFNCJSONState.jstNone;
end;

function TTMSFNCJSONReader.ReadQuoted: string;
begin
  Result := InternalReadQuoted(true);
end;

function TTMSFNCJSONReader.InternalReadQuoted(const BuildString: boolean): string;
var
  pc, c: String;
  s: string;
begin
  Result := '';
  s := '';
  pc := '';
  while not FReader.Eof do
  begin
    c := ReadChar;
    if (c = '"') and (pc <> '\') then
      Break
    else
      s := s + c;

    pc := c;
  end;

  if BuildString then
    Result := s;
end;

function TTMSFNCJSONReader.ReadString: string;
begin
  case NextPeek of
    TTMSFNCJSONState.jstDoubleQuoted:
      Result := ReadQuoted;
    TTMSFNCJSONState.jstInt64:
      Result := IntToStr(FPeekedInt64);
    TTMSFNCJSONState.jstNumber:
      Result := ArrayOfCharToString(FPeekedNumber);
    TTMSFNCJSONState.jstBuffered:
      Result := FPeekedString;
  else
    raise EInvalidStateException.Create(TTMSFNCJSONState.jstDoubleQuoted);
  end;
  FPeeked := TTMSFNCJSONState.jstNone;

  Result := TTMSFNCUtils.UnescapeString(Result);
end;

{ TTMSFNCJSONStreamReader }

constructor TTMSFNCJSONStreamReader.Create(const aStream: TStream);
begin
  FStream := aStream;
  FReadStream := TStringStream.Create(''{$IFDEF WEBLIB}, TEncoding.Ansi{$ENDIF});
  FReadStream.CopyFrom(FStream, FStream.Size);
  FReadStream.Position := 0;
end;

destructor TTMSFNCJSONStreamReader.Destroy;
begin
  FReadStream.Free;
  inherited;
end;

function TTMSFNCJSONStreamReader.NextChar: char;
begin
  if (Eof) then
    raise EEndOfInputReached.Create;
  Result := ReadChar;
end;

function TTMSFNCJSONStreamReader.PeekChar: char;
var
  p: Int64;
begin
  p := FReadStream.Position;
  Result := ReadChar;
  FReadStream.Position := p;
end;

function TTMSFNCJSONStreamReader.ReadChar: char;
var
  i: Integer;
  s: string;
begin
  Result := #0;
  {$IFDEF ZEROSTRINGINDEX}
  i := 0;
  {$ELSE}
  i := 1;
  {$ENDIF}
  if FReadStream.Position < FReadStream.Size then
  begin
    s := FReadStream.ReadString(1);
    {$IFNDEF WEBLIB}
    {$HINTS OFF}
    {$WARNINGS OFF}
    {$IFDEF LCLLIB}
    {$IF FPC_FULLVERSION = 30200}
    //BUG IN TStringStream in FPC 3.2
    FReadStream.Position := FReadStream.Position + 1;
    {$IFEND}
    {$ENDIF}
    {$HINTS ON}
    {$WARNINGS ON}
    {$ENDIF}
    if (s <> '') {$IFDEF WEBLIB} and Assigned(s) {$ENDIF} then
      Result := s[i]
  end;
end;

procedure TTMSFNCJSONStreamReader.Backup(const c: char);
begin
  FReadStream.Position := FReadStream.Position - 1;
end;

procedure TTMSFNCJSONStreamReader.MoveNext(const Count: integer = 1);
begin
  FReadStream.Position := FReadStream.Position + Count;
end;

function TTMSFNCJSONStreamReader.Eof: boolean;
begin
  Result := FReadStream.Position = FReadStream.Size;
end;

{ TTMSFNCJSONReader.EInvalidStateException }

constructor TTMSFNCJSONReader.EInvalidStateException.Create(const AState: TTMSFNCJSONState);
begin
  inherited CreateFmt('Invalid Json parser state. Expected state: %d', [Ord(AState)]);
end;

{ TTMSFNCJSONReader.EUnterminatedArray }

constructor TTMSFNCJSONReader.EUnterminatedArray.Create;
begin
  inherited Create('Unterminated array');
end;

{ TTMSFNCJSONReader.EUnterminatedObject }

constructor TTMSFNCJSONReader.EUnterminatedObject.Create;
begin
  inherited Create('Unterminated object');
end;

{ TTMSFNCJSONReader.ENameExpected }

constructor TTMSFNCJSONReader.ENameExpected.Create;
begin
  inherited Create('Name expected');
end;

{ TTMSFNCJSONReader.EColonExpected }

constructor TTMSFNCJSONReader.EColonExpected.Create;
begin
  inherited Create('Colon expected');
end;

{ TTMSFNCJSONReader.EReaderClosed }

constructor TTMSFNCJSONReader.EReaderClosed.Create;
begin
  inherited Create('Reader already closed');
end;

{ TTMSFNCJSONReader.EMultipleRootNotAllowed }

constructor TTMSFNCJSONReader.EMultipleRootNotAllowed.Create;
begin
  inherited Create('Multiple root values not allowed');
end;

{ TTMSFNCJSONReader.EExpectedValue }

constructor TTMSFNCJSONReader.EExpectedValue.Create;
begin
  inherited Create('Value expected but invalid character found');
end;

{ TTMSFNCJSONReader.EObjectOrArrayExpected }

constructor TTMSFNCJSONReader.EObjectOrArrayExpected.Create;
begin
  inherited Create('Object or array expected as top-level value');
end;

{ TTMSFNCJSONReader.ETooManyDepthLevels }

constructor TTMSFNCJSONReader.ETooManyDepthLevels.Create;
begin
  inherited Create('Maximum level of nested structured reached.');
end;

{ TTMSFNCJSONStreamReader.EInvalidJsonInput }

constructor TTMSFNCJSONStreamReader.EInvalidJsonInput.Create;
begin
  inherited Create('Invalid JSON Input');
end;

{ TTMSFNCJSONStreamReader.EInternalError }

constructor TTMSFNCJSONStreamReader.EInternalError.Create;
begin
  inherited Create('JSON stream reader internal error');
end;

{ TTMSFNCJSONStreamReader.EEndOfInputReached }

constructor TTMSFNCJSONStreamReader.EEndOfInputReached.Create;
begin
  inherited Create('End of JSON input reached.');
end;

{ TTMSFNCJSONReader.EInvalidEscaped }

constructor TTMSFNCJSONReader.EInvalidEscaped.Create;
begin
  inherited Create('Invalid escaped sequence');
end;

end.
