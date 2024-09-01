{ ***************************************************************************
  sgcP2P component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcP2P_STUN_Helpers;

interface

{$I sgcVer.inc}
{$IFDEF SGC_STUN}

uses
  Classes, SysUtils,
  // sgc
  sgcP2P_STUN_Classes, sgcBase_Helpers, sgcP2P_STUN_Types;

function sgcSwapWord(Value: Word): Word;
function sgcSwapCardinal(Value: Cardinal): Cardinal;
function sgcGetSTUNMessageClass(Value: Word): TsgcStunMessageClass;
function sgcGetSTUNMessageMethod(Value: Word): TsgcStunMessageMethod;
procedure sgcSTUNAddByte(Value: Byte; var Bytes: TBytes);
procedure sgcSTUNAddBytes(Value: TBytes; var Bytes: TBytes);
procedure sgcSTUNAddWord(Value: Word; var Bytes: TBytes);
procedure sgcSTUNAddCardinal(Value: Cardinal; var Bytes: TBytes);
function sgcSTUNXorIpV4(aIpAddress: String; aCookie: Cardinal): TBytes;
function sgcSTUNXorIpV6(aIpAddress: String; aCookie: Cardinal;
  aTransactionId: TBytes): TBytes;

function sgcGetStringFromBytes(const aBytes: TBytes; aOffset: Integer = 0;
  aLength: Integer = 0): String;
function sgcGetWordFromBytes(const aBytes: TBytes; aOffset: Integer = 0): Word;
function sgcGetCardinalFromBytes(const aBytes: TBytes; aOffset: Integer = 0)
  : Cardinal;
function sgcGetUInt64FromBytes(const aBytes: TBytes; aOffset: Integer = 0):
    UInt64;
function GetCRC(const aBytes: TBytes): Cardinal;
function IsSTUNPacket(const aByte: Byte): Boolean;

function sgcGetTransactionId(const aTransactionId: string = ''): TBytes;
function sgcGetTieBreaker: TBytes;

{$ENDIF}

implementation

{$IFDEF SGC_STUN}

uses
  Math;

var
  crc_table: array [0 .. 255] Of Cardinal;
  crc_table_computed: Boolean = false;

procedure DoCRC_MakeTable;
var
  n: Integer;
  c: Cardinal;
  k: Integer;
begin
  for n := 0 to Length(crc_table) - 1 do
  begin
    c := n;
    for k := 0 to 7 do
    begin
      if (c and 1) <> 0 then
        c := $EDB88320 XOR (c shr 1)
      else
        c := c shr 1;
    end;
    crc_table[n] := c;
  end;
  crc_table_computed := true;
end;

function DoCRC_Update(crc: Cardinal; buf: TBytes): Cardinal;
var
  c: Cardinal;
  n: Integer;
begin
  c := crc xor $FFFFFFFF;
  if not crc_table_computed then
    DoCRC_MakeTable;
  for n := 0 to Length(buf) - 1 do
    c := crc_table[(c XOR buf[n]) and $000000FF] XOR (c shr 8);
  result := c XOR $FFFFFFFF;
end;

function GetCRC(const aBytes: TBytes): Cardinal;
begin
  result := DoCRC_Update(0, aBytes);
end;

function sgcSwapCardinal(Value: Cardinal): Cardinal;
begin
  result := (sgcSwapWord(Value and $FFFF) shl 16) or sgcSwapWord(Value shr 16);
end;

function sgcSwapWord(Value: Word): Word;
begin
  result := ((Value and $FF) shl 8) or (Value shr 8);
end;

function sgcGetSTUNMessageClass(Value: Word): TsgcStunMessageClass;
var
  vClass: Word;
begin
  vClass := Value and $110;
  result := TsgcStunMessageClass(((vClass shr 4) and 1) or (vClass shr 7));
end;

function sgcGetSTUNMessageMethod(Value: Word): TsgcStunMessageMethod;
begin
  result := TsgcStunMessageMethod((Value and $F) or ((Value and $E0) shr 1) or
    ((Value and $3E00) shr 2));
end;

function sgcGetWordFromBytes(const aBytes: TBytes; aOffset: Integer = 0): Word;
begin
  result := 0;
  if aOffset + 1 <= Length(aBytes) then
  begin
    result := aBytes[aOffset] * 256;
    result := result + aBytes[aOffset + 1];
  end;
end;

function sgcGetCardinalFromBytes(const aBytes: TBytes; aOffset: Integer = 0)
  : Cardinal;
begin
  result := 0;
  if aOffset + 3 <= Length(aBytes) then
  begin
    result := aBytes[aOffset] * 256 * 256 * 256;
    result := result + aBytes[aOffset + 1] * 256 * 256;
    result := result + aBytes[aOffset + 2] * 256;
    result := result + aBytes[aOffset + 3];
  end;
end;

procedure sgcSTUNAddByte(Value: Byte; var Bytes: TBytes);
begin
  SetLength(Bytes, Length(Bytes) + 1);
  Bytes[Length(Bytes) - 1] := Value;
end;

procedure sgcSTUNAddBytes(Value: TBytes; var Bytes: TBytes);
var
  i: Integer;
begin
  if Assigned(Value) then
  begin
    i := Length(Bytes);
    SetLength(Bytes, Length(Bytes) + Length(Value));
    sgcMove(Value[0], Bytes[i], Length(Value));
  end;
end;

procedure sgcSTUNAddWord(Value: Word; var Bytes: TBytes);
var
  oBytes: TBytes;
  vValue: Word;
begin
  vValue := sgcSwapWord(Value);

  SetLength(oBytes, SizeOf(vValue));
  sgcMove(vValue, oBytes[0], SizeOf(vValue));

  sgcSTUNAddBytes(oBytes, Bytes);
end;

procedure sgcSTUNAddCardinal(Value: Cardinal; var Bytes: TBytes);
var
  oBytes: TBytes;
  vValue: Cardinal;
begin
  vValue := sgcSwapCardinal(Value);

  SetLength(oBytes, SizeOf(vValue));
  sgcMove(vValue, oBytes[0], SizeOf(vValue));

  sgcSTUNAddBytes(oBytes, Bytes);
end;

function sgcGetStringFromBytes(const aBytes: TBytes; aOffset: Integer = 0;
  aLength: Integer = 0): String;
var
  oBytes: TBytes;
begin
  if aOffset = 0 then
    result := sgcGetUTF8StringFromBytes(aBytes)
  else
  begin
    if aLength = 0 then
      aLength := Length(aBytes) - aOffset;
    SetLength(oBytes, aLength);
    sgcMove(aBytes[aOffset], oBytes[0], Length(oBytes));
    result := sgcGetUTF8StringFromBytes(oBytes);
  end;
end;

function sgcSTUNXorIpV6(aIpAddress: String; aCookie: Cardinal;
  aTransactionId: TBytes): TBytes;
var
  i: Integer;
  oXOR: TBytes;
  oList: TStringList;
begin
  oList := TStringList.Create;
  Try
    oList.Delimiter := ':';
    oList.DelimitedText := aIpAddress;
    for i := 0 to oList.Count - 1 do
      sgcSTUNAddWord(StrToInt('$' + oList[i]), result);
  Finally
    sgcFree(oList);
  End;

  // xor
  if aCookie <> 0 then
  begin
    sgcSTUNAddCardinal(aCookie, oXOR);
    sgcSTUNAddBytes(aTransactionId, oXOR);

    for i := 0 to Length(result) - 1 do
      result[i] := result[i] xor oXOR[i];
  end;
end;

function sgcSTUNXorIpV4(aIpAddress: String; aCookie: Cardinal): TBytes;
var
  i: Integer;
  oXOR: TBytes;
  oList: TStringList;
begin
  SetLength(result, 4);

  oList := TStringList.Create;
  Try
    oList.Delimiter := '.';
    oList.DelimitedText := aIpAddress;
    for i := 0 to Length(result) - 1 do
      result[i] := StrToInt(oList[i]);
  Finally
    sgcFree(oList);
  End;

  if aCookie <> 0 then
  begin
    sgcSTUNAddCardinal(aCookie, oXOR);
    for i := 0 to Length(result) - 1 do
      result[i] := result[i] xor oXOR[i];
  end;
end;

function IsSTUNPacket(const aByte: Byte): Boolean;
begin
  result := aByte <= 3;
end;

function sgcGetTransactionId(const aTransactionId: string = ''): TBytes;
  function GetRandomByte: Byte;
  begin
    result := RandomRange(97, 123);
  end;

var
  i: Integer;
begin
  Randomize;

  SetLength(result, 12);
  for i := 0 to 11 do
  begin
    if (aTransactionId <> '') then
    begin
      if i < Length(aTransactionId) then
        result[i] := Ord(aTransactionId[{$IFDEF SGC_ZEROBASEDSTRINGS}i{$ELSE}i +
          1{$ENDIF}])
      else
        result[i] := 0;
    end
    else
      result[i] := GetRandomByte;
  end;
end;

function sgcGetTieBreaker: TBytes;
var
  i: Integer;
begin
  Randomize;

  SetLength(result, 8);
  for i := 0 to 7 do
    result[i] := RandomRange(0, 255);
end;

function sgcGetUInt64FromBytes(const aBytes: TBytes; aOffset: Integer = 0):
    UInt64;
begin
  result := 0;
  if aOffset + 7 <= Length(aBytes) then
  begin
    result := aBytes[aOffset] * 256 * 256 * 256 * 256 * 256 * 256 * 256;
    result := result + aBytes[aOffset + 1] * 256 * 256 * 256 * 256 * 256 * 256;
    result := result + aBytes[aOffset + 2] * 256 * 256 * 256 * 256 * 256;
    result := result + aBytes[aOffset + 3] * 256 * 256 * 256 * 256;
    result := result + aBytes[aOffset + 4] * 256 * 256 * 256;
    result := result + aBytes[aOffset + 5] * 256 * 256;
    result := result + aBytes[aOffset + 6] * 256;
    result := result + aBytes[aOffset + 7];
  end;
end;

{$ENDIF}

end.
