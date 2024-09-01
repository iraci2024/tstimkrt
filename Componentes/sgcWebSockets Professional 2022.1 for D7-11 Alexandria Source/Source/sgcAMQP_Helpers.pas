{ ***************************************************************************
  sgcAMQP component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcAMQP_Helpers;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils,
  // sgc
  sgcAMQP_Classes, sgcBase_Helpers;

// ... amqp frame
function sgcGetAMQPFrameType(const aValue: Integer): TsgcAMQPFrameType;
function sgcGetAMQPFrameTypeValue(const aValue: TsgcAMQPFrameType): Integer;
// ... amqp classes
function sgcGetAMQPClass(const aValue: Integer): TsgcAMQPClass;
function sgcGetAMQPClassValue(const aValue: TsgcAMQPClass): Integer;
// ... amqp methods
function sgcGetAMQPMethod(const aClass: TsgcAMQPClass; const aValue: Integer)
  : TsgcAMQPMethod;
function sgcGetAMQPConnection(const aValue: Integer): TsgcAMQPMethod;
function sgcGetAMQPConnectionValue(const aValue: TsgcAMQPMethod): Integer;
function sgcGetAMQPChannel(const aValue: Integer): TsgcAMQPMethod;
function sgcGetAMQPChannelValue(const aValue: TsgcAMQPMethod): Integer;
function sgcGetAMQPExchange(const aValue: Integer): TsgcAMQPMethod;
function sgcGetAMQPExchangeValue(const aValue: TsgcAMQPMethod): Integer;
function sgcGetAMQPQueue(const aValue: Integer): TsgcAMQPMethod;
function sgcGetAMQPQueueValue(const aValue: TsgcAMQPMethod): Integer;
function sgcGetAMQPBasic(const aValue: Integer): TsgcAMQPMethod;
function sgcGetAMQPBasicValue(const aValue: TsgcAMQPMethod): Integer;
function sgcGetAMQPTX(const aValue: Integer): TsgcAMQPMethod;
function sgcGetAMQPTXValue(const aValue: TsgcAMQPMethod): Integer;
// ... amqp auth
function sgcGetAMQPAuthentication(const aValue: string): TsgcAMQPAuthentication;
function sgcGetAMQPAuthentications(const aValue: string)
  : TsgcAMQPAuthentications;
function sgcGetAMQPAuthenticationValue(const aValue
  : TsgcAMQPAuthentication): string;

// ... bytes
function sgcReadAMQPBytes(const aStream: TStream; aSize: Integer): TBytes;

function sgcReadAMQPByte(const aBytes: TBytes; var aOffset: Integer): Byte;
function sgcReadAMQPInt8(const aBytes: TBytes; var aOffset: Integer): ShortInt;
function sgcReadAMQPUInt8(const aBytes: TBytes; var aOffset: Integer): Byte;
function sgcReadAMQPBoolean(const aBytes: TBytes; var aOffset: Integer)
  : Boolean;
function sgcReadAMQPInt16(const aBytes: TBytes; var aOffset: Integer): Int16;
function sgcReadAMQPUInt16(const aBytes: TBytes; var aOffset: Integer): UInt16;
function sgcReadAMQPInt32(const aBytes: TBytes; var aOffset: Integer): Int32;
function sgcReadAMQPUInt32(const aBytes: TBytes; var aOffset: Integer)
  : Cardinal;
function sgcReadAMQPInt64(const aBytes: TBytes; var aOffset: Integer): Int64;
function sgcReadAMQPUInt64(const aBytes: TBytes; var aOffset: Integer): UInt64;
function sgcReadAMQPSingle(const aBytes: TBytes; var aOffset: Integer): Single;
function sgcReadAMQPDouble(const aBytes: TBytes; var aOffset: Integer): Double;
function sgcReadAMQPLongString(const aBytes: TBytes;
  var aOffset: Integer): string;
function sgcReadAMQPDecimal(const aBytes: TBytes; var aOffset: Integer): Double;
function sgcReadAMQPStringList(const aBytes: TBytes; var aOffset: Integer;
  var aList: TStringList): TBytes;
function sgcReadAMQPLongStringAsBytes(const aBytes: TBytes;
  var aOffset: Integer): TBytes;
function sgcReadAMQPShortString(const aBytes: TBytes;
  var aOffset: Integer): string;
function sgcReadAMQPFieldValue(const aBytes: TBytes;
  var aOffset: Integer): string;
function sgcReadAMQPFieldTable(const aBytes: TBytes;
  var aOffset: Integer): string;
function sgcReadAMQPFieldArray(const aBytes: TBytes;
  var aOffset: Integer): string;

// ... write
procedure sgcWriteAMQPBytes(const aStream: TStream; const aBytes: TBytes);
procedure sgcWriteAMQPByte(const aValue: Byte; var aBytes: TBytes);
procedure sgcWriteAMQPInt8(const aValue: Int8; var aBytes: TBytes);
procedure sgcWriteAMQPUInt8(const aValue: UInt8; var aBytes: TBytes);
procedure sgcWriteAMQPInt16(const aValue: Int16; var aBytes: TBytes);
procedure sgcWriteAMQPUInt16(const aValue: UInt16; var aBytes: TBytes);
procedure sgcWriteAMQPInt32(const aValue: Int32; var aBytes: TBytes);
procedure sgcWriteAMQPUInt32(const aValue: UInt32; var aBytes: TBytes);
procedure sgcWriteAMQPInt64(const aValue: Int64; var aBytes: TBytes);
procedure sgcWriteAMQPUInt64(const aValue: UInt64; var aBytes: TBytes);
procedure sgcWriteAMQPShortString(const aValue: string; var aBytes: TBytes);
procedure sgcWriteAMQPLongString(const aValue: string;
  var aBytes: TBytes); overload;
procedure sgcWriteAMQPLongString(const aSource: TBytes;
  var aBytes: TBytes); overload;
procedure sgcWriteAMQPStringList(const aList: TStringList; var aBytes: TBytes);
procedure sgcWriteAMQPBoolean(const aValue: Boolean; var aBytes: TBytes);
procedure sgcWriteAMQPBit(aOffset, aBifOffset: Integer; var aBytes: TBytes);
procedure sgcWriteAMQPFieldTable(const aValue: string; var aBytes: TBytes);

// ... exception
procedure DoRaiseAMQPException(const aCode: Integer);
function GetAMQPExceptionMessage(const aCode: Integer): string;

implementation

uses
  sgcJSON, sgcAMQP_Const,
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF};

function sgcAMQPBoolToStr(aValue: Boolean): string;
begin
  if aValue then
    result := 'true'
  else
    result := 'false';
end;

function sgcGetAMQPConnection(const aValue: Integer): TsgcAMQPMethod;
begin
  case aValue of
    10:
      result := amqpConnStart;
    11:
      result := amqpConnStartOk;
    20:
      result := amqpConnSecure;
    21:
      result := amqpConnSecureOk;
    30:
      result := amqpConnTune;
    31:
      result := amqpConnTuneOk;
    40:
      result := amqpConnOpen;
    41:
      result := amqpConnOpenOk;
    50:
      result := amqpConnClose;
    51:
      result := amqpConnCloseOk;
  else
    result := amqpConnNone;
  end;
end;

function sgcGetAMQPConnectionValue(const aValue: TsgcAMQPMethod): Integer;
begin
  case aValue of
    amqpConnStart:
      result := 10;
    amqpConnStartOk:
      result := 11;
    amqpConnSecure:
      result := 20;
    amqpConnSecureOk:
      result := 21;
    amqpConnTune:
      result := 30;
    amqpConnTuneOk:
      result := 31;
    amqpConnOpen:
      result := 40;
    amqpConnOpenOk:
      result := 41;
    amqpConnClose:
      result := 50;
    amqpConnCloseOk:
      result := 51;
  else
    result := 0;
  end;
end;

function sgcGetAMQPChannel(const aValue: Integer): TsgcAMQPMethod;
begin
  case aValue of
    10:
      result := amqpChannOpen;
    11:
      result := amqpChannOpenOk;
    20:
      result := amqpChannFlow;
    21:
      result := amqpChannFlowOk;
    40:
      result := amqpChannClose;
    41:
      result := amqpChannCloseOk;
  else
    result := amqpChannNone;
  end;
end;

function sgcGetAMQPChannelValue(const aValue: TsgcAMQPMethod): Integer;
begin
  case aValue of
    amqpChannOpen:
      result := 10;
    amqpChannOpenOk:
      result := 11;
    amqpChannFlow:
      result := 20;
    amqpChannFlowOk:
      result := 21;
    amqpChannClose:
      result := 40;
    amqpChannCloseOk:
      result := 41;
  else
    result := 0;
  end;
end;

function sgcGetAMQPExchange(const aValue: Integer): TsgcAMQPMethod;
begin
  case aValue of
    10:
      result := amqpExchDeclare;
    11:
      result := amqpExchDeclareOk;
    20:
      result := amqpExchDelete;
    21:
      result := amqpExchDeleteOk;
  else
    result := amqpExchNone;
  end;
end;

function sgcGetAMQPExchangeValue(const aValue: TsgcAMQPMethod): Integer;
begin
  case aValue of
    amqpExchDeclare:
      result := 10;
    amqpExchDeclareOk:
      result := 11;
    amqpExchDelete:
      result := 20;
    amqpExchDeleteOk:
      result := 21;
  else
    result := 0;
  end;
end;

function sgcGetAMQPQueue(const aValue: Integer): TsgcAMQPMethod;
begin
  case aValue of
    10:
      result := amqpQueueDeclare;
    11:
      result := amqpQueueDeclareOk;
    20:
      result := amqpQueueBind;
    21:
      result := amqpQueueBindOk;
    50:
      result := amqpQueueUnBind;
    51:
      result := amqpQueueUnBindOk;
    30:
      result := amqpQueuePurge;
    31:
      result := amqpQueuePurgeOk;
    40:
      result := amqpQueueDelete;
    41:
      result := amqpQueueDeleteOk;
  else
    result := amqpQueueNone;
  end;
end;

function sgcGetAMQPQueueValue(const aValue: TsgcAMQPMethod): Integer;
begin
  case aValue of
    amqpQueueDeclare:
      result := 10;
    amqpQueueDeclareOk:
      result := 11;
    amqpQueueBind:
      result := 20;
    amqpQueueBindOk:
      result := 21;
    amqpQueueUnBind:
      result := 50;
    amqpQueueUnBindOk:
      result := 51;
    amqpQueuePurge:
      result := 30;
    amqpQueuePurgeOk:
      result := 31;
    amqpQueueDelete:
      result := 40;
    amqpQueueDeleteOk:
      result := 41;
  else
    result := 0;
  end;
end;

function sgcGetAMQPBasic(const aValue: Integer): TsgcAMQPMethod;
begin
  case aValue of
    10:
      result := amqpBasicQos;
    11:
      result := amqpBasicQosOk;
    20:
      result := amqpBasicConsume;
    21:
      result := amqpBasicConsumeOk;
    30:
      result := amqpBasicCancel;
    31:
      result := amqpBasicCancelOk;
    40:
      result := amqpBasicPublish;
    50:
      result := amqpBasicReturn;
    60:
      result := amqpBasicDeliver;
    70:
      result := amqpBasicGet;
    71:
      result := amqpBasicGetOk;
    72:
      result := amqpBasicGetEmpty;
    80:
      result := amqpBasicAck;
    90:
      result := amqpBasicReject;
    100:
      result := amqpBasicRecoverAsync;
    110:
      result := amqpBasicRecover;
    111:
      result := amqpBasicRecoverOk;
  else
    result := amqpBasicNone;
  end;

end;

function sgcGetAMQPBasicValue(const aValue: TsgcAMQPMethod): Integer;
begin
  case aValue of
    amqpBasicQos:
      result := 10;
    amqpBasicQosOk:
      result := 11;
    amqpBasicConsume:
      result := 20;
    amqpBasicConsumeOk:
      result := 21;
    amqpBasicCancel:
      result := 30;
    amqpBasicCancelOk:
      result := 31;
    amqpBasicPublish:
      result := 40;
    amqpBasicReturn:
      result := 50;
    amqpBasicDeliver:
      result := 60;
    amqpBasicGet:
      result := 70;
    amqpBasicGetOk:
      result := 71;
    amqpBasicGetEmpty:
      result := 72;
    amqpBasicAck:
      result := 80;
    amqpBasicReject:
      result := 90;
    amqpBasicRecoverAsync:
      result := 100;
    amqpBasicRecover:
      result := 110;
    amqpBasicRecoverOk:
      result := 111;
  else
    result := 0;
  end;
end;

function sgcGetAMQPTX(const aValue: Integer): TsgcAMQPMethod;
begin
  case aValue of
    10:
      result := amqpTxSelect;
    11:
      result := amqpTxSelectOk;
    20:
      result := amqpTxCommit;
    21:
      result := amqpTxCommitOk;
    30:
      result := amqpTxRollback;
    31:
      result := amqpTxRollbackOk;
  else
    result := amqpTxNone;
  end;
end;

function sgcGetAMQPTXValue(const aValue: TsgcAMQPMethod): Integer;
begin
  case aValue of
    amqpTxSelect:
      result := 10;
    amqpTxSelectOk:
      result := 11;
    amqpTxCommit:
      result := 20;
    amqpTxCommitOk:
      result := 21;
    amqpTxRollback:
      result := 30;
    amqpTxRollbackOk:
      result := 31;
  else
    result := 0;
  end;
end;

function sgcGetAMQPClass(const aValue: Integer): TsgcAMQPClass;
begin
  case aValue of
    10:
      result := amqpClassConnection;
    20:
      result := amqpClassChannel;
    40:
      result := amqpClassExchange;
    50:
      result := amqpClassQueue;
    60:
      result := amqpClassBasic;
    90:
      result := amqpClassTx;
  else
    result := amqpClassNone;
  end;
end;

function sgcGetAMQPClassValue(const aValue: TsgcAMQPClass): Integer;
begin
  case aValue of
    amqpClassConnection:
      result := 10;
    amqpClassChannel:
      result := 20;
    amqpClassExchange:
      result := 40;
    amqpClassQueue:
      result := 50;
    amqpClassBasic:
      result := 60;
    amqpClassTx:
      result := 90;
  else
    result := 0;
  end;
end;

function sgcReadAMQPBytes(const aStream: TStream; aSize: Integer): TBytes;
begin
  ReadTIdBytesFromStream(aStream, TIdBytes(result), aSize);
end;

procedure sgcWriteAMQPBytes(const aStream: TStream; const aBytes: TBytes);
begin
  WriteTIdBytesToStream(aStream, TIdBytes(aBytes));
end;

procedure DoRaiseAMQPException(const aCode: Integer);
var
  oException: TsgcAMQPException;
begin
  oException := TsgcAMQPException.Create(GetAMQPExceptionMessage(aCode));
  oException.Code := aCode;
  raise oException;
end;

function sgcGetAMQPFrameType(const aValue: Integer): TsgcAMQPFrameType;
begin
  case aValue of
    1:
      result := amqpFrameMethod;
    2:
      result := amqpFrameHeader;
    3:
      result := amqpFrameBody;
    8:
      result := amqpFrameHeartBeat;
  else
    result := amqpFrameNone;
  end;
end;

function sgcGetAMQPFrameTypeValue(const aValue: TsgcAMQPFrameType): Integer;
begin
  case aValue of
    amqpFrameMethod:
      result := 1;
    amqpFrameHeader:
      result := 2;
    amqpFrameBody:
      result := 3;
    amqpFrameHeartBeat:
      result := 8;
  else
    result := 0;
  end;
end;

function sgcReadAMQPLongString(const aBytes: TBytes;
  var aOffset: Integer): string;
begin
  result := sgcBytesToStringRaw(sgcReadAMQPLongStringAsBytes(aBytes, aOffset));
end;

function sgcReadAMQPShortString(const aBytes: TBytes;
  var aOffset: Integer): string;
var
  vLength: Byte;
  vBytes: TBytes;
begin
  result := '';

  vLength := sgcReadAMQPByte(aBytes, aOffset);
  if vLength > 0 then
  begin
    SetLength(vBytes, vLength);
    sgcMove(aBytes[aOffset], vBytes[0], Length(vBytes));
    result := sgcGetUTF8StringFromBytes(vBytes);
    aOffset := aOffset + vLength;
  end;
end;

function sgcReadAMQPLongStringAsBytes(const aBytes: TBytes;
  var aOffset: Integer): TBytes;
var
  vLength: Integer;
begin
  result := nil;

  vLength := sgcGetUInt32FromBytes(aBytes, aOffset);
  aOffset := aOffset + 4;
  if vLength > 0 then
  begin
    SetLength(result, vLength);
    sgcMove(aBytes[aOffset], result[0], Length(result));
    aOffset := aOffset + vLength;
  end;
end;

function sgcReadAMQPStringList(const aBytes: TBytes; var aOffset: Integer;
  var aList: TStringList): TBytes;
var
  i: Integer;
  vBytes: TBytes;
  vFieldName, vFieldValue: string;
begin
  vBytes := sgcReadAMQPLongStringAsBytes(aBytes, aOffset);

  i := 0;
  while i < Length(vBytes) do
  begin
    vFieldName := sgcReadAMQPShortString(vBytes, i);
    vFieldValue := sgcReadAMQPFieldValue(vBytes, i);
    aList.Add(vFieldName + aList.NameValueSeparator + vFieldValue);
  end;
end;

function sgcReadAMQPFieldValue(const aBytes: TBytes;
  var aOffset: Integer): string;
var
  vChar: Char;
  vFS: TFormatSettings;
begin
  vFS.DecimalSeparator := '.';
  vFS.ThousandSeparator := ',';

  vChar := Chr(sgcReadAMQPByte(aBytes, aOffset));
  case vChar of
    't': // boolean
      result := sgcAMQPBoolToStr(sgcReadAMQPBoolean(aBytes, aOffset));
    'b': // int8
      result := IntToStr(sgcReadAMQPInt8(aBytes, aOffset));
    'B': // uint8
      result := IntToStr(sgcReadAMQPUInt8(aBytes, aOffset));
    'U': // int16
      result := IntToStr(sgcReadAMQPInt16(aBytes, aOffset));
    'u': // uint16
      result := IntToStr(sgcReadAMQPUInt16(aBytes, aOffset));
    'I': // int32
      result := IntToStr(sgcReadAMQPInt32(aBytes, aOffset));
    'i': // uint32
      result := IntToStr(sgcReadAMQPUInt32(aBytes, aOffset));
    'L': // int64
      result := IntToStr(sgcReadAMQPInt64(aBytes, aOffset));
    'l': // uint64
      result := IntToStr(sgcReadAMQPUInt64(aBytes, aOffset));
    'f': // single
      result := FormatFloat('0.#', sgcReadAMQPSingle(aBytes, aOffset));
    'd': // double
      result := FormatFloat('0.#', sgcReadAMQPDouble(aBytes, aOffset));
    'D': // decimal
      result := FormatFloat('0.#', sgcReadAMQPDecimal(aBytes, aOffset), vFS);
    's': // short-string
      result := sgcReadAMQPShortString(aBytes, aOffset);
    'S': // long-string
      result := sgcReadAMQPLongString(aBytes, aOffset);
    'A': // field array
      result := sgcReadAMQPFieldArray(aBytes, aOffset);
    'T': // timestamp
      result := IntToStr(sgcReadAMQPInt64(aBytes, aOffset));
    'F': // field-table
      result := sgcReadAMQPFieldTable(aBytes, aOffset);
    'V': // no field
      ;
  end;
end;

function sgcReadAMQPFieldTable(const aBytes: TBytes;
  var aOffset: Integer): string;
var
  i: Integer;
  vBytes: TBytes;
  vFieldName, vFieldValue: string;
  vValue: Double;
begin
  vBytes := sgcReadAMQPLongStringAsBytes(aBytes, aOffset);

  result := '{';
  i := 0;
  while i < Length(vBytes) do
  begin
    vFieldName := sgcReadAMQPShortString(vBytes, i);
    vFieldValue := sgcReadAMQPFieldValue(vBytes, i);

    if result <> '{' then
      result := result + ', ';
    result := result + '"' + vFieldName + '": ';
    if (vFieldValue = 'true') or (vFieldValue = 'false') or
      (TryStrToFloat(vFieldValue, vValue) = True) then
      result := result + vFieldValue
    else
      result := result + '"' + vFieldValue + '"';
  end;
  result := result + '}';
end;

function sgcReadAMQPBoolean(const aBytes: TBytes; var aOffset: Integer)
  : Boolean;
begin
  result := sgcReadAMQPByte(aBytes, aOffset) = 1;
end;

function sgcReadAMQPUInt16(const aBytes: TBytes; var aOffset: Integer): UInt16;
begin
  result := sgcGetUInt16FromBytes(aBytes, aOffset);
  aOffset := aOffset + SizeOf(UInt16);
end;

function sgcReadAMQPUInt32(const aBytes: TBytes; var aOffset: Integer)
  : Cardinal;
begin
  result := sgcGetUInt32FromBytes(aBytes, aOffset);
  aOffset := aOffset + SizeOf(UInt32);
end;

function sgcReadAMQPUInt64(const aBytes: TBytes; var aOffset: Integer): UInt64;
begin
  result := sgcGetUInt64FromBytes(aBytes, aOffset);
  aOffset := aOffset + SizeOf(UInt64);
end;

function sgcReadAMQPDecimal(const aBytes: TBytes; var aOffset: Integer): Double;
begin
  result := sgcGetDecimalFromBytes(aBytes, aOffset);
  aOffset := aOffset + 5;
end;

function sgcReadAMQPInt8(const aBytes: TBytes; var aOffset: Integer): ShortInt;
begin
  result := sgcGetInt8FromBytes(aBytes, aOffset);
  aOffset := aOffset + SizeOf(Int8);
end;

function sgcReadAMQPUInt8(const aBytes: TBytes; var aOffset: Integer): Byte;
begin
  result := sgcGetUInt8FromBytes(aBytes, aOffset);
  aOffset := aOffset + SizeOf(UInt8);
end;

function sgcReadAMQPInt16(const aBytes: TBytes; var aOffset: Integer): Int16;
begin
  result := sgcGetInt16FromBytes(aBytes, aOffset);
  aOffset := aOffset + SizeOf(Int16);
end;

function sgcReadAMQPInt32(const aBytes: TBytes; var aOffset: Integer): Int32;
begin
  result := sgcGetInt32FromBytes(aBytes, aOffset);
  aOffset := aOffset + SizeOf(Int32);
end;

function sgcReadAMQPInt64(const aBytes: TBytes; var aOffset: Integer): Int64;
begin
  result := sgcGetInt64FromBytes(aBytes, aOffset);
  aOffset := aOffset + SizeOf(Int64);
end;

function sgcReadAMQPSingle(const aBytes: TBytes; var aOffset: Integer): Single;
begin
  result := sgcGetSingleFromBytes(aBytes, aOffset);
  aOffset := aOffset + SizeOf(Single);
end;

function sgcReadAMQPByte(const aBytes: TBytes; var aOffset: Integer): Byte;
begin
  result := sgcReadByte(aBytes, aOffset);
  inc(aOffset);
end;

function sgcReadAMQPDouble(const aBytes: TBytes; var aOffset: Integer): Double;
begin
  result := sgcGetDoubleFromBytes(aBytes, aOffset);
  aOffset := aOffset + SizeOf(Double);
end;

function sgcReadAMQPFieldArray(const aBytes: TBytes;
  var aOffset: Integer): string;
var
  i: Integer;
  vBytes: TBytes;
  vFieldValue: string;
  vValue: Double;
begin
  vBytes := sgcReadAMQPLongStringAsBytes(aBytes, aOffset);

  result := '[';
  i := 0;
  while i < Length(vBytes) do
  begin
    vFieldValue := sgcReadAMQPFieldValue(vBytes, i);

    if result <> '[' then
      result := result + ', ';
    if (vFieldValue = 'true') or (vFieldValue = 'false') or
      (TryStrToFloat(vFieldValue, vValue) = True) then
      result := result + vFieldValue
    else
      result := result + '"' + vFieldValue + '"';
  end;
  result := result + ']';

end;

procedure sgcWriteAMQPStringList(const aList: TStringList; var aBytes: TBytes);
var
  i: Integer;
  vBytes: TBytes;
begin
  for i := 0 to aList.Count - 1 do
  begin
    sgcWriteAMQPShortString(aList.Names[i], vBytes);
    sgcWriteAMQPByte(Byte('S'), vBytes);
    sgcWriteAMQPLongString(aList.ValueFromIndex[i], vBytes);
  end;

  sgcWriteAMQPUInt32(Length(vBytes), aBytes);
  sgcWriteBytes(vBytes, aBytes);
end;

procedure sgcWriteAMQPShortString(const aValue: string; var aBytes: TBytes);
var
  vBytes: TBytes;
begin
  vBytes := sgcGetBytesFromUTF8String(aValue);

  sgcWriteByte(Length(vBytes), aBytes);
  sgcWriteBytes(vBytes, aBytes);
end;

procedure sgcWriteAMQPLongString(const aValue: string; var aBytes: TBytes);
begin
  sgcWriteAMQPLongString(sgcGetBytesFromUTF8String(aValue), aBytes);
end;

procedure sgcWriteAMQPUInt32(const aValue: UInt32; var aBytes: TBytes);
var
  vBytes: TBytes;
begin
  sgcWriteUInt32(aValue, vBytes);
  sgcReverseBytes(vBytes);
  sgcWriteBytes(vBytes, aBytes);
end;

procedure sgcWriteAMQPInt32(const aValue: Int32; var aBytes: TBytes);
var
  vBytes: TBytes;
begin
  sgcWriteInt32(aValue, vBytes);
  sgcReverseBytes(vBytes);
  sgcWriteBytes(vBytes, aBytes);
end;

procedure sgcWriteAMQPInt8(const aValue: Int8; var aBytes: TBytes);
begin
  sgcWriteInt8(aValue, aBytes);
end;

procedure sgcWriteAMQPUInt8(const aValue: UInt8; var aBytes: TBytes);
begin
  sgcWriteUInt8(aValue, aBytes);
end;

procedure sgcWriteAMQPInt16(const aValue: Int16; var aBytes: TBytes);
var
  vBytes: TBytes;
begin
  sgcWriteInt16(aValue, vBytes);
  sgcReverseBytes(vBytes);
  sgcWriteBytes(vBytes, aBytes);
end;

procedure sgcWriteAMQPUInt16(const aValue: UInt16; var aBytes: TBytes);
var
  vBytes: TBytes;
begin
  sgcWriteUInt16(aValue, vBytes);
  sgcReverseBytes(vBytes);
  sgcWriteBytes(vBytes, aBytes);
end;

procedure sgcWriteAMQPInt64(const aValue: Int64; var aBytes: TBytes);
var
  vBytes: TBytes;
begin
  sgcWriteInt64(aValue, vBytes);
  sgcReverseBytes(vBytes);
  sgcWriteBytes(vBytes, aBytes);
end;

procedure sgcWriteAMQPUInt64(const aValue: UInt64; var aBytes: TBytes);
var
  vBytes: TBytes;
begin
  sgcWriteUInt64(aValue, vBytes);
  sgcReverseBytes(vBytes);
  sgcWriteBytes(vBytes, aBytes);
end;

function sgcGetAMQPAuthentication(const aValue: string): TsgcAMQPAuthentication;
begin
  if aValue = 'ANNONYMOUS' then
    result := amqpAuthAnnonymous
  else if aValue = 'PLAIN' then
    result := amqpAuthPlain
  else if aValue = 'AMQPLAIN' then
    result := amqpAuthAMQPlain
  else if aValue = 'EXTERNAL' then
    result := amqpAuthExternal
  else
    result := amqpAuthNone;
end;

function sgcGetAMQPAuthenticationValue(const aValue
  : TsgcAMQPAuthentication): string;
begin
  case aValue of
    amqpAuthAnnonymous:
      result := 'ANNONYMOUS';
    amqpAuthPlain:
      result := 'PLAIN';
    amqpAuthAMQPlain:
      result := 'AMQPLAIN';
    amqpAuthExternal:
      result := 'EXTERNAL';
  else
    result := '';
  end;
end;

procedure sgcWriteAMQPByte(const aValue: Byte; var aBytes: TBytes);
var
  vBytes: TBytes;
begin
  sgcWriteByte(aValue, vBytes);
  sgcReverseBytes(vBytes);
  sgcWriteBytes(vBytes, aBytes);
end;

function sgcGetAMQPAuthentications(const aValue: string)
  : TsgcAMQPAuthentications;
var
  i: Integer;
  oMechanisms: TStringList;
begin
  result := [];

  oMechanisms := TStringList.Create;
  Try
    oMechanisms.Delimiter := ' ';
    oMechanisms.DelimitedText := aValue;
    for i := 0 to oMechanisms.Count - 1 do
      result := result + [sgcGetAMQPAuthentication(oMechanisms[i])];
  Finally
    sgcFree(oMechanisms);
  End;
end;

procedure sgcWriteAMQPBoolean(const aValue: Boolean; var aBytes: TBytes);
begin
  if aValue then
    sgcWriteAMQPByte(1, aBytes)
  else
    sgcWriteAMQPByte(0, aBytes);
end;

function sgcGetAMQPMethod(const aClass: TsgcAMQPClass; const aValue: Integer)
  : TsgcAMQPMethod;
begin
  case aClass of
    amqpClassConnection:
      result := sgcGetAMQPConnection(aValue);
    amqpClassChannel:
      result := sgcGetAMQPChannel(aValue);
    amqpClassExchange:
      result := sgcGetAMQPExchange(aValue);
    amqpClassQueue:
      result := sgcGetAMQPQueue(aValue);
    amqpClassBasic:
      result := sgcGetAMQPBasic(aValue);
    amqpClassTx:
      result := sgcGetAMQPTX(aValue);
  else
    result := amqpMethodNone;
  end;
end;

procedure sgcWriteAMQPFieldTable(const aValue: string; var aBytes: TBytes);
var
  i: Integer;
  oJSON: TsgcJSON;
  vBytes: TBytes;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(aValue);
    for i := 0 to oJSON.Count - 1 do
    begin
      sgcWriteAMQPShortString(oJSON.Item[i].Name, vBytes);
      sgcWriteAMQPByte($53, vBytes);
      sgcWriteAMQPLongString(String(oJSON.Item[i].Value), vBytes);
    end;
  Finally
    sgcFree(oJSON);
  End;

  sgcWriteAMQPLongString(vBytes, aBytes);
end;

procedure sgcWriteAMQPLongString(const aSource: TBytes;
  var aBytes: TBytes); overload;
var
  vBytes: TBytes;
begin
  sgcWriteUInt32(Length(aSource), vBytes);
  sgcReverseBytes(vBytes);

  sgcWriteBytes(vBytes, aBytes);
  sgcWriteBytes(aSource, aBytes);
end;

procedure sgcWriteAMQPBit(aOffset, aBifOffset: Integer; var aBytes: TBytes);
begin
  case aBifOffset of
    0:
      aBytes[aOffset] := aBytes[aOffset] or $1;
    1:
      aBytes[aOffset] := aBytes[aOffset] or $2;
    2:
      aBytes[aOffset] := aBytes[aOffset] or $4;
    3:
      aBytes[aOffset] := aBytes[aOffset] or $8;
    4:
      aBytes[aOffset] := aBytes[aOffset] or $10;
    5:
      aBytes[aOffset] := aBytes[aOffset] or $20;
    6:
      aBytes[aOffset] := aBytes[aOffset] or $40;
    7:
      aBytes[aOffset] := aBytes[aOffset] or $80;
  end;
end;

function GetAMQPExceptionMessage(const aCode: Integer): string;
begin
  case aCode of
    CS_AMQP_ERROR_CONTENT_TOO_LARGE:
      result := S_AMQP_ERROR_CONTENT_TOO_LARGE;
    CS_AMQP_ERROR_NO_CONSUMERS:
      result := S_AMQP_ERROR_NO_CONSUMERS;
    CS_AMQP_ERROR_CONNECTION_FORCED:
      result := S_AMQP_ERROR_CONNECTION_FORCED;
    CS_AMQP_ERROR_INVALID_PATH:
      result := S_AMQP_ERROR_INVALID_PATH;
    CS_AMQP_ERROR_ACCESS_REFUSED:
      result := S_AMQP_ERROR_ACCESS_REFUSED;
    CS_AMQP_ERROR_NOT_FOUND:
      result := S_AMQP_ERROR_NOT_FOUND;
    CS_AMQP_ERROR_RESOURCE_LOCKED:
      result := S_AMQP_ERROR_RESOURCE_LOCKED;
    CS_AMQP_ERROR_PRECONDITION_FAILED:
      result := S_AMQP_ERROR_PRECONDITION_FAILED;
    CS_AMQP_ERROR_FRAME:
      result := S_AMQP_ERROR_FRAME;
    CS_AMQP_ERROR_SYNTAX:
      result := S_AMQP_ERROR_SYNTAX;
    CS_AMQP_ERROR_COMMAND_INVALID:
      result := S_AMQP_ERROR_COMMAND_INVALID;
    CS_AMQP_ERROR_CHANNEL:
      result := S_AMQP_ERROR_CHANNEL;
    CS_AMQP_ERROR_UNEXPECTED_FRAME:
      result := S_AMQP_ERROR_UNEXPECTED_FRAME;
    CS_AMQP_ERROR_RESOURCE:
      result := S_AMQP_ERROR_RESOURCE;
    CS_AMQP_ERROR_NOT_ALLOWED:
      result := S_AMQP_ERROR_NOT_ALLOWED;
    CS_AMQP_ERROR_NOT_IMPLEMENTED:
      result := S_AMQP_ERROR_NOT_IMPLEMENTED;
    CS_AMQP_ERROR_INTERNAL:
      result := S_AMQP_ERROR_INTERNAL;
  else
    result := '';
  end;
end;

end.
