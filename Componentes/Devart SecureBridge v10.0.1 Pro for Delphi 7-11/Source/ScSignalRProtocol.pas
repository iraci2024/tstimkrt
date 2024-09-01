//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2018-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSignalRProtocol;

interface

uses
  Classes, SysUtils, TypInfo, Variants,
  ScTypes, ScCLRClasses, ScUtils,
  ScJson, ScJsonSerializator, ScPipe,
  ScSignalRConsts, ScSignalRMessages;

type
  HubException = class(Exception);

  IScInvocationBinder = interface
    function GetParameterTypes(const MethodName: string; out ParamTypes: TExtTypeInfoArray;
      out AsArray: boolean): boolean;
    function GetReturnType(const InvocationId: string): TExtTypeInfo;
    function GetStreamItemType(const StreamId: string): TExtTypeInfo;
  end;

  TScTransferFormat = (tfBinary, tfText);
  TScTransferFormats = set of TScTransferFormat;

  TScTransferFormatHelper = class
  public
    class function ToString(Value: TScTransferFormat): string; {$IFDEF VER12P}reintroduce;{$ELSE}{$IFDEF FPC}reintroduce;{$ENDIF}{$ENDIF}
  end;

  TScTextMessageParser = class
  public
    class function TryParseMessage(var Buffer: TScReadOnlySequence; out Payload: TScReadOnlySequence): boolean;
  end;

  TScTextMessageFormatter = class
  public
    class procedure WriteRecordSeparator(Output: TStream); overload;
    class procedure WriteRecordSeparator(Output: TScPipeWriter); overload;
  end;

  TScHandshakeProtocol = class
  public
    class procedure WriteRequestMessage(RequestMessage: TScHandshakeRequestMessage; Output: TScPipeWriter);
    class procedure WriteResponseMessage(ResponseMessage: TScHandshakeResponseMessage; Output: TScPipeWriter);
    class function TryParseResponseMessage(var Buffer: TScReadOnlySequence; out ResponseMessage: TScHandshakeResponseMessage): boolean;
    class function TryParseRequestMessage(var Buffer: TScReadOnlySequence; out RequestMessage: TScHandshakeRequestMessage): boolean;
  end;

  TScHubProtocol = class
  private
    FName: string;
    FVersion: integer;
    FMinorVersion: integer;
    FTransferFormat: TScTransferFormat;
  public
    function TryParseMessage(var Input: TScReadOnlySequence; Binder: IScInvocationBinder; out HubMessage: TScHubMessage): boolean; virtual; abstract;
    procedure WriteMessage(HubMessage: TScHubMessage; Output: TStream); virtual; abstract;
    function GetMessageBytes(HubMessage: TScHubMessage): TBytes; virtual; abstract;
    function IsVersionSupported(AVersion: integer): boolean;  virtual; abstract;

    property Name: string read FName;
    property Version: integer read FVersion;
    property MinorVersion: integer read FMinorVersion;
    property TransferFormat: TScTransferFormat read FTransferFormat;
  end;

  TScJsonHubProtocol = class (TScHubProtocol)
  private
    FObjectSerializer: TObjectSerializer;
    FObjectDeserializer: TJSONToObjectDeserializer;

    function ParseMessage(Stream: TScReadOnlyStream; Binder: IScInvocationBinder): TScHubMessage;
    function ReadHeaders(Reader: TJSONTextReader): TStrValueStringList;

    procedure WriteMessageCore(HubMessage: TScHubMessage; Output: TStream);
    procedure WriteHeaders(Writer: TJSONTextWriter; HubMessage: TScHubInvocationMessage);
    procedure WriteCompletionMessage(Writer: TJSONTextWriter; HubMessage: TScCompletionMessage);
    procedure WriteCancelInvocationMessage(Writer: TJSONTextWriter; HubMessage: TScCancelInvocationMessage);
    procedure WriteStreamItemMessage(Writer: TJSONTextWriter; HubMessage: TScStreamItemMessage);
    procedure WriteInvocationMessage(Writer: TJSONTextWriter; HubMessage: TScInvocationMessage);
    procedure WriteStreamInvocationMessage(Writer: TJSONTextWriter; HubMessage: TScStreamInvocationMessage);
    procedure WriteCloseMessage(Writer: TJSONTextWriter; HubMessage: TScCloseMessage);
    procedure WriteArguments(Writer: TJSONTextWriter; const Arguments: TVariantArray; AsArray: boolean);
    procedure WriteStreamIds(Writer: TJSONTextWriter; const StreamIds: TStringArray);
    procedure WriteInvocationId(Writer: TJSONTextWriter; HubMessage: TScHubInvocationMessage);
    procedure WriteMessageType(Writer: TJSONTextWriter; MessageType: integer);

    function BindCancelInvocationMessage(const InvocationId: string): TScHubMessage;
    function BindCompletionMessage(const InvocationId, Error: string; const AResult: Variant;
      HasResult: boolean; Binder: IScInvocationBinder): TScHubMessage;
    function BindStreamItemMessage(const InvocationId: string; const Item: Variant;
      HasItem: boolean; Binder: IScInvocationBinder): TScHubMessage;
    function BindStreamInvocationMessage(const InvocationId, Target: string; const Arguments: TVariantArray;
      HasArguments: boolean; const StreamIds: TStringArray; Binder: IScInvocationBinder): TScHubMessage;
    function BindInvocationMessage(const InvocationId, Target: string; const Arguments: TVariantArray;
      AsArray: boolean; HasArguments: boolean; const StreamIds: TStringArray; Binder: IScInvocationBinder): TScHubMessage;
    function BindTypes(Args: TJSONArray; const ParamTypes: TExtTypeInfoArray; AsArray: boolean): TVariantArray;
    function BindCloseMessage(const Error: string): TScCloseMessage;
    function ApplyHeaders(HubMessage: TScHubMessage; Headers: TStrValueStringList): TScHubMessage;

  public
    constructor Create;
    destructor Destroy; override;

    function TryParseMessage(var Input: TScReadOnlySequence; Binder: IScInvocationBinder; out HubMessage: TScHubMessage): boolean; override;
    procedure WriteMessage(HubMessage: TScHubMessage; Output: TStream); override;
    function GetMessageBytes(HubMessage: TScHubMessage): TBytes; override;
    function IsVersionSupported(AVersion: integer): boolean;  override;
  end;

  TJsonUtils = class
  public
    class function CheckRead(Reader: TJSONTextReader): boolean;
    class function TokenToStr(TokenType: TJSONTag): string;
    class function ReadAsInt32(Reader: TJSONTextReader; const PropertyName: string): integer;
    class function ReadAsString(Reader: TJSONTextReader; const PropertyName: string): string;
  end;

implementation

const
  // This record separator is supposed to be used only for JSON payloads where $1e character
  // will not occur (is not a valid character) and therefore it is safe to not escape it
  TextMessageFormatter_RecordSeparator = $1e;

const
  ResultPropertyName = 'result';
  ItemPropertyName = 'item';
  InvocationIdPropertyName = 'invocationId';
  StreamIdsPropertyName = 'streamIds';
  TypePropertyName = 'type';
  ErrorPropertyName = 'error';
  TargetPropertyName = 'target';
  ArgumentsPropertyName = 'arguments';
  HeadersPropertyName = 'headers';
  ProtocolName = 'json';
  ProtocolVersion = 1;
  ProtocolPropertyName = 'protocol';
  ProtocolVersionPropertyName = 'version';
  MinorVersionPropertyName = 'minorVersion';

const
  InvocationMessageType = 1;
  StreamItemMessageType = 2;
  CompletionMessageType = 3;
  StreamInvocationMessageType = 4;
  CancelInvocationMessageType = 5;
  PingMessageType = 6;
  CloseMessageType = 7;

{ TScTransferFormatHelper }

class function TScTransferFormatHelper.ToString(Value: TScTransferFormat): string;
begin
  case Value of
    tfBinary:
      Result := 'Binary';
    tfText:
      Result := 'Text';
  else
    Assert(False);
  end;
end;

{ TScTextMessageParser }

class function TScTextMessageParser.TryParseMessage(var Buffer: TScReadOnlySequence; out Payload: TScReadOnlySequence): boolean;
var
  Pos: integer;
  TmpBuffer: TScReadOnlySequence;
  Position: TScSequencePosition;
begin
  if Buffer.IsSingleSegment then begin
    Pos := TScMemoryRefHelper.PosOf(Buffer.First, TextMessageFormatter_RecordSeparator);
    if Pos = -1 then begin
      Payload := nil;
      Result := False;
    end
    else begin
      Payload := Buffer.Slice(0, Pos);
      TmpBuffer := Buffer;
      Buffer := Buffer.Slice(Pos + 1);
      TmpBuffer.Free;
      Result := True;
    end;
  end
  else begin
    Position := Buffer.PositionOf(TextMessageFormatter_RecordSeparator);
    if Position.Obj = nil then begin
      Payload := nil;
      Result := False;
    end
    else begin
      Payload := Buffer.Slice(0, Position);
      TmpBuffer := Buffer;
      Buffer := Buffer.Slice(Buffer.GetPosition(1, Position));
      TmpBuffer.Free;
      Result := True;
    end;
  end;
end;

{ TScTextMessageFormatter }

class procedure TScTextMessageFormatter.WriteRecordSeparator(Output: TStream);
var
  b: byte;
begin
  b := TextMessageFormatter_RecordSeparator;
  Output.Write(b, 1);
end;

class procedure TScTextMessageFormatter.WriteRecordSeparator(Output: TScPipeWriter);
var
  MemoryRef: TScMemoryRef;
begin
  MemoryRef := Output.GetMemory;
  PByteArray(MemoryRef.Memory)[MemoryRef.Offset] := TextMessageFormatter_RecordSeparator;
  Output.Advance(1);
end;

{ TScHandshakeProtocol }

class procedure TScHandshakeProtocol.WriteRequestMessage(RequestMessage: TScHandshakeRequestMessage; Output: TScPipeWriter);
var
  OutputStream: TScPipeWriterStream;
  Writer: TJSONTextWriter;
begin
  Writer := nil;
  OutputStream := TScPipeWriterStream.Create(Output);
  try
    Writer := TJSONTextWriter.Create(OutputStream);

    Writer.WriteObjectBegin;
    Writer.WriteString(ProtocolPropertyName);
    Writer.WriteValueSeparator;
    Writer.WriteString(RequestMessage.Protocol);

    Writer.WriteElementSeparator;
    Writer.WriteString(ProtocolVersionPropertyName);
    Writer.WriteValueSeparator;
    Writer.WriteInt32(RequestMessage.Version);
    Writer.WriteObjectEnd;
  finally
    Writer.Free;
    OutputStream.Free;
  end;

  TScTextMessageFormatter.WriteRecordSeparator(Output);
end;

class procedure TScHandshakeProtocol.WriteResponseMessage(ResponseMessage: TScHandshakeResponseMessage; Output: TScPipeWriter);
var
  OutputStream: TScPipeWriterStream;
  Writer: TJSONTextWriter;
begin
  Writer := nil;
  OutputStream := TScPipeWriterStream.Create(Output);
  try
    Writer := TJSONTextWriter.Create(OutputStream);

    Writer.WriteObjectBegin;
    if ResponseMessage.Error <> '' then begin
      Writer.WriteString(ErrorPropertyName);
      Writer.WriteValueSeparator;
      Writer.WriteString(ResponseMessage.Error);
      Writer.WriteElementSeparator;
    end;

    Writer.WriteString(MinorVersionPropertyName);
    Writer.WriteValueSeparator;
    Writer.WriteInt32(ResponseMessage.MinorVersion);
    Writer.WriteObjectEnd;
  finally
    Writer.Free;
    OutputStream.Free;
  end;

  TScTextMessageFormatter.WriteRecordSeparator(Output);
end;

class function TScHandshakeProtocol.TryParseResponseMessage(var Buffer: TScReadOnlySequence;
  out ResponseMessage: TScHandshakeResponseMessage): boolean;
var
  Payload: TScReadOnlySequence;
  PayloadStream: TScReadOnlyStream;
  Reader: TJSONTextReader;
  MemberName: string;
  MinorVersion: integer;
  Error: string;
begin
  Result := False;
  if not TScTextMessageParser.TryParseMessage(Buffer, Payload) then begin
    ResponseMessage := nil;
    Exit;
  end;

  try
    Reader := nil;
    PayloadStream := TScReadOnlyStream.Create(Payload);
    try
      Reader := TJSONTextReader.Create;
      Reader.SetStream(PayloadStream);
      Reader.Initialize;

      TJsonUtils.CheckRead(Reader);
      if Reader.TokenType <> jtObject then
        raise InvalidDataException.CreateFmt(SUnexpectedJsonObjectTokenType, [TJsonUtils.TokenToStr(Reader.TokenType)]);

      MinorVersion := 0;
      Error := '';

      while TJsonUtils.CheckRead(Reader) do begin
        case Reader.TokenType of
          jtString: begin
            MemberName := Reader.ReadString;
            if not Reader.ReadValueSeparator then
              raise JSONException.Create(cInvalidValueSeparator);

            if MemberName = TypePropertyName then
              // a handshake response does not have a type
              // check the incoming message was not any other type of message
              raise InvalidDataException.Create(SExpectedHandshakeResponse)
            else
            if MemberName = ErrorPropertyName then
              Error := TJsonUtils.ReadAsString(Reader, ErrorPropertyName)
            else
            if MemberName = MinorVersionPropertyName then
              MinorVersion := TJsonUtils.ReadAsInt32(Reader, MinorVersionPropertyName)
            else
              Reader.Skip;
          end;
          jtObjectEnd:
            Break;
          else
            raise InvalidDataException.CreateFmt(SUnexpectedHandshakeResponseToken, [TJsonUtils.TokenToStr(Reader.TokenType)]);
        end;
      end;

      ResponseMessage := TScHandshakeResponseMessage.Create(MinorVersion, Error);
      Result := True;
    finally
      Reader.Free;
      PayloadStream.Free;
    end;
  finally
    Payload.Free;
  end;
end;

class function TScHandshakeProtocol.TryParseRequestMessage(var Buffer: TScReadOnlySequence;
  out RequestMessage: TScHandshakeRequestMessage): boolean;

  function GetPayloadAsString(Payload: TScReadOnlySequence): string;
  begin
    // For error messages, we want to print the payload as text
    Result := Encoding.UTF8.GetString(Payload.ToArray);
  end;

var
  Payload: TScReadOnlySequence;
  PayloadStream: TScReadOnlyStream;
  Reader: TJSONTextReader;
  MemberName: string;
  Protocol: string;
  ProtocolVersion: integer;
begin
  Result := False;
  if not TScTextMessageParser.TryParseMessage(Buffer, Payload) then begin
    RequestMessage := nil;
    Exit;
  end;

  try
    Reader := nil;
    PayloadStream := TScReadOnlyStream.Create(Payload);
    try
      Reader := TJSONTextReader.Create;
      Reader.SetStream(PayloadStream);
      Reader.Initialize;

      TJsonUtils.CheckRead(Reader);
      if Reader.TokenType <> jtObject then
        raise InvalidDataException.CreateFmt(SUnexpectedJsonObjectTokenType, [TJsonUtils.TokenToStr(Reader.TokenType)]);

      Protocol := '';
      ProtocolVersion := -1;

      while TJsonUtils.CheckRead(Reader) do begin
        case Reader.TokenType of
          jtString: begin
            MemberName := Reader.ReadString;
            if not Reader.ReadValueSeparator then
              raise JSONException.Create(cInvalidValueSeparator);

            if MemberName = ProtocolPropertyName then
              Protocol := TJsonUtils.ReadAsString(Reader, ProtocolPropertyName)
            else
            if MemberName = ProtocolVersionPropertyName then
              ProtocolVersion := TJsonUtils.ReadAsInt32(Reader, ProtocolVersionPropertyName)
            else
              Reader.Skip;
          end;
          jtObjectEnd:
            Break;
          else
            raise InvalidDataException.CreateFmt(SUnexpectedRequestToken, [TJsonUtils.TokenToStr(Reader.TokenType), GetPayloadAsString(Payload)]);
        end;
      end;

      if Protocol = '' then
        raise InvalidDataException.CreateFmt(SMissingRequiredProperty + SMessageContent, [ProtocolPropertyName, GetPayloadAsString(Payload)]);
      if ProtocolVersion = -1 then
        raise InvalidDataException.CreateFmt(SMissingRequiredProperty + SMessageContent, [ProtocolVersionPropertyName, GetPayloadAsString(Payload)]);

      RequestMessage := TScHandshakeRequestMessage.Create(Protocol, ProtocolVersion);
      Result := True;
    finally
      Reader.Free;
      PayloadStream.Free;
    end;
  finally
    Payload.Free;
  end;
end;

{ TJsonUtils }

class function TJsonUtils.CheckRead(Reader: TJSONTextReader): boolean;
begin
  if Reader.ReadTag = jtNone then
    raise InvalidDataException.Create(SUnexpectedJsonEnd);

  Result := True;
end;

class function TJsonUtils.TokenToStr(TokenType: TJSONTag): string;
begin
  Result := GetEnumName(TypeInfo(TJSONTag), integer(TokenType));
end;

class function TJsonUtils.ReadAsInt32(Reader: TJSONTextReader; const PropertyName: string): integer;
var
  Subtype: TJSONTag;
  Val: string;
begin
  Reader.ReadTag;

  if Reader.TokenType <> jtNumber then
    raise InvalidDataException.CreateFmt(SExpectedPropertyOfType, [PropertyName, TJsonUtils.TokenToStr(jtNumber)]);

  Val := Reader.ReadNumber(Subtype);
  Result := StrToInt(Val);
end;

class function TJsonUtils.ReadAsString(Reader: TJSONTextReader; const PropertyName: string): string;
begin
  Reader.ReadTag;

  if Reader.TokenType <> jtString then
    raise InvalidDataException.CreateFmt(SExpectedPropertyOfType, [PropertyName, TJsonUtils.TokenToStr(jtString)]);

  Result := Reader.ReadString;
end;

{ TScJsonHubProtocol }

constructor TScJsonHubProtocol.Create;
begin
  inherited Create;

  FObjectSerializer := TObjectSerializer.Create;
  FObjectDeserializer := TJSONToObjectDeserializer.Create;

  FName := ProtocolName;
  FVersion := ProtocolVersion;
  FTransferFormat := tfText;
end;

destructor TScJsonHubProtocol.Destroy;
begin
  FObjectSerializer.Free;
  FObjectDeserializer.Free;

  inherited;
end;

function TScJsonHubProtocol.IsVersionSupported(AVersion: integer): boolean;
begin
  Result := AVersion = FVersion;
end;

function TScJsonHubProtocol.TryParseMessage(var Input: TScReadOnlySequence;
  Binder: IScInvocationBinder; out HubMessage: TScHubMessage): boolean;
var
  Payload: TScReadOnlySequence;
  PayloadStream: TScReadOnlyStream;
begin
  if not TScTextMessageParser.TryParseMessage(Input, Payload) then begin
    HubMessage := nil;
    Result := False;
    Exit;
  end;

  PayloadStream := nil;
  try
    PayloadStream := TScReadOnlyStream.Create(Payload);
    HubMessage := ParseMessage(PayloadStream, Binder);
    Result := HubMessage <> nil;
  finally
    PayloadStream.Free;
    Payload.Free;
  end;
end;

function TScJsonHubProtocol.ParseMessage(Stream: TScReadOnlyStream; Binder: IScInvocationBinder): TScHubMessage;
var
  MessageType: integer;
  InvocationId, Target, Error: string;
  HasItem, HasResult, HasArguments: boolean;
  Item, AResult: Variant;
  ItemToken, ResultToken: TJSONValue;
  Headers: TStrValueStringList;
  Completed: boolean;
  Reader: TJSONTextReader;
  HubMessage: TScHubMessage;
  MemberName: string;
  ReturnType, ItemType: TExtTypeInfo;
  ParamTypes: TExtTypeInfoArray;
  AsArray: boolean;
  ArgumentsToken: TJSONValue;
  ArgumentBindingException: Exception;
  Arguments: TVariantArray;
  StreamIds: TStringArray;
  Deserializer: TJSONDeserializer;
  n: integer;
begin
   Result := nil;

  try
    MessageType := -1;
    InvocationId := '';
    Target := '';
    Error := '';
    HasItem := False;
    Item := Unassigned;
    ItemToken := nil;
    HasResult := False;
    AResult := Unassigned;
    ResultToken := nil;
    HasArguments := False;
    StreamIds := nil;
    Arguments := nil;
    ArgumentsToken := nil;
    ArgumentBindingException := nil;
    Headers := nil;
    Completed := False;
    HubMessage := nil;

    try
      Deserializer := TJSONDeserializer.Create;
      Reader := TJSONTextReader.Create;
      try
        Reader.SetStream(Stream);
        Reader.Initialize;

        TJsonUtils.CheckRead(Reader);

        // We're always parsing a JSON object
        if Reader.TokenType <> jtObject then
          raise InvalidDataException.CreateFmt(SUnexpectedJsonObjectTokenType, [TJsonUtils.TokenToStr(Reader.TokenType)]);

        repeat
          case Reader.TokenType of
            jtString: begin
              MemberName := Reader.ReadString;
              if not Reader.ReadValueSeparator then
                raise JSONException.Create(cInvalidValueSeparator);

              if MemberName = TypePropertyName then begin
                MessageType := TJsonUtils.ReadAsInt32(Reader, TypePropertyName);
              end
              else
              if MemberName = InvocationIdPropertyName then begin
                InvocationId := TJsonUtils.ReadAsString(Reader, InvocationIdPropertyName);
              end
              else
              if MemberName = StreamIdsPropertyName then begin
                TJsonUtils.CheckRead(Reader);

                if Reader.TokenType <> jtArray then
                  raise InvalidDataException.CreateFmt(SExpectedPropertyOfType, [StreamIdsPropertyName, TJsonUtils.TokenToStr(jtArray)]);

                n := 0;
                while not (Reader.ReadTag in [jtArrayEnd, jtNone]) do begin
                  if Reader.TokenType = jtComma then
                    Continue;

                  SetLength(StreamIds, n + 1);
                  StreamIds[n] := Reader.ReadString;
                  Inc(n);
                end;

                if Reader.TokenType = jtNone then
                  raise JSONException.Create(SUnexpectedJsonEnd);
              end
              else
              if MemberName = TargetPropertyName then begin
                Target := TJsonUtils.ReadAsString(Reader, TargetPropertyName);
              end
              else
              if MemberName = ErrorPropertyName then begin
                Error := TJsonUtils.ReadAsString(Reader, ErrorPropertyName);
              end
              else
              if MemberName = ResultPropertyName then begin
                ResultToken := Deserializer.FromJSONReader(Reader);
                if ResultToken = nil then
                  raise JSONException.Create(SUnexpectedJsonEnd);
                HasResult := True;
              end
              else
              if MemberName = ItemPropertyName then begin
                ItemToken := Deserializer.FromJSONReader(Reader);
                if ItemToken = nil then
                  raise JSONException.Create(SUnexpectedJsonEnd);
                HasItem := True;
              end
              else
              if MemberName = ArgumentsPropertyName then begin
                ArgumentsToken := Deserializer.FromJSONReader(Reader);
                if ArgumentsToken = nil then
                  raise JSONException.Create(SUnexpectedJsonEnd);
                if not (ArgumentsToken is TJSONArray) then
                  raise InvalidDataException.CreateFmt(SExpectedPropertyOfType, [ArgumentsPropertyName, TJsonUtils.TokenToStr(jtArray)]);

                HasArguments := True;
              end
              else
              if MemberName = HeadersPropertyName then begin
                TJsonUtils.CheckRead(Reader);
                Headers := ReadHeaders(Reader);
              end
              else begin
                // Skip the value for this property
                Reader.Skip;
              end;
            end;

            jtObjectEnd:
              Completed := True;
          end;

        until Completed or not TJsonUtils.CheckRead(Reader);

      finally
        Reader.Free;
        Deserializer.Free;
      end;

      case MessageType of
        InvocationMessageType: begin
          if ArgumentsToken <> nil then begin
            // We weren't able to bind the arguments because they came before the 'target', so try to bind now that we've read everything.
            try
            {$IFNDEF VER11P}
              SetLength(ParamTypes, 0);
            {$ENDIF}
              if Binder.GetParameterTypes(Target, ParamTypes, AsArray) then
                Arguments := BindTypes(ArgumentsToken as TJSONArray, ParamTypes, AsArray);
            except
              on E: Exception do
                ArgumentBindingException := CloneException(E);
            end;
          end;

          if ArgumentBindingException <> nil then begin
            HubMessage := TScInvocationBindingFailureMessage.Create(InvocationId, Target, ArgumentBindingException);
            ArgumentBindingException := nil;
          end
          else begin
            HubMessage := BindInvocationMessage(InvocationId, Target, Arguments, AsArray, HasArguments, StreamIds, Binder);
            Arguments := nil;
          end;
        end;

        StreamInvocationMessageType: begin
          if ArgumentsToken <> nil then begin
            // We weren't able to bind the arguments because they came before the 'target', so try to bind now that we've read everything.
            try
              if Binder.GetParameterTypes(Target, ParamTypes, AsArray) then
                Arguments := BindTypes(ArgumentsToken as TJSONArray, ParamTypes, AsArray);
            except
              on E: Exception do
                ArgumentBindingException := CloneException(E);
            end;
          end;

          if ArgumentBindingException <> nil then begin
            HubMessage := TScInvocationBindingFailureMessage.Create(InvocationId, Target, ArgumentBindingException);
            ArgumentBindingException := nil;
          end
          else begin
            HubMessage := BindStreamInvocationMessage(InvocationId, Target, Arguments, HasArguments, StreamIds, Binder);
            Arguments := nil;
          end;
        end;

        StreamItemMessageType: begin
          if ItemToken <> nil then begin
            try
              ItemType := Binder.GetStreamItemType(InvocationId);
              Item := FObjectDeserializer.ToVariant(ItemToken, ItemType.VarType, ItemType.ClassType);
            except
              on E: Exception do
                HubMessage := TScStreamBindingFailureMessage.Create(InvocationId, CloneException(E));
            end;
          end;

          if HubMessage = nil then begin
            HubMessage := BindStreamItemMessage(InvocationId, Item, HasItem, Binder);
            Item := Unassigned;
          end;
        end;

        CompletionMessageType: begin
          if ResultToken <> nil then begin
            ReturnType := Binder.GetReturnType(InvocationId);
            AResult := FObjectDeserializer.ToVariant(ResultToken, ReturnType.VarType, ReturnType.ClassType);
          end;

          HubMessage := BindCompletionMessage(InvocationId, Error, AResult, HasResult, Binder);
          AResult := Unassigned;
        end;

        CancelInvocationMessageType: begin
          HubMessage := BindCancelInvocationMessage(InvocationId);
        end;

        PingMessageType: begin
          Result := TScPingMessage.Create;
          Exit;
        end;

        CloseMessageType: begin
          Result := BindCloseMessage(Error);
          Exit;
        end;

        -1: begin
          raise InvalidDataException.CreateFmt(SMissingRequiredProperty, [TypePropertyName]);
        end;

        else begin
          // Future protocol changes can add message types, old clients can ignore them
          Result := nil;
          Exit;
        end;
      end;

      Result := ApplyHeaders(HubMessage, Headers);
      HubMessage := nil;
    finally
      if TVarData(Item).VType = varByRef then
        TObject(TVarData(Item).VPointer).Free;
      if TVarData(AResult).VType = varByRef then
        TObject(TVarData(AResult).VPointer).Free;

      ItemToken.Free;
      ResultToken.Free;
      ArgumentsToken.Free;
      ArgumentBindingException.Free;
      Headers.Free;
      HubMessage.Free;

      for n := 0 to Length(Arguments) - 1 do begin
        if TVarData(Arguments[n]).VType = varByRef then
          TObject(TVarData(Arguments[n]).VPointer).Free;
      end;
    end;
  except
    on jEx: JSONException do
      raise InvalidDataException.Create(SErrorReadingJson + #13#10 + jEx.Message);
  end;
end;

function TScJsonHubProtocol.ReadHeaders(Reader: TJSONTextReader): TStrValueStringList;
var
  PropertyName: string;
begin
  if Reader.TokenType <> jtObject then
    raise InvalidDataException.CreateFmt(SExpectedPropertyOfType, [HeadersPropertyName, TJsonUtils.TokenToStr(jtObject)]);

  Result := TStrValueStringList.Create;
  try
    while Reader.ReadTag <> jtNone do begin
      case Reader.TokenType of
        jtString: begin
          PropertyName := Reader.ReadString;
          if not Reader.ReadValueSeparator then
            raise JSONException.Create(cInvalidValueSeparator);

          TJsonUtils.CheckRead(Reader);

          if Reader.TokenType <> jtString then
            raise InvalidDataException.CreateFmt(SExpectedHeaderOfType, [PropertyName, TJsonUtils.TokenToStr(jtString)]);

          Result[PropertyName] := Reader.ReadString;
        end;

        jtObjectEnd:
          Exit;
      end;
    end;

    raise JSONException.Create(SUnexpectedHeadersEnd);
  except
    Result.Free;
    raise;
  end;
end;

function TScJsonHubProtocol.BindCancelInvocationMessage(const InvocationId: string): TScHubMessage;
begin
  if InvocationId = '' then
    raise InvalidDataException.CreateFmt(SMissingRequiredProperty, [InvocationIdPropertyName]);

  Result := TScCancelInvocationMessage.Create(InvocationId);
end;

function TScJsonHubProtocol.BindCompletionMessage(const InvocationId, Error: string; const AResult: Variant;
  HasResult: boolean; Binder: IScInvocationBinder): TScHubMessage;
begin
  if InvocationId = '' then
    raise InvalidDataException.CreateFmt(SMissingRequiredProperty, [InvocationIdPropertyName]);

  if (Error <> '') and HasResult then
    raise InvalidDataException.Create(SExpectedErrorOrResult);

  if HasResult then
    Result := TScCompletionMessage.Create(InvocationId, Error, AResult, True)
  else
    Result := TScCompletionMessage.Create(InvocationId, Error, Unassigned, False);
end;

function TScJsonHubProtocol.BindStreamItemMessage(const InvocationId: string; const Item: Variant;
  HasItem: boolean; Binder: IScInvocationBinder): TScHubMessage;
begin
  if InvocationId = '' then
    raise InvalidDataException.CreateFmt(SMissingRequiredProperty, [InvocationIdPropertyName]);

  if not HasItem then
    raise InvalidDataException.CreateFmt(SMissingRequiredProperty, [ItemPropertyName]);

  Result := TScStreamItemMessage.Create(InvocationId, Item);
end;

function TScJsonHubProtocol.BindStreamInvocationMessage(const InvocationId, Target: string;
  const Arguments: TVariantArray; HasArguments: boolean; const StreamIds: TStringArray; Binder: IScInvocationBinder): TScHubMessage;
begin
  if InvocationId = '' then
    raise InvalidDataException.CreateFmt(SMissingRequiredProperty, [InvocationIdPropertyName]);

  if Target = '' then
    raise InvalidDataException.CreateFmt(SMissingRequiredProperty, [TargetPropertyName]);

  if not HasArguments then
    raise InvalidDataException.CreateFmt(SMissingRequiredProperty, [ArgumentsPropertyName]);

  Result := TScStreamInvocationMessage.Create(InvocationId, Target, Arguments, True, StreamIds);
end;

function TScJsonHubProtocol.BindInvocationMessage(const InvocationId, Target: string;
  const Arguments: TVariantArray; AsArray: boolean; HasArguments: boolean;
  const StreamIds: TStringArray; Binder: IScInvocationBinder): TScHubMessage;
begin
  if Target = '' then
    raise InvalidDataException.CreateFmt(SMissingRequiredProperty, [TargetPropertyName]);

  if not HasArguments then
    raise InvalidDataException.CreateFmt(SMissingRequiredProperty, [ArgumentsPropertyName]);

  Result := TScInvocationMessage.Create(InvocationId, Target, Arguments, AsArray, True, StreamIds);
end;

function TScJsonHubProtocol.BindTypes(Args: TJSONArray; const ParamTypes: TExtTypeInfoArray;
  AsArray: boolean): TVariantArray;

  procedure ProcessArgs(Args: TJSONArray; ArgsCount: integer);
  var
    i: integer;
  begin
    SetLength(Result, ArgsCount);
    if ArgsCount = 0 then
      Exit;

    for i := 0 to ArgsCount - 1 do
      Result[i] := Unassigned;

    try
      for i := 0 to ArgsCount - 1 do begin
        if AsArray then
          Result[i] := FObjectDeserializer.ToVariant(Args.Elements[i], ParamTypes[0].VarType, ParamTypes[0].ClassType)
        else
          Result[i] := FObjectDeserializer.ToVariant(Args.Elements[i], ParamTypes[i].VarType, ParamTypes[i].ClassType);
      end;
    except
      for i := 0 to ArgsCount - 1 do begin
        if TVarData(Result[i]).VType = varByRef then
          TObject(TVarData(Result[i]).VPointer).Free;
        Result[i] := Unassigned;
      end;

      raise InvalidDataException.Create(SErrorBindingArguments);
    end;
  end;

var
  Deserializer: TJSONDeserializer;
  ParamsToken: TJSONValue;
  ParamCount, ArgsCount: integer;
begin
  ParamCount := Length(ParamTypes);
  ArgsCount := Args.Elements.Count;

  if ParamCount <> ArgsCount then
    raise InvalidDataException.CreateFmt(SInvocationProvidesUnexpectedArgument, [ArgsCount, ParamCount]);

  if AsArray then begin
    Deserializer := TJSONDeserializer.Create;
    try
      if ArgsCount <> 1 then
        raise InvalidDataException.CreateFmt(SInvocationProvidesUnexpectedArgument, [ArgsCount, ParamCount]);

      ParamsToken := Deserializer.FromText(Args.Elements[0].AsString);
      if not (ParamsToken is TJSONArray) then
        raise InvalidDataException.Create(SErrorBindingArguments);

      ArgsCount := TJSONArray(ParamsToken).Elements.Count;
      ProcessArgs(TJSONArray(ParamsToken), ArgsCount);
    finally
      Deserializer.Free;
    end;
  end
  else
    ProcessArgs(Args, ArgsCount);
end;

function TScJsonHubProtocol.BindCloseMessage(const Error: string): TScCloseMessage;
begin
  Result := TScCloseMessage.Create(Error);
end;

function TScJsonHubProtocol.ApplyHeaders(HubMessage: TScHubMessage; Headers: TStrValueStringList): TScHubMessage;
begin
  if (Headers <> nil) and (HubMessage is TScHubInvocationMessage) then
    TScHubInvocationMessage(HubMessage).Headers := Headers;

  Result := HubMessage;
end;

function TScJsonHubProtocol.GetMessageBytes(HubMessage: TScHubMessage): TBytes;
var
  StreamWriter: TStream;
begin
  StreamWriter := TMemoryStream.Create;
  try
    WriteMessage(HubMessage, StreamWriter);

    StreamWriter.Position := 0;
    SetLength(Result, StreamWriter.Size);
    if StreamWriter.Size > 0 then
      StreamWriter.Read(Result[0], Length(Result));
  finally
    StreamWriter.Free;
  end;
end;

procedure TScJsonHubProtocol.WriteMessage(HubMessage: TScHubMessage; Output: TStream);
begin
  WriteMessageCore(HubMessage, Output);
  TScTextMessageFormatter.WriteRecordSeparator(Output);
end;

procedure TScJsonHubProtocol.WriteMessageCore(HubMessage: TScHubMessage; Output: TStream);
var
  Writer: TJSONTextWriter;
begin
  Writer := TJSONTextWriter.Create(Output);
  try
    Writer.WriteObjectBegin;

    if HubMessage is TScInvocationMessage then begin
      WriteMessageType(Writer, InvocationMessageType);
      WriteHeaders(Writer, TScInvocationMessage(HubMessage));
      WriteInvocationMessage(Writer, TScInvocationMessage(HubMessage));
    end
    else
    if HubMessage is TScStreamInvocationMessage then begin
      WriteMessageType(Writer, StreamInvocationMessageType);
      WriteHeaders(Writer, TScStreamInvocationMessage(HubMessage));
      WriteStreamInvocationMessage(Writer, TScStreamInvocationMessage(HubMessage));
    end
    else
    if HubMessage is TScStreamItemMessage then begin
      WriteMessageType(Writer, StreamItemMessageType);
      WriteHeaders(Writer, TScStreamItemMessage(HubMessage));
      WriteStreamItemMessage(Writer, TScStreamItemMessage(HubMessage));
    end
    else
    if HubMessage is TScCompletionMessage then begin
      WriteMessageType(Writer, CompletionMessageType);
      WriteHeaders(Writer, TScCompletionMessage(HubMessage));
      WriteCompletionMessage(Writer, TScCompletionMessage(HubMessage));
    end
    else
    if HubMessage is TScCancelInvocationMessage then begin
      WriteMessageType(Writer, CancelInvocationMessageType);
      WriteHeaders(Writer, TScCancelInvocationMessage(HubMessage));
      WriteCancelInvocationMessage(Writer, TScCancelInvocationMessage(HubMessage));
    end
    else
    if HubMessage is TScPingMessage then begin
      WriteMessageType(Writer, PingMessageType);
    end
    else
    if HubMessage is TScCloseMessage then begin
      WriteMessageType(Writer, CloseMessageType);
      WriteCloseMessage(Writer, TScCloseMessage(HubMessage));
    end
    else
      raise InvalidOperationException.CreateFmt(SUnsupportedMessageType, [HubMessage.ClassName]);

    Writer.WriteObjectEnd;
  finally
    Writer.Free;
  end;
end;

procedure TScJsonHubProtocol.WriteHeaders(Writer: TJSONTextWriter; HubMessage: TScHubInvocationMessage);
var
  i: integer;
begin
  if (HubMessage.Headers <> nil) and (HubMessage.Headers.Count > 0) then begin
    Writer.WriteElementSeparator;
    Writer.WriteString(HeadersPropertyName);
    Writer.WriteValueSeparator;
    Writer.WriteObjectBegin;

    for i := 0 to HubMessage.Headers.Count - 1 do begin
      if i > 0 then
        Writer.WriteElementSeparator;

      Writer.WriteString(HubMessage.Headers.Keys[i]);
      Writer.WriteValueSeparator;
      Writer.WriteString(HubMessage.Headers.Values[i]);
    end;

    Writer.WriteObjectEnd;
  end;
end;

procedure TScJsonHubProtocol.WriteCompletionMessage(Writer: TJSONTextWriter; HubMessage: TScCompletionMessage);
begin
  WriteInvocationId(Writer, HubMessage);

  if HubMessage.Error <> '' then begin
    Writer.WriteElementSeparator;
    Writer.WriteString(ErrorPropertyName);
    Writer.WriteValueSeparator;
    Writer.WriteString(HubMessage.Error);
  end
  else
  if HubMessage.HasResult then begin
    Writer.WriteElementSeparator;
    Writer.WriteString(ResultPropertyName);
    Writer.WriteValueSeparator;
    FObjectSerializer.WriteVariant(Writer, HubMessage.Result);
  end;
end;

procedure TScJsonHubProtocol.WriteCancelInvocationMessage(Writer: TJSONTextWriter; HubMessage: TScCancelInvocationMessage);
begin
  WriteInvocationId(Writer, HubMessage);
end;

procedure TScJsonHubProtocol.WriteStreamItemMessage(Writer: TJSONTextWriter; HubMessage: TScStreamItemMessage);
begin
  WriteInvocationId(Writer, HubMessage);

  Writer.WriteElementSeparator;
  Writer.WriteString(ItemPropertyName);
  Writer.WriteValueSeparator;
  FObjectSerializer.WriteVariant(Writer, HubMessage.Item);
end;

procedure TScJsonHubProtocol.WriteInvocationMessage(Writer: TJSONTextWriter; HubMessage: TScInvocationMessage);
begin
  WriteInvocationId(Writer, HubMessage);

  Writer.WriteElementSeparator;
  Writer.WriteString(TargetPropertyName);
  Writer.WriteValueSeparator;
  Writer.WriteString(HubMessage.Target);

  WriteArguments(Writer, HubMessage.Arguments, HubMessage.AsArray);
  WriteStreamIds(Writer, HubMessage.StreamIds);
end;

procedure TScJsonHubProtocol.WriteStreamInvocationMessage(Writer: TJSONTextWriter; HubMessage: TScStreamInvocationMessage);
begin
  WriteInvocationId(Writer, HubMessage);

  Writer.WriteElementSeparator;
  Writer.WriteString(TargetPropertyName);
  Writer.WriteValueSeparator;
  Writer.WriteString(HubMessage.Target);

  WriteArguments(Writer, HubMessage.Arguments, HubMessage.AsArray);
  WriteStreamIds(Writer, HubMessage.StreamIds);
end;

procedure TScJsonHubProtocol.WriteCloseMessage(Writer: TJSONTextWriter; HubMessage: TScCloseMessage);
begin
  if HubMessage.Error <> '' then begin
    Writer.WriteElementSeparator;
    Writer.WriteString(ErrorPropertyName);
    Writer.WriteValueSeparator;
    Writer.WriteString(HubMessage.Error);
  end;
end;

procedure TScJsonHubProtocol.WriteArguments(Writer: TJSONTextWriter;
  const Arguments: TVariantArray; AsArray: boolean);
var
  i: integer;
begin
  Writer.WriteElementSeparator;
  Writer.WriteString(ArgumentsPropertyName);
  Writer.WriteValueSeparator;
  Writer.WriteArrayBegin;

  if AsArray then
    Writer.WriteArrayBegin;

  for i := 0 to Length(Arguments) - 1 do begin
    if i > 0 then
      Writer.WriteElementSeparator;

    FObjectSerializer.WriteVariant(Writer, Arguments[i]);
  end;

  if AsArray then
    Writer.WriteArrayEnd;

  Writer.WriteArrayEnd;
end;

procedure TScJsonHubProtocol.WriteStreamIds(Writer: TJSONTextWriter; const StreamIds: TStringArray);
var
  i: integer;
begin
  if Length(StreamIds) = 0 then
    Exit;

  Writer.WriteElementSeparator;
  Writer.WriteString(StreamIdsPropertyName);
  Writer.WriteValueSeparator;
  Writer.WriteArrayBegin;

  for i := 0 to Length(StreamIds) - 1 do begin
    if i > 0 then
      Writer.WriteElementSeparator;

    Writer.WriteString(StreamIds[i]);
  end;

  Writer.WriteArrayEnd;
end;

procedure TScJsonHubProtocol.WriteInvocationId(Writer: TJSONTextWriter; HubMessage: TScHubInvocationMessage);
begin
  if HubMessage.InvocationId <> '' then begin
    Writer.WriteElementSeparator;
    Writer.WriteString(InvocationIdPropertyName);
    Writer.WriteValueSeparator;
    Writer.WriteString(HubMessage.InvocationId);
  end;
end;

procedure TScJsonHubProtocol.WriteMessageType(Writer: TJSONTextWriter; MessageType: integer);
begin
  Writer.WriteString(TypePropertyName);
  Writer.WriteValueSeparator;
  Writer.WriteInt32(MessageType);
end;

end.
