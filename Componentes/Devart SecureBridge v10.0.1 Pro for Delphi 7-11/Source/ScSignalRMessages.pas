//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2018-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSignalRMessages;

interface

uses
  Classes, SysUtils, SyncObjs, Variants, TypInfo,
  ScTypes, ScCLRClasses,
  ScSignalRConsts;

type
  TVariantArray = array of Variant;

  TExtTypeInfo = record
    VarType: TVarType;
    ClassType: TClass;
  end;
  TExtTypeInfoArray = array of TExtTypeInfo;

  TScOnWriteItem = procedure (Sender: TObject; const Item: Variant) of object;
  TScOnComplete = procedure (Sender: TObject; const Item: Variant) of object;
  TScOnFail = procedure (Sender: TObject; E: Exception) of object;

  TScInvocationCaller = class(TPersistent)
  private
    FOnWriteItem: TScOnWriteItem;
    FOnComplete: TScOnComplete;
    FOnFail: TScOnFail;

  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure WriteItem(var Item: Variant); virtual;
    procedure Complete(var Item: Variant); virtual;
    procedure Fail(E: Exception); virtual;

    property OnWriteItem: TScOnWriteItem read FOnWriteItem write FOnWriteItem;
    property OnComplete: TScOnComplete read FOnComplete write FOnComplete;
    property OnFail: TScOnFail read FOnFail write FOnFail;
  end;

  TScHubMessage = class {$IFDEF VER12P}abstract{$ENDIF}
  end;

  TScHubInvocationMessage = class(TScHubMessage)
  private
    FHeaders: TStrValueStringList;
    FInvocationId: string;
    procedure SetHeaders(Value: TStrValueStringList);

  public
    constructor Create(const AInvocationId: string);
    destructor Destroy; override;

    property Headers: TStrValueStringList read FHeaders write SetHeaders;
    property InvocationId: string read FInvocationId;
  end;

  TScHubMethodInvocationMessage = class(TScHubInvocationMessage)
  private
    FTarget: string;
    FStreamIds: TStringArray;
    FArguments: TVariantArray;
    FAsArray: boolean;
    FOwnsObjects: boolean;

    function ArgumentsToStr: string;
    function StreamIdsToStr: string;

  public
    constructor Create(const AInvocationId, ATarget: string;
      const AArguments: TVariantArray; AAsArray: boolean; AOwnsObjects: boolean;
      const AStreamIds: TStringArray = nil);
    destructor Destroy; override;

    property Target: string read FTarget;
    property Arguments: TVariantArray read FArguments;
    property AsArray: boolean read FAsArray;
    property StreamIds: TStringArray read FStreamIds;
  end;

  TScInvocationMessage = class(TScHubMethodInvocationMessage)
  private
  public
    constructor Create(const ATarget: string; const AArguments: TVariantArray;
      AAsArray: boolean; AOwnsObjects: boolean); overload;
    constructor Create(const AInvocationId, ATarget: string;
      const AArguments: TVariantArray; AAsArray: boolean; AOwnsObjects: boolean;
      const AStreamIds: TStringArray = nil); overload;

    function ToString: string; {$IFDEF VER12P}override;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}
  end;

  TScStreamInvocationMessage = class(TScHubMethodInvocationMessage)
  private
  public
    constructor Create(const AInvocationId, ATarget: string;
      const AArguments: TVariantArray; AOwnsObjects: boolean; const AStreamIds: TStringArray = nil);

    function ToString: string; {$IFDEF VER12P}override;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}
  end;

  TScHandshakeRequestMessage = class(TScHubMessage)
  private
    FProtocol: string;
    FVersion: integer;
  public
    constructor Create(const AProtocol: string; AVersion: integer);

    property Protocol: string read FProtocol;
    property Version: integer read FVersion;
  end;

  TScHandshakeResponseMessage = class(TScHubMessage)
  private
    FError: string;
    FMinorVersion: integer;
  public
    constructor Create(AMinorVersion: integer; const AError: string);

    property Error: string read FError;
    property MinorVersion: integer read FMinorVersion;
  end;

  TScCancelInvocationMessage = class(TScHubInvocationMessage)
  public
    constructor Create(const AInvocationId: string);
  end;

  TScCloseMessage = class(TScHubMessage)
  private
    FError: string;
  public
    constructor Create(const AError: string);

    property Error: string read FError;
  end;

  TScCompletionMessage = class(TScHubInvocationMessage)
  private
    FError: string;
    FHasResult: boolean;
  public
    Result: Variant;

    constructor Create(const AInvocationId, AError: string; const AResult: Variant; AHasResult: boolean);
    destructor Destroy; override;

    function ToString: string; {$IFDEF VER12P}override;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}

    class function WithError(const InvocationId, Error: string): TScCompletionMessage;
    class function WithResult(const InvocationId: string; const Payload: Variant): TScCompletionMessage;
    class function Empty(const InvocationId: string): TScCompletionMessage;

    property Error: string read FError;
    property HasResult: boolean read FHasResult;
  end;

  TScInvocationBindingFailureMessage = class(TScHubInvocationMessage)
  private
    FBindingFailure: Exception;
    FTarget: string;
  public
    constructor Create(const AInvocationId, ATarget: string; ABindingFailure: Exception);
    destructor Destroy; override;

    property BindingFailure: Exception read FBindingFailure;
    property Target: string read FTarget;
  end;

  TScPingMessage = class(TScHubMessage)
  public
    constructor Create;
    class function Default: TScPingMessage;
  end;

  TScStreamBindingFailureMessage = class(TScHubMessage)
  private
    FBindingFailure: Exception;
    FId: string;
  public
    constructor Create(const AId: string; ABindingFailure: Exception);
    destructor Destroy; override;

    property BindingFailure: Exception read FBindingFailure;
    property Id: string read FId;
  end;

  TScStreamItemMessage = class(TScHubInvocationMessage)
  private
  public
    Item: Variant;

    constructor Create(const AInvocationId: string; const AItem: Variant);
    destructor Destroy; override;

    function ToString: string; {$IFDEF VER12P}override;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}
  end;

implementation

var
  CreateLock: TCriticalSection;
  DefaultPingMessage: TScPingMessage = nil;

{ TScInvocationCaller }

procedure TScInvocationCaller.AssignTo(Dest: TPersistent);
begin
  if Dest is TScInvocationCaller then begin
    TScInvocationCaller(Dest).FOnWriteItem := FOnWriteItem;
    TScInvocationCaller(Dest).FOnComplete := FOnComplete;
    TScInvocationCaller(Dest).FOnFail := FOnFail;
  end
  else
    inherited;
end;

procedure TScInvocationCaller.WriteItem(var Item: Variant);
begin
  try
    if Assigned(FOnWriteItem) then
      FOnWriteItem(Self, Item);
  except
  end;
end;

procedure TScInvocationCaller.Complete(var Item: Variant);
begin
  try
    if Assigned(FOnComplete) then
      FOnComplete(Self, Item);
  except
  end;
end;

procedure TScInvocationCaller.Fail(E: Exception);
begin
  try
    if Assigned(FOnFail) then
      FOnFail(Self, E);
  except
  end;
end;

{ TScHubInvocationMessage }

constructor TScHubInvocationMessage.Create(const AInvocationId: string);
begin
  inherited Create;

  FInvocationId := AInvocationId;
end;

destructor TScHubInvocationMessage.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

procedure TScHubInvocationMessage.SetHeaders(Value: TStrValueStringList);
begin
  if Value = nil then
    FreeAndNil(FHeaders)
  else begin
    if FHeaders = nil then
      FHeaders := TStrValueStringList.Create;

    FHeaders.Assign(Value);
  end;
end;

{ TScHubMethodInvocationMessage }

constructor TScHubMethodInvocationMessage.Create(const AInvocationId, ATarget: string;
  const AArguments: TVariantArray; AAsArray: boolean; AOwnsObjects: boolean;
  const AStreamIds: TStringArray = nil);
begin
  inherited Create(AInvocationId);

  if ATarget = '' then
    raise ArgumentException.Create('Target');

  FTarget := ATarget;
  FArguments := AArguments;
  FAsArray := AAsArray;
  FOwnsObjects := AOwnsObjects;
  FStreamIds := AStreamIds;
end;

destructor TScHubMethodInvocationMessage.Destroy;
var
  i: integer;
begin
  if FOwnsObjects then
    for i := 0 to Length(FArguments) - 1 do begin
      if TVarData(FArguments[i]).VType = varByRef then
        TObject(TVarData(FArguments[i]).VPointer).Free;
    end;

  inherited;
end;

function TScHubMethodInvocationMessage.ArgumentsToStr: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Length(FArguments) - 1 do begin
    if Result <> '' then
      Result := Result + ', ';

    if VarIsNull(FArguments[i]) or VarIsEmpty(FArguments[i]) then
      Result := Result + '<null>'
    else
      Result := Result + FArguments[i];
  end;
end;

function TScHubMethodInvocationMessage.StreamIdsToStr: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Length(FStreamIds) - 1 do begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + FStreamIds[i];
  end;
end;

{ TScInvocationMessage }

constructor TScInvocationMessage.Create(const ATarget: string; const AArguments: TVariantArray;
  AAsArray: boolean; AOwnsObjects: boolean);
begin
  inherited Create('', ATarget, AArguments, AAsArray, AOwnsObjects);
end;

constructor TScInvocationMessage.Create(const AInvocationId, ATarget: string;
  const AArguments: TVariantArray; AAsArray: boolean; AOwnsObjects: boolean;
  const AStreamIds: TStringArray = nil);
begin
  inherited Create(AInvocationId, ATarget, AArguments, AAsArray, AOwnsObjects, AStreamIds);
end;

function TScInvocationMessage.ToString: string;
var
  Args, AStreamIds: string;
begin
  Args := ArgumentsToStr;
  AStreamIds := StreamIdsToStr;
  Result := Format('InvocationMessage { InvocationId: "%s", Target: "$s", Arguments: [ %s ], StreamIds: [ %s ] }', [FInvocationId, FTarget, Args, AStreamIds]);
end;

{ TScStreamInvocationMessage }

constructor TScStreamInvocationMessage.Create(const AInvocationId, ATarget: string;
  const AArguments: TVariantArray; AOwnsObjects: boolean; const AStreamIds: TStringArray = nil);
begin
  inherited Create(AInvocationId, ATarget, AArguments, False, AOwnsObjects, AStreamIds);
end;

function TScStreamInvocationMessage.ToString: string;
var
  Args, AStreamIds: string;
begin
  Args := ArgumentsToStr;
  AStreamIds := StreamIdsToStr;
  Result := Format('StreamInvocation { InvocationId: "%s", Target: "$s", Arguments: [ %s ], StreamIds: [ %s ] }', [FInvocationId, FTarget, Args, AStreamIds]);
end;

{ TScHandshakeRequestMessage }

constructor TScHandshakeRequestMessage.Create(const AProtocol: string; AVersion: integer);
begin
  inherited Create;

  FProtocol := AProtocol;
  FVersion := AVersion;
end;

{ TScHandshakeResponseMessage }

constructor TScHandshakeResponseMessage.Create(AMinorVersion: integer; const AError: string);
begin
  inherited Create;

  FError := AError;
  FMinorVersion := AMinorVersion;
end;

{ TScCancelInvocationMessage }

constructor TScCancelInvocationMessage.Create(const AInvocationId: string);
begin
  inherited Create(AInvocationId);
end;

{ TScCloseMessage }

constructor TScCloseMessage.Create(const AError: string);
begin
  inherited Create;

  FError := AError;
end;

{ TScCompletionMessage }

constructor TScCompletionMessage.Create(const AInvocationId, AError: string; const AResult: Variant; AHasResult: boolean);
begin
  inherited Create(AInvocationId);

  if (AError <> '') and AHasResult then
    raise ArgumentException.Create(SExpectedErrorOrResult);

  FError := AError;
  Self.Result := AResult;
  FHasResult := AHasResult;
end;

destructor TScCompletionMessage.Destroy;
begin
  if TVarData(Self.Result).VType = varByRef then
    TObject(TVarData(Self.Result).VPointer).Free;

  inherited;
end;

function TScCompletionMessage.ToString: string;
var
  ErrorStr, ResultStr: string;
begin
  if FError = '' then
    ErrorStr := '<null>'
  else
    ErrorStr := '"' + Error + '"';

  if HasResult then
    if VarIsNull(Self.Result) or VarIsEmpty(Self.Result) then
      ResultStr := ', Result: <null>'
    else
      ResultStr := ', Result: ' + string(Self.Result)
  else
    ResultStr := '';

  Result := Format('Completion { InvocationId: "%s", Error: %s%s }', [FInvocationId, ErrorStr, ResultStr]);
end;

class function TScCompletionMessage.WithError(const InvocationId, Error: string): TScCompletionMessage;
begin
  Result := TScCompletionMessage.Create(InvocationId, Error, Unassigned, False);
end;

class function TScCompletionMessage.WithResult(const InvocationId: string; const Payload: Variant): TScCompletionMessage;
begin
  Result := TScCompletionMessage.Create(InvocationId, '', Payload, True);
end;

class function TScCompletionMessage.Empty(const InvocationId: string): TScCompletionMessage;
begin
  Result := TScCompletionMessage.Create(InvocationId, '', Unassigned, False);
end;

{ TScInvocationBindingFailureMessage }

constructor TScInvocationBindingFailureMessage.Create(const AInvocationId, ATarget: string; ABindingFailure: Exception);
begin
  inherited Create(AInvocationId);

  FTarget := ATarget;
  FBindingFailure := ABindingFailure;
end;

destructor TScInvocationBindingFailureMessage.Destroy;
begin
  FBindingFailure.Free;
  inherited;
end;

{ TScPingMessage }

constructor TScPingMessage.Create;
begin
  inherited Create;
end;

class function TScPingMessage.Default: TScPingMessage;
begin
  CreateLock.Enter;
  try
    if DefaultPingMessage = nil then
      DefaultPingMessage := TScPingMessage.Create;
  finally
    CreateLock.Leave;
  end;

  Result := DefaultPingMessage;
end;

{ TScStreamBindingFailureMessage }

constructor TScStreamBindingFailureMessage.Create(const AId: string; ABindingFailure: Exception);
begin
  inherited Create;

  FId := AId;
  FBindingFailure := ABindingFailure;
end;

destructor TScStreamBindingFailureMessage.Destroy;
begin
  FBindingFailure.Free;
  inherited;
end;

{ TScStreamItemMessage }

constructor TScStreamItemMessage.Create(const AInvocationId: string; const AItem: Variant);
begin
  inherited Create(AInvocationId);

  Item := AItem;
end;

destructor TScStreamItemMessage.Destroy;
begin
  if TVarData(Item).VType = varByRef then
    TObject(TVarData(Item).VPointer).Free;

  inherited;
end;

function TScStreamItemMessage.ToString: string;
var
  ItemStr: string;
begin
  if VarIsNull(Item) or VarIsEmpty(Item) then
    ItemStr := '<null>'
  else
    ItemStr := string(Item);

  Result := Format('StreamItem { InvocationId: "%s", Item: %s }', [FInvocationId, ItemStr]);
end;

initialization
  CreateLock := TCriticalSection.Create;

finalization
  CreateLock.Free;
  DefaultPingMessage.Free;

end.
