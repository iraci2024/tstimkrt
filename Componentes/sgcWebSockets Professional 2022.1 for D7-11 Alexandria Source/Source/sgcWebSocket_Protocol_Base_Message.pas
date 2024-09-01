{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Protocol_Base_Message;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
  {$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  {$IFDEF MSWINDOWS}Windows{$ELSE}SyncObjs{$ENDIF},
  // sgcbase
  sgcBase_Classes,
  // sgcWebSocket
  sgcWebSocket_Classes, sgcJSON, sgcWebSocket_Classes_SyncObjs;

type

  TsgcWSAckMessage = class(TPersistent)
  private
    FConnection: TsgcWSConnection;
    FText: String;
    FDate: TDateTime;
    FID: String;
  public
    destructor Destroy; override;
  public
    property Connection: TsgcWSConnection read FConnection write FConnection;
    property Text: String read FText write FText;
    property Date: TDateTime read FDate write FDate;
    property ID: String read FID write FID;
  end;

  TsgcWSAckMessageList = class
    (TsgcThreadList{$IFDEF NEXTGEN}<TsgcWSAckMessage>{$ENDIF})
  private
    FOnException: TsgcTimerOnException;
    FOnMessage: TsgcWSMessageEvent;
    FTimeout: Integer;
    FTimer: TsgcTimer;
    FInterval: Integer;
  private
    procedure DoProcessList;
  protected
    procedure OnTimerEvent(Sender: TObject); virtual;
    procedure OnExceptionEvent(Sender: TObject; E: Exception); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure Clear; override;
  public
    procedure Add(const aMessage: TsgcWSAckMessage);
    procedure Delete(aID: string);
  public
    function GetMessage(const aID: String): TsgcWSAckMessage;
  public
    procedure Start;
    procedure Stop;
  public
    property Interval: Integer read FInterval write FInterval;
    property Timeout: Integer read FTimeout write FTimeout;
  public
    property OnException: TsgcTimerOnException read FOnException write FOnException;
    property OnMessage: TsgcWSMessageEvent read FOnMessage write FOnMessage;
  end;

  TsgcWSMessage_Base = class(TComponent)
  private
    FCS: TsgcCriticalSection;
  protected
    procedure DoEnterCS; virtual;
    procedure DoLeaveCS; virtual;
  private
    function GetObjectJSON(aJSONObject: IsgcObjectJSON = nil): IsgcJSON;
  private
    FJSON: IsgcJSON;
    function GetJSON: IsgcJSON;
  protected
    property JSON: IsgcJSON read GetJSON write FJSON;
  protected
    procedure Clear(aForceClear: Boolean = False); virtual;
  protected
    function DoReadJSONValue(const aField: string; aJSONObject: IsgcObjectJSON =
        nil): Variant; overload; virtual;
    function DoReadJSONValue(aItem: Integer; aJSONObject: TsgcObjectJSON = nil):
        Variant; overload; virtual;
  protected
    procedure DoWriteJSONValue(const aName, aValue: string; aJSONObject:
        IsgcObjectJSON = nil); overload; virtual;
    procedure DoWriteJSONValue(const aName: string; aValue: Integer; aJSONObject:
        IsgcObjectJSON = nil); overload; virtual;
    procedure DoWriteJSONValue(const aName: string; aValue: Int64; aJSONObject:
        IsgcObjectJSON = nil); overload; virtual;
    procedure DoWriteJSONValue(const aName: string; aValue: Double; aJSONObject:
        IsgcObjectJSON = nil); overload; virtual;
    procedure DoWriteJSONValue(const aName: string; aValue: Boolean; aJSONObject:
        IsgcObjectJSON = nil); overload; virtual;
  protected
    function DoAddJSONObject(const aName: String; const aValue: String = '';
        aJSONObject: IsgcObjectJSON = nil): IsgcObjectJSON;
  protected
    FIsReading: Boolean;
    FIsWriting: Boolean;
  public
    procedure DoEnterRead(const aText: String);
    procedure DoLeaveRead;
    procedure DoEnterWrite(aForceClear: Boolean = False);
    procedure DoLeaveWrite;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Read(const aMessage: String); virtual;
    function Write: string; virtual;
  private
    FThreadId: Cardinal;
    FConnectionId: String;
  public
    property ThreadId: Cardinal read FThreadId write FThreadId;
    property ConnectionId: String read FConnectionId write FConnectionId;
  private
    FEncodeBase64: Boolean;
  public
    property EncodeBase64: Boolean read FEncodeBase64 write FEncodeBase64;
  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses sgcWebSocket_Helpers;

constructor TsgcWSMessage_Base.Create(aOwner: TComponent);
begin
  inherited;
  FCS := TsgcCriticalSection.Create;
  EncodeBase64 := True;
end;

destructor TsgcWSMessage_Base.Destroy;
begin
  FreeJSONInstance(FJSON);
  sgcFree(FCS);
  inherited;
end;

function TsgcWSMessage_Base.DoAddJSONObject(const aName: String; const aValue: String = '';
    aJSONObject: IsgcObjectJSON = nil): IsgcObjectJSON;
begin
  result := GetObjectJSON.Node[aName];
  if not Assigned(result) then
    result := GetObjectJSON.AddObject(aName, aValue)
  else
    result.JSONObject.Read(aValue)
end;

procedure TsgcWSMessage_Base.Clear(aForceClear: Boolean = False);
begin
  if (not FIsWriting and not FIsReading) or (aForceClear = True) then
  begin
    JSON.Clear;
    FIsWriting := True;
  end;
end;

procedure TsgcWSMessage_Base.DoEnterCS;
begin
  FCS.Acquire;
end;

procedure TsgcWSMessage_Base.DoEnterRead(const aText: String);
begin
  if not FIsReading then
  begin
    Clear;
    FIsReading := True;
    JSON.Read(aText);
  end;
end;

procedure TsgcWSMessage_Base.DoEnterWrite(aForceClear: Boolean = False);
begin
  if not FIsWriting or aForceClear then
  begin
    Clear(aForceClear);
    FIsWriting := True;
  end;
end;

procedure TsgcWSMessage_Base.DoLeaveCS;
begin
  FCS.Release;
end;

procedure TsgcWSMessage_Base.DoLeaveRead;
begin
  FIsReading := False;
  JSON.Clear;
end;

procedure TsgcWSMessage_Base.DoLeaveWrite;
begin
  FIsWriting := False;
  Clear;
end;

function TsgcWSMessage_Base.DoReadJSONValue(const aField: string; aJSONObject:
    IsgcObjectJSON = nil): Variant;
begin
  Try
    if GetObjectJSON(aJSONObject).Node[aField] <> nil then
    begin
      if GetObjectJSON(aJSONObject).Node[aField].JSONType = sgcJSONObject then
        Result := GetObjectJSON(aJSONObject).Node[aField].JSONObject.Text
      else if GetObjectJSON(aJSONObject).Node[aField].JSONType = sgcJSONList then
      begin
        GetObjectJSON(aJSONObject).Node[aField].JSONObject.IsArray := True;
        Result := GetObjectJSON(aJSONObject).Node[aField].JSONObject.Text;
      end
      else if GetObjectJSON(aJSONObject).Node[aField].JSONType = sgcJSONString then
        Result := GetObjectJSON(aJSONObject).Node[aField].Value
      else
        Result := GetObjectJSON(aJSONObject).Node[aField].Value;
    end;
  Except
    Result := '';
  End;
end;

function TsgcWSMessage_Base.DoReadJSONValue(aItem: Integer; aJSONObject:
    TsgcObjectJSON = nil): Variant;
begin
  Try
    if aItem < JSON.count then
    begin
      if JSON.Item[aItem] <> nil then
      begin
        if JSON.Item[aItem].JSONType = sgcJSONString then
          Result := JSON.Item[aItem].Value
        else
          Result := JSON.Item[aItem].Value;
      end;
    end;
  Except
    Result := '';
  End;
end;

procedure TsgcWSMessage_Base.DoWriteJSONValue(const aName, aValue: string;
    aJSONObject: IsgcObjectJSON = nil);
begin
  GetObjectJSON(aJSONObject).AddPair(aName, aValue);
end;

procedure TsgcWSMessage_Base.DoWriteJSONValue(const aName: string; aValue:
    Integer; aJSONObject: IsgcObjectJSON = nil);
begin
  GetObjectJSON(aJSONObject).AddPair(aName, aValue);
end;

procedure TsgcWSMessage_Base.DoWriteJSONValue(const aName: string; aValue:
    Double; aJSONObject: IsgcObjectJSON = nil);
begin
  GetObjectJSON(aJSONObject).AddPair(aName, aValue);
end;

procedure TsgcWSMessage_Base.DoWriteJSONValue(const aName: string; aValue:
    Boolean; aJSONObject: IsgcObjectJSON = nil);
begin
  GetObjectJSON(aJSONObject).AddPair(aName, aValue);
end;

procedure TsgcWSMessage_Base.DoWriteJSONValue(const aName: string; aValue:
    Int64; aJSONObject: IsgcObjectJSON = nil);
begin
  GetObjectJSON(aJSONObject).AddPair(aName, aValue);
end;

function TsgcWSMessage_Base.GetJSON: IsgcJSON;
begin
  if not Assigned(FJSON) then
    FJSON := GetJSONInstance;
  Result := FJSON;
end;

function TsgcWSMessage_Base.GetObjectJSON(aJSONObject: IsgcObjectJSON = nil):
    IsgcJSON;
begin
  if Assigned(aJSONObject) then
    Result := aJSONObject.JSONObject
  else
    Result := JSON;
end;

procedure TsgcWSMessage_Base.Read(const aMessage: String);
begin
  DoLeaveRead;
end;

function TsgcWSMessage_Base.Write: string;
begin
  Try
    Result := String(JSON.Text);
  Finally
    DoLeaveWrite;
  End;
end;

destructor TsgcWSAckMessage.Destroy;
begin
  FConnection := nil;
  inherited;
end;

constructor TsgcWSAckMessageList.Create;
begin
  inherited;
  Interval := 60;
  Timeout := 300;
end;

destructor TsgcWSAckMessageList.Destroy;
begin
  sgcThreadFree(FTimer);
  Clear;
  inherited;
end;

procedure TsgcWSAckMessageList.Add(const aMessage: TsgcWSAckMessage);
var
  oMessage: TsgcWSAckMessage;
begin
  oMessage := GetMessage(aMessage.ID);
  if not Assigned(oMessage) then
  begin
    aMessage.Date := Now;
    AddKey(aMessage, aMessage.ID);
  end;
end;

procedure TsgcWSAckMessageList.Clear;
var
  i: Integer;
  oAckMessage: TsgcWSAckMessage;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSAckMessage>{$ENDIF};
begin
  oList := LockList;
  Try
    for i := oList.Count - 1 Downto 0 do
    begin
      oAckMessage := TsgcWSAckMessage(oList.Items[i]);
      DeleteKey(i, oAckMessage.ID);
      sgcFree(oAckMessage);
    end;
  Finally
    UnlockList;
  End;
  inherited;
end;

procedure TsgcWSAckMessageList.Delete(aID: string);
var
  i: Integer;
  oAckMessage: TsgcWSAckMessage;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSAckMessage>{$ENDIF};
begin
  oList := LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcWSAckMessage(oList.Items[i]).ID = aID then
      begin
        oAckMessage := TsgcWSAckMessage(oList.Items[i]);
        DeleteKey(i, aID);
        sgcFree(oAckMessage);
        break;
      end;
    end;
  Finally
    UnlockList;
  End;
end;

procedure TsgcWSAckMessageList.DoProcessList;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSAckMessage>{$ENDIF};
begin
  oList := LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if Trunc((Now - TsgcWSAckMessage(oList.Items[i]).Date) * 86400) > Timeout then
      begin
        TsgcWSAckMessage(oList.Items[i]).Date := Now;
        if Assigned(FOnMessage) then
          FOnMessage(TsgcWSAckMessage(oList.Items[i]).Connection, TsgcWSAckMessage(oList.Items[i]).Text);
      end;
    end;
  Finally
    UnlockList;
  End;
end;

procedure TsgcWSAckMessageList.Start;
begin
  if not Assigned(FTimer) then
  begin
    FTimer := TsgcTimer.Create;
    FTimer.DebugName := 'Ack Timer';
    FTimer.OnTimer := OnTimerEvent;
    FTimer.OnException := OnExceptionEvent;
  end;
  FTimer.Interval := Interval * 1000;

  if FTimer.Interval > 0 then
  begin
    if not FTimer.Enabled then
      FTimer.Enabled := True;
  end;
end;

procedure TsgcWSAckMessageList.Stop;
begin
  if Assigned(FTimer) then
    FTimer.Enabled := False;
end;

function TsgcWSAckMessageList.GetMessage(const aID: String): TsgcWSAckMessage;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSAckMessage>{$ENDIF};
begin
  result := nil;

  oList := LockList;
  Try
    case ListType of
      tdlThreadDictionary, tdlOnlyDictionary:
        begin
          result := TsgcWSAckMessage(GetValue(aID));
        end;
      tdlOnlyThread:
        begin
          for i := 0 to oList.Count - 1 do
          begin
            if TsgcWSAckMessage(oList.Items[i]).ID = aID then
            begin
              result := TsgcWSAckMessage(oList.Items[i]);
              break;
            end;
          end;
        end;
    end;
  Finally
    UnlockList;
  End;
end;

procedure TsgcWSAckMessageList.OnExceptionEvent(Sender: TObject; E: Exception);
begin
  if Assigned(FOnException) then
    FOnException(Sender, E);
end;

procedure TsgcWSAckMessageList.OnTimerEvent(Sender: TObject);
begin
  DoProcessList;
end;

{$ENDIF}

end.
