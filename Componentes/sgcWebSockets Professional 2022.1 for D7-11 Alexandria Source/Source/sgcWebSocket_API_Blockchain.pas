{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_API_Blockchain;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes, SysUtils,
  // sgc
  sgcWebSocket_Classes, sgcJSON;

type
  TsgcWSBitfinexNewTransactionEvent = procedure(Sender: TObject;
    Transaction: String) of object;
  TsgcWSBitfinexNewBlockEvent = procedure(Sender: TObject;
    Block: String) of object;

  TsgcWS_API_Blockchain = class(TsgcWSAPI_client)
    { from TsgcWSComponent }
  protected
    procedure DoEventMessage(aConnection: TsgcWSConnection;
      const Text: string); override;
    { from TsgcWSComponent }

    { from TsgcWSAPI_client }
  protected
    function GetURL: String; override;
    { from TsgcWSAPI_client }

    { JSON }
  private
    FJSON: TsgcJSON;
    function GetJSON: TsgcJSON;
  protected
    property JSON: TsgcJSON read GetJSON;
    { JSON }

    { procedures }
  public
    procedure Ping;
  public
    procedure SubscribeTransactions;
    procedure UnSubscribeTransactions;
  public
    procedure SubscribeAddress(const aAddress: String);
    procedure UnSubscribeAddress(const aAddress: String);
  public
    procedure SubscribeBlocks;
    procedure UnSubscribeBlocks;
    { procedures }

  public
    constructor Create(aOwner: TComponent); override;

    { events }
  private
    FOnBlockchainPong: TNotifyEvent;
    FOnBlockchainNewBlock: TsgcWSBitfinexNewBlockEvent;
    FOnBlockchainNewTransaction: TsgcWSBitfinexNewTransactionEvent;
  protected
    procedure DoEventPong;
    procedure DoEventNewTransaction(const aTransaction: String);
    procedure DoEventNewBlock(aBlock: String);
  published
    property OnBlockchainNewBlock
      : TsgcWSBitfinexNewBlockEvent read FOnBlockchainNewBlock write
      FOnBlockchainNewBlock;
    property OnBlockchainNewTransaction: TsgcWSBitfinexNewTransactionEvent read
      FOnBlockchainNewTransaction write FOnBlockchainNewTransaction;
    property OnBlockchainPong
      : TNotifyEvent read FOnBlockchainPong write FOnBlockchainPong;

    { events }
  end;
{$ENDIF}

implementation

{$IFDEF SGC_APIS}

constructor TsgcWS_API_Blockchain.Create(aOwner: TComponent);
begin
  inherited;
end;

procedure TsgcWS_API_Blockchain.DoEventMessage(aConnection: TsgcWSConnection;
  const Text: string);
begin
  JSON.Clear;
  JSON.Read(aConnection.MsgReceived);

  if JSON.Node['op'] <> nil then
  begin
    if JSON.Node['op'].Value = 'utx' then
      DoEventNewTransaction(aConnection.MsgReceived)
    else if JSON.Node['op'].Value = 'block' then
      DoEventNewBlock(aConnection.MsgReceived)
    else if JSON.Node['op'].Value = 'pong' then
      DoEventPong
    else
      inherited
  end
  else
    inherited;
end;

procedure TsgcWS_API_Blockchain.DoEventNewBlock(aBlock: String);
begin
  if Assigned(FOnBlockchainNewBlock) then
    FOnBlockchainNewBlock(self, aBlock);
end;

procedure TsgcWS_API_Blockchain.DoEventNewTransaction
  (const aTransaction: String);
begin
  if Assigned(FOnBlockchainNewTransaction) then
    FOnBlockchainNewTransaction(self, aTransaction);
end;

procedure TsgcWS_API_Blockchain.DoEventPong;
begin
  if Assigned(FOnBlockchainPong) then
    FOnBlockchainPong(self);
end;

function TsgcWS_API_Blockchain.GetJSON: TsgcJSON;
begin
  if not Assigned(FJSON) then
    FJSON := TsgcJSON.Create(self);
  Result := FJSON;
end;

function TsgcWS_API_Blockchain.GetURL: String;
begin
  Result := 'wss://ws.blockchain.info/inv/';
end;

procedure TsgcWS_API_Blockchain.Ping;
begin
  FClient.WriteData('{"op":"ping"}');
end;

procedure TsgcWS_API_Blockchain.SubscribeAddress(const aAddress: String);
begin
  FClient.WriteData('{"op":"addr_sub", "addr":"' + aAddress + '"}');
end;

procedure TsgcWS_API_Blockchain.SubscribeBlocks;
begin
  FClient.WriteData('{"op":"blocks_sub"}');
end;

procedure TsgcWS_API_Blockchain.SubscribeTransactions;
begin
  FClient.WriteData('{"op":"unconfirmed_sub"}');
end;

procedure TsgcWS_API_Blockchain.UnSubscribeAddress(const aAddress: String);
begin
  FClient.WriteData('{"op":"addr_unsub", "addr":"' + aAddress + '"}');
end;

procedure TsgcWS_API_Blockchain.UnSubscribeBlocks;
begin
  FClient.WriteData('{"op":"blocks_unsub"}');
end;

procedure TsgcWS_API_Blockchain.UnSubscribeTransactions;
begin
  FClient.WriteData('{"op":"unconfirmed_unsub"}');
end;
{$ENDIF}

end.
