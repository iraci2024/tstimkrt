{ ***************************************************************************
  sgcBase component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcBase_Sync;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils,
  // sgc
  sgcBase_Classes;

type
  TsgcSyncOnMessage = procedure(Sender: TObject; const Value: string) of object;

  TsgcSyncMessages = class(TsgcThreadSafeBase)
  private
    FData: TStringList;
    FOnMessage: TsgcSyncOnMessage;
    function GetData: TStringList;
  protected
    property Data: TStringList read GetData write FData;
  protected
    procedure DoData;
  public
    procedure Synchronize(const aText: String);
    procedure Queue(const aText: String);
  public
    property OnMessage: TsgcSyncOnMessage read FOnMessage write FOnMessage;
  end;

  function sgcSyncMessages: TsgcSyncMessages;


implementation

uses
  sgcBase_Helpers;

var
  oSyncMessages: TsgcSyncMessages = nil;

function sgcSyncMessages: TsgcSyncMessages;
begin
  if not Assigned(oSyncMessages) then
    oSyncMessages := TsgcSyncMessages.Create;
  result := oSyncMessages;
end;

procedure TsgcSyncMessages.DoData;
begin
  DoEnterCS;
  Try
    while Data.Count > 0 do
    begin
      if Assigned(FOnMessage) then
        FOnMessage(self, Data[0]);
      Data.Delete(0);
    end;
  Finally
    DoLeaveCS;
  End;
end;

function TsgcSyncMessages.GetData: TStringList;
begin
  if not Assigned(FData) then
    FData := TStringList.Create;
  Result := FData;
end;

procedure TsgcSyncMessages.Queue(const aText: String);
begin
  {$IFDEF D2010}
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnMessage) then
        FOnMessage(self, aText);
    end);
  {$ELSE}
  DoEnterCS;
  Try
    Data.Add(aText);
  Finally
    DoLeaveCS;
  End;
  {$IFDEF D7}
  TThread.Synchronize(nil, DoData);
  {$ELSE}
  TThread.Queue(nil, DoData);
  {$ENDIF}
  {$ENDIF}
end;

procedure TsgcSyncMessages.Synchronize(const aText: String);
begin
  {$IFDEF D2010}
  TThread.Synchronize(nil,
    procedure
    begin
      if Assigned(FOnMessage) then
        FOnMessage(self, aText);
    end);
  {$ELSE}
  DoEnterCS;
  Try
    Data.Add(aText);
  Finally
    DoLeaveCS;
  End;
  TThread.Synchronize(nil, DoData);
  {$ENDIF}
end;

initialization

finalization
  sgcFree(oSyncMessages);

end.
