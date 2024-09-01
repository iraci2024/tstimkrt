{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2022                                        }
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

unit FMX.TMSFNCWhatsAppReceiver;

{$I FMX.TMSFNCDefines.inc}

interface

uses
  {$IFNDEF WEBLIB}
  FMX.TMSFNCWebSocketClient, FMX.TMSFNCWebSocketCommon,
  {$ENDIF}
  {$IFDEF WEBLIB}
  WEBLib.WebSocketClient, Web,
  {$ENDIF}
  FMX.TMSFNCTypes, Classes, Generics.Collections;

type
  TTMSFNCWhatsAppReceiverMessageType = (wamtText, wamtImage, wamtDocument, wamtContacts,
    wamtAudio, wamtLocation, wamtVideo, wamtSticker);

  TTMSFNCWhatsAppReceiverContactType = (wactHome, wactWork);
  TTMSFNCWhatsAppReceiverContactPhoneType = (waptHome, waptWork, waptCell, waptMain, waptIphone);

  TTMSFNCWhatsAppReceiverContactAddress = class(TPersistent)
  private
    FStreet: string;
    FZIP: string;
    FState: string;
    FCountryCode: string;
    FAddressType: TTMSFNCWhatsAppReceiverContactType;
    FCountry: string;
    FCity: string;
  public
    procedure Assign(Source: TPersistent); override;
    function ToJSON: string;
    property Street: string read FStreet write FStreet;
    property City: string read FCity write FCity;
    property State: string read FState write FState;
    property ZIP: string read FZIP write FZIP;
    property Country: string read FCountry write FCountry;
    property CountryCode: string read FCountryCode write FCountryCode;
    property AddressType: TTMSFNCWhatsAppReceiverContactType read FAddressType write FAddressType;
  end;

  TTMSFNCWhatsAppReceiverContactEmail = class(TPersistent)
  private
    FEmail: string;
    FEmailType: TTMSFNCWhatsAppReceiverContactType;
  public
    procedure Assign(Source: TPersistent); override;
    function ToJSON: string;
    property Email: string read FEmail write FEmail;
    property EmailType: TTMSFNCWhatsAppReceiverContactType read FEmailType write FEmailType;
  end;

  TTMSFNCWhatsAppReceiverContactPhone = class(TPersistent)
  private
    FPhoneType: TTMSFNCWhatsAppReceiverContactPhoneType;
    FPhone: string;
    FWaID: string;
  public
    procedure Assign(Source: TPersistent); override;
    function ToJSON: string;
    property Phone: string read FPhone write FPhone;
    property PhoneType: TTMSFNCWhatsAppReceiverContactPhoneType read FPhoneType write FPhoneType;
    property WhatsAppID: string read FWaID write FWaID;
  end;

  TTMSFNCWhatsAppReceiverContactURL = class(TPersistent)
  private
    FURLType: TTMSFNCWhatsAppReceiverContactType;
    FURL: string;
  public
    procedure Assign(Source: TPersistent); override;
    function ToJSON: string;
    property URL: string read FURL write FURL;
    property URLType: TTMSFNCWhatsAppReceiverContactType read FURLType write FURLType;
  end;

  TTMSFNCWhatsAppReceiverContactName = class(TPersistent)
  private
    FLastName: string;
    FFormattedName: string;
    FMiddleName: string;
    FFirstName: string;
    FPrefix: string;
    FSuffix: string;
  public
    procedure Assign(Source: TPersistent); override;
    function ToJSON: string;
    property FormattedName: string read FFormattedName write FFormattedName;
    property FirstName: string read FFirstName write FFirstName;
    property LastName: string read FLastName write FLastName;
    property MiddleName: string read FMiddleName write FMiddleName;
    property Suffix: string read FSuffix write FSuffix;
    property Prefix: string read FPrefix write FPrefix;
  end;

  TTMSFNCWhatsAppReceiverContactOrganization = class(TPersistent)
  private
    FDepartment: string;
    FCompany: string;
    FTitle: string;
  public
    procedure Assign(Source: TPersistent); override;
    function ToJSON: string;
    property Company: string read FCompany write FCompany;
    property Department: string read FDepartment write FDepartment;
    property Title: string read FTitle write FTitle;
  end;

  TTMSFNCWhatsAppReceiverContactAddresses = class(TObjectList<TTMSFNCWhatsAppReceiverContactAddress>)
  public
    function ToJSON: string;
  end;

  TTMSFNCWhatsAppReceiverContactEmails = class(TObjectList<TTMSFNCWhatsAppReceiverContactEmail>)
  public
    function ToJSON: string;
  end;

  TTMSFNCWhatsAppReceiverContactPhones = class(TObjectList<TTMSFNCWhatsAppReceiverContactPhone>)
  public
    function ToJSON: string;
  end;

  TTMSFNCWhatsAppReceiverContactURLs = class(TObjectList<TTMSFNCWhatsAppReceiverContactURL>)
  public
    function ToJSON: string;
  end;

  TTMSFNCWhatsAppReceiverContact = class(TPersistent)
  private
    FPhones: TTMSFNCWhatsAppReceiverContactPhones;
    FBirthday: TDateTime;
    FURLs: TTMSFNCWhatsAppReceiverContactURLs;
    FOrganization: TTMSFNCWhatsAppReceiverContactOrganization;
    FContactName: TTMSFNCWhatsAppReceiverContactName;
    FEmails: TTMSFNCWhatsAppReceiverContactEmails;
    FAddresses: TTMSFNCWhatsAppReceiverContactAddresses;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ToJSON: string;
    property Addresses: TTMSFNCWhatsAppReceiverContactAddresses read FAddresses write FAddresses;
    property Birthday: TDateTime read FBirthday write FBirthday;
    property Emails: TTMSFNCWhatsAppReceiverContactEmails read FEmails write FEmails;
    property ContactName: TTMSFNCWhatsAppReceiverContactName read FContactName write FContactName;
    property Organization: TTMSFNCWhatsAppReceiverContactOrganization read FOrganization write FOrganization;
    property Phones: TTMSFNCWhatsAppReceiverContactPhones read FPhones write FPhones;
    property URLs: TTMSFNCWhatsAppReceiverContactURLs read FURLs write FURLs;
  end;

  TTMSFNCWhatsAppReceiverContacts = class(TObjectList<TTMSFNCWhatsAppReceiverContact>)
  public
    function ToJSON: string;
  end;

  TTMSFNCWhatsAppReceiverText = class(TPersistent)
  private
    FBody: string;
  public
    procedure Assign(Source: TPersistent); override;
    function ToJSON: string;
    property Body: string read FBody write FBody;
  end;

  TTMSFNCWhatsAppReceiverMedia = class(TPersistent)
  private
    FMimeType: string;
    FFilename: string;
    FID: string;
    FCaption: string;
    FChecksum: string;
    FFileType: TTMSFNCWhatsAppReceiverMessageType;
  public
    procedure Assign(Source: TPersistent); override;
    function ToJSON: string;
    property FileType: TTMSFNCWhatsAppReceiverMessageType read FFileType write FFileType;
    property MimeType: string read FMimeType write FMimeType;
    property ID: string read FID write FID;
    property Caption: string read FCaption write FCaption;
    property Filename: string read FFilename write FFilename;
    property Checksum: string read FChecksum write FChecksum;
  end;

  TTMSFNCWhatsAppReceiverLocation = class(TPersistent)
  private
    FLocationName: string;
    FLatitude: double;
    FLongitude: double;
    FAddress: string;
  public
    procedure Assign(Source: TPersistent); override;
    function ToJSON: string;
    property Longitude: double read FLongitude write FLongitude;
    property Latitude: double read FLatitude write FLatitude;
    property LocationName: string read FLocationName write FLocationName;
    property Address: string read FAddress write FAddress;
  end;

  TTMSFNCWhatsAppReceiverMessageFrom = class(TPersistent)
  private
    FName: string;
    FID: string;
    FPhone: string;
  public
    procedure Assign(Source: TPersistent); override;
    property Name: string read FName write FName;
    property WhatsAppID: string read FID write FID;
    property Phone: string read FPhone write FPhone;
  end;

  TTMSFNCWhatsAppReceiverMessage = class(TPersistent)
  private
    FLocation: TTMSFNCWhatsAppReceiverLocation;
    FMedia: TTMSFNCWhatsAppReceiverMedia;
    FID: string;
    FTimeStamp: string;
    FContacts: TTMSFNCWhatsAppReceiverContacts;
    FText: TTMSFNCWhatsAppReceiverText;
    FMessageType: TTMSFNCWhatsAppReceiverMessageType;
    FIsReply: Boolean;
    FFrom: TTMSFNCWhatsAppReceiverMessageFrom;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ToJSON: string;
    property IsReply: Boolean read FIsReply write FIsReply;
    property ID: string read FID write FID;
    property From: TTMSFNCWhatsAppReceiverMessageFrom read FFrom write FFrom;
    property TimeStamp: string read FTimeStamp write FTimeStamp;
    property Contacts: TTMSFNCWhatsAppReceiverContacts read FContacts write FContacts;
    property Location: TTMSFNCWhatsAppReceiverLocation read FLocation write FLocation;
    property Media: TTMSFNCWhatsAppReceiverMedia read FMedia write FMedia;
    property Text: TTMSFNCWhatsAppReceiverText read FText write FText;
    property MessageType: TTMSFNCWhatsAppReceiverMessageType read FMessageType write FMessageType;
  end;

  TTMSFNCCustomWhatsAppMessageReceivedEvent = procedure(Sender: TObject; AMessage: TTMSFNCWhatsAppReceiverMessage) of object;

  {$IFNDEF WEBLIB}
  TTMSFNCCustomWhatsAppReceiver = class(TTMSFNCCustomWebsocketClient)
  {$ENDIF}
  {$IFDEF WEBLIB}
  TTMSFNCCustomWhatsAppReceiver = class(TSocketClientBase)
  {$ENDIF}
  private
    FOnWhatsAppMessageReceived: TTMSFNCCustomWhatsAppMessageReceivedEvent;
  protected
    procedure DoMessageReceived(AMessage: TTMSFNCWhatsAppReceiverMessage); virtual;
    {$IFNDEF WEBLIB}
    function GetInstance: NativeUInt; override;
    function GetDocURL: string; override;
    procedure MessageReceived(Sender: TObject; AConnection: TTMSFNCWebSocketConnection; const AMessage: string); override;
    {$ENDIF}
    {$IFDEF WEBLIB}
    procedure DoMessage(AEvent: TJSMessageEvent); override;
    {$ENDIF}
    property OnWhatsAppMessageReceived: TTMSFNCCustomWhatsAppMessageReceivedEvent read FOnWhatsAppMessageReceived write FOnWhatsAppMessageReceived;
  public
    constructor Create(aOwner: TComponent); override;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TTMSFNCWhatsAppReceiver = class(TTMSFNCCustomWhatsAppReceiver)
  {$IFNDEF WEBLIB}
  public
    property AutoSyncEvents;
    property ConnectTimeout;
    property OnPing;
    property OnPong;
  {$ENDIF}
  published
    property Active;
    property HostName;
    property Port;
    property PathName;
    property OnConnect;
    property OnDisconnect;
    property OnWhatsAppMessageReceived;
    property OnMessageReceived;
    property OnBinaryDataReceived;
  end;

implementation

uses
  SysUtils,
  {$IFNDEF WEBLIB}
  JSON,
  {$ENDIF}
  {$IFDEF WEBLIB}
  JS, WEBLib.JSON,
  {$ENDIF}
  FMX.TMSFNCUtils;

{$R TMSFNCWhatsAppReceiver.res}

{ TTMSFNCCustomWhatsAppReceiver }


constructor TTMSFNCCustomWhatsAppReceiver.Create(aOwner: TComponent);
begin
  inherited Create(AOwner);
  UseSSL := True;
end;

{$IFNDEF WEBLIB}
function TTMSFNCCustomWhatsAppReceiver.GetDocURL: string;
begin
  Result := 'https://download.tmssoftware.com/doc/tmsfncwebsocket/components/ttmsfncwhatsappreceiver';
end;

function TTMSFNCCustomWhatsAppReceiver.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

procedure TTMSFNCCustomWhatsAppReceiver.MessageReceived(Sender: TObject;
  AConnection: TTMSFNCWebSocketConnection; const AMessage: string);
{$ENDIF}
{$IFDEF WEBLIB}
procedure TTMSFNCCustomWhatsAppReceiver.DoMessage(AEvent: TJSMessageEvent);
{$ENDIF}
var
  msgType, d: string;
  jsVal, val, contact, msgObj, msgDet: TJSONValue;
  entr, ch, cont, msg, p: TJSONArray;
  msgRec: TTMSFNCWhatsAppReceiverMessage;
  I, J: Integer;
  fs: TFormatSettings;
begin
  inherited;
  {$IFNDEF WEBLIB}
  jsVal := TTMSFNCUtils.ParseJSON(AMessage);
  {$ENDIF}
  {$IFDEF WEBLIB}
  if not Assigned(AEvent) then
    Exit;

  msgType := JS.toString(AEvent.data);
  jsVal := TTMSFNCUtils.ParseJSON(msgType);
  {$ENDIF}
  msgRec := TTMSFNCWhatsAppReceiverMessage.Create;
  try
    if Assigned(jsVal) then
    begin
      entr := jsVal['entry'].AsArray;
      ch := entr[0]['changes'].AsArray;
      val := ch[0]['value'];
      cont := val['contacts'].AsArray;
      msg := val['messages'].AsArray;
      if not Assigned(cont) or not Assigned(msg) then
        Exit;

      contact := cont[0];
      msgRec.From.WhatsAppID := contact['wa_id'].AsString;
      msgRec.From.Name := contact['profile']['name'].AsString;

      msgObj := msg[0];
      msgRec.IsReply := Assigned(msgObj['context']);
      msgRec.TimeStamp := msgObj['timestamp'].AsString;
      msgRec.From.Phone := msgObj['from'].AsString;
      msgRec.ID := msgObj['id'].AsString;

      msgType := msgObj['type'].AsString;
      if msgType = 'text' then
      begin
        msgRec.MessageType := wamtText;
        msgDet := msgObj['text'];
        msgRec.Text.Body := msgDet['body'].AsString;
      end
      else if msgType = 'image' then
      begin
        msgRec.MessageType := wamtImage;
        msgDet := msgObj['image'];
        msgRec.Media.ID := msgDet['id'].AsString;
        msgRec.Media.MimeType := msgDet['mime_type'].AsString;
        msgRec.Media.Checksum := msgDet['sha256'].AsString;
        msgRec.Media.Caption := msgDet['caption'].AsString;
      end
      else if msgType = 'document' then
      begin
        msgRec.MessageType := wamtDocument;
        msgDet := msgObj['document'];
        msgRec.Media.ID := msgDet['id'].AsString;
        msgRec.Media.FileName := msgDet['filename'].AsString;
        msgRec.Media.MimeType := msgDet['mime_type'].AsString;
        msgRec.Media.Checksum := msgDet['sha256'].AsString;
      end
      else if msgType = 'contacts' then
      begin
        msgRec.MessageType := wamtContacts;
        ch := msgObj['contacts'].AsArray;
        for I := 0 to ch.Length - 1 do
        begin
          msgRec.Contacts.Add(TTMSFNCWhatsAppReceiverContact.Create);

          msgRec.Contacts.Items[I].ContactName.FirstName := ch[I]['name']['first_name'].AsString;
          msgRec.Contacts.Items[I].ContactName.FormattedName := ch[I]['name']['formatted_name'].AsString;
          msgRec.Contacts.Items[I].ContactName.MiddleName := ch[I]['name']['middle_name'].AsString;
          msgRec.Contacts.Items[I].ContactName.LastName := ch[I]['name']['last_name'].AsString;
          msgRec.Contacts.Items[I].ContactName.Prefix := ch[I]['name']['name-prefix'].AsString;
          msgRec.Contacts.Items[I].ContactName.Suffix := ch[I]['name']['name_suffix'].AsString;

          p := ch[I]['phones'].AsArray;
          if Assigned(p) then
          begin
            for J := 0 to p.Length - 1 do
            begin
              msgRec.Contacts.Items[I].Phones.Add(TTMSFNCWhatsAppReceiverContactPhone.Create);
              msgRec.Contacts.Items[I].Phones[J].Phone := p[J]['phone'].AsString;
              msgRec.Contacts.Items[I].Phones[J].WhatsAppID := p[J]['wa_id'].AsString;
              d := p[J]['type'].AsString;
              if d = 'HOME' then
                msgRec.Contacts.Items[I].Phones[J].PhoneType := waptHome
              else if d = 'WORK' then
                msgRec.Contacts.Items[I].Phones[J].PhoneType := waptWork
              else if d = 'CELL' then
                msgRec.Contacts.Items[I].Phones[J].PhoneType := waptCell
              else if d = 'MAIN' then
                msgRec.Contacts.Items[I].Phones[J].PhoneType := waptMain
              else if d = 'IPHONE' then
                msgRec.Contacts.Items[I].Phones[J].PhoneType := waptIphone;
            end;
          end;

          d := ch[I]['birthday'].AsString;
          if d <> '' then
          begin
            fs := TFormatSettings.Create;
            fs.DateSeparator := '-';
            fs.ShortDateFormat := 'yyyy-MM-dd';
            msgRec.Contacts[I].Birthday := StrToDate(d, fs);
          end;

          p := ch[I]['addresses'].AsArray;
          if Assigned(p) then
          begin
            for J := 0 to p.Length - 1 do
            begin
              msgRec.Contacts.Items[I].Addresses.Add(TTMSFNCWhatsAppReceiverContactAddress.Create);
              msgRec.Contacts.Items[I].Addresses[J].Street := p[J]['street'].AsString;
              msgRec.Contacts.Items[I].Addresses[J].City := p[J]['city'].AsString;
              msgRec.Contacts.Items[I].Addresses[J].State := p[J]['state'].AsString;
              msgRec.Contacts.Items[I].Addresses[J].ZIP := p[J]['zip'].AsString;
              msgRec.Contacts.Items[I].Addresses[J].Country := p[J]['country'].AsString;
              msgRec.Contacts.Items[I].Addresses[J].CountryCode := p[J]['country_code'].AsString;
              if p[J]['type'].AsString = 'HOME' then
                msgRec.Contacts.Items[I].Addresses[J].AddressType := wactHome
              else
                msgRec.Contacts.Items[I].Addresses[J].AddressType := wactWork;
            end;
          end;

          p := ch[I]['emails'].AsArray;
          if Assigned(p) then
          begin
            for J := 0 to p.Length - 1 do
            begin
              msgRec.Contacts.Items[I].Emails.Add(TTMSFNCWhatsAppReceiverContactEmail.Create);
              msgRec.Contacts.Items[I].Emails[J].Email := p[J]['email'].AsString;
              if p[J]['type'].AsString = 'HOME' then
                msgRec.Contacts.Items[I].Emails[J].EmailType := wactHome
              else
                msgRec.Contacts.Items[I].Emails[J].EmailType := wactWork;
            end;
          end;

          p := ch[I]['urls'].AsArray;
          if Assigned(p) then
          begin
            for J := 0 to p.Length - 1 do
            begin
              msgRec.Contacts.Items[I].URLs.Add(TTMSFNCWhatsAppReceiverContactURL.Create);
              msgRec.Contacts.Items[I].URLs[J].URL := p[J]['url'].AsString;
              if p[J]['type'].AsString = 'HOME' then
                msgRec.Contacts.Items[I].URLs[J].URLType := wactHome
              else
                msgRec.Contacts.Items[I].URLs[J].URLType := wactWork;
            end;
          end;

          msgDet := ch[I]['org'];
          if Assigned(msgDet) then
          begin
            msgRec.Contacts.Items[I].Organization.Department := msgDet['department'].AsString;
            msgRec.Contacts.Items[I].Organization.Company := msgDet['company'].AsString;
            msgRec.Contacts.Items[I].Organization.Title := msgDet['title'].AsString;
          end;
        end;
      end
      else if msgType = 'audio' then
      begin
        msgRec.MessageType := wamtAudio;
        msgDet := msgObj['audio'];
        msgRec.Media.ID := msgDet['id'].AsString;
        msgRec.Media.MimeType := msgDet['mime_type'].AsString;
        msgRec.Media.Checksum := msgDet['sha256'].AsString;
      end
      else if msgType = 'location' then
      begin
        msgRec.MessageType := wamtLocation;
        msgDet := msgObj['location'];
        msgRec.Location.Longitude := msgDet['longitude'].AsDouble;
        msgRec.Location.Latitude := msgDet['latitude'].AsDouble;
        msgRec.Location.LocationName := msgDet['name'].AsString;
        msgRec.Location.Address := msgDet['address'].AsString;
      end
      else if msgType = 'video' then
      begin
        msgRec.MessageType := wamtVideo;
        msgDet := msgObj['video'];
        msgRec.Media.ID := msgDet['id'].AsString;
        msgRec.Media.MimeType := msgDet['mime_type'].AsString;
        msgRec.Media.Checksum := msgDet['sha256'].AsString;
        msgRec.Media.Caption := msgDet['caption'].AsString;
      end
      else if msgType = 'sticker' then
      begin
        msgRec.MessageType := wamtSticker;
        msgDet := msgObj['sticker'];
        msgRec.Media.ID := msgDet['id'].AsString;
        msgRec.Media.MimeType := msgDet['mime_type'].AsString;
        msgRec.Media.Checksum := msgDet['sha256'].AsString;
      end;

      DoMessageReceived(msgRec);
    end;
  finally
    jsVal.Free;
    msgRec.Free;
  end;
end;

procedure TTMSFNCCustomWhatsAppReceiver.DoMessageReceived(
  AMessage: TTMSFNCWhatsAppReceiverMessage);
begin
  if Assigned(OnWhatsAppMessageReceived) then
    OnWhatsAppMessageReceived(Self, AMessage);
end;

{ TTMSFNCWhatsAppReceiverMessage }

procedure TTMSFNCWhatsAppReceiverMessage.Assign(Source: TPersistent);
var
  I: Integer;
  c: TTMSFNCWhatsAppReceiverContact;
begin
  if Source is TTMSFNCWhatsAppReceiverMessage then
  begin
    FFrom.Assign((Source as TTMSFNCWhatsAppReceiverMessage).From);
    FText.Assign((Source as TTMSFNCWhatsAppReceiverMessage).Text);
    FMessageType := (Source as TTMSFNCWhatsAppReceiverMessage).MessageType;
    FMedia.Assign((Source as TTMSFNCWhatsAppReceiverMessage).Media);
    FLocation.Assign((Source as TTMSFNCWhatsAppReceiverMessage).Location);
    FContacts.Clear;
    for I := 0 to (Source as TTMSFNCWhatsAppReceiverMessage).Contacts.Count - 1 do
    begin
      c := TTMSFNCWhatsAppReceiverContact.Create;
      c.Assign((Source as TTMSFNCWhatsAppReceiverMessage).Contacts[I]);
      FContacts.Add(c);
    end;

    FID := (Source as TTMSFNCWhatsAppReceiverMessage).ID;
    FTimeStamp := (Source as TTMSFNCWhatsAppReceiverMessage).TimeStamp	;
    FIsReply:= (Source as TTMSFNCWhatsAppReceiverMessage).IsReply	;
  end
  else
    inherited;
end;

constructor TTMSFNCWhatsAppReceiverMessage.Create;
begin
  FLocation := TTMSFNCWhatsAppReceiverLocation.Create;
  FText := TTMSFNCWhatsAppReceiverText.Create;
  FMedia := TTMSFNCWhatsAppReceiverMedia.Create;
  FContacts := TTMSFNCWhatsAppReceiverContacts.Create;
  FFrom := TTMSFNCWhatsAppReceiverMessageFrom.Create;
end;

destructor TTMSFNCWhatsAppReceiverMessage.Destroy;
begin
  FLocation.Free;
  FText.Free;
  FMedia.Free;
  FContacts.Free;
  FFrom.Free;
  inherited;
end;

function TTMSFNCWhatsAppReceiverMessage.ToJSON: string;
begin
  Result := '{';
  Result := Result + '"messaging_product": "whatsapp",';

  case MessageType of
    wamtText:
    begin
      Result := Result + '"type": "text",';
      Result := Result + '"text": ' + Text.ToJSON;
    end;
    wamtAudio:
    begin
      Result := Result + '"type": "audio",';
      Result := Result + '"audio": ' + Media.ToJSON;
    end;
    wamtContacts:
    begin
      Result := Result + '"type": "contacts",';
      Result := Result + '"contacts": ' + Contacts.ToJSON;
    end;
    wamtDocument:
    begin
      Result := Result + '"type": "document",';
      Result := Result + '"document": ' + Media.ToJSON;
    end;
    wamtImage:
    begin
      Result := Result + '"type": "image",';
      Result := Result + '"image": ' + Media.ToJSON;
    end;
    wamtVideo:
    begin
      Result := Result + '"type": "video",';
      Result := Result + '"video": ' + Media.ToJSON;
    end;
    wamtLocation:
    begin
      Result := Result + '"type": "location",';
      Result := Result + '"location": ' + Location.ToJSON;
    end;
    wamtSticker:
    begin
      Result := Result + '"type": "sticker",';
      Result := Result + '"sticker": ' + Media.ToJSON;
    end;
  end;

  Result := Result + '}';
end;

{ TTMSFNCWhatsAppReceiverLocation }

procedure TTMSFNCWhatsAppReceiverLocation.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCWhatsAppReceiverLocation then
  begin
    FLocationName := (Source as TTMSFNCWhatsAppReceiverLocation).LocationName;
    FLatitude := (Source as TTMSFNCWhatsAppReceiverLocation).Latitude;
    FLongitude := (Source as TTMSFNCWhatsAppReceiverLocation).Longitude;
    FAddress := (Source as TTMSFNCWhatsAppReceiverLocation).Address;
  end
  else
    inherited;
end;

function TTMSFNCWhatsAppReceiverLocation.ToJSON: string;
begin
  Result := '{';
  Result := Result + '"longitude": ' + TTMSFNCUtils.FloatToStrDot(Longitude) + ',';
  Result := Result + '"latitude": ' + TTMSFNCUtils.FloatToStrDot(Latitude);

  if LocationName <> '' then
  begin
    Result := Result + ',"name": "' + LocationName + '",';
    Result := Result + '"address": "' + Address + '"';
  end;
  Result := Result + '}';
end;

{ TTMSFNCWhatsAppReceiverMedia }

procedure TTMSFNCWhatsAppReceiverMedia.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCWhatsAppReceiverMedia then
  begin
    FCaption := (Source As TTMSFNCWhatsAppReceiverMedia).Caption;
    FFileName := (Source As TTMSFNCWhatsAppReceiverMedia).FileName;
    FFileType := (Source As TTMSFNCWhatsAppReceiverMedia).FileType;
    FID := (Source As TTMSFNCWhatsAppReceiverMedia).ID;
  end
  else
    inherited;
end;

function TTMSFNCWhatsAppReceiverMedia.ToJSON: string;
begin
  Result := '{';
  Result := Result + '"id": "' + ID + '",';

  if FileType = wamtDocument then
    Result := Result + '"filename": "' + FileName +'",';

  if not (FileType in [wamtAudio, wamtSticker]) then
    Result := Result + '"caption": "' + Caption + '"';

  Result := Result + '}';
end;

{ TTMSFNCWhatsAppReceiverText }

procedure TTMSFNCWhatsAppReceiverText.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCWhatsAppReceiverText then
  begin
    FBody := (Source as TTMSFNCWhatsAppReceiverText).Body;
  end
  else
    inherited;
end;

function TTMSFNCWhatsAppReceiverText.ToJSON: String;
begin
  Result := '{';
  Result := Result + '"body": "' + TTMSFNCUtils.EscapeString(Body) + '",';
  Result := Result + '}';
end;

{ TTMSFNCWhatsAppReceiverContactAddress }

procedure TTMSFNCWhatsAppReceiverContactAddress.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCWhatsAppReceiverContactAddress then
  begin
    FStreet := (Source as TTMSFNCWhatsAppReceiverContactAddress).Street;
    FZIP := (Source as TTMSFNCWhatsAppReceiverContactAddress).ZIP;
    FState := (Source as TTMSFNCWhatsAppReceiverContactAddress).State;
    FCountryCode := (Source as TTMSFNCWhatsAppReceiverContactAddress).CountryCode;
    FAddressType := (Source as TTMSFNCWhatsAppReceiverContactAddress).AddressType;
    FCountry := (Source as TTMSFNCWhatsAppReceiverContactAddress).Country;
    FCity := (Source as TTMSFNCWhatsAppReceiverContactAddress).City;
  end
  else
    inherited;
end;

function TTMSFNCWhatsAppReceiverContactAddress.ToJSON: string;
begin
  Result := '{';
  Result := Result + '"street": "' + Street +'",';
  Result := Result + '"city": "' + City + '",';
  Result := Result + '"zip": "' + ZIP + '",';
  Result := Result + '"state": "' + State + '",';
  Result := Result + '"country": "' + Country + '",';
  Result := Result + '"country_code": "' + CountryCode +'",';

  case AddressType of
    wactHome: Result := Result + '"type": "HOME"';
    wactWork: Result := Result + '"type": "WORK"';
  end;

  Result := Result + '}';
end;

{ TTMSFNCWhatsAppReceiverContactEmail }

procedure TTMSFNCWhatsAppReceiverContactEmail.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCWhatsAppReceiverContactEmail then
  begin
    FEmail := (Source as TTMSFNCWhatsAppReceiverContactEmail).Email;
    FEmailType := (Source as TTMSFNCWhatsAppReceiverContactEmail).EmailType;
  end
  else
    inherited;

end;

function TTMSFNCWhatsAppReceiverContactEmail.ToJSON: string;
begin
  Result := '{';
  Result := Result + '"email": "' + Email +'",';

  case EmailType of
    wactHome: Result := Result + '"type": "HOME"';
    wactWork: Result := Result + '"type": "WORK"';
  end;

  Result := Result + '}';
end;

{ TTMSFNCWhatsAppReceiverContactPhone }

procedure TTMSFNCWhatsAppReceiverContactPhone.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCWhatsAppReceiverContactPhone then
  begin
    FPhone := (Source as TTMSFNCWhatsAppReceiverContactPhone).Phone;
    FPhoneType := (Source as TTMSFNCWhatsAppReceiverContactPhone).PhoneType;
    FWaID := (Source as TTMSFNCWhatsAppReceiverContactPhone).WhatsAppID;
  end
  else
    inherited;

end;

function TTMSFNCWhatsAppReceiverContactPhone.ToJSON: string;
begin
  Result := '{';
  Result := Result + '"phone": "' + Phone +'",';

  case PhoneType of
    waptHome: Result := Result + '"type": "HOME"';
    waptWork: Result := Result + '"type": "WORK"';
    waptCell: Result := Result + '"type": "CELL"';
    waptMain: Result := Result + '"type": "MAIN"';
    waptIphone: Result := Result + '"type": "IPHONE"';
  end;

  Result := Result + '}';
end;

{ TTMSFNCWhatsAppReceiverContactURL }

procedure TTMSFNCWhatsAppReceiverContactURL.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCWhatsAppReceiverContactURL then
  begin
    FURL := (Source as TTMSFNCWhatsAppReceiverContactURL).URL;
    FURLType := (Source as TTMSFNCWhatsAppReceiverContactURL).URLType;
  end
  else
    inherited;
end;

function TTMSFNCWhatsAppReceiverContactURL.ToJSON: string;
begin
  Result := '{';
  Result := Result + '"url": "' + URL +'",';

  case URLType of
    wactHome: Result := Result + '"type": "HOME"';
    wactWork: Result := Result + '"type": "WORK"';
  end;

  Result := Result + '}';
end;

{ TTMSFNCWhatsAppReceiverContactName }

procedure TTMSFNCWhatsAppReceiverContactName.Assign(Source: TPersistent);
begin
 if Source is TTMSFNCWhatsAppReceiverContactName then
  begin
    FLastName := (Source as TTMSFNCWhatsAppReceiverContactName).LastName;
    FFormattedName := (Source as TTMSFNCWhatsAppReceiverContactName).FormattedName;
    FMiddleName := (Source as TTMSFNCWhatsAppReceiverContactName).MiddleName;
    FFirstName := (Source as TTMSFNCWhatsAppReceiverContactName).FirstName;
    FPrefix := (Source as TTMSFNCWhatsAppReceiverContactName).Prefix;
    FSuffix := (Source as TTMSFNCWhatsAppReceiverContactName).Suffix;
  end
  else
    inherited;

end;

function TTMSFNCWhatsAppReceiverContactName.ToJSON: string;
begin
  Result := '{';
  Result := Result + '"formatted_name": "' + FirstName + ' ' + LastName + '",';
  Result := Result + '"first_name": "' + FirstName + '",';
  Result := Result + '"last_name": "' + LastName + '",';
  Result := Result + '"middle_name": "' + LastName + '",';
  Result := Result + '"suffix": "' + Suffix + '",';
  Result := Result + '"prefix": "' + Prefix + '"';
  Result := Result + '}';
end;

{ TTMSFNCWhatsAppReceiverContactOrganization }

procedure TTMSFNCWhatsAppReceiverContactOrganization.Assign(
  Source: TPersistent);
begin
  if Source is TTMSFNCWhatsAppReceiverContactOrganization then
  begin
    FDepartment := (Source as TTMSFNCWhatsAppReceiverContactOrganization).Department;
    FCompany := (Source as TTMSFNCWhatsAppReceiverContactOrganization).Company;
    FTitle := (Source as TTMSFNCWhatsAppReceiverContactOrganization).Title;
  end
  else
    inherited;
end;

function TTMSFNCWhatsAppReceiverContactOrganization.ToJSON: string;
begin
  Result := '{';
  Result := Result + '"company": "' + Company + '",';
  Result := Result + '"department": "' + Department + '",';
  Result := Result + '"title": "' + Title + '"';
  Result := Result + '}';
end;

{ TTMSFNCWhatsAppReceiverContactAddresses }

function TTMSFNCWhatsAppReceiverContactAddresses.ToJSON: string;
var
  I: Integer;
begin
  Result := '[';
  for I := 0 to Count - 1 do
    Result := Result + Items[I].ToJSON + ',';
  if Count <> 0 then
    SetLength(Result, Length(Result) - 1);

  Result := Result + ']';
end;

{ TTMSFNCWhatsAppReceiverContactEmails }

function TTMSFNCWhatsAppReceiverContactEmails.ToJSON: string;
var
  I: Integer;
begin
  Result := '[';
  for I := 0 to Count - 1 do
    Result := Result + Items[I].ToJSON + ',';
  if Count <> 0 then
    SetLength(Result, Length(Result) - 1);

  Result := Result + ']';
end;

{ TTMSFNCWhatsAppReceiverContactPhones }

function TTMSFNCWhatsAppReceiverContactPhones.ToJSON: string;
var
  I: Integer;
begin
  Result := '[';
  for I := 0 to Count - 1 do
    Result := Result + Items[I].ToJSON + ',';
  if Count <> 0 then
    SetLength(Result, Length(Result) - 1);

  Result := Result + ']';
end;

{ TTMSFNCWhatsAppReceiverContactURLs }

function TTMSFNCWhatsAppReceiverContactURLs.ToJSON: string;
var
  I: Integer;
begin
  Result := '[';
  for I := 0 to Count - 1 do
    Result := Result + Items[I].ToJSON + ',';
  if Count <> 0 then
    SetLength(Result, Length(Result) - 1);

  Result := Result + ']';
end;

{ TTMSFNCWhatsAppReceiverContact }

procedure TTMSFNCWhatsAppReceiverContact.Assign(Source: TPersistent);
var
  p: TTMSFNCWhatsAppReceiverContactPhone;
  f: TTMSFNCWhatsAppReceiverContactURL;
  e: TTMSFNCWhatsAppReceiverContactEmail;
  a: TTMSFNCWhatsAppReceiverContactAddress;
  I: Integer;
begin
  if Source is TTMSFNCWhatsAppReceiverContact then
  begin
    FPhones.Clear;
    for I := 0 to (Source as TTMSFNCWhatsAppReceiverContact).Phones.Count - 1 do
    begin
      p := TTMSFNCWhatsAppReceiverContactPhone.Create;
      p.Assign((Source as TTMSFNCWhatsAppReceiverContact).Phones[I]);
      FPhones.Add(p);
    end;

    FBirthday := (Source as TTMSFNCWhatsAppReceiverContact).Birthday;
    FURLs.Clear;
    for I := 0 to (Source as TTMSFNCWhatsAppReceiverContact).URLS.Count - 1 do
    begin
      f := TTMSFNCWhatsAppReceiverContactURL.Create;
      F.Assign((Source as TTMSFNCWhatsAppReceiverContact).URLS[I]);
      FURLS.Add(F);
    end;
    FOrganization.Assign((Source as TTMSFNCWhatsAppReceiverContact).Organization);
    FContactName.Assign((Source as TTMSFNCWhatsAppReceiverContact).ContactName);
    FEmails.Clear;
    for I := 0 to (Source as TTMSFNCWhatsAppReceiverContact).Emails.Count - 1 do
    begin
      e := TTMSFNCWhatsAppReceiverContactEmail.Create;
      e.Assign((Source as TTMSFNCWhatsAppReceiverContact).Emails[I]);
      FEmails.Add(e);
    end;
    FAddresses.Clear;
    for I := 0 to (Source as TTMSFNCWhatsAppReceiverContact).Addresses.Count - 1 do
    begin
      a := TTMSFNCWhatsAppReceiverContactAddress.Create;
      a.Assign((Source as TTMSFNCWhatsAppReceiverContact).Addresses[I]);
      FAddresses.Add(a);
    end;
  end
  else
    inherited;
end;

constructor TTMSFNCWhatsAppReceiverContact.Create;
begin
  FAddresses := TTMSFNCWhatsAppReceiverContactAddresses.Create;
  FURLs := TTMSFNCWhatsAppReceiverContactURLs.Create;
  FEmails := TTMSFNCWhatsAppReceiverContactEmails.Create;
  FPhones := TTMSFNCWhatsAppReceiverContactPhones.Create;
  FContactName := TTMSFNCWhatsAppReceiverContactName.Create;
  FOrganization := TTMSFNCWhatsAppReceiverContactOrganization.Create;
end;

destructor TTMSFNCWhatsAppReceiverContact.Destroy;
begin
  FAddresses.Free;
  FURLs.Free;
  FEmails.Free;
  FPhones.Free;
  FContactName.Free;
  FOrganization.Free;
  inherited;
end;

function TTMSFNCWhatsAppReceiverContact.ToJSON: string;
var
  f: TFormatSettings;
begin
  Result := '{';
  Result := Result + '"addresses": ' + Addresses.ToJSON + ',';
  f := TFormatSettings.Create;
  f.DateSeparator := '-';
  f.ShortDateFormat := 'yyyy-MM-dd';

  if Birthday <> 0 then
    Result := Result + '"birthday": "' + DateTimeToStr(Birthday, f) + '",';

  Result := Result + '"emails": ' + Emails.ToJSON + ',';
  Result := Result + '"name": ' + ContactName.ToJSON + ',';
  Result := Result + '"org": ' + Organization.ToJSON + ',';
  Result := Result + '"phones": ' + Phones.ToJSON + ',';
  Result := Result + '"urls": ' + URLs.ToJSON + ',';
  Result := Result + '}';
end;

{ TTMSFNCWhatsAppReceiverContacts }

function TTMSFNCWhatsAppReceiverContacts.ToJSON: String;
var
  I: Integer;
begin
  Result := '[';
  for I := 0 to Count - 1 do
    Result := Result + Items[I].ToJSON + ',';
  if Count <> 0 then
    SetLength(Result, Length(Result) - 1);

  Result := Result + ']';
end;

{ TTMSFNCWhatsAppReceiverMessageFrom }

procedure TTMSFNCWhatsAppReceiverMessageFrom.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCWhatsAppReceiverMessageFrom then
  begin
    FName := (Source as TTMSFNCWhatsAppReceiverMessageFrom).Name;
    FID := (Source as TTMSFNCWhatsAppReceiverMessageFrom).WhatsAppID;
    FPhone := (Source as TTMSFNCWhatsAppReceiverMessageFrom).Phone;
  end
  else
    inherited;

end;

end.
