{ ***************************************************************************
  sgcHTTP Google component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_Google_PubSub;

interface

{$I sgcVer.inc}
{$IFDEF SGC_GOOGLE_CLOUD}

uses
  Classes, SysUtils,
  // sgc
  sgcHTTP_Google_Cloud;

type
  TsgcHTTP_Google_Cloud_PubSub_Client = class(TsgcHTTP_Google_Cloud_Client)
    { from TsgcHTTP_Google_Cloud_Client }
  protected
    function DoGetScope: string; override;
    { from TsgcHTTP_Google_Cloud_Client }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    { constructor / destructor }

    { iam policy }
  public
    function GetIamPolicy(const aResource: string): string;
    function SetIamPolicy(const aResource: string): string;
    function TestIamPermissions(const aResource: string;
      aPermissions: TStrings): string;
    { iam policy }

    { projects.snapshots }
  public
    function CreateSnapshot(const aProject, aSnapshot, aSubscription: string;
      const aLabels: TStrings = nil): string;
    function DeleteSnapshot(const aProject, aSnapshot: string): string;
    function ListSnapshots(const aProject: string; aPageSize: Integer = 0;
      aPageToken: String = ''): string;
    { projects.snapshots }

    { projects.subscriptions }
  public
    function Acknowledge(const aProject, aSubscription, aAckId: string): string;
    function CreateSubscription(const aProject, aSubscription, aTopic: string;
      const aPushEndPoint: String = ''; aAckDeadlineSeconds: Integer = 0;
      aRetainAckedMessages: Boolean = False;
      aMessageRetentionDuration: String = '604800s';
      const aLabels: TStrings = nil;
      aExpirationPolicy: String = '2678400s'): string;
    function DeleteSubscripton(const aProject, aSubscription: string): string;
    function GetSubscription(const aProject, aSubscription: string): string;
    function ListSubscriptions(const aProject: string; aPageSize: Integer = 0;
      aPageToken: String = ''): string;
    function ModifyAckDeadlineSubscription(const aProject,
      aSubscription: string; aAckIds: TStrings;
      aAckDeadlineSeconds: Integer = 0): string;
    function ModifyPushConfigSubscription(const aProject, aSubscription: string;
      const aPushEndPoint: string = ''; const aAttributes: TStrings = nil;
      const aServiceAccountEmail: String = ''; aAudience: String = ''): string;
    function Pull(const aProject, aSubscription: string;
      aMaxMessages: Integer = MaxInt): string;
    function Seek(const aProject, aSubscription: string; aTimeUTC: String;
      const aSnapshot: String): string;
    { projects.subscriptions }

    { projects.topics }
  public
    function CreateTopic(const aProject, aTopic: string): string;
    function DeleteTopic(const aProject, aTopic: string): string;
    function GetTopic(const aProject, aTopic: string): string;
    function ListTopics(const aProject: string; aPageSize: Integer = 0;
      aPageToken: String = ''): string;
    function Publish(const aProject, aTopic, aMessage: string;
      const aAttributes: TStrings = nil;
      const aOrderingKey: string = ''): string;
    { projects.topics }

    { projects.topics.subscriptions }
  public
    function ListTopicSubscriptions(const aProject, aTopic: string;
      aPageSize: Integer = 0; aPageToken: String = ''): string;
    { projects.topics.subscriptions }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_GOOGLE_CLOUD}

uses
  sgcBase_Helpers;

const
  CS_GOOGLE_CLOUD_ENPOINT_PUBSUB = 'https://pubsub.googleapis.com/v1';

const
  CS_GOOGLE_CLOUD_SCOPE_PUBSUB = 'https://www.googleapis.com/auth/pubsub';

constructor TsgcHTTP_Google_Cloud_PubSub_Client.Create(aOwner: TComponent);
begin
  inherited;
  GoogleCloudOptions.JWT.API_Endpoint := 'https://pubsub.googleapis.com/';
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.Acknowledge(const aProject,
  aSubscription, aAckId: string): string;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST,
    Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB +
    '/projects/%s/subscriptions/%s:acknowledge', [aProject, aSubscription]),
    Format('{"ackIds": ["%s"]}', [aAckId]));
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.CreateSnapshot(const aProject,
  aSnapshot, aSubscription: string; const aLabels: TStrings = nil): string;
var
  i: Integer;
  vBody: string;
begin
  vBody := '{"subscription": "' + aSubscription + '"';
  vBody := vBody + ', "labels": {';
  if Assigned(aLabels) then
  begin
    for i := 0 to aLabels.Count - 1 do
    begin
      if i > 0 then
        vBody := vBody + ',';
      vBody := '"' + aLabels.Names[i] + '": "' + aLabels.ValueFromIndex
        [i] + '"';
    end;
  end;
  vBody := vBody + '}}';

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_PUT,
    Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB + '/projects/%s/snapshots/%s',
    [aProject, aSnapshot]), vBody);
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.CreateSubscription(const aProject,
  aSubscription, aTopic: string; const aPushEndPoint: String = '';
  aAckDeadlineSeconds: Integer = 0; aRetainAckedMessages: Boolean = False;
  aMessageRetentionDuration: String = '604800s'; const aLabels: TStrings = nil;
  aExpirationPolicy: String = '2678400s'): string;
var
  i: Integer;
  vBody: String;
begin
  vBody := '{"topic": "projects/' + aProject + '/topics/' + aTopic + '"';
  if aPushEndPoint <> '' then
    vBody := vBody + ', "pushConfig": {"pushEndpoint": "' +
      aPushEndPoint + '"}';
  vBody := vBody + ', "ackDeadlineSeconds": ' + IntToStr(aAckDeadlineSeconds);
  vBody := vBody + ', "retainAckedMessages": ';
  if aRetainAckedMessages then
    vBody := vBody + 'true'
  else
    vBody := vBody + 'false';
  vBody := vBody + ', "messageRetentionDuration": "' +
    aMessageRetentionDuration + '"';
  if Assigned(aLabels) then
  begin
    if aLabels.Count > 0 then
      vBody := vBody + ', "labels": {';
    for i := 0 to aLabels.Count - 1 do
    begin
      if i > 0 then
        vBody := vBody + ',';
      vBody := vBody + '"' + aLabels.Names[i] + '": "' +
        aLabels.ValueFromIndex[i] + '"';
    end;
  end;
  vBody := vBody + ', "expirationPolicy": {"ttl": "' + aExpirationPolicy + '"}';
  vBody := vBody + '}';

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_PUT,
    Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB + '/projects/%s/subscriptions/%s',
    [aProject, aSubscription]), vBody);
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.CreateTopic(const aProject,
  aTopic: string): string;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_PUT,
    Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB + '/projects/%s/topics/%s',
    [aProject, aTopic]));
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.DeleteSnapshot(const aProject,
  aSnapshot: string): string;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_DELETE,
    Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB + '/projects/%s/snapshots/%s',
    [aProject, aSnapshot]));
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.DeleteSubscripton(const aProject,
  aSubscription: string): string;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_DELETE,
    Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB + '/projects/%s/subscriptions/%s',
    [aProject, aSubscription]));
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.DeleteTopic(const aProject,
  aTopic: string): string;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_DELETE,
    Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB + '/projects/%s/topics/%s',
    [aProject, aTopic]));
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.DoGetScope: string;
begin
  inherited;
  result := CS_GOOGLE_CLOUD_SCOPE_PUBSUB;
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.GetIamPolicy(const aResource
  : string): string;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_GET,
    Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB + '%s:getIamPolicy', [aResource]));
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.GetSubscription(const aProject,
  aSubscription: string): string;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_GET,
    Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB + '/projects/%s/subscriptions/%s',
    [aProject, aSubscription]));
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.GetTopic(const aProject,
  aTopic: string): string;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_GET,
    Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB + '/projects/%s/topics/%s',
    [aProject, aTopic]));
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.ListSnapshots(const aProject
  : string; aPageSize: Integer = 0; aPageToken: String = ''): string;
var
  i: Integer;
  vURL: String;
  oParams: TStringList;
begin
  oParams := TStringList.Create;
  Try
    if aPageSize > 0 then
      oParams.Add('pageSize=' + IntToStr(aPageSize));
    if aPageToken <> '' then
      oParams.Add('pageToken=' + aPageToken);

    vURL := Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB + '/projects/%s/snapshots',
      [aProject]);
    for i := 0 to oParams.Count - 1 do
    begin
      if i = 0 then
        vURL := vURL + '?'
      else
        vURL := vURL + '&';
      vURL := vURL + oParams[i];
    end;
  Finally
    sgcFree(oParams);
  End;

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_GET, vURL);
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.ListSubscriptions
  (const aProject: string; aPageSize: Integer = 0;
  aPageToken: String = ''): string;
var
  i: Integer;
  vURL: String;
  oParams: TStringList;
begin
  oParams := TStringList.Create;
  Try
    if aPageSize > 0 then
      oParams.Add('pageSize=' + IntToStr(aPageSize));
    if aPageToken <> '' then
      oParams.Add('pageToken=' + aPageToken);

    vURL := Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB +
      '/projects/%s/subscriptions', [aProject]);
    for i := 0 to oParams.Count - 1 do
    begin
      if i = 0 then
        vURL := vURL + '?'
      else
        vURL := vURL + '&';
      vURL := vURL + oParams[i];
    end;
  Finally
    sgcFree(oParams);
  End;

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_GET, vURL);
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.ListTopicSubscriptions
  (const aProject, aTopic: string; aPageSize: Integer = 0;
  aPageToken: String = ''): string;
var
  i: Integer;
  vURL: String;
  oParams: TStringList;
begin
  oParams := TStringList.Create;
  Try
    if aPageSize > 0 then
      oParams.Add('pageSize=' + IntToStr(aPageSize));
    if aPageToken <> '' then
      oParams.Add('pageToken=' + aPageToken);

    vURL := Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB +
      '/projects/%s/topics/%s/subscriptions', [aProject, aTopic]);
    for i := 0 to oParams.Count - 1 do
    begin
      if i = 0 then
        vURL := vURL + '?'
      else
        vURL := vURL + '&';
      vURL := vURL + oParams[i];
    end;
  Finally
    sgcFree(oParams);
  End;

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_GET, vURL);
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.ListTopics(const aProject: string;
  aPageSize: Integer = 0; aPageToken: String = ''): string;
var
  i: Integer;
  vURL: String;
  oParams: TStringList;
begin
  oParams := TStringList.Create;
  Try
    if aPageSize > 0 then
      oParams.Add('pageSize=' + IntToStr(aPageSize));
    if aPageToken <> '' then
      oParams.Add('pageToken=' + aPageToken);

    vURL := Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB + '/projects/%s/topics',
      [aProject]);
    for i := 0 to oParams.Count - 1 do
    begin
      if i = 0 then
        vURL := vURL + '?'
      else
        vURL := vURL + '&';
      vURL := vURL + oParams[i];
    end;
  Finally
    sgcFree(oParams);
  End;

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_GET, vURL);
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.ModifyAckDeadlineSubscription
  (const aProject, aSubscription: string; aAckIds: TStrings;
  aAckDeadlineSeconds: Integer = 0): string;
var
  i: Integer;
  vBody: String;
begin
  vBody := '{"ackIds": [';
  for i := 0 to aAckIds.Count - 1 do
  begin
    if i > 0 then
      vBody := vBody + ',';
    vBody := vBody + '"' + aAckIds[i] + '"';
  end;
  vBody := vBody + '], "ackDeadlineSeconds": ' + IntToStr(aAckDeadlineSeconds);
  vBody := vBody + '}';

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST,
    Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB +
    '/projects/%s/subscriptions/%s:modifyAckDeadline',
    [aProject, aSubscription]), vBody);
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.ModifyPushConfigSubscription
  (const aProject, aSubscription: string; const aPushEndPoint: string = '';
  const aAttributes: TStrings = nil; const aServiceAccountEmail: String = '';
  aAudience: String = ''): string;
var
  i: Integer;
  vBody: String;
begin
  vBody := '{"pushConfig": {"pushEndpoint": "' + aPushEndPoint + '"';
  vBody := vBody + ', "attributes": {';
  if Assigned(aAttributes) then
  begin
    for i := 0 to aAttributes.Count - 1 do
    begin
      if i > 0 then
        vBody := vBody + ',';
      vBody := vBody + '"' + aAttributes.Names[i] + '": "' +
        aAttributes.ValueFromIndex[i] + '"';
    end;
  end;
  vBody := vBody + '}, "oidcToken": {"serviceAccountEmail": "' +
    aServiceAccountEmail + '"';
  vBody := vBody + ', "audience": "' + aAudience + '"}';
  vBody := vBody + '}}';

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST,
    Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB +
    '/projects/%s/subscriptions/%s:modifyPushConfig',
    [aProject, aSubscription]), vBody);
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.Publish(const aProject, aTopic,
  aMessage: string; const aAttributes: TStrings = nil;
  const aOrderingKey: string = ''): string;
var
  i: Integer;
  vBody: string;
begin
  vBody := '{"messages":[{"data": "' + EncodeBase64(aMessage, True) + '"';
  if Assigned(aAttributes) then
  begin
    // attributes
    if aAttributes.Count > 0 then
    begin
      vBody := vBody + ', "attributes": {';
      for i := 0 to aAttributes.Count - 1 do
      begin
        if i > 0 then
          vBody := vBody + ',';
        vBody := vBody + sgcQuotedStr(aAttributes.Names[i]) + ': ' +
          sgcQuotedStr(aAttributes.ValueFromIndex[i]);
      end;
      vBody := vBody + '}';
    end;
    // ordering key
    if aOrderingKey <> '' then
      vBody := vBody + ', "ordering_key": ' + sgcQuotedStr(aOrderingKey);
  end;
  vBody := vBody + '}]}';

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST,
    Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB + '/projects/%s/topics/%s:publish',
    [aProject, aTopic]), vBody);
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.Pull(const aProject,
  aSubscription: string; aMaxMessages: Integer = MaxInt): string;
var
  vBody: string;
begin
  vBody := '{"returnImmediately": false, "maxMessages": ' +
    IntToStr(aMaxMessages) + '}';

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST,
    Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB +
    '/projects/%s/subscriptions/%s:pull', [aProject, aSubscription]), vBody);
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.Seek(const aProject,
  aSubscription: string; aTimeUTC: String; const aSnapshot: String): string;
var
  vBody: string;
begin
  vBody := Format('{"time": "%s", "snapshot": "%s"}', [aTimeUTC, aSnapshot]);

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST,
    Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB +
    '/projects/%s/subscriptions/%s:seek', [aProject, aSubscription]), vBody);
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.SetIamPolicy(const aResource
  : string): string;
begin
  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_GET,
    Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB + '%s:setIamPolicy', [aResource]));
end;

function TsgcHTTP_Google_Cloud_PubSub_Client.TestIamPermissions(const aResource
  : string; aPermissions: TStrings): string;
var
  i: Integer;
  vBody: string;
begin
  vBody := '{"permissions": [';
  for i := 0 to aPermissions.Count - 1 do
  begin
    if i > 0 then
      vBody := vBody + ',';
    vBody := vBody + aPermissions[i];
  end;
  vBody := vBody + ']}';

  result := DoHTTP(CS_GOOGLE_CLOUD_HTTP_POST,
    Format(CS_GOOGLE_CLOUD_ENPOINT_PUBSUB + '%s:testIamPermissions',
    [aResource]), vBody);
end;

{$ENDIF}

end.
