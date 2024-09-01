program SignalRClient;

uses
  Forms,
  uSignalRClient in 'uSignalRClient.pas' {fmChatForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmChatForm, fmChatForm);
  Application.Run;
end.
