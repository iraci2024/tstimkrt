program SSHServerService;

uses
  SvcMgr,
  SSHServerSrv in 'SSHServerSrv.pas' {SSHServer: TSSHServerSrv};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TSSHServerSrv, SSHServer);
  Application.Run;
end.
