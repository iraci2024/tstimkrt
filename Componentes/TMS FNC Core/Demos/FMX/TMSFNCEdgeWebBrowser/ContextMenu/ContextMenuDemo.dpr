program ContextMenuDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UContextMenuDemo in 'UContextMenuDemo.pas' {ContextMenuForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TContextMenuForm, ContextMenuForm);
  Application.Run;
end.
