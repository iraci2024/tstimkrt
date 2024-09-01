object SSHServerSrv: TSSHServerSrv
  OldCreateOrder = False
  OnCreate = ServiceCreate
  OnDestroy = ServiceDestroy
  AllowPause = False
  DisplayName = 'Devart SSH Server'
  OnExecute = ServiceExecute
  OnStart = ServiceStart
  OnStop = ServiceStop
  Left = 192
  Top = 114
  Height = 150
  Width = 215
  object ScSSHServer: TScSSHServer
    HostKeyAlgorithms = [aaDSA, aaRSA]
    Storage = ScFileStorage
    Left = 24
    Top = 16
  end
  object ScFileStorage: TScFileStorage
    StoreUserPassword = False
    Left = 128
    Top = 16
  end
end
