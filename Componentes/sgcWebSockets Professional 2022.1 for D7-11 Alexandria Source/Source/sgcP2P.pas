{ ***************************************************************************
  sgcP2P component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }
unit sgcP2P;

interface

{$I sgcVer.inc}
{$IFDEF SGC_P2P}

uses
  Classes,
  sgcUDP_Client {$IFDEF SGC_EDT_PRO}, sgcUDP_Server {$ENDIF}
{$IFDEF SGC_STUN}, sgcP2P_STUN_Client{$IFDEF SGC_EDT_PRO},
  sgcP2P_STUN_Server{$ENDIF}{$ENDIF}
{$IFDEF SGC_TURN}, sgcP2P_TURN_Client, sgcP2P_TURN_Server{$ENDIF}
{$IFDEF SGC_ICE}, sgcP2P_ICE_Client{$ENDIF};

type
  TsgcUDPCLient = class(TsgcUDPCLient_Base)
  published
    property Host;
    property Port;
    property IPVersion;
    property LogFile;
    property Proxy;
    property NotifyEvents;
  published
    property OnUDPRead;
    property OnUDPException;
  published
    property Version;
  end;

{$IFDEF SGC_EDT_PRO}

  TsgcUDPServer = class(TsgcUDPServer_Base)
  published
    property Active;
    property Host;
    property Port;
    property IPVersion;
    property Bindings;
    property LogFile;
    property NotifyEvents;
    property WatchDog;
  published
    property OnStartup;
    property OnShutdown;
    property OnUDPRead;
    property OnUDPException;
    property OnBeforeWatchDog;
  published
    property Version;
  end;
{$ENDIF}
{$IFDEF SGC_STUN}

  TsgcSTUNClient = class(TsgcSTUNCLient_Base)
  published
    property Host;
    property Port;
    property IPVersion;
    property RetransmissionOptions;
    property STUNOptions;
    property Transport;
    property LogFile;
    property NotifyEvents;
  published
    property OnSTUNException;
    property OnSTUNResponseError;
    property OnSTUNResponseSuccess;
    property OnSTUNBeforeSend;
  published
    property Version;
  end;

{$IFDEF SGC_EDT_PRO}

  TsgcSTUNServer = class(TsgcSTUNServer_Base)
  published
    property Active;
    property Host;
    property Port;
    property IPVersion;
    property STUNOptions;
    property LogFile;
    property NotifyEvents;
  published
    property OnSTUNException;
    property OnSTUNRequestError;
    property OnSTUNRequestSuccess;
    property OnSTUNRequestAuthorization;
  published
    property Version;
  end;
{$ENDIF}
{$ENDIF}

{$IFDEF SGC_TURN}

  TsgcTURNClient = class(TsgcTURNCLient_Base)
  published
    property Host;
    property Port;
    property IPVersion;
    property RetransmissionOptions;
    property STUNOptions;
    property Transport;
    property LogFile;
    property NotifyEvents;
    property TURNOptions;
  published
    property OnSTUNException;
    property OnSTUNResponseError;
    property OnSTUNResponseSuccess;
    property OnSTUNBeforeSend;
  published
    property OnTURNAllocate;
    property OnTURNRefresh;
    property OnTURNCreatePermission;
    property OnTURNChannelBind;
    property OnTURNChannelData;
    property OnTURNDataIndication;
    property OnTURNICMPIndication;
  published
    property Version;
  end;

  TsgcTURNServer = class(TsgcTURNServer_Base)
  published
    property Active;
    property Host;
    property Port;
    property IPVersion;
    property STUNOptions;
    property LogFile;
    property NotifyEvents;
    property TURNOptions;
  published
    property OnSTUNException;
    property OnSTUNRequestError;
    property OnSTUNRequestSuccess;
    property OnSTUNRequestAuthorization;
  published
    property OnTURNBeforeAllocate;
    property OnTURNChannelDataDiscarded;
    property OnTURNMessageDiscarded;
    property OnTURNCreateAllocation;
    property OnTURNDeleteAllocation;
  published
    property Version;
  end;
{$ENDIF}

{$IFDEF SGC_ICE}
  TsgcICEClient = class(TsgcICECLient_Base)
  published
    property TURNClient;
    property ICEOptions;
  published
    property OnICECandidate;
    property OnICECandidateError;
    property OnICECandidatePairNominated;
    property OnICECandidatePairFailed;
    property OnICEReceiveBindingRequest;
    property OnICEException;
  published
    property Version;
  end;
{$ENDIF}
{$ENDIF}

implementation

end.
