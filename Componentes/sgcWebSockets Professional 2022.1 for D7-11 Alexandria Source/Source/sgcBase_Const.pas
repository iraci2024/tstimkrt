{***************************************************************************
 sgcBase component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcBase_Const;

interface

{$I sgcVer.inc}

Const
  CS_APPLICATION_NAME = 'sgcWebSockets';
  CS_VERSION = '2022.1.0'
  ;
  {$IFDEF SGC_EDT_ENT}
    CS_EDITION = 'Enterprise';
  {$ELSE}
    {$IFDEF SGC_EDT_PRO}
      CS_EDITION = 'Professional';
    {$ELSE}
      CS_EDITION = 'Standard';
    {$ENDIF}
  {$ENDIF}
  {$IFDEF SGC_LIC_SITE}
  CS_LICENSE = 'Site';
  {$ELSE}
    {$IFDEF SGC_LIC_TEAM}
    CS_LICENSE = 'Team';
    {$ELSE}
      {$IFDEF SGC_LIC_SINGLE}
      CS_LICENSE = 'Single';
      {$ELSE}
      CS_LICENSE = '';
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

{$IFDEF SGC_WRITECONST}Var{$ELSE}Const{$ENDIF}
  CS_USER_AGENT{$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'Mozilla/5.0 (' + CS_APPLICATION_NAME + ' ' + CS_VERSION + ')';

implementation

end.
