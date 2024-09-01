{*************************************************************************}
{ TAdvPanel, TAdvPanelGroup, TAdvPanelStyler component                    }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written                                                                 }
{   TMS Software                                                          }
{   copyright � 2000 - 2020                                               }
{   Email : info@tmssoftware.com                                          }
{   Web : http://www.tmssoftware.com                                      }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The source         }
{ code remains property of the writer and may not be distributed          }
{ freely as such.                                                         }
{*************************************************************************}

unit advpnlregde;

interface

{$I TMSDEFS.INC}

uses
  AdvPanel, Classes, HTMLSDE, AdvPnlDe,
  AdvImage, AdvImgDE, DesignIntf, DesignEditors;

procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TAdvImage), TAdvPanel, 'Background', TAdvImageProperty);
  {$IFDEF DELPHI_UNICODE}
  RegisterPropertyEditor(TypeInfo(String), TAdvPanel, 'Text', THTMLStringProperty);
  RegisterPropertyEditor(TypeInfo(String), TPanelCaption, 'Text', THTMLStringProperty);
  {$ENDIF}
  RegisterComponentEditor(TAdvPanel,TAdvPanelEditor);
end;

end.
