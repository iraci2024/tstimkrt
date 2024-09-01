{*************************************************************************}
{                                                                         }
{ written by TMS Software                                                 }
{           copyright (c)  2020 - 2021                                    }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit LCLTMSFNCGeneralRegDE;

{$I LCLTMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, LCLTMSFNCCustomControl, LCLTMSFNCStyles, LCLTMSFNCCustomComponent, LCLTMSFNCGeneralDE
  {$IFNDEF LCLWEBLIB}
  ,DesignIntF
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,WEBLib.DesignIntF, Web
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,ComponentEditors, PropEdits
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,VCL.Graphics
  {$ENDIF}
  ;

procedure Register;

implementation


uses
{$IFDEF VCLLIB}
  ToolsApi, System.SysUtils, VCL.Dialogs,
{$ENDIF}
  LCLTMSFNCGraphicsTypes;
{$IFDEF FNCLIB}
{$IFDEF VCLLIB}
const TMSPRODUCTOFFSET = 700;

{$I TMSProductSplash.inc}
{$ENDIF}
{$ENDIF}

procedure Register;
begin
  {$IFNDEF WEBLIB}
  RegisterPropertyEditor(TypeInfo(string), TPersistent,'APIKey', TTMSFNCAPIKeyProperty);
  {$ENDIF}
  {$IFDEF FNCLIB}
  RegisterPropertyEditor(TypeInfo(string),TTMSFNCStylesManager,'StyleResource', TTMSFNCStylesManagerStyleResourceProperty);
  {$ENDIF}
  RegisterComponentEditor(TTMSFNCCustomComponent, TTMSFNCDefaultEditor);
  RegisterComponentEditor(TTMSFNCCustomControl, TTMSFNCDefaultEditor);
  {$IFNDEF LCLLIB}
  RegisterSelectionEditor(TTMSFNCCustomControl, TTMSFNCGeneralSelectionEditor);
  {$ENDIF}
  RegisterPropertyEditor(TypeInfo(string),TTMSFNCAppearanceGlobalFont,'Name', TTMSFNCGlobalFontNamePropertyEditor);
  {$IFDEF FNCLIB}
  {$IFDEF VCLLIB}
  RegisterPropertyEditor(TypeInfo(TColor), TPersistent, '', TTMSFNCColorProperty);
  ForceDemandLoadState(dlDisable);
  AddSplash;
  {$ENDIF}
  {$ENDIF}

  {$IFNDEF LCLWEBLIB}
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'AlignWithMargins');
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'Cursor');
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'CustomHint');
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'HelpContext');
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'HelpKeyword');
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'HelpType');
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'Margins');
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'ParentCustomHint');
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'Width');
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'Height');
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'Position');
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'Size');
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'Left');
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'Top');
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'Visible');
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'Tag');
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'Hint');
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'Touch');
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'StyleName');
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'OnGesture');
  UnlistPublishedProperty(TTMSFNCCustomComponent, 'OnTap');
  {$ENDIF}
end;

{$IFDEF WEBLIB}
initialization
  Register;
{$ENDIF}

end.

