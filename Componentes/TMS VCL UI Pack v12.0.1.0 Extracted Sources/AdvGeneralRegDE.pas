{*************************************************************************}
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2017                                              }
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

unit AdvGeneralRegDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, Controls, AdvGeneralDE,
  AdvCustomComponent,
  {$IFDEF FMXLIB}
  AdvBaseControl, AdvCalendar, AdvHTMLText, AdvListEditor, AdvPopup, AdvSearchEdit,
  AdvToolBar, AdvCustomControl, AdvFindDialog, AdvGridRTF,
  AdvHotspotImageEditor, AdvMemo, AdvMemoDialogs, AdvPlanner, AdvPrintPreview,
  AdvReplaceDialog, AdvTouchKeyboard, FMX.GridExcelIO,
  {$ENDIF}
  {$IFDEF WIN32}
  SysUtils, DesignEditors, DesignIntf
  {$ENDIF}
  ;

procedure Register;

implementation

procedure Register;
begin
  {$IFDEF WIN32}
  RegisterComponentEditor(TAdvCustomComponent, TAdvDefaultEditor);
  {$IFDEF FMXLIB}
  RegisterComponentEditor(TAdvGridRTFIO, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvFindDialog, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvMemoCustomStyler, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvMemoFindDialog, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvMemoFindAndReplaceDialog, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvPlannerItemEditor, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvPlannerAdapter, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvGridPrintPreviewDialog, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvReplaceDialog, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvPopupTouchKeyboard, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvGridExcelIO, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvHotSpotImageEditorDialog, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvCustomControl, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvBaseControl, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvCalendarPicker, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvHTMLText, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvListEditor, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvPopup, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvSearchEdit, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvCustomToolBarElement, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvCustomToolBar, TAdvDefaultEditor);
  RegisterComponentEditor(TAdvCustomDockPanel, TAdvDefaultEditor);
  {$ENDIF}

  UnlistPublishedProperty(TAdvCustomComponent, 'AlignWithMargins');
  UnlistPublishedProperty(TAdvCustomComponent, 'Cursor');
  UnlistPublishedProperty(TAdvCustomComponent, 'CustomHint');
  UnlistPublishedProperty(TAdvCustomComponent, 'HelpContext');
  UnlistPublishedProperty(TAdvCustomComponent, 'HelpKeyword');
  UnlistPublishedProperty(TAdvCustomComponent, 'HelpType');
  UnlistPublishedProperty(TAdvCustomComponent, 'Margins');
  UnlistPublishedProperty(TAdvCustomComponent, 'ParentCustomHint');
  UnlistPublishedProperty(TAdvCustomComponent, 'Width');
  UnlistPublishedProperty(TAdvCustomComponent, 'Height');
  UnlistPublishedProperty(TAdvCustomComponent, 'Position');
  UnlistPublishedProperty(TAdvCustomComponent, 'Size');
  UnlistPublishedProperty(TAdvCustomComponent, 'Left');
  UnlistPublishedProperty(TAdvCustomComponent, 'Top');
  UnlistPublishedProperty(TAdvCustomComponent, 'Visible');
  UnlistPublishedProperty(TAdvCustomComponent, 'Tag');
  UnlistPublishedProperty(TAdvCustomComponent, 'Hint');
  UnlistPublishedProperty(TAdvCustomComponent, 'Touch');
  UnlistPublishedProperty(TAdvCustomComponent, 'StyleName');
  UnlistPublishedProperty(TAdvCustomComponent, 'OnGesture');
  UnlistPublishedProperty(TAdvCustomComponent, 'OnTap');
  {$ENDIF}
end;

end.

