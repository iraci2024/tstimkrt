{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2021 - 2022                               }
{            Email : info@tmssoftware.com                            }
{            Web : https://www.tmssoftware.com                       }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit VCL.TMSFNCPrinters;

{$I VCL.TMSFNCDefines.inc}
{$IFDEF LCLLIB}
  {$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
  {$DEFINE ANDROIDIOSWEBLIB}
  {$DEFINE ANDROIDWEBLIB}
  {$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF ANDROID}
  {$DEFINE ANDROIDIOSLIB}
  {$DEFINE ANDROIDIOSWEBLIB}
  {$DEFINE ANDROIDWEBLIB}
{$ENDIF}
{$IFDEF IOS}
  {$DEFINE ANDROIDIOSLIB}
  {$DEFINE ANDROIDIOSWEBLIB}
{$ENDIF}
{$IFDEF FMXLIB}
  {$DEFINE FMXVCLLIB}
{$ENDIF}
{$IFDEF VCLLIB}
  {$DEFINE FMXVCLLIB}
{$ENDIF}

interface

uses
  Classes, Types, VCL.TMSFNCUtils, VCL.TMSFNCTypes, VCL.TMSFNCGraphics, VCL.Graphics, Printers
  {$IFDEF WEBLib}
  , web, JS
  {$ENDIF}
  {$IFNDEF LCLWEBLIB}
  , System.UITypes
  {$ENDIF}
  {$IFDEF LCLLIB}
  , LCLType
  {$ENDIF}
  {$IFDEF FMXLIB}
  {$ENDIF}
  {$IFDEF ANDROID}
  , FMX.Helpers.Android, AndroidApi.JNI.JavaTypes, AndroidApi.JNIBridge
  , AndroidApi.Helpers,AndroidApi.JNI.GraphicsContentViewText
  {$ENDIF}
  {$IFDEF IOS}
  ,iOSApi.UIKit, iOSapi.CocoaTypes , iOSApi.Foundation, iOSApi.CoreGraphics, Macapi.ObjectiveC,
  MacApi.ObjcRuntime
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 2; // Build nr.

  //v1.0.0.0: First release
  //v1.0.0.1: Fixed : Issue with BeginScene in Delphi 11
  //v1.0.0.2: Fixed : Memory leak

  {$IFDEF FMXLIB}
  PRINTDPI = 72;
  {$ENDIF}
  {$IFDEF CMNLIB}
  PRINTDPI = 100;
  {$ENDIF}
  {$IFDEF WEBLIB}
  PRINTDPI = 1;
  {$ENDIF}

{$IFDEF IOS}
  UIPrintInfoOutputGeneral = 0;
  {$EXTERNALSYM UIPrintInfoOutputGeneral}
  UIPrintInfoOutputPhoto = 1;
  {$EXTERNALSYM UIPrintInfoOutputPhoto}
  UIPrintInfoOutputGrayscale = 2;
  {$EXTERNALSYM UIPrintInfoOutputGrayscale}
  UIPrintInfoOutputPhotoGrayscale = 3;
  {$EXTERNALSYM UIPrintInfoOutputPhotoGrayscale}
  UIPrintInfoOrientationPortrait = 0;
  {$EXTERNALSYM UIPrintInfoOrientationPortrait}
  UIPrintInfoOrientationLandscape = 1;
  {$EXTERNALSYM UIPrintInfoOrientationLandscape}
{$ENDIF}

type
{$IFDEF ANDROID}

  /////////////////////////////////////////
  ///                                   ///
  ///          ANDROID CLASSES          ///
  ///                                   ///
  /////////////////////////////////////////
  TTMSFNCPrinter = class;
  TPrintSize = (psISO_A0, psISO_A1, psISO_A10, psISO_A2, psISO_A3, psISO_A4, psISO_A5, psISO_A6, psISO_A7, psISO_A8, psISO_A9,
                  psISO_B0, psISO_B1, psISO_B10, psISO_B2, psISO_B3, psISO_B4, psISO_B5, psISO_B6, psISO_B7, psISO_B8, psISO_B9,
                  psISO_C0, psISO_C1, psISO_C10, psISO_C2, psISO_C3, psISO_C4, psISO_C5, psISO_C6, psISO_C7, psISO_C8, psISO_C9,
                  psJIS_B0, psJIS_B1, psJIS_B10, psJIS_B2, psJIS_B3, psJIS_B4, psJIS_B5, psJIS_B6, psJIS_B7, psJIS_B8, psJIS_B9, psJIS_EXEC,
                  psJPN_CHOU2, psJPN_CHOU3, psJPN_CHOU4, psJPN_HAGAKI, psJPN_KAHU, psJPN_KAKU2, psJPN_OUFUKU, psJPN_YOU4,
                  psNA_FOOLSCAP, psNA_GOVT_LETTER, psNA_INDEX_3X5, psNA_INDEX_4X6, psNA_INDEX_5X8, psNA_JUNIOR_LEGAL, psNA_LEDGER,
                  psNA_LEGAL, psNA_LETTER, psNA_MONARCH, psNA_QUARTO, psNA_TABLOID,
                  psOM_DAI_PA_KAI, psOM_JUURO_KU_KAI, psOM_PA_KAI,
                  psPRC_1, psPRC_10, psPRC_16K, psPRC_2, psPRC_3, psPRC_4, psPRC_5, psPRC_6, psPRC_7, psPRC_8, psPRC_9,
                  psROC_16K, psROC_8K, psUNKNOWN_LANDSCAPE, psUNKNOWN_PORTRAIT);

  JPrintManager = interface; //android.print.PrintManager
  JPrintJob = interface;
  JPrintDocumentAdapter = interface;
  JFNCPrintDocumentAdapter = interface;
  JFNCPrintDocumentAdapterListener = interface;
  JPrintAttributes_Builder = interface;
  JPrintAttributes = interface; //android.print.PrintAttributes
  JPrintAttributes_Resolution = interface; //android.print.PrintAttributes$Resolution
  JPrintAttributes_Margins = interface; //android.print.PrintAttributes$Margins
  JPrintAttributes_MediaSize = interface; //android.print.PrintAttributes$MediaSize

  JPrintManagerClass = interface(JObjectClass)
  ['{7F56A1C8-FC5E-435C-930A-5EAC1D1E8571}']
  end;

  [JavaSignature('android/print/PrintManager')]
  JPrintManager = interface(JObject)
  ['{8CF65E90-1E7F-4658-B0B9-4FB40114EB78}']
    function getPrintJobs: JList; cdecl;
    function print(printJobName: JString; documentAdapter: JPrintDocumentAdapter; PrintAttributes: JPrintAttributes): JPrintJob;
  end;
  TJPrintManager = class(TJavaGenericImport<JPrintManagerClass, JPrintManager>);

  JPrintJobClass = interface(JObjectClass)
  end;

  [JavaSignature('android/print/PrintJob')]
  JPrintJob = interface(JObject)
  ['{8A914568-3CBC-45CA-9783-4590410352E2}']
  procedure cancel; cdecl;
  function equals(obj: JObject): Boolean; cdecl;
  function hashCode: integer; cdecl;
  function isBlocked: Boolean; cdecl;
  function isCancelled: Boolean; cdecl;
  function isCompleted: Boolean; cdecl;
  function isFailed: Boolean; cdecl;
  function isQueued: Boolean; cdecl;
  function isStarted: Boolean; cdecl;
  procedure restart; cdecl;
  end;
  TJPrintJob = class(TJavaGenericImport<JPrintJobClass, JPrintJob>);

  JPrintDocumentAdapterClass = interface(JObjectClass)
    ['{9E14C8BC-810E-4BEA-9635-AA79DA6D3476}']
    function _GetEXTRA_PRINT_PREVIEW: JString; cdecl;
    function init: JPrintDocumentAdapter; cdecl;
    property EXTRA_PRINT_PREVIEW: JString read _GetEXTRA_PRINT_PREVIEW;
  end;

  [JavaSignature('android/print/PrintDocumentAdapter')]
  JPrintDocumentAdapter = interface(JObject)
    ['{CE4783E1-D54F-46FA-B2DC-DB65A20DC786}']
    procedure onFinish; cdecl;
    procedure onStart; cdecl;
  end;
  TJPrintDocumentAdapter = class(TJavaGenericImport<JPrintDocumentAdapterClass, JPrintDocumentAdapter>) end;

  JFNCPrintDocumentAdapterClass = interface(JObjectClass)
  ['{2367BEE9-732E-4BA4-B8FD-863B4028F7DB}']
    function init(context: JContext): JFNCPrintDocumentAdapter; cdecl;
  end;

  [JavaSignature('FNCPrintDocumentAdapter')]
  JFNCPrintDocumentAdapter = interface(JObject)
  ['{3C06CA10-5314-4A11-8584-600BC2879EF0}']
    procedure setListener(l: JFNCPrintDocumentAdapterListener); cdecl;
    procedure setPageHeight(h: Integer); cdecl;
    procedure setPageWidth(w: Integer); cdecl;
    procedure setPrintName(jn: JString); cdecl;
    procedure setDPI(d: integer); cdecl;
    procedure setMyPrintAttributes(pa: JPrintAttributes); cdecl;
    procedure newPage; cdecl;
    procedure endDoc; cdecl;
    function getMyPageCanvas: JCanvas; cdecl;
    function getPageHeight: integer; cdecl;
    function getPageWidth: integer; cdecl;
    function getPrintName: JString; cdecl;
    function getPageNumber: integer; cdecl;
    function getDPI: integer; cdecl;
    function getMyPrintAttributes: JPrintAttributes; cdecl;
  end;
  TJFNCPrintDocumentAdapter = class(TJavaGenericImport<JFNCPrintDocumentAdapterClass, JFNCPrintDocumentAdapter>);

  JFNCPrintDocumentAdapterListenerClass = interface(IJavaClass)
  ['{CF3D751A-EEF4-4BCF-B4F5-C766EC76B3DA}']
  end;

  [JavaSignature('FNCPrintDocumentAdapter$FNCPrintDocumentAdapterListener')]
  JFNCPrintDocumentAdapterListener = interface(IJavaInstance)
  ['{148558C6-ACFA-4BE7-A8BA-3C2F8F66A1BF}']
    procedure onWriteContent; cdecl;
  end;
  TFNCPrintDocumentAdapterListener = class(TJavaGenericImport<JFNCPrintDocumentAdapterListenerClass, JFNCPrintDocumentAdapterListener>) end;

  TTMSFNCPrintDocumentAdapterListener = class(TJavaLocal, JFNCPrintDocumentAdapterListener)
  private
    FPrinter: TTMSFNCPrinter;
  public
    procedure onWriteContent; cdecl;
  end;

  JPrintAttributes_BuilderClass = interface(JObjectClass)
  ['{62D77F93-F2EA-4484-9D7E-7C904EBE8947}']
    function init: JPrintAttributes_Builder; cdecl;
  end;
  [JavaSignature('android/print/PrintAttributes$Builder')]
  JPrintAttributes_Builder = interface(JObject)
  ['{E4A8CF83-046F-453A-AEB6-6446FF689939}']
    function build: JPrintAttributes; cdecl;
    function setColorMode(colorMode: Integer): JPrintAttributes_Builder; cdecl;
    function setMediaSize(mediaSize: JPrintAttributes_MediaSize): JPrintAttributes_Builder; cdecl;
    function setMargins(Margins: JPrintAttributes_Margins): JPrintAttributes_Builder; cdecl;
    function setResolution(Resolution: JPrintAttributes_Resolution): JPrintAttributes_Builder; cdecl;
  end;
  TJPrintAttributes_Builder = class(TJavaGenericImport<JPrintAttributes_BuilderClass, JPrintAttributes_Builder>) end;

  JPrintAttributesClass = interface(JObjectClass)
  ['{D9B40228-CC37-4573-896E-10D7800670CE}']
    function _GetCOLOR_MODE_COLOR: Integer; cdecl;
    function _GetCOLOR_MODE_MONOCHROME: Integer; cdecl;
    function _GetDUPLEX_MODE_LONG_EDGE: Integer; cdecl;
    function _GetDUPLEX_MODE_NONE: Integer; cdecl;
    function _GetDUPLEX_MODE_SHORT_EDGE: Integer; cdecl;
    property COLOR_MODE_COLOR: Integer read _GetCOLOR_MODE_COLOR;
    property COLOR_MODE_MONOCHROME: Integer read _GetCOLOR_MODE_MONOCHROME;
    property DUPLEX_MODE_LONG_EDGE: Integer read _GetDUPLEX_MODE_LONG_EDGE;
    property DUPLEX_MODE_NONE: Integer read _GetDUPLEX_MODE_NONE;
    property DUPLEX_MODE_SHORT_EDGE: Integer read _GetDUPLEX_MODE_SHORT_EDGE;
  end;

  [JavaSignature('android/print/PrintAttributes')]
  JPrintAttributes = interface(JObject)
    ['{312C7538-43CD-4607-B8E2-04CF431C9B5B}']
    function describeContents: Integer; cdecl;
    function equals(obj: JObject): Boolean; cdecl;
    function getColorMode: Integer; cdecl;
    function getDuplexMode: Integer; cdecl;
    function getMediaSize: JPrintAttributes_MediaSize; cdecl;
    function getMinMargins: JPrintAttributes_Margins; cdecl;
    function getResolution: JPrintAttributes_Resolution; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJPrintAttributes = class(TJavaGenericImport<JPrintAttributesClass, JPrintAttributes>) end;

  JPrintAttributes_ResolutionClass = interface(JObjectClass)
  ['{890A9602-792D-417F-B413-41CD6576A178}']
  end;
  [JavaSignature('android/print/PrintAttribute$Resolution')]
  JPrintAttributes_Resolution = interface(JObject)
  ['{0E4C6F96-FB83-4187-B26C-332F9DBDEBD3}']
  end;
  TJPrintAttributes_Resolution = class(TJavaGenericImport<JPrintAttributes_ResolutionClass, JPrintAttributes_Resolution>) end;

  JPrintAttributes_MarginsClass = interface(JObjectClass)
  ['{CB38E8EF-9112-4F1F-B713-0B2E03A8EDF3}']
  end;
  [JavaSignature('android/print/PrintAttribute$Margins')]
  JPrintAttributes_Margins = interface(JObject)
  ['{21BF97D5-0C39-4F71-BA25-1B1C168BF23E}']
  end;
  TJPrintAttributes_Margins = class(TJavaGenericImport<JPrintAttributes_MarginsClass, JPrintAttributes_Margins>) end;

  JPrintAttributes_MediaSizeClass = interface(JObjectClass)
  ['{0A2D852F-16AE-49A1-BCFE-E1E3E12A15C4}']
    function _GetISO_A0: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_A1: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_A10: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_A2: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_A3: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_A4: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_A5: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_A6: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_A7: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_A8: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_A9: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_B0: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_B1: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_B10: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_B2: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_B3: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_B4: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_B5: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_B6: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_B7: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_B8: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_B9: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_C0: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_C1: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_C10: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_C2: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_C3: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_C4: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_C5: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_C6: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_C7: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_C8: JPrintAttributes_MediaSize; cdecl;
    function _GetISO_C9: JPrintAttributes_MediaSize; cdecl;
    function _GetJIS_B0: JPrintAttributes_MediaSize; cdecl;
    function _GetJIS_B1: JPrintAttributes_MediaSize; cdecl;
    function _GetJIS_B10: JPrintAttributes_MediaSize; cdecl;
    function _GetJIS_B2: JPrintAttributes_MediaSize; cdecl;
    function _GetJIS_B3: JPrintAttributes_MediaSize; cdecl;
    function _GetJIS_B4: JPrintAttributes_MediaSize; cdecl;
    function _GetJIS_B5: JPrintAttributes_MediaSize; cdecl;
    function _GetJIS_B6: JPrintAttributes_MediaSize; cdecl;
    function _GetJIS_B7: JPrintAttributes_MediaSize; cdecl;
    function _GetJIS_B8: JPrintAttributes_MediaSize; cdecl;
    function _GetJIS_B9: JPrintAttributes_MediaSize; cdecl;
    function _GetJIS_EXEC: JPrintAttributes_MediaSize; cdecl;
    function _GetJPN_CHOU2: JPrintAttributes_MediaSize; cdecl;
    function _GetJPN_CHOU3: JPrintAttributes_MediaSize; cdecl;
    function _GetJPN_CHOU4: JPrintAttributes_MediaSize; cdecl;
    function _GetJPN_HAGAKI: JPrintAttributes_MediaSize; cdecl;
    function _GetJPN_KAHU: JPrintAttributes_MediaSize; cdecl;
    function _GetJPN_KAKU2: JPrintAttributes_MediaSize; cdecl;
    function _GetJPN_OUFUKU: JPrintAttributes_MediaSize; cdecl;
    function _GetJPN_YOU4: JPrintAttributes_MediaSize; cdecl;
    function _GetNA_FOOLSCAP: JPrintAttributes_MediaSize; cdecl;
    function _GetNA_GOVT_LETTER: JPrintAttributes_MediaSize; cdecl;
    function _GetNA_INDEX_3X5: JPrintAttributes_MediaSize; cdecl;
    function _GetNA_INDEX_4X6: JPrintAttributes_MediaSize; cdecl;
    function _GetNA_INDEX_5X8: JPrintAttributes_MediaSize; cdecl;
    function _GetNA_JUNIOR_LEGAL: JPrintAttributes_MediaSize; cdecl;
    function _GetNA_LEDGER: JPrintAttributes_MediaSize; cdecl;
    function _GetNA_LEGAL: JPrintAttributes_MediaSize; cdecl;
    function _GetNA_LETTER: JPrintAttributes_MediaSize; cdecl;
    function _GetNA_MONARCH: JPrintAttributes_MediaSize; cdecl;
    function _GetNA_QUARTO: JPrintAttributes_MediaSize; cdecl;
    function _GetNA_TABLOID: JPrintAttributes_MediaSize; cdecl;
    function _GetOM_DAI_PA_KAI: JPrintAttributes_MediaSize; cdecl;
    function _GetOM_JUURO_KU_KAI: JPrintAttributes_MediaSize; cdecl;
    function _GetOM_PA_KAI: JPrintAttributes_MediaSize; cdecl;
    function _GetPRC_1: JPrintAttributes_MediaSize; cdecl;
    function _GetPRC_10: JPrintAttributes_MediaSize; cdecl;
    function _GetPRC_16K: JPrintAttributes_MediaSize; cdecl;
    function _GetPRC_2: JPrintAttributes_MediaSize; cdecl;
    function _GetPRC_3: JPrintAttributes_MediaSize; cdecl;
    function _GetPRC_4: JPrintAttributes_MediaSize; cdecl;
    function _GetPRC_5: JPrintAttributes_MediaSize; cdecl;
    function _GetPRC_6: JPrintAttributes_MediaSize; cdecl;
    function _GetPRC_7: JPrintAttributes_MediaSize; cdecl;
    function _GetPRC_8: JPrintAttributes_MediaSize; cdecl;
    function _GetPRC_9: JPrintAttributes_MediaSize; cdecl;
    function _GetROC_16K: JPrintAttributes_MediaSize; cdecl;
    function _GetROC_8K: JPrintAttributes_MediaSize; cdecl;
    function _GetUNKNOWN_LANDSCAPE: JPrintAttributes_MediaSize; cdecl;
    function _GetUNKNOWN_PORTRAIT: JPrintAttributes_MediaSize; cdecl;
    function init(id: JString; label_: JString; widthMils: Integer; heightMils: Integer): JPrintAttributes_MediaSize; cdecl;
    property ISO_A0: JPrintAttributes_MediaSize read _GetISO_A0;
    property ISO_A1: JPrintAttributes_MediaSize read _GetISO_A1;
    property ISO_A10: JPrintAttributes_MediaSize read _GetISO_A10;
    property ISO_A2: JPrintAttributes_MediaSize read _GetISO_A2;
    property ISO_A3: JPrintAttributes_MediaSize read _GetISO_A3;
    property ISO_A4: JPrintAttributes_MediaSize read _GetISO_A4;
    property ISO_A5: JPrintAttributes_MediaSize read _GetISO_A5;
    property ISO_A6: JPrintAttributes_MediaSize read _GetISO_A6;
    property ISO_A7: JPrintAttributes_MediaSize read _GetISO_A7;
    property ISO_A8: JPrintAttributes_MediaSize read _GetISO_A8;
    property ISO_A9: JPrintAttributes_MediaSize read _GetISO_A9;
    property ISO_B0: JPrintAttributes_MediaSize read _GetISO_B0;
    property ISO_B1: JPrintAttributes_MediaSize read _GetISO_B1;
    property ISO_B10: JPrintAttributes_MediaSize read _GetISO_B10;
    property ISO_B2: JPrintAttributes_MediaSize read _GetISO_B2;
    property ISO_B3: JPrintAttributes_MediaSize read _GetISO_B3;
    property ISO_B4: JPrintAttributes_MediaSize read _GetISO_B4;
    property ISO_B5: JPrintAttributes_MediaSize read _GetISO_B5;
    property ISO_B6: JPrintAttributes_MediaSize read _GetISO_B6;
    property ISO_B7: JPrintAttributes_MediaSize read _GetISO_B7;
    property ISO_B8: JPrintAttributes_MediaSize read _GetISO_B8;
    property ISO_B9: JPrintAttributes_MediaSize read _GetISO_B9;
    property ISO_C0: JPrintAttributes_MediaSize read _GetISO_C0;
    property ISO_C1: JPrintAttributes_MediaSize read _GetISO_C1;
    property ISO_C10: JPrintAttributes_MediaSize read _GetISO_C10;
    property ISO_C2: JPrintAttributes_MediaSize read _GetISO_C2;
    property ISO_C3: JPrintAttributes_MediaSize read _GetISO_C3;
    property ISO_C4: JPrintAttributes_MediaSize read _GetISO_C4;
    property ISO_C5: JPrintAttributes_MediaSize read _GetISO_C5;
    property ISO_C6: JPrintAttributes_MediaSize read _GetISO_C6;
    property ISO_C7: JPrintAttributes_MediaSize read _GetISO_C7;
    property ISO_C8: JPrintAttributes_MediaSize read _GetISO_C8;
    property ISO_C9: JPrintAttributes_MediaSize read _GetISO_C9;
    property JIS_B0: JPrintAttributes_MediaSize read _GetJIS_B0;
    property JIS_B1: JPrintAttributes_MediaSize read _GetJIS_B1;
    property JIS_B10: JPrintAttributes_MediaSize read _GetJIS_B10;
    property JIS_B2: JPrintAttributes_MediaSize read _GetJIS_B2;
    property JIS_B3: JPrintAttributes_MediaSize read _GetJIS_B3;
    property JIS_B4: JPrintAttributes_MediaSize read _GetJIS_B4;
    property JIS_B5: JPrintAttributes_MediaSize read _GetJIS_B5;
    property JIS_B6: JPrintAttributes_MediaSize read _GetJIS_B6;
    property JIS_B7: JPrintAttributes_MediaSize read _GetJIS_B7;
    property JIS_B8: JPrintAttributes_MediaSize read _GetJIS_B8;
    property JIS_B9: JPrintAttributes_MediaSize read _GetJIS_B9;
    property JIS_EXEC: JPrintAttributes_MediaSize read _GetJIS_EXEC;
    property JPN_CHOU2: JPrintAttributes_MediaSize read _GetJPN_CHOU2;
    property JPN_CHOU3: JPrintAttributes_MediaSize read _GetJPN_CHOU3;
    property JPN_CHOU4: JPrintAttributes_MediaSize read _GetJPN_CHOU4;
    property JPN_HAGAKI: JPrintAttributes_MediaSize read _GetJPN_HAGAKI;
    property JPN_KAHU: JPrintAttributes_MediaSize read _GetJPN_KAHU;
    property JPN_KAKU2: JPrintAttributes_MediaSize read _GetJPN_KAKU2;
    property JPN_OUFUKU: JPrintAttributes_MediaSize read _GetJPN_OUFUKU;
    property JPN_YOU4: JPrintAttributes_MediaSize read _GetJPN_YOU4;
    property NA_FOOLSCAP: JPrintAttributes_MediaSize read _GetNA_FOOLSCAP;
    property NA_GOVT_LETTER: JPrintAttributes_MediaSize read _GetNA_GOVT_LETTER;
    property NA_INDEX_3X5: JPrintAttributes_MediaSize read _GetNA_INDEX_3X5;
    property NA_INDEX_4X6: JPrintAttributes_MediaSize read _GetNA_INDEX_4X6;
    property NA_INDEX_5X8: JPrintAttributes_MediaSize read _GetNA_INDEX_5X8;
    property NA_JUNIOR_LEGAL: JPrintAttributes_MediaSize read _GetNA_JUNIOR_LEGAL;
    property NA_LEDGER: JPrintAttributes_MediaSize read _GetNA_LEDGER;
    property NA_LEGAL: JPrintAttributes_MediaSize read _GetNA_LEGAL;
    property NA_LETTER: JPrintAttributes_MediaSize read _GetNA_LETTER;
    property NA_MONARCH: JPrintAttributes_MediaSize read _GetNA_MONARCH;
    property NA_QUARTO: JPrintAttributes_MediaSize read _GetNA_QUARTO;
    property NA_TABLOID: JPrintAttributes_MediaSize read _GetNA_TABLOID;
    property OM_DAI_PA_KAI: JPrintAttributes_MediaSize read _GetOM_DAI_PA_KAI;
    property OM_JUURO_KU_KAI: JPrintAttributes_MediaSize read _GetOM_JUURO_KU_KAI;
    property OM_PA_KAI: JPrintAttributes_MediaSize read _GetOM_PA_KAI;
    property PRC_1: JPrintAttributes_MediaSize read _GetPRC_1;
    property PRC_10: JPrintAttributes_MediaSize read _GetPRC_10;
    property PRC_16K: JPrintAttributes_MediaSize read _GetPRC_16K;
    property PRC_2: JPrintAttributes_MediaSize read _GetPRC_2;
    property PRC_3: JPrintAttributes_MediaSize read _GetPRC_3;
    property PRC_4: JPrintAttributes_MediaSize read _GetPRC_4;
    property PRC_5: JPrintAttributes_MediaSize read _GetPRC_5;
    property PRC_6: JPrintAttributes_MediaSize read _GetPRC_6;
    property PRC_7: JPrintAttributes_MediaSize read _GetPRC_7;
    property PRC_8: JPrintAttributes_MediaSize read _GetPRC_8;
    property PRC_9: JPrintAttributes_MediaSize read _GetPRC_9;
    property ROC_16K: JPrintAttributes_MediaSize read _GetROC_16K;
    property ROC_8K: JPrintAttributes_MediaSize read _GetROC_8K;
    property UNKNOWN_LANDSCAPE: JPrintAttributes_MediaSize read _GetUNKNOWN_LANDSCAPE;
    property UNKNOWN_PORTRAIT: JPrintAttributes_MediaSize read _GetUNKNOWN_PORTRAIT;
  end;

  [JavaSignature('android/print/PrintAttributes$MediaSize')]
  JPrintAttributes_MediaSize = interface(JObject)
    ['{1F95EC3E-8188-4438-8575-C44CABB52DE5}']
    function asLandscape: JPrintAttributes_MediaSize; cdecl;
    function asPortrait: JPrintAttributes_MediaSize; cdecl;
    function equals(obj: JObject): Boolean; cdecl;
    function getHeightMils: Integer; cdecl;
    function getId: JString; cdecl;
    function getLabel(packageManager: JPackageManager): JString; cdecl;
    function getWidthMils: Integer; cdecl;
    function hashCode: Integer; cdecl;
    function isPortrait: Boolean; cdecl;
    function toString: JString; cdecl;
  end;
  TJPrintAttributes_MediaSize = class(TJavaGenericImport<JPrintAttributes_MediaSizeClass, JPrintAttributes_MediaSize>) end;
{$ENDIF}

  /////////////////////////////////////////
  ///                                   ///
  ///            iOS CLASSES            ///
  ///                                   ///
  /////////////////////////////////////////

{$IFDEF IOS}
  PNSDictionary = Pointer;
  UIPrintInfoOutputType = NSInteger;
  UIPrintInfoOrientation = NSInteger;
  UIView = interface;
  UIPrintInteractionController = interface;
  UIPrintInteractionCompletionHandler = procedure
    (param1: UIPrintInteractionController; param2: Boolean; param3: NSError)
    of object;

  UIResponderClass = interface(NSObjectClass)
  ['{E63112FD-A161-4E78-857F-6B28F30E0883}']
  end;
  UIResponder = interface(NSObject)
    ['{3294BC42-563A-4D5A-BE0D-391D08B4346D}']
    function becomeFirstResponder: Boolean; cdecl;
    function canBecomeFirstResponder: Boolean; cdecl;
    function canResignFirstResponder: Boolean; cdecl;
    function inputAccessoryView: UIView; cdecl;
    function inputView: UIView; cdecl;
    function isFirstResponder: Boolean; cdecl;
    function nextResponder: UIResponder; cdecl;
    procedure reloadInputViews; cdecl;
    function resignFirstResponder: Boolean; cdecl;
    function undoManager: NSUndoManager; cdecl;
  end;
  TUIResponder = class(TOCGenericImport<UIResponderClass, UIResponder>)  end;

  UIViewClass = interface(UIResponderClass)
  ['{B4074E20-38BD-4BA1-8148-71FDDB554C9B}']
    function areAnimationsEnabled: Boolean; cdecl;
    procedure beginAnimations(animationID: Pointer; context: Pointer); cdecl;
    procedure commitAnimations; cdecl;
    procedure setAnimationBeginsFromCurrentState(fromCurrentState: Boolean); cdecl;
    procedure setAnimationDelay(delay: NSTimeInterval); cdecl;
    procedure setAnimationDelegate(delegate: Pointer); cdecl;
    procedure setAnimationDuration(duration: NSTimeInterval); cdecl;
    procedure setAnimationRepeatAutoreverses(repeatAutoreverses: Boolean); cdecl;
    procedure setAnimationRepeatCount(repeatCount: Single); cdecl;
    procedure setAnimationStartDate(startDate: NSDate); cdecl;
    procedure setAnimationsEnabled(enabled: Boolean); cdecl;
  end;

  UIView = interface(UIResponder)
  ['{26D2EE24-0825-48D2-9CAF-7653B191A89D}']
    procedure addSubview(view: UIView); cdecl;
    procedure addConstraints(constraints: NSArray); cdecl;
    procedure removeConstraints(constraints: NSArray); cdecl;
    procedure updateConstraintsIfNeeded; cdecl;
    procedure updateConstraints; cdecl;
    function needsUpdateConstraints: Boolean; cdecl;
    procedure setNeedsUpdateConstraints; cdecl;
    function translatesAutoresizingMaskIntoConstraints: Boolean; cdecl;
    procedure setTranslatesAutoresizingMaskIntoConstraints(flag: Boolean); cdecl;
    function alpha: CGFloat; cdecl;
    function autoresizesSubviews: Boolean; cdecl;
    function bounds: CGRect; cdecl;
    procedure bringSubviewToFront(view: UIView); cdecl;
    function center: CGPoint; cdecl;
    function clearsContextBeforeDrawing: Boolean; cdecl;
    function clipsToBounds: Boolean; cdecl;
    function contentScaleFactor: CGFloat; cdecl;
    function contentStretch: CGRect; cdecl;
    function convertPoint(point: CGPoint; fromView: UIView): CGPoint; cdecl; overload;
    [MethodName('convertPoint:toView:')]
    function convertPointToView(point: CGPoint; toView: UIView): CGPoint; cdecl; overload;
    function convertRect(rect: CGRect; fromView: UIView): CGRect; cdecl; overload;
    function constraints: NSArray; cdecl;
    procedure didAddSubview(subview: UIView); cdecl;
    procedure didMoveToSuperview; cdecl;
    procedure didMoveToWindow; cdecl;
    procedure drawRect(rect: CGRect); cdecl; overload;
    procedure drawViewHierarchyInRectAfterScreenUpdates(rect: CGRect; afterUpdates: Boolean); cdecl;
    function endEditing(force: Boolean): Boolean; cdecl;
    procedure exchangeSubviewAtIndex(index1: NSInteger; withSubviewAtIndex: NSInteger); cdecl;
    function frame: CGRect; cdecl;
    function gestureRecognizers: NSArray; cdecl;
    function initWithFrame(frame: CGRect): Pointer; cdecl;
    procedure insertSubview(view: UIView; aboveSubview: UIView); cdecl; overload;
    procedure insertSubview(view: UIView; atIndex: NSInteger); cdecl; overload;
    function isDescendantOfView(view: UIView): Boolean; cdecl;
    function isExclusiveTouch: Boolean; cdecl;
    function isHidden: Boolean; cdecl;
    function isMultipleTouchEnabled: Boolean; cdecl;
    function isOpaque: Boolean; cdecl;
    function isUserInteractionEnabled: Boolean; cdecl;
    procedure layoutIfNeeded; cdecl;
    procedure layoutSubviews; cdecl;
    procedure removeFromSuperview; cdecl;
    procedure sendSubviewToBack(view: UIView); cdecl;
    procedure setAlpha(alpha: CGFloat); cdecl;
    procedure setAutoresizesSubviews(autoresizesSubviews: Boolean); cdecl;
    procedure setBounds(bounds: CGRect); cdecl;
    procedure setCenter(center: CGPoint); cdecl;
    procedure setClearsContextBeforeDrawing(clearsContextBeforeDrawing: Boolean); cdecl;
    procedure setClipsToBounds(clipsToBounds: Boolean); cdecl;
    procedure setContentScaleFactor(contentScaleFactor: CGFloat); cdecl;
    procedure setContentStretch(contentStretch: CGRect); cdecl;
    procedure setExclusiveTouch(exclusiveTouch: Boolean); cdecl;
    procedure setFrame(frame: CGRect); cdecl;
    procedure setGestureRecognizers(gestureRecognizers: NSArray); cdecl;
    procedure setHidden(hidden: Boolean); cdecl;
    procedure setMultipleTouchEnabled(multipleTouchEnabled: Boolean); cdecl;
    procedure setNeedsDisplay; cdecl;
    procedure setNeedsDisplayInRect(rect: CGRect); cdecl;
    procedure setNeedsLayout; cdecl;
    procedure setOpaque(opaque: Boolean); cdecl;
    procedure setTag(tag: NSInteger); cdecl;
    procedure setTransform(transform: CGAffineTransform); cdecl;
    procedure setUserInteractionEnabled(userInteractionEnabled: Boolean); cdecl;
    function sizeThatFits(size: CGSize): CGSize; cdecl;
    procedure sizeToFit; cdecl;
    function subviews: NSArray; cdecl;
    function superview: Pointer; cdecl;
    function tag: NSInteger; cdecl;
    function transform: CGAffineTransform; cdecl;
    function viewWithTag(tag: NSInteger): UIView; cdecl;
    procedure willMoveToSuperview(newSuperview: UIView); cdecl;
    procedure willRemoveSubview(subview: UIView); cdecl;
  end;
  TUIView = class(TOCGenericImport<UIViewClass, UIView>)  end;

  UIPrintPaperClass = interface(NSObjectClass)
  ['{172BD61F-F17B-470B-A4E4-E05079D0E87A}']
    {class} function bestPaperForPageSize(contentSize: CGSize; withPapersFromArray: NSArray): Pointer; cdecl;
  end;
  UIPrintPaper = interface(NSObject)
  ['{D2351A9B-804A-4FCF-AF93-30C25CCF8B21}']
    function paperSize: CGSize; cdecl;
    function printRect: CGRect; cdecl;
    function printableRect: CGRect; cdecl;
  end;
  TUIPrintPaper = class(TOCGenericImport<UIPrintPaperClass, UIPrintPaper>)  end;

  UIPrintInfoClass = interface(NSObjectClass)
    ['{260A7728-FF89-4383-9636-DDB3D31FEE75}']
    function printInfo: Pointer; cdecl;
    function printInfoWithDictionary(dictionary: NSDictionary): Pointer; cdecl;
  end;
  UIPrintInfo = interface(NSObject)
    ['{535895F1-8E39-4A16-8DF9-3507C3366D37}']
    function dictionaryRepresentation: NSDictionary; cdecl;
    function jobName: NSString; cdecl;
    function orientation: UIPrintInfoOrientation; cdecl;
    function outputType: UIPrintInfoOutputType; cdecl;
    function printerID: NSString; cdecl;
    procedure setJobName(jobName: NSString); cdecl;
    procedure setOrientation(orientation: UIPrintInfoOrientation); cdecl;
    procedure setOutputType(outputType: UIPrintInfoOutputType); cdecl;
    procedure setPrinterID(printerID: NSString); cdecl;
  end;
  TUIPrintInfo = class(TOCGenericImport<UIPrintInfoClass, UIPrintInfo>)  end;

  UIPrintInteractionControllerClass = interface(NSObjectClass)
    ['{6CDA3A1E-64B6-477F-A520-5F45B7E7262C}']
    function canPrintData(data: NSData): Boolean; cdecl;
    function canPrintURL(url: NSURL): Boolean; cdecl;
    function isPrintingAvailable: Boolean; cdecl;
    function printableUTIs: NSSet; cdecl;
    function sharedPrintController: Pointer; cdecl;
  end;
  UIPrintInteractionController = interface(NSObject)
  ['{1B17C3B9-2512-4FBF-A890-CFEF5EF1F300}']
    function delegate: Pointer; cdecl;
    procedure dismissAnimated(animated: Boolean); cdecl;
    function presentFromRect(rect: CGRect; inView: UIView; animated: Boolean; completionHandler: UIPrintInteractionCompletionHandler): Boolean; cdecl;
    function printInfo: UIPrintInfo; cdecl;
    function printPaper: UIPrintPaper; cdecl;
    function printingItem: Pointer; cdecl;
    function printingItems: NSArray; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setPrintInfo(printInfo: UIPrintInfo); cdecl;
    procedure setPrintingItem(printingItem: Pointer); cdecl;
    procedure setPrintingItems(printingItems: NSArray); cdecl;
    procedure setShowsPageRange(showsPageRange: Boolean); cdecl;
    function showsPageRange: Boolean; cdecl;
  end;
  TUIPrintInteractionController = class(TOCGenericImport<UIPrintInteractionControllerClass, UIPrintInteractionController>)  end;
{$ENDIF}

  /////////////////////////////////////////
  ///                                   ///
  ///           PRINTER CLASS           ///
  ///                                   ///
  /////////////////////////////////////////

  TTMSFNCPrinterDrawContentCallBack = {$IFNDEF LCLLIB}reference to {$ENDIF}procedure{$IFDEF LCLLIB} of object{$ENDIF};

  TTMSFNCPrinter = class
  private
  {$IFDEF ANDROIDIOSLIB}
    FDPI: Integer;
    FOrientation: TPrinterOrientation;
    FPageHeight: Integer;
    FPageNumber: Integer;
    FPageWidth: Integer;
    {$IFDEF IOS}
    FPDFData: NSMutableData;
    FPrintCompleted: Boolean;
    FPrintInfo: UIPrintInfo;
    FPrintInteractionController: UIPrintInteractionController;
    {$ENDIF}
    {$IFDEF ANDROID}
    mp: JFNCPrintDocumentAdapter;
    pa: JPrintAttributes;
    pdaListener: TTMSFNCPrintDocumentAdapterListener;
    pm: JPrintManager;
    FPrintSize: TPrintSize;
    {$ENDIF}
  {$ENDIF}
    FGraphics: TTMSFNCGraphics;
    FOnDrawContent: TTMSFNCPrinterDrawContentCallBack;
    function GetDPI: Integer;
    {$IFDEF WEBLIB}
    procedure SetPageHeight(const Value: Integer);
    procedure SetPageWidth(const Value: Integer);
    {$ENDIF}
    {$IFDEF ANDROID}
    function GetPrintJobName: string;
    procedure SetPrintAttributes;
    procedure SetPrintJobName(const Value: string);
    procedure setPrintSize(const Value: TPrintSize);
    procedure SetDPI(const Value: Integer);
    {$ENDIF}
    {$IFDEF IOS}
    procedure PrintCompletedHandler(param1: UIPrintInteractionController; param2: Boolean; param3: NSError); virtual;
    {$ENDIF}
  {$IFNDEF ANDROIDIOSWEBLIB}
    procedure SetDevice(const Value: string);
  {$ENDIF}
    function GetDevice: string;
    function GetOrientation: TPrinterOrientation;
    function GetPageHeight: Integer;
    function GetPageNumber: Integer;
    function GetPageWidth: Integer;
    procedure SetOrientation(const Value: TPrinterOrientation);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure BeginDoc;
    procedure EndDoc;
    procedure NewPage;
    property Device: string read GetDevice {$IFNDEF ANDROIDIOSWEBLIB} write SetDevice {$ENDIF};
    property DPI: Integer read GetDPI {$IFDEF ANDROID} write SetDPI {$ENDIF};
    property Graphics: TTMSFNCGraphics read FGraphics;
    property Orientation: TPrinterOrientation read GetOrientation write SetOrientation;
    property PageHeight: Integer read GetPageHeight {$IFDEF WEBLIB} write SetPageHeight {$ENDIF};
    property PageNumber: Integer read GetPageNumber;
    property PageWidth: Integer read GetPageWidth {$IFDEF WEBLIB} write SetPageWidth {$ENDIF};
    {$IFDEF IOS}
    property PrintCompleted: Boolean read FPrintCompleted write FPrintCompleted;
    {$ENDIF}
    {$IFDEF ANDROID}
    property PrintJobName: string read GetPrintJobName write SetPrintJobName;
    property PrintSize: TPrintSize read FPrintSize write setPrintSize;
    {$ENDIF}
    property OnDrawContent: TTMSFNCPrinterDrawContentCallBack read FOnDrawContent write FOnDrawContent;
  end;

function TMSFNCPrinter: TTMSFNCPrinter;

{$IFDEF IOS}
procedure UIGraphicsBeginPDFContextToData(data: PNSData; bounds: CGRect; documentInfo: PNSDictionary); cdecl; external libUIKit name _PU + 'UIGraphicsBeginPDFContextToData';
procedure UIGraphicsEndPDFContext; cdecl; external libUIKit name _PU + 'UIGraphicsEndPDFContext';
procedure UIGraphicsBeginPDFPageWithInfo(bounds: CGRect; pageInfo: PNSDictionary); cdecl; external libUIKit name _PU + 'UIGraphicsBeginPDFPageWithInfo';
{$ENDIF}

implementation

uses
  {%H-}Math, VCL.Forms, SysUtils
  {$IFDEF FMXLIB}
  {$IFDEF ANDROID}
  , VCL.TMSFNCGraphics.Android
  {$ENDIF}
  {$IFDEF IOS}
  , VCL.TMSFNCGraphics.iOS
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  , Printers.Win
  {$ENDIF}
  , System.Math.Vectors
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  , Windows
  {$ENDIF}
  {$IFDEF WEBLIB}
  , Contnrs
  {$ENDIF}
  {$IFDEF IOS}
  , Macapi.Helpers
  {$ENDIF}
  ;
var
  FTMSFNCPrinter: TTMSFNCPrinter = nil;

function TMSFNCPrinter: TTMSFNCPrinter;
begin
  if FTMSFNCPrinter = nil then
    FTMSFNCPrinter := TTMSFNCPrinter.Create;
  Result := FTMSFNCPrinter;
end;

{$IFDEF ANDROID}
function PrintSizeToJJPrintAttributes_MediaSize(ps: TPrintSize): JPrintAttributes_MediaSize;
begin
  case ps of
    psISO_A0: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_A0;
    psISO_A1: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_A1;
    psISO_A10: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_A10;
    psISO_A2: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_A2;
    psISO_A3: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_A3;
    psISO_A4: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_A4;
    psISO_A5: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_A5;
    psISO_A6: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_A6;
    psISO_A7: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_A7;
    psISO_A8: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_A8;
    psISO_A9: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_A9;
    psISO_B0: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_B0;
    psISO_B1: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_B1;
    psISO_B10: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_B10;
    psISO_B2: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_B2;
    psISO_B3: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_B3;
    psISO_B4: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_B4;
    psISO_B5: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_B5;
    psISO_B6: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_B6;
    psISO_B7: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_B7;
    psISO_B8: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_B8;
    psISO_B9: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_B9;
    psISO_C0: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_C0;
    psISO_C1: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_C1;
    psISO_C10: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_C10;
    psISO_C2: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_C2;
    psISO_C3: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_C3;
    psISO_C4: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_C4;
    psISO_C5: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_C5;
    psISO_C6: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_C6;
    psISO_C7: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_C7;
    psISO_C8: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_C8;
    psISO_C9: Result := TJPrintAttributes_MediaSize.JavaClass.ISO_C9;
    psJIS_B0: Result := TJPrintAttributes_MediaSize.JavaClass.JIS_B0;
    psJIS_B1: Result := TJPrintAttributes_MediaSize.JavaClass.JIS_B1;
    psJIS_B10: Result := TJPrintAttributes_MediaSize.JavaClass.JIS_B10;
    psJIS_B2: Result := TJPrintAttributes_MediaSize.JavaClass.JIS_B2;
    psJIS_B3: Result := TJPrintAttributes_MediaSize.JavaClass.JIS_B3;
    psJIS_B4: Result := TJPrintAttributes_MediaSize.JavaClass.JIS_B4;
    psJIS_B5: Result := TJPrintAttributes_MediaSize.JavaClass.JIS_B5;
    psJIS_B6: Result := TJPrintAttributes_MediaSize.JavaClass.JIS_B6;
    psJIS_B7: Result := TJPrintAttributes_MediaSize.JavaClass.JIS_B7;
    psJIS_B8: Result := TJPrintAttributes_MediaSize.JavaClass.JIS_B8;
    psJIS_B9: Result := TJPrintAttributes_MediaSize.JavaClass.JIS_B9;
    psJIS_EXEC: Result := TJPrintAttributes_MediaSize.JavaClass.JIS_EXEC;
    psJPN_CHOU2: Result := TJPrintAttributes_MediaSize.JavaClass.JPN_CHOU2;
    psJPN_CHOU3: Result := TJPrintAttributes_MediaSize.JavaClass.JPN_CHOU3;
    psJPN_CHOU4: Result := TJPrintAttributes_MediaSize.JavaClass.JPN_CHOU4;
    psJPN_HAGAKI: Result := TJPrintAttributes_MediaSize.JavaClass.JPN_HAGAKI;
    psJPN_KAHU: Result := TJPrintAttributes_MediaSize.JavaClass.JPN_KAHU;
    psJPN_KAKU2: Result := TJPrintAttributes_MediaSize.JavaClass.JPN_KAKU2;
    psJPN_OUFUKU: Result := TJPrintAttributes_MediaSize.JavaClass.JPN_OUFUKU;
    psJPN_YOU4: Result := TJPrintAttributes_MediaSize.JavaClass.JPN_YOU4;
    psNA_FOOLSCAP: Result := TJPrintAttributes_MediaSize.JavaClass.NA_FOOLSCAP;
    psNA_GOVT_LETTER: Result := TJPrintAttributes_MediaSize.JavaClass.NA_GOVT_LETTER;
    psNA_INDEX_3X5: Result := TJPrintAttributes_MediaSize.JavaClass.NA_INDEX_3X5;
    psNA_INDEX_4X6: Result := TJPrintAttributes_MediaSize.JavaClass.NA_INDEX_4X6;
    psNA_INDEX_5X8: Result := TJPrintAttributes_MediaSize.JavaClass.NA_INDEX_5X8;
    psNA_JUNIOR_LEGAL: Result := TJPrintAttributes_MediaSize.JavaClass.NA_JUNIOR_LEGAL;
    psNA_LEDGER: Result := TJPrintAttributes_MediaSize.JavaClass.NA_LEDGER;
    psNA_LEGAL: Result := TJPrintAttributes_MediaSize.JavaClass.NA_LEGAL;
    psNA_LETTER: Result := TJPrintAttributes_MediaSize.JavaClass.NA_LETTER;
    psNA_MONARCH: Result := TJPrintAttributes_MediaSize.JavaClass.NA_MONARCH;
    psNA_QUARTO: Result := TJPrintAttributes_MediaSize.JavaClass.NA_QUARTO;
    psNA_TABLOID: Result := TJPrintAttributes_MediaSize.JavaClass.NA_TABLOID;
    psOM_DAI_PA_KAI: Result := TJPrintAttributes_MediaSize.JavaClass.OM_DAI_PA_KAI;
    psOM_JUURO_KU_KAI: Result := TJPrintAttributes_MediaSize.JavaClass.OM_JUURO_KU_KAI;
    psOM_PA_KAI: Result := TJPrintAttributes_MediaSize.JavaClass.OM_PA_KAI;
    psPRC_1: Result := TJPrintAttributes_MediaSize.JavaClass.PRC_1;
    psPRC_10: Result := TJPrintAttributes_MediaSize.JavaClass.PRC_10;
    psPRC_16K: Result := TJPrintAttributes_MediaSize.JavaClass.PRC_16K;
    psPRC_2: Result := TJPrintAttributes_MediaSize.JavaClass.PRC_2;
    psPRC_3: Result := TJPrintAttributes_MediaSize.JavaClass.PRC_3;
    psPRC_4: Result := TJPrintAttributes_MediaSize.JavaClass.PRC_4;
    psPRC_5: Result := TJPrintAttributes_MediaSize.JavaClass.PRC_5;
    psPRC_6: Result := TJPrintAttributes_MediaSize.JavaClass.PRC_6;
    psPRC_7: Result := TJPrintAttributes_MediaSize.JavaClass.PRC_7;
    psPRC_8: Result := TJPrintAttributes_MediaSize.JavaClass.PRC_8;
    psPRC_9: Result := TJPrintAttributes_MediaSize.JavaClass.PRC_9;
    psROC_16K: Result := TJPrintAttributes_MediaSize.JavaClass.ROC_16K;
    psROC_8K: Result := TJPrintAttributes_MediaSize.JavaClass.ROC_8K;
    psUNKNOWN_LANDSCAPE: Result := TJPrintAttributes_MediaSize.JavaClass.UNKNOWN_LANDSCAPE;
    else
      Result := TJPrintAttributes_MediaSize.JavaClass.UNKNOWN_PORTRAIT;
  end;
end;
{$ENDIF}

{ TPrinter }

procedure TTMSFNCPrinter.BeginDoc;
begin

  {$IFNDEF ANDROIDIOSLIB}
  Printer.BeginDoc;

  FGraphics := TTMSFNCGraphics.Create(Printer.Canvas{$IFDEF CMNLIB}, True{$ENDIF});
  {$IFDEF FMXLIB}
  {$IFNDEF MACOS}
  FGraphics.BeginScene;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  FGraphics.Canvas.SetMatrix(TMatrix.CreateScaling(DPI / 72, DPI / 72));
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}

  {$IFDEF ANDROID}
  FGraphics := TTMSFNCGraphics.Create(nil, True);

  pm.print(StringToJString(PrintJobName), TJPrintDocumentAdapter.Wrap((mp as ILocalObject).GetObjectID), mp.getMyPrintAttributes);
  {$ENDIF}

  {$IFDEF IOS}
  FPrintCompleted := False;
  FGraphics := TTMSFNCGraphics.Create(nil, True);

  FPrintInteractionController := TUIPrintInteractionController.Wrap(TUIPrintInteractionController.OCClass.sharedPrintController);
  FPrintInteractionController.setPrintInfo(FPrintInfo);

  FPDFData := TNSMutableData.Wrap(TNSMutableData.OCClass.data);
  UIGraphicsBeginPDFContextToData(((FPDFData as ILocalObject).GetObjectID), CGRectMake(0, 0, FPageWidth, FPageHeight), nil);
  FPageNumber := 0;
  NewPage;
  {$ENDIF}

  FGraphics.BeginPrinting;

  {$IFNDEF ANDROID}
  if Assigned(OnDrawContent) then
    OnDrawContent();
  {$ENDIF}
end;

constructor TTMSFNCPrinter.Create;
begin
  {$IFDEF ANDROID}
  pm := TJPrintManager.Wrap(SharedActivityEx.getSystemService(TJContext.JavaClass.PRINT_SERVICE));
  pdaListener := TTMSFNCPrintDocumentAdapterListener.Create;
  pdaListener.FPrinter := Self;
  mp := TJFNCPrintDocumentAdapter.JavaClass.init(SharedActivityEx);
  mp.setListener(pdaListener);
  FPrintSize := psISO_A4;
  {$ENDIF}
  {$IFDEF IOS}
  FPageWidth := 595;
  FPageHeight := 842;

  FPrintInfo := TUIPrintInfo.Wrap(TUIPrintInfo.OCClass.printInfo);
  FPrintInfo.setJobName(StrToNSStr('TMSPrinterJob'));
  FPrintInfo.setOutputType(UIPrintInfoOutputGeneral);
  FPrintInfo.setOrientation(UIPrintInfoOrientationPortrait);
  {$ENDIF}
  {$IFDEF ANDROIDIOSLIB}
  FDPI := 72;
  FOrientation := TPrinterOrientation.poPortrait;
  {$ENDIF}
end;

destructor TTMSFNCPrinter.Destroy;
begin
  {$IFDEF ANDROID}
  pdaListener.Free;
  {$ENDIF}
  inherited;
end;

procedure TTMSFNCPrinter.EndDoc;
{$IFDEF IOS}
var
  v: UIView;
{$ENDIF}
begin
  {$IFNDEF ANDROIDIOSLIB}
  {$IFDEF FMXLIB}
  {$IFNDEF MACOS}
  FGraphics.EndScene;
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  FGraphics.EndPrinting;

  {$IFDEF ANDROID}
  TTMSFNCGraphicsContextAndroid(FGraphics.Context).SetNativeCanvas(nil);
  {$ENDIF}

  {$IFDEF IOS}
  UIGraphicsEndPDFContext;

  FPrintInteractionController.setPrintingItem((FPDFData as ILocalObject).GetObjectId);
  FPrintInteractionController.setShowsPageRange(True);

  v := TUIView.Create;
  FPrintInteractionController.presentFromRect(CGRectMake(0, 0, v.bounds.size.width , v.bounds.size.height), v, True, PrintCompletedHandler);

  TTMSFNCGraphicsContextiOS(FGraphics.Context).SetNativeContext(nil);
  {$ENDIF}

  FGraphics.Free;
  FGraphics := nil;

  {$IFDEF ANDROID}
  mp.endDoc;
  {$ENDIF}

  {$IFNDEF ANDROIDIOSLIB}
  Printer.EndDoc;
  {$ENDIF}
end;

function TTMSFNCPrinter.GetDevice: string;
begin
  {$IFDEF FMXLIB}
  {$IFDEF MSWINDOWS}
  Result := Printer.ActivePrinter.Device;
  {$ENDIF}
  {$IFDEF MACOS}
  Result := Printer.ActivePrinter.Device;
  {$ENDIF}
  {$IFDEF IOS}
  Result := 'iOS Print';
  {$ENDIF}
  {$IFDEF ANDROID}
  Result := 'Android Print';
  {$ENDIF}
  {$ENDIF}
  {$IFDEF CMNLIB}
  Result := Printer.Printers[Printer.PrinterIndex];
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := 'WEB Print';
  {$ENDIF}
end;

function TTMSFNCPrinter.GetDPI: Integer;
begin
  {$IFDEF FMXLIB}
  {$IFDEF UNIX}
  if Printer.ActivePrinter.ActiveDPI.Y > 0 then
    Result := Printer.ActivePrinter.ActiveDPI.Y
  else
    Result := 72;
  {$ENDIF}
  {$IFDEF MACOS}
  if Printer.ActivePrinter.ActiveDPI.Y > 0 then
    Result := Printer.ActivePrinter.ActiveDPI.Y
  else
    Result := 72;
  {$ENDIF}
  {$IFDEF ANDROID}
  if Assigned(mp) then
    Result := mp.getDPI
  else
    Result := FDPI;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  if Printer is TPrinterWin then
    Result := GetDeviceCaps(TPrinterWin(Printer).Handle, LOGPIXELSY)
  else
    Result := 72;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF LCLLIB}
  Result := Printer.YDPI;
  {$ENDIF}
  {$IFDEF VCLLIB}
  Result := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := Printer.DPI;
  {$ENDIF}
end;

function TTMSFNCPrinter.GetOrientation: TPrinterOrientation;
begin
  {$IFNDEF ANDROIDIOSLIB}
  Result := Printer.Orientation;
  {$ENDIF}
  {$IFDEF ANDROIDIOSLIB}
  Result := FOrientation;
  {$ENDIF}
end;

function TTMSFNCPrinter.GetPageHeight: Integer;
begin
  {$IFDEF ANDROID}
  if Assigned(mp) then
    Result := mp.getPageHeight
  else
    Result := -1;
  {$ENDIF}
  {$IFDEF IOS}
  Result := FPageHeight;
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := Printer.PageHeight;
  {$ENDIF}
  {$IFNDEF ANDROIDIOSWEBLIB}
  Result := Round(Printer.PageHeight * PRINTDPI / DPI);
  {$ENDIF}
end;

function TTMSFNCPrinter.GetPageNumber: Integer;
begin
  {$IFNDEF ANDROIDIOSLIB}
  Result := Printer.PageNumber;
  {$ENDIF}
  {$IFDEF IOS}
  Result := FPageNumber;
  {$ENDIF}
  {$IFDEF ANDROID}
  Result := mp.getPageNumber;
  {$ENDIF}
end;

function TTMSFNCPrinter.GetPageWidth: Integer;
begin
  {$IFDEF ANDROIDIOSLIB}
  {$IFDEF ANDROID}
  Result := mp.getPageWidth;
  {$ENDIF}
  {$IFDEF IOS}
  Result := FPageWidth;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := Printer.PageWidth;
  {$ENDIF}
  {$IFNDEF ANDROIDIOSWEBLIB}
  Result := Round(Printer.PageWidth * PRINTDPI / DPI);
  {$ENDIF}
end;

procedure TTMSFNCPrinter.NewPage;
{$IFDEF IOS}
var
  pageInfo: NSMutableDictionary;
  cc: CGContextRef;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  TTMSFNCGraphicsContextAndroid(FGraphics.Context).SetNativeCanvas(nil);
  mp.newPage;
  TTMSFNCGraphicsContextAndroid(FGraphics.Context).SetNativeCanvas(mp.getMyPageCanvas);
  {$ENDIF}
  {$IFDEF IOS}
  pageInfo := TNSMutableDictionary.Wrap(TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.alloc).init);
  UIGraphicsBeginPDFPageWithInfo(CGRectMake(0, 0, FPageWidth, FPageHeight), (pageInfo as ILocalObject).GetObjectID);
  pageInfo.release;
  TTMSFNCGraphicsContextiOS(FGraphics.Context).SetNativeContext(nil);
  cc := UIGraphicsGetCurrentContext;
  TTMSFNCGraphicsContextiOS(FGraphics.Context).SetNativeContext(cc);
  Inc(FPageNumber);
  {$ENDIF}
  {$IFNDEF ANDROIDIOSLIB}
  Printer.NewPage;
  {$ENDIF}
end;

{$IFDEF IOS}
procedure TTMSFNCPrinter.PrintCompletedHandler(param1: UIPrintInteractionController; param2: Boolean; param3: NSError);
begin
  FPrintCompleted := True;
end;
{$ENDIF}

{$IFNDEF ANDROIDIOSWEBLIB}
procedure TTMSFNCPrinter.SetDevice(const Value: string);
var
  i: integer;
begin
  {$IFDEF FMXLIB}
  for i := 0 to Printer.Count - 1 do
  begin
    if Pos(Value, Printer.Printers[i].Device) > 0 then
    begin
      Printer.ActivePrinter := Printer.Printers[i];
      break;
    end;
  end;

  Printer.ActivePrinter.SelectDPI(72,72);
  {$ENDIF}
  {$IFDEF CMNLIB}
  i := Printer.Printers.IndexOf(Value);
  if i >= 0 then
  begin
    Printer.PrinterIndex := i;
  end;
  {$ENDIF}
end;
{$ENDIF}

procedure TTMSFNCPrinter.SetOrientation(const Value: TPrinterOrientation);
{$IFDEF IOS}
var
  temp: Integer;
{$ENDIF}
begin
  {$IFDEF ANDROIDIOSLIB}
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    {$IFDEF ANDROID}
    SetPrintAttributes;
    {$ENDIF}
    {$IFDEF IOS}
    case FOrientation of
      TPrinterOrientation.poPortrait: FPrintInfo.setOrientation(UIPrintInfoOrientationPortrait);
      TPrinterOrientation.poLandscape: FPrintInfo.setOrientation(UIPrintInfoOrientationLandscape);
    end;
    temp := FPageWidth;
    FPageWidth := FPageHeight;
    FPageHeight := temp;
    {$ENDIF}
  end;
  {$ENDIF}

  {$IFNDEF ANDROIDIOSLIB}
  Printer.Orientation := Value;
  {$ENDIF}
end;

{$IFDEF ANDROID}
procedure TTMSFNCPrinter.SetDPI(const Value: Integer);
begin
  if Assigned(mp) then
    mp.setDPI(Value);
  FDPI := Value;
end;
{$ENDIF}

{$IFDEF WEBLIB}
procedure TTMSFNCPrinter.SetPageHeight(const Value: Integer);
begin
  Printer.PageHeight := Value;
end;

procedure TTMSFNCPrinter.SetPageWidth(const Value: Integer);
begin
  Printer.PageWidth := Value;
end;
{$ENDIF}

{$IFDEF ANDROID}
function TTMSFNCPrinter.GetPrintJobName: string;
begin
  if Assigned(mp) then
    Result := JStringToString(mp.getPrintName)
  else
    Result := 'TMSPrintJob';
end;

procedure TTMSFNCPrinter.SetPrintAttributes;
var
  pab: JPrintAttributes_Builder;
begin
  pab := TJPrintAttributes_Builder.JavaClass.init;
  if FOrientation = TPrinterOrientation.poPortrait then
    pab.setMediaSize(PrintSizeToJJPrintAttributes_MediaSize(FPrintSize))
  else
    pab.setMediaSize(PrintSizeToJJPrintAttributes_MediaSize(FPrintSize).asLandscape);

  mp.setMyPrintAttributes(pab.build);
end;

procedure TTMSFNCPrinter.SetPrintJobName(const Value: string);
begin
  if Assigned(mp) then
    mp.setPrintName(StringToJString(Value));
end;

procedure TTMSFNCPrinter.setPrintSize(const Value: TPrintSize);
begin
  if FPrintSize <> Value then
  begin
    FPrintSize := Value;
    SetPrintAttributes
  end;
end;
{$ENDIF}

{ TTMSFNCPrintDocumentAdapterListener }

{$IFDEF ANDROID}
procedure TTMSFNCPrintDocumentAdapterListener.onWriteContent;
begin
  if not Assigned(FPrinter.FGraphics) then
    FPrinter.FGraphics := TTMSFNCGraphics.Create(nil, True);

  TTMSFNCGraphicsContextAndroid(FPrinter.Graphics.Context).SetNativeCanvas(FPrinter.mp.getMyPageCanvas);
  if Assigned(FPrinter) then
  begin
    TThread.Queue(TThread.CurrentThread,
    procedure
    begin
      if Assigned(FPrinter.OnDrawContent) then
        FPrinter.OnDrawContent();
    end);
  end;
end;
{$ENDIF}

{$IFNDEF WEBLIB}
initialization
begin

end;

finalization
begin
  if Assigned(FTMSFNCPrinter) then
  begin
    FTMSFNCPrinter.Free;
    FTMSFNCPrinter := nil;
  end;
end;
{$ENDIF}

end.
