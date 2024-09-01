{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LCLTMSFNCCorePkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  LCLTMSFNCCloudBase, LCLTMSFNCCustomControl, LCLTMSFNCGraphics, 
  LCLTMSFNCHTMLEngine, LCLTMSFNCStyles, LCLTMSFNCUtils, LCLTMSFNCTypes, 
  LCLTMSFNCXPVS, LCLTMSFNCBitmapContainerReg, LCLTMSFNCCustomComponent, 
  LCLTMSFNCCustomScrollControl, LCLTMSFNCScrollBar, LCLTMSFNCScrollBarReg, 
  LCLTMSFNCURLBitmapContainer, LCLTMSFNCGDIPlusApi, LCLTMSFNCGDIPlusClasses, 
  LCLTMSFNCGraphicsGeneral, LCLTMSFNCGraphicsTypes, LCLTMSFNCGraphicsWin, 
  LCLTMSFNCGraphicsUnix, LCLTMSFNCJSONReader, LCLTMSFNCJSONWriter, 
  LCLTMSFNCPersistence, LCLTMSFNCUndo, LCLTMSFNCHint, LCLTMSFNCHintReg, 
  LCLTMSFNCPopup, LCLTMSFNCPopupReg, LCLTMSFNCGraphicsPDFEngine, 
  LCLTMSFNCGraphicsPDFIOReg, LCLTMSFNCPDFCoreLibBase, LCLTMSFNCPDFGraphicsLib, 
  LCLTMSFNCPDFGraphicsLibGeneral, LCLTMSFNCPDFGraphicsLibHTMLEngine, 
  LCLTMSFNCPDFIO, LCLTMSFNCPDFLib, LCLTMSFNCPDFLibGeneral, 
  LCLTMSFNCPDFLibGeneralDefault, LCLTMSFNCPDFLibGeneralUnix, 
  LCLTMSFNCPDFLibGeneralWin, LCLTMSFNCPDFLibReg, LCLTMSFNCPDFRichTextLib, 
  LCLTMSFNCPDFRichTextLibGeneral, LCLTMSFNCGraphicsSVGEngine, 
  LCLTMSFNCWebBrowser, LCLTMSFNCWebBrowserReg, LCLTMSFNCWebBrowserWin, 
  LCLTMSFNCWebBrowserUnix, LCLTMSFNCCloudBaseUnix, LCLTMSFNCCloudBaseWin, 
  LCLTMSFNCStylesReg, LCLTMSFNCStylesEditor, LCLTMSFNCDataBinding, 
  LCLTMSFNCDataBindingEditor, LCLTMSFNCDataBindingReg, LCLTMSFNCGraphicsTools, 
  LCLTMSFNCBitmapEditor, LCLTMSFNCCustomWEBControl, 
  LCLTMSFNCGraphicsAppearanceEditor, LCLTMSFNCEditorPanel, LCLTMSFNCPrinters, 
  LCLTMSFNCPrintIO, LCLTMSFNCPrintIOReg, LCLTMSFNCCustomWEBComponent, 
  LCLTMSFNCBitmapContainerEditor, LCLTMSFNCEditorButton, 
  LCLTMSFNCEditorListView, LCLTMSFNCResponsiveManager, LCLTMSFNCStateManager, 
  LCLTMSFNCResponsiveManagerReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LCLTMSFNCBitmapContainerReg', 
    @LCLTMSFNCBitmapContainerReg.Register);
  RegisterUnit('LCLTMSFNCScrollBarReg', @LCLTMSFNCScrollBarReg.Register);
  RegisterUnit('LCLTMSFNCHintReg', @LCLTMSFNCHintReg.Register);
  RegisterUnit('LCLTMSFNCPopupReg', @LCLTMSFNCPopupReg.Register);
  RegisterUnit('LCLTMSFNCGraphicsPDFIOReg', @LCLTMSFNCGraphicsPDFIOReg.Register
    );
  RegisterUnit('LCLTMSFNCPDFLibReg', @LCLTMSFNCPDFLibReg.Register);
  RegisterUnit('LCLTMSFNCWebBrowserReg', @LCLTMSFNCWebBrowserReg.Register);
  RegisterUnit('LCLTMSFNCStylesReg', @LCLTMSFNCStylesReg.Register);
  RegisterUnit('LCLTMSFNCDataBindingReg', @LCLTMSFNCDataBindingReg.Register);
  RegisterUnit('LCLTMSFNCPrintIOReg', @LCLTMSFNCPrintIOReg.Register);
  RegisterUnit('LCLTMSFNCResponsiveManagerReg', 
    @LCLTMSFNCResponsiveManagerReg.Register);
end;

initialization
  RegisterPackage('LCLTMSFNCCorePkg', @Register);
end.
