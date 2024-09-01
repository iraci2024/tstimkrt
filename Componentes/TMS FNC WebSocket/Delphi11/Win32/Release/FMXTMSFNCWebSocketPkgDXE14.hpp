// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMXTMSFNCWebSocketPkgDXE14.dpk' rev: 35.00 (Windows)

#ifndef Fmxtmsfncwebsocketpkgdxe14HPP
#define Fmxtmsfncwebsocketpkgdxe14HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// (rtl)
#include <SysInit.hpp>
#include <FMX.TMSFNCWebSocketClient.hpp>
#include <FMX.TMSFNCWebSocketClientReg.hpp>
#include <FMX.TMSFNCWebSocketCommon.hpp>
#include <FMX.TMSFNCWebSocketServer.hpp>
#include <FMX.TMSFNCWebSocketServerReg.hpp>
#include <FMX.TMSFNCWhatsAppReceiver.hpp>
#include <FMX.TMSFNCWhatsAppReceiverReg.hpp>
#include <FMX.TMSFNCWhatsAppServer.hpp>
#include <FMX.TMSFNCWhatsAppServerReg.hpp>
#include <System.UITypes.hpp>	// (rtl)
#include <Winapi.Windows.hpp>	// (rtl)
#include <Winapi.PsAPI.hpp>	// (rtl)
#include <System.Character.hpp>	// (rtl)
#include <System.Internal.ExcUtils.hpp>	// (rtl)
#include <System.SysUtils.hpp>	// (rtl)
#include <System.VarUtils.hpp>	// (rtl)
#include <System.Variants.hpp>	// (rtl)
#include <System.TypInfo.hpp>	// (rtl)
#include <System.Math.hpp>	// (rtl)
#include <System.Generics.Defaults.hpp>	// (rtl)
#include <System.Rtti.hpp>	// (rtl)
#include <System.TimeSpan.hpp>	// (rtl)
#include <System.Classes.hpp>	// (rtl)
#include <System.SyncObjs.hpp>	// (rtl)
#include <System.DateUtils.hpp>	// (rtl)
#include <IdGlobal.hpp>	// (IndySystem)
#include <System.AnsiStrings.hpp>	// (rtl)
#include <IdWinsock2.hpp>	// (IndySystem)
#include <IdWship6.hpp>	// (IndySystem)
#include <IdIDN.hpp>	// (IndySystem)
#include <IdStackWindows.hpp>	// (IndySystem)
#include <IdStack.hpp>	// (IndySystem)
#include <IdComponent.hpp>	// (IndySystem)
#include <IdIOHandler.hpp>	// (IndyCore)
#include <IdIOHandlerStack.hpp>	// (IndyCore)
#include <IdFIPS.hpp>	// (IndyProtocols)
#include <Winapi.ShellAPI.hpp>	// (rtl)
#include <System.IOUtils.hpp>	// (rtl)
#include <System.IniFiles.hpp>	// (rtl)
#include <System.Win.Registry.hpp>	// (rtl)
#include <IdGlobalProtocols.hpp>	// (IndyProtocols)
#include <IdSSLOpenSSLHeaders.hpp>	// (IndyProtocols)
#include <IdThread.hpp>	// (IndyCore)
#include <IdSSL.hpp>	// (IndyProtocols)
#include <IdSSLOpenSSL.hpp>	// (IndyProtocols)
#include <IdCoderMIME.hpp>	// (IndyProtocols)
#include <IdAuthentication.hpp>	// (IndyProtocols)
#include <IdHTTPHeaderInfo.hpp>	// (IndyProtocols)
#include <System.Actions.hpp>	// (rtl)
#include <System.UIConsts.hpp>	// (rtl)
#include <System.Messaging.hpp>	// (rtl)
#include <System.Devices.hpp>	// (rtl)
#include <FMX.Utils.hpp>	// (fmx)
#include <FMX.Text.hpp>	// (fmx)
#include <FMX.TextLayout.hpp>	// (fmx)
#include <FMX.Graphics.hpp>	// (fmx)
#include <FMX.BehaviorManager.hpp>	// (fmx)
#include <FMX.DialogService.Sync.hpp>	// (fmx)
#include <FMX.Dialogs.hpp>	// (fmx)
#include <FMX.MultiResBitmap.hpp>	// (fmx)
#include <FMX.ImgList.hpp>	// (fmx)
#include <Winapi.D2D1.hpp>	// (rtl)
#include <Winapi.UxTheme.hpp>	// (rtl)
#include <System.Win.ComObj.hpp>	// (rtl)
#include <Winapi.MsCTF.hpp>	// (rtl)
#include <FMX.Helpers.Win.hpp>	// (fmx)
#include <FMX.Objects.hpp>	// (fmx)
#include <FMX.DialogService.hpp>	// (fmx)
#include <FMX.Menus.hpp>	// (fmx)
#include <FMX.Types3D.hpp>	// (fmx)
#include <Winapi.GDIPOBJ.hpp>	// (rtl)
#include <FMX.Canvas.GDIP.hpp>	// (fmx)
#include <FMX.Printer.hpp>	// (fmx)
#include <FMX.Presentation.Factory.hpp>	// (fmx)
#include <FMX.Styles.hpp>	// (fmx)
#include <FMX.Controls.Win.hpp>	// (fmx)
#include <FMX.Presentation.Win.hpp>	// (fmx)
#include <FMX.Presentation.Win.Style.hpp>	// (fmx)
#include <FMX.Styles.Objects.hpp>	// (fmx)
#include <FMX.Styles.Switch.hpp>	// (fmx)
#include <FMX.Switch.Style.hpp>	// (fmx)
#include <FMX.Switch.Win.hpp>	// (fmx)
#include <FMX.StdCtrls.hpp>	// (fmx)
#include <FMX.InertialMovement.hpp>	// (fmx)
#include <FMX.Filter.Custom.hpp>	// (fmx)
#include <FMX.Effects.hpp>	// (fmx)
#include <FMX.Layouts.hpp>	// (fmx)
#include <FMX.Clipboard.Win.hpp>	// (fmx)
#include <FMX.MagnifierGlass.hpp>	// (fmx)
#include <FMX.Edit.Style.hpp>	// (fmx)
#include <FMX.Edit.Win.hpp>	// (fmx)
#include <FMX.Edit.hpp>	// (fmx)
#include <FMX.DialogHelper.hpp>	// (fmx)
#include <FMX.Dialogs.Win.hpp>	// (fmx)
#include <FMX.Canvas.D2D.hpp>	// (fmx)
#include <FMX.Context.DX9.hpp>	// (fmx)
#include <FMX.Context.DX11.hpp>	// (fmx)
#include <FMX.Gestures.hpp>	// (fmx)
#include <FMX.Gestures.Win.hpp>	// (fmx)
#include <System.Win.InternetExplorer.hpp>	// (rtl)
#include <Winapi.EdgeUtils.hpp>	// (rtl)
#include <FMX.WebBrowser.Win.hpp>	// (fmx)
#include <FMX.WebBrowser.hpp>	// (fmx)
#include <FMX.ListBox.hpp>	// (fmx)
#include <FMX.DateTimeCtrls.hpp>	// (fmx)
#include <FMX.ExtCtrls.hpp>	// (fmx)
#include <FMX.Calendar.Style.hpp>	// (fmx)
#include <FMX.Calendar.hpp>	// (fmx)
#include <FMX.Pickers.hpp>	// (fmx)
#include <FMX.Platform.Win.hpp>	// (fmx)
#include <FMX.Canvas.GPU.hpp>	// (fmx)
#include <FMX.TextLayout.GPU.hpp>	// (fmx)
#include <FMX.Header.hpp>	// (fmx)
#include <FMX.Forms.hpp>	// (fmx)
#include <FMX.Ani.hpp>	// (fmx)
#include <FMX.Types.hpp>	// (fmx)
#include <FMX.Platform.hpp>	// (fmx)
#include <FMX.Controls.hpp>	// (fmx)
#include <System.JSON.hpp>	// (rtl)
#include <System.NetEncoding.hpp>	// (rtl)
#include <FMX.TMSFNCUtils.hpp>	// (FMXTMSFNCCorePkgDXE14)
#include <FMX.TMSFNCPersistence.hpp>	// (FMXTMSFNCCorePkgDXE14)
#include <FMX.TMSFNCGDIPlusApi.hpp>	// (FMXTMSFNCCorePkgDXE14)
#include <FMX.TMSFNCGDIPlusClasses.hpp>	// (FMXTMSFNCCorePkgDXE14)
#include <FMX.TMSFNCGraphicsTypes.hpp>	// (FMXTMSFNCCorePkgDXE14)
#include <FMX.TMSFNCGraphics.General.hpp>	// (FMXTMSFNCCorePkgDXE14)
#include <Xml.Win.msxmldom.hpp>	// (xmlrtl)
#include <Xml.xmldom.hpp>	// (xmlrtl)
#include <Xml.XMLSchema.hpp>	// (xmlrtl)
#include <Xml.xmlutil.hpp>	// (xmlrtl)
#include <FMX.TMSFNCStyles.hpp>	// (FMXTMSFNCCorePkgDXE14)
// PRG_EXT: .bpl
// BPI_DIR: C:\Users\Public\Documents\Embarcadero\Studio\22.0\Dcp
// OBJ_DIR: C:\Program Files (x86)\Embarcadero\Componentes\TMS FNC WebSocket\Win32\Release
// OBJ_EXT: .obj

//-- user supplied -----------------------------------------------------------

namespace Fmxtmsfncwebsocketpkgdxe14
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
}	/* namespace Fmxtmsfncwebsocketpkgdxe14 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMXTMSFNCWEBSOCKETPKGDXE14)
using namespace Fmxtmsfncwebsocketpkgdxe14;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmxtmsfncwebsocketpkgdxe14HPP
