// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCUtils.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncutilsHPP
#define Vcl_TmsfncutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <Vcl.Graphics.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ImgList.hpp>
#include <System.Rtti.hpp>
#include <System.Generics.Collections.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Imaging.jpeg.hpp>
#include <Vcl.Imaging.pngimage.hpp>
#include <Vcl.Imaging.GIFImg.hpp>
#include <Vcl.Dialogs.hpp>
#include <System.JSON.hpp>

//-- user supplied -----------------------------------------------------------

#pragma link "wininet.a"


namespace Vcl
{
namespace Tmsfncutils
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE TTMSFNCUtilsLoadFileCompleteEvent;
typedef System::DelphiInterface<TTMSFNCUtilsLoadFileCompleteEvent> _di_TTMSFNCUtilsLoadFileCompleteEvent;
__interface DELPHIINTERFACE TTMSFNCUtilsFileCallBackEvent;
typedef System::DelphiInterface<TTMSFNCUtilsFileCallBackEvent> _di_TTMSFNCUtilsFileCallBackEvent;
class DELPHICLASS TTMSFNCUtils;
struct TTMSFNCComponentHelper /* Helper for class 'System::Classes::TComponent*' */;
struct TTMSFNCControlHelper /* Helper for class 'Vcl::Controls::TControl*' */;
class DELPHICLASS TTMSFNCClipBoard;
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<System::WideChar> TTMSFNCUtilsCharSet;

typedef System::UnicodeString TTMSFNCUtilsFile;

typedef System::UnicodeString TTMSFNCUtilsLoadFile;

__interface TTMSFNCUtilsLoadFileCompleteEvent  : public System::IInterface 
{
	virtual void __fastcall Invoke(const System::UnicodeString AFile) = 0 ;
};

__interface TTMSFNCUtilsFileCallBackEvent  : public System::IInterface 
{
	virtual void __fastcall Invoke(const System::UnicodeString AFile, const bool AResult) = 0 ;
};

class PASCALIMPLEMENTATION TTMSFNCUtils : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	__classmethod void __fastcall GetTimeZoneInfo(int &ABias, int &ABiasDay, bool &AIsDaylight);
	
public:
	template<typename T> __classmethod T __fastcall IfElse(bool ACondition, T AResultTRUE, T AResultFALSE);
	__classmethod void __fastcall OpenURL(System::UnicodeString AURL);
	__classmethod void __fastcall OpenFile(System::UnicodeString AFile, Vcl::Controls::TControl* AControlReference = (Vcl::Controls::TControl*)(0x0));
	__classmethod void __fastcall GetFonts(System::Classes::TStringList* FontList);
	__classmethod void __fastcall SetFontSize(Vcl::Graphics::TFont* AFont, float ASize, float AScaleFactor = 1.000000E+00f);
	__classmethod void __fastcall ScaleForCurrentDPI(Vcl::Forms::TCustomForm* AForm);
	__classmethod void __fastcall ScaleForDPI(Vcl::Controls::TControl* AControl, int ADPI);
	__classmethod NativeUInt __fastcall GetHInstance();
	__classmethod bool __fastcall IsHTMLUnicode(System::UnicodeString AValue);
	__classmethod bool __fastcall IsHTML(System::UnicodeString AValue);
	__classmethod System::UnicodeString __fastcall GetDocumentsPath();
	__classmethod System::UnicodeString __fastcall GetAppPath();
	__classmethod System::UnicodeString __fastcall GetTempPath();
	__classmethod System::Types::TPointF __fastcall GetMousePos();
	__classmethod bool __fastcall IsHighDPIScale(System::Classes::TComponent* AOwner = (System::Classes::TComponent*)(0x0), int ADefaultDPI = 0xffffffff);
	__classmethod float __fastcall GetDPIScale(System::Classes::TComponent* AOwner = (System::Classes::TComponent*)(0x0), int ADefaultDPI = 0xffffffff);
	__classmethod System::UnicodeString __fastcall AddBackslash(const System::UnicodeString AValue);
	__classmethod System::UnicodeString __fastcall AddForwardslash(const System::UnicodeString AValue);
	__classmethod System::Classes::TResourceStream* __fastcall GetResourceStream(System::UnicodeString AResourceName)/* overload */;
	__classmethod System::Classes::TResourceStream* __fastcall GetResourceStream(System::UnicodeString AResourceName, NativeUInt AInstance)/* overload */;
	__classmethod System::Classes::TComponent* __fastcall Clone(System::Classes::TComponent* AComponent);
	__classmethod System::UnicodeString __fastcall HTMLStrip(System::UnicodeString AHTML);
	__classmethod int __fastcall VarPos(System::UnicodeString ASubValue, System::UnicodeString AValue, int &AResult);
	__classmethod int __fastcall VarPosNoCase(System::UnicodeString ASubValue, System::UnicodeString AValue, int &AResult);
	__classmethod System::WideChar __fastcall CharInStr(System::UnicodeString s, int Index);
	__classmethod System::TArray__1<System::Byte> __fastcall HexStrToBytes(const System::UnicodeString AValue);
	__classmethod System::UnicodeString __fastcall URLDecode(const System::UnicodeString AURL, const bool ATrim = true);
	__classmethod System::UnicodeString __fastcall URLEncode(const System::UnicodeString AURL);
	__classmethod System::UnicodeString __fastcall EnDeCrypt(const System::UnicodeString AValue);
	__classmethod System::UnicodeString __fastcall SaveBitmapToHexStr(Vcl::Graphics::TPicture* const ABitmap);
	__classmethod System::UnicodeString __fastcall SaveStreamToHexStr(System::Classes::TStream* const AStream);
	__classmethod System::UnicodeString __fastcall SaveBitmapToBase64(Vcl::Graphics::TPicture* const ABitmap, const bool AURL = false);
	__classmethod System::UnicodeString __fastcall SaveStreamToBase64(System::Classes::TStream* const AStream, const bool AURL = false);
	__classmethod Vcl::Tmsfnctypes::TTMSFNCBitmap* __fastcall CreateBitmapFromHexStr(const System::UnicodeString AHexStr);
	__classmethod Vcl::Tmsfnctypes::TTMSFNCBitmap* __fastcall CreateBitmapFromBase64(const System::UnicodeString ABase64, const bool AURL = false);
	__classmethod int __fastcall MulDivInt(int nNumber, int nNumerator, int nDenominator)/* overload */;
	__classmethod float __fastcall MulDivSingle(float nNumber, float nNumerator, float nDenominator)/* overload */;
	__classmethod System::UnicodeString __fastcall FormatBytesAsString(System::Extended ASize);
	__classmethod System::UnicodeString __fastcall GetImageType(System::Classes::TStream* AData, bool AResetPosition = true)/* overload */;
	__classmethod System::UnicodeString __fastcall GetImageType(Vcl::Tmsfnctypes::TTMSFNCBitmap* ABitmap, bool AResetPosition = true)/* overload */;
	__classmethod void __fastcall LoadBitmapFromImageList(Vcl::Tmsfnctypes::TTMSFNCBitmap* ABitmap, Vcl::Imglist::TCustomImageList* AImageList, int AIndex);
	__classmethod void __fastcall LoadBitmapFromHexStr(const System::UnicodeString AHexStr, Vcl::Graphics::TPicture* const ABitmap);
	__classmethod void __fastcall LoadStreamFromHexStr(const System::UnicodeString AHexStr, System::Classes::TStream* const AStream);
	__classmethod void __fastcall LoadBitmapFromBase64(const System::UnicodeString ABase64, Vcl::Graphics::TPicture* const ABitmap, const bool AURL = false);
	__classmethod void __fastcall LoadStreamFromBase64(const System::UnicodeString ABase64, System::Classes::TStream* const AStream, const bool AURL = false);
	__classmethod System::Classes::TMemoryStream* __fastcall ConvertBitmapToJPEGStream(Vcl::Graphics::TPicture* ABitmap, float AQuality = 1.000000E+00f, System::Uitypes::TColor ABackgroundColor = (System::Uitypes::TColor)(0xffffff));
	__classmethod bool __fastcall FindGraphicClass(const void *Buffer, const __int64 BufferSize, /* out */ Vcl::Graphics::TGraphicClass &GraphicClass);
	__classmethod void __fastcall Split(const System::WideChar ADelimiter, System::UnicodeString AInput, System::Classes::TStrings* const AStrings, const bool AClearEmptyStrings = false);
	__classmethod void __fastcall Log(const System::UnicodeString AMessage);
	__classmethod System::TDateTime __fastcall EpochToDateTime(const System::UnicodeString AEpoch);
	__classmethod void __fastcall SplitURL(System::UnicodeString AURL, System::UnicodeString &AHost, System::UnicodeString &APath, System::UnicodeString &AQuery, int &APort);
	__classmethod void __fastcall PlayAudio(System::UnicodeString AAudioBase64);
	__classmethod System::UnicodeString __fastcall DateTimeToISO(const System::TDateTime ADateTime, bool AAddDelimiters = false, bool AAddMilliSeconds = false);
	__classmethod System::UnicodeString __fastcall DateToISO(const System::TDate ADate, bool AAddDelimiters = false);
	__classmethod System::TDateTime __fastcall ISOToDateTime(const System::UnicodeString AISOString, bool AIsUTC = false);
	__classmethod System::TDate __fastcall ISOToDate(const System::UnicodeString AISOString);
	__classmethod System::UnicodeString __fastcall DateTimeToUTC(const System::TDateTime ADateTime);
	__classmethod __int64 __fastcall DateTimeToMilliSeconds(const System::TDateTime ADateTime);
	__classmethod System::UnicodeString __fastcall GetMimeType(System::UnicodeString AFile)/* overload */;
	__classmethod System::UnicodeString __fastcall GetMimeType(System::Classes::TStream* AData, bool AResetPosition = true)/* overload */;
	__classmethod System::UnicodeString __fastcall GetFileExtension(System::UnicodeString AMime);
	__classmethod System::UnicodeString __fastcall FileToBase64(const System::UnicodeString AFile);
	__classmethod void __fastcall LoadFile(const System::UnicodeString AFile, const _di_TTMSFNCUtilsLoadFileCompleteEvent ALoadFileComplete);
	__classmethod System::UnicodeString __fastcall Decode64(const System::UnicodeString AValue, const bool AURL = false)/* overload */;
	__classmethod System::UnicodeString __fastcall Encode64(const System::UnicodeString AValue, const bool AURL = false)/* overload */;
	__classmethod System::UnicodeString __fastcall Encode64(const System::TArray__1<System::Byte> AValue, const bool AURL = false)/* overload */;
	__classmethod System::UnicodeString __fastcall GetJSONProp(System::Json::TJSONValue* AJSONValue, System::UnicodeString APropertyName);
	__classmethod bool __fastcall GetJSONValueAsBoolean(System::Json::TJSONValue* AJSONValue);
	__classmethod System::UnicodeString __fastcall GetJSONValueAsString(System::Json::TJSONValue* AJSONValue);
	__classmethod System::Json::TJSONValue* __fastcall GetJSONValue(System::Json::TJSONValue* AJSONValue, System::UnicodeString APropertyName);
	__classmethod System::Json::TJSONValue* __fastcall FindJSONValue(System::Json::TJSONValue* AJSONValue, System::UnicodeString APath);
	__classmethod double __fastcall GetJSONDoubleValue(System::Json::TJSONValue* AJSONValue, System::UnicodeString APropertyName);
	__classmethod double __fastcall GetJSONValueAsDouble(System::Json::TJSONValue* AJSONValue);
	__classmethod int __fastcall GetJSONIntegerValue(System::Json::TJSONValue* AJSONValue, System::UnicodeString APropertyName);
	__classmethod int __fastcall GetJSONValueAsInteger(System::Json::TJSONValue* AJSONValue);
	__classmethod bool __fastcall IsJSONTrue(System::Json::TJSONValue* AJSONValue);
	__classmethod bool __fastcall IsJSONFalse(System::Json::TJSONValue* AJSONValue);
	__classmethod int __fastcall GetJSONArraySize(System::Json::TJSONArray* AJSONArray);
	__classmethod System::Json::TJSONValue* __fastcall GetJSONArrayItem(System::Json::TJSONArray* AJSONArray, int AIndex);
	__classmethod int __fastcall GetJSONObjectSize(System::Json::TJSONObject* AJSONObject);
	__classmethod System::Json::TJSONValue* __fastcall GetJSONObjectItem(System::Json::TJSONObject* AJSONObject, int AIndex);
	__classmethod System::UnicodeString __fastcall GetJSONObjectName(System::Json::TJSONObject* AJSONObject, int AIndex);
	__classmethod System::Json::TJSONValue* __fastcall ParseJSON(System::UnicodeString AJSON);
	__classmethod bool __fastcall IsJSON(System::UnicodeString AJSON);
	__classmethod double __fastcall StrToFloatDot(System::UnicodeString AValue);
	__classmethod System::UnicodeString __fastcall FloatToStrDot(double AValue);
	__classmethod bool __fastcall TryStrToFloatDot(System::UnicodeString AValue, double &AResult);
	__classmethod System::TArray__1<System::Byte> __fastcall Decode64ToBytes(const System::UnicodeString AValue, const bool AURL = false)/* overload */;
	__classmethod bool __fastcall MatchStrEx(System::UnicodeString AValue1, System::UnicodeString AValue2, bool ACaseSensitive);
	__classmethod bool __fastcall MatchStr(System::UnicodeString AValue1, System::UnicodeString AValue2, bool ACaseSensitive);
	__classmethod System::UnicodeString __fastcall StripLogicSpaces(System::UnicodeString AValue);
	__classmethod System::WideChar __fastcall FirstChar(TTMSFNCUtilsCharSet ACharset, System::UnicodeString AValue, int &spos);
	__classmethod bool __fastcall CharInSet(System::WideChar ch, TTMSFNCUtilsCharSet ACharSet)/* overload */;
	__classmethod TTMSFNCUtilsCharSet __fastcall AddCharSet(TTMSFNCUtilsCharSet ACharSet1, TTMSFNCUtilsCharSet ACharSet2);
	__classmethod TTMSFNCUtilsCharSet __fastcall SubCharSet(TTMSFNCUtilsCharSet ACharSet1, TTMSFNCUtilsCharSet ACharSet2);
	__classmethod bool __fastcall IsLetterOrDigit(System::WideChar AChar);
	__classmethod TTMSFNCUtilsCharSet __fastcall AlphaCharSet();
	__classmethod TTMSFNCUtilsCharSet __fastcall NumericCharSet();
	__classmethod TTMSFNCUtilsCharSet __fastcall CreateCharSet(System::UnicodeString AValue);
	__classmethod bool __fastcall IsDate(System::UnicodeString AValue, System::TDateTime &ADate);
	__classmethod int __fastcall IPos(System::UnicodeString su, System::UnicodeString s);
	__classmethod System::UnicodeString __fastcall UnFixMarkup(System::UnicodeString su, bool SpecialChars = true, bool SpecialCharsOnly = false);
	__classmethod System::UnicodeString __fastcall FixMarkup(System::UnicodeString su, bool SpecialChars = true);
	__classmethod bool __fastcall TagReplaceString(const System::UnicodeString Srch, const System::UnicodeString Repl, System::UnicodeString &Dest);
	__classmethod bool __fastcall Matches(System::WideChar * s0a, System::WideChar * s1a)/* overload */;
	__classmethod bool __fastcall Matches(System::UnicodeString s0a, System::UnicodeString s1a)/* overload */;
	__classmethod System::UnicodeString __fastcall StripThousandSep(System::UnicodeString ps);
	__classmethod int __fastcall ClosingParenthesis(System::UnicodeString s1);
	__classmethod int __fastcall GetMarkupIndex(const System::UnicodeString Markup);
	__classmethod System::UnicodeString __fastcall GetSpecialChar(const int Value);
	__classmethod int __fastcall IndexOfTextInArray(const System::UnicodeString AText, const System::UnicodeString *AValues, const int AValues_High);
	__classmethod Vcl::Forms::TCustomForm* __fastcall GetParentForm(Vcl::Controls::TControl* AControl);
	__classmethod Vcl::Forms::TCustomForm* __fastcall GetOwnerForm(System::Classes::TComponent* AComponent);
	__classmethod int __fastcall Message(const System::UnicodeString AMessage)/* overload */;
	__classmethod int __fastcall Message(const System::UnicodeString AMessage, const System::Uitypes::TMsgDlgType ADialogType, const System::Uitypes::TMsgDlgButtons AButtons, const int AHelpContext)/* overload */;
	__classmethod bool __fastcall SelectFile(System::UnicodeString &AFileName, const System::UnicodeString ADefaultFolder = System::UnicodeString(), const System::UnicodeString AFilter = System::UnicodeString(), const _di_TTMSFNCUtilsFileCallBackEvent ACallBack = _di_TTMSFNCUtilsFileCallBackEvent());
	__classmethod bool __fastcall SaveFile(System::UnicodeString &AFileName, const System::UnicodeString AFilter = System::UnicodeString(), const _di_TTMSFNCUtilsFileCallBackEvent ACallBack = _di_TTMSFNCUtilsFileCallBackEvent());
	__classmethod System::UnicodeString __fastcall EscapeString(const System::UnicodeString AValue);
	__classmethod System::UnicodeString __fastcall UnescapeString(const System::UnicodeString AValue);
	__classmethod System::Classes::TMemoryStream* __fastcall DownloadImage(System::UnicodeString AURL);
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Vcl::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
public:
	/* TObject.Create */ inline __fastcall TTMSFNCUtils() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCUtils() { }
	
};


enum DECLSPEC_DENUM TTMSFNCClipBoardFormat : unsigned char { cfText, cfRTF, cfHTML, cfBitmap, cfBitmapWin, cfStream, cfRichTextStream };

class PASCALIMPLEMENTATION TTMSFNCClipBoard : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetFormat(TTMSFNCClipBoardFormat AFormat);
	__classmethod virtual bool __fastcall HasFormat(TTMSFNCClipBoardFormat AFormat)/* overload */;
	__classmethod virtual bool __fastcall HasContent();
	__classmethod virtual void __fastcall Clear();
	__classmethod virtual void __fastcall SetText(System::UnicodeString AText);
	__classmethod virtual System::UnicodeString __fastcall GetText();
	__classmethod virtual void __fastcall SetStream(System::Classes::TMemoryStream* AStream);
	__classmethod virtual System::Classes::TMemoryStream* __fastcall GetStream();
	__classmethod virtual void __fastcall SetBitmap(Vcl::Tmsfnctypes::TTMSFNCBitmap* ABitmap);
	__classmethod virtual Vcl::Tmsfnctypes::TTMSFNCBitmap* __fastcall GetBitmap();
	__classmethod virtual void __fastcall SetRichTextStream(System::Classes::TMemoryStream* AStream);
	__classmethod virtual System::Classes::TMemoryStream* __fastcall GetRichTextStream();
	__classmethod virtual void __fastcall SetRTF(System::UnicodeString ARTF);
	__classmethod virtual System::UnicodeString __fastcall GetRTF();
	__classmethod virtual void __fastcall SetHTML(System::UnicodeString AHTML);
	__classmethod virtual System::UnicodeString __fastcall GetHTML();
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Vcl::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
public:
	/* TObject.Create */ inline __fastcall TTMSFNCClipBoard() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCClipBoard() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::WideChar PthDel = (System::WideChar)(0x5c);
static const System::Int8 FPC_FULLVERSION = System::Int8(0x0);
extern DELPHI_PACKAGE System::Word CF_FNCSTREAM;
extern DELPHI_PACKAGE System::Word CF_FNCRICHTEXTSTREAM;
extern DELPHI_PACKAGE System::Word CF_FNCRTF;
extern DELPHI_PACKAGE System::Word CF_FNCBITMAP;
extern DELPHI_PACKAGE System::Word CF_FNCHTML;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ExtractFileNameEx(System::UnicodeString AFile);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetMimeTypeEx(System::UnicodeString AFile);
}	/* namespace Tmsfncutils */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCUTILS)
using namespace Vcl::Tmsfncutils;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncutilsHPP
