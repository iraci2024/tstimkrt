// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCGDIPlusClasses.pas' rev: 35.00 (Windows)

#ifndef Fmx_TmsfncgdiplusclassesHPP
#define Fmx_TmsfncgdiplusclassesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.ActiveX.hpp>
#include <FMX.TMSFNCGDIPlusApi.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncgdiplusclasses
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGPRegion;
class DELPHICLASS TGPFontFamily;
class DELPHICLASS TGPFontCollection;
class DELPHICLASS TGPInstalledFontCollection;
class DELPHICLASS TGPPrivateFontCollection;
class DELPHICLASS TGPFont;
class DELPHICLASS TGPImage;
class DELPHICLASS TGPBitmap;
class DELPHICLASS TGPCustomLineCap;
class DELPHICLASS TGPCachedBitmap;
class DELPHICLASS TGPImageAttributes;
class DELPHICLASS TGPMatrix;
class DELPHICLASS TGPBrush;
class DELPHICLASS TGPSolidBrush;
class DELPHICLASS TGPTextureBrush;
class DELPHICLASS TGPLinearGradientBrush;
class DELPHICLASS TGPHatchBrush;
class DELPHICLASS TGPPen;
class DELPHICLASS TGPStringFormat;
class DELPHICLASS TGPGraphicsPath;
class DELPHICLASS TGPGraphicsPathIterator;
class DELPHICLASS TGPPathGradientBrush;
class DELPHICLASS TGPGraphics;
class DELPHICLASS TGPAdjustableArrowCap;
class DELPHICLASS TGPMetafile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGPRegion : public Fmx::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Fmx::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeRegion;
	Fmx::Tmsfncgdiplusapi::Status lastResult;
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetStatus(Fmx::Tmsfncgdiplusapi::Status status);
	void __fastcall SetNativeRegion(void * ANativeRegion);
	
public:
	__fastcall TGPRegion(void * ANativeRegion)/* overload */;
	__fastcall TGPRegion()/* overload */;
	__fastcall TGPRegion(const Fmx::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	__fastcall TGPRegion(const Fmx::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	__fastcall TGPRegion(TGPGraphicsPath* path)/* overload */;
	__fastcall TGPRegion(System::PByte regionData, int size)/* overload */;
	__fastcall TGPRegion(HRGN hRgn, int Dummy1, int Dummy2)/* overload */;
	TGPRegion* __fastcall FromHRGN(HRGN hRgn);
	__fastcall virtual ~TGPRegion();
	TGPRegion* __fastcall Clone();
	Fmx::Tmsfncgdiplusapi::Status __fastcall MakeInfinite();
	Fmx::Tmsfncgdiplusapi::Status __fastcall MakeEmpty();
	unsigned __fastcall GetDataSize();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetData(System::PByte buffer, unsigned bufferSize, PUINT sizeFilled = (PUINT)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall Intersect(const Fmx::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Intersect(const Fmx::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Intersect(TGPGraphicsPath* path)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Intersect(TGPRegion* region)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Union(const Fmx::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Union(const Fmx::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Union(TGPGraphicsPath* path)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Union(TGPRegion* region)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Xor_(const Fmx::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Xor_(const Fmx::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Xor_(TGPGraphicsPath* path)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Xor_(TGPRegion* region)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Exclude(const Fmx::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Exclude(const Fmx::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Exclude(TGPGraphicsPath* path)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Exclude(TGPRegion* region)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Complement(const Fmx::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Complement(const Fmx::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Complement(TGPGraphicsPath* path)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Complement(TGPRegion* region)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Translate(float dx, float dy)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Translate(int dx, int dy)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Transform(TGPMatrix* matrix);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetBounds(/* out */ Fmx::Tmsfncgdiplusapi::TGPRect &rect, TGPGraphics* g)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetBounds(/* out */ Fmx::Tmsfncgdiplusapi::TGPRectF &rect, TGPGraphics* g)/* overload */;
	HRGN __fastcall GetHRGN(TGPGraphics* g);
	System::LongBool __fastcall IsEmpty(TGPGraphics* g);
	System::LongBool __fastcall IsInfinite(TGPGraphics* g);
	System::LongBool __fastcall IsVisible(int x, int y, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsVisible(const Fmx::Tmsfncgdiplusapi::TGPPoint &point, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsVisible(float x, float y, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsVisible(const Fmx::Tmsfncgdiplusapi::TGPPointF &point, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsVisible(int x, int y, int width, int height, TGPGraphics* g)/* overload */;
	System::LongBool __fastcall IsVisible(const Fmx::Tmsfncgdiplusapi::TGPRect &rect, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsVisible(float x, float y, float width, float height, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsVisible(const Fmx::Tmsfncgdiplusapi::TGPRectF &rect, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsEqual(TGPRegion* region, TGPGraphics* g);
	unsigned __fastcall GetRegionScansCount(TGPMatrix* matrix);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetRegionScans(TGPMatrix* matrix, Fmx::Tmsfncgdiplusapi::PGPRectF rects, /* out */ int &count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetRegionScans(TGPMatrix* matrix, Fmx::Tmsfncgdiplusapi::PGPRect rects, /* out */ int &count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};


class PASCALIMPLEMENTATION TGPFontFamily : public Fmx::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Fmx::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeFamily;
	Fmx::Tmsfncgdiplusapi::Status lastResult;
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetStatus(Fmx::Tmsfncgdiplusapi::Status status);
	
public:
	__fastcall TGPFontFamily(void * nativeOrig, Fmx::Tmsfncgdiplusapi::Status status)/* overload */;
	__fastcall TGPFontFamily()/* overload */;
	__fastcall TGPFontFamily(System::WideString name, TGPFontCollection* fontCollection)/* overload */;
	__fastcall virtual ~TGPFontFamily();
	__classmethod TGPFontFamily* __fastcall GenericSansSerif();
	__classmethod TGPFontFamily* __fastcall GenericSerif();
	__classmethod TGPFontFamily* __fastcall GenericMonospace();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetFamilyName(/* out */ System::UnicodeString &name, System::Word language = (System::Word)(0x0));
	TGPFontFamily* __fastcall Clone();
	System::LongBool __fastcall IsAvailable();
	System::LongBool __fastcall IsStyleAvailable(int style);
	Fmx::Tmsfncgdiplusapi::UINT16 __fastcall GetEmHeight(int style);
	Fmx::Tmsfncgdiplusapi::UINT16 __fastcall GetCellAscent(int style);
	Fmx::Tmsfncgdiplusapi::UINT16 __fastcall GetCellDescent(int style);
	Fmx::Tmsfncgdiplusapi::UINT16 __fastcall GetLineSpacing(int style);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};


class PASCALIMPLEMENTATION TGPFontCollection : public Fmx::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Fmx::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeFontCollection;
	Fmx::Tmsfncgdiplusapi::Status lastResult;
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetStatus(Fmx::Tmsfncgdiplusapi::Status status);
	
public:
	__fastcall TGPFontCollection();
	__fastcall virtual ~TGPFontCollection();
	int __fastcall GetFamilyCount();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetFamilies(int numSought, /* out */ TGPFontFamily* *gpfamilies, const int gpfamilies_High, /* out */ int &numFound);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};


class PASCALIMPLEMENTATION TGPInstalledFontCollection : public TGPFontCollection
{
	typedef TGPFontCollection inherited;
	
public:
	__fastcall TGPInstalledFontCollection();
	__fastcall virtual ~TGPInstalledFontCollection();
};


class PASCALIMPLEMENTATION TGPPrivateFontCollection : public TGPFontCollection
{
	typedef TGPFontCollection inherited;
	
public:
	__fastcall TGPPrivateFontCollection();
	__fastcall virtual ~TGPPrivateFontCollection();
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddFontFile(System::WideString filename);
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddMemoryFont(void * memory, int length);
};


class PASCALIMPLEMENTATION TGPFont : public Fmx::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Fmx::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeFont;
	Fmx::Tmsfncgdiplusapi::Status lastResult;
	void __fastcall SetNativeFont(void * Font);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetStatus(Fmx::Tmsfncgdiplusapi::Status status);
	
public:
	__fastcall TGPFont(void * font, Fmx::Tmsfncgdiplusapi::Status status)/* overload */;
	__fastcall TGPFont(HDC hdc)/* overload */;
	__fastcall TGPFont(HDC hdc, Winapi::Windows::PLogFontA logfont)/* overload */;
	__fastcall TGPFont(HDC hdc, Winapi::Windows::PLogFontW logfont)/* overload */;
	__fastcall TGPFont(HDC hdc, HFONT hfont)/* overload */;
	__fastcall TGPFont(TGPFontFamily* family, float emSize, int style, Fmx::Tmsfncgdiplusapi::Unit_ unit_)/* overload */;
	__fastcall TGPFont(System::WideString familyName, float emSize, int style, Fmx::Tmsfncgdiplusapi::Unit_ unit_, TGPFontCollection* fontCollection)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetLogFontA(TGPGraphics* g, /* out */ tagLOGFONTA &logfontA);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetLogFontW(TGPGraphics* g, /* out */ tagLOGFONTW &logfontW);
	TGPFont* __fastcall Clone();
	__fastcall virtual ~TGPFont();
	System::LongBool __fastcall IsAvailable();
	int __fastcall GetStyle();
	float __fastcall GetSize();
	Fmx::Tmsfncgdiplusapi::Unit_ __fastcall GetUnit();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
	float __fastcall GetHeight(TGPGraphics* graphics)/* overload */;
	float __fastcall GetHeight(float dpi)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetFamily(TGPFontFamily* family);
};


class PASCALIMPLEMENTATION TGPImage : public Fmx::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Fmx::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeImage;
	Fmx::Tmsfncgdiplusapi::Status lastResult;
	Fmx::Tmsfncgdiplusapi::Status loadStatus;
	void __fastcall SetNativeImage(void * ANativeImage);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetStatus(Fmx::Tmsfncgdiplusapi::Status status);
	
public:
	__fastcall TGPImage(void * ANativeImage, Fmx::Tmsfncgdiplusapi::Status status)/* overload */;
	__fastcall TGPImage(System::WideString filename, System::LongBool useEmbeddedColorManagement)/* overload */;
	__fastcall TGPImage(_di_IStream stream, System::LongBool useEmbeddedColorManagement)/* overload */;
	TGPImage* __fastcall FromFile(System::WideString filename, System::LongBool useEmbeddedColorManagement = false);
	TGPImage* __fastcall FromStream(_di_IStream stream, System::LongBool useEmbeddedColorManagement = false);
	__fastcall virtual ~TGPImage();
	TGPImage* __fastcall Clone();
	Fmx::Tmsfncgdiplusapi::Status __fastcall Save(System::WideString filename, const GUID &clsidEncoder, Fmx::Tmsfncgdiplusapi::PEncoderParameters encoderParams = (Fmx::Tmsfncgdiplusapi::PEncoderParameters)(0x0))/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Save(_di_IStream stream, const GUID &clsidEncoder, Fmx::Tmsfncgdiplusapi::PEncoderParameters encoderParams = (Fmx::Tmsfncgdiplusapi::PEncoderParameters)(0x0))/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall SaveAdd(Fmx::Tmsfncgdiplusapi::PEncoderParameters encoderParams)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall SaveAdd(TGPImage* newImage, Fmx::Tmsfncgdiplusapi::PEncoderParameters encoderParams)/* overload */;
	Fmx::Tmsfncgdiplusapi::ImageType __fastcall GetType();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetPhysicalDimension(/* out */ Fmx::Tmsfncgdiplusapi::TGPSizeF &size);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetBounds(/* out */ Fmx::Tmsfncgdiplusapi::TGPRectF &srcRect, /* out */ Fmx::Tmsfncgdiplusapi::Unit_ &srcUnit);
	unsigned __fastcall GetWidth();
	unsigned __fastcall GetHeight();
	float __fastcall GetHorizontalResolution();
	float __fastcall GetVerticalResolution();
	unsigned __fastcall GetFlags();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetRawFormat(/* out */ GUID &format);
	int __fastcall GetPixelFormat();
	int __fastcall GetPaletteSize();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetPalette(Fmx::Tmsfncgdiplusapi::PColorPalette palette, int size);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetPalette(Fmx::Tmsfncgdiplusapi::PColorPalette palette);
	TGPImage* __fastcall GetThumbnailImage(unsigned thumbWidth, unsigned thumbHeight, Fmx::Tmsfncgdiplusapi::ImageAbort callback = 0x0, void * callbackData = (void *)(0x0));
	unsigned __fastcall GetFrameDimensionsCount();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetFrameDimensionsList(System::PGUID dimensionIDs, unsigned count);
	unsigned __fastcall GetFrameCount(const GUID &dimensionID);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SelectActiveFrame(const GUID &dimensionID, unsigned frameIndex);
	Fmx::Tmsfncgdiplusapi::Status __fastcall RotateFlip(Fmx::Tmsfncgdiplusapi::RotateFlipType rotateFlipType);
	unsigned __fastcall GetPropertyCount();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetPropertyIdList(unsigned numOfProperty, Winapi::Activex::PPropID list);
	unsigned __fastcall GetPropertyItemSize(unsigned propId);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetPropertyItem(unsigned propId, unsigned propSize, Fmx::Tmsfncgdiplusapi::PPropertyItem buffer);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetPropertySize(/* out */ unsigned &totalBufferSize, /* out */ unsigned &numProperties);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetAllPropertyItems(unsigned totalBufferSize, unsigned numProperties, Fmx::Tmsfncgdiplusapi::PPropertyItem allItems);
	Fmx::Tmsfncgdiplusapi::Status __fastcall RemovePropertyItem(unsigned propId);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetPropertyItem(const Fmx::Tmsfncgdiplusapi::PropertyItem &item);
	unsigned __fastcall GetEncoderParameterListSize(const GUID &clsidEncoder);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetEncoderParameterList(const GUID &clsidEncoder, unsigned size, Fmx::Tmsfncgdiplusapi::PEncoderParameters buffer);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};


class PASCALIMPLEMENTATION TGPBitmap : public TGPImage
{
	typedef TGPImage inherited;
	
public:
	__fastcall TGPBitmap(void * nativeBitmap)/* overload */;
	__fastcall TGPBitmap(System::WideString filename, System::LongBool useEmbeddedColorManagement)/* overload */;
	__fastcall TGPBitmap(_di_IStream stream, System::LongBool useEmbeddedColorManagement)/* overload */;
	HIDESBASE TGPBitmap* __fastcall FromFile(System::WideString filename, System::LongBool useEmbeddedColorManagement = false);
	HIDESBASE TGPBitmap* __fastcall FromStream(_di_IStream stream, System::LongBool useEmbeddedColorManagement = false);
	__fastcall TGPBitmap(int width, int height, int stride, int format, System::PByte scan0)/* overload */;
	__fastcall TGPBitmap(int width, int height, int format)/* overload */;
	__fastcall TGPBitmap(int width, int height, TGPGraphics* target)/* overload */;
	HIDESBASE TGPBitmap* __fastcall Clone(const Fmx::Tmsfncgdiplusapi::TGPRect &rect, int format)/* overload */;
	HIDESBASE TGPBitmap* __fastcall Clone(int x, int y, int width, int height, int format)/* overload */;
	HIDESBASE TGPBitmap* __fastcall Clone(const Fmx::Tmsfncgdiplusapi::TGPRectF &rect, int format)/* overload */;
	HIDESBASE TGPBitmap* __fastcall Clone(float x, float y, float width, float height, int format)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall LockBits(const Fmx::Tmsfncgdiplusapi::TGPRect &rect, unsigned flags, int format, /* out */ Fmx::Tmsfncgdiplusapi::BitmapData &lockedBitmapData);
	Fmx::Tmsfncgdiplusapi::Status __fastcall UnlockBits(Fmx::Tmsfncgdiplusapi::BitmapData &lockedBitmapData);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetPixel(int x, int y, /* out */ unsigned &color);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetPixel(int x, int y, unsigned color);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetResolution(float xdpi, float ydpi);
	__fastcall TGPBitmap(tagBITMAPINFO &gdiBitmapInfo, void * gdiBitmapData)/* overload */;
	__fastcall TGPBitmap(HBITMAP hbm, HPALETTE hpal)/* overload */;
	__fastcall TGPBitmap(HICON hicon, int Dummy1)/* overload */;
	__fastcall TGPBitmap(NativeUInt hInstance, System::WideString bitmapName)/* overload */;
	TGPBitmap* __fastcall FromBITMAPINFO(tagBITMAPINFO &gdiBitmapInfo, void * gdiBitmapData);
	TGPBitmap* __fastcall FromHBITMAP(HBITMAP hbm, HPALETTE hpal);
	TGPBitmap* __fastcall FromHICON(HICON hicon);
	TGPBitmap* __fastcall FromResource(NativeUInt hInstance, System::WideString bitmapName);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetHBITMAP(unsigned colorBackground, /* out */ HBITMAP &hbmReturn);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetHICON(/* out */ HICON &hicon);
public:
	/* TGPImage.Create */ inline __fastcall TGPBitmap(void * ANativeImage, Fmx::Tmsfncgdiplusapi::Status status)/* overload */ : TGPImage(ANativeImage, status) { }
	/* TGPImage.Destroy */ inline __fastcall virtual ~TGPBitmap() { }
	
};


class PASCALIMPLEMENTATION TGPCustomLineCap : public Fmx::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Fmx::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeCap;
	Fmx::Tmsfncgdiplusapi::Status lastResult;
	void __fastcall SetNativeCap(void * ANativeCap);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetStatus(Fmx::Tmsfncgdiplusapi::Status status);
	
public:
	__fastcall TGPCustomLineCap(void * ANativeCap, Fmx::Tmsfncgdiplusapi::Status status)/* overload */;
	__fastcall TGPCustomLineCap()/* overload */;
	__fastcall TGPCustomLineCap(TGPGraphicsPath* fillPath, TGPGraphicsPath* strokePath, int baseCap, float baseInset)/* overload */;
	__fastcall virtual ~TGPCustomLineCap();
	TGPCustomLineCap* __fastcall Clone();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetStrokeCap(int strokeCap);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetStrokeCaps(int startCap, int endCap);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetStrokeCaps(/* out */ int &startCap, /* out */ int &endCap);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetStrokeJoin(Fmx::Tmsfncgdiplusapi::LineJoin lineJoin);
	Fmx::Tmsfncgdiplusapi::LineJoin __fastcall GetStrokeJoin();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetBaseCap(int baseCap);
	int __fastcall GetBaseCap();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetBaseInset(float inset);
	float __fastcall GetBaseInset();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetWidthScale(float widthScale);
	float __fastcall GetWidthScale();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};


class PASCALIMPLEMENTATION TGPCachedBitmap : public Fmx::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Fmx::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeCachedBitmap;
	Fmx::Tmsfncgdiplusapi::Status lastResult;
	
public:
	__fastcall TGPCachedBitmap(TGPBitmap* bitmap, TGPGraphics* graphics);
	__fastcall virtual ~TGPCachedBitmap();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};


class PASCALIMPLEMENTATION TGPImageAttributes : public Fmx::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Fmx::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeImageAttr;
	Fmx::Tmsfncgdiplusapi::Status lastResult;
	void __fastcall SetNativeImageAttr(void * ANativeImageAttr);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetStatus(Fmx::Tmsfncgdiplusapi::Status status);
	
public:
	__fastcall TGPImageAttributes(void * imageAttr, Fmx::Tmsfncgdiplusapi::Status status)/* overload */;
	__fastcall TGPImageAttributes()/* overload */;
	__fastcall virtual ~TGPImageAttributes();
	TGPImageAttributes* __fastcall Clone();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetToIdentity(Fmx::Tmsfncgdiplusapi::ColorAdjustType type_ = (Fmx::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall Reset(Fmx::Tmsfncgdiplusapi::ColorAdjustType type_ = (Fmx::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetColorMatrix(const Fmx::Tmsfncgdiplusapi::ColorMatrix &colorMatrix, Fmx::Tmsfncgdiplusapi::ColorMatrixFlags mode = (Fmx::Tmsfncgdiplusapi::ColorMatrixFlags)(0x0), Fmx::Tmsfncgdiplusapi::ColorAdjustType type_ = (Fmx::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall ClearColorMatrix(Fmx::Tmsfncgdiplusapi::ColorAdjustType type_ = (Fmx::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetColorMatrices(const Fmx::Tmsfncgdiplusapi::ColorMatrix &colorMatrix, const Fmx::Tmsfncgdiplusapi::ColorMatrix &grayMatrix, Fmx::Tmsfncgdiplusapi::ColorMatrixFlags mode = (Fmx::Tmsfncgdiplusapi::ColorMatrixFlags)(0x0), Fmx::Tmsfncgdiplusapi::ColorAdjustType type_ = (Fmx::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall ClearColorMatrices(Fmx::Tmsfncgdiplusapi::ColorAdjustType Type_ = (Fmx::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetThreshold(float threshold, Fmx::Tmsfncgdiplusapi::ColorAdjustType type_ = (Fmx::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall ClearThreshold(Fmx::Tmsfncgdiplusapi::ColorAdjustType type_ = (Fmx::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetGamma(float gamma, Fmx::Tmsfncgdiplusapi::ColorAdjustType type_ = (Fmx::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall ClearGamma(Fmx::Tmsfncgdiplusapi::ColorAdjustType type_ = (Fmx::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetNoOp(Fmx::Tmsfncgdiplusapi::ColorAdjustType type_ = (Fmx::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall ClearNoOp(Fmx::Tmsfncgdiplusapi::ColorAdjustType Type_ = (Fmx::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetColorKey(unsigned colorLow, unsigned colorHigh, Fmx::Tmsfncgdiplusapi::ColorAdjustType type_ = (Fmx::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall ClearColorKey(Fmx::Tmsfncgdiplusapi::ColorAdjustType type_ = (Fmx::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetOutputChannel(Fmx::Tmsfncgdiplusapi::ColorChannelFlags channelFlags, Fmx::Tmsfncgdiplusapi::ColorAdjustType type_ = (Fmx::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall ClearOutputChannel(Fmx::Tmsfncgdiplusapi::ColorAdjustType type_ = (Fmx::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetOutputChannelColorProfile(System::WideString colorProfileFilename, Fmx::Tmsfncgdiplusapi::ColorAdjustType type_ = (Fmx::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall ClearOutputChannelColorProfile(Fmx::Tmsfncgdiplusapi::ColorAdjustType type_ = (Fmx::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetRemapTable(unsigned mapSize, Fmx::Tmsfncgdiplusapi::PColorMap map, Fmx::Tmsfncgdiplusapi::ColorAdjustType type_ = (Fmx::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall ClearRemapTable(Fmx::Tmsfncgdiplusapi::ColorAdjustType type_ = (Fmx::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetBrushRemapTable(unsigned mapSize, Fmx::Tmsfncgdiplusapi::PColorMap map);
	Fmx::Tmsfncgdiplusapi::Status __fastcall ClearBrushRemapTable();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetWrapMode(Fmx::Tmsfncgdiplusapi::WrapMode wrap, unsigned color = (unsigned)(0xff000000), System::LongBool clamp = false);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetAdjustedPalette(Fmx::Tmsfncgdiplusapi::PColorPalette colorPalette, Fmx::Tmsfncgdiplusapi::ColorAdjustType colorAdjustType);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};


typedef System::StaticArray<float, 6> TMatrixArray;

class PASCALIMPLEMENTATION TGPMatrix : public Fmx::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Fmx::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeMatrix;
	Fmx::Tmsfncgdiplusapi::Status lastResult;
	void __fastcall SetNativeMatrix(void * ANativeMatrix);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetStatus(Fmx::Tmsfncgdiplusapi::Status status);
	
public:
	__fastcall TGPMatrix(void * ANativeMatrix)/* overload */;
	__fastcall TGPMatrix()/* overload */;
	__fastcall TGPMatrix(float m11, float m12, float m21, float m22, float dx, float dy)/* overload */;
	__fastcall TGPMatrix(const Fmx::Tmsfncgdiplusapi::TGPRectF &rect, const Fmx::Tmsfncgdiplusapi::TGPPointF &dstplg)/* overload */;
	__fastcall TGPMatrix(const Fmx::Tmsfncgdiplusapi::TGPRect &rect, const Fmx::Tmsfncgdiplusapi::TGPPoint &dstplg)/* overload */;
	__fastcall virtual ~TGPMatrix();
	TGPMatrix* __fastcall Clone();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetElements(const TMatrixArray &m);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetElements(float m11, float m12, float m21, float m22, float dx, float dy);
	float __fastcall OffsetX();
	float __fastcall OffsetY();
	Fmx::Tmsfncgdiplusapi::Status __fastcall Reset();
	Fmx::Tmsfncgdiplusapi::Status __fastcall Multiply(TGPMatrix* matrix, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall Translate(float offX, float offY, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall Scale(float scaleX, float scaleY, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall Rotate(float angle, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall RotateAt(float angle, const Fmx::Tmsfncgdiplusapi::TGPPointF &center, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall Shear(float shearX, float shearY, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall Invert();
	Fmx::Tmsfncgdiplusapi::Status __fastcall TransformPoints(Fmx::Tmsfncgdiplusapi::PGPPointF pts, int count = 0x1)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall TransformPoints(Fmx::Tmsfncgdiplusapi::PGPPoint pts, int count = 0x1)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall TransformVectors(Fmx::Tmsfncgdiplusapi::PGPPointF pts, int count = 0x1)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall TransformVectors(Fmx::Tmsfncgdiplusapi::PGPPoint pts, int count = 0x1)/* overload */;
	System::LongBool __fastcall IsInvertible();
	System::LongBool __fastcall IsIdentity();
	System::LongBool __fastcall IsEqual(TGPMatrix* matrix);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};


class PASCALIMPLEMENTATION TGPBrush : public Fmx::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Fmx::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeBrush;
	Fmx::Tmsfncgdiplusapi::Status lastResult;
	void __fastcall SetNativeBrush(void * ANativeBrush);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetStatus(Fmx::Tmsfncgdiplusapi::Status status);
	
public:
	__fastcall TGPBrush(void * ANativeBrush, Fmx::Tmsfncgdiplusapi::Status status)/* overload */;
	__fastcall TGPBrush()/* overload */;
	__fastcall virtual ~TGPBrush();
	virtual TGPBrush* __fastcall Clone();
	Fmx::Tmsfncgdiplusapi::BrushType __fastcall GetType();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};


class PASCALIMPLEMENTATION TGPSolidBrush : public TGPBrush
{
	typedef TGPBrush inherited;
	
public:
	__fastcall TGPSolidBrush(unsigned color)/* overload */;
	__fastcall TGPSolidBrush()/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetColor(/* out */ unsigned &color);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetColor(unsigned color);
public:
	/* TGPBrush.Create */ inline __fastcall TGPSolidBrush(void * ANativeBrush, Fmx::Tmsfncgdiplusapi::Status status)/* overload */ : TGPBrush(ANativeBrush, status) { }
	/* TGPBrush.Destroy */ inline __fastcall virtual ~TGPSolidBrush() { }
	
};


class PASCALIMPLEMENTATION TGPTextureBrush : public TGPBrush
{
	typedef TGPBrush inherited;
	
public:
	__fastcall TGPTextureBrush(TGPImage* image, Fmx::Tmsfncgdiplusapi::WrapMode wrapMode)/* overload */;
	__fastcall TGPTextureBrush(TGPImage* image, Fmx::Tmsfncgdiplusapi::WrapMode wrapMode, const Fmx::Tmsfncgdiplusapi::TGPRectF &dstRect)/* overload */;
	__fastcall TGPTextureBrush(TGPImage* image, const Fmx::Tmsfncgdiplusapi::TGPRectF &dstRect, TGPImageAttributes* imageAttributes)/* overload */;
	__fastcall TGPTextureBrush(TGPImage* image, const Fmx::Tmsfncgdiplusapi::TGPRect &dstRect, TGPImageAttributes* imageAttributes)/* overload */;
	__fastcall TGPTextureBrush(TGPImage* image, Fmx::Tmsfncgdiplusapi::WrapMode wrapMode, const Fmx::Tmsfncgdiplusapi::TGPRect &dstRect)/* overload */;
	__fastcall TGPTextureBrush(TGPImage* image, Fmx::Tmsfncgdiplusapi::WrapMode wrapMode, float dstX, float dstY, float dstWidth, float dstHeight)/* overload */;
	__fastcall TGPTextureBrush(TGPImage* image, Fmx::Tmsfncgdiplusapi::WrapMode wrapMode, int dstX, int dstY, int dstWidth, int dstHeight)/* overload */;
	__fastcall TGPTextureBrush()/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetTransform(TGPMatrix* matrix);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetTransform(TGPMatrix* matrix);
	Fmx::Tmsfncgdiplusapi::Status __fastcall ResetTransform();
	Fmx::Tmsfncgdiplusapi::Status __fastcall MultiplyTransform(TGPMatrix* matrix, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall TranslateTransform(float dx, float dy, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall ScaleTransform(float sx, float sy, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall RotateTransform(float angle, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetWrapMode(Fmx::Tmsfncgdiplusapi::WrapMode wrapMode);
	Fmx::Tmsfncgdiplusapi::WrapMode __fastcall GetWrapMode();
	TGPImage* __fastcall GetImage();
public:
	/* TGPBrush.Create */ inline __fastcall TGPTextureBrush(void * ANativeBrush, Fmx::Tmsfncgdiplusapi::Status status)/* overload */ : TGPBrush(ANativeBrush, status) { }
	/* TGPBrush.Destroy */ inline __fastcall virtual ~TGPTextureBrush() { }
	
};


class PASCALIMPLEMENTATION TGPLinearGradientBrush : public TGPBrush
{
	typedef TGPBrush inherited;
	
public:
	__fastcall TGPLinearGradientBrush()/* overload */;
	__fastcall TGPLinearGradientBrush(const Fmx::Tmsfncgdiplusapi::TGPPointF &point1, const Fmx::Tmsfncgdiplusapi::TGPPointF &point2, unsigned color1, unsigned color2)/* overload */;
	__fastcall TGPLinearGradientBrush(const Fmx::Tmsfncgdiplusapi::TGPPoint &point1, const Fmx::Tmsfncgdiplusapi::TGPPoint &point2, unsigned color1, unsigned color2)/* overload */;
	__fastcall TGPLinearGradientBrush(const Fmx::Tmsfncgdiplusapi::TGPRectF &rect, unsigned color1, unsigned color2, Fmx::Tmsfncgdiplusapi::LinearGradientMode mode)/* overload */;
	__fastcall TGPLinearGradientBrush(const Fmx::Tmsfncgdiplusapi::TGPRect &rect, unsigned color1, unsigned color2, Fmx::Tmsfncgdiplusapi::LinearGradientMode mode)/* overload */;
	__fastcall TGPLinearGradientBrush(const Fmx::Tmsfncgdiplusapi::TGPRectF &rect, unsigned color1, unsigned color2, float angle, System::LongBool isAngleScalable)/* overload */;
	__fastcall TGPLinearGradientBrush(const Fmx::Tmsfncgdiplusapi::TGPRect &rect, unsigned color1, unsigned color2, float angle, System::LongBool isAngleScalable)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetLinearColors(unsigned color1, unsigned color2);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetLinearColors(/* out */ unsigned &color1, /* out */ unsigned &color2);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetRectangle(/* out */ Fmx::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetRectangle(/* out */ Fmx::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetGammaCorrection(System::LongBool useGammaCorrection);
	System::LongBool __fastcall GetGammaCorrection();
	int __fastcall GetBlendCount();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetBlend(Winapi::Windows::PSingle blendFactors, Winapi::Windows::PSingle blendPositions, int count);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetBlend(Winapi::Windows::PSingle blendFactors, Winapi::Windows::PSingle blendPositions, int count);
	int __fastcall GetInterpolationColorCount();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetInterpolationColors(Fmx::Tmsfncgdiplusapi::PGPColor presetColors, Winapi::Windows::PSingle blendPositions, int count);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetInterpolationColors(Fmx::Tmsfncgdiplusapi::PGPColor presetColors, Winapi::Windows::PSingle blendPositions, int count);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetBlendBellShape(float focus, float scale = 1.000000E+00f);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetBlendTriangularShape(float focus, float scale = 1.000000E+00f);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetTransform(TGPMatrix* matrix);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetTransform(TGPMatrix* matrix);
	Fmx::Tmsfncgdiplusapi::Status __fastcall ResetTransform();
	Fmx::Tmsfncgdiplusapi::Status __fastcall MultiplyTransform(TGPMatrix* matrix, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall TranslateTransform(float dx, float dy, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall ScaleTransform(float sx, float sy, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall RotateTransform(float angle, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetWrapMode(Fmx::Tmsfncgdiplusapi::WrapMode wrapMode);
	Fmx::Tmsfncgdiplusapi::WrapMode __fastcall GetWrapMode();
public:
	/* TGPBrush.Create */ inline __fastcall TGPLinearGradientBrush(void * ANativeBrush, Fmx::Tmsfncgdiplusapi::Status status)/* overload */ : TGPBrush(ANativeBrush, status) { }
	/* TGPBrush.Destroy */ inline __fastcall virtual ~TGPLinearGradientBrush() { }
	
};


class PASCALIMPLEMENTATION TGPHatchBrush : public TGPBrush
{
	typedef TGPBrush inherited;
	
public:
	__fastcall TGPHatchBrush()/* overload */;
	__fastcall TGPHatchBrush(Fmx::Tmsfncgdiplusapi::HatchStyle hatchStyle, unsigned foreColor, unsigned backColor)/* overload */;
	Fmx::Tmsfncgdiplusapi::HatchStyle __fastcall GetHatchStyle();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetForegroundColor(/* out */ unsigned &color);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetBackgroundColor(/* out */ unsigned &color);
public:
	/* TGPBrush.Create */ inline __fastcall TGPHatchBrush(void * ANativeBrush, Fmx::Tmsfncgdiplusapi::Status status)/* overload */ : TGPBrush(ANativeBrush, status) { }
	/* TGPBrush.Destroy */ inline __fastcall virtual ~TGPHatchBrush() { }
	
};


class PASCALIMPLEMENTATION TGPPen : public Fmx::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Fmx::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativePen;
	Fmx::Tmsfncgdiplusapi::Status lastResult;
	void __fastcall SetNativePen(void * ANativePen);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetStatus(Fmx::Tmsfncgdiplusapi::Status status);
	
public:
	__fastcall TGPPen(void * ANativePen, Fmx::Tmsfncgdiplusapi::Status status)/* overload */;
	__fastcall TGPPen(unsigned color, float width)/* overload */;
	__fastcall TGPPen(TGPBrush* brush, float width)/* overload */;
	__fastcall virtual ~TGPPen();
	TGPPen* __fastcall Clone();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetWidth(float width);
	float __fastcall GetWidth();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetLineCap(int startCap, int endCap, int dashCap);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetStartCap(int startCap);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetEndCap(int endCap);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetDashCap(int dashCap);
	int __fastcall GetStartCap();
	int __fastcall GetEndCap();
	int __fastcall GetDashCap();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetLineJoin(Fmx::Tmsfncgdiplusapi::LineJoin lineJoin);
	Fmx::Tmsfncgdiplusapi::LineJoin __fastcall GetLineJoin();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetCustomStartCap(TGPCustomLineCap* customCap);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetCustomStartCap(TGPCustomLineCap* customCap);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetCustomEndCap(TGPCustomLineCap* customCap);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetCustomEndCap(TGPCustomLineCap* customCap);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetMiterLimit(float miterLimit);
	float __fastcall GetMiterLimit();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetAlignment(Fmx::Tmsfncgdiplusapi::PenAlignment penAlignment);
	Fmx::Tmsfncgdiplusapi::PenAlignment __fastcall GetAlignment();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetTransform(TGPMatrix* matrix);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetTransform(TGPMatrix* matrix);
	Fmx::Tmsfncgdiplusapi::Status __fastcall ResetTransform();
	Fmx::Tmsfncgdiplusapi::Status __fastcall MultiplyTransform(TGPMatrix* matrix, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall TranslateTransform(float dx, float dy, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall ScaleTransform(float sx, float sy, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall RotateTransform(float angle, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	int __fastcall GetPenType();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetColor(unsigned color);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetBrush(TGPBrush* brush);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetColor(/* out */ unsigned &Color);
	TGPBrush* __fastcall GetBrush();
	Fmx::Tmsfncgdiplusapi::DashStyle __fastcall GetDashStyle();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetDashStyle(Fmx::Tmsfncgdiplusapi::DashStyle dashStyle);
	float __fastcall GetDashOffset();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetDashOffset(float dashOffset);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetDashPattern(Winapi::Windows::PSingle dashArray, int count);
	int __fastcall GetDashPatternCount();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetDashPattern(Winapi::Windows::PSingle dashArray, int count);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetCompoundArray(Winapi::Windows::PSingle compoundArray, int count);
	int __fastcall GetCompoundArrayCount();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetCompoundArray(Winapi::Windows::PSingle compoundArray, int count);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};


class PASCALIMPLEMENTATION TGPStringFormat : public Fmx::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Fmx::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeFormat;
	Fmx::Tmsfncgdiplusapi::Status lastError;
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetStatus(Fmx::Tmsfncgdiplusapi::Status newStatus);
	void __fastcall Assign(TGPStringFormat* source);
	__fastcall TGPStringFormat(void * clonedStringFormat, Fmx::Tmsfncgdiplusapi::Status status)/* overload */;
	
public:
	__fastcall TGPStringFormat(int formatFlags, System::Word language)/* overload */;
	__fastcall TGPStringFormat(TGPStringFormat* format)/* overload */;
	__fastcall virtual ~TGPStringFormat();
	__classmethod TGPStringFormat* __fastcall GenericDefault();
	__classmethod TGPStringFormat* __fastcall GenericTypographic();
	TGPStringFormat* __fastcall Clone();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetFormatFlags(int flags);
	int __fastcall GetFormatFlags();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetAlignment(Fmx::Tmsfncgdiplusapi::StringAlignment align);
	Fmx::Tmsfncgdiplusapi::StringAlignment __fastcall GetAlignment();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetLineAlignment(Fmx::Tmsfncgdiplusapi::StringAlignment align);
	Fmx::Tmsfncgdiplusapi::StringAlignment __fastcall GetLineAlignment();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetHotkeyPrefix(Fmx::Tmsfncgdiplusapi::HotkeyPrefix hotkeyPrefix);
	Fmx::Tmsfncgdiplusapi::HotkeyPrefix __fastcall GetHotkeyPrefix();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetTabStops(float firstTabOffset, int count, Winapi::Windows::PSingle tabStops);
	int __fastcall GetTabStopCount();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetTabStops(int count, Winapi::Windows::PSingle firstTabOffset, Winapi::Windows::PSingle tabStops);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetDigitSubstitution(System::Word language, Fmx::Tmsfncgdiplusapi::StringDigitSubstitute substitute);
	System::Word __fastcall GetDigitSubstitutionLanguage();
	Fmx::Tmsfncgdiplusapi::StringDigitSubstitute __fastcall GetDigitSubstitutionMethod();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetTrimming(Fmx::Tmsfncgdiplusapi::StringTrimming trimming);
	Fmx::Tmsfncgdiplusapi::StringTrimming __fastcall GetTrimming();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetMeasurableCharacterRanges(int rangeCount, Fmx::Tmsfncgdiplusapi::PCharacterRange ranges);
	int __fastcall GetMeasurableCharacterRangeCount();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};


class PASCALIMPLEMENTATION TGPGraphicsPath : public Fmx::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Fmx::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativePath;
	Fmx::Tmsfncgdiplusapi::Status lastResult;
	void __fastcall SetNativePath(void * ANativePath);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetStatus(Fmx::Tmsfncgdiplusapi::Status status);
	__fastcall TGPGraphicsPath(void * ANativePath)/* overload */;
	
public:
	__fastcall TGPGraphicsPath(TGPGraphicsPath* path)/* overload */;
	__fastcall TGPGraphicsPath(Fmx::Tmsfncgdiplusapi::FillMode fillMode)/* overload */;
	__fastcall TGPGraphicsPath(Fmx::Tmsfncgdiplusapi::PGPPointF points, System::PByte types, int count, Fmx::Tmsfncgdiplusapi::FillMode fillMode)/* overload */;
	__fastcall TGPGraphicsPath(Fmx::Tmsfncgdiplusapi::PGPPoint points, System::PByte types, int count, Fmx::Tmsfncgdiplusapi::FillMode fillMode)/* overload */;
	__fastcall virtual ~TGPGraphicsPath();
	TGPGraphicsPath* __fastcall Clone();
	Fmx::Tmsfncgdiplusapi::Status __fastcall Reset();
	Fmx::Tmsfncgdiplusapi::FillMode __fastcall GetFillMode();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetFillMode(Fmx::Tmsfncgdiplusapi::FillMode fillmode);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetPathData(Fmx::Tmsfncgdiplusapi::TPathData* pathData);
	Fmx::Tmsfncgdiplusapi::Status __fastcall StartFigure();
	Fmx::Tmsfncgdiplusapi::Status __fastcall CloseFigure();
	Fmx::Tmsfncgdiplusapi::Status __fastcall CloseAllFigures();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetMarker();
	Fmx::Tmsfncgdiplusapi::Status __fastcall ClearMarkers();
	Fmx::Tmsfncgdiplusapi::Status __fastcall Reverse();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetLastPoint(/* out */ Fmx::Tmsfncgdiplusapi::TGPPointF &lastPoint);
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddLine(const Fmx::Tmsfncgdiplusapi::TGPPointF &pt1, const Fmx::Tmsfncgdiplusapi::TGPPointF &pt2)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddLine(float x1, float y1, float x2, float y2)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddLines(Fmx::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddLine(const Fmx::Tmsfncgdiplusapi::TGPPoint &pt1, const Fmx::Tmsfncgdiplusapi::TGPPoint &pt2)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddLine(int x1, int y1, int x2, int y2)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddLines(Fmx::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddArc(const Fmx::Tmsfncgdiplusapi::TGPRectF &rect, float startAngle, float sweepAngle)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddArc(float x, float y, float width, float height, float startAngle, float sweepAngle)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddArc(const Fmx::Tmsfncgdiplusapi::TGPRect &rect, float startAngle, float sweepAngle)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddArc(int x, int y, int width, int height, float startAngle, float sweepAngle)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddBezier(const Fmx::Tmsfncgdiplusapi::TGPPointF &pt1, const Fmx::Tmsfncgdiplusapi::TGPPointF &pt2, const Fmx::Tmsfncgdiplusapi::TGPPointF &pt3, const Fmx::Tmsfncgdiplusapi::TGPPointF &pt4)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddBezier(float x1, float y1, float x2, float y2, float x3, float y3, float x4, float y4)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddBeziers(Fmx::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddBezier(const Fmx::Tmsfncgdiplusapi::TGPPoint &pt1, const Fmx::Tmsfncgdiplusapi::TGPPoint &pt2, const Fmx::Tmsfncgdiplusapi::TGPPoint &pt3, const Fmx::Tmsfncgdiplusapi::TGPPoint &pt4)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddBezier(int x1, int y1, int x2, int y2, int x3, int y3, int x4, int y4)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddBeziers(Fmx::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddCurve(Fmx::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddCurve(Fmx::Tmsfncgdiplusapi::PGPPointF points, int count, float tension)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddCurve(Fmx::Tmsfncgdiplusapi::PGPPointF points, int count, int offset, int numberOfSegments, float tension)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddCurve(Fmx::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddCurve(Fmx::Tmsfncgdiplusapi::PGPPoint points, int count, float tension)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddCurve(Fmx::Tmsfncgdiplusapi::PGPPoint points, int count, int offset, int numberOfSegments, float tension)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddClosedCurve(Fmx::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddClosedCurve(Fmx::Tmsfncgdiplusapi::PGPPointF points, int count, float tension)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddClosedCurve(Fmx::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddClosedCurve(Fmx::Tmsfncgdiplusapi::PGPPoint points, int count, float tension)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddRectangle(const Fmx::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddRectangles(Fmx::Tmsfncgdiplusapi::PGPRectF rects, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddRectangle(const Fmx::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddRectangles(Fmx::Tmsfncgdiplusapi::PGPRect rects, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddEllipse(const Fmx::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddEllipse(float x, float y, float width, float height)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddEllipse(const Fmx::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddEllipse(int x, int y, int width, int height)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddPie(const Fmx::Tmsfncgdiplusapi::TGPRectF &rect, float startAngle, float sweepAngle)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddPie(float x, float y, float width, float height, float startAngle, float sweepAngle)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddPie(const Fmx::Tmsfncgdiplusapi::TGPRect &rect, float startAngle, float sweepAngle)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddPie(int x, int y, int width, int height, float startAngle, float sweepAngle)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddPolygon(Fmx::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddPolygon(Fmx::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddPath(TGPGraphicsPath* addingPath, System::LongBool connect);
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddString(System::WideString string_, int length, TGPFontFamily* family, int style, float emSize, const Fmx::Tmsfncgdiplusapi::TGPPointF &origin, TGPStringFormat* format)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddString(System::WideString string_, int length, TGPFontFamily* family, int style, float emSize, const Fmx::Tmsfncgdiplusapi::TGPRectF &layoutRect, TGPStringFormat* format)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddString(System::WideString string_, int length, TGPFontFamily* family, int style, float emSize, const Fmx::Tmsfncgdiplusapi::TGPPoint &origin, TGPStringFormat* format)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddString(System::WideString string_, int length, TGPFontFamily* family, int style, float emSize, const Fmx::Tmsfncgdiplusapi::TGPRect &layoutRect, TGPStringFormat* format)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Transform(TGPMatrix* matrix);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetBounds(/* out */ Fmx::Tmsfncgdiplusapi::TGPRectF &bounds, TGPMatrix* matrix = (TGPMatrix*)(0x0), TGPPen* pen = (TGPPen*)(0x0))/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetBounds(/* out */ Fmx::Tmsfncgdiplusapi::TGPRect &bounds, TGPMatrix* matrix = (TGPMatrix*)(0x0), TGPPen* pen = (TGPPen*)(0x0))/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Flatten(TGPMatrix* matrix = (TGPMatrix*)(0x0), float flatness = 2.500000E-01f);
	Fmx::Tmsfncgdiplusapi::Status __fastcall Widen(TGPPen* pen, TGPMatrix* matrix = (TGPMatrix*)(0x0), float flatness = 2.500000E-01f);
	Fmx::Tmsfncgdiplusapi::Status __fastcall Outline(TGPMatrix* matrix = (TGPMatrix*)(0x0), float flatness = 2.500000E-01f);
	Fmx::Tmsfncgdiplusapi::Status __fastcall Warp(Fmx::Tmsfncgdiplusapi::PGPPointF destPoints, int count, const Fmx::Tmsfncgdiplusapi::TGPRectF &srcRect, TGPMatrix* matrix = (TGPMatrix*)(0x0), Fmx::Tmsfncgdiplusapi::WarpMode warpMode = (Fmx::Tmsfncgdiplusapi::WarpMode)(0x0), float flatness = 2.500000E-01f);
	int __fastcall GetPointCount();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetPathTypes(System::PByte types, int count);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetPathPoints(Fmx::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetPathPoints(Fmx::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
	System::LongBool __fastcall IsVisible(const Fmx::Tmsfncgdiplusapi::TGPPointF &point, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsVisible(float x, float y, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsVisible(const Fmx::Tmsfncgdiplusapi::TGPPoint &point, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsVisible(int x, int y, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsOutlineVisible(const Fmx::Tmsfncgdiplusapi::TGPPointF &point, TGPPen* pen, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsOutlineVisible(float x, float y, TGPPen* pen, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsOutlineVisible(const Fmx::Tmsfncgdiplusapi::TGPPoint &point, TGPPen* pen, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsOutlineVisible(int x, int y, TGPPen* pen, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
};


class PASCALIMPLEMENTATION TGPGraphicsPathIterator : public Fmx::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Fmx::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeIterator;
	Fmx::Tmsfncgdiplusapi::Status lastResult;
	void __fastcall SetNativeIterator(void * ANativeIterator);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetStatus(Fmx::Tmsfncgdiplusapi::Status status);
	
public:
	__fastcall TGPGraphicsPathIterator(TGPGraphicsPath* path);
	__fastcall virtual ~TGPGraphicsPathIterator();
	int __fastcall NextSubpath(/* out */ int &startIndex, /* out */ int &endIndex, /* out */ System::LongBool &isClosed)/* overload */;
	int __fastcall NextSubpath(TGPGraphicsPath* path, /* out */ System::LongBool &isClosed)/* overload */;
	int __fastcall NextPathType(/* out */ System::Byte &pathType, /* out */ int &startIndex, /* out */ int &endIndex);
	int __fastcall NextMarker(/* out */ int &startIndex, /* out */ int &endIndex)/* overload */;
	int __fastcall NextMarker(TGPGraphicsPath* path)/* overload */;
	int __fastcall GetCount();
	int __fastcall GetSubpathCount();
	System::LongBool __fastcall HasCurve();
	void __fastcall Rewind();
	int __fastcall Enumerate(Fmx::Tmsfncgdiplusapi::PGPPointF points, System::PByte types, int count);
	int __fastcall CopyData(Fmx::Tmsfncgdiplusapi::PGPPointF points, System::PByte types, int startIndex, int endIndex);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};


class PASCALIMPLEMENTATION TGPPathGradientBrush : public TGPBrush
{
	typedef TGPBrush inherited;
	
public:
	__fastcall TGPPathGradientBrush(Fmx::Tmsfncgdiplusapi::PGPPointF points, int count, Fmx::Tmsfncgdiplusapi::WrapMode wrapMode)/* overload */;
	__fastcall TGPPathGradientBrush(Fmx::Tmsfncgdiplusapi::PGPPoint points, int count, Fmx::Tmsfncgdiplusapi::WrapMode wrapMode)/* overload */;
	__fastcall TGPPathGradientBrush(TGPGraphicsPath* path)/* overload */;
	__fastcall TGPPathGradientBrush()/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetCenterColor(/* out */ unsigned &Color);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetCenterColor(unsigned color);
	int __fastcall GetPointCount();
	int __fastcall GetSurroundColorCount();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetSurroundColors(Fmx::Tmsfncgdiplusapi::PARGB colors, int &count);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetSurroundColors(Fmx::Tmsfncgdiplusapi::PARGB colors, int &count);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetGraphicsPath(TGPGraphicsPath* path);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetGraphicsPath(TGPGraphicsPath* path);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetCenterPoint(/* out */ Fmx::Tmsfncgdiplusapi::TGPPointF &point)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetCenterPoint(/* out */ Fmx::Tmsfncgdiplusapi::TGPPoint &point)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetCenterPoint(const Fmx::Tmsfncgdiplusapi::TGPPointF &point)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetCenterPoint(const Fmx::Tmsfncgdiplusapi::TGPPoint &point)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetRectangle(/* out */ Fmx::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetRectangle(/* out */ Fmx::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetGammaCorrection(System::LongBool useGammaCorrection)/* overload */;
	System::LongBool __fastcall GetGammaCorrection()/* overload */;
	int __fastcall GetBlendCount();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetBlend(Winapi::Windows::PSingle blendFactors, Winapi::Windows::PSingle blendPositions, int count);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetBlend(Winapi::Windows::PSingle blendFactors, Winapi::Windows::PSingle blendPositions, int count);
	int __fastcall GetInterpolationColorCount();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetInterpolationColors(Fmx::Tmsfncgdiplusapi::PARGB presetColors, Winapi::Windows::PSingle blendPositions, int count);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetInterpolationColors(Fmx::Tmsfncgdiplusapi::PARGB presetColors, Winapi::Windows::PSingle blendPositions, int count);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetBlendBellShape(float focus, float scale = 1.000000E+00f);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetBlendTriangularShape(float focus, float scale = 1.000000E+00f);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetTransform(TGPMatrix* matrix);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetTransform(TGPMatrix* matrix);
	Fmx::Tmsfncgdiplusapi::Status __fastcall ResetTransform();
	Fmx::Tmsfncgdiplusapi::Status __fastcall MultiplyTransform(TGPMatrix* matrix, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall TranslateTransform(float dx, float dy, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall ScaleTransform(float sx, float sy, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall RotateTransform(float angle, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetFocusScales(/* out */ float &xScale, /* out */ float &yScale);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetFocusScales(float xScale, float yScale);
	Fmx::Tmsfncgdiplusapi::WrapMode __fastcall GetWrapMode();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetWrapMode(Fmx::Tmsfncgdiplusapi::WrapMode wrapMode);
public:
	/* TGPBrush.Create */ inline __fastcall TGPPathGradientBrush(void * ANativeBrush, Fmx::Tmsfncgdiplusapi::Status status)/* overload */ : TGPBrush(ANativeBrush, status) { }
	/* TGPBrush.Destroy */ inline __fastcall virtual ~TGPPathGradientBrush() { }
	
};


class PASCALIMPLEMENTATION TGPGraphics : public Fmx::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Fmx::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeGraphics;
	Fmx::Tmsfncgdiplusapi::Status lastResult;
	void __fastcall SetNativeGraphics(void * graphics);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetStatus(Fmx::Tmsfncgdiplusapi::Status status);
	void * __fastcall GetNativeGraphics();
	void * __fastcall GetNativePen(TGPPen* pen);
	__fastcall TGPGraphics(void * graphics)/* overload */;
	
public:
	TGPGraphics* __fastcall FromHDC(HDC hdc)/* overload */;
	TGPGraphics* __fastcall FromHDC(HDC hdc, NativeUInt hdevice)/* overload */;
	TGPGraphics* __fastcall FromHWND(HWND hwnd, System::LongBool icm = false);
	TGPGraphics* __fastcall FromImage(TGPImage* image);
	__fastcall TGPGraphics(HDC hdc, int Dummy1, int Dummy2)/* overload */;
	__fastcall TGPGraphics(HDC hdc, NativeUInt hdevice)/* overload */;
	__fastcall TGPGraphics(HWND hwnd, System::LongBool icm)/* overload */;
	__fastcall TGPGraphics(TGPImage* image)/* overload */;
	__fastcall virtual ~TGPGraphics();
	void __fastcall Flush(Fmx::Tmsfncgdiplusapi::FlushIntention intention = (Fmx::Tmsfncgdiplusapi::FlushIntention)(0x0));
	HDC __fastcall GetHDC();
	void __fastcall ReleaseHDC(HDC hdc);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetRenderingOrigin(int x, int y);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetRenderingOrigin(/* out */ int &x, /* out */ int &y);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetCompositingMode(Fmx::Tmsfncgdiplusapi::CompositingMode compositingMode);
	Fmx::Tmsfncgdiplusapi::CompositingMode __fastcall GetCompositingMode();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetCompositingQuality(int compositingQuality);
	int __fastcall GetCompositingQuality();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetTextRenderingHint(Fmx::Tmsfncgdiplusapi::TextRenderingHint newMode);
	Fmx::Tmsfncgdiplusapi::TextRenderingHint __fastcall GetTextRenderingHint();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetTextContrast(unsigned contrast);
	unsigned __fastcall GetTextContrast();
	int __fastcall GetInterpolationMode();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetInterpolationMode(int interpolationMode);
	int __fastcall GetSmoothingMode();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetSmoothingMode(int smoothingMode);
	int __fastcall GetPixelOffsetMode();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetPixelOffsetMode(int pixelOffsetMode);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetTransform(TGPMatrix* matrix);
	Fmx::Tmsfncgdiplusapi::Status __fastcall ResetTransform();
	Fmx::Tmsfncgdiplusapi::Status __fastcall MultiplyTransform(TGPMatrix* matrix, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall TranslateTransform(float dx, float dy, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall ScaleTransform(float sx, float sy, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall RotateTransform(float angle, Fmx::Tmsfncgdiplusapi::MatrixOrder order = (Fmx::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetTransform(TGPMatrix* matrix);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetPageUnit(Fmx::Tmsfncgdiplusapi::Unit_ unit_);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetPageScale(float scale);
	Fmx::Tmsfncgdiplusapi::Unit_ __fastcall GetPageUnit();
	float __fastcall GetPageScale();
	float __fastcall GetDpiX();
	float __fastcall GetDpiY();
	Fmx::Tmsfncgdiplusapi::Status __fastcall TransformPoints(Fmx::Tmsfncgdiplusapi::CoordinateSpace destSpace, Fmx::Tmsfncgdiplusapi::CoordinateSpace srcSpace, Fmx::Tmsfncgdiplusapi::PGPPointF pts, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall TransformPoints(Fmx::Tmsfncgdiplusapi::CoordinateSpace destSpace, Fmx::Tmsfncgdiplusapi::CoordinateSpace srcSpace, Fmx::Tmsfncgdiplusapi::PGPPoint pts, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetNearestColor(unsigned &color);
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawLine(TGPPen* pen, float x1, float y1, float x2, float y2)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawLine(TGPPen* pen, const Fmx::Tmsfncgdiplusapi::TGPPointF &pt1, const Fmx::Tmsfncgdiplusapi::TGPPointF &pt2)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawLines(TGPPen* pen, Fmx::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawLine(TGPPen* pen, int x1, int y1, int x2, int y2)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawLine(TGPPen* pen, const Fmx::Tmsfncgdiplusapi::TGPPoint &pt1, const Fmx::Tmsfncgdiplusapi::TGPPoint &pt2)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawLines(TGPPen* pen, Fmx::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawArc(TGPPen* pen, float x, float y, float width, float height, float startAngle, float sweepAngle)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawArc(TGPPen* pen, const Fmx::Tmsfncgdiplusapi::TGPRectF &rect, float startAngle, float sweepAngle)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawArc(TGPPen* pen, int x, int y, int width, int height, float startAngle, float sweepAngle)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawArc(TGPPen* pen, const Fmx::Tmsfncgdiplusapi::TGPRect &rect, float startAngle, float sweepAngle)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawBezier(TGPPen* pen, float x1, float y1, float x2, float y2, float x3, float y3, float x4, float y4)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawBezier(TGPPen* pen, const Fmx::Tmsfncgdiplusapi::TGPPointF &pt1, const Fmx::Tmsfncgdiplusapi::TGPPointF &pt2, const Fmx::Tmsfncgdiplusapi::TGPPointF &pt3, const Fmx::Tmsfncgdiplusapi::TGPPointF &pt4)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawBeziers(TGPPen* pen, Fmx::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawBezier(TGPPen* pen, int x1, int y1, int x2, int y2, int x3, int y3, int x4, int y4)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawBezier(TGPPen* pen, const Fmx::Tmsfncgdiplusapi::TGPPoint &pt1, const Fmx::Tmsfncgdiplusapi::TGPPoint &pt2, const Fmx::Tmsfncgdiplusapi::TGPPoint &pt3, const Fmx::Tmsfncgdiplusapi::TGPPoint &pt4)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawBeziers(TGPPen* pen, Fmx::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawRectangle(TGPPen* pen, const Fmx::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawRectangle(TGPPen* pen, float x, float y, float width, float height)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawRectangles(TGPPen* pen, Fmx::Tmsfncgdiplusapi::PGPRectF rects, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawRectangle(TGPPen* pen, const Fmx::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawRectangle(TGPPen* pen, int x, int y, int width, int height)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawRectangles(TGPPen* pen, Fmx::Tmsfncgdiplusapi::PGPRect rects, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawEllipse(TGPPen* pen, const Fmx::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawEllipse(TGPPen* pen, float x, float y, float width, float height)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawEllipse(TGPPen* pen, const Fmx::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawEllipse(TGPPen* pen, int x, int y, int width, int height)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawPie(TGPPen* pen, const Fmx::Tmsfncgdiplusapi::TGPRectF &rect, float startAngle, float sweepAngle)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawPie(TGPPen* pen, float x, float y, float width, float height, float startAngle, float sweepAngle)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawPie(TGPPen* pen, const Fmx::Tmsfncgdiplusapi::TGPRect &rect, float startAngle, float sweepAngle)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawPie(TGPPen* pen, int x, int y, int width, int height, float startAngle, float sweepAngle)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawPolygon(TGPPen* pen, Fmx::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawPolygon(TGPPen* pen, Fmx::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawPath(TGPPen* pen, TGPGraphicsPath* path);
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawCurve(TGPPen* pen, Fmx::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawCurve(TGPPen* pen, Fmx::Tmsfncgdiplusapi::PGPPointF points, int count, float tension)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawCurve(TGPPen* pen, Fmx::Tmsfncgdiplusapi::PGPPointF points, int count, int offset, int numberOfSegments, float tension = 5.000000E-01f)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawCurve(TGPPen* pen, Fmx::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawCurve(TGPPen* pen, Fmx::Tmsfncgdiplusapi::PGPPoint points, int count, float tension)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawCurve(TGPPen* pen, Fmx::Tmsfncgdiplusapi::PGPPoint points, int count, int offset, int numberOfSegments, float tension = 5.000000E-01f)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawClosedCurve(TGPPen* pen, Fmx::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawClosedCurve(TGPPen* pen, Fmx::Tmsfncgdiplusapi::PGPPointF points, int count, float tension)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawClosedCurve(TGPPen* pen, Fmx::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawClosedCurve(TGPPen* pen, Fmx::Tmsfncgdiplusapi::PGPPoint points, int count, float tension)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall Clear(unsigned color);
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillRectangle(TGPBrush* brush, const Fmx::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillRectangle(TGPBrush* brush, float x, float y, float width, float height)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillRectangles(TGPBrush* brush, Fmx::Tmsfncgdiplusapi::PGPRectF rects, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillRectangle(TGPBrush* brush, const Fmx::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillRectangle(TGPBrush* brush, int x, int y, int width, int height)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillRectangles(TGPBrush* brush, Fmx::Tmsfncgdiplusapi::PGPRect rects, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillPolygon(TGPBrush* brush, Fmx::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillPolygon(TGPBrush* brush, Fmx::Tmsfncgdiplusapi::PGPPointF points, int count, Fmx::Tmsfncgdiplusapi::FillMode fillMode)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillPolygon(TGPBrush* brush, Fmx::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillPolygon(TGPBrush* brush, Fmx::Tmsfncgdiplusapi::PGPPoint points, int count, Fmx::Tmsfncgdiplusapi::FillMode fillMode)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillEllipse(TGPBrush* brush, const Fmx::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillEllipse(TGPBrush* brush, float x, float y, float width, float height)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillEllipse(TGPBrush* brush, const Fmx::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillEllipse(TGPBrush* brush, int x, int y, int width, int height)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillPie(TGPBrush* brush, const Fmx::Tmsfncgdiplusapi::TGPRectF &rect, float startAngle, float sweepAngle)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillPie(TGPBrush* brush, float x, float y, float width, float height, float startAngle, float sweepAngle)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillPie(TGPBrush* brush, const Fmx::Tmsfncgdiplusapi::TGPRect &rect, float startAngle, float sweepAngle)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillPie(TGPBrush* brush, int x, int y, int width, int height, float startAngle, float sweepAngle)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillPath(TGPBrush* brush, TGPGraphicsPath* path);
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillClosedCurve(TGPBrush* brush, Fmx::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillClosedCurve(TGPBrush* brush, Fmx::Tmsfncgdiplusapi::PGPPointF points, int count, Fmx::Tmsfncgdiplusapi::FillMode fillMode, float tension = 5.000000E-01f)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillClosedCurve(TGPBrush* brush, Fmx::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillClosedCurve(TGPBrush* brush, Fmx::Tmsfncgdiplusapi::PGPPoint points, int count, Fmx::Tmsfncgdiplusapi::FillMode fillMode, float tension = 5.000000E-01f)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall FillRegion(TGPBrush* brush, TGPRegion* region);
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawString(System::UnicodeString string_, TGPFont* font, const Fmx::Tmsfncgdiplusapi::TGPRectF &layoutRect, TGPStringFormat* stringFormat, TGPBrush* brush)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawString(System::WideString string_, int length, TGPFont* font, const Fmx::Tmsfncgdiplusapi::TGPRectF &layoutRect, TGPStringFormat* stringFormat, TGPBrush* brush)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawString(System::WideString string_, int length, TGPFont* font, const Fmx::Tmsfncgdiplusapi::TGPPointF &origin, TGPBrush* brush)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawString(System::WideString string_, int length, TGPFont* font, const Fmx::Tmsfncgdiplusapi::TGPPointF &origin, TGPStringFormat* stringFormat, TGPBrush* brush)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall MeasureString(System::WideString string_, int length, TGPFont* font, const Fmx::Tmsfncgdiplusapi::TGPRectF &layoutRect, TGPStringFormat* stringFormat, /* out */ Fmx::Tmsfncgdiplusapi::TGPRectF &boundingBox, System::PInteger codepointsFitted = (System::PInteger)(0x0), System::PInteger linesFilled = (System::PInteger)(0x0))/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall MeasureString(System::WideString string_, int length, TGPFont* font, const Fmx::Tmsfncgdiplusapi::TGPSizeF &layoutRectSize, TGPStringFormat* stringFormat, /* out */ Fmx::Tmsfncgdiplusapi::TGPSizeF &size, System::PInteger codepointsFitted = (System::PInteger)(0x0), System::PInteger linesFilled = (System::PInteger)(0x0))/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall MeasureString(System::WideString string_, int length, TGPFont* font, const Fmx::Tmsfncgdiplusapi::TGPPointF &origin, TGPStringFormat* stringFormat, /* out */ Fmx::Tmsfncgdiplusapi::TGPRectF &boundingBox)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall MeasureString(System::WideString string_, int length, TGPFont* font, const Fmx::Tmsfncgdiplusapi::TGPRectF &layoutRect, /* out */ Fmx::Tmsfncgdiplusapi::TGPRectF &boundingBox)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall MeasureString(System::WideString string_, int length, TGPFont* font, const Fmx::Tmsfncgdiplusapi::TGPPointF &origin, /* out */ Fmx::Tmsfncgdiplusapi::TGPRectF &boundingBox)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall MeasureCharacterRanges(System::WideString string_, int length, TGPFont* font, const Fmx::Tmsfncgdiplusapi::TGPRectF &layoutRect, TGPStringFormat* stringFormat, int regionCount, TGPRegion* const *regions, const int regions_High)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawDriverString(Fmx::Tmsfncgdiplusapi::PUINT16 text, int length, TGPFont* font, TGPBrush* brush, Fmx::Tmsfncgdiplusapi::PGPPointF positions, int flags, TGPMatrix* matrix);
	Fmx::Tmsfncgdiplusapi::Status __fastcall MeasureDriverString(Fmx::Tmsfncgdiplusapi::PUINT16 text, int length, TGPFont* font, Fmx::Tmsfncgdiplusapi::PGPPointF positions, int flags, TGPMatrix* matrix, /* out */ Fmx::Tmsfncgdiplusapi::TGPRectF &boundingBox);
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawCachedBitmap(TGPCachedBitmap* cb, int x, int y);
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, const Fmx::Tmsfncgdiplusapi::TGPPointF &point)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, float x, float y)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, const Fmx::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, float x, float y, float width, float height)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, const Fmx::Tmsfncgdiplusapi::TGPPoint &point)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, int x, int y)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, const Fmx::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, int x, int y, int width, int height)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, Fmx::Tmsfncgdiplusapi::PGPPointF destPoints, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, Fmx::Tmsfncgdiplusapi::PGPPoint destPoints, int count)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, float x, float y, float srcx, float srcy, float srcwidth, float srcheight, Fmx::Tmsfncgdiplusapi::Unit_ srcUnit)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, const Fmx::Tmsfncgdiplusapi::TGPRectF &destRect, float srcx, float srcy, float srcwidth, float srcheight, Fmx::Tmsfncgdiplusapi::Unit_ srcUnit, TGPImageAttributes* imageAttributes = (TGPImageAttributes*)(0x0), Fmx::Tmsfncgdiplusapi::ImageAbort callback = 0x0, void * callbackData = (void *)(0x0))/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, Fmx::Tmsfncgdiplusapi::PGPPointF destPoints, int count, float srcx, float srcy, float srcwidth, float srcheight, Fmx::Tmsfncgdiplusapi::Unit_ srcUnit, TGPImageAttributes* imageAttributes = (TGPImageAttributes*)(0x0), Fmx::Tmsfncgdiplusapi::ImageAbort callback = 0x0, void * callbackData = (void *)(0x0))/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, int x, int y, int srcx, int srcy, int srcwidth, int srcheight, Fmx::Tmsfncgdiplusapi::Unit_ srcUnit)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, const Fmx::Tmsfncgdiplusapi::TGPRect &destRect, int srcx, int srcy, int srcwidth, int srcheight, Fmx::Tmsfncgdiplusapi::Unit_ srcUnit, TGPImageAttributes* imageAttributes = (TGPImageAttributes*)(0x0), Fmx::Tmsfncgdiplusapi::ImageAbort callback = 0x0, void * callbackData = (void *)(0x0))/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, Fmx::Tmsfncgdiplusapi::PGPPoint destPoints, int count, int srcx, int srcy, int srcwidth, int srcheight, Fmx::Tmsfncgdiplusapi::Unit_ srcUnit, TGPImageAttributes* imageAttributes = (TGPImageAttributes*)(0x0), Fmx::Tmsfncgdiplusapi::ImageAbort callback = 0x0, void * callbackData = (void *)(0x0))/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetClip(TGPGraphics* g, Fmx::Tmsfncgdiplusapi::CombineMode combineMode = (Fmx::Tmsfncgdiplusapi::CombineMode)(0x0))/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetClip(const Fmx::Tmsfncgdiplusapi::TGPRectF &rect, Fmx::Tmsfncgdiplusapi::CombineMode combineMode = (Fmx::Tmsfncgdiplusapi::CombineMode)(0x0))/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetClip(const Fmx::Tmsfncgdiplusapi::TGPRect &rect, Fmx::Tmsfncgdiplusapi::CombineMode combineMode = (Fmx::Tmsfncgdiplusapi::CombineMode)(0x0))/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetClip(TGPGraphicsPath* path, Fmx::Tmsfncgdiplusapi::CombineMode combineMode = (Fmx::Tmsfncgdiplusapi::CombineMode)(0x0))/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetClip(TGPRegion* region, Fmx::Tmsfncgdiplusapi::CombineMode combineMode = (Fmx::Tmsfncgdiplusapi::CombineMode)(0x0))/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetClip(HRGN hRgn, Fmx::Tmsfncgdiplusapi::CombineMode combineMode = (Fmx::Tmsfncgdiplusapi::CombineMode)(0x0))/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall IntersectClip(const Fmx::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall IntersectClip(const Fmx::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall IntersectClip(TGPRegion* region)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall ExcludeClip(const Fmx::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall ExcludeClip(const Fmx::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall ExcludeClip(TGPRegion* region)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall ResetClip();
	Fmx::Tmsfncgdiplusapi::Status __fastcall TranslateClip(float dx, float dy)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall TranslateClip(int dx, int dy)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetClip(TGPRegion* region);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetClipBounds(/* out */ Fmx::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetClipBounds(/* out */ Fmx::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	System::LongBool __fastcall IsClipEmpty();
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetVisibleClipBounds(/* out */ Fmx::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetVisibleClipBounds(/* out */ Fmx::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	System::LongBool __fastcall IsVisibleClipEmpty();
	System::LongBool __fastcall IsVisible(int x, int y)/* overload */;
	System::LongBool __fastcall IsVisible(const Fmx::Tmsfncgdiplusapi::TGPPoint &point)/* overload */;
	System::LongBool __fastcall IsVisible(int x, int y, int width, int height)/* overload */;
	System::LongBool __fastcall IsVisible(const Fmx::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	System::LongBool __fastcall IsVisible(float x, float y)/* overload */;
	System::LongBool __fastcall IsVisible(const Fmx::Tmsfncgdiplusapi::TGPPointF &point)/* overload */;
	System::LongBool __fastcall IsVisible(float x, float y, float width, float height)/* overload */;
	System::LongBool __fastcall IsVisible(const Fmx::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	unsigned __fastcall Save();
	Fmx::Tmsfncgdiplusapi::Status __fastcall Restore(unsigned gstate);
	unsigned __fastcall BeginContainer(const Fmx::Tmsfncgdiplusapi::TGPRectF &dstrect, const Fmx::Tmsfncgdiplusapi::TGPRectF &srcrect, Fmx::Tmsfncgdiplusapi::Unit_ unit_)/* overload */;
	unsigned __fastcall BeginContainer(const Fmx::Tmsfncgdiplusapi::TGPRect &dstrect, const Fmx::Tmsfncgdiplusapi::TGPRect &srcrect, Fmx::Tmsfncgdiplusapi::Unit_ unit_)/* overload */;
	unsigned __fastcall BeginContainer()/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall EndContainer(unsigned state);
	Fmx::Tmsfncgdiplusapi::Status __fastcall AddMetafileComment(System::PByte data, unsigned sizeData);
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};


class PASCALIMPLEMENTATION TGPAdjustableArrowCap : public TGPCustomLineCap
{
	typedef TGPCustomLineCap inherited;
	
public:
	__fastcall TGPAdjustableArrowCap(float height, float width, System::LongBool isFilled);
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetHeight(float height);
	float __fastcall GetHeight();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetWidth(float width);
	float __fastcall GetWidth();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetMiddleInset(float middleInset);
	float __fastcall GetMiddleInset();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetFillState(System::LongBool isFilled);
	System::LongBool __fastcall IsFilled();
public:
	/* TGPCustomLineCap.Destroy */ inline __fastcall virtual ~TGPAdjustableArrowCap() { }
	
};


class PASCALIMPLEMENTATION TGPMetafile : public TGPImage
{
	typedef TGPImage inherited;
	
public:
	__fastcall TGPMetafile(HMETAFILE hWmf, Fmx::Tmsfncgdiplusapi::WmfPlaceableFileHeader &wmfPlaceableFileHeader, System::LongBool deleteWmf)/* overload */;
	__fastcall TGPMetafile(HENHMETAFILE hEmf, System::LongBool deleteEmf)/* overload */;
	__fastcall TGPMetafile(System::WideString filename)/* overload */;
	__fastcall TGPMetafile(System::WideString filename, Fmx::Tmsfncgdiplusapi::WmfPlaceableFileHeader &wmfPlaceableFileHeader)/* overload */;
	__fastcall TGPMetafile(_di_IStream stream)/* overload */;
	__fastcall TGPMetafile(HDC referenceHdc, int type_, System::WideChar * description)/* overload */;
	__fastcall TGPMetafile(HDC referenceHdc, const Fmx::Tmsfncgdiplusapi::TGPRectF &frameRect, int frameUnit, int type_, System::WideChar * description)/* overload */;
	__fastcall TGPMetafile(HDC referenceHdc, const Fmx::Tmsfncgdiplusapi::TGPRect &frameRect, int frameUnit, int type_, System::WideChar * description)/* overload */;
	__fastcall TGPMetafile(System::WideString fileName, HDC referenceHdc, int type_, System::WideChar * description)/* overload */;
	__fastcall TGPMetafile(System::WideString fileName, HDC referenceHdc, const Fmx::Tmsfncgdiplusapi::TGPRectF &frameRect, int frameUnit, int type_, System::WideChar * description)/* overload */;
	__fastcall TGPMetafile(System::WideString fileName, HDC referenceHdc, const Fmx::Tmsfncgdiplusapi::TGPRect &frameRect, int frameUnit, int type_, System::WideChar * description)/* overload */;
	__fastcall TGPMetafile(_di_IStream stream, HDC referenceHdc, int type_, System::WideChar * description)/* overload */;
	__fastcall TGPMetafile(_di_IStream stream, HDC referenceHdc, const Fmx::Tmsfncgdiplusapi::TGPRectF &frameRect, int frameUnit, int type_, System::WideChar * description)/* overload */;
	__fastcall TGPMetafile(_di_IStream stream, HDC referenceHdc, const Fmx::Tmsfncgdiplusapi::TGPRect &frameRect, int frameUnit, int type_, System::WideChar * description)/* overload */;
	__fastcall TGPMetafile()/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetMetafileHeader(HMETAFILE hWmf, Fmx::Tmsfncgdiplusapi::WmfPlaceableFileHeader &wmfPlaceableFileHeader, Fmx::Tmsfncgdiplusapi::TMetafileHeader* header)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetMetafileHeader(HENHMETAFILE hEmf, Fmx::Tmsfncgdiplusapi::TMetafileHeader* header)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetMetafileHeader(System::WideString filename, Fmx::Tmsfncgdiplusapi::TMetafileHeader* header)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetMetafileHeader(_di_IStream stream, Fmx::Tmsfncgdiplusapi::TMetafileHeader* header)/* overload */;
	Fmx::Tmsfncgdiplusapi::Status __fastcall GetMetafileHeader(Fmx::Tmsfncgdiplusapi::TMetafileHeader* header)/* overload */;
	HENHMETAFILE __fastcall GetHENHMETAFILE();
	Fmx::Tmsfncgdiplusapi::Status __fastcall SetDownLevelRasterizationLimit(unsigned metafileRasterizationLimitDpi);
	unsigned __fastcall GetDownLevelRasterizationLimit();
	unsigned __fastcall EmfToWmfBits(HENHMETAFILE hemf, unsigned cbData16, System::PByte pData16, int iMapMode = 0x8, int eFlags = 0x0);
public:
	/* TGPImage.Create */ inline __fastcall TGPMetafile(void * ANativeImage, Fmx::Tmsfncgdiplusapi::Status status)/* overload */ : TGPImage(ANativeImage, status) { }
	/* TGPImage.Create */ inline __fastcall TGPMetafile(System::WideString filename, System::LongBool useEmbeddedColorManagement)/* overload */ : TGPImage(filename, useEmbeddedColorManagement) { }
	/* TGPImage.Create */ inline __fastcall TGPMetafile(_di_IStream stream, System::LongBool useEmbeddedColorManagement)/* overload */ : TGPImage(stream, useEmbeddedColorManagement) { }
	/* TGPImage.Destroy */ inline __fastcall virtual ~TGPMetafile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGPFontFamily* GenericSansSerifFontFamily;
extern DELPHI_PACKAGE TGPFontFamily* GenericSerifFontFamily;
extern DELPHI_PACKAGE TGPFontFamily* GenericMonospaceFontFamily;
extern DELPHI_PACKAGE TGPStringFormat* GenericTypographicStringFormatBuffer;
extern DELPHI_PACKAGE TGPStringFormat* GenericDefaultStringFormatBuffer;
extern DELPHI_PACKAGE Fmx::Tmsfncgdiplusapi::GdiplusStartupInput StartupInput;
extern DELPHI_PACKAGE unsigned gdiplusToken;
extern DELPHI_PACKAGE Fmx::Tmsfncgdiplusapi::Status __fastcall GetImageDecodersSize(/* out */ unsigned &numDecoders, /* out */ unsigned &size);
extern DELPHI_PACKAGE Fmx::Tmsfncgdiplusapi::Status __fastcall GetImageDecoders(unsigned numDecoders, unsigned size, Fmx::Tmsfncgdiplusapi::PImageCodecInfo decoders);
extern DELPHI_PACKAGE Fmx::Tmsfncgdiplusapi::Status __fastcall GetImageEncodersSize(/* out */ unsigned &numEncoders, /* out */ unsigned &size);
extern DELPHI_PACKAGE Fmx::Tmsfncgdiplusapi::Status __fastcall GetImageEncoders(unsigned numEncoders, unsigned size, Fmx::Tmsfncgdiplusapi::PImageCodecInfo encoders);
extern DELPHI_PACKAGE void __fastcall InitializeGdiPlus(void);
extern DELPHI_PACKAGE void __fastcall FinalizeGdiPlus(void);
}	/* namespace Tmsfncgdiplusclasses */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCGDIPLUSCLASSES)
using namespace Fmx::Tmsfncgdiplusclasses;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncgdiplusclassesHPP
