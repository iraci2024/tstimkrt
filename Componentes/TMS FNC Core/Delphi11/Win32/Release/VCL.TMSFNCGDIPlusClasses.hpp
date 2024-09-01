// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCGDIPlusClasses.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncgdiplusclassesHPP
#define Vcl_TmsfncgdiplusclassesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.ActiveX.hpp>
#include <VCL.TMSFNCGDIPlusApi.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
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
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPRegion : public Vcl::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Vcl::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeRegion;
	Vcl::Tmsfncgdiplusapi::Status lastResult;
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetStatus(Vcl::Tmsfncgdiplusapi::Status status);
	void __fastcall SetNativeRegion(void * ANativeRegion);
	
public:
	__fastcall TGPRegion(void * ANativeRegion)/* overload */;
	__fastcall TGPRegion()/* overload */;
	__fastcall TGPRegion(const Vcl::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	__fastcall TGPRegion(const Vcl::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	__fastcall TGPRegion(TGPGraphicsPath* path)/* overload */;
	__fastcall TGPRegion(System::PByte regionData, int size)/* overload */;
	__fastcall TGPRegion(HRGN hRgn, int Dummy1, int Dummy2)/* overload */;
	TGPRegion* __fastcall FromHRGN(HRGN hRgn);
	__fastcall virtual ~TGPRegion();
	TGPRegion* __fastcall Clone();
	Vcl::Tmsfncgdiplusapi::Status __fastcall MakeInfinite();
	Vcl::Tmsfncgdiplusapi::Status __fastcall MakeEmpty();
	unsigned __fastcall GetDataSize();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetData(System::PByte buffer, unsigned bufferSize, PUINT sizeFilled = (PUINT)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall Intersect(const Vcl::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Intersect(const Vcl::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Intersect(TGPGraphicsPath* path)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Intersect(TGPRegion* region)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Union(const Vcl::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Union(const Vcl::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Union(TGPGraphicsPath* path)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Union(TGPRegion* region)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Xor_(const Vcl::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Xor_(const Vcl::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Xor_(TGPGraphicsPath* path)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Xor_(TGPRegion* region)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Exclude(const Vcl::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Exclude(const Vcl::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Exclude(TGPGraphicsPath* path)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Exclude(TGPRegion* region)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Complement(const Vcl::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Complement(const Vcl::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Complement(TGPGraphicsPath* path)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Complement(TGPRegion* region)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Translate(float dx, float dy)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Translate(int dx, int dy)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Transform(TGPMatrix* matrix);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetBounds(/* out */ Vcl::Tmsfncgdiplusapi::TGPRect &rect, TGPGraphics* g)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetBounds(/* out */ Vcl::Tmsfncgdiplusapi::TGPRectF &rect, TGPGraphics* g)/* overload */;
	HRGN __fastcall GetHRGN(TGPGraphics* g);
	System::LongBool __fastcall IsEmpty(TGPGraphics* g);
	System::LongBool __fastcall IsInfinite(TGPGraphics* g);
	System::LongBool __fastcall IsVisible(int x, int y, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsVisible(const Vcl::Tmsfncgdiplusapi::TGPPoint &point, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsVisible(float x, float y, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsVisible(const Vcl::Tmsfncgdiplusapi::TGPPointF &point, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsVisible(int x, int y, int width, int height, TGPGraphics* g)/* overload */;
	System::LongBool __fastcall IsVisible(const Vcl::Tmsfncgdiplusapi::TGPRect &rect, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsVisible(float x, float y, float width, float height, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsVisible(const Vcl::Tmsfncgdiplusapi::TGPRectF &rect, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsEqual(TGPRegion* region, TGPGraphics* g);
	unsigned __fastcall GetRegionScansCount(TGPMatrix* matrix);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetRegionScans(TGPMatrix* matrix, Vcl::Tmsfncgdiplusapi::PGPRectF rects, /* out */ int &count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetRegionScans(TGPMatrix* matrix, Vcl::Tmsfncgdiplusapi::PGPRect rects, /* out */ int &count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPFontFamily : public Vcl::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Vcl::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeFamily;
	Vcl::Tmsfncgdiplusapi::Status lastResult;
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetStatus(Vcl::Tmsfncgdiplusapi::Status status);
	
public:
	__fastcall TGPFontFamily(void * nativeOrig, Vcl::Tmsfncgdiplusapi::Status status)/* overload */;
	__fastcall TGPFontFamily()/* overload */;
	__fastcall TGPFontFamily(System::WideString name, TGPFontCollection* fontCollection)/* overload */;
	__fastcall virtual ~TGPFontFamily();
	__classmethod TGPFontFamily* __fastcall GenericSansSerif();
	__classmethod TGPFontFamily* __fastcall GenericSerif();
	__classmethod TGPFontFamily* __fastcall GenericMonospace();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetFamilyName(/* out */ System::UnicodeString &name, System::Word language = (System::Word)(0x0));
	TGPFontFamily* __fastcall Clone();
	System::LongBool __fastcall IsAvailable();
	System::LongBool __fastcall IsStyleAvailable(int style);
	Vcl::Tmsfncgdiplusapi::UINT16 __fastcall GetEmHeight(int style);
	Vcl::Tmsfncgdiplusapi::UINT16 __fastcall GetCellAscent(int style);
	Vcl::Tmsfncgdiplusapi::UINT16 __fastcall GetCellDescent(int style);
	Vcl::Tmsfncgdiplusapi::UINT16 __fastcall GetLineSpacing(int style);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPFontCollection : public Vcl::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Vcl::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeFontCollection;
	Vcl::Tmsfncgdiplusapi::Status lastResult;
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetStatus(Vcl::Tmsfncgdiplusapi::Status status);
	
public:
	__fastcall TGPFontCollection();
	__fastcall virtual ~TGPFontCollection();
	int __fastcall GetFamilyCount();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetFamilies(int numSought, /* out */ TGPFontFamily* *gpfamilies, const int gpfamilies_High, /* out */ int &numFound);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPInstalledFontCollection : public TGPFontCollection
{
	typedef TGPFontCollection inherited;
	
public:
	__fastcall TGPInstalledFontCollection();
	__fastcall virtual ~TGPInstalledFontCollection();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPPrivateFontCollection : public TGPFontCollection
{
	typedef TGPFontCollection inherited;
	
public:
	__fastcall TGPPrivateFontCollection();
	__fastcall virtual ~TGPPrivateFontCollection();
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddFontFile(System::WideString filename);
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddMemoryFont(void * memory, int length);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPFont : public Vcl::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Vcl::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeFont;
	Vcl::Tmsfncgdiplusapi::Status lastResult;
	void __fastcall SetNativeFont(void * Font);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetStatus(Vcl::Tmsfncgdiplusapi::Status status);
	
public:
	__fastcall TGPFont(void * font, Vcl::Tmsfncgdiplusapi::Status status)/* overload */;
	__fastcall TGPFont(HDC hdc)/* overload */;
	__fastcall TGPFont(HDC hdc, Winapi::Windows::PLogFontA logfont)/* overload */;
	__fastcall TGPFont(HDC hdc, Winapi::Windows::PLogFontW logfont)/* overload */;
	__fastcall TGPFont(HDC hdc, HFONT hfont)/* overload */;
	__fastcall TGPFont(TGPFontFamily* family, float emSize, int style, Vcl::Tmsfncgdiplusapi::Unit_ unit_)/* overload */;
	__fastcall TGPFont(System::WideString familyName, float emSize, int style, Vcl::Tmsfncgdiplusapi::Unit_ unit_, TGPFontCollection* fontCollection)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetLogFontA(TGPGraphics* g, /* out */ tagLOGFONTA &logfontA);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetLogFontW(TGPGraphics* g, /* out */ tagLOGFONTW &logfontW);
	TGPFont* __fastcall Clone();
	__fastcall virtual ~TGPFont();
	System::LongBool __fastcall IsAvailable();
	int __fastcall GetStyle();
	float __fastcall GetSize();
	Vcl::Tmsfncgdiplusapi::Unit_ __fastcall GetUnit();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
	float __fastcall GetHeight(TGPGraphics* graphics)/* overload */;
	float __fastcall GetHeight(float dpi)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetFamily(TGPFontFamily* family);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPImage : public Vcl::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Vcl::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeImage;
	Vcl::Tmsfncgdiplusapi::Status lastResult;
	Vcl::Tmsfncgdiplusapi::Status loadStatus;
	void __fastcall SetNativeImage(void * ANativeImage);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetStatus(Vcl::Tmsfncgdiplusapi::Status status);
	
public:
	__fastcall TGPImage(void * ANativeImage, Vcl::Tmsfncgdiplusapi::Status status)/* overload */;
	__fastcall TGPImage(System::WideString filename, System::LongBool useEmbeddedColorManagement)/* overload */;
	__fastcall TGPImage(_di_IStream stream, System::LongBool useEmbeddedColorManagement)/* overload */;
	TGPImage* __fastcall FromFile(System::WideString filename, System::LongBool useEmbeddedColorManagement = false);
	TGPImage* __fastcall FromStream(_di_IStream stream, System::LongBool useEmbeddedColorManagement = false);
	__fastcall virtual ~TGPImage();
	TGPImage* __fastcall Clone();
	Vcl::Tmsfncgdiplusapi::Status __fastcall Save(System::WideString filename, const GUID &clsidEncoder, Vcl::Tmsfncgdiplusapi::PEncoderParameters encoderParams = (Vcl::Tmsfncgdiplusapi::PEncoderParameters)(0x0))/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Save(_di_IStream stream, const GUID &clsidEncoder, Vcl::Tmsfncgdiplusapi::PEncoderParameters encoderParams = (Vcl::Tmsfncgdiplusapi::PEncoderParameters)(0x0))/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall SaveAdd(Vcl::Tmsfncgdiplusapi::PEncoderParameters encoderParams)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall SaveAdd(TGPImage* newImage, Vcl::Tmsfncgdiplusapi::PEncoderParameters encoderParams)/* overload */;
	Vcl::Tmsfncgdiplusapi::ImageType __fastcall GetType();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetPhysicalDimension(/* out */ Vcl::Tmsfncgdiplusapi::TGPSizeF &size);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetBounds(/* out */ Vcl::Tmsfncgdiplusapi::TGPRectF &srcRect, /* out */ Vcl::Tmsfncgdiplusapi::Unit_ &srcUnit);
	unsigned __fastcall GetWidth();
	unsigned __fastcall GetHeight();
	float __fastcall GetHorizontalResolution();
	float __fastcall GetVerticalResolution();
	unsigned __fastcall GetFlags();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetRawFormat(/* out */ GUID &format);
	int __fastcall GetPixelFormat();
	int __fastcall GetPaletteSize();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetPalette(Vcl::Tmsfncgdiplusapi::PColorPalette palette, int size);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetPalette(Vcl::Tmsfncgdiplusapi::PColorPalette palette);
	TGPImage* __fastcall GetThumbnailImage(unsigned thumbWidth, unsigned thumbHeight, Vcl::Tmsfncgdiplusapi::ImageAbort callback = 0x0, void * callbackData = (void *)(0x0));
	unsigned __fastcall GetFrameDimensionsCount();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetFrameDimensionsList(System::PGUID dimensionIDs, unsigned count);
	unsigned __fastcall GetFrameCount(const GUID &dimensionID);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SelectActiveFrame(const GUID &dimensionID, unsigned frameIndex);
	Vcl::Tmsfncgdiplusapi::Status __fastcall RotateFlip(Vcl::Tmsfncgdiplusapi::RotateFlipType rotateFlipType);
	unsigned __fastcall GetPropertyCount();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetPropertyIdList(unsigned numOfProperty, Winapi::Activex::PPropID list);
	unsigned __fastcall GetPropertyItemSize(unsigned propId);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetPropertyItem(unsigned propId, unsigned propSize, Vcl::Tmsfncgdiplusapi::PPropertyItem buffer);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetPropertySize(/* out */ unsigned &totalBufferSize, /* out */ unsigned &numProperties);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetAllPropertyItems(unsigned totalBufferSize, unsigned numProperties, Vcl::Tmsfncgdiplusapi::PPropertyItem allItems);
	Vcl::Tmsfncgdiplusapi::Status __fastcall RemovePropertyItem(unsigned propId);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetPropertyItem(const Vcl::Tmsfncgdiplusapi::PropertyItem &item);
	unsigned __fastcall GetEncoderParameterListSize(const GUID &clsidEncoder);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetEncoderParameterList(const GUID &clsidEncoder, unsigned size, Vcl::Tmsfncgdiplusapi::PEncoderParameters buffer);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};

#pragma pack(pop)

#pragma pack(push,4)
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
	HIDESBASE TGPBitmap* __fastcall Clone(const Vcl::Tmsfncgdiplusapi::TGPRect &rect, int format)/* overload */;
	HIDESBASE TGPBitmap* __fastcall Clone(int x, int y, int width, int height, int format)/* overload */;
	HIDESBASE TGPBitmap* __fastcall Clone(const Vcl::Tmsfncgdiplusapi::TGPRectF &rect, int format)/* overload */;
	HIDESBASE TGPBitmap* __fastcall Clone(float x, float y, float width, float height, int format)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall LockBits(const Vcl::Tmsfncgdiplusapi::TGPRect &rect, unsigned flags, int format, /* out */ Vcl::Tmsfncgdiplusapi::BitmapData &lockedBitmapData);
	Vcl::Tmsfncgdiplusapi::Status __fastcall UnlockBits(Vcl::Tmsfncgdiplusapi::BitmapData &lockedBitmapData);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetPixel(int x, int y, /* out */ unsigned &color);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetPixel(int x, int y, unsigned color);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetResolution(float xdpi, float ydpi);
	__fastcall TGPBitmap(tagBITMAPINFO &gdiBitmapInfo, void * gdiBitmapData)/* overload */;
	__fastcall TGPBitmap(HBITMAP hbm, HPALETTE hpal)/* overload */;
	__fastcall TGPBitmap(HICON hicon, int Dummy1)/* overload */;
	__fastcall TGPBitmap(NativeUInt hInstance, System::WideString bitmapName)/* overload */;
	TGPBitmap* __fastcall FromBITMAPINFO(tagBITMAPINFO &gdiBitmapInfo, void * gdiBitmapData);
	TGPBitmap* __fastcall FromHBITMAP(HBITMAP hbm, HPALETTE hpal);
	TGPBitmap* __fastcall FromHICON(HICON hicon);
	TGPBitmap* __fastcall FromResource(NativeUInt hInstance, System::WideString bitmapName);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetHBITMAP(unsigned colorBackground, /* out */ HBITMAP &hbmReturn);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetHICON(/* out */ HICON &hicon);
public:
	/* TGPImage.Create */ inline __fastcall TGPBitmap(void * ANativeImage, Vcl::Tmsfncgdiplusapi::Status status)/* overload */ : TGPImage(ANativeImage, status) { }
	/* TGPImage.Destroy */ inline __fastcall virtual ~TGPBitmap() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPCustomLineCap : public Vcl::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Vcl::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeCap;
	Vcl::Tmsfncgdiplusapi::Status lastResult;
	void __fastcall SetNativeCap(void * ANativeCap);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetStatus(Vcl::Tmsfncgdiplusapi::Status status);
	
public:
	__fastcall TGPCustomLineCap(void * ANativeCap, Vcl::Tmsfncgdiplusapi::Status status)/* overload */;
	__fastcall TGPCustomLineCap()/* overload */;
	__fastcall TGPCustomLineCap(TGPGraphicsPath* fillPath, TGPGraphicsPath* strokePath, int baseCap, float baseInset)/* overload */;
	__fastcall virtual ~TGPCustomLineCap();
	TGPCustomLineCap* __fastcall Clone();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetStrokeCap(int strokeCap);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetStrokeCaps(int startCap, int endCap);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetStrokeCaps(/* out */ int &startCap, /* out */ int &endCap);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetStrokeJoin(Vcl::Tmsfncgdiplusapi::LineJoin lineJoin);
	Vcl::Tmsfncgdiplusapi::LineJoin __fastcall GetStrokeJoin();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetBaseCap(int baseCap);
	int __fastcall GetBaseCap();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetBaseInset(float inset);
	float __fastcall GetBaseInset();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetWidthScale(float widthScale);
	float __fastcall GetWidthScale();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPCachedBitmap : public Vcl::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Vcl::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeCachedBitmap;
	Vcl::Tmsfncgdiplusapi::Status lastResult;
	
public:
	__fastcall TGPCachedBitmap(TGPBitmap* bitmap, TGPGraphics* graphics);
	__fastcall virtual ~TGPCachedBitmap();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPImageAttributes : public Vcl::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Vcl::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeImageAttr;
	Vcl::Tmsfncgdiplusapi::Status lastResult;
	void __fastcall SetNativeImageAttr(void * ANativeImageAttr);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetStatus(Vcl::Tmsfncgdiplusapi::Status status);
	
public:
	__fastcall TGPImageAttributes(void * imageAttr, Vcl::Tmsfncgdiplusapi::Status status)/* overload */;
	__fastcall TGPImageAttributes()/* overload */;
	__fastcall virtual ~TGPImageAttributes();
	TGPImageAttributes* __fastcall Clone();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetToIdentity(Vcl::Tmsfncgdiplusapi::ColorAdjustType type_ = (Vcl::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall Reset(Vcl::Tmsfncgdiplusapi::ColorAdjustType type_ = (Vcl::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetColorMatrix(const Vcl::Tmsfncgdiplusapi::ColorMatrix &colorMatrix, Vcl::Tmsfncgdiplusapi::ColorMatrixFlags mode = (Vcl::Tmsfncgdiplusapi::ColorMatrixFlags)(0x0), Vcl::Tmsfncgdiplusapi::ColorAdjustType type_ = (Vcl::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall ClearColorMatrix(Vcl::Tmsfncgdiplusapi::ColorAdjustType type_ = (Vcl::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetColorMatrices(const Vcl::Tmsfncgdiplusapi::ColorMatrix &colorMatrix, const Vcl::Tmsfncgdiplusapi::ColorMatrix &grayMatrix, Vcl::Tmsfncgdiplusapi::ColorMatrixFlags mode = (Vcl::Tmsfncgdiplusapi::ColorMatrixFlags)(0x0), Vcl::Tmsfncgdiplusapi::ColorAdjustType type_ = (Vcl::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall ClearColorMatrices(Vcl::Tmsfncgdiplusapi::ColorAdjustType Type_ = (Vcl::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetThreshold(float threshold, Vcl::Tmsfncgdiplusapi::ColorAdjustType type_ = (Vcl::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall ClearThreshold(Vcl::Tmsfncgdiplusapi::ColorAdjustType type_ = (Vcl::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetGamma(float gamma, Vcl::Tmsfncgdiplusapi::ColorAdjustType type_ = (Vcl::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall ClearGamma(Vcl::Tmsfncgdiplusapi::ColorAdjustType type_ = (Vcl::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetNoOp(Vcl::Tmsfncgdiplusapi::ColorAdjustType type_ = (Vcl::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall ClearNoOp(Vcl::Tmsfncgdiplusapi::ColorAdjustType Type_ = (Vcl::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetColorKey(unsigned colorLow, unsigned colorHigh, Vcl::Tmsfncgdiplusapi::ColorAdjustType type_ = (Vcl::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall ClearColorKey(Vcl::Tmsfncgdiplusapi::ColorAdjustType type_ = (Vcl::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetOutputChannel(Vcl::Tmsfncgdiplusapi::ColorChannelFlags channelFlags, Vcl::Tmsfncgdiplusapi::ColorAdjustType type_ = (Vcl::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall ClearOutputChannel(Vcl::Tmsfncgdiplusapi::ColorAdjustType type_ = (Vcl::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetOutputChannelColorProfile(System::WideString colorProfileFilename, Vcl::Tmsfncgdiplusapi::ColorAdjustType type_ = (Vcl::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall ClearOutputChannelColorProfile(Vcl::Tmsfncgdiplusapi::ColorAdjustType type_ = (Vcl::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetRemapTable(unsigned mapSize, Vcl::Tmsfncgdiplusapi::PColorMap map, Vcl::Tmsfncgdiplusapi::ColorAdjustType type_ = (Vcl::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall ClearRemapTable(Vcl::Tmsfncgdiplusapi::ColorAdjustType type_ = (Vcl::Tmsfncgdiplusapi::ColorAdjustType)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetBrushRemapTable(unsigned mapSize, Vcl::Tmsfncgdiplusapi::PColorMap map);
	Vcl::Tmsfncgdiplusapi::Status __fastcall ClearBrushRemapTable();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetWrapMode(Vcl::Tmsfncgdiplusapi::WrapMode wrap, unsigned color = (unsigned)(0xff000000), System::LongBool clamp = false);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetAdjustedPalette(Vcl::Tmsfncgdiplusapi::PColorPalette colorPalette, Vcl::Tmsfncgdiplusapi::ColorAdjustType colorAdjustType);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};

#pragma pack(pop)

typedef System::StaticArray<float, 6> TMatrixArray;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPMatrix : public Vcl::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Vcl::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeMatrix;
	Vcl::Tmsfncgdiplusapi::Status lastResult;
	void __fastcall SetNativeMatrix(void * ANativeMatrix);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetStatus(Vcl::Tmsfncgdiplusapi::Status status);
	
public:
	__fastcall TGPMatrix(void * ANativeMatrix)/* overload */;
	__fastcall TGPMatrix()/* overload */;
	__fastcall TGPMatrix(float m11, float m12, float m21, float m22, float dx, float dy)/* overload */;
	__fastcall TGPMatrix(const Vcl::Tmsfncgdiplusapi::TGPRectF &rect, const Vcl::Tmsfncgdiplusapi::TGPPointF &dstplg)/* overload */;
	__fastcall TGPMatrix(const Vcl::Tmsfncgdiplusapi::TGPRect &rect, const Vcl::Tmsfncgdiplusapi::TGPPoint &dstplg)/* overload */;
	__fastcall virtual ~TGPMatrix();
	TGPMatrix* __fastcall Clone();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetElements(const TMatrixArray &m);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetElements(float m11, float m12, float m21, float m22, float dx, float dy);
	float __fastcall OffsetX();
	float __fastcall OffsetY();
	Vcl::Tmsfncgdiplusapi::Status __fastcall Reset();
	Vcl::Tmsfncgdiplusapi::Status __fastcall Multiply(TGPMatrix* matrix, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall Translate(float offX, float offY, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall Scale(float scaleX, float scaleY, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall Rotate(float angle, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall RotateAt(float angle, const Vcl::Tmsfncgdiplusapi::TGPPointF &center, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall Shear(float shearX, float shearY, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall Invert();
	Vcl::Tmsfncgdiplusapi::Status __fastcall TransformPoints(Vcl::Tmsfncgdiplusapi::PGPPointF pts, int count = 0x1)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall TransformPoints(Vcl::Tmsfncgdiplusapi::PGPPoint pts, int count = 0x1)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall TransformVectors(Vcl::Tmsfncgdiplusapi::PGPPointF pts, int count = 0x1)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall TransformVectors(Vcl::Tmsfncgdiplusapi::PGPPoint pts, int count = 0x1)/* overload */;
	System::LongBool __fastcall IsInvertible();
	System::LongBool __fastcall IsIdentity();
	System::LongBool __fastcall IsEqual(TGPMatrix* matrix);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPBrush : public Vcl::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Vcl::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeBrush;
	Vcl::Tmsfncgdiplusapi::Status lastResult;
	void __fastcall SetNativeBrush(void * ANativeBrush);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetStatus(Vcl::Tmsfncgdiplusapi::Status status);
	
public:
	__fastcall TGPBrush(void * ANativeBrush, Vcl::Tmsfncgdiplusapi::Status status)/* overload */;
	__fastcall TGPBrush()/* overload */;
	__fastcall virtual ~TGPBrush();
	virtual TGPBrush* __fastcall Clone();
	Vcl::Tmsfncgdiplusapi::BrushType __fastcall GetType();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPSolidBrush : public TGPBrush
{
	typedef TGPBrush inherited;
	
public:
	__fastcall TGPSolidBrush(unsigned color)/* overload */;
	__fastcall TGPSolidBrush()/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetColor(/* out */ unsigned &color);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetColor(unsigned color);
public:
	/* TGPBrush.Create */ inline __fastcall TGPSolidBrush(void * ANativeBrush, Vcl::Tmsfncgdiplusapi::Status status)/* overload */ : TGPBrush(ANativeBrush, status) { }
	/* TGPBrush.Destroy */ inline __fastcall virtual ~TGPSolidBrush() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPTextureBrush : public TGPBrush
{
	typedef TGPBrush inherited;
	
public:
	__fastcall TGPTextureBrush(TGPImage* image, Vcl::Tmsfncgdiplusapi::WrapMode wrapMode)/* overload */;
	__fastcall TGPTextureBrush(TGPImage* image, Vcl::Tmsfncgdiplusapi::WrapMode wrapMode, const Vcl::Tmsfncgdiplusapi::TGPRectF &dstRect)/* overload */;
	__fastcall TGPTextureBrush(TGPImage* image, const Vcl::Tmsfncgdiplusapi::TGPRectF &dstRect, TGPImageAttributes* imageAttributes)/* overload */;
	__fastcall TGPTextureBrush(TGPImage* image, const Vcl::Tmsfncgdiplusapi::TGPRect &dstRect, TGPImageAttributes* imageAttributes)/* overload */;
	__fastcall TGPTextureBrush(TGPImage* image, Vcl::Tmsfncgdiplusapi::WrapMode wrapMode, const Vcl::Tmsfncgdiplusapi::TGPRect &dstRect)/* overload */;
	__fastcall TGPTextureBrush(TGPImage* image, Vcl::Tmsfncgdiplusapi::WrapMode wrapMode, float dstX, float dstY, float dstWidth, float dstHeight)/* overload */;
	__fastcall TGPTextureBrush(TGPImage* image, Vcl::Tmsfncgdiplusapi::WrapMode wrapMode, int dstX, int dstY, int dstWidth, int dstHeight)/* overload */;
	__fastcall TGPTextureBrush()/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetTransform(TGPMatrix* matrix);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetTransform(TGPMatrix* matrix);
	Vcl::Tmsfncgdiplusapi::Status __fastcall ResetTransform();
	Vcl::Tmsfncgdiplusapi::Status __fastcall MultiplyTransform(TGPMatrix* matrix, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall TranslateTransform(float dx, float dy, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall ScaleTransform(float sx, float sy, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall RotateTransform(float angle, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetWrapMode(Vcl::Tmsfncgdiplusapi::WrapMode wrapMode);
	Vcl::Tmsfncgdiplusapi::WrapMode __fastcall GetWrapMode();
	TGPImage* __fastcall GetImage();
public:
	/* TGPBrush.Create */ inline __fastcall TGPTextureBrush(void * ANativeBrush, Vcl::Tmsfncgdiplusapi::Status status)/* overload */ : TGPBrush(ANativeBrush, status) { }
	/* TGPBrush.Destroy */ inline __fastcall virtual ~TGPTextureBrush() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPLinearGradientBrush : public TGPBrush
{
	typedef TGPBrush inherited;
	
public:
	__fastcall TGPLinearGradientBrush()/* overload */;
	__fastcall TGPLinearGradientBrush(const Vcl::Tmsfncgdiplusapi::TGPPointF &point1, const Vcl::Tmsfncgdiplusapi::TGPPointF &point2, unsigned color1, unsigned color2)/* overload */;
	__fastcall TGPLinearGradientBrush(const Vcl::Tmsfncgdiplusapi::TGPPoint &point1, const Vcl::Tmsfncgdiplusapi::TGPPoint &point2, unsigned color1, unsigned color2)/* overload */;
	__fastcall TGPLinearGradientBrush(const Vcl::Tmsfncgdiplusapi::TGPRectF &rect, unsigned color1, unsigned color2, Vcl::Tmsfncgdiplusapi::LinearGradientMode mode)/* overload */;
	__fastcall TGPLinearGradientBrush(const Vcl::Tmsfncgdiplusapi::TGPRect &rect, unsigned color1, unsigned color2, Vcl::Tmsfncgdiplusapi::LinearGradientMode mode)/* overload */;
	__fastcall TGPLinearGradientBrush(const Vcl::Tmsfncgdiplusapi::TGPRectF &rect, unsigned color1, unsigned color2, float angle, System::LongBool isAngleScalable)/* overload */;
	__fastcall TGPLinearGradientBrush(const Vcl::Tmsfncgdiplusapi::TGPRect &rect, unsigned color1, unsigned color2, float angle, System::LongBool isAngleScalable)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetLinearColors(unsigned color1, unsigned color2);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetLinearColors(/* out */ unsigned &color1, /* out */ unsigned &color2);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetRectangle(/* out */ Vcl::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetRectangle(/* out */ Vcl::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetGammaCorrection(System::LongBool useGammaCorrection);
	System::LongBool __fastcall GetGammaCorrection();
	int __fastcall GetBlendCount();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetBlend(Winapi::Windows::PSingle blendFactors, Winapi::Windows::PSingle blendPositions, int count);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetBlend(Winapi::Windows::PSingle blendFactors, Winapi::Windows::PSingle blendPositions, int count);
	int __fastcall GetInterpolationColorCount();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetInterpolationColors(Vcl::Tmsfncgdiplusapi::PGPColor presetColors, Winapi::Windows::PSingle blendPositions, int count);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetInterpolationColors(Vcl::Tmsfncgdiplusapi::PGPColor presetColors, Winapi::Windows::PSingle blendPositions, int count);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetBlendBellShape(float focus, float scale = 1.000000E+00f);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetBlendTriangularShape(float focus, float scale = 1.000000E+00f);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetTransform(TGPMatrix* matrix);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetTransform(TGPMatrix* matrix);
	Vcl::Tmsfncgdiplusapi::Status __fastcall ResetTransform();
	Vcl::Tmsfncgdiplusapi::Status __fastcall MultiplyTransform(TGPMatrix* matrix, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall TranslateTransform(float dx, float dy, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall ScaleTransform(float sx, float sy, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall RotateTransform(float angle, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetWrapMode(Vcl::Tmsfncgdiplusapi::WrapMode wrapMode);
	Vcl::Tmsfncgdiplusapi::WrapMode __fastcall GetWrapMode();
public:
	/* TGPBrush.Create */ inline __fastcall TGPLinearGradientBrush(void * ANativeBrush, Vcl::Tmsfncgdiplusapi::Status status)/* overload */ : TGPBrush(ANativeBrush, status) { }
	/* TGPBrush.Destroy */ inline __fastcall virtual ~TGPLinearGradientBrush() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPHatchBrush : public TGPBrush
{
	typedef TGPBrush inherited;
	
public:
	__fastcall TGPHatchBrush()/* overload */;
	__fastcall TGPHatchBrush(Vcl::Tmsfncgdiplusapi::HatchStyle hatchStyle, unsigned foreColor, unsigned backColor)/* overload */;
	Vcl::Tmsfncgdiplusapi::HatchStyle __fastcall GetHatchStyle();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetForegroundColor(/* out */ unsigned &color);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetBackgroundColor(/* out */ unsigned &color);
public:
	/* TGPBrush.Create */ inline __fastcall TGPHatchBrush(void * ANativeBrush, Vcl::Tmsfncgdiplusapi::Status status)/* overload */ : TGPBrush(ANativeBrush, status) { }
	/* TGPBrush.Destroy */ inline __fastcall virtual ~TGPHatchBrush() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPPen : public Vcl::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Vcl::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativePen;
	Vcl::Tmsfncgdiplusapi::Status lastResult;
	void __fastcall SetNativePen(void * ANativePen);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetStatus(Vcl::Tmsfncgdiplusapi::Status status);
	
public:
	__fastcall TGPPen(void * ANativePen, Vcl::Tmsfncgdiplusapi::Status status)/* overload */;
	__fastcall TGPPen(unsigned color, float width)/* overload */;
	__fastcall TGPPen(TGPBrush* brush, float width)/* overload */;
	__fastcall virtual ~TGPPen();
	TGPPen* __fastcall Clone();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetWidth(float width);
	float __fastcall GetWidth();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetLineCap(int startCap, int endCap, int dashCap);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetStartCap(int startCap);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetEndCap(int endCap);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetDashCap(int dashCap);
	int __fastcall GetStartCap();
	int __fastcall GetEndCap();
	int __fastcall GetDashCap();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetLineJoin(Vcl::Tmsfncgdiplusapi::LineJoin lineJoin);
	Vcl::Tmsfncgdiplusapi::LineJoin __fastcall GetLineJoin();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetCustomStartCap(TGPCustomLineCap* customCap);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetCustomStartCap(TGPCustomLineCap* customCap);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetCustomEndCap(TGPCustomLineCap* customCap);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetCustomEndCap(TGPCustomLineCap* customCap);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetMiterLimit(float miterLimit);
	float __fastcall GetMiterLimit();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetAlignment(Vcl::Tmsfncgdiplusapi::PenAlignment penAlignment);
	Vcl::Tmsfncgdiplusapi::PenAlignment __fastcall GetAlignment();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetTransform(TGPMatrix* matrix);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetTransform(TGPMatrix* matrix);
	Vcl::Tmsfncgdiplusapi::Status __fastcall ResetTransform();
	Vcl::Tmsfncgdiplusapi::Status __fastcall MultiplyTransform(TGPMatrix* matrix, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall TranslateTransform(float dx, float dy, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall ScaleTransform(float sx, float sy, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall RotateTransform(float angle, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	int __fastcall GetPenType();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetColor(unsigned color);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetBrush(TGPBrush* brush);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetColor(/* out */ unsigned &Color);
	TGPBrush* __fastcall GetBrush();
	Vcl::Tmsfncgdiplusapi::DashStyle __fastcall GetDashStyle();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetDashStyle(Vcl::Tmsfncgdiplusapi::DashStyle dashStyle);
	float __fastcall GetDashOffset();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetDashOffset(float dashOffset);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetDashPattern(Winapi::Windows::PSingle dashArray, int count);
	int __fastcall GetDashPatternCount();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetDashPattern(Winapi::Windows::PSingle dashArray, int count);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetCompoundArray(Winapi::Windows::PSingle compoundArray, int count);
	int __fastcall GetCompoundArrayCount();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetCompoundArray(Winapi::Windows::PSingle compoundArray, int count);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPStringFormat : public Vcl::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Vcl::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeFormat;
	Vcl::Tmsfncgdiplusapi::Status lastError;
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetStatus(Vcl::Tmsfncgdiplusapi::Status newStatus);
	void __fastcall Assign(TGPStringFormat* source);
	__fastcall TGPStringFormat(void * clonedStringFormat, Vcl::Tmsfncgdiplusapi::Status status)/* overload */;
	
public:
	__fastcall TGPStringFormat(int formatFlags, System::Word language)/* overload */;
	__fastcall TGPStringFormat(TGPStringFormat* format)/* overload */;
	__fastcall virtual ~TGPStringFormat();
	__classmethod TGPStringFormat* __fastcall GenericDefault();
	__classmethod TGPStringFormat* __fastcall GenericTypographic();
	TGPStringFormat* __fastcall Clone();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetFormatFlags(int flags);
	int __fastcall GetFormatFlags();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetAlignment(Vcl::Tmsfncgdiplusapi::StringAlignment align);
	Vcl::Tmsfncgdiplusapi::StringAlignment __fastcall GetAlignment();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetLineAlignment(Vcl::Tmsfncgdiplusapi::StringAlignment align);
	Vcl::Tmsfncgdiplusapi::StringAlignment __fastcall GetLineAlignment();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetHotkeyPrefix(Vcl::Tmsfncgdiplusapi::HotkeyPrefix hotkeyPrefix);
	Vcl::Tmsfncgdiplusapi::HotkeyPrefix __fastcall GetHotkeyPrefix();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetTabStops(float firstTabOffset, int count, Winapi::Windows::PSingle tabStops);
	int __fastcall GetTabStopCount();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetTabStops(int count, Winapi::Windows::PSingle firstTabOffset, Winapi::Windows::PSingle tabStops);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetDigitSubstitution(System::Word language, Vcl::Tmsfncgdiplusapi::StringDigitSubstitute substitute);
	System::Word __fastcall GetDigitSubstitutionLanguage();
	Vcl::Tmsfncgdiplusapi::StringDigitSubstitute __fastcall GetDigitSubstitutionMethod();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetTrimming(Vcl::Tmsfncgdiplusapi::StringTrimming trimming);
	Vcl::Tmsfncgdiplusapi::StringTrimming __fastcall GetTrimming();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetMeasurableCharacterRanges(int rangeCount, Vcl::Tmsfncgdiplusapi::PCharacterRange ranges);
	int __fastcall GetMeasurableCharacterRangeCount();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPGraphicsPath : public Vcl::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Vcl::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativePath;
	Vcl::Tmsfncgdiplusapi::Status lastResult;
	void __fastcall SetNativePath(void * ANativePath);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetStatus(Vcl::Tmsfncgdiplusapi::Status status);
	__fastcall TGPGraphicsPath(void * ANativePath)/* overload */;
	
public:
	__fastcall TGPGraphicsPath(TGPGraphicsPath* path)/* overload */;
	__fastcall TGPGraphicsPath(Vcl::Tmsfncgdiplusapi::FillMode fillMode)/* overload */;
	__fastcall TGPGraphicsPath(Vcl::Tmsfncgdiplusapi::PGPPointF points, System::PByte types, int count, Vcl::Tmsfncgdiplusapi::FillMode fillMode)/* overload */;
	__fastcall TGPGraphicsPath(Vcl::Tmsfncgdiplusapi::PGPPoint points, System::PByte types, int count, Vcl::Tmsfncgdiplusapi::FillMode fillMode)/* overload */;
	__fastcall virtual ~TGPGraphicsPath();
	TGPGraphicsPath* __fastcall Clone();
	Vcl::Tmsfncgdiplusapi::Status __fastcall Reset();
	Vcl::Tmsfncgdiplusapi::FillMode __fastcall GetFillMode();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetFillMode(Vcl::Tmsfncgdiplusapi::FillMode fillmode);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetPathData(Vcl::Tmsfncgdiplusapi::TPathData* pathData);
	Vcl::Tmsfncgdiplusapi::Status __fastcall StartFigure();
	Vcl::Tmsfncgdiplusapi::Status __fastcall CloseFigure();
	Vcl::Tmsfncgdiplusapi::Status __fastcall CloseAllFigures();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetMarker();
	Vcl::Tmsfncgdiplusapi::Status __fastcall ClearMarkers();
	Vcl::Tmsfncgdiplusapi::Status __fastcall Reverse();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetLastPoint(/* out */ Vcl::Tmsfncgdiplusapi::TGPPointF &lastPoint);
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddLine(const Vcl::Tmsfncgdiplusapi::TGPPointF &pt1, const Vcl::Tmsfncgdiplusapi::TGPPointF &pt2)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddLine(float x1, float y1, float x2, float y2)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddLines(Vcl::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddLine(const Vcl::Tmsfncgdiplusapi::TGPPoint &pt1, const Vcl::Tmsfncgdiplusapi::TGPPoint &pt2)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddLine(int x1, int y1, int x2, int y2)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddLines(Vcl::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddArc(const Vcl::Tmsfncgdiplusapi::TGPRectF &rect, float startAngle, float sweepAngle)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddArc(float x, float y, float width, float height, float startAngle, float sweepAngle)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddArc(const Vcl::Tmsfncgdiplusapi::TGPRect &rect, float startAngle, float sweepAngle)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddArc(int x, int y, int width, int height, float startAngle, float sweepAngle)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddBezier(const Vcl::Tmsfncgdiplusapi::TGPPointF &pt1, const Vcl::Tmsfncgdiplusapi::TGPPointF &pt2, const Vcl::Tmsfncgdiplusapi::TGPPointF &pt3, const Vcl::Tmsfncgdiplusapi::TGPPointF &pt4)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddBezier(float x1, float y1, float x2, float y2, float x3, float y3, float x4, float y4)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddBeziers(Vcl::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddBezier(const Vcl::Tmsfncgdiplusapi::TGPPoint &pt1, const Vcl::Tmsfncgdiplusapi::TGPPoint &pt2, const Vcl::Tmsfncgdiplusapi::TGPPoint &pt3, const Vcl::Tmsfncgdiplusapi::TGPPoint &pt4)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddBezier(int x1, int y1, int x2, int y2, int x3, int y3, int x4, int y4)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddBeziers(Vcl::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddCurve(Vcl::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddCurve(Vcl::Tmsfncgdiplusapi::PGPPointF points, int count, float tension)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddCurve(Vcl::Tmsfncgdiplusapi::PGPPointF points, int count, int offset, int numberOfSegments, float tension)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddCurve(Vcl::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddCurve(Vcl::Tmsfncgdiplusapi::PGPPoint points, int count, float tension)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddCurve(Vcl::Tmsfncgdiplusapi::PGPPoint points, int count, int offset, int numberOfSegments, float tension)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddClosedCurve(Vcl::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddClosedCurve(Vcl::Tmsfncgdiplusapi::PGPPointF points, int count, float tension)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddClosedCurve(Vcl::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddClosedCurve(Vcl::Tmsfncgdiplusapi::PGPPoint points, int count, float tension)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddRectangle(const Vcl::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddRectangles(Vcl::Tmsfncgdiplusapi::PGPRectF rects, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddRectangle(const Vcl::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddRectangles(Vcl::Tmsfncgdiplusapi::PGPRect rects, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddEllipse(const Vcl::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddEllipse(float x, float y, float width, float height)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddEllipse(const Vcl::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddEllipse(int x, int y, int width, int height)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddPie(const Vcl::Tmsfncgdiplusapi::TGPRectF &rect, float startAngle, float sweepAngle)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddPie(float x, float y, float width, float height, float startAngle, float sweepAngle)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddPie(const Vcl::Tmsfncgdiplusapi::TGPRect &rect, float startAngle, float sweepAngle)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddPie(int x, int y, int width, int height, float startAngle, float sweepAngle)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddPolygon(Vcl::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddPolygon(Vcl::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddPath(TGPGraphicsPath* addingPath, System::LongBool connect);
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddString(System::WideString string_, int length, TGPFontFamily* family, int style, float emSize, const Vcl::Tmsfncgdiplusapi::TGPPointF &origin, TGPStringFormat* format)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddString(System::WideString string_, int length, TGPFontFamily* family, int style, float emSize, const Vcl::Tmsfncgdiplusapi::TGPRectF &layoutRect, TGPStringFormat* format)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddString(System::WideString string_, int length, TGPFontFamily* family, int style, float emSize, const Vcl::Tmsfncgdiplusapi::TGPPoint &origin, TGPStringFormat* format)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddString(System::WideString string_, int length, TGPFontFamily* family, int style, float emSize, const Vcl::Tmsfncgdiplusapi::TGPRect &layoutRect, TGPStringFormat* format)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Transform(TGPMatrix* matrix);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetBounds(/* out */ Vcl::Tmsfncgdiplusapi::TGPRectF &bounds, TGPMatrix* matrix = (TGPMatrix*)(0x0), TGPPen* pen = (TGPPen*)(0x0))/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetBounds(/* out */ Vcl::Tmsfncgdiplusapi::TGPRect &bounds, TGPMatrix* matrix = (TGPMatrix*)(0x0), TGPPen* pen = (TGPPen*)(0x0))/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Flatten(TGPMatrix* matrix = (TGPMatrix*)(0x0), float flatness = 2.500000E-01f);
	Vcl::Tmsfncgdiplusapi::Status __fastcall Widen(TGPPen* pen, TGPMatrix* matrix = (TGPMatrix*)(0x0), float flatness = 2.500000E-01f);
	Vcl::Tmsfncgdiplusapi::Status __fastcall Outline(TGPMatrix* matrix = (TGPMatrix*)(0x0), float flatness = 2.500000E-01f);
	Vcl::Tmsfncgdiplusapi::Status __fastcall Warp(Vcl::Tmsfncgdiplusapi::PGPPointF destPoints, int count, const Vcl::Tmsfncgdiplusapi::TGPRectF &srcRect, TGPMatrix* matrix = (TGPMatrix*)(0x0), Vcl::Tmsfncgdiplusapi::WarpMode warpMode = (Vcl::Tmsfncgdiplusapi::WarpMode)(0x0), float flatness = 2.500000E-01f);
	int __fastcall GetPointCount();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetPathTypes(System::PByte types, int count);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetPathPoints(Vcl::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetPathPoints(Vcl::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
	System::LongBool __fastcall IsVisible(const Vcl::Tmsfncgdiplusapi::TGPPointF &point, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsVisible(float x, float y, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsVisible(const Vcl::Tmsfncgdiplusapi::TGPPoint &point, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsVisible(int x, int y, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsOutlineVisible(const Vcl::Tmsfncgdiplusapi::TGPPointF &point, TGPPen* pen, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsOutlineVisible(float x, float y, TGPPen* pen, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsOutlineVisible(const Vcl::Tmsfncgdiplusapi::TGPPoint &point, TGPPen* pen, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
	System::LongBool __fastcall IsOutlineVisible(int x, int y, TGPPen* pen, TGPGraphics* g = (TGPGraphics*)(0x0))/* overload */;
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPGraphicsPathIterator : public Vcl::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Vcl::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeIterator;
	Vcl::Tmsfncgdiplusapi::Status lastResult;
	void __fastcall SetNativeIterator(void * ANativeIterator);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetStatus(Vcl::Tmsfncgdiplusapi::Status status);
	
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
	int __fastcall Enumerate(Vcl::Tmsfncgdiplusapi::PGPPointF points, System::PByte types, int count);
	int __fastcall CopyData(Vcl::Tmsfncgdiplusapi::PGPPointF points, System::PByte types, int startIndex, int endIndex);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPPathGradientBrush : public TGPBrush
{
	typedef TGPBrush inherited;
	
public:
	__fastcall TGPPathGradientBrush(Vcl::Tmsfncgdiplusapi::PGPPointF points, int count, Vcl::Tmsfncgdiplusapi::WrapMode wrapMode)/* overload */;
	__fastcall TGPPathGradientBrush(Vcl::Tmsfncgdiplusapi::PGPPoint points, int count, Vcl::Tmsfncgdiplusapi::WrapMode wrapMode)/* overload */;
	__fastcall TGPPathGradientBrush(TGPGraphicsPath* path)/* overload */;
	__fastcall TGPPathGradientBrush()/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetCenterColor(/* out */ unsigned &Color);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetCenterColor(unsigned color);
	int __fastcall GetPointCount();
	int __fastcall GetSurroundColorCount();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetSurroundColors(Vcl::Tmsfncgdiplusapi::PARGB colors, int &count);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetSurroundColors(Vcl::Tmsfncgdiplusapi::PARGB colors, int &count);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetGraphicsPath(TGPGraphicsPath* path);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetGraphicsPath(TGPGraphicsPath* path);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetCenterPoint(/* out */ Vcl::Tmsfncgdiplusapi::TGPPointF &point)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetCenterPoint(/* out */ Vcl::Tmsfncgdiplusapi::TGPPoint &point)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetCenterPoint(const Vcl::Tmsfncgdiplusapi::TGPPointF &point)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetCenterPoint(const Vcl::Tmsfncgdiplusapi::TGPPoint &point)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetRectangle(/* out */ Vcl::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetRectangle(/* out */ Vcl::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetGammaCorrection(System::LongBool useGammaCorrection)/* overload */;
	System::LongBool __fastcall GetGammaCorrection()/* overload */;
	int __fastcall GetBlendCount();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetBlend(Winapi::Windows::PSingle blendFactors, Winapi::Windows::PSingle blendPositions, int count);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetBlend(Winapi::Windows::PSingle blendFactors, Winapi::Windows::PSingle blendPositions, int count);
	int __fastcall GetInterpolationColorCount();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetInterpolationColors(Vcl::Tmsfncgdiplusapi::PARGB presetColors, Winapi::Windows::PSingle blendPositions, int count);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetInterpolationColors(Vcl::Tmsfncgdiplusapi::PARGB presetColors, Winapi::Windows::PSingle blendPositions, int count);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetBlendBellShape(float focus, float scale = 1.000000E+00f);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetBlendTriangularShape(float focus, float scale = 1.000000E+00f);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetTransform(TGPMatrix* matrix);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetTransform(TGPMatrix* matrix);
	Vcl::Tmsfncgdiplusapi::Status __fastcall ResetTransform();
	Vcl::Tmsfncgdiplusapi::Status __fastcall MultiplyTransform(TGPMatrix* matrix, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall TranslateTransform(float dx, float dy, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall ScaleTransform(float sx, float sy, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall RotateTransform(float angle, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetFocusScales(/* out */ float &xScale, /* out */ float &yScale);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetFocusScales(float xScale, float yScale);
	Vcl::Tmsfncgdiplusapi::WrapMode __fastcall GetWrapMode();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetWrapMode(Vcl::Tmsfncgdiplusapi::WrapMode wrapMode);
public:
	/* TGPBrush.Create */ inline __fastcall TGPPathGradientBrush(void * ANativeBrush, Vcl::Tmsfncgdiplusapi::Status status)/* overload */ : TGPBrush(ANativeBrush, status) { }
	/* TGPBrush.Destroy */ inline __fastcall virtual ~TGPPathGradientBrush() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPGraphics : public Vcl::Tmsfncgdiplusapi::TGdiplusBase
{
	typedef Vcl::Tmsfncgdiplusapi::TGdiplusBase inherited;
	
protected:
	void *nativeGraphics;
	Vcl::Tmsfncgdiplusapi::Status lastResult;
	void __fastcall SetNativeGraphics(void * graphics);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetStatus(Vcl::Tmsfncgdiplusapi::Status status);
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
	void __fastcall Flush(Vcl::Tmsfncgdiplusapi::FlushIntention intention = (Vcl::Tmsfncgdiplusapi::FlushIntention)(0x0));
	HDC __fastcall GetHDC();
	void __fastcall ReleaseHDC(HDC hdc);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetRenderingOrigin(int x, int y);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetRenderingOrigin(/* out */ int &x, /* out */ int &y);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetCompositingMode(Vcl::Tmsfncgdiplusapi::CompositingMode compositingMode);
	Vcl::Tmsfncgdiplusapi::CompositingMode __fastcall GetCompositingMode();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetCompositingQuality(int compositingQuality);
	int __fastcall GetCompositingQuality();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetTextRenderingHint(Vcl::Tmsfncgdiplusapi::TextRenderingHint newMode);
	Vcl::Tmsfncgdiplusapi::TextRenderingHint __fastcall GetTextRenderingHint();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetTextContrast(unsigned contrast);
	unsigned __fastcall GetTextContrast();
	int __fastcall GetInterpolationMode();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetInterpolationMode(int interpolationMode);
	int __fastcall GetSmoothingMode();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetSmoothingMode(int smoothingMode);
	int __fastcall GetPixelOffsetMode();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetPixelOffsetMode(int pixelOffsetMode);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetTransform(TGPMatrix* matrix);
	Vcl::Tmsfncgdiplusapi::Status __fastcall ResetTransform();
	Vcl::Tmsfncgdiplusapi::Status __fastcall MultiplyTransform(TGPMatrix* matrix, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall TranslateTransform(float dx, float dy, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall ScaleTransform(float sx, float sy, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall RotateTransform(float angle, Vcl::Tmsfncgdiplusapi::MatrixOrder order = (Vcl::Tmsfncgdiplusapi::MatrixOrder)(0x0));
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetTransform(TGPMatrix* matrix);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetPageUnit(Vcl::Tmsfncgdiplusapi::Unit_ unit_);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetPageScale(float scale);
	Vcl::Tmsfncgdiplusapi::Unit_ __fastcall GetPageUnit();
	float __fastcall GetPageScale();
	float __fastcall GetDpiX();
	float __fastcall GetDpiY();
	Vcl::Tmsfncgdiplusapi::Status __fastcall TransformPoints(Vcl::Tmsfncgdiplusapi::CoordinateSpace destSpace, Vcl::Tmsfncgdiplusapi::CoordinateSpace srcSpace, Vcl::Tmsfncgdiplusapi::PGPPointF pts, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall TransformPoints(Vcl::Tmsfncgdiplusapi::CoordinateSpace destSpace, Vcl::Tmsfncgdiplusapi::CoordinateSpace srcSpace, Vcl::Tmsfncgdiplusapi::PGPPoint pts, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetNearestColor(unsigned &color);
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawLine(TGPPen* pen, float x1, float y1, float x2, float y2)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawLine(TGPPen* pen, const Vcl::Tmsfncgdiplusapi::TGPPointF &pt1, const Vcl::Tmsfncgdiplusapi::TGPPointF &pt2)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawLines(TGPPen* pen, Vcl::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawLine(TGPPen* pen, int x1, int y1, int x2, int y2)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawLine(TGPPen* pen, const Vcl::Tmsfncgdiplusapi::TGPPoint &pt1, const Vcl::Tmsfncgdiplusapi::TGPPoint &pt2)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawLines(TGPPen* pen, Vcl::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawArc(TGPPen* pen, float x, float y, float width, float height, float startAngle, float sweepAngle)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawArc(TGPPen* pen, const Vcl::Tmsfncgdiplusapi::TGPRectF &rect, float startAngle, float sweepAngle)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawArc(TGPPen* pen, int x, int y, int width, int height, float startAngle, float sweepAngle)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawArc(TGPPen* pen, const Vcl::Tmsfncgdiplusapi::TGPRect &rect, float startAngle, float sweepAngle)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawBezier(TGPPen* pen, float x1, float y1, float x2, float y2, float x3, float y3, float x4, float y4)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawBezier(TGPPen* pen, const Vcl::Tmsfncgdiplusapi::TGPPointF &pt1, const Vcl::Tmsfncgdiplusapi::TGPPointF &pt2, const Vcl::Tmsfncgdiplusapi::TGPPointF &pt3, const Vcl::Tmsfncgdiplusapi::TGPPointF &pt4)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawBeziers(TGPPen* pen, Vcl::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawBezier(TGPPen* pen, int x1, int y1, int x2, int y2, int x3, int y3, int x4, int y4)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawBezier(TGPPen* pen, const Vcl::Tmsfncgdiplusapi::TGPPoint &pt1, const Vcl::Tmsfncgdiplusapi::TGPPoint &pt2, const Vcl::Tmsfncgdiplusapi::TGPPoint &pt3, const Vcl::Tmsfncgdiplusapi::TGPPoint &pt4)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawBeziers(TGPPen* pen, Vcl::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawRectangle(TGPPen* pen, const Vcl::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawRectangle(TGPPen* pen, float x, float y, float width, float height)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawRectangles(TGPPen* pen, Vcl::Tmsfncgdiplusapi::PGPRectF rects, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawRectangle(TGPPen* pen, const Vcl::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawRectangle(TGPPen* pen, int x, int y, int width, int height)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawRectangles(TGPPen* pen, Vcl::Tmsfncgdiplusapi::PGPRect rects, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawEllipse(TGPPen* pen, const Vcl::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawEllipse(TGPPen* pen, float x, float y, float width, float height)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawEllipse(TGPPen* pen, const Vcl::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawEllipse(TGPPen* pen, int x, int y, int width, int height)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawPie(TGPPen* pen, const Vcl::Tmsfncgdiplusapi::TGPRectF &rect, float startAngle, float sweepAngle)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawPie(TGPPen* pen, float x, float y, float width, float height, float startAngle, float sweepAngle)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawPie(TGPPen* pen, const Vcl::Tmsfncgdiplusapi::TGPRect &rect, float startAngle, float sweepAngle)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawPie(TGPPen* pen, int x, int y, int width, int height, float startAngle, float sweepAngle)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawPolygon(TGPPen* pen, Vcl::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawPolygon(TGPPen* pen, Vcl::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawPath(TGPPen* pen, TGPGraphicsPath* path);
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawCurve(TGPPen* pen, Vcl::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawCurve(TGPPen* pen, Vcl::Tmsfncgdiplusapi::PGPPointF points, int count, float tension)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawCurve(TGPPen* pen, Vcl::Tmsfncgdiplusapi::PGPPointF points, int count, int offset, int numberOfSegments, float tension = 5.000000E-01f)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawCurve(TGPPen* pen, Vcl::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawCurve(TGPPen* pen, Vcl::Tmsfncgdiplusapi::PGPPoint points, int count, float tension)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawCurve(TGPPen* pen, Vcl::Tmsfncgdiplusapi::PGPPoint points, int count, int offset, int numberOfSegments, float tension = 5.000000E-01f)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawClosedCurve(TGPPen* pen, Vcl::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawClosedCurve(TGPPen* pen, Vcl::Tmsfncgdiplusapi::PGPPointF points, int count, float tension)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawClosedCurve(TGPPen* pen, Vcl::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawClosedCurve(TGPPen* pen, Vcl::Tmsfncgdiplusapi::PGPPoint points, int count, float tension)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall Clear(unsigned color);
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillRectangle(TGPBrush* brush, const Vcl::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillRectangle(TGPBrush* brush, float x, float y, float width, float height)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillRectangles(TGPBrush* brush, Vcl::Tmsfncgdiplusapi::PGPRectF rects, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillRectangle(TGPBrush* brush, const Vcl::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillRectangle(TGPBrush* brush, int x, int y, int width, int height)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillRectangles(TGPBrush* brush, Vcl::Tmsfncgdiplusapi::PGPRect rects, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillPolygon(TGPBrush* brush, Vcl::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillPolygon(TGPBrush* brush, Vcl::Tmsfncgdiplusapi::PGPPointF points, int count, Vcl::Tmsfncgdiplusapi::FillMode fillMode)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillPolygon(TGPBrush* brush, Vcl::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillPolygon(TGPBrush* brush, Vcl::Tmsfncgdiplusapi::PGPPoint points, int count, Vcl::Tmsfncgdiplusapi::FillMode fillMode)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillEllipse(TGPBrush* brush, const Vcl::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillEllipse(TGPBrush* brush, float x, float y, float width, float height)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillEllipse(TGPBrush* brush, const Vcl::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillEllipse(TGPBrush* brush, int x, int y, int width, int height)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillPie(TGPBrush* brush, const Vcl::Tmsfncgdiplusapi::TGPRectF &rect, float startAngle, float sweepAngle)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillPie(TGPBrush* brush, float x, float y, float width, float height, float startAngle, float sweepAngle)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillPie(TGPBrush* brush, const Vcl::Tmsfncgdiplusapi::TGPRect &rect, float startAngle, float sweepAngle)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillPie(TGPBrush* brush, int x, int y, int width, int height, float startAngle, float sweepAngle)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillPath(TGPBrush* brush, TGPGraphicsPath* path);
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillClosedCurve(TGPBrush* brush, Vcl::Tmsfncgdiplusapi::PGPPointF points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillClosedCurve(TGPBrush* brush, Vcl::Tmsfncgdiplusapi::PGPPointF points, int count, Vcl::Tmsfncgdiplusapi::FillMode fillMode, float tension = 5.000000E-01f)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillClosedCurve(TGPBrush* brush, Vcl::Tmsfncgdiplusapi::PGPPoint points, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillClosedCurve(TGPBrush* brush, Vcl::Tmsfncgdiplusapi::PGPPoint points, int count, Vcl::Tmsfncgdiplusapi::FillMode fillMode, float tension = 5.000000E-01f)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall FillRegion(TGPBrush* brush, TGPRegion* region);
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawString(System::UnicodeString string_, TGPFont* font, const Vcl::Tmsfncgdiplusapi::TGPRectF &layoutRect, TGPStringFormat* stringFormat, TGPBrush* brush)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawString(System::WideString string_, int length, TGPFont* font, const Vcl::Tmsfncgdiplusapi::TGPRectF &layoutRect, TGPStringFormat* stringFormat, TGPBrush* brush)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawString(System::WideString string_, int length, TGPFont* font, const Vcl::Tmsfncgdiplusapi::TGPPointF &origin, TGPBrush* brush)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawString(System::WideString string_, int length, TGPFont* font, const Vcl::Tmsfncgdiplusapi::TGPPointF &origin, TGPStringFormat* stringFormat, TGPBrush* brush)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall MeasureString(System::WideString string_, int length, TGPFont* font, const Vcl::Tmsfncgdiplusapi::TGPRectF &layoutRect, TGPStringFormat* stringFormat, /* out */ Vcl::Tmsfncgdiplusapi::TGPRectF &boundingBox, System::PInteger codepointsFitted = (System::PInteger)(0x0), System::PInteger linesFilled = (System::PInteger)(0x0))/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall MeasureString(System::WideString string_, int length, TGPFont* font, const Vcl::Tmsfncgdiplusapi::TGPSizeF &layoutRectSize, TGPStringFormat* stringFormat, /* out */ Vcl::Tmsfncgdiplusapi::TGPSizeF &size, System::PInteger codepointsFitted = (System::PInteger)(0x0), System::PInteger linesFilled = (System::PInteger)(0x0))/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall MeasureString(System::WideString string_, int length, TGPFont* font, const Vcl::Tmsfncgdiplusapi::TGPPointF &origin, TGPStringFormat* stringFormat, /* out */ Vcl::Tmsfncgdiplusapi::TGPRectF &boundingBox)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall MeasureString(System::WideString string_, int length, TGPFont* font, const Vcl::Tmsfncgdiplusapi::TGPRectF &layoutRect, /* out */ Vcl::Tmsfncgdiplusapi::TGPRectF &boundingBox)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall MeasureString(System::WideString string_, int length, TGPFont* font, const Vcl::Tmsfncgdiplusapi::TGPPointF &origin, /* out */ Vcl::Tmsfncgdiplusapi::TGPRectF &boundingBox)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall MeasureCharacterRanges(System::WideString string_, int length, TGPFont* font, const Vcl::Tmsfncgdiplusapi::TGPRectF &layoutRect, TGPStringFormat* stringFormat, int regionCount, TGPRegion* const *regions, const int regions_High)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawDriverString(Vcl::Tmsfncgdiplusapi::PUINT16 text, int length, TGPFont* font, TGPBrush* brush, Vcl::Tmsfncgdiplusapi::PGPPointF positions, int flags, TGPMatrix* matrix);
	Vcl::Tmsfncgdiplusapi::Status __fastcall MeasureDriverString(Vcl::Tmsfncgdiplusapi::PUINT16 text, int length, TGPFont* font, Vcl::Tmsfncgdiplusapi::PGPPointF positions, int flags, TGPMatrix* matrix, /* out */ Vcl::Tmsfncgdiplusapi::TGPRectF &boundingBox);
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawCachedBitmap(TGPCachedBitmap* cb, int x, int y);
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, const Vcl::Tmsfncgdiplusapi::TGPPointF &point)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, float x, float y)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, const Vcl::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, float x, float y, float width, float height)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, const Vcl::Tmsfncgdiplusapi::TGPPoint &point)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, int x, int y)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, const Vcl::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, int x, int y, int width, int height)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, Vcl::Tmsfncgdiplusapi::PGPPointF destPoints, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, Vcl::Tmsfncgdiplusapi::PGPPoint destPoints, int count)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, float x, float y, float srcx, float srcy, float srcwidth, float srcheight, Vcl::Tmsfncgdiplusapi::Unit_ srcUnit)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, const Vcl::Tmsfncgdiplusapi::TGPRectF &destRect, float srcx, float srcy, float srcwidth, float srcheight, Vcl::Tmsfncgdiplusapi::Unit_ srcUnit, TGPImageAttributes* imageAttributes = (TGPImageAttributes*)(0x0), Vcl::Tmsfncgdiplusapi::ImageAbort callback = 0x0, void * callbackData = (void *)(0x0))/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, Vcl::Tmsfncgdiplusapi::PGPPointF destPoints, int count, float srcx, float srcy, float srcwidth, float srcheight, Vcl::Tmsfncgdiplusapi::Unit_ srcUnit, TGPImageAttributes* imageAttributes = (TGPImageAttributes*)(0x0), Vcl::Tmsfncgdiplusapi::ImageAbort callback = 0x0, void * callbackData = (void *)(0x0))/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, int x, int y, int srcx, int srcy, int srcwidth, int srcheight, Vcl::Tmsfncgdiplusapi::Unit_ srcUnit)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, const Vcl::Tmsfncgdiplusapi::TGPRect &destRect, int srcx, int srcy, int srcwidth, int srcheight, Vcl::Tmsfncgdiplusapi::Unit_ srcUnit, TGPImageAttributes* imageAttributes = (TGPImageAttributes*)(0x0), Vcl::Tmsfncgdiplusapi::ImageAbort callback = 0x0, void * callbackData = (void *)(0x0))/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall DrawImage(TGPImage* image, Vcl::Tmsfncgdiplusapi::PGPPoint destPoints, int count, int srcx, int srcy, int srcwidth, int srcheight, Vcl::Tmsfncgdiplusapi::Unit_ srcUnit, TGPImageAttributes* imageAttributes = (TGPImageAttributes*)(0x0), Vcl::Tmsfncgdiplusapi::ImageAbort callback = 0x0, void * callbackData = (void *)(0x0))/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetClip(TGPGraphics* g, Vcl::Tmsfncgdiplusapi::CombineMode combineMode = (Vcl::Tmsfncgdiplusapi::CombineMode)(0x0))/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetClip(const Vcl::Tmsfncgdiplusapi::TGPRectF &rect, Vcl::Tmsfncgdiplusapi::CombineMode combineMode = (Vcl::Tmsfncgdiplusapi::CombineMode)(0x0))/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetClip(const Vcl::Tmsfncgdiplusapi::TGPRect &rect, Vcl::Tmsfncgdiplusapi::CombineMode combineMode = (Vcl::Tmsfncgdiplusapi::CombineMode)(0x0))/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetClip(TGPGraphicsPath* path, Vcl::Tmsfncgdiplusapi::CombineMode combineMode = (Vcl::Tmsfncgdiplusapi::CombineMode)(0x0))/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetClip(TGPRegion* region, Vcl::Tmsfncgdiplusapi::CombineMode combineMode = (Vcl::Tmsfncgdiplusapi::CombineMode)(0x0))/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetClip(HRGN hRgn, Vcl::Tmsfncgdiplusapi::CombineMode combineMode = (Vcl::Tmsfncgdiplusapi::CombineMode)(0x0))/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall IntersectClip(const Vcl::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall IntersectClip(const Vcl::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall IntersectClip(TGPRegion* region)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall ExcludeClip(const Vcl::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall ExcludeClip(const Vcl::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall ExcludeClip(TGPRegion* region)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall ResetClip();
	Vcl::Tmsfncgdiplusapi::Status __fastcall TranslateClip(float dx, float dy)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall TranslateClip(int dx, int dy)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetClip(TGPRegion* region);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetClipBounds(/* out */ Vcl::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetClipBounds(/* out */ Vcl::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	System::LongBool __fastcall IsClipEmpty();
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetVisibleClipBounds(/* out */ Vcl::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetVisibleClipBounds(/* out */ Vcl::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	System::LongBool __fastcall IsVisibleClipEmpty();
	System::LongBool __fastcall IsVisible(int x, int y)/* overload */;
	System::LongBool __fastcall IsVisible(const Vcl::Tmsfncgdiplusapi::TGPPoint &point)/* overload */;
	System::LongBool __fastcall IsVisible(int x, int y, int width, int height)/* overload */;
	System::LongBool __fastcall IsVisible(const Vcl::Tmsfncgdiplusapi::TGPRect &rect)/* overload */;
	System::LongBool __fastcall IsVisible(float x, float y)/* overload */;
	System::LongBool __fastcall IsVisible(const Vcl::Tmsfncgdiplusapi::TGPPointF &point)/* overload */;
	System::LongBool __fastcall IsVisible(float x, float y, float width, float height)/* overload */;
	System::LongBool __fastcall IsVisible(const Vcl::Tmsfncgdiplusapi::TGPRectF &rect)/* overload */;
	unsigned __fastcall Save();
	Vcl::Tmsfncgdiplusapi::Status __fastcall Restore(unsigned gstate);
	unsigned __fastcall BeginContainer(const Vcl::Tmsfncgdiplusapi::TGPRectF &dstrect, const Vcl::Tmsfncgdiplusapi::TGPRectF &srcrect, Vcl::Tmsfncgdiplusapi::Unit_ unit_)/* overload */;
	unsigned __fastcall BeginContainer(const Vcl::Tmsfncgdiplusapi::TGPRect &dstrect, const Vcl::Tmsfncgdiplusapi::TGPRect &srcrect, Vcl::Tmsfncgdiplusapi::Unit_ unit_)/* overload */;
	unsigned __fastcall BeginContainer()/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall EndContainer(unsigned state);
	Vcl::Tmsfncgdiplusapi::Status __fastcall AddMetafileComment(System::PByte data, unsigned sizeData);
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetLastStatus();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPAdjustableArrowCap : public TGPCustomLineCap
{
	typedef TGPCustomLineCap inherited;
	
public:
	__fastcall TGPAdjustableArrowCap(float height, float width, System::LongBool isFilled);
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetHeight(float height);
	float __fastcall GetHeight();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetWidth(float width);
	float __fastcall GetWidth();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetMiddleInset(float middleInset);
	float __fastcall GetMiddleInset();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetFillState(System::LongBool isFilled);
	System::LongBool __fastcall IsFilled();
public:
	/* TGPCustomLineCap.Destroy */ inline __fastcall virtual ~TGPAdjustableArrowCap() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGPMetafile : public TGPImage
{
	typedef TGPImage inherited;
	
public:
	__fastcall TGPMetafile(HMETAFILE hWmf, Vcl::Tmsfncgdiplusapi::WmfPlaceableFileHeader &wmfPlaceableFileHeader, System::LongBool deleteWmf)/* overload */;
	__fastcall TGPMetafile(HENHMETAFILE hEmf, System::LongBool deleteEmf)/* overload */;
	__fastcall TGPMetafile(System::WideString filename)/* overload */;
	__fastcall TGPMetafile(System::WideString filename, Vcl::Tmsfncgdiplusapi::WmfPlaceableFileHeader &wmfPlaceableFileHeader)/* overload */;
	__fastcall TGPMetafile(_di_IStream stream)/* overload */;
	__fastcall TGPMetafile(HDC referenceHdc, int type_, System::WideChar * description)/* overload */;
	__fastcall TGPMetafile(HDC referenceHdc, const Vcl::Tmsfncgdiplusapi::TGPRectF &frameRect, int frameUnit, int type_, System::WideChar * description)/* overload */;
	__fastcall TGPMetafile(HDC referenceHdc, const Vcl::Tmsfncgdiplusapi::TGPRect &frameRect, int frameUnit, int type_, System::WideChar * description)/* overload */;
	__fastcall TGPMetafile(System::WideString fileName, HDC referenceHdc, int type_, System::WideChar * description)/* overload */;
	__fastcall TGPMetafile(System::WideString fileName, HDC referenceHdc, const Vcl::Tmsfncgdiplusapi::TGPRectF &frameRect, int frameUnit, int type_, System::WideChar * description)/* overload */;
	__fastcall TGPMetafile(System::WideString fileName, HDC referenceHdc, const Vcl::Tmsfncgdiplusapi::TGPRect &frameRect, int frameUnit, int type_, System::WideChar * description)/* overload */;
	__fastcall TGPMetafile(_di_IStream stream, HDC referenceHdc, int type_, System::WideChar * description)/* overload */;
	__fastcall TGPMetafile(_di_IStream stream, HDC referenceHdc, const Vcl::Tmsfncgdiplusapi::TGPRectF &frameRect, int frameUnit, int type_, System::WideChar * description)/* overload */;
	__fastcall TGPMetafile(_di_IStream stream, HDC referenceHdc, const Vcl::Tmsfncgdiplusapi::TGPRect &frameRect, int frameUnit, int type_, System::WideChar * description)/* overload */;
	__fastcall TGPMetafile()/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetMetafileHeader(HMETAFILE hWmf, Vcl::Tmsfncgdiplusapi::WmfPlaceableFileHeader &wmfPlaceableFileHeader, Vcl::Tmsfncgdiplusapi::TMetafileHeader* header)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetMetafileHeader(HENHMETAFILE hEmf, Vcl::Tmsfncgdiplusapi::TMetafileHeader* header)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetMetafileHeader(System::WideString filename, Vcl::Tmsfncgdiplusapi::TMetafileHeader* header)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetMetafileHeader(_di_IStream stream, Vcl::Tmsfncgdiplusapi::TMetafileHeader* header)/* overload */;
	Vcl::Tmsfncgdiplusapi::Status __fastcall GetMetafileHeader(Vcl::Tmsfncgdiplusapi::TMetafileHeader* header)/* overload */;
	HENHMETAFILE __fastcall GetHENHMETAFILE();
	Vcl::Tmsfncgdiplusapi::Status __fastcall SetDownLevelRasterizationLimit(unsigned metafileRasterizationLimitDpi);
	unsigned __fastcall GetDownLevelRasterizationLimit();
	unsigned __fastcall EmfToWmfBits(HENHMETAFILE hemf, unsigned cbData16, System::PByte pData16, int iMapMode = 0x8, int eFlags = 0x0);
public:
	/* TGPImage.Create */ inline __fastcall TGPMetafile(void * ANativeImage, Vcl::Tmsfncgdiplusapi::Status status)/* overload */ : TGPImage(ANativeImage, status) { }
	/* TGPImage.Create */ inline __fastcall TGPMetafile(System::WideString filename, System::LongBool useEmbeddedColorManagement)/* overload */ : TGPImage(filename, useEmbeddedColorManagement) { }
	/* TGPImage.Create */ inline __fastcall TGPMetafile(_di_IStream stream, System::LongBool useEmbeddedColorManagement)/* overload */ : TGPImage(stream, useEmbeddedColorManagement) { }
	/* TGPImage.Destroy */ inline __fastcall virtual ~TGPMetafile() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGPFontFamily* GenericSansSerifFontFamily;
extern DELPHI_PACKAGE TGPFontFamily* GenericSerifFontFamily;
extern DELPHI_PACKAGE TGPFontFamily* GenericMonospaceFontFamily;
extern DELPHI_PACKAGE TGPStringFormat* GenericTypographicStringFormatBuffer;
extern DELPHI_PACKAGE TGPStringFormat* GenericDefaultStringFormatBuffer;
extern DELPHI_PACKAGE Vcl::Tmsfncgdiplusapi::GdiplusStartupInput StartupInput;
extern DELPHI_PACKAGE unsigned gdiplusToken;
extern DELPHI_PACKAGE Vcl::Tmsfncgdiplusapi::Status __fastcall GetImageDecodersSize(/* out */ unsigned &numDecoders, /* out */ unsigned &size);
extern DELPHI_PACKAGE Vcl::Tmsfncgdiplusapi::Status __fastcall GetImageDecoders(unsigned numDecoders, unsigned size, Vcl::Tmsfncgdiplusapi::PImageCodecInfo decoders);
extern DELPHI_PACKAGE Vcl::Tmsfncgdiplusapi::Status __fastcall GetImageEncodersSize(/* out */ unsigned &numEncoders, /* out */ unsigned &size);
extern DELPHI_PACKAGE Vcl::Tmsfncgdiplusapi::Status __fastcall GetImageEncoders(unsigned numEncoders, unsigned size, Vcl::Tmsfncgdiplusapi::PImageCodecInfo encoders);
extern DELPHI_PACKAGE void __fastcall InitializeGdiPlus(void);
extern DELPHI_PACKAGE void __fastcall FinalizeGdiPlus(void);
}	/* namespace Tmsfncgdiplusclasses */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCGDIPLUSCLASSES)
using namespace Vcl::Tmsfncgdiplusclasses;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncgdiplusclassesHPP
