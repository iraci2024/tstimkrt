// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCGraphicsTools.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncgraphicstoolsHPP
#define Vcl_TmsfncgraphicstoolsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <VCL.TMSFNCGraphicsTypes.hpp>
#include <Vcl.Graphics.hpp>
#include <System.UITypes.hpp>
#include <System.Generics.Collections.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfncgraphicstools
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<tagRGBTRIPLE, 4096> TRGBTripleArray;

typedef TRGBTripleArray *PRGBTripleArray;

typedef System::DynamicArray<bool> Vcl_Tmsfncgraphicstools__1;

typedef System::DynamicArray<System::DynamicArray<bool> > TBool2DArray;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon __fastcall GetMagicWandPolygon(Vcl::Tmsfnctypes::TTMSFNCBitmap* ABitmap, float X, float Y, float Tolerance = 1.000000E-01f, float DensityValue = 5.000000E+01f, float DensityMax = 5.000000E+01f);
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall GetPixel(Vcl::Tmsfnctypes::TTMSFNCBitmap* ABitmap, float AX, float AY);
extern DELPHI_PACKAGE System::Types::TRectF __fastcall GetPolygonBoundsRect(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon);
extern DELPHI_PACKAGE Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon __fastcall InsertPointInPolygon(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon, const System::Types::TPointF &APoint, int AIndex);
extern DELPHI_PACKAGE Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon __fastcall RemovePointInPolygon(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon, int AIndex);
}	/* namespace Tmsfncgraphicstools */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCGRAPHICSTOOLS)
using namespace Vcl::Tmsfncgraphicstools;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncgraphicstoolsHPP
