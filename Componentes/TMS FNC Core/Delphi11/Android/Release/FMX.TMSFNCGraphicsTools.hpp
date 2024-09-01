// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCGraphicsTools.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncgraphicstoolsHPP
#define Fmx_TmsfncgraphicstoolsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.Graphics.hpp>
#include <System.UITypes.hpp>
#include <System.Generics.Collections.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncgraphicstools
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<bool> Fmx_Tmsfncgraphicstools__1;

typedef System::DynamicArray<System::DynamicArray<bool> > TBool2DArray;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon __fastcall GetMagicWandPolygon(Fmx::Tmsfnctypes::TTMSFNCBitmap* ABitmap, float X, float Y, float Tolerance = 1.000000E-01f, float DensityValue = 5.000000E+01f, float DensityMax = 5.000000E+01f);
extern DELPHI_PACKAGE System::Uitypes::TAlphaColor __fastcall GetPixel(Fmx::Tmsfnctypes::TTMSFNCBitmap* ABitmap, float AX, float AY);
extern DELPHI_PACKAGE System::Types::TRectF __fastcall GetPolygonBoundsRect(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon);
extern DELPHI_PACKAGE Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon __fastcall InsertPointInPolygon(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon, const System::Types::TPointF &APoint, int AIndex);
extern DELPHI_PACKAGE Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon __fastcall RemovePointInPolygon(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsPathPolygon APolygon, int AIndex);
}	/* namespace Tmsfncgraphicstools */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCGRAPHICSTOOLS)
using namespace Fmx::Tmsfncgraphicstools;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncgraphicstoolsHPP
