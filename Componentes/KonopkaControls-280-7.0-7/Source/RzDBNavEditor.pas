{===============================================================================
  RzDBNavEditor Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2021 by Embarcadero Technologies, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzDBNavigatorImageIndexProperty
    Property editor for TRzDBNavigatorImageIndexes properties.


  Modification History
  ------------------------------------------------------------------------------
  7.0.0  (15 March 2021)
    * Removed old conditional defines. All code designed only for RAD Studio 10.4+.
    * Added High-DPI support for all elements in all controls
    * Added improvments and optimizations in code
  ------------------------------------------------------------------------------
  6.5.0  (28 Nov 2020)
    * Removed old conditional defines. All code designed for RAD Studio 10+.
    * Updated all uses clauses to reference complete unit scope names.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Initial release.
===============================================================================}

{$I RzComps.inc}

unit RzDBNavEditor;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Vcl.ImgList,
  DesignIntf,
  DesignMenus,
  DesignEditors,
  VCLEditors,
  RzSelectImageEditor;

type

  {========================================================}
  {== TRzDBNavigatorImageIndexProperty Class Declaration ==}
  {========================================================}

  TRzDBNavigatorImageIndexProperty = class( TRzCustomImageIndexProperty )
  protected
    function GetImageList: TCustomImageList; override;
  end;


implementation

uses
  RzDBNav;

{==============================================}
{== TRzDBNavigatorImageIndexProperty Methods ==}
{==============================================}

function TRzDBNavigatorImageIndexProperty.GetImageList: TCustomImageList;
begin
  Result := ( GetComponent( 0 ) as TRzDBNavigatorImageIndexes ).Navigator.Images;
end;

end.

