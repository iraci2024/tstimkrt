{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ Express Cross Platform Library controls }
{ }
{ Copyright (c) 2000-2021 Developer Express Inc. }
{ ALL RIGHTS RESERVED }
{ }
{ The entire contents of this file is protected by U.S. and }
{ International Copyright Laws. Unauthorized reproduction, }
{ reverse-engineering, and distribution of all or any portion of }
{ the code contained in this file is strictly prohibited and may }
{ result in severe civil and criminal penalties and will be }
{ prosecuted to the maximum extent possible under the law. }
{ }
{ RESTRICTIONS }
{ }
{ THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES }
{ (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE }
{ SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS }
{ LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM }
{ ONLY. }
{ }
{ THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED }
{ FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE }
{ COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE }
{ AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT }
{ AND PERMISSION FROM DEVELOPER EXPRESS INC. }
{ }
{ CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON }
{ ADDITIONAL RESTRICTIONS. }
{ }
{ ******************************************************************** }

unit dxFluentDesignFormInterfaces; // for internal use

{$I cxVer.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, Messages, Windows,
  dxCore, dxCoreGraphics;

type
  TdxFluentDesignNavigationControlDisplayMode = (ncdmInline, ncdmOverlay,
    ncdmOverlayMinimal);
  TdxFluentDesignNavigationControlChangeType = (nchtAppearance, nchtBounds,
    nchtVisibility);
  TdxFluentDesignNavigationControlChangeTypes = set of
    TdxFluentDesignNavigationControlChangeType;

  IdxFluentDesignNavigationControlListener = interface
    ['{71365666-FE08-438D-B5C6-46DE2781945A}']
    procedure Changed(AChanges: TdxFluentDesignNavigationControlChangeTypes);
  end;

  IdxFluentDesignContainerControl = interface
    ['{D0B5352A-B84A-407F-9269-7141054A8AB8}']
    function CanBeOpaque: Boolean;
    procedure NotifyTurnedOpaque;
  end;

  IdxFluentDesignNavigationControl = interface
    ['{872FF75F-4368-4F3E-99F9-B277771FFD65}']
    function IsAdaptable: Boolean;
    function IsCollapsed: Boolean;
    function GetAcrylicEnabled: Boolean;
    function GetBackgroundColor: TdxAlphaColor;
    function GetBorderColor: TdxAlphaColor;
    function GetDisplayMode: TdxFluentDesignNavigationControlDisplayMode;
    function GetForegroundColor: TColor;
    procedure SetDisplayMode(AValue
      : TdxFluentDesignNavigationControlDisplayMode);

    procedure AddListener(AListener: IdxFluentDesignNavigationControlListener);
    procedure RemoveListener(AListener
      : IdxFluentDesignNavigationControlListener);
  end;

  IdxAcrylicHostControl = interface
    ['{FC0606D4-3A22-4758-8578-D4C570BFE1D9}']
    function GetAcrylicColor: TdxAlphaColor;
    function IsAcrylic: Boolean;
  end;

  IdxAcrylicHostControlListener = interface
    ['{76D7B1B4-101D-4A67-A93C-6D04868790F4}']
    procedure Changed(AValue: IdxAcrylicHostControl;
      AHasAcrylicParent: Boolean);
  end;

  IdxFluentDesignForm = interface(IdxAcrylicHostControl)
    ['{E5798CE2-65CF-48FD-B23A-B0F4297BF7EA}']
    function GetNavigationControlBackgroundColor: TdxAlphaColor;
    function GetNavigationControlForegroundColor: TColor;
    function GetExtendedNavigationControlCaptionWidth: Integer;
    function IsNavigationControlCollapsed: Boolean;
  end;

implementation

end.
