{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2017 - 2021                               }
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

unit FMX.TMSFNCGraphics.iOS;

{$I FMX.TMSFNCDefines.inc}

interface

{$IFDEF IOS}

uses
  Classes, MacApi.CoreFoundation, iOSApi.CoreGraphics, MacApi.ObjectiveC, iOSApi.Foundation, iOSApi.CoreText, iOSApi.CocoaTypes, iOSApi.UIKit,
  FMX.TMSFNCGraphicsTypes, FMX.TMSFNCTypes, {%H-}Types, FMX.TMSFNCGraphics, FMX.Graphics,
  FMX.TMSFNCUtils
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  ,UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  ;

type
  TTMSFNCGraphicsContextiOS = class(TTMSFNCGraphicsContext)
  private
    FSaveMatrix: TTMSFNCGraphicsMatrix;
    FScale: Single;
    FActivePath: TTMSFNCGraphicsPath;
    FMapping: Boolean;
    FNeedsRendering: Boolean;
    FFont: TTMSFNCGraphicsFont;
    FFill: TTMSFNCGraphicsFill;
    FStroke: TTMSFNCGraphicsStroke;
    FContextSize: TSizeF;
    FCGContext: CGContextRef;
    FBitmap: TBitmap;
    FBitmapData: TBitmapData;
  protected
    function GetNativeCanvas: Pointer; override;
    function GetFlipped(const R: TRectF): TRectF;
    function DrawTextInternal(Text: String; Rect: TRectF; AWordWrap: Boolean; AHorizontalAlignment: TTMSFNCGraphicsTextAlign; Calculate: Boolean): TRectF;
    procedure SaveContext;
    procedure RestoreContext;
    procedure DestroyResources;
    procedure ApplyFill;
    procedure ApplyStroke;
    procedure DrawLinearGradient(ARect: TRectF);
    procedure FontChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure StrokeChanged(Sender: TObject);
  public
    constructor Create(const AGraphics: TTMSFNCGraphics); override;
    destructor Destroy; override;
    function GetFillColor: TTMSFNCGraphicsColor; override;
    function CalculateText(AText: string; ARect: TRectF; AWordWrapping: Boolean): TRectF; override;
    function SetTextAngle(ARect: TRectF; {%H-}AAngle: Single): TRectF; override;
    function CreatePath: Pointer; override;
    function GetMatrix: TTMSFNCGraphicsMatrix; override;
    procedure Render; override;
    procedure PathOpen(APath: Pointer); override;
    procedure PathMoveTo({%H-}APath: Pointer; APoint: TPointF); override;
    procedure PathLineTo(APath: Pointer; APoint: TPointF); override;
    procedure PathClose(APath: Pointer); override;
    procedure ResetClip; override;
    procedure ResetTransform; override;
    procedure ScaleTransform(AX, AY: Single); override;
    procedure RotateTransform(AAngle: Single); override;
    procedure TranslateTransform(AX, AY: Single); override;
    procedure SetTextQuality({%H-}ATextQuality: TTMSFNCGraphicsTextQuality); override;
    procedure SetAntiAliasing(AAntiAliasing: Boolean); override;
    procedure SetShowAcceleratorChar({%H-}AShowAcceleratorChar: Boolean); override;
    procedure SetSize(AWidth, AHeight: Single); override;
    procedure SetMatrix(AMatrix: TTMSFNCGraphicsMatrix); override;
    procedure SetScale(AScale: Single); override;
    procedure ResetTextAngle({%H-}AAngle: Single); override;
    procedure BeginScene; override;
    procedure EndScene; override;
    procedure BeginPrinting; override;
    procedure EndPrinting; override;
    procedure StartSpecialPen; override;
    procedure StopSpecialPen; override;
    procedure RestoreState(AState: TTMSFNCGraphicsSaveState); override;
    procedure SaveState({%H-}AState: TTMSFNCGraphicsSaveState); override;
    procedure SetFontSize(ASize: Integer); override;
    procedure SetFontColor(AColor: TTMSFNCGraphicsColor); override;
    procedure SetFontName(AName: string); override;
    procedure SetFont(AFont: TTMSFNCGraphicsFont); override;
    procedure SetFontStyles(AStyle: TFontStyles); override;
    procedure SetFill(AFill: TTMSFNCGraphicsFill); override;
    procedure SetFillKind(AKind: TTMSFNCGraphicsFillKind); override;
    procedure SetFillColor(AColor: TTMSFNCGraphicsColor); override;
    procedure SetStroke(AStroke: TTMSFNCGraphicsStroke); override;
    procedure SetStrokeKind(AKind: TTMSFNCGraphicsStrokeKind); override;
    procedure SetStrokeColor(AColor: TTMSFNCGraphicsColor); override;
    procedure SetStrokeWidth(AWidth: Single); override;
    procedure DrawLine({%H-}AStroke: TTMSFNCGraphicsStroke; AFromPoint: TPointF; AToPoint: TPointF; {%H-}AModifyPointModeFrom: TTMSFNCGraphicsModifyPointMode = gcpmRightDown; {%H-}AModifyPointModeTo: TTMSFNCGraphicsModifyPointMode = gcpmRightDown); override;
    procedure DrawPolygon({%H-}AStroke: TTMSFNCGraphicsStroke; APolygon: TTMSFNCGraphicsPathPolygon); override;
    procedure FillPolygon({%H-}AFill: TTMSFNCGraphicsFill; APolygon: TTMSFNCGraphicsPathPolygon); override;
    procedure DrawPolyline({%H-}AStroke: TTMSFNCGraphicsStroke; APolyline: TTMSFNCGraphicsPathPolygon); override;
    procedure FillPolyline({%H-}AFill: TTMSFNCGraphicsFill; APolyline: TTMSFNCGraphicsPathPolygon); override;
    procedure FillArc({%H-}AFill: TTMSFNCGraphicsFill; {%H-}ACenter, {%H-}ARadius: TPointF; {%H-}AStartAngle, {%H-}ASweepAngle: Single); override;
    procedure DrawArc({%H-}AStroke: TTMSFNCGraphicsStroke; ACenter, ARadius: TPointF; AStartAngle, ASweepAngle: Single); override;
    procedure FillRect({%H-}AFill: TTMSFNCGraphicsFill; ARect: TRectF; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawRect({%H-}AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; ASides: TTMSFNCGraphicsSides; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure FillRoundRect({%H-}AFill: TTMSFNCGraphicsFill; ARect: TRectF; ARounding: Single; ACorners: TTMSFNCGraphicsCorners; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawRoundRect({%H-}AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; ARounding: Single; ACorners: TTMSFNCGraphicsCorners; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure FillEllipse({%H-}AFill: TTMSFNCGraphicsFill; ARect: TRectF; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawEllipse({%H-}AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawBitmap(ABitmap: TTMSFNCDrawBitmap; {%H-}ASrcRect, ADstRect: TRectF; {%H-}AOpacity: Single); override;
    procedure ClipRect(ARect: TRectF); override;
    procedure ClipPath({%H-}APath: TTMSFNCGraphicsPath); override;
    procedure DrawFocusPath({%H-}AStroke: TTMSFNCGraphicsStroke; {%H-}APath: TTMSFNCGraphicsPath; {%H-}AColor: TTMSFNCGraphicsColor); override;
    procedure DrawFocusRectangle({%H-}AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; AColor: TTMSFNCGraphicsColor; {%H-}AModifyRectMode: TTMSFNCGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawText(AText: string; ARect: TRectF; AWordWrapping: Boolean; AHorizontalAlign, AVerticalAlign: TTMSFNCGraphicsTextAlign; ATrimming: TTMSFNCGraphicsTextTrimming; {%H-}AAngle: Single); override;
    procedure DrawPath({%H-}AStroke: TTMSFNCGraphicsStroke; APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode = pdmPolygon); override;
    procedure FillPath({%H-}AFill: TTMSFNCGraphicsFill; APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode = pdmPolygon); override;
    procedure SetNativeContext(ACGContext: CGContextRef);
  end;

function GetNativeContextClass: TTMSFNCGraphicsContextClass;

{$ENDIF}

implementation

{$IFDEF IOS}

uses
  SysUtils, Math;

const
  NSUnderlineStyleNone = $00;
  NSUnderlineStyleSingle = $01;
  NSUnderlineStyleThick = $02;
  NSUnderlineStyleDouble = $09;
  NSUnderlineShadingSolid = $0000;
  NSUnderlineShadingDot = $0100;
  NSUnderlineShadingDash = $0200;
  NSUnderlineShadingDashDot = $0300;
  NSUnderlineShadingDashDotDot = $0400;
  UIFontDescriptorTraitItalic = 1 shl 0;
  UIFontDescriptorTraitBold = 1 shl 1;
  NSUnderlinePatternSolid = 0;
  NSUnderlinePatternDot = 256;
  NSUnderlinePatternDash = 512;
  NSUnderlinePatternDashDot = 768;
  NSUnderlinePatternDashDotDot = 1024;
  NSLineBreakByWordWrapping = 0;

type
  PNSDictionary = Pointer;
  NSLineBreakMode = cardinal;
  NSStringDrawingOptions = cardinal;
  NSTextAlignment = cardinal;
  NSWritingDirection = cardinal;
  UIFontDescriptor = interface;
  UIFontDescriptorSymbolicTraits = UInt32;

  NSTextAttachmentClass = interface(NSObjectClass)
    ['{47E479D1-9140-4E7C-8261-177249AE4BDF}']
  end;
  NSTextAttachment = interface(NSObject)
    ['{E7C893FE-D13D-4875-ADAA-0AA79D0125F4}']
    function initWithData(contentData: NSData; uti: NSString): Pointer; cdecl;
    function image: UIImage; cdecl;
    procedure setImage(image: UIImage); cdecl;
    function contents: NSData; cdecl;
    procedure setContents(contents: NSData); cdecl;
    function bounds: CGRect; cdecl;
    procedure setBounds(bounds: CGRect); cdecl;
  end;
  TNSTextAttachment = class(TOCGenericImport<NSTextAttachmentClass, NSTextAttachment>)  end;

  NSStringDrawingContextClass = interface(NSObjectClass)
  ['{A15F7DDC-F8A8-4BF8-A40D-CB2BE4CBE6AD}']
  end;
  NSStringDrawingContext = interface(NSObject)
  ['{E5D6C8AB-2C60-432D-8F78-674A58941B38}']
  end;
  TNSStringDrawingContext = class(TOCGenericImport<NSStringDrawingContextClass, NSStringDrawingContext>)  end;

  NSAttributedStringExClass = interface(NSAttributedStringClass)
  ['{E03D8900-A2B1-4B19-BA3C-4FCD5AC4AD04}']
    function attributedStringWithAttachment(attachment: NSTextAttachment): Pointer; cdecl;
  end;

  NSAttributedStringEx = interface(NSAttributedString)
  ['{552373B5-4E9B-4598-AEEC-F42D1F28C1C3}']
    procedure drawInRect(aRect: CGRect); cdecl;
    function boundingRectWithSize(size: CGSize; options: NSStringDrawingOptions; context: NSStringDrawingContext): CGRect; cdecl;
    function dataFromRange(range: NSRange; documentAttributes: NSDictionary; error: NSError): NSData; cdecl;
    function initWithData(data: NSData; options: NSDictionary; documentAttributes: NSDictionary; error: NSError): Pointer; cdecl;
  end;
  TNSAttributedStringEx = class(TOCGenericImport<NSAttributedStringExClass, NSAttributedStringEx>)  end;

  UIFontDescriptorClass = interface(NSObjectClass)
    ['{CB4BF3A8-9509-47D8-8A44-C96903303564}']
    {class} function fontDescriptorWithFontAttributes(attributes: NSDictionary): UIFontDescriptor; cdecl;
    {class} [MethodName('fontDescriptorWithName:size:')]
    function fontDescriptorWithNameSize(fontName: NSString; size: CGFloat): UIFontDescriptor; cdecl;
    {class} [MethodName('fontDescriptorWithName:matrix:')]
    function fontDescriptorWithNameMatrix(fontName: NSString; matrix: CGAffineTransform): UIFontDescriptor; cdecl;
    {class} function preferredFontDescriptorWithTextStyle(style: NSString): UIFontDescriptor; cdecl;
  end;

  UIFontDescriptor = interface(NSObject)
    ['{4E4A3072-20EE-468F-B5DD-897E00D7A3AE}']
    function postscriptName: NSString; cdecl;
    function pointSize: CGFloat; cdecl;
    function matrix: CGAffineTransform; cdecl;
    function symbolicTraits: UIFontDescriptorSymbolicTraits; cdecl;
    function objectForKey(anAttribute: NSString): Pointer; cdecl;
    function fontAttributes: NSDictionary; cdecl;
    function matchingFontDescriptorsWithMandatoryKeys(mandatoryKeys: NSSet): NSArray; cdecl;
    function initWithFontAttributes(attributes: NSDictionary): Pointer{instancetype}; cdecl;
    function fontDescriptorByAddingAttributes(attributes: NSDictionary): UIFontDescriptor; cdecl;
    function fontDescriptorWithSymbolicTraits(symbolicTraits: UIFontDescriptorSymbolicTraits): UIFontDescriptor; cdecl;
    function fontDescriptorWithSize(newPointSize: CGFloat): UIFontDescriptor; cdecl;
    function fontDescriptorWithMatrix(matrix: CGAffineTransform): UIFontDescriptor; cdecl;
    function fontDescriptorWithFace(newFace: NSString): UIFontDescriptor; cdecl;
    function fontDescriptorWithFamily(newFamily: NSString): UIFontDescriptor; cdecl;
  end;

  TUIFontDescriptor = class(TOCGenericImport<UIFontDescriptorClass, UIFontDescriptor>)
  end;

  UIFontClass = interface(NSObjectClass)
    ['{F21CAA74-9F23-42C5-A0F3-CECA57AFB3BC}']
    {class} function boldSystemFontOfSize(fontSize: CGFloat): Pointer; cdecl;
    {class} function buttonFontSize: CGFloat; cdecl;
    {class} function familyNames: NSArray; cdecl;
    {class} function fontNamesForFamilyName(familyName: NSString): NSArray; cdecl;
    {class} function fontWithName(fontName: NSString; size: CGFloat): Pointer; cdecl;
    {class} function italicSystemFontOfSize(fontSize: CGFloat): Pointer; cdecl;
    {class} function labelFontSize: CGFloat; cdecl;
    {class} function smallSystemFontSize: CGFloat; cdecl;
    {class} function systemFontOfSize(fontSize: CGFloat): Pointer; cdecl;
    {class} function systemFontSize: CGFloat; cdecl;
    {class} function fontWithDescriptor(descriptor: UIFontDescriptor; size: CGFloat): Pointer; cdecl;
  end;
  UIFont = interface(NSObject)
    ['{026495EC-177F-4517-9B25-C2F2371A110D}']
    function ascender: CGFloat; cdecl;
    function capHeight: CGFloat; cdecl;
    function descender: CGFloat; cdecl;
    function familyName: NSString; cdecl;
    function fontName: NSString; cdecl;
    function fontWithSize(fontSize: CGFloat): UIFont; cdecl;
    function leading: CGFloat; cdecl;
    function lineHeight: CGFloat; cdecl;
    function pointSize: CGFloat; cdecl;
    function xHeight: CGFloat; cdecl;
    function fontDescriptor: UIFontDescriptor; cdecl;
  end;
  TUIFont = class(TOCGenericImport<UIFontClass, UIFont>)  end;

  NSStringExClass = interface(NSStringClass)
  ['{DC6AFF07-2E8E-40A3-9EB3-3AE908AC8AE3}']
  end;
  NSStringEx = interface(NSString)
  ['{C5696015-1BB2-4649-AA1F-A8274FAD0603}']
    function cString: MarshaledAString; cdecl;
    function sizeWithFont(font: UIFont; forWidth: CGFloat; lineBreakMode: NSLineBreakMode): CGSize; cdecl; overload;
    function sizeWithFont(font: UIFont): CGSize; cdecl; overload;
    function drawAtPoint(aPoint: NSPoint; withFont: UIFont): CGSize; cdecl; overload;
    procedure drawAtPoint(aPoint: NSPoint; withAttributes: NSDictionary); cdecl; overload;
    procedure drawInRect(aRect: NSRect; withAttributes: NSDictionary); cdecl; overload;
    function drawInRect(aRect: NSRect; withFont: UIFont): CGSize; cdecl; overload;
    function drawInRect(aRect: NSRect; withFont: UIFont; lineBreakMode: NSLineBreakMode; alignment: NSTextAlignment): CGSize; cdecl; overload;
    function sizeWithAttributes(aAttributes: NSDictionary): NSSize; cdecl;
    function boundingRectWithSize(size: CGSize; options: NSStringDrawingOptions; attributes: NSDictionary; context: NSStringDrawingContext): CGRect; cdecl;
  end;
  TNSStringEx = class(TOCGenericImport<NSStringExClass, NSStringEx>)  end;

  NSParagraphStyle = interface;

  NSParagraphStyleClass = interface(NSObjectClass)
  ['{C4DFBBF6-EDE3-4E34-9816-826F7F445585}']
    function defaultParagraphStyle: NSParagraphStyle; cdecl;
    function defaultWritingDirectionForLanguage(languageName: NSString): NSWritingDirection; cdecl;
  end;
  NSParagraphStyle = interface(NSObject)
  ['{BDAA7FD2-9A2A-4C6D-A77A-75315F406540}']
    function alignment: NSTextAlignment; cdecl;
    function firstLineHeadIndent: CGFloat; cdecl;
    function headIndent: CGFloat; cdecl;
    function tailIndent: CGFloat; cdecl;
    function lineHeightMultiple: CGFloat; cdecl;
    function maximumLineHeight: CGFloat; cdecl;
    function minimumLineHeight: CGFloat; cdecl;
    function lineSpacing: CGFloat; cdecl;
    function paragraphSpacing: CGFloat; cdecl;
    function paragraphSpacingBefore: CGFloat; cdecl;
    function tabStops: NSArray; cdecl;
    function defaultTabInterval: CGFloat; cdecl;
    function lineBreakMode: NSLineBreakMode; cdecl;
    function hyphenationFactor: CGFloat; cdecl;
    function baseWritingDirection: NSWritingDirection; cdecl;
  end;
  TNSParagraphStyle = class(TOCGenericImport<NSParagraphStyleClass, NSParagraphStyle>)  end;

  NSMutableParagraphStyleClass = interface(NSObjectClass)
  ['{C4DFBBF6-EDE3-4E34-9816-826F7F445585}']
    function defaultWritingDirectionForLanguage(languageName: NSString): NSWritingDirection; cdecl;
  end;
  NSMutableParagraphStyle = interface(NSObject)
  ['{BDAA7FD2-9A2A-4C6D-A77A-75315F406540}']
    function alignment: NSTextAlignment; cdecl;
    procedure setAlignment(alignment: NSTextAlignment); cdecl;
    function firstLineHeadIndent: CGFloat; cdecl;
    procedure setFirstLineHeadIndent(firstLineHeadIndent: CGFloat); cdecl;
    function headIndent: CGFloat; cdecl;
    procedure setHeadIndent(headIndent: CGFloat); cdecl;
    function tailIndent: CGFloat; cdecl;
    procedure setTailIndent(tailIndent: CGFloat); cdecl;
    function lineHeightMultiple: CGFloat; cdecl;
    procedure setLineHeightMultiple(lineHeightMultiple: CGFloat); cdecl;
    function maximumLineHeight: CGFloat; cdecl;
    procedure setMaximumLineHeight(maximumLineHeight: CGFloat); cdecl;
    function minimumLineHeight: CGFloat; cdecl;
    procedure setMinimumLineHeight(minimumLineHeight: CGFloat); cdecl;
    function lineSpacing: CGFloat; cdecl;
    procedure setLineSpacing(lineSpacing: CGFloat); cdecl;
    function paragraphSpacing: CGFloat; cdecl;
    procedure setParagraphSpacing(paragraphSpacing: CGFloat); cdecl;
    function paragraphSpacingBefore: CGFloat; cdecl;
    procedure setParagraphSpacingBefore(paragraphSpacingBefore: CGFloat); cdecl;
    function tabStops: NSArray; cdecl;
    function defaultTabInterval: CGFloat; cdecl;
    function lineBreakMode: NSLineBreakMode; cdecl;
    procedure setLineBreakMode(lineBreakMode: NSLineBreakMode); cdecl;
    function hyphenationFactor: CGFloat; cdecl;
    procedure setHyphenationFactor(hyphenationFactor: CGFloat); cdecl;
    function style: NSParagraphStyle; cdecl;
    procedure setStyle(style: NSParagraphStyle); cdecl;
  end;
  TNSMutableParagraphStyle = class(TOCGenericImport<NSMutableParagraphStyleClass, NSMutableParagraphStyle>)  end;

  NSLayoutManager = interface;

  NSTextContainerClass = interface(NSObjectClass)
    ['{76FC0807-D297-4A5C-A439-C9D881EC83B5}']
  end;
  NSTextContainer = interface(NSObject)
    ['{B2A0C26E-E652-405C-984D-0445CCA979FD}']
    function heightTracksTextView: Boolean; cdecl;
    function initWithSize(size: CGSize): Pointer; cdecl;
    function isSimpleRectangularTextContainer: Boolean; cdecl;
    function layoutManager: NSLayoutManager; cdecl;
    function lineFragmentPadding: Single; cdecl;
    procedure replaceLayoutManager(newLayoutManager: NSLayoutManager); cdecl;
    procedure setContainerSize(size: CGSize); cdecl;
    procedure setHeightTracksTextView(flag: Boolean); cdecl;
    procedure setLayoutManager(layoutManager: NSLayoutManager); cdecl;
    procedure setLineFragmentPadding(pad: Single); cdecl;
    procedure setWidthTracksTextView(flag: Boolean); cdecl;
    function widthTracksTextView: Boolean; cdecl;
  end;
  TNSTextContainer = class(TOCGenericImport<NSTextContainerClass, NSTextContainer>)  end;

  NSLayoutManagerDelegate = interface
  ['{EFA4EB76-4617-4953-AF61-F6DB3FCB233F}']
  end;

  NSTextStorageDelegate = interface
  ['{882A0DD8-C627-4191-BCE5-7F1CF7780209}']
  end;

  NSLayoutManagerClass = interface(NSObjectClass)
    ['{6BA8AC5B-7B26-42B5-90C5-7FB194F2F306}']
  end;
  NSLayoutManager = interface(NSObject)
    ['{4084A21D-7F14-4732-A33E-D0ADA49B4D59}']
    procedure addTextContainer(container: NSTextContainer); cdecl;
    function delegate: NSLayoutManagerDelegate; cdecl;
    procedure setDelegate(delegate: NSLayoutManagerDelegate); cdecl;
    function glyphRangeForBoundingRect(bounds: NSRect; inTextContainer: NSTextContainer): NSRange; cdecl;
    function characterRangeForGlyphRange(glyphRange: NSRange; actualGlyphRange: PNSRange): NSRange; cdecl;
    function glyphRangeForTextContainer(container: NSTextContainer): NSRange; cdecl;
    procedure drawGlyphsForGlyphRange(glyphsToShow: NSRange; atPoint: CGPoint); cdecl;
    procedure drawBackgroundForGlyphRange(glyphsToShow: NSRange; atPoint: CGPoint); cdecl;
    function textContainers: NSArray; cdecl;
    function numberOfGlyphs: NSUInteger; cdecl;
  end;
  TNSLayoutManager = class(TOCGenericImport<NSLayoutManagerClass, NSLayoutManager>)  end;

  NSTextStorageClass = interface(NSMutableAttributedStringClass)
    ['{EB0E07CA-3010-498B-9307-49744A89849C}']
  end;
  NSTextStorage = interface(NSMutableAttributedString)
    ['{53CEA2E6-F675-44D8-8320-F0305E8F0E86}']
    procedure addLayoutManager(obj: NSLayoutManager); cdecl;
    function changeInLength: NSInteger; cdecl;
    function delegate: NSTextStorageDelegate; cdecl;
    procedure edited(editedMask: NSUInteger; range: NSRange; changeInLength: NSInteger); cdecl;
    function editedMask: NSUInteger; cdecl;
    function editedRange: NSRange; cdecl;
    procedure ensureAttributesAreFixedInRange(range: NSRange); cdecl;
    function fixesAttributesLazily: Boolean; cdecl;
    procedure invalidateAttributesInRange(range: NSRange); cdecl;
    function layoutManagers: NSArray; cdecl;
    procedure processEditing; cdecl;
    procedure removeLayoutManager(obj: NSLayoutManager); cdecl;
    procedure setDelegate(delegate: NSTextStorageDelegate); cdecl;
  end;
  TNSTextStorage = class(TOCGenericImport<NSTextStorageClass, NSTextStorage>)  end;

procedure UIGraphicsPopContext; cdecl; external libUIKit name _PU + 'UIGraphicsPopContext';
procedure UIGraphicsPushContext(context: CGContextRef); cdecl; external libUIKit name _PU + 'UIGraphicsPushContext';

function NSStrEx(AString: String): NSString;
begin
  Result := TNSString.Wrap(TNSString.OCClass.stringWithCharacters(PChar(AString), AString.Length));
end;

function GetNativeContextClass: TTMSFNCGraphicsContextClass;
begin
  Result := TTMSFNCGraphicsContextiOS;
end;

function ConvertToUIColor(AColor: TTMSFNCGraphicsColor; AOpacity: Single): UIColor;
var
  R: CGFloat;
  G: CGFloat;
  B: CGFloat;
  A: CGFloat;
begin
  if AColor = TAlphaColorRec.Null then
  begin
    Result := TUIColor.Wrap(TUIColor.OCClass.clearColor);
    Exit;
  end;

  A := AOpacity;
  R := TAlphaColorRec(AColor).R / 255;
  G := TAlphaColorRec(AColor).G / 255;
  B := TAlphaColorRec(AColor).B / 255;
  Result := TUIColor.Wrap(TUIColor.OCClass.colorWithRed(R, G, B, A));
end;

{ TTMSFNCGraphicsContextiOS }

procedure TTMSFNCGraphicsContextiOS.ApplyFill;
begin
  if not Assigned(FCGContext) then
     Exit;

  case FFill.Kind of
    gfkNone:
    begin
      CGContextSetRGBFillColor(FCGContext, TTMSFNCGraphics.GetColorRed(gcNull) / 255, TTMSFNCGraphics.GetColorGreen(gcNull) / 255,
        TTMSFNCGraphics.GetColorBlue(gcNull) / 255, 0);
    end;
    gfkSolid:
    begin
      CGContextSetRGBFillColor(FCGContext, TTMSFNCGraphics.GetColorRed(FFill.Color) / 255, TTMSFNCGraphics.GetColorGreen(FFill.Color) / 255,
        TTMSFNCGraphics.GetColorBlue(FFill.Color) / 255, FFill.Opacity);
    end;
  end;
end;

procedure TTMSFNCGraphicsContextiOS.ApplyStroke;
var
  f: array of CGFloat;
begin
  if not Assigned(FCGContext) then
     Exit;

  CGContextSetLineWidth(FCGContext, FStroke.Width);
  CGContextSetRGBStrokeColor(FCGContext, TTMSFNCGraphics.GetColorRed(FStroke.Color) / 255, TTMSFNCGraphics.GetColorGreen(FStroke.Color) / 255, TTMSFNCGraphics.GetColorBlue(FStroke.Color) / 255, FStroke.Opacity);

  case FStroke.Kind of
    gskSolid, gskNone: CGContextSetLineDash(FCGContext, 0, nil, 0);
    gskDash:
    begin
      SetLength(f, 2);
      f[0] := 3;
      f[1] := 1;
      CGContextSetLineDash(FCGContext, 0, @f[0], Length(f));
    end;
    gskDot:
    begin
      SetLength(f, 2);
      f[0] := 1;
      f[1] := 1;
      CGContextSetLineDash(FCGContext, 0, @f[0], Length(f));
    end;
    gskDashDot:
    begin
      SetLength(f, 4);
      f[0] := 3;
      f[1] := 1;
      f[2] := 1;
      f[3] := 1;
      CGContextSetLineDash(FCGContext, 0, @f[0], Length(f));
    end;
    gskDashDotDot:
    begin
      SetLength(f, 6);
      f[0] := 3;
      f[1] := 1;
      f[2] := 1;
      f[3] := 1;
      f[4] := 1;
      f[5] := 1;
      CGContextSetLineDash(FCGContext, 0, @f[0], Length(f));
    end;
  end;
end;

procedure TTMSFNCGraphicsContextiOS.BeginPrinting;
begin
end;

procedure TTMSFNCGraphicsContextiOS.EndPrinting;
begin
end;

procedure TTMSFNCGraphicsContextiOS.BeginScene;
begin
  if not Assigned(FCGContext) then
    Exit;

  Canvas.BeginScene;
  Canvas.Clear(gcNull);
end;

function TTMSFNCGraphicsContextiOS.CalculateText(AText: string; ARect: TRectF;
  AWordWrapping: Boolean): TRectF;
begin
  Result := RectF(0, 0, 0, 0);
  if not Assigned(FCGContext) then
    Exit;

  SaveContext;
  Result := DrawTextInternal(AText, ARect, AWordWrapping, gtaLeading, True);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextiOS.ClipPath(APath: TTMSFNCGraphicsPath);
var
  pth: CGMutablePathRef;
begin
  if not Assigned(FCGContext) then
    Exit;

  pth := CGMutablePathRef(ConvertToPath(APath));
  try
    SaveContext;
    CGContextBeginPath(FCGContext);
    CGContextAddPath(FCGContext, pth);
    CGContextClip(FCGContext);
  finally
    CGPathRelease(pth);
  end;
end;

procedure TTMSFNCGraphicsContextiOS.ClipRect(ARect: TRectF);
begin
  if not Assigned(FCGContext) then
    Exit;

  SaveContext;
  CGContextBeginPath(FCGContext);
  CGContextAddRect(FCGContext, CGRectMake(ARect.Left, ARect.Top, ARect.Width, ARect.Height));
  CGContextClip(FCGContext);
end;

constructor TTMSFNCGraphicsContextiOS.Create(const AGraphics: TTMSFNCGraphics);
begin
  inherited;
  FScale := TTMSFNCUtils.GetDPIScale;
  FNeedsRendering := True;
  FContextSize.cx := 0;
  FContextSize.cy := 0;
  FFont := TTMSFNCGraphicsFont.Create;
  FFont.OnChanged := FontChanged;
  FStroke := TTMSFNCGraphicsStroke.Create;
  FStroke.OnChanged := StrokeChanged;
  FFill := TTMSFNCGraphicsFill.Create;
  FFill.OnChanged := FillChanged;
end;

function TTMSFNCGraphicsContextiOS.CreatePath: Pointer;
begin
  Result := CGPathCreateMutable;
end;

destructor TTMSFNCGraphicsContextiOS.Destroy;
begin
  Render;

  if Assigned(FFont) then
  begin
    FFont.Free;
    FFont := nil;
  end;

  if Assigned(FFill) then
  begin
    FFill.Free;
    FFill := nil;
  end;

  if Assigned(FStroke) then
  begin
    FStroke.Free;
    FStroke := nil;
  end;

  DestroyResources;
  inherited;
end;

procedure TTMSFNCGraphicsContextiOS.DestroyResources;
begin
  if Assigned(FCGContext) then
  begin
    RestoreContext;
    CGContextRelease(FCGContext);
    FCGContext := nil;
  end;

  if Assigned(FBitmap) then
  begin
    if FMapping then
    begin
      FBitmap.UnMap(FBitmapData);
      FMapping := False;
    end;
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

procedure TTMSFNCGraphicsContextiOS.DrawArc(AStroke: TTMSFNCGraphicsStroke;
  ACenter, ARadius: TPointF; AStartAngle, ASweepAngle: Single);
begin
  if not Assigned(FCGContext) then
    Exit;

  SaveContext;
  CGContextBeginPath(FCGContext);
  {$HINTS OFF}
  {$IF COMPILERVERSION < 34}
  CGContextAddArc(FCGContext, ACenter.X, ACenter.Y, ARadius.X, DegToRad(AStartAngle), DegToRad(AStartAngle + ASweepAngle), 0);
  {$ELSE}
  CGContextAddArc(FCGContext, ACenter.X, ACenter.Y, ARadius.X, DegToRad(AStartAngle), DegToRad(AStartAngle + ASweepAngle), False);
  {$IFEND}
  {$HINTS ON}
  ApplyStroke;
  CGContextDrawPath(FCGContext, kCGPathStroke);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextiOS.DrawBitmap(ABitmap: TTMSFNCDrawBitmap;
  ASrcRect, ADstRect: TRectF; AOpacity: Single);
var
  R: TRectF;
  img: CGImageRef;

  function CreateQImage: CGImageRef;
  var
    ImageRef: CGImageRef;
    CtxRef: CGContextRef;
    ColorSpace: CGColorSpaceRef;
    BitmapData: TBitmapData;
  begin
    Result := nil;
    ColorSpace := CGColorSpaceCreateDeviceRGB;
    try
      if ABitmap.Map(TMapAccess.Read, BitmapData) then
      begin
        CtxRef := CGBitmapContextCreate(BitmapData.Data, ABitmap.Width, ABitmap.Height, 8, 4 * ABitmap.Width, ColorSpace, kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big );
        try
          ImageRef := CGBitmapContextCreateImage(CtxRef);
          Result := ImageRef;
        finally
          CGContextRelease(CtxRef);
        end;
      end;
    finally
      CGColorSpaceRelease(ColorSpace);
    end;
  end;
begin
  if not Assigned(ABitmap) or not Assigned(FCGContext) then
    Exit;

  SaveContext;
  CGContextTranslateCTM(FCGContext, 0, FContextSize.Height);
  CGContextScaleCTM(FCGContext, 1.0, -1.0);
  R := GetFlipped(ADstRect);
  img := CreateQImage;
  try
    CGContextDrawImage(FCGContext, CGRectMake(R.Left, R.Top, R.Width, r.Height), img);
  finally
    CFRelease(img);
  end;
  Restorecontext;
end;

procedure TTMSFNCGraphicsContextiOS.DrawEllipse(AStroke: TTMSFNCGraphicsStroke;
  ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FCGContext) then
    Exit;

  r := ARect;
  SaveContext;
  CGContextBeginPath(FCGContext);
  CGContextAddEllipseInRect(FCGContext, CGRectMake(r.Left, r.Top, r.Width, r.Height));
  ApplyStroke;
  CGContextDrawPath(FCGContext, kCGPathStroke);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextiOS.DrawFocusPath(
  AStroke: TTMSFNCGraphicsStroke; APath: TTMSFNCGraphicsPath;
  AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FCGContext) then
    Exit;
end;

procedure TTMSFNCGraphicsContextiOS.DrawFocusRectangle(
  AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; AColor: TTMSFNCGraphicsColor;
  AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
begin
  if not Assigned(FCGContext) then
    Exit;

end;

procedure TTMSFNCGraphicsContextiOS.DrawLine(AStroke: TTMSFNCGraphicsStroke;
  AFromPoint, AToPoint: TPointF; AModifyPointModeFrom,
  AModifyPointModeTo: TTMSFNCGraphicsModifyPointMode);
begin
  if not Assigned(FCGContext) then
    Exit;

  SaveContext;
  CGContextBeginPath(FCGContext);
  CGContextMoveToPoint(FCGContext, AFromPoint.X, AFromPoint.Y);
  CGContextAddLineToPoint(FCGContext, AToPoint.X, AToPoint.Y);
  CGContextStrokePath(FCGContext);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextiOS.DrawLinearGradient(ARect: TRectF);
var
  cs: CGColorSpaceRef;
  colorarr: array of CGColorRef;
  positionsarr: array of CGFloat;
  colors, positions: Pointer;
  gradient: CGGradientRef;
  f0, f1: Boolean;
  I: Integer;
  o: Integer;
  a, sx, sy: single;
  c, pt0, pt1: CGPoint;
  st, ist: CGAffineTransform;
  ivs: CGPoint;
  rd: CGFloat;
  it: TTMSFNCGraphicsFillGradientItem;
begin
  if not Assigned(FCGContext) then
    Exit;

  case FFill.GradientMode of
    gfgmDefault:
    begin
      SetLength(positionsarr, 0);
      if (FFill.ColorTo <> gcnull) and (FFill.ColorMirror <> gcNull) then
      begin
        SetLength(colorarr, 4);
        colorarr[0] := TUIColor.Wrap((ConvertToUIColor(FFill.Color, FFill.Opacity) as ILocalObject).GetObjectID).CGColor;
        colorarr[1] := TUIColor.Wrap((ConvertToUIColor(FFill.ColorTo, FFill.Opacity) as ILocalObject).GetObjectID).CGColor;
        colorarr[2] := TUIColor.Wrap((ConvertToUIColor(FFill.ColorMirror, FFill.Opacity) as ILocalObject).GetObjectID).CGColor;
        colorarr[3] := TUIColor.Wrap((ConvertToUIColor(FFill.ColorMirrorTo, FFill.Opacity) as ILocalObject).GetObjectID).CGColor;
      end
      else if FFill.ColorTo <> gcNull then
      begin
        SetLength(colorarr, 2);
        colorarr[0] := TUIColor.Wrap((ConvertToUIColor(FFill.Color, FFill.Opacity) as ILocalObject).GetObjectID).CGColor;
        colorarr[1] := TUIColor.Wrap((ConvertToUIColor(FFill.ColorTo, FFill.Opacity) as ILocalObject).GetObjectID).CGColor;
      end;
    end;
    gfgmCollection:
    begin
      f0 := False;
      f1 := False;
      for I := 0 to FFill.GradientItems.Count - 1 do
      begin
        it := FFill.GradientItems[I];
        if it.Position = 0.0 then
          f0 := True;

        if it.Position = 1.0 then
          f1 := True;
      end;

      SetLength(colorarr, FFill.GradientItems.Count);
      SetLength(positionsarr, FFill.GradientItems.Count);

      o := 0;
      if not f0 and (FFill.GradientItems.Count > 0) then
      begin
        SetLength(colorarr, Length(colorarr) + 1);
        colorarr[0] := TUIColor.Wrap((ConvertToUIColor(FFill.GradientItems[0].Color, FFill.GradientItems[0].Opacity) as ILocalObject).GetObjectID).CGColor;
        SetLength(positionsarr, Length(positionsarr) + 1);
        positionsarr[0] := 0;
        o := 1;
      end;

      for I := 0 to FFill.GradientItems.Count - 1 do
      begin
        it := FFill.GradientItems[I];
        colorarr[I + o] := TUIColor.Wrap((ConvertToUIColor(it.Color, it.Opacity) as ILocalObject).GetObjectID).CGColor;
        positionsarr[I + o] := it.Position;
      end;

      if not f1 and (FFill.GradientItems.Count > 0) then
      begin
        SetLength(colorarr, Length(colorarr) + 1);
        colorarr[Length(colorarr) - 1] := TUIColor.Wrap((ConvertToUIColor(FFill.GradientItems[FFill.GradientItems.Count - 1].Color,
          FFill.GradientItems[FFill.GradientItems.Count - 1].Opacity) as ILocalObject).GetObjectID).CGColor;
        SetLength(positionsarr, Length(positionsarr) + 1);
        positionsarr[Length(positionsarr) - 1] := 1;
      end;
    end;
  end;

  cs := CGColorSpaceCreateDeviceRGB;

  colors := TNSArray.OCClass.arrayWithObjects(@colorarr[0], Length(colorarr));
  positions := nil;
  if Length(positionsarr) > 0 then
    positions := @positionsarr[0];

  gradient := CGGradientCreateWithColors(cs, colors, positions);

  case FFill.GradientType of
    gfgtLinear:
    begin
      case FFill.Orientation of
        gfoHorizontal: CGContextDrawLinearGradient(FCGContext, gradient, CGPointMake(ARect.Left, ARect.Top + ARect.Height / 2), CGPointMake(ARect.Right, ARect.Top + ARect.Height / 2), 0);
        gfoVertical: CGContextDrawLinearGradient(FCGContext, gradient, CGPointMake(ARect.Left + ARect.Width / 2, ARect.Top), CGPointMake(ARect.Left + ARect.Width / 2, ARect.Bottom), 0);
        gfoCustom:
        begin
          a := DegToRad(FFill.GradientAngle);
          c := CGPointMake(ARect.Left + ARect.Width / 2, ARect.Top + ARect.Height / 2);
          pt0 := CGPointMake(c.x - Cos(a) * ARect.Width / 2, c.y - Sin(a) * ARect.height / 2);
          pt1 := CGPointMake(c.x + Cos(a) * ARect.Width / 2, c.y + Sin(a) * ARect.height / 2);
          CGContextDrawLinearGradient(FCGContext, gradient, pt0, pt1, 0);
        end;
      end;
    end;
    gfgtRadial:
    begin
      sx := 1;
      sy := 1;
      if (ARect.Width > 0) and (ARect.Height > 0) then
      begin
        if ARect.Width > ARect.Height then
        begin
          sy := 1;
          sx := ARect.Width / ARect.Height;
        end
        else if ARect.Height > ARect.Width then
        begin
          sx := 1;
          sy := ARect.Height / ARect.Width;
        end;
      end;

      st := CGAffineTransformMakeScale(sx, sy);
      ist := CGAffineTransformInvert(st);
      ivs := CGPointMake(ist.a, ist.d);

      c := CGPointMake(FFill.GradientCenterPoint.X * ivs.x, FFill.GradientCenterPoint.Y * ivs.y);

      rd := (ARect.Width / 2) * ivs.x;
      if sy > sx then
        rd := (ARect.Height / 2) * ivs.y;

      CGContextScaleCTM(FCGContext, st.a, st.d);

      CGContextDrawRadialGradient(FCGContext, gradient, c, 0, c, rd, kCGGradientDrawsBeforeStartLocation);

      CGContextScaleCTM(FCGContext, ivs.x, ivs.y);
    end;
  end;
  CGColorSpaceRelease(cs);
  CGGradientRelease(gradient);
end;

procedure TTMSFNCGraphicsContextiOS.DrawPath(AStroke: TTMSFNCGraphicsStroke;
  APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode);
var
  p: TTMSFNCGraphicsPathPolygon;
  pth: CGMutablePathRef;
begin
  if Assigned(APath) then
  begin
    if APathMode = pdmPath then
    begin
      if not Assigned(FCGContext) then
        Exit;

      pth := CGMutablePathRef(ConvertToPath(APath));
      try
        SaveContext;
        CGContextBeginPath(FCGContext);
        CGContextAddPath(FCGContext, pth);
        ApplyStroke;
        CGContextDrawPath(FCGContext, kCGPathStroke);
        RestoreContext;
      finally
        CGPathRelease(pth);
      end;
    end
    else
    begin
      FActivePath := APath;
      SetLength(p, 0);
      APath.FlattenToPolygon(p);
      case APathMode of
        pdmPolygon: DrawPolygon(AStroke, p);
        pdmPolyline: DrawPolyline(AStroke, p);
      end;
      FActivePath := nil;
    end;
  end;
end;

procedure TTMSFNCGraphicsContextiOS.DrawPolygon(AStroke: TTMSFNCGraphicsStroke;
  APolygon: TTMSFNCGraphicsPathPolygon);
var
  I: Integer;
begin
  if not Assigned(FCGContext) then
    Exit;

  if Length(APolygon) = 0 then
    Exit;

  SaveContext;
  CGContextBeginPath(FCGContext);
  CGContextMoveToPoint(FCGContext, APolygon[0].X, APolygon[0].Y);
  for I := 1 to Length(APolygon) - 1 do
    CGContextAddLineToPoint(FCGContext, APolygon[I].X, APolygon[I].Y);
  CGContextClosePath(FCGContext);
  ApplyStroke;
  CGContextDrawPath(FCGContext, kCGPathStroke);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextiOS.DrawPolyline(AStroke: TTMSFNCGraphicsStroke;
  APolyline: TTMSFNCGraphicsPathPolygon);
var
  I: Integer;
begin
  if not Assigned(FCGContext) then
    Exit;

  if Length(APolyline) = 0 then
    Exit;

  SaveContext;
  CGContextBeginPath(FCGContext);
  CGContextMoveToPoint(FCGContext, APolyline[0].X, APolyline[0].Y);
  for I := 1 to Length(APolyline) - 1 do
    CGContextAddLineToPoint(FCGContext, APolyline[I].X, APolyline[I].Y);
  ApplyStroke;
  CGContextDrawPath(FCGContext, kCGPathStroke);
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextiOS.DrawRect(AStroke: TTMSFNCGraphicsStroke;
  ARect: TRectF; ASides: TTMSFNCGraphicsSides;
  AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FCGContext) then
    Exit;

  r := ARect;
  if gsTop in ASides then
    DrawLine(AStroke, PointF(r.Left, r.Top), PointF(r.Right, r.Top));
  if gsLeft in ASides then
    DrawLine(AStroke, PointF(r.Left, r.Top), PointF(r.Left, r.Bottom));
  if gsBottom in ASides then
    DrawLine(AStroke, PointF(r.Left, r.Bottom), PointF(r.Right, r.Bottom));
  if gsRight in ASides then
    DrawLine(AStroke, PointF(r.Right, r.Top), PointF(r.Right, r.Bottom));
end;

procedure TTMSFNCGraphicsContextiOS.DrawRoundRect(
  AStroke: TTMSFNCGraphicsStroke; ARect: TRectF; ARounding: Single;
  ACorners: TTMSFNCGraphicsCorners;
  AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  pth: TTMSFNCGraphicsPath;
  r: TRectF;
  rc: Single;
begin
  r := ARect;
  rc := ARounding;

  pth := TTMSFNCGraphicsPath.Create;
  try
    if gcBottomLeft in ACorners then
    begin
      pth.MoveTo(PointF(r.Left + rc, r.Bottom));
      pth.AddArc(PointF(r.Left + rc, r.Bottom - rc), PointF(rc, rc), -270, 90);
      pth.LineTo(PointF(r.Left, r.Bottom - rc));
    end
    else
    begin
      pth.MoveTo(PointF(r.Left, r.Bottom));
    end;

    if gcTopLeft in ACorners then
    begin
      pth.LineTo(PointF(r.Left, r.Top + rc));
      pth.AddArc(PointF(r.Left + rc, r.Top + rc), PointF(rc, rc), -180, 90);
      pth.LineTo(PointF(r.Left + rc, r.Top));
    end
    else
      pth.LineTo(PointF(r.Left, r.Top));

    if gcTopRight in ACorners then
    begin
      pth.LineTo(PointF(r.Right - rc, r.Top));
      pth.AddArc(PointF(r.Right - rc, r.Top + rc), PointF(rc, rc), -90, 90);
      pth.LineTo(PointF(r.Right, r.Top + rc));
    end
    else
      pth.LineTo(PointF(r.Right, r.Top));

    if gcBottomRight in ACorners then
    begin
      pth.LineTo(PointF(r.Right, r.Bottom - rc));
      pth.AddArc(PointF(r.Right - rc, r.Bottom - rc), PointF(rc, rc), 0, 90);
      pth.LineTo(PointF(r.Right - rc, r.Bottom));
    end
    else
      pth.LineTo(PointF(r.Right, r.Bottom));

    if gcBottomLeft in ACorners then
      pth.LineTo(PointF(r.Left + rc, r.Bottom))
    else
      pth.LineTo(PointF(r.Left, r.Bottom));

    pth.ClosePath;

    DrawPath(FStroke, pth);
  finally
    pth.Free;
  end;
end;

procedure TTMSFNCGraphicsContextiOS.DrawText(AText: string; ARect: TRectF;
  AWordWrapping: Boolean; AHorizontalAlign,
  AVerticalAlign: TTMSFNCGraphicsTextAlign;
  ATrimming: TTMSFNCGraphicsTextTrimming; AAngle: Single);
var
  R: TRectF;
begin
  if not Assigned(FCGContext) then
    Exit;

  SaveContext;
  R := DrawTextInternal(AText, ARect, AWordWrapping, AHorizontalAlign, True);
  case AVerticalAlign of
    gtaCenter: DrawTextInternal(AText, RectF(ARect.Left, ARect.Top + (ARect.Height - r.Height) / 2, ARect.Right, ARect.Top + (ARect.Height - r.Height) / 2 + r.Height), AWordWrapping, AHorizontalAlign, False);
    gtaLeading: DrawTextInternal(AText, RectF(ARect.Left, ARect.Top, ARect.Right, ARect.Top + r.Height), AWordWrapping, AHorizontalAlign, False);
    gtaTrailing: DrawTextInternal(AText, RectF(ARect.Left, ARect.Bottom - r.Height, ARect.Right, ARect.Bottom), AWordWrapping, AHorizontalAlign, False);
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextiOS.EndScene;
begin
  if not Assigned(FCGContext) then
    Exit;

  Render;
  Canvas.EndScene;
end;

procedure TTMSFNCGraphicsContextiOS.FillArc(AFill: TTMSFNCGraphicsFill; ACenter,
  ARadius: TPointF; AStartAngle, ASweepAngle: Single);
begin
  if not Assigned(FCGContext) then
    Exit;

  SaveContext;
  CGContextBeginPath(FCGContext);
  {$HINTS OFF}
  {$IF COMPILERVERSION < 34}
  CGContextAddArc(FCGContext, ACenter.X, ACenter.Y, ARadius.X, DegToRad(AStartAngle), DegToRad(AStartAngle + ASweepAngle), 0);
  {$ELSE}
  CGContextAddArc(FCGContext, ACenter.X, ACenter.Y, ARadius.X, DegToRad(AStartAngle), DegToRad(AStartAngle + ASweepAngle), False);
  {$IFEND}
  {$HINTS ON}
  case AFill.Kind of
    gfkSolid:
    begin
      ApplyFill;
      CGContextDrawPath(FCGContext, kCGPathFill);
    end;
    gfkGradient:
    begin
      CGContextClip(FCGContext);
      DrawLinearGradient(RectF(ACenter.X - ARadius.X, ACenter.Y - ARadius.Y, ACenter.X + ARadius.X, ACenter.Y + ARadius.Y));
    end;
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextiOS.FillChanged(Sender: TObject);
begin
  if not Assigned(FCGContext) then
     Exit;

  ApplyFill;
end;

procedure TTMSFNCGraphicsContextiOS.FillEllipse(AFill: TTMSFNCGraphicsFill;
  ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FCGContext) then
    Exit;

  r := ARect;
  SaveContext;
  CGContextBeginPath(FCGContext);
  CGContextAddEllipseInRect(FCGContext, CGRectMake(r.Left, r.Top, r.Width, r.Height));
  case AFill.Kind of
    gfkSolid:
    begin
      ApplyFill;
      CGContextDrawPath(FCGContext, kCGPathFill);
    end;
    gfkGradient:
    begin
      CGContextClip(FCGContext);
      DrawLinearGradient(r);
    end;
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextiOS.FillPath(AFill: TTMSFNCGraphicsFill;
  APath: TTMSFNCGraphicsPath; APathMode: TTMSFNCGraphicsPathDrawMode);
var
  p: TTMSFNCGraphicsPathPolygon;
  pth: CGMutablePathRef;
begin
  if Assigned(APath) then
  begin
    if APathMode = pdmPath then
    begin
      SaveContext;
      pth := CGMutablePathRef(ConvertToPath(APath));
      CGContextBeginPath(FCGContext);
      CGContextAddPath(FCGContext, pth);
      case AFill.Kind of
        gfkSolid:
        begin
          ApplyFill;
          CGContextDrawPath(FCGContext, kCGPathFill);
        end;
        gfkGradient:
        begin
          CGContextClip(FCGContext);
          DrawLinearGradient(APath.GetBounds);
        end;
      end;
      RestoreContext;
    end
    else
    begin
      FActivePath := APath;
      SetLength(p, 0);
      APath.FlattenToPolygon(p);
      case APathMode of
        pdmPolygon: FillPolygon(AFill, p);
        pdmPolyline: FillPolyline(AFill, p);
      end;
      FActivePath := nil;
    end;
    FActivePath := nil;
  end;
end;

procedure TTMSFNCGraphicsContextiOS.FillPolygon(AFill: TTMSFNCGraphicsFill;
  APolygon: TTMSFNCGraphicsPathPolygon);
var
  I: Integer;
begin
  if not Assigned(FCGContext) then
    Exit;

  if Length(APolygon) = 0 then
    Exit;

  SaveContext;
  CGContextBeginPath(FCGContext);
  CGContextMoveToPoint(FCGContext, APolygon[0].X, APolygon[0].Y);
  for I := 1 to Length(APolygon) - 1 do
    CGContextAddLineToPoint(FCGContext, APolygon[I].X, APolygon[I].Y);
  CGContextClosePath(FCGContext);
  case AFill.Kind of
    gfkSolid:
    begin
      ApplyFill;
      CGContextDrawPath(FCGContext, kCGPathFill);
    end;
    gfkGradient:
    begin
      CGContextClip(FCGContext);
      if Assigned(FActivePath) then
        DrawLinearGradient(FActivePath.GetBounds);
    end;
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextiOS.FillPolyline(AFill: TTMSFNCGraphicsFill;
  APolyline: TTMSFNCGraphicsPathPolygon);
var
  I: Integer;
begin
  if not Assigned(FCGContext) then
    Exit;

  if Length(APolyline) = 0 then
    Exit;

  SaveContext;
  CGContextBeginPath(FCGContext);
  CGContextMoveToPoint(FCGContext, APolyline[0].X, APolyline[0].Y);
  for I := 1 to Length(APolyline) - 1 do
    CGContextAddLineToPoint(FCGContext, APolyline[I].X, APolyline[I].Y);
  case AFill.Kind of
    gfkSolid:
    begin
      ApplyFill;
      CGContextDrawPath(FCGContext, kCGPathFill);
    end;
    gfkGradient:
    begin
      CGContextClip(FCGContext);
      if Assigned(FActivePath) then
        DrawLinearGradient(FActivePath.GetBounds);
    end;
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextiOS.FillRect(AFill: TTMSFNCGraphicsFill;
  ARect: TRectF; AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  r: TRectF;
begin
  if not Assigned(FCGContext) then
    Exit;

  r := ARect;
  SaveContext;
  CGContextBeginPath(FCGContext);
  CGContextAddRect(FCGContext, CGRectMake(r.Left, r.Top, r.Width, r.Height));
  case AFill.Kind of
    gfkSolid:
    begin
      ApplyFill;
      CGContextDrawPath(FCGContext, kCGPathFill);
    end;
    gfkGradient:
    begin
      CGContextClip(FCGContext);
      DrawLinearGradient(r);
    end;
  end;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextiOS.FillRoundRect(AFill: TTMSFNCGraphicsFill;
  ARect: TRectF; ARounding: Single; ACorners: TTMSFNCGraphicsCorners;
  AModifyRectMode: TTMSFNCGraphicsModifyRectMode);
var
  pth: TTMSFNCGraphicsPath;
  r: TRectF;
  rc: Single;
begin
  r := ARect;
  rc := ARounding;

  pth := TTMSFNCGraphicsPath.Create;
  try
    if gcBottomLeft in ACorners then
    begin
      pth.MoveTo(PointF(r.Left + rc, r.Bottom));
      pth.AddArc(PointF(r.Left + rc, r.Bottom - rc), PointF(rc, rc), -270, 90);
      pth.LineTo(PointF(r.Left, r.Bottom - rc));
    end
    else
    begin
      pth.MoveTo(PointF(r.Left, r.Bottom));
    end;

    if gcTopLeft in ACorners then
    begin
      pth.LineTo(PointF(r.Left, r.Top + rc));
      pth.AddArc(PointF(r.Left + rc, r.Top + rc), PointF(rc, rc), -180, 90);
      pth.LineTo(PointF(r.Left + rc, r.Top));
    end
    else
      pth.LineTo(PointF(r.Left, r.Top));

    if gcTopRight in ACorners then
    begin
      pth.LineTo(PointF(r.Right - rc, r.Top));
      pth.AddArc(PointF(r.Right - rc, r.Top + rc), PointF(rc, rc), -90, 90);
      pth.LineTo(PointF(r.Right, r.Top + rc));
    end
    else
      pth.LineTo(PointF(r.Right, r.Top));

    if gcBottomRight in ACorners then
    begin
      pth.LineTo(PointF(r.Right, r.Bottom - rc));
      pth.AddArc(PointF(r.Right - rc, r.Bottom - rc), PointF(rc, rc), 0, 90);
      pth.LineTo(PointF(r.Right - rc, r.Bottom));
    end
    else
      pth.LineTo(PointF(r.Right, r.Bottom));

    if gcBottomLeft in ACorners then
      pth.LineTo(PointF(r.Left + rc, r.Bottom))
    else
      pth.LineTo(PointF(r.Left, r.Bottom));

    pth.ClosePath;

    FillPath(FFill, pth);
  finally
    pth.Free;
  end;
end;

procedure TTMSFNCGraphicsContextiOS.FontChanged(Sender: TObject);
begin
end;

function TTMSFNCGraphicsContextiOS.GetFillColor: TTMSFNCGraphicsColor;
begin
  Result := Graphics.Fill.Color;
end;

function TTMSFNCGraphicsContextiOS.GetFlipped(const R: TRectF): TRectF;
begin
  Result.Left := R.Left;
  Result.Top := FContextSize.Height - R.Bottom;
  Result.Right := R.Right;
  Result.Bottom := FContextSize.Height - R.Top;
end;

function TTMSFNCGraphicsContextiOS.GetMatrix: TTMSFNCGraphicsMatrix;
begin
  Result := FSaveMatrix;
end;

function TTMSFNCGraphicsContextiOS.GetNativeCanvas: Pointer;
begin
  Result := FCGContext;
end;

procedure TTMSFNCGraphicsContextiOS.PathClose(APath: Pointer);
begin
  CGPathCloseSubpath(APath);
end;

procedure TTMSFNCGraphicsContextiOS.PathLineTo(APath: Pointer; APoint: TPointF);
begin
  CGPathAddLineToPoint(APath, nil, APoint.X, APoint.Y);
end;

procedure TTMSFNCGraphicsContextiOS.PathMoveTo(APath: Pointer; APoint: TPointF);
begin
  CGPathMoveToPoint(APath, nil, APoint.X, APoint.Y);
end;

procedure TTMSFNCGraphicsContextiOS.PathOpen(APath: Pointer);
begin
end;

procedure TTMSFNCGraphicsContextiOS.Render;
begin
  if not FNeedsRendering then
    Exit;

  if Assigned(FCGContext) then
  begin
    if FMapping then
    begin
      FBitmap.Unmap(FBitmapData);
      FMapping := False;
    end;
    Canvas.DrawBitmap(FBitmap, RectF(0, 0, FBitmap.Width, FBitmap.Height), RectF(0, 0, FContextSize.Width, FContextSize.Height), 1);
  end;

  FNeedsRendering := False;
  DestroyResources;
end;

procedure TTMSFNCGraphicsContextiOS.ResetClip;
begin
  if not Assigned(FCGContext) then
    Exit;

  RestoreContext;
end;

procedure TTMSFNCGraphicsContextiOS.ResetTextAngle(AAngle: Single);
begin
  if not Assigned(FCGContext) then
    Exit;

  if AAngle <> 0 then
    RestoreContext;
end;

procedure TTMSFNCGraphicsContextiOS.ResetTransform;
begin
  if not Assigned(FCGContext) then
    Exit;

  RestoreContext;
  SaveContext;
end;

procedure TTMSFNCGraphicsContextiOS.RestoreState(
  AState: TTMSFNCGraphicsSaveState);
begin
  if not Assigned(FCGContext) then
    Exit;

  RestoreContext;
end;

procedure TTMSFNCGraphicsContextiOS.RotateTransform(AAngle: Single);
begin
  if not Assigned(FCGContext) then
    Exit;

  CGContextRotateCTM(FCGContext, DegToRad(AAngle));
end;

function TTMSFNCGraphicsContextiOS.DrawTextInternal(Text: string; Rect: TRectF; AWordWrap: Boolean; AHorizontalAlignment: TTMSFNCGraphicsTextAlign; Calculate: Boolean): TRectF;
var
  txt: NSString;
  dic: NSMutableDictionary;
  r: CGRect;
  ft: UIFont;
  ftdesc: UIFontDescriptor;
  ftms: Cardinal;
  par: NSMutableParagraphStyle;
begin
  SaveContext;
  UIGraphicsPushContext(FCGContext);
  txt := TNSString.Wrap(TNSString.OCClass.stringWithCharacters(@Text[Low(string)],Text.Length));
  dic := TNSMutableDictionary.Wrap(TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.alloc).init);
  ftms := 0;
  if TFontStyle.fsBold in FFont.Style then
    ftms := ftms or UIFontDescriptorTraitBold;

  if TFontStyle.fsItalic in FFont.Style then
    ftms := ftms or UIFontDescriptorTraitItalic;

  ft := TUIFont.Wrap(TUIFont.OCClass.fontWithName(NSSTREx(FFont.Name), FFont.Size));
  if not Assigned(ft.fontDescriptor) then
    ft := TUIFont.Wrap(TUIFont.OCClass.fontWithName(NSSTREx('Helvetica Neue'), FFont.Size));

  ftdesc := ft.fontDescriptor.fontDescriptorWithSymbolicTraits(ftms);
  ft := TUIFont.Wrap(TUIFont.OCClass.fontWithDescriptor(ftdesc, 0));
  dic.setValue((ft as ILocalObject).GetObjectID, NSSTREx('NSFont'));
  if TFontStyle.fsUnderline in FFont.Style then
    dic.setValue(TNSNumber.OCClass.numberWithInt(NSUnderlineStyleSingle), NSStrEx('NSUnderline'));
  if TFontStyle.fsStrikeOut in FFont.Style then
    dic.setValue(TNSNumber.OCClass.numberWithInt(NSUnderlineStyleSingle), NSStrEx('NSStrikethrough'));
  dic.setValue((ConvertToUIColor(FFont.Color, 1) as ILocalObject).GetObjectID, NSSTREx('NSColor'));
  par := TNSMutableParagraphStyle.Wrap(TNSMutableParagraphStyle.Wrap(TNSMutableParagraphStyle.OCClass.alloc).init);
  case AHorizontalAlignment of
    gtaLeading: par.setAlignment(0);
    gtaCenter: par.setAlignment(1);
    gtaTrailing: par.setAlignment(2);
  end;
  if AWordWrap then
    par.setLineBreakMode(NSLineBreakByWordWrapping);

  dic.setValue((par as ILocalObject).GetObjectID, NSSTREx('NSParagraphStyle'));
  if not Calculate then
    TNSStringEx.Wrap((txt as ILocalObject).GetObjectID).drawInRect(CGRectMake(Rect.Left, Rect.Top, Rect.Width, Rect.Height), dic);

  r := TNSStringEx.Wrap((txt as ILocalObject).GetObjectID).boundingRectWithSize(CGSizeMake(Rect.Width, Rect.Height), 1 shl 0, dic, nil);
  par.release;
  dic.release;
  Result := RectF(Rect.Left + r.origin.x, Rect.Top + r.origin.y, Rect.Left + r.origin.x + r.size.width, Rect.Top + r.origin.y + r.size.height);
  UIGraphicsPopContext;
  RestoreContext;
end;

procedure TTMSFNCGraphicsContextiOS.SaveContext;
begin
  if not Assigned(FCGContext) then
    Exit;

  CGContextSaveGState(FCGContext);
end;

procedure TTMSFNCGraphicsContextiOS.RestoreContext;
begin
  if not Assigned(FCGContext) then
    Exit;

  CGContextRestoreGState(FCGContext);
end;

procedure TTMSFNCGraphicsContextiOS.SaveState(AState: TTMSFNCGraphicsSaveState);
begin
  if not Assigned(FCGContext) then
    Exit;

  SaveContext;
end;

procedure TTMSFNCGraphicsContextiOS.ScaleTransform(AX, AY: Single);
begin
  if not Assigned(FCGContext) then
    Exit;

  CGContextScaleCTM(FCGContext, AX, AY);
end;

procedure TTMSFNCGraphicsContextiOS.SetSize(AWidth, AHeight: Single);
var
  cs: CGColorSpaceRef;
begin
  FContextSize.cx := AWidth;
  FContextSize.cy := AHeight;

  DestroyResources;

  FBitmap := TBitmap.Create(Round(AWidth * FScale), Round(AHeight * FScale));
  if FBitmap.Map(TMapAccess.Write, FBitmapData) then
  begin
    FMapping := True;
    cs := CGColorSpaceCreateDeviceRGB;
    FCGContext := CGBitmapContextCreate(FBitmapData.Data, FBitmapData.Width, FBitmapData.Height, 8, FBitmapData.Pitch, cs, kCGImageAlphaPremultipliedLast);
    CGColorSpaceRelease(cs);
    CGContextTranslateCTM(FCGContext, 0, FBitmapData.Height);
    CGContextScaleCTM(FCGContext, FScale, -FScale);
    SaveContext;
  end;
end;

procedure TTMSFNCGraphicsContextiOS.SetTextQuality(ATextQuality: TTMSFNCGraphicsTextQuality);
begin
end;

procedure TTMSFNCGraphicsContextiOS.SetShowAcceleratorChar(AShowAcceleratorChar: Boolean);
begin
end;

procedure TTMSFNCGraphicsContextiOS.SetAntiAliasing(AAntiAliasing: Boolean);
begin
  if not Assigned(FCGContext) then
    Exit;

  {$HINTS OFF}
  {$IF COMPILERVERSION < 34}
  CGContextSetAllowsAntialiasing(FCGContext, Integer(AAntiAliasing));
  {$ELSE}
  CGContextSetAllowsAntialiasing(FCGContext, AAntiAliasing);
  {$IFEND}
  {$HINTS ON}
end;

procedure TTMSFNCGraphicsContextiOS.SetNativeContext(ACGContext: CGContextRef);
begin
  FCGContext := ACGContext;
end;

procedure TTMSFNCGraphicsContextiOS.SetFill(AFill: TTMSFNCGraphicsFill);
begin
  if not Assigned(FCGContext) then
    Exit;

  FFill.Assign(AFill);
end;

procedure TTMSFNCGraphicsContextiOS.SetFillColor(AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FCGContext) then
    Exit;

  FFill.Color := AColor;
end;

procedure TTMSFNCGraphicsContextiOS.SetFillKind(AKind: TTMSFNCGraphicsFillKind);
begin
  if not Assigned(FCGContext) then
    Exit;

  FFill.Kind := AKind;
end;

procedure TTMSFNCGraphicsContextiOS.SetFont(AFont: TTMSFNCGraphicsFont);
begin
  if not Assigned(FCGContext) then
    Exit;

  FFont.Assign(AFont);
end;

procedure TTMSFNCGraphicsContextiOS.SetFontColor(AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FCGContext) then
    Exit;

  FFont.Color := AColor;
end;

procedure TTMSFNCGraphicsContextiOS.SetFontName(AName: string);
begin
  if not Assigned(FCGContext) then
    Exit;

  FFont.Name := AName;
end;

procedure TTMSFNCGraphicsContextiOS.SetFontSize(ASize: Integer);
begin
  if not Assigned(FCGContext) then
    Exit;

  FFont.Size := ASize;
end;

procedure TTMSFNCGraphicsContextiOS.SetFontStyles(AStyle: TFontStyles);
begin
  if not Assigned(FCGContext) then
    Exit;

  FFont.Style := AStyle;
end;

procedure TTMSFNCGraphicsContextiOS.SetMatrix(AMatrix: TTMSFNCGraphicsMatrix);
begin
  FSaveMatrix := AMatrix;

  if Assigned(FCGContext) then
    CGContextConcatCTM(FCGContext, CGAffineTransformMake(AMatrix.m11, AMatrix.m12, AMatrix.m21, AMatrix.m22, AMatrix.m31, AMatrix.m32));
end;

procedure TTMSFNCGraphicsContextiOS.SetScale(AScale: Single);
begin
  FScale := AScale;
end;

procedure TTMSFNCGraphicsContextiOS.SetStroke(AStroke: TTMSFNCGraphicsStroke);
begin
  if not Assigned(FCGContext) then
    Exit;

  FStroke.Assign(AStroke);
end;

procedure TTMSFNCGraphicsContextiOS.SetStrokeColor(
  AColor: TTMSFNCGraphicsColor);
begin
  if not Assigned(FCGContext) then
    Exit;

  FStroke.Color := AColor;
end;

procedure TTMSFNCGraphicsContextiOS.SetStrokeKind(
  AKind: TTMSFNCGraphicsStrokeKind);
begin
  if not Assigned(FCGContext) then
    Exit;

  FStroke.Kind := AKind;
end;

procedure TTMSFNCGraphicsContextiOS.SetStrokeWidth(AWidth: Single);
begin
  if not Assigned(FCGContext) then
    Exit;

  FStroke.Width := AWidth;
end;

function TTMSFNCGraphicsContextiOS.SetTextAngle(ARect: TRectF;
  AAngle: Single): TRectF;
var
  ar: Single;
  cx: TPointF;
  w, h, c, s, cw, ch: Single;
begin
  Result := ARect;
  if not Assigned(FCGContext) then
    Exit;

  if AAngle <> 0 then
  begin
    ar := DegToRad(AAngle);
    cx.X := Result.Left + Result.Width / 2;
    cx.Y := Result.Top + Result.Height / 2;

    SaveContext;
    CGContextConcatCTM(FCGContext, CGAffineTransformRotate(CGAffineTransformMakeTranslation(cx.X, cx.Y), ar));

    w := Result.Width;
    h := Result.Height;
    c := Cos(ar);
    s := Sin(ar);

    cw := (Abs(w * c) + Abs(h * s));
    ch := (Abs(w * s) + Abs(h * c));

    Result := RectF(-cw / 2, -ch / 2, cw / 2, ch / 2);
  end;
end;

procedure TTMSFNCGraphicsContextiOS.StartSpecialPen;
begin
  if not Assigned(FCGContext) then
    Exit;
end;

procedure TTMSFNCGraphicsContextiOS.StopSpecialPen;
begin
  if not Assigned(FCGContext) then
    Exit;
end;

procedure TTMSFNCGraphicsContextiOS.StrokeChanged(Sender: TObject);
begin
  if not Assigned(FCGContext) then
     Exit;

  ApplyStroke;
end;

procedure TTMSFNCGraphicsContextiOS.TranslateTransform(AX, AY: Single);
begin
  if not Assigned(FCGContext) then
    Exit;

  CGContextTranslateCTM(FCGContext, AX, AY);
end;

{$ENDIF}

end.
