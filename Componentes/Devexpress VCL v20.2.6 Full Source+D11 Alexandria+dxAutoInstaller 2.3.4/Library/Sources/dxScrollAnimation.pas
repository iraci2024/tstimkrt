{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ Express Cross Platform Library graphics classes }
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

unit dxScrollAnimation;

{$I cxVer.inc}

interface

uses
  SysUtils, dxAnimation;

const
  dxScrollAnimationTime: Cardinal = 500;

type
  TdxRecordPixelScrollData = record // for internal use
    Index: Integer;
    ScrollOffset: Integer;
    constructor Create(AIndex: Integer; AOffset: Integer);
  end;

  TdxCustomScrollAnimationHelper<T> = class // for internal use
  strict private
    FAnimation: TdxAnimationTransition;
    FAnimationLength: Integer;
    FIsAnimationProcessing: Boolean;
    FPrevAnimatedPosition: Integer;
    FStartPosition: T;
    FTargetPosition: T;
    procedure DestroyAnimation;
    procedure InternalDoAnimation(Sender: TdxAnimationTransition;
      var APosition: Integer; var AFinished: Boolean);
    procedure InternalSetTargetPosition(AValue: T);
    procedure ResetTargetPosition;
  protected
    procedure AfterAnimationEnded(AIsRaiseEvent: Boolean); virtual;
    procedure AnimationTerminated(Sender: TObject); virtual;
    procedure BeforeAnimationStarted; virtual;
    function CalculateAnimatedPosition(var APosition: Integer): T;
      virtual; abstract;
    function CalculateScrollDelta(AValue: T): Integer; virtual; abstract;
    procedure CheckPosition(var AValue: T); virtual;
    procedure DoAnimation(Sender: TdxAnimationTransition;
      var APosition: Integer; var AFinished: Boolean); virtual;
    function GetOwnerPosition: T; virtual; abstract;
    function GetUnassignedTargetPosition: T; virtual; abstract;
    function IsAnimationActive: Boolean; virtual;
    function IsAnimationProcessing: Boolean;
    function IsAnimationStartNeeded: Boolean; virtual;
    function IsEqual(AValue1, AValue2: T): Boolean; virtual; abstract;
    function IsPositionChanged(AValue1, AValue2: T): Boolean; virtual;
    procedure PositionChanged; virtual;
    procedure SetTargetPosition(AValue: T); virtual;
    procedure StopAnimation(AIsRaiseEvent: Boolean); virtual;
    procedure UpdateOwnerPosition(AValue: T); virtual; abstract;

    property AnimationLength: Integer read FAnimationLength;
    property PrevAnimatedPosition: Integer read FPrevAnimatedPosition;
    property StartPosition: T read FStartPosition;
    property TargetPosition: T read FTargetPosition;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetPosition(AValue: T);
  end;

  TdxScrollAnimationHelper = class(TdxCustomScrollAnimationHelper<Integer>)
  // for internal use
  protected
    function CalculateAnimatedPosition(var APosition: Integer)
      : Integer; override;
    function CalculateScrollDelta(AValue: Integer): Integer; override;
    function GetUnassignedTargetPosition: Integer; override;
    function IsEqual(AValue1, AValue2: Integer): Boolean; override;
  end;

  TdxRecordPixelScrollAnimationHelper = class
    (TdxCustomScrollAnimationHelper<TdxRecordPixelScrollData>)
  // for internal use
  protected
    function GetUnassignedTargetPosition: TdxRecordPixelScrollData; override;
    function IsEqual(AValue1, AValue2: TdxRecordPixelScrollData)
      : Boolean; override;
    function IsPositionChanged(AValue1, AValue2: TdxRecordPixelScrollData)
      : Boolean; override;
  end;

implementation

uses
  Math;

{ TdxRecordPixelScrollData }

constructor TdxRecordPixelScrollData.Create(AIndex: Integer; AOffset: Integer);
begin
  Index := AIndex;
  ScrollOffset := AOffset;
end;

{ TdxCustomScrollAnimationHelper }

constructor TdxCustomScrollAnimationHelper<T>.Create;
begin
  inherited Create;
  ResetTargetPosition;
end;

destructor TdxCustomScrollAnimationHelper<T>.Destroy;
begin
  DestroyAnimation;
  inherited;
end;

procedure TdxCustomScrollAnimationHelper<T>.SetPosition(AValue: T);
var
  ATime: Cardinal;
  AWasActive: Boolean;
begin
  CheckPosition(AValue);
  if not IsEqual(FTargetPosition, AValue) then
  begin
    FAnimationLength := CalculateScrollDelta(AValue);
    if IsAnimationStartNeeded then
    begin
      AWasActive := IsAnimationActive;
      DestroyAnimation;
      if not AWasActive then
        BeforeAnimationStarted;
      SetTargetPosition(AValue);
      if FAnimationLength = 0 then
        ATime := 10
      else
        ATime := dxScrollAnimationTime;
      FAnimation := TdxAnimationTransition.Create(ATime,
        ateAccelerateDecelerate, Abs(FAnimationLength));
      FAnimation.OnAnimate := InternalDoAnimation;
      FAnimation.FreeOnTerminate := False;
      FAnimation.OnTerminate := AnimationTerminated;
      FAnimation.Resume;
    end;
  end;
end;

procedure TdxCustomScrollAnimationHelper<T>.AfterAnimationEnded
  (AIsRaiseEvent: Boolean);
begin
  if AIsRaiseEvent and IsPositionChanged(StartPosition, GetOwnerPosition) then
    PositionChanged;
end;

procedure TdxCustomScrollAnimationHelper<T>.AnimationTerminated
  (Sender: TObject);
begin
  ResetTargetPosition;
  AfterAnimationEnded(True);
end;

procedure TdxCustomScrollAnimationHelper<T>.BeforeAnimationStarted;
begin
  FStartPosition := GetOwnerPosition;
end;

procedure TdxCustomScrollAnimationHelper<T>.CheckPosition(var AValue: T);
begin
end;

procedure TdxCustomScrollAnimationHelper<T>.DoAnimation
  (Sender: TdxAnimationTransition; var APosition: Integer;
  var AFinished: Boolean);
var
  AValue: T;
begin
  AValue := CalculateAnimatedPosition(APosition);
  FPrevAnimatedPosition := APosition;
  UpdateOwnerPosition(AValue);
  AFinished := IsEqual(AValue, TargetPosition);
end;

function TdxCustomScrollAnimationHelper<T>.IsAnimationActive: Boolean;
begin
  Result := not IsEqual(FTargetPosition, GetUnassignedTargetPosition);
end;

function TdxCustomScrollAnimationHelper<T>.IsAnimationProcessing: Boolean;
begin
  Result := FIsAnimationProcessing;
end;

function TdxCustomScrollAnimationHelper<T>.IsAnimationStartNeeded: Boolean;
begin
  Result := FAnimationLength <> 0
end;

function TdxCustomScrollAnimationHelper<T>.IsPositionChanged(AValue1,
  AValue2: T): Boolean;
begin
  Result := not IsEqual(AValue1, AValue2);
end;

procedure TdxCustomScrollAnimationHelper<T>.PositionChanged;
begin
end;

procedure TdxCustomScrollAnimationHelper<T>.SetTargetPosition(AValue: T);
begin
  InternalSetTargetPosition(AValue);
  FPrevAnimatedPosition := 0;
end;

procedure TdxCustomScrollAnimationHelper<T>.StopAnimation
  (AIsRaiseEvent: Boolean);
begin
  DestroyAnimation;
  AfterAnimationEnded(AIsRaiseEvent);
end;

procedure TdxCustomScrollAnimationHelper<T>.DestroyAnimation;
begin
  FreeAndNil(FAnimation);
  ResetTargetPosition;
end;

procedure TdxCustomScrollAnimationHelper<T>.InternalDoAnimation
  (Sender: TdxAnimationTransition; var APosition: Integer;
  var AFinished: Boolean);
begin
  FIsAnimationProcessing := True;
  try
    DoAnimation(Sender, APosition, AFinished);
  finally
    FIsAnimationProcessing := False;
  end;
end;

procedure TdxCustomScrollAnimationHelper<T>.InternalSetTargetPosition
  (AValue: T);
begin
  FTargetPosition := AValue;
end;

procedure TdxCustomScrollAnimationHelper<T>.ResetTargetPosition;
begin
  InternalSetTargetPosition(GetUnassignedTargetPosition);
end;

function TdxScrollAnimationHelper.CalculateAnimatedPosition
  (var APosition: Integer): Integer;
begin
  Result := TargetPosition - AnimationLength + Sign(AnimationLength) *
    APosition;
end;

function TdxScrollAnimationHelper.CalculateScrollDelta(AValue: Integer)
  : Integer;
begin
  Result := AValue - GetOwnerPosition;
end;

function TdxScrollAnimationHelper.GetUnassignedTargetPosition: Integer;
begin
  Result := -1;
end;

function TdxScrollAnimationHelper.IsEqual(AValue1, AValue2: Integer): Boolean;
begin
  Result := AValue1 = AValue2;
end;

function TdxRecordPixelScrollAnimationHelper.GetUnassignedTargetPosition
  : TdxRecordPixelScrollData;
begin
  Result.Index := -1;
  Result.ScrollOffset := MaxInt;
end;

function TdxRecordPixelScrollAnimationHelper.IsEqual(AValue1,
  AValue2: TdxRecordPixelScrollData): Boolean;
begin
  Result := (AValue1.Index = AValue2.Index) and
    (AValue1.ScrollOffset = AValue2.ScrollOffset);
end;

function TdxRecordPixelScrollAnimationHelper.IsPositionChanged(AValue1,
  AValue2: TdxRecordPixelScrollData): Boolean;
begin
  Result := AValue1.Index <> AValue2.Index;
end;

end.
