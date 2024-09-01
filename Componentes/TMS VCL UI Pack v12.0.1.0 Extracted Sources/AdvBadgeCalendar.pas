{*************************************************************************}
{ TMS TAdvBadgeCalendar component                                         }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2020                                              }
{           Email : info@tmssoftware.com                                  }
{           Web : https://www.tmssoftware.com                             }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvBadgeCalendar;

{$I TMSDEFS.INC}

interface

uses
  Windows, SysUtils, Classes, Types, Graphics, Controls, Dialogs, ComCtrls,
  PlannerCal, AdvBadge, AdvStyleIF;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First version

type
  TAdvBadgeCalendar = class;

  TAdvBadgeCalendarBadgeEvent = procedure(Index: Integer; Date: TDateTime; Text: String) of object;

  TAdvBadgeCalendarBadge = class(TCollectionItem)
  private
    FBadgeCtrl: TAdvBadge;
    FColor: TColor;
    FDate: TDate;
    FText: string;
    FTextColor: TColor;
    procedure BadgeClick(Sender: TObject);
    procedure CreateBadge;
    procedure SetColor(const Value: TColor);
    procedure SetDate(const Value: TDate);
    procedure SetText(const Value: string);
    procedure SetTextColor(const Value: TColor);
    procedure UpdateBadge;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Color: TColor read FColor write SetColor default clRed;
    property Date: TDate read FDate write SetDate;
    property Text: string read FText write SetText;
    property TextColor: TColor read FTextColor write SetTextColor default clWhite;
  end;

  TAdvBadgeCalendarBadgeCollection = class(TOwnedCollection)
  private
    FOwner: TAdvBadgeCalendar;
    function GetItem(Index: Integer): TAdvBadgeCalendarBadge;
    procedure SetItem(Index: Integer; const Value: TAdvBadgeCalendarBadge);
  public
    constructor Create(AOwner: TAdvBadgeCalendar);
    function Add: TAdvBadgeCalendarBadge;
    function Insert(Index: Integer): TAdvBadgeCalendarBadge;
    property AdvBadgeCalendar: TAdvBadgeCalendar read FOwner;
    property Items[Index: Integer]: TAdvBadgeCalendarBadge read GetItem write SetItem; default;
  end;

  TAdvBadgeCalendar = class(TPlannerCalendar)
  private
    FBadges: TAdvBadgeCalendarBadgeCollection;
    FDefaultBadgeColor: TColor;
    FDefaultBadgeTextColor: TColor;
    FUpdateBadges: Boolean;
    FOnBadgeItemClick: TAdvBadgeCalendarBadgeEvent;

    procedure SetDefaultBadgeColor(const Value: TColor);
    procedure SetDefaultBadgeTextColor(const Value: TColor);
    procedure UpdateBadges;
  protected
    procedure DoBadgeItemClick(Index: Integer; Date: TDateTime; Text: String); virtual;
    procedure Loaded; override;
    procedure MonthChange; override;
    procedure YearChange; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Badges: TAdvBadgeCalendarBadgeCollection read FBadges write FBadges;
    property DefaultBadgeColor: TColor read FDefaultBadgeColor write SetDefaultBadgeColor default clRed;
    property DefaultBadgeTextColor: TColor read FDefaultBadgeTextColor write SetDefaultBadgeTextColor default clWhite;

    property OnBadgeItemClick: TAdvBadgeCalendarBadgeEvent read FOnBadgeItemClick write FOnBadgeItemClick;
  end;

implementation

{ TAdvBadgeCalendar }

constructor TAdvBadgeCalendar.Create(AOwner: TComponent);
begin
  inherited;
  FBadges := TAdvBadgeCalendarBadgeCollection.Create(Self);
  FDefaultBadgeColor := clRed;
  FDefaultBadgeTextColor := clWhite;
  FUpdateBadges := True;
end;

destructor TAdvBadgeCalendar.Destroy;
begin
  FBadges.Free;
  inherited;
end;

procedure TAdvBadgeCalendar.DoBadgeItemClick(Index: Integer; Date: TDateTime;
  Text: String);
begin
  if Assigned(OnBadgeItemClick) then
    OnBadgeItemClick(Index, Date, Text);
end;

procedure TAdvBadgeCalendar.Loaded;
begin
  inherited;
  UpdateBadges;
end;

procedure TAdvBadgeCalendar.MonthChange;
begin
  inherited;
  UpdateBadges;
end;

procedure TAdvBadgeCalendar.Paint;
begin
  inherited;
  if FUpdateBadges then
  begin
    FUpdateBadges := False;
    UpdateBadges;
  end;
end;

procedure TAdvBadgeCalendar.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  FUpdateBadges := True;
end;

procedure TAdvBadgeCalendar.SetDefaultBadgeColor(const Value: TColor);
begin
  if Value <> FDefaultBadgeColor then
  begin
    FDefaultBadgeColor := Value;
  end;
end;

procedure TAdvBadgeCalendar.SetDefaultBadgeTextColor(const Value: TColor);
begin
  if FDefaultBadgeTextColor <> Value then
    FDefaultBadgeTextColor := Value;
end;

procedure TAdvBadgeCalendar.UpdateBadges;
var
  I: Integer;
begin
  if Assigned(Badges) then
  begin
    for I := 0 to Badges.Count - 1 do
    begin
      Badges.Items[I].UpdateBadge;
    end;
  end;
end;

procedure TAdvBadgeCalendar.YearChange;
begin
  inherited;
  UpdateBadges;
end;

{ TAdvBadgeCalendarBadgeCollection }

function TAdvBadgeCalendarBadgeCollection.Add: TAdvBadgeCalendarBadge;
begin
  Result := TAdvBadgeCalendarBadge(inherited Add);
end;

constructor TAdvBadgeCalendarBadgeCollection.Create(AOwner: TAdvBadgeCalendar);
begin
  inherited Create(AOwner, TAdvBadgeCalendarBadge);
  FOwner := AOwner;
end;

function TAdvBadgeCalendarBadgeCollection.GetItem(
  Index: Integer): TAdvBadgeCalendarBadge;
begin
  Result := TAdvBadgeCalendarBadge(inherited Items[Index]);
end;

function TAdvBadgeCalendarBadgeCollection.Insert(
  Index: Integer): TAdvBadgeCalendarBadge;
begin
  Result := TAdvBadgeCalendarBadge(inherited Insert(Index));
end;

procedure TAdvBadgeCalendarBadgeCollection.SetItem(Index: Integer;
  const Value: TAdvBadgeCalendarBadge);
begin
  inherited Items[Index] := Value;
end;

{ TAdvBadgeCalendarBadge }

procedure TAdvBadgeCalendarBadge.BadgeClick(Sender: TObject);
begin
  (Collection as TAdvBadgeCalendarBadgeCollection).FOwner.DoBadgeItemClick(ID, Date, Text);
end;

constructor TAdvBadgeCalendarBadge.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FBadgeCtrl := nil;
  FText := '';
  FDate := 0;
  FColor := (Collection as TAdvBadgeCalendarBadgeCollection).FOwner.FDefaultBadgeColor;
  FTextColor := (Collection as TAdvBadgeCalendarBadgeCollection).FOwner.FDefaultBadgeTextColor;
end;

procedure TAdvBadgeCalendarBadge.CreateBadge;
begin
  FBadgeCtrl := TAdvBadge.Create((Collection as TAdvBadgeCalendarBadgeCollection).FOwner);
  FBadgeCtrl.Parent := (Collection as TAdvBadgeCalendarBadgeCollection).FOwner.Parent;
  FBadgeCtrl.OnClick := BadgeClick;
  UpdateBadge;
end;

destructor TAdvBadgeCalendarBadge.Destroy;
begin
  inherited;
end;

procedure TAdvBadgeCalendarBadge.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    UpdateBadge;
  end;
end;

procedure TAdvBadgeCalendarBadge.SetDate(const Value: TDate);
begin
  if FDate <> Value then
  begin
    FDate := Value;

    if (Value > 0) and (Text <> '') then
    begin
      if not Assigned(FBadgeCtrl) then
        CreateBadge
      else
        UpdateBadge;
    end
    else
    begin
      if Assigned(FBadgeCtrl) then
      begin
        FBadgeCtrl.Free;
        FBadgeCtrl := nil;
      end;
    end;     
  end;
end;

procedure TAdvBadgeCalendarBadge.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;

    if (Value <> '') and (Date > 0) then
    begin
      if not Assigned(FBadgeCtrl) then
        CreateBadge
      else
        UpdateBadge;
    end
    else
    begin
      if Assigned(FBadgeCtrl) then
      begin
        FBadgeCtrl.Free;
        FBadgeCtrl := nil;
      end;
    end;     
  end;
end;

procedure TAdvBadgeCalendarBadge.SetTextColor(const Value: TColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    UpdateBadge;
  end;
end;

procedure TAdvBadgeCalendarBadge.UpdateBadge;
var
  r: TRect;
begin
  if Assigned(FBadgeCtrl) and (Date > 0) and not (csDestroying in FBadgeCtrl.ComponentState) then
  begin
    r := (Collection as TAdvBadgeCalendarBadgeCollection).FOwner.DateToRect(Date);
    if (r.Top > 0) and (r.Left > 0) and (r.Right > 0) and (r.Bottom > 0) then
    begin
      FBadgeCtrl.Visible := true;
      FBadgeCtrl.Color := FColor;
      FBadgeCtrl.Font.Color := FTextColor;
      FBadgeCtrl.Text := Text;
      FBadgeCtrl.Left := (Collection as TAdvBadgeCalendarBadgeCollection).FOwner.Left + r.Right - (r.Width div 3);
      FBadgeCtrl.Top := (Collection as TAdvBadgeCalendarBadgeCollection).FOwner.Top + r.Top + (r.Height div 4) - (FBadgeCtrl.Height div 2);
    end
    else
    begin
      FBadgeCtrl.Visible := False;
    end;
  end;
end;

end.
