
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//  StringMap Editor
//////////////////////////////////////////////////

{$I SB.inc}

unit ScStringMapEditor;

interface

{$IFNDEF FPC}
{$DEFINE USE_VALEDIT}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  StdCtrls, ScDesignUtils, ScHttp,
{$IFDEF USE_VALEDIT}
  ValEdit,
{$ENDIF}
  ScEditor, Buttons;

type
  //Don't delete, that don't used TScEditorForm.ReplaceMemos
  TValuesMemo = class(TMemo);

  TScStringMapEditorForm = class(TScEditorForm)
  private
    FComponent: TComponent;
    FValueEditor: {$IFDEF USE_VALEDIT}TValueListEditor{$ELSE}TValuesMemo{$ENDIF};

    procedure UpdateHeaders();

    procedure SetHeaders(const Value: TScWebHeaderCollection);
    function GetLines: TStrings;
  protected
    FHeaders: TScWebHeaderCollection;

    function GetComponent: TComponent; override;
    procedure SetComponent(Value: TComponent); override;

    procedure DoSave; override;

    property Lines: TStrings read GetLines;
  public
    constructor Create(Owner: TComponent; ScDesignUtilsClass: TScDesignUtilsClass); override;

    property Headers: TScWebHeaderCollection read FHeaders write SetHeaders;
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

{ TStringMapEditorForm }

constructor TScStringMapEditorForm.Create(Owner: TComponent; ScDesignUtilsClass: TScDesignUtilsClass);
const
  Margin = 8;
begin
  inherited;

  FValueEditor := {$IFDEF USE_VALEDIT}TValueListEditor{$ELSE}TValuesMemo{$ENDIF}.Create(Self);
  FValueEditor.Parent := Self;
  FValueEditor.Left := Margin;
  FValueEditor.Top := Margin;
  FValueEditor.Width := ClientWidth - Margin * 2;
  FValueEditor.Height := ClientHeight - BtnPanel.Height - Margin * 2;
  FValueEditor.Anchors := [akLeft, akTop, akRight, akBottom];
  FValueEditor.TabOrder := 0;
  BtnPanel.TabOrder := 1;

  {$IFDEF USE_VALEDIT}
  FValueEditor.KeyOptions := [keyEdit, keyAdd, keyDelete]; //keyUnique
  {$ELSE}
  FValueEditor.ScrollBars := ssBoth;
  FValueEditor.WordWrap := False;
  {$ENDIF}
end;

procedure TScStringMapEditorForm.UpdateHeaders;
begin
  Lines.Clear;

  if Assigned(Headers) then
    Lines.Text := Headers.Text;
end;

procedure TScStringMapEditorForm.SetHeaders(const Value: TScWebHeaderCollection);
begin
  FHeaders := Value;
  UpdateHeaders;
end;

function TScStringMapEditorForm.GetLines: TStrings;
begin
  Result := FValueEditor.{$IFDEF USE_VALEDIT}Strings{$ELSE}Lines{$ENDIF};
end;

function TScStringMapEditorForm.GetComponent: TComponent;
begin
  Result := FComponent;
end;

procedure TScStringMapEditorForm.SetComponent(Value: TComponent);
begin
  FComponent := Value;
end;

procedure TScStringMapEditorForm.DoSave;
begin
  Headers.Text := Lines.Text;
end;

end.
