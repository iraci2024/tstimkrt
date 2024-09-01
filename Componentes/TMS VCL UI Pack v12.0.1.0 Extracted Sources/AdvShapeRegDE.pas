{********************************************************************}
{ AdvShapeRegDE components                                           }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2007 - 2020                                 }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit AdvShapeRegDE;

interface
{$I TMSDEFS.INC}

uses
  Classes, AdvShape, htmlde, DesignIntf, DesignEditors;

type
  TAdvShapeEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const Prop:IProperty; var Continue:Boolean); override;
  public
  end;


procedure Register;

implementation

uses
  SysUtils;


procedure TAdvShapeEditor.EditProperty(const Prop:IProperty; var Continue:Boolean);
var
  PropName: string;
begin
  PropName := Prop.GetName;
  if (CompareText(PropName, 'Text') = 0) then
  begin
    Prop.Edit;
    Continue := False;
  end;
end;


procedure Register;
begin
  {$IFDEF DELPHI_UNICODE}
  RegisterPropertyEditor(TypeInfo(String), TAdvShape, 'Text', THTMLStringProperty);
  {$ENDIF}
  RegisterComponentEditor(TAdvShape,TAdvShapeEditor);
end;



end.

