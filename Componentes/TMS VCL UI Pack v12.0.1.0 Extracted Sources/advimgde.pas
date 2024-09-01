{*************************************************************************}
{ TAdvImage design editor                                                 }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written                                                                 }
{   TMS Software                                                          }
{   copyright � 2000 - 2016                                               }
{   Email : info@tmssoftware.com                                          }
{   Web : http://www.tmssoftware.com                                      }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit advimgde;

interface

{$I TMSDEFS.INC}

uses
  Forms, Windows, Classes, Controls,Dialogs,ExtDlgs,AdvImage,
  DesignIntf, DesignEditors
  ;


type
  TAdvImageProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: String); override;
    function GetValue: String; override;
  end;


implementation

{ TAdvImageProperty }

procedure TAdvImageProperty.Edit;
var
  OpenDialog: TOpenPictureDialog;
begin
  inherited;
  OpenDialog:=TOpenPictureDialog.Create(nil);

  OpenDialog.Filter:= 'All (*.jpg;*.jpeg;*.gif;*.bmp;*.ico;*.emf;*.wmf;*.svg)|*.jpg;*.jpeg;*.gif;*.bmp;*.ico;*.emf;*.wmf;*.svg|JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.jpeg|GIF files (*.gif)|*.gif|Bitmaps (*.bmp)|*.bmp|Icons (*.ico)|*.ico|';
  OpenDialog.Filter := OpenDialog.Filter+'Enhanced Metafiles (*.emf)|*.emf|Metafiles (*.wmf)|*.wmf|Scalable Vector Graphics (*.svg)|*.svg';

  if OpenDialog.Execute then
  begin
    TAdvImage(GetOrdValue).LoadFromFile(Opendialog.FileName);
    Designer.Modified;
  end;
  Opendialog.Free;
end;

function TAdvImageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TAdvImageProperty.GetValue: String;
begin
  if not TAdvImage(GetOrdValue).Empty then
    Result := '(TAdvImage)'
  else
    Result := '(None)';
end;

procedure TAdvImageProperty.SetValue(const Value: String);
begin
  inherited;
  if Value = '' then
  begin
    TAdvImage(GetOrdValue).Assign(nil);
    Designer.Modified;
  end;
end;



end.
