{*************************************************************************}
{                                                                         }
{ written by TMS Software                                                 }
{           copyright (c) 2020 - 2021                                     }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit FMX.TMSFNCDataBindingRegDE;

{$I FMX.TMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, FMX.TMSFNCDataBindingDE, FMX.TMSFNCDataBinding, FMX.TMSFNCTypes
  {$IFNDEF LCLWEBLIB}
  ,DesignIntF
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,WEBLib.DesignIntf
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,ComponentEditors, PropEdits
  {$ENDIF}
  ;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponentEditor(TTMSFNCDataBinder, TTMSFNCDataBinderEditor);
  RegisterPropertyEditor(TypeInfo(string),TTMSFNCDataBinderPropertyName,'Value', TTMSFNCDataBinderObjectProperty);
  RegisterPropertyEditor(TypeInfo(string),TTMSFNCDataBinderFieldName,'Value', TTMSFNCDataBinderFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TTMSFNCDataBinderItem,'PropertyName', TTMSFNCDataBinderObjectProperty);
  RegisterPropertyEditor(TypeInfo(string),TTMSFNCDataBinderItem,'ColumnsPropertyName', TTMSFNCDataBinderColumnsProperty);
  RegisterPropertyEditor(TypeInfo(string),TTMSFNCDataBinderItem,'ColumnsSubPropertyName', TTMSFNCDataBinderColumnsSubProperty);
  RegisterPropertyEditor(TypeInfo(string),TTMSFNCDataBinderItem,'FieldName', TTMSFNCDataBinderFieldNameProperty);
end;

{$IFDEF WEBLIB}
initialization
  Register;
{$ENDIF}

end.

