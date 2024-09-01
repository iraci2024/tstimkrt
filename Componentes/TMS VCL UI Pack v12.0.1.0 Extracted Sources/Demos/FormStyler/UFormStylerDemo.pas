{********************************************************************}
{ TMS Office Form Styler Demo                                        }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012                                        }
{            Email : info@tmssoftware.com                            }
{            Website : http://www.tmssoftware.com                    }
{********************************************************************}

unit UFormStylerDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdvSmoothGauge, AdvSmoothTrackBar, AdvSmoothListBox, AdvSmoothMenu,
  AdvAppStyler, StdCtrls, AdvStyleIF, AdvSmoothCalendar;

type
  TForm225 = class(TForm)
    AdvFormStyler1: TAdvFormStyler;
    AdvSmoothMenu1: TAdvSmoothMenu;
    AdvSmoothListBox1: TAdvSmoothListBox;
    AdvSmoothTrackBar1: TAdvSmoothTrackBar;
    AdvSmoothGauge1: TAdvSmoothGauge;
    ComboBox1: TComboBox;
    AdvSmoothCalendar1: TAdvSmoothCalendar;
    Label19: TLabel;
    Label18: TLabel;
    procedure Label19Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form225: TForm225;

implementation
uses
  ShellAPI;

{$R *.dfm}

procedure TForm225.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
