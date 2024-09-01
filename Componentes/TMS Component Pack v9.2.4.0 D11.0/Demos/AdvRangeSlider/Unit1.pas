unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, AdvTrackBar, Vcl.StdCtrls,
  AdvAppStyler, AdvStyleIF;

type
  TForm1 = class(TForm)
    AdvRangeSlider1: TAdvRangeSlider;
    AdvRangeSlider2: TAdvRangeSlider;
    PaintBox1: TPaintBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    procedure PaintBox1Paint(Sender: TObject);
    procedure AdvRangeSlider1ChangeLeft(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.AdvRangeSlider1ChangeLeft(Sender: TObject);
begin
  Paintbox1.Invalidate;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  r: TRect;
  h: integer;
begin
  //

  R := Paintbox1.ClientRect;
  Paintbox1.Canvas.Brush.Color := clWhite;
  Paintbox1.Canvas.Pen.Color := $F0F0F0;
  Paintbox1.Canvas.Rectangle(r);

  h := r.Height;

  r.Left := (AdvRangeSlider1.PositionLeft * 2) * 3;
  r.Right := (AdvRangeSlider1.PositionRight * 2) * 3;
  r.Top := h - (AdvRangeSlider2.PositionRight * 2) * 3;
  r.Bottom := h- (AdvRangeSlider2.PositionLeft * 2) * 3;


  Paintbox1.Canvas.Brush.Color := clYellow;
  Paintbox1.Canvas.Pen.Color := clRed;

  Paintbox1.Canvas.Rectangle(R);
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
var
  ts: TThumbShape;
begin
  ts := TThumbShape(RadioGroup1.ItemIndex);

  AdvRangeSlider1.ThumbLeft.Shape := ts;
  AdvRangeSlider1.ThumbRight.Shape := ts;

  AdvRangeSlider2.ThumbLeft.Shape := ts;
  AdvRangeSlider2.ThumbRight.Shape := ts;
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
var
  st: TTMSStyle;
begin
  case RadioGroup2.ItemIndex of
  0: st := tsOffice2007Luna;
  1: st := tsOffice2010Blue;
  2: st := tsOffice2013White;
  3: st := tsOffice2016White;
  end;

  AdvRangeSlider1.SetComponentStyle(st);
  AdvRangeSlider2.SetComponentStyle(st);
end;

end.
