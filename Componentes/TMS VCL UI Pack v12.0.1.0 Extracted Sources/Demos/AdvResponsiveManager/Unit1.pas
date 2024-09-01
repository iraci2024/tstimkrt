unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCL.TMSFNCCustomComponent,
  VCL.TMSFNCStateManager, VCL.TMSFNCResponsiveManager, AdvGlowButton,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Imaging.jpeg, Vcl.Imaging.GIFImg, Vcl.Grids;

const
  MAXIMG = 20;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    AdvGlowButton1: TAdvGlowButton;
    AdvGlowButton2: TAdvGlowButton;
    AdvGlowButton3: TAdvGlowButton;
    AdvGlowButton4: TAdvGlowButton;
    Edit1: TEdit;
    AdvGlowButton5: TAdvGlowButton;
    AdvGlowButton6: TAdvGlowButton;
    AdvGlowButton7: TAdvGlowButton;
    ScrollBox1: TScrollBox;
    TMSFNCResponsiveManager1: TTMSFNCResponsiveManager;
    PaintBox1: TPaintBox;
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    jpgs: array[0..MAXIMG] of TJPEGImage;
  end;

var
  Form1: TForm1;

implementation

uses
  Winapi.Wincodec;

//{$DEFINE HIGHQ}

{$R *.dfm}

procedure TForm1.FormShow(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to MAXIMG do
  begin
    jpgs[i] := TJPEGImage.Create;
    jpgs[i].LoadFromFile('.\data\'+(i+1).tostring+'.jpg');
  end;
end;

// high quality strectch
procedure ResizeBitmap(Bitmap: TBitmap; const NewWidth, NewHeight: integer);
var
  Factory: IWICImagingFactory;
  Scaler: IWICBitmapScaler;
  Source : TWICImage;
begin
  Source := TWICImage.Create;
  try
    Factory := TWICImage.ImagingFactory;
    Source.Assign(Bitmap);
    Factory.CreateBitmapScaler(Scaler);
    Scaler.Initialize(Source.Handle, NewWidth, NewHeight,
      WICBitmapInterpolationModeCubic);
    Source.Handle := IWICBitmap(Scaler);
    Bitmap.Assign(Source);
    Scaler := nil;
    Factory := nil;
  finally
    Source.Free;
  end;
end;

// draw nr. of columns on paintbox depending on Tag property
procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  i: integer;
  x,y: integer;
  w: integer;
  done: boolean;
  r: TRect;
  ar: single;
  {$IFDEF HIGHQ}
  bmp: TBitmap;
  {$ENDIF}
begin
  w := paintbox1.Width div paintbox1.Tag;
  i := 0;

  repeat
    x := i mod paintbox1.Tag;
    y := i div paintbox1.Tag;
    r := Rect(x * w, y * w, (x + 1) * w, (y + 1) * w);

    inflaterect(r, -5, -5);

    ar := jpgs[i].Width/jpgs[i].Height;

    r.Bottom := r.Top + Round((r.Right - r.Left)/ar);

    {$IFDEF HIGHQ}
    bmp := TBitmap.Create;
    try
      bmp.Width := jpgs[i].Width;
      bmp.Height := jpgs[i].Height;
      bmp.Canvas.Draw(0,0,jpgs[i]);
      ResizeBitmap(bmp, r.Right - r.Left, r.Bottom - r.Top);
      paintbox1.Canvas.Draw(r.Left, r.Top, bmp);
    finally
      bmp.Free;
    end;
    {$ELSE}
    paintbox1.Canvas.StretchDraw(r,jpgs[i]);
    {$ENDIF}

    inc(i);

    done := (y > paintbox1.Height) or (i > MAXIMG);

  until done;
end;

end.
