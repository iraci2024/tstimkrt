{*************************************************************************}
{ TMS VCL Core utilities                                                  }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2021                                              }
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

unit AdvCore;

interface

uses
  Classes, Windows, Messages, Graphics, JPEG, Controls, Types, Forms;

procedure CaptureControl(AControl: TControl; ABitmap: TBitmap);
procedure CaptureControlToJpeg(AControl: TControl; AFileName: string; AQuality: integer = 70);

implementation

uses
  Winapi.DwmApi, SysUtils;

procedure CaptureControl(AControl: TControl; ABitmap: TBitmap);
var
  LDesktopDC: HDC;
  LRect: TRect;
  LDWMRect: TRect;
  LPt: TPoint;
begin
  LPt := AControl.ClientToScreen(Point(0,0));

  LRect := Rect(LPt.X, LPt.Y, LPt.X + AControl.Width, LPt.Y + AControl.Height);

  if (AControl is TWinControl) and (AControl as TWinControl).HandleAllocated then
  begin
    GetWindowRect((AControl as TWinControl).Handle, LRect);
  end;

  if (AControl is TCustomForm) and (AControl as TCustomForm).HandleAllocated then
  begin
    GetWindowRect((AControl as TCustomForm).Handle, LRect);

    if (Win32MajorVersion >= 6) and DwmCompositionEnabled then
    begin
      if (DwmGetWindowAttribute((AControl as TCustomForm).Handle, DWMWA_EXTENDED_FRAME_BOUNDS, @LDWMRect, SizeOf(LDWMRect)) = S_OK) then
      begin
        LRect := LDWMRect;
      end;
    end;
  end;

  LDesktopDC := GetWindowDC(GetDesktopWindow);
  try
    ABitmap.PixelFormat := pf24bit;
    ABitmap.Height := LRect.Bottom - LRect.Top;
    ABitmap.Width := LRect.Right - LRect.Left;

    BitBlt(ABitmap.Canvas.Handle, 0, 0, ABitmap.Width, ABitmap.Height, LDesktopDC, LRect.Left, LRect.Top, SRCCOPY);
  finally
    ReleaseDC(GetDesktopWindow, LDesktopDC);
  end;
end;

procedure CaptureControlToJpeg(AControl: TControl; AFileName: string; AQuality: integer = 70);
var
  LJpeg: TJpegImage;
  LBmp: TBitmap;
begin
  LBmp := TBitmap.Create;
  try
    CaptureControl(AControl, LBmp);
    LJpeg := TJpegImage.Create;
    try
      LJpeg.Assign(LBmp);
      LJpeg.CompressionQuality := AQuality;
      LJpeg.SaveToFile(AFileName);
    finally
      LJpeg.Free;
    end;
  finally
    LBmp.Free;
  end;
end;

end.
