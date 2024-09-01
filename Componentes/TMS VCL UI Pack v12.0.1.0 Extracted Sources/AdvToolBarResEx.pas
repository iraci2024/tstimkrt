{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2016                                      }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit AdvToolBarResEx;

{$I TMSDEFS.INC}

interface

{$IFDEF WEBLIB}
const
  AdvTOOLBAREXPAND = 'data:image/PNG;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAYAAADEUlfTAAAABGdBTUEAALGPC/xhBQAAAAlwSFlzAAAOvwAADr8B'+
                        'OAVTJAAAABh0RVh0U29mdHdhcmUAcGFpbnQubmV0IDQuMC4zjOaXUAAAACVJREFUGFdj+P//P07MEBYW9h8Xxi8J'+
                        'AjglYACnBAzglMAEDAwARZ1DA4NRF38AAAAASUVORK5CYII=';
  AdvTOOLBAREXPANDLARGE = AdvTOOLBAREXPAND;

  AdvTOOLBAREXPAND2 = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAYAAADEUlfTAAAABGdBTUEAALGPC/xhBQAAAAlwSFlzAAAOvgAADr4'+
                         'B6kKxwAAAABl0RVh0U29mdHdhcmUAcGFpbnQubmV0IDQuMC4xNkRpr/UAAAA3SURBVBhXY3j69KktLgyS/I8LE5RsQxeE4jaQpCKIgSQIlgBi'+
                         'RQYQADGgAqgSMABVkI2QYGAAANnukE/t/B6ZAAAAAElFTkSuQmCC';
  AdvTOOLBAREXPAND2LARGE = AdvTOOLBAREXPAND;

  AdvTOOLBAROPTIONSMENU = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAJCAYAAAD+WDajAAAABGdBTUEAALGPC/xhBQAAAAlwSFlzAAAOvwA'+
                             'ADr8BOAVTJAAAABh0RVh0U29mdHdhcmUAcGFpbnQubmV0IDQuMC4zjOaXUAAAACpJREFUKFNjwAvCwsL+48IM///jxvh1YhOEYZz2giVg'+
                             'AKcEDOCUwAQMDAAqj1EKDJG5XAAAAABJRU5ErkJggg==';
  AdvTOOLBAROPTIONSMENULARGE = AdvTOOLBAROPTIONSMENU;

  AdvTOOLBARQUICKMENU = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAYAAADEUlfTAAAABGdBTUEAALGPC/xhBQAAAAlwSFlzAAAOwgAADs'+
                           'IBFShKgAAAABl0RVh0U29mdHdhcmUAcGFpbnQubmV0IDQuMC4xNkRpr/UAAAAlSURBVBhXYwCC/9gwEAApKAMbJl4SnY0igCQIw3gkcUowMP'+
                           'wHALBNZJw15C+yAAAAAElFTkSuQmCC';
  AdvTOOLBARQUICKMENU2 = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAYAAADEUlfTAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAA'+
                            'AAJcEhZcwAADsMAAA7DAcdvqGQAAAAZdEVYdFNvZnR3YXJlAHBhaW50Lm5ldCA0LjAuMTZEaa/1AAAAL0lEQVQYV2P4jxswwCRBNAYGESCA'+
                            'IQHCIAIEYAIgAGejC8D4YEBQEqvE/////wMA7H6pV7j5gXMAAAAASUVORK5CYII=';
{$ENDIF}

{$R AdvToolBarEx.res}

implementation

{$IFDEF WEBLIB}
uses
  AdvTypes;

initialization
begin
  TAdvBitmap.CreateFromResource(AdvTOOLBAREXPAND);
  TAdvBitmap.CreateFromResource(AdvTOOLBAREXPAND2);
  TAdvBitmap.CreateFromResource(AdvTOOLBAROPTIONSMENU);
  TAdvBitmap.CreateFromResource(AdvTOOLBARQUICKMENU);
  TAdvBitmap.CreateFromResource(AdvTOOLBARQUICKMENU2);
end;
{$ENDIF}

end.
