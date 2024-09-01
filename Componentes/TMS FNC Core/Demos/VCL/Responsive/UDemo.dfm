object Form20: TForm20
  Left = 0
  Top = 0
  Caption = 'TMS FNC Responsive Manager Demo'
  ClientHeight = 627
  ClientWidth = 649
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 120
  TextHeight = 20
  object Memo1: TMemo
    Left = 0
    Top = 55
    Width = 649
    Height = 572
    Align = alClient
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -23
    Font.Name = 'Segoe UI'
    Font.Style = []
    Lines.Strings = (
      'What is Lorem Ipsum?'
      ''
      'Lorem Ipsum is simply dummy text of the printing and '
      'typesetting industry. Lorem Ipsum has been the industry'#39's '
      'standard dummy text ever since the 1500s, when an unknown '
      'printer took a galley of type and scrambled it to make a type '
      
        'specimen book. It has survived not only five centuries, but also' +
        ' '
      'the leap into electronic typesetting, remaining essentially '
      'unchanged. It was popularised in the 1960s with the release of '
      'Letraset sheets containing Lorem Ipsum passages, and more '
      'recently with desktop publishing software like Aldus '
      'PageMaker including versions of Lorem Ipsum.'
      ''
      'Where does it come from?'
      ''
      'Contrary to popular belief, Lorem Ipsum is not simply random '
      
        'text. It has roots in a piece of classical Latin literature from' +
        ' 45 '
      'BC, making it over 2000 years old. Richard McClintock, a Latin '
      'professor at Hampden-Sydney College in Virginia, looked up '
      'one of the more obscure Latin words, consectetur, from a '
      'Lorem Ipsum passage, and going through the cites of the word '
      'in classical literature, discovered the undoubtable source. '
      'Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of "de '
      'Finibus Bonorum et Malorum" (The Extremes of Good and Evil) '
      
        'by Cicero, written in 45 BC. This book is a treatise on the theo' +
        'ry '
      
        'of ethics, very popular during the Renaissance. The first line o' +
        'f '
      'Lorem Ipsum, "Lorem ipsum dolor sit amet..", comes from a '
      'line in section 1.10.32.')
    ParentFont = False
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 649
    Height = 55
    Align = alTop
    Color = 15790320
    TabOrder = 2
    object Button1: TButton
      Left = 10
      Top = 10
      Width = 150
      Height = 35
      Caption = 'Open'
      ImageIndex = 1
      ImageName = 'Open'
      Images = VirtualImageList1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 170
      Top = 10
      Width = 150
      Height = 35
      Caption = 'Save'
      ImageIndex = 3
      ImageName = 'Save'
      Images = VirtualImageList1
      TabOrder = 1
    end
    object Button3: TButton
      Left = 326
      Top = 10
      Width = 150
      Height = 35
      Caption = 'Copy'
      ImageIndex = 0
      ImageName = 'Copy'
      Images = VirtualImageList1
      TabOrder = 3
    end
    object Button4: TButton
      Left = 482
      Top = 10
      Width = 150
      Height = 35
      Caption = 'Paste'
      ImageIndex = 2
      ImageName = 'Paste'
      Images = VirtualImageList1
      TabOrder = 2
    end
  end
  object TMSFNCResponsiveManager1: TTMSFNCResponsiveManager
    Left = 170
    Top = 240
    Width = 26
    Height = 26
    Visible = True
    ActiveState = 0
    States = <
      item
        Name = 'State 1'
        Content = 
          '{"components":[{"Memo1":{"content":{"$type":"TMemo","Align":5,"A' +
          'lignment":0,"AlignWithMargins":false,"Anchors":15,"BevelEdges":1' +
          '5,"BevelInner":2,"BevelKind":0,"BevelOuter":1,"BiDiMode":0,"Bord' +
          'erStyle":1,"CanUndoSelText":false,"CharCase":0,"Color":"#FFFFFF"' +
          ',"Constraints":{"$type":"TSizeConstraints","MaxHeight":0,"MaxWid' +
          'th":0,"MinHeight":0,"MinWidth":0},"Ctl3D":true,"Cursor":0,"Custo' +
          'mHint":"","DoubleBuffered":false,"DragCursor":-12,"DragKind":0,"' +
          'DragMode":0,"EditMargins":{"$type":"TEditMargins","Auto":false,"' +
          'Left":0,"Right":0},"Enabled":true,"Font":{"$type":"TFont","Chars' +
          'et":1,"Color":"#000000","Height":-23,"Name":"Segoe UI","Orientat' +
          'ion":0,"Pitch":0,"Quality":0,"Size":14,"IsFMX":false,"Style":0},' +
          '"Height":572,"HelpContext":0,"HelpKeyword":"","HelpType":1,"Hide' +
          'Selection":true,"Hint":"","ImeMode":3,"ImeName":"","Left":0,"Lin' +
          'es":["What is Lorem Ipsum?","","Lorem Ipsum is simply dummy text' +
          ' of the printing and ","typesetting industry. Lorem Ipsum has be' +
          'en the industry'#39's ","standard dummy text ever since the 1500s, w' +
          'hen an unknown ","printer took a galley of type and scrambled it' +
          ' to make a type ","specimen book. It has survived not only five ' +
          'centuries, but also ","the leap into electronic typesetting, rem' +
          'aining essentially ","unchanged. It was popularised in the 1960s' +
          ' with the release of ","Letraset sheets containing Lorem Ipsum p' +
          'assages, and more ","recently with desktop publishing software l' +
          'ike Aldus ","PageMaker including versions of Lorem Ipsum.","","W' +
          'here does it come from?","","Contrary to popular belief, Lorem I' +
          'psum is not simply random ","text. It has roots in a piece of cl' +
          'assical Latin literature from 45 ","BC, making it over 2000 year' +
          's old. Richard McClintock, a Latin ","professor at Hampden-Sydne' +
          'y College in Virginia, looked up ","one of the more obscure Lati' +
          'n words, consectetur, from a ","Lorem Ipsum passage, and going t' +
          'hrough the cites of the word ","in classical literature, discove' +
          'red the undoubtable source. ","Lorem Ipsum comes from sections 1' +
          '.10.32 and 1.10.33 of \u0022de ","Finibus Bonorum et Malorum\u00' +
          '22 (The Extremes of Good and Evil) ","by Cicero, written in 45 B' +
          'C. This book is a treatise on the theory ","of ethics, very popu' +
          'lar during the Renaissance. The first line of ","Lorem Ipsum, \u' +
          '0022Lorem ipsum dolor sit amet..\u0022, comes from a ","line in ' +
          'section 1.10.32."],"Margins":{"$type":"TMargins","Bottom":3,"Lef' +
          't":3,"Right":3,"Top":3},"MaxLength":0,"Name":"Memo1","OEMConvert' +
          '":false,"ParentBiDiMode":true,"ParentColor":false,"ParentCtl3D":' +
          'true,"ParentCustomHint":true,"ParentDoubleBuffered":true,"Parent' +
          'Font":false,"ParentShowHint":true,"PopupMenu":"","ReadOnly":fals' +
          'e,"ScrollBars":0,"ShowHint":false,"StyleElements":7,"StyleName":' +
          '"","TabOrder":0,"TabStop":true,"Tag":0,"Top":55,"Touch":{"$type"' +
          ':"TTouchManager","GestureManager":"","InteractiveGestureOptions"' +
          ':30,"InteractiveGestures":18,"ParentTabletOptions":true,"TabletO' +
          'ptions":1},"Visible":true,"WantReturns":true,"WantTabs":false,"W' +
          'idth":649,"WordWrap":true}, "parent":"Form20"}},{"Panel1":{"cont' +
          'ent":{"$type":"TPanel","Align":1,"Alignment":2,"AlignWithMargins' +
          '":false,"Anchors":7,"AutoSize":false,"BevelEdges":15,"BevelInner' +
          '":0,"BevelKind":0,"BevelOuter":2,"BevelWidth":1,"BiDiMode":0,"Bo' +
          'rderStyle":0,"BorderWidth":0,"Caption":"","Color":"#F0F0F0","Con' +
          'straints":{"$type":"TSizeConstraints","MaxHeight":0,"MaxWidth":0' +
          ',"MinHeight":0,"MinWidth":0},"Ctl3D":true,"Cursor":0,"CustomHint' +
          '":"","DockSite":false,"DoubleBuffered":false,"DragCursor":-12,"D' +
          'ragKind":0,"DragMode":0,"Enabled":true,"Font":{"$type":"TFont","' +
          'Charset":1,"Color":"#000000","Height":-15,"Name":"Segoe UI","Ori' +
          'entation":0,"Pitch":0,"Quality":0,"Size":9,"IsFMX":false,"Style"' +
          ':0},"FullRepaint":true,"Height":55,"HelpContext":0,"HelpKeyword"' +
          ':"","HelpType":1,"Hint":"","Left":0,"Locked":false,"Margins":{"$' +
          'type":"TMargins","Bottom":3,"Left":3,"Right":3,"Top":3},"Name":"' +
          'Panel1","Padding":{"$type":"TPadding","Bottom":0,"Left":0,"Right' +
          '":0,"Top":0},"ParentBackground":true,"ParentBiDiMode":true,"Pare' +
          'ntColor":false,"ParentCtl3D":true,"ParentCustomHint":true,"Paren' +
          'tDoubleBuffered":true,"ParentFont":true,"ParentShowHint":true,"P' +
          'opupMenu":"","ShowCaption":true,"ShowHint":false,"StyleElements"' +
          ':7,"StyleName":"","TabOrder":2,"TabStop":false,"Tag":0,"Top":0,"' +
          'Touch":{"$type":"TTouchManager","GestureManager":"","Interactive' +
          'GestureOptions":16,"InteractiveGestures":16,"ParentTabletOptions' +
          '":true,"TabletOptions":1},"UseDockManager":true,"VerticalAlignme' +
          'nt":2,"Visible":true,"Width":649}, "parent":"Form20"}},{"Button1' +
          '":{"content":{"$type":"TButton","Action":"","Align":0,"AlignWith' +
          'Margins":false,"Anchors":3,"BiDiMode":0,"Cancel":false,"Caption"' +
          ':"Open","CommandLinkHint":"","Constraints":{"$type":"TSizeConstr' +
          'aints","MaxHeight":0,"MaxWidth":0,"MinHeight":0,"MinWidth":0},"C' +
          'ursor":0,"CustomHint":"","Default":false,"DisabledImageIndex":-1' +
          ',"DisabledImageName":"","DisabledImages":"","DoubleBuffered":fal' +
          'se,"DragCursor":-12,"DragKind":0,"DragMode":0,"DropDownMenu":"",' +
          '"ElevationRequired":false,"Enabled":true,"Font":{"$type":"TFont"' +
          ',"Charset":1,"Color":"#000000","Height":-15,"Name":"Segoe UI","O' +
          'rientation":0,"Pitch":0,"Quality":0,"Size":9,"IsFMX":false,"Styl' +
          'e":0},"Height":35,"HelpContext":0,"HelpKeyword":"","HelpType":1,' +
          '"Hint":"","HotImageIndex":-1,"HotImageName":"","ImageAlignment":' +
          '0,"ImageIndex":1,"ImageMargins":{"$type":"TImageMargins","Bottom' +
          '":0,"Left":0,"Right":0,"Top":0},"ImageName":"Open","Images":"Vir' +
          'tualImageList1","Left":10,"Margins":{"$type":"TMargins","Bottom"' +
          ':3,"Left":3,"Right":3,"Top":3},"ModalResult":0,"Name":"Button1",' +
          '"ParentBiDiMode":true,"ParentCustomHint":true,"ParentDoubleBuffe' +
          'red":true,"ParentFont":true,"ParentShowHint":true,"PopupMenu":""' +
          ',"PressedImageIndex":-1,"PressedImageName":"","SelectedImageInde' +
          'x":-1,"SelectedImageName":"","ShowHint":false,"Style":0,"StyleEl' +
          'ements":7,"StyleName":"","StylusHotImageIndex":-1,"StylusHotImag' +
          'eName":"","TabOrder":0,"TabStop":true,"Tag":0,"Top":10,"Visible"' +
          ':true,"Width":150,"WordWrap":false}, "parent":"Panel1"}},{"Butto' +
          'n2":{"content":{"$type":"TButton","Action":"","Align":0,"AlignWi' +
          'thMargins":false,"Anchors":3,"BiDiMode":0,"Cancel":false,"Captio' +
          'n":"Save","CommandLinkHint":"","Constraints":{"$type":"TSizeCons' +
          'traints","MaxHeight":0,"MaxWidth":0,"MinHeight":0,"MinWidth":0},' +
          '"Cursor":0,"CustomHint":"","Default":false,"DisabledImageIndex":' +
          '-1,"DisabledImageName":"","DisabledImages":"","DoubleBuffered":f' +
          'alse,"DragCursor":-12,"DragKind":0,"DragMode":0,"DropDownMenu":"' +
          '","ElevationRequired":false,"Enabled":true,"Font":{"$type":"TFon' +
          't","Charset":1,"Color":"#000000","Height":-15,"Name":"Segoe UI",' +
          '"Orientation":0,"Pitch":0,"Quality":0,"Size":9,"IsFMX":false,"St' +
          'yle":0},"Height":35,"HelpContext":0,"HelpKeyword":"","HelpType":' +
          '1,"Hint":"","HotImageIndex":-1,"HotImageName":"","ImageAlignment' +
          '":0,"ImageIndex":3,"ImageMargins":{"$type":"TImageMargins","Bott' +
          'om":0,"Left":0,"Right":0,"Top":0},"ImageName":"Save","Images":"V' +
          'irtualImageList1","Left":170,"Margins":{"$type":"TMargins","Bott' +
          'om":3,"Left":3,"Right":3,"Top":3},"ModalResult":0,"Name":"Button' +
          '2","ParentBiDiMode":true,"ParentCustomHint":true,"ParentDoubleBu' +
          'ffered":true,"ParentFont":true,"ParentShowHint":true,"PopupMenu"' +
          ':"","PressedImageIndex":-1,"PressedImageName":"","SelectedImageI' +
          'ndex":-1,"SelectedImageName":"","ShowHint":false,"Style":0,"Styl' +
          'eElements":7,"StyleName":"","StylusHotImageIndex":-1,"StylusHotI' +
          'mageName":"","TabOrder":1,"TabStop":true,"Tag":0,"Top":10,"Visib' +
          'le":true,"Width":150,"WordWrap":false}, "parent":"Panel1"}},{"Bu' +
          'tton3":{"content":{"$type":"TButton","Action":"","Align":0,"Alig' +
          'nWithMargins":false,"Anchors":3,"BiDiMode":0,"Cancel":false,"Cap' +
          'tion":"Copy","CommandLinkHint":"","Constraints":{"$type":"TSizeC' +
          'onstraints","MaxHeight":0,"MaxWidth":0,"MinHeight":0,"MinWidth":' +
          '0},"Cursor":0,"CustomHint":"","Default":false,"DisabledImageInde' +
          'x":-1,"DisabledImageName":"","DisabledImages":"","DoubleBuffered' +
          '":false,"DragCursor":-12,"DragKind":0,"DragMode":0,"DropDownMenu' +
          '":"","ElevationRequired":false,"Enabled":true,"Font":{"$type":"T' +
          'Font","Charset":1,"Color":"#000000","Height":-15,"Name":"Segoe U' +
          'I","Orientation":0,"Pitch":0,"Quality":0,"Size":9,"IsFMX":false,' +
          '"Style":0},"Height":35,"HelpContext":0,"HelpKeyword":"","HelpTyp' +
          'e":1,"Hint":"","HotImageIndex":-1,"HotImageName":"","ImageAlignm' +
          'ent":0,"ImageIndex":0,"ImageMargins":{"$type":"TImageMargins","B' +
          'ottom":0,"Left":0,"Right":0,"Top":0},"ImageName":"Copy","Images"' +
          ':"VirtualImageList1","Left":326,"Margins":{"$type":"TMargins","B' +
          'ottom":3,"Left":3,"Right":3,"Top":3},"ModalResult":0,"Name":"But' +
          'ton3","ParentBiDiMode":true,"ParentCustomHint":true,"ParentDoubl' +
          'eBuffered":true,"ParentFont":true,"ParentShowHint":true,"PopupMe' +
          'nu":"","PressedImageIndex":-1,"PressedImageName":"","SelectedIma' +
          'geIndex":-1,"SelectedImageName":"","ShowHint":false,"Style":0,"S' +
          'tyleElements":7,"StyleName":"","StylusHotImageIndex":-1,"StylusH' +
          'otImageName":"","TabOrder":3,"TabStop":true,"Tag":0,"Top":10,"Vi' +
          'sible":true,"Width":150,"WordWrap":false}, "parent":"Panel1"}},{' +
          '"Button4":{"content":{"$type":"TButton","Action":"","Align":0,"A' +
          'lignWithMargins":false,"Anchors":3,"BiDiMode":0,"Cancel":false,"' +
          'Caption":"Paste","CommandLinkHint":"","Constraints":{"$type":"TS' +
          'izeConstraints","MaxHeight":0,"MaxWidth":0,"MinHeight":0,"MinWid' +
          'th":0},"Cursor":0,"CustomHint":"","Default":false,"DisabledImage' +
          'Index":-1,"DisabledImageName":"","DisabledImages":"","DoubleBuff' +
          'ered":false,"DragCursor":-12,"DragKind":0,"DragMode":0,"DropDown' +
          'Menu":"","ElevationRequired":false,"Enabled":true,"Font":{"$type' +
          '":"TFont","Charset":1,"Color":"#000000","Height":-15,"Name":"Seg' +
          'oe UI","Orientation":0,"Pitch":0,"Quality":0,"Size":9,"IsFMX":fa' +
          'lse,"Style":0},"Height":35,"HelpContext":0,"HelpKeyword":"","Hel' +
          'pType":1,"Hint":"","HotImageIndex":-1,"HotImageName":"","ImageAl' +
          'ignment":0,"ImageIndex":2,"ImageMargins":{"$type":"TImageMargins' +
          '","Bottom":0,"Left":0,"Right":0,"Top":0},"ImageName":"Paste","Im' +
          'ages":"VirtualImageList1","Left":482,"Margins":{"$type":"TMargin' +
          's","Bottom":3,"Left":3,"Right":3,"Top":3},"ModalResult":0,"Name"' +
          ':"Button4","ParentBiDiMode":true,"ParentCustomHint":true,"Parent' +
          'DoubleBuffered":true,"ParentFont":true,"ParentShowHint":true,"Po' +
          'pupMenu":"","PressedImageIndex":-1,"PressedImageName":"","Select' +
          'edImageIndex":-1,"SelectedImageName":"","ShowHint":false,"Style"' +
          ':0,"StyleElements":7,"StyleName":"","StylusHotImageIndex":-1,"St' +
          'ylusHotImageName":"","TabOrder":2,"TabStop":true,"Tag":0,"Top":1' +
          '0,"Visible":true,"Width":150,"WordWrap":false}, "parent":"Panel1' +
          '"}}]}'
        Default = True
        Constraint.Width = 649.000000000000000000
        Constraint.Height = 627.000000000000000000
      end
      item
        Name = 'State 2'
        Content = 
          '{"components":[{"Memo1":{"content":{"$type":"TMemo","Align":5,"A' +
          'lignment":0,"AlignWithMargins":false,"Anchors":15,"BevelEdges":1' +
          '5,"BevelInner":2,"BevelKind":0,"BevelOuter":1,"BiDiMode":0,"Bord' +
          'erStyle":1,"CanUndoSelText":false,"CharCase":0,"Color":"#FFFFFF"' +
          ',"Constraints":{"$type":"TSizeConstraints","MaxHeight":0,"MaxWid' +
          'th":0,"MinHeight":0,"MinWidth":0},"Ctl3D":true,"Cursor":0,"Custo' +
          'mHint":"","DoubleBuffered":false,"DragCursor":-12,"DragKind":0,"' +
          'DragMode":0,"EditMargins":{"$type":"TEditMargins","Auto":false,"' +
          'Left":0,"Right":0},"Enabled":true,"Font":{"$type":"TFont","Chars' +
          'et":1,"Color":"#000000","Height":-18,"Name":"Segoe UI","Orientat' +
          'ion":0,"Pitch":0,"Quality":0,"Size":11,"IsFMX":false,"Style":0},' +
          '"Height":487,"HelpContext":0,"HelpKeyword":"","HelpType":1,"Hide' +
          'Selection":true,"Hint":"","ImeMode":3,"ImeName":"","Left":0,"Lin' +
          'es":["What is Lorem Ipsum?","","Lorem Ipsum is simply dummy text' +
          ' of the printing ","and typesetting industry. Lorem Ipsum has be' +
          'en the ","industry'#39's standard dummy text ever since the 1500s, "' +
          ',"when an unknown printer took a galley of type and ","scrambled' +
          ' it to make a type specimen book. It has ","survived not only fi' +
          've centuries, but also the leap into ","electronic typesetting, ' +
          'remaining essentially ","unchanged. It was popularised in the 19' +
          '60s with the ","release of Letraset sheets containing Lorem Ipsu' +
          'm ","passages, and more recently with desktop publishing ","soft' +
          'ware like Aldus PageMaker including versions of ","Lorem Ipsum."' +
          ',"","Where does it come from?","","Contrary to popular belief, L' +
          'orem Ipsum is not simply ","random text. It has roots in a piece' +
          ' of classical Latin ","literature from 45 BC, making it over 200' +
          '0 years old. ","Richard McClintock, a Latin professor at Hampden' +
          '-","Sydney College in Virginia, looked up one of the more ","obs' +
          'cure Latin words, consectetur, from a Lorem Ipsum ","passage, an' +
          'd going through the cites of the word in ","classical literature' +
          ', discovered the undoubtable source. ","Lorem Ipsum comes from s' +
          'ections 1.10.32 and ","1.10.33 of \u0022de Finibus Bonorum et Ma' +
          'lorum\u0022 (The ","Extremes of Good and Evil) by Cicero, writte' +
          'n in 45 BC. ","This book is a treatise on the theory of ethics, ' +
          'very ","popular during the Renaissance. The first line of Lorem ' +
          '","Ipsum, \u0022Lorem ipsum dolor sit amet..\u0022, comes from a' +
          ' ","line in section 1.10.32."],"Margins":{"$type":"TMargins","Bo' +
          'ttom":3,"Left":3,"Right":3,"Top":3},"MaxLength":0,"Name":"Memo1"' +
          ',"OEMConvert":false,"ParentBiDiMode":true,"ParentColor":false,"P' +
          'arentCtl3D":true,"ParentCustomHint":true,"ParentDoubleBuffered":' +
          'true,"ParentFont":false,"ParentShowHint":true,"PopupMenu":"","Re' +
          'adOnly":false,"ScrollBars":0,"ShowHint":false,"StyleElements":7,' +
          '"StyleName":"","TabOrder":0,"TabStop":true,"Tag":0,"Top":49,"Tou' +
          'ch":{"$type":"TTouchManager","GestureManager":"","InteractiveGes' +
          'tureOptions":30,"InteractiveGestures":18,"ParentTabletOptions":t' +
          'rue,"TabletOptions":1},"Visible":true,"WantReturns":true,"WantTa' +
          'bs":false,"Width":439,"WordWrap":true}, "parent":"Form20"}},{"Pa' +
          'nel1":{"content":{"$type":"TPanel","Align":1,"Alignment":2,"Alig' +
          'nWithMargins":false,"Anchors":7,"AutoSize":false,"BevelEdges":15' +
          ',"BevelInner":0,"BevelKind":0,"BevelOuter":2,"BevelWidth":1,"BiD' +
          'iMode":0,"BorderStyle":0,"BorderWidth":0,"Caption":"","Color":"#' +
          'F0F0F0","Constraints":{"$type":"TSizeConstraints","MaxHeight":0,' +
          '"MaxWidth":0,"MinHeight":0,"MinWidth":0},"Ctl3D":true,"Cursor":0' +
          ',"CustomHint":"","DockSite":false,"DoubleBuffered":false,"DragCu' +
          'rsor":-12,"DragKind":0,"DragMode":0,"Enabled":true,"Font":{"$typ' +
          'e":"TFont","Charset":1,"Color":"#000000","Height":-15,"Name":"Se' +
          'goe UI","Orientation":0,"Pitch":0,"Quality":0,"Size":9,"IsFMX":f' +
          'alse,"Style":0},"FullRepaint":true,"Height":49,"HelpContext":0,"' +
          'HelpKeyword":"","HelpType":1,"Hint":"","Left":0,"Locked":false,"' +
          'Margins":{"$type":"TMargins","Bottom":3,"Left":3,"Right":3,"Top"' +
          ':3},"Name":"Panel1","Padding":{"$type":"TPadding","Bottom":0,"Le' +
          'ft":0,"Right":0,"Top":0},"ParentBackground":true,"ParentBiDiMode' +
          '":true,"ParentColor":false,"ParentCtl3D":true,"ParentCustomHint"' +
          ':true,"ParentDoubleBuffered":true,"ParentFont":true,"ParentShowH' +
          'int":true,"PopupMenu":"","ShowCaption":true,"ShowHint":false,"St' +
          'yleElements":7,"StyleName":"","TabOrder":2,"TabStop":false,"Tag"' +
          ':0,"Top":0,"Touch":{"$type":"TTouchManager","GestureManager":"",' +
          '"InteractiveGestureOptions":16,"InteractiveGestures":16,"ParentT' +
          'abletOptions":true,"TabletOptions":1},"UseDockManager":true,"Ver' +
          'ticalAlignment":2,"Visible":true,"Width":439}, "parent":"Form20"' +
          '}},{"Button1":{"content":{"$type":"TButton","Action":"","Align":' +
          '0,"AlignWithMargins":false,"Anchors":3,"BiDiMode":0,"Cancel":fal' +
          'se,"Caption":"Open","CommandLinkHint":"","Constraints":{"$type":' +
          '"TSizeConstraints","MaxHeight":0,"MaxWidth":0,"MinHeight":0,"Min' +
          'Width":0},"Cursor":0,"CustomHint":"","Default":false,"DisabledIm' +
          'ageIndex":-1,"DisabledImageName":"","DisabledImages":"","DoubleB' +
          'uffered":false,"DragCursor":-12,"DragKind":0,"DragMode":0,"DropD' +
          'ownMenu":"","ElevationRequired":false,"Enabled":true,"Font":{"$t' +
          'ype":"TFont","Charset":1,"Color":"#000000","Height":-15,"Name":"' +
          'Segoe UI","Orientation":0,"Pitch":0,"Quality":0,"Size":9,"IsFMX"' +
          ':false,"Style":0},"Height":25,"HelpContext":0,"HelpKeyword":"","' +
          'HelpType":1,"Hint":"","HotImageIndex":-1,"HotImageName":"","Imag' +
          'eAlignment":0,"ImageIndex":1,"ImageMargins":{"$type":"TImageMarg' +
          'ins","Bottom":0,"Left":0,"Right":0,"Top":0},"ImageName":"Open","' +
          'Images":"VirtualImageList1","Left":10,"Margins":{"$type":"TMargi' +
          'ns","Bottom":3,"Left":3,"Right":3,"Top":3},"ModalResult":0,"Name' +
          '":"Button1","ParentBiDiMode":true,"ParentCustomHint":true,"Paren' +
          'tDoubleBuffered":true,"ParentFont":true,"ParentShowHint":true,"P' +
          'opupMenu":"","PressedImageIndex":-1,"PressedImageName":"","Selec' +
          'tedImageIndex":-1,"SelectedImageName":"","ShowHint":false,"Style' +
          '":0,"StyleElements":7,"StyleName":"","StylusHotImageIndex":-1,"S' +
          'tylusHotImageName":"","TabOrder":0,"TabStop":true,"Tag":0,"Top":' +
          '10,"Visible":true,"Width":100,"WordWrap":false}, "parent":"Panel' +
          '1"}},{"Button2":{"content":{"$type":"TButton","Action":"","Align' +
          '":0,"AlignWithMargins":false,"Anchors":3,"BiDiMode":0,"Cancel":f' +
          'alse,"Caption":"Save","CommandLinkHint":"","Constraints":{"$type' +
          '":"TSizeConstraints","MaxHeight":0,"MaxWidth":0,"MinHeight":0,"M' +
          'inWidth":0},"Cursor":0,"CustomHint":"","Default":false,"Disabled' +
          'ImageIndex":-1,"DisabledImageName":"","DisabledImages":"","Doubl' +
          'eBuffered":false,"DragCursor":-12,"DragKind":0,"DragMode":0,"Dro' +
          'pDownMenu":"","ElevationRequired":false,"Enabled":true,"Font":{"' +
          '$type":"TFont","Charset":1,"Color":"#000000","Height":-15,"Name"' +
          ':"Segoe UI","Orientation":0,"Pitch":0,"Quality":0,"Size":9,"IsFM' +
          'X":false,"Style":0},"Height":25,"HelpContext":0,"HelpKeyword":""' +
          ',"HelpType":1,"Hint":"","HotImageIndex":-1,"HotImageName":"","Im' +
          'ageAlignment":0,"ImageIndex":3,"ImageMargins":{"$type":"TImageMa' +
          'rgins","Bottom":0,"Left":0,"Right":0,"Top":0},"ImageName":"Save"' +
          ',"Images":"VirtualImageList1","Left":116,"Margins":{"$type":"TMa' +
          'rgins","Bottom":3,"Left":3,"Right":3,"Top":3},"ModalResult":0,"N' +
          'ame":"Button2","ParentBiDiMode":true,"ParentCustomHint":true,"Pa' +
          'rentDoubleBuffered":true,"ParentFont":true,"ParentShowHint":true' +
          ',"PopupMenu":"","PressedImageIndex":-1,"PressedImageName":"","Se' +
          'lectedImageIndex":-1,"SelectedImageName":"","ShowHint":false,"St' +
          'yle":0,"StyleElements":7,"StyleName":"","StylusHotImageIndex":-1' +
          ',"StylusHotImageName":"","TabOrder":1,"TabStop":true,"Tag":0,"To' +
          'p":10,"Visible":true,"Width":100,"WordWrap":false}, "parent":"Pa' +
          'nel1"}},{"Button3":{"content":{"$type":"TButton","Action":"","Al' +
          'ign":0,"AlignWithMargins":false,"Anchors":3,"BiDiMode":0,"Cancel' +
          '":false,"Caption":"Copy","CommandLinkHint":"","Constraints":{"$t' +
          'ype":"TSizeConstraints","MaxHeight":0,"MaxWidth":0,"MinHeight":0' +
          ',"MinWidth":0},"Cursor":0,"CustomHint":"","Default":false,"Disab' +
          'ledImageIndex":-1,"DisabledImageName":"","DisabledImages":"","Do' +
          'ubleBuffered":false,"DragCursor":-12,"DragKind":0,"DragMode":0,"' +
          'DropDownMenu":"","ElevationRequired":false,"Enabled":true,"Font"' +
          ':{"$type":"TFont","Charset":1,"Color":"#000000","Height":-15,"Na' +
          'me":"Segoe UI","Orientation":0,"Pitch":0,"Quality":0,"Size":9,"I' +
          'sFMX":false,"Style":0},"Height":25,"HelpContext":0,"HelpKeyword"' +
          ':"","HelpType":1,"Hint":"","HotImageIndex":-1,"HotImageName":"",' +
          '"ImageAlignment":0,"ImageIndex":0,"ImageMargins":{"$type":"TImag' +
          'eMargins","Bottom":0,"Left":0,"Right":0,"Top":0},"ImageName":"Co' +
          'py","Images":"VirtualImageList1","Left":222,"Margins":{"$type":"' +
          'TMargins","Bottom":3,"Left":3,"Right":3,"Top":3},"ModalResult":0' +
          ',"Name":"Button3","ParentBiDiMode":true,"ParentCustomHint":true,' +
          '"ParentDoubleBuffered":true,"ParentFont":true,"ParentShowHint":t' +
          'rue,"PopupMenu":"","PressedImageIndex":-1,"PressedImageName":"",' +
          '"SelectedImageIndex":-1,"SelectedImageName":"","ShowHint":false,' +
          '"Style":0,"StyleElements":7,"StyleName":"","StylusHotImageIndex"' +
          ':-1,"StylusHotImageName":"","TabOrder":3,"TabStop":true,"Tag":0,' +
          '"Top":10,"Visible":true,"Width":100,"WordWrap":false}, "parent":' +
          '"Panel1"}},{"Button4":{"content":{"$type":"TButton","Action":"",' +
          '"Align":0,"AlignWithMargins":false,"Anchors":3,"BiDiMode":0,"Can' +
          'cel":false,"Caption":"Paste","CommandLinkHint":"","Constraints":' +
          '{"$type":"TSizeConstraints","MaxHeight":0,"MaxWidth":0,"MinHeigh' +
          't":0,"MinWidth":0},"Cursor":0,"CustomHint":"","Default":false,"D' +
          'isabledImageIndex":-1,"DisabledImageName":"","DisabledImages":""' +
          ',"DoubleBuffered":false,"DragCursor":-12,"DragKind":0,"DragMode"' +
          ':0,"DropDownMenu":"","ElevationRequired":false,"Enabled":true,"F' +
          'ont":{"$type":"TFont","Charset":1,"Color":"#000000","Height":-15' +
          ',"Name":"Segoe UI","Orientation":0,"Pitch":0,"Quality":0,"Size":' +
          '9,"IsFMX":false,"Style":0},"Height":25,"HelpContext":0,"HelpKeyw' +
          'ord":"","HelpType":1,"Hint":"","HotImageIndex":-1,"HotImageName"' +
          ':"","ImageAlignment":0,"ImageIndex":2,"ImageMargins":{"$type":"T' +
          'ImageMargins","Bottom":0,"Left":0,"Right":0,"Top":0},"ImageName"' +
          ':"Paste","Images":"VirtualImageList1","Left":328,"Margins":{"$ty' +
          'pe":"TMargins","Bottom":3,"Left":3,"Right":3,"Top":3},"ModalResu' +
          'lt":0,"Name":"Button4","ParentBiDiMode":true,"ParentCustomHint":' +
          'true,"ParentDoubleBuffered":true,"ParentFont":true,"ParentShowHi' +
          'nt":true,"PopupMenu":"","PressedImageIndex":-1,"PressedImageName' +
          '":"","SelectedImageIndex":-1,"SelectedImageName":"","ShowHint":f' +
          'alse,"Style":0,"StyleElements":7,"StyleName":"","StylusHotImageI' +
          'ndex":-1,"StylusHotImageName":"","TabOrder":2,"TabStop":true,"Ta' +
          'g":0,"Top":10,"Visible":true,"Width":100,"WordWrap":false}, "par' +
          'ent":"Panel1"}}]}'
        Constraint.Width = 439.000000000000000000
        Constraint.Height = 536.000000000000000000
      end
      item
        Name = 'State 3'
        Content = 
          '{"components":[{"Memo1":{"content":{"$type":"TMemo","Align":5,"A' +
          'lignment":0,"AlignWithMargins":false,"Anchors":15,"BevelEdges":1' +
          '5,"BevelInner":2,"BevelKind":0,"BevelOuter":1,"BiDiMode":0,"Bord' +
          'erStyle":1,"CanUndoSelText":false,"CharCase":0,"Color":"#FFFFFF"' +
          ',"Constraints":{"$type":"TSizeConstraints","MaxHeight":0,"MaxWid' +
          'th":0,"MinHeight":0,"MinWidth":0},"Ctl3D":true,"Cursor":0,"Custo' +
          'mHint":"","DoubleBuffered":false,"DragCursor":-12,"DragKind":0,"' +
          'DragMode":0,"EditMargins":{"$type":"TEditMargins","Auto":false,"' +
          'Left":0,"Right":0},"Enabled":true,"Font":{"$type":"TFont","Chars' +
          'et":1,"Color":"#000000","Height":-18,"Name":"Segoe UI","Orientat' +
          'ion":0,"Pitch":0,"Quality":0,"Size":11,"IsFMX":false,"Style":0},' +
          '"Height":458,"HelpContext":0,"HelpKeyword":"","HelpType":1,"Hide' +
          'Selection":true,"Hint":"","ImeMode":3,"ImeName":"","Left":0,"Lin' +
          'es":["What is Lorem Ipsum?","","Lorem Ipsum is simply ","dummy t' +
          'ext of the printing ","and typesetting industry. ","Lorem Ipsum ' +
          'has been the ","industry'#39's standard dummy ","text ever since the' +
          ' 1500s, ","when an unknown printer ","took a galley of type and ' +
          '","scrambled it to make a type ","specimen book. It has ","survi' +
          'ved not only five ","centuries, but also the leap ","into ","ele' +
          'ctronic typesetting, ","remaining essentially ","unchanged. It w' +
          'as ","popularised in the 1960s ","with the ","release of Letrase' +
          't sheets ","containing Lorem Ipsum ","passages, and more ","rece' +
          'ntly with desktop ","publishing ","software like Aldus ","PageMa' +
          'ker including ","versions of ","Lorem Ipsum.","","Where does it ' +
          'come from?","","Contrary to popular belief, ","Lorem Ipsum is no' +
          't simply ","random text. It has roots in ","a piece of classical' +
          ' Latin ","literature from 45 BC, ","making it over 2000 years ",' +
          '"old. ","Richard McClintock, a Latin ","professor at Hampden-","' +
          'Sydney College in Virginia, ","looked up one of the more ","obsc' +
          'ure Latin words, ","consectetur, from a Lorem ","Ipsum ","passag' +
          'e, and going ","through the cites of the ","word in ","classical' +
          ' literature, ","discovered the undoubtable ","source. ","Lorem I' +
          'psum comes from ","sections 1.10.32 and ","1.10.33 of \u0022de F' +
          'inibus ","Bonorum et Malorum\u0022 (The ","Extremes of Good and ' +
          'Evil) ","by Cicero, written in 45 BC. ","This book is a treatise' +
          ' on ","the theory of ethics, very ","popular during the ","Renai' +
          'ssance. The first line of ","Lorem ","Ipsum, \u0022Lorem ipsum d' +
          'olor ","sit amet..\u0022, comes from a ","line in section 1.10.3' +
          '2."],"Margins":{"$type":"TMargins","Bottom":3,"Left":3,"Right":3' +
          ',"Top":3},"MaxLength":0,"Name":"Memo1","OEMConvert":false,"Paren' +
          'tBiDiMode":true,"ParentColor":false,"ParentCtl3D":true,"ParentCu' +
          'stomHint":true,"ParentDoubleBuffered":true,"ParentFont":false,"P' +
          'arentShowHint":true,"PopupMenu":"","ReadOnly":false,"ScrollBars"' +
          ':0,"ShowHint":false,"StyleElements":7,"StyleName":"","TabOrder":' +
          '0,"TabStop":true,"Tag":0,"Top":78,"Touch":{"$type":"TTouchManage' +
          'r","GestureManager":"","InteractiveGestureOptions":30,"Interacti' +
          'veGestures":18,"ParentTabletOptions":true,"TabletOptions":1},"Vi' +
          'sible":true,"WantReturns":true,"WantTabs":false,"Width":229,"Wor' +
          'dWrap":true}, "parent":"Form20"}},{"Panel1":{"content":{"$type":' +
          '"TPanel","Align":1,"Alignment":2,"AlignWithMargins":false,"Ancho' +
          'rs":7,"AutoSize":false,"BevelEdges":15,"BevelInner":0,"BevelKind' +
          '":0,"BevelOuter":2,"BevelWidth":1,"BiDiMode":0,"BorderStyle":0,"' +
          'BorderWidth":0,"Caption":"","Color":"#F0F0F0","Constraints":{"$t' +
          'ype":"TSizeConstraints","MaxHeight":0,"MaxWidth":0,"MinHeight":0' +
          ',"MinWidth":0},"Ctl3D":true,"Cursor":0,"CustomHint":"","DockSite' +
          '":false,"DoubleBuffered":false,"DragCursor":-12,"DragKind":0,"Dr' +
          'agMode":0,"Enabled":true,"Font":{"$type":"TFont","Charset":1,"Co' +
          'lor":"#000000","Height":-15,"Name":"Segoe UI","Orientation":0,"P' +
          'itch":0,"Quality":0,"Size":9,"IsFMX":false,"Style":0},"FullRepai' +
          'nt":true,"Height":78,"HelpContext":0,"HelpKeyword":"","HelpType"' +
          ':1,"Hint":"","Left":0,"Locked":false,"Margins":{"$type":"TMargin' +
          's","Bottom":3,"Left":3,"Right":3,"Top":3},"Name":"Panel1","Paddi' +
          'ng":{"$type":"TPadding","Bottom":0,"Left":0,"Right":0,"Top":0},"' +
          'ParentBackground":true,"ParentBiDiMode":true,"ParentColor":false' +
          ',"ParentCtl3D":true,"ParentCustomHint":true,"ParentDoubleBuffere' +
          'd":true,"ParentFont":true,"ParentShowHint":true,"PopupMenu":"","' +
          'ShowCaption":true,"ShowHint":false,"StyleElements":7,"StyleName"' +
          ':"","TabOrder":2,"TabStop":false,"Tag":0,"Top":0,"Touch":{"$type' +
          '":"TTouchManager","GestureManager":"","InteractiveGestureOptions' +
          '":16,"InteractiveGestures":16,"ParentTabletOptions":true,"Tablet' +
          'Options":1},"UseDockManager":true,"VerticalAlignment":2,"Visible' +
          '":true,"Width":229}, "parent":"Form20"}},{"Button1":{"content":{' +
          '"$type":"TButton","Action":"","Align":0,"AlignWithMargins":false' +
          ',"Anchors":3,"BiDiMode":0,"Cancel":false,"Caption":"Open","Comma' +
          'ndLinkHint":"","Constraints":{"$type":"TSizeConstraints","MaxHei' +
          'ght":0,"MaxWidth":0,"MinHeight":0,"MinWidth":0},"Cursor":0,"Cust' +
          'omHint":"","Default":false,"DisabledImageIndex":-1,"DisabledImag' +
          'eName":"","DisabledImages":"","DoubleBuffered":false,"DragCursor' +
          '":-12,"DragKind":0,"DragMode":0,"DropDownMenu":"","ElevationRequ' +
          'ired":false,"Enabled":true,"Font":{"$type":"TFont","Charset":1,"' +
          'Color":"#000000","Height":-15,"Name":"Segoe UI","Orientation":0,' +
          '"Pitch":0,"Quality":0,"Size":9,"IsFMX":false,"Style":0},"Height"' +
          ':25,"HelpContext":0,"HelpKeyword":"","HelpType":1,"Hint":"","Hot' +
          'ImageIndex":-1,"HotImageName":"","ImageAlignment":0,"ImageIndex"' +
          ':1,"ImageMargins":{"$type":"TImageMargins","Bottom":0,"Left":0,"' +
          'Right":0,"Top":0},"ImageName":"Open","Images":"VirtualImageList1' +
          '","Left":10,"Margins":{"$type":"TMargins","Bottom":3,"Left":3,"R' +
          'ight":3,"Top":3},"ModalResult":0,"Name":"Button1","ParentBiDiMod' +
          'e":true,"ParentCustomHint":true,"ParentDoubleBuffered":true,"Par' +
          'entFont":true,"ParentShowHint":true,"PopupMenu":"","PressedImage' +
          'Index":-1,"PressedImageName":"","SelectedImageIndex":-1,"Selecte' +
          'dImageName":"","ShowHint":false,"Style":0,"StyleElements":7,"Sty' +
          'leName":"","StylusHotImageIndex":-1,"StylusHotImageName":"","Tab' +
          'Order":0,"TabStop":true,"Tag":0,"Top":10,"Visible":true,"Width":' +
          '100,"WordWrap":false}, "parent":"Panel1"}},{"Button2":{"content"' +
          ':{"$type":"TButton","Action":"","Align":0,"AlignWithMargins":fal' +
          'se,"Anchors":3,"BiDiMode":0,"Cancel":false,"Caption":"Save","Com' +
          'mandLinkHint":"","Constraints":{"$type":"TSizeConstraints","MaxH' +
          'eight":0,"MaxWidth":0,"MinHeight":0,"MinWidth":0},"Cursor":0,"Cu' +
          'stomHint":"","Default":false,"DisabledImageIndex":-1,"DisabledIm' +
          'ageName":"","DisabledImages":"","DoubleBuffered":false,"DragCurs' +
          'or":-12,"DragKind":0,"DragMode":0,"DropDownMenu":"","ElevationRe' +
          'quired":false,"Enabled":true,"Font":{"$type":"TFont","Charset":1' +
          ',"Color":"#000000","Height":-15,"Name":"Segoe UI","Orientation":' +
          '0,"Pitch":0,"Quality":0,"Size":9,"IsFMX":false,"Style":0},"Heigh' +
          't":25,"HelpContext":0,"HelpKeyword":"","HelpType":1,"Hint":"","H' +
          'otImageIndex":-1,"HotImageName":"","ImageAlignment":0,"ImageInde' +
          'x":3,"ImageMargins":{"$type":"TImageMargins","Bottom":0,"Left":0' +
          ',"Right":0,"Top":0},"ImageName":"Save","Images":"VirtualImageLis' +
          't1","Left":116,"Margins":{"$type":"TMargins","Bottom":3,"Left":3' +
          ',"Right":3,"Top":3},"ModalResult":0,"Name":"Button2","ParentBiDi' +
          'Mode":true,"ParentCustomHint":true,"ParentDoubleBuffered":true,"' +
          'ParentFont":true,"ParentShowHint":true,"PopupMenu":"","PressedIm' +
          'ageIndex":-1,"PressedImageName":"","SelectedImageIndex":-1,"Sele' +
          'ctedImageName":"","ShowHint":false,"Style":0,"StyleElements":7,"' +
          'StyleName":"","StylusHotImageIndex":-1,"StylusHotImageName":"","' +
          'TabOrder":1,"TabStop":true,"Tag":0,"Top":10,"Visible":true,"Widt' +
          'h":100,"WordWrap":false}, "parent":"Panel1"}},{"Button3":{"conte' +
          'nt":{"$type":"TButton","Action":"","Align":0,"AlignWithMargins":' +
          'false,"Anchors":3,"BiDiMode":0,"Cancel":false,"Caption":"Copy","' +
          'CommandLinkHint":"","Constraints":{"$type":"TSizeConstraints","M' +
          'axHeight":0,"MaxWidth":0,"MinHeight":0,"MinWidth":0},"Cursor":0,' +
          '"CustomHint":"","Default":false,"DisabledImageIndex":-1,"Disable' +
          'dImageName":"","DisabledImages":"","DoubleBuffered":false,"DragC' +
          'ursor":-12,"DragKind":0,"DragMode":0,"DropDownMenu":"","Elevatio' +
          'nRequired":false,"Enabled":true,"Font":{"$type":"TFont","Charset' +
          '":1,"Color":"#000000","Height":-15,"Name":"Segoe UI","Orientatio' +
          'n":0,"Pitch":0,"Quality":0,"Size":9,"IsFMX":false,"Style":0},"He' +
          'ight":25,"HelpContext":0,"HelpKeyword":"","HelpType":1,"Hint":""' +
          ',"HotImageIndex":-1,"HotImageName":"","ImageAlignment":0,"ImageI' +
          'ndex":0,"ImageMargins":{"$type":"TImageMargins","Bottom":0,"Left' +
          '":0,"Right":0,"Top":0},"ImageName":"Copy","Images":"VirtualImage' +
          'List1","Left":10,"Margins":{"$type":"TMargins","Bottom":3,"Left"' +
          ':3,"Right":3,"Top":3},"ModalResult":0,"Name":"Button3","ParentBi' +
          'DiMode":true,"ParentCustomHint":true,"ParentDoubleBuffered":true' +
          ',"ParentFont":true,"ParentShowHint":true,"PopupMenu":"","Pressed' +
          'ImageIndex":-1,"PressedImageName":"","SelectedImageIndex":-1,"Se' +
          'lectedImageName":"","ShowHint":false,"Style":0,"StyleElements":7' +
          ',"StyleName":"","StylusHotImageIndex":-1,"StylusHotImageName":""' +
          ',"TabOrder":3,"TabStop":true,"Tag":0,"Top":41,"Visible":true,"Wi' +
          'dth":100,"WordWrap":false}, "parent":"Panel1"}},{"Button4":{"con' +
          'tent":{"$type":"TButton","Action":"","Align":0,"AlignWithMargins' +
          '":false,"Anchors":3,"BiDiMode":0,"Cancel":false,"Caption":"Paste' +
          '","CommandLinkHint":"","Constraints":{"$type":"TSizeConstraints"' +
          ',"MaxHeight":0,"MaxWidth":0,"MinHeight":0,"MinWidth":0},"Cursor"' +
          ':0,"CustomHint":"","Default":false,"DisabledImageIndex":-1,"Disa' +
          'bledImageName":"","DisabledImages":"","DoubleBuffered":false,"Dr' +
          'agCursor":-12,"DragKind":0,"DragMode":0,"DropDownMenu":"","Eleva' +
          'tionRequired":false,"Enabled":true,"Font":{"$type":"TFont","Char' +
          'set":1,"Color":"#000000","Height":-15,"Name":"Segoe UI","Orienta' +
          'tion":0,"Pitch":0,"Quality":0,"Size":9,"IsFMX":false,"Style":0},' +
          '"Height":25,"HelpContext":0,"HelpKeyword":"","HelpType":1,"Hint"' +
          ':"","HotImageIndex":-1,"HotImageName":"","ImageAlignment":0,"Ima' +
          'geIndex":2,"ImageMargins":{"$type":"TImageMargins","Bottom":0,"L' +
          'eft":0,"Right":0,"Top":0},"ImageName":"Paste","Images":"VirtualI' +
          'mageList1","Left":116,"Margins":{"$type":"TMargins","Bottom":3,"' +
          'Left":3,"Right":3,"Top":3},"ModalResult":0,"Name":"Button4","Par' +
          'entBiDiMode":true,"ParentCustomHint":true,"ParentDoubleBuffered"' +
          ':true,"ParentFont":true,"ParentShowHint":true,"PopupMenu":"","Pr' +
          'essedImageIndex":-1,"PressedImageName":"","SelectedImageIndex":-' +
          '1,"SelectedImageName":"","ShowHint":false,"Style":0,"StyleElemen' +
          'ts":7,"StyleName":"","StylusHotImageIndex":-1,"StylusHotImageNam' +
          'e":"","TabOrder":2,"TabStop":true,"Tag":0,"Top":41,"Visible":tru' +
          'e,"Width":100,"WordWrap":false}, "parent":"Panel1"}}]}'
        Constraint.Width = 229.000000000000000000
        Constraint.Height = 536.000000000000000000
      end>
    Control = Owner
    OnAfterLoadState = TMSFNCResponsiveManager1AfterLoadState
  end
  object VirtualImageList1: TVirtualImageList
    AutoFill = True
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Copy'
        Name = 'Copy'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Open'
        Name = 'Open'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Paste'
        Name = 'Paste'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Save'
        Name = 'Save'
      end>
    ImageCollection = ImageCollection1
    Left = 248
    Top = 248
  end
  object ImageCollection1: TImageCollection
    Images = <
      item
        Name = 'Copy'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000300000003008060000005702F9
              87000000097048597300000EC300000EC301C76FA86400000164494441546881
              ED993D8E833010851F514E43C52D922A54F4EC41360CCA41767BBADD2AB7E046
              D9662D210866FC331313F96B4046587ECC63FC1040E6B514F301227A04CCF74D
              441F01F73B73883C5F4B445F91E7B4725CBBD0759DD3447DDF9BD39688A05589
              D81530A85522BA80A669CCA98A88E802CAB25415B1FA0E182EB7BBF5FACFE769
              3166440CC30008BF139B029E2D908396884D01AE4CBAD11C11112216B2D002D0
              15C05DE0D6BE61A94C1052FB801AC1167AC6EFF5BCC858528874A1E2EAB5162F
              A277214037D146B590EF9E31C3A9DD8A6D64804EA24DB90BB172548A16425555
              18C711605442D442BED4750D002C11225D688E4F1CE18A50B1906F15392292B4
              904BA24DD642165A4C126D5216F249B4495AC885242CB485ED21AA0890ACE2AA
              009F2F28222AFE8F2169D489F7B390798A3191B450CA6994C5FB5948826C210B
              D942A10B08255B8843B69085DD5B68F702442D24F54F60CAEE2B9079357F1D07
              AEF14300B2790000000049454E44AE426082}
          end>
      end
      item
        Name = 'Open'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000300000003008060000005702F9
              87000000097048597300000EC300000EC301C76FA8640000034B494441546881
              ED98BF4EDB401CC73FA5AD10A056912CA44AADD488AD5BBA790C5B3A580D4FD0
              E4090A2F70907B01E813904E8C5065A837DCA9DEC888584837246A11B5554568
              810E3E47879D3FBE98F047CD674974BEF37DBF77BFBBDF9D61C284091326FCCF
              3CB8894E025F6C029594D5DBC0A265CB669ACA53A38A4A8BA178801CB09EB6F2
              23534126E8E2A7E70BCC2D2C0DAC7F76B2CFAF832D8062DA3EC66640173FF362
              9199E7C5E162669F19F7331603BAF8B98525A6E70BA9DA4D4DE78CFBBA6220F0
              C51AF09E300E3363227E54BA0646586C039979B13876F1A00C04BEC8A3C43F7E
              9A67F6E51B1E8E108FB7C1148065CB5654F0E455F5DE88871BC803E3E6DE1B30
              DE462F3A6D7E7FFBCCD9C9FE38F474097C7139A44AD3B2E56B63033F0FB638FF
              7D34A2AC6B25078621D4396EDE15F1001E1886D0D9F7BDEBEA7C07F80278F153
              67E08B225000DEA9DF7E7C0203037F7FB4F8F3A3652A344E1DA8E9DB761CCB96
              1EE1E86E2833AB40D1B265F7E81FF8A26CD972070C0C74B28D7E1BA8469D3AD2
              CD0365E02DC993A74738BA3B965DF2002FF0C5B25E217A0FA45C03179D369DE3
              54F78B5E4417941D47BA3947BAEBC021E1993F2E1E55B60E1C3AD25D538237A2
              878E7437F5CAA90C9C1E7D1D453868B72B47BA05600F581ED24667D591EE9E23
              5DFD7059D14D0C3570797E9A65F46B4A7C1ED805F23DEAB409C3C653FFE31480
              DD7E2686AE81CE7193CBF35323D58A9636F5DB248FE81E506B8892A7173AD22D
              A216AE565C50652B5A5905A80E3590217C3E28411592DB61B5214A7580C017B9
              E8B9654B4F19F2D40857B436CB8E743FC63B1918429DE326179D5EB39A0A4FFD
              BE8D95AF3444A91EF8221FF8621B38210CAFDDC01727D18ED310A5AAF68E88C4
              657FA0812C894B4B5065ADB8D510A58DC017D1822EC79AE58075753304A8C59E
              17E3FDF435701D892BB6F020CCC010C6F3A06BEBFBC01739154EF5417DF45D03
              191317000D516AD3FBE35977E4F50CDB871A03AEBA3D672063E2BA561AA2D422
              194A5D7ACE40869DA74B8AF3FC157AEC3AA948CC40C6C4654CE08B0A74779DBA
              69FB84810C896B5436B398D08FA897107E1DCBB0F767A16AD9B26EDA283103B7
              241EB49930213103778C15FD28DD8BBBFE59A53EACC25D3650B36C796BF13C61
              C2841BE21F5B4146C4C260C8180000000049454E44AE426082}
          end>
      end
      item
        Name = 'Paste'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000300000003008060000005702F9
              87000000097048597300000EC300000EC301C76FA86400000195494441546881
              ED97C171833010453F9914C00C0DA4048E1CE30A883B208D880F0DB804A78394
              601F39A68434C0D81D2487608F0362A448C29232BC9331B0BB7FBEB4AC80C849
              5C042199037807F024B9FD09604BF2C345AE310F8EE2EC212F1EC3FF7B477926
              B812905BDE37E6D1754092C9CDEF2FD7F1C7B872C01B7F72802401D4268966DC
              688698C64CBA50DF894947C98A3621990238D924937159727D27C6029BAC68A9
              7A5FB6840E98EF28F744CB69998054F620C9B355390B11FD268E5E806C13CBBA
              C5262BDAE35245F49D78C6CFDEFB4556B4CA5147D781DD90C4297D27D2BE132F
              0076A63174BF03398043DF09D33C8B11FD1E703E0BE9AC5B97AC0ECC613989BE
              917CD5793054072A925A87A0C51CB850D7FAC36BD334B7971549A89C08D2813C
              BF1EE0944E0429A02C4B6D11410A00F445042B00D01311B400402D62F12E64C2
              A81B8DA9005C3B53F00EA808CA01D53743E64CF40EAC027CB30AF0CD2AC037AB
              00DFAC027C231370BC771136C8046C119188C9349A15ED19C0C6432D46FCCB3D
              10158B1F6814C7436BA277207ABE018A8065C550CA8E2E0000000049454E44AE
              426082}
          end>
      end
      item
        Name = 'Save'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000300000003008060000005702F9
              87000000097048597300000EC300000EC301C76FA864000000EB494441546881
              EDD9211282401406E0A7E3112560D24BC819F004341D8304CC348B1582C146B2
              7004661C833603C2AEC8B2BFCBFC5F647798F7CFE3057645887A99D41F847EF2
              4414D245107BEFBAA7C8424C98E936AC0F731B75286D16C7D635E73BC000680C
              80C600680C80C600680C80C600680C8036FE00455EDAA8A3D5F57C53AE6B03A4
              51A67DC9508ABC94D3FEA2DCA3FDA9BF570F49A34CD22833569849E39F817FC7
              0068DA21AE1BFAA44E750AD7C4F90E30005AE71968A2FA6E879E19E73BC00068
              0C8036CA003BDB45F4F1714F2C2212FAC9564496764BF99EF69E3888BD9538D6
              09A21FBD00190829DB0DDCFDFE0000000049454E44AE426082}
          end>
      end>
    Left = 348
    Top = 248
  end
  object VirtualImageList2: TVirtualImageList
    AutoFill = True
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Copy'
        Name = 'Copy'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Open'
        Name = 'Open'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Paste'
        Name = 'Paste'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Save'
        Name = 'Save'
      end>
    ImageCollection = ImageCollection1
    Width = 32
    Height = 32
    Left = 248
    Top = 318
  end
end
