inherited fmPropertyPageColorFont: TfmPropertyPageColorFont
  Left = 321
  Top = 227
  HelpType = htKeyword
  HelpKeyword = 'ColoursFonts'
  Caption = 'fmPropertyPageColorFont'
  ExplicitLeft = 321
  ExplicitTop = 227
  DesignSize = (
    384
    414)
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel [0]
    Left = 11
    Top = 336
    Width = 38
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Preview'
    ExplicitTop = 303
  end
  object Bevel2: TBevel [1]
    Left = 60
    Top = 344
    Width = 317
    Height = 3
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsTopLine
    ExplicitTop = 311
    ExplicitWidth = 314
  end
  inherited Panel1: TPanel
    ExplicitWidth = 381
    inherited Bevel1: TBevel
      ExplicitWidth = 381
    end
    inherited stSectionDetails: TLabel
      ExplicitWidth = 369
    end
  end
  object rePreview: TRichEdit
    Left = 12
    Top = 351
    Width = 362
    Height = 54
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'TASK: Shoot yourself in the foot.'
      ''
      'C: You shoot yourself in the foot.'
      ''
      
        'C++: You accidentally create a dozen instances of yourself and s' +
        'hoot '
      'them all in the foot. Providing emergency medical assistance is '
      'impossible '
      
        'since you can'#39't tell which are bitwise copies and which are just' +
        ' pointing '
      'at '
      'others and saying,"That'#39's me, over there."'
      ''
      
        'FORTRAN: You shoot yourself in each toe until you run out of toe' +
        's, '
      'then '
      
        'you read in the next foot and repeat. If you run out of bullets,' +
        ' you '
      'continue with the attempts to shoot yourself anyway because you '
      'have '
      'no exception-handling capability.'
      ''
      'Pascal: The compiler won'#39't let you shoot yourself in the foot.'
      ''
      
        'Ada: After correctly packing your foot, you attempt to concurren' +
        'tly '
      'load '
      
        'the gun, pull the trigger, scream, and shoot yourself in the foo' +
        't. '
      'When '
      
        'you try, however, you discover you can'#39't because your foot is of' +
        ' the '
      'wrong type.'
      ''
      'COBOL: Using a COLT 45 HANDGUN, AIM gun at LEG.FOOT, THEN '
      'place '
      'ARM.HAND.FINGER on HANDGUN.TRIGGER and SQUEEZE. '
      'THEN return '
      'HANDGUN to HOLSTER. CHECK whether shoelace needs to be re-'
      'tied.'
      ''
      
        'LISP: You shoot yourself in the appendage which holds the gun wi' +
        'th '
      'which '
      
        'you shoot yourself in the appendage which holds the gun with whi' +
        'ch '
      'you '
      
        'shoot yourself in the appendage which holds the gun with which y' +
        'ou '
      'shoot '
      
        'yourself in the appendage which holds the gun with which you sho' +
        'ot '
      
        'yourself in the appendage which holds the gun with which you sho' +
        'ot '
      'yourself in the appendage which holds...'
      ''
      'FORTH: Foot in yourself shoot.'
      ''
      
        'Prolog: You tell your program that you want to be shot in the fo' +
        'ot. '
      'The '
      
        'program figures out how to do it, but the syntax doesn'#39't permit ' +
        'it to '
      'explain it to you.'
      ''
      'BASIC: Shoot yourself in the foot with a water pistol. On large '
      'systems, '
      'continue until entire lower body is waterlogged.'
      ''
      
        'Visual Basic: You'#39'll really only appear to have shot yourself in' +
        ' the foot, '
      'but '
      'you'#39'll have had so much fun doing it that you won'#39't care.'
      ''
      
        'HyperTalk: Put the first bullet of gun into foot left of leg of ' +
        'you. '
      'Answer '
      'the result.'
      ''
      
        'Motif: You spend days writing a UIL description of your foot, th' +
        'e '
      'bullet, '
      'its '
      
        'trajectory, and the intricate scrollwork onthe ivory handles of ' +
        'the '
      'gun. '
      
        'When you finally get around to pulling the trigger, the gun jams' +
        '.'
      ''
      
        'APL: You shoot yourself in the foot, then spend all day figuring' +
        ' out '
      'how '
      'to '
      'do it in fewer characters.'
      ''
      
        'SNOBOL: If you succeed, shoot yourself in the left foot. If you ' +
        'fail, '
      'shoot '
      'yourself in the right foot.'
      ''
      
        'Unix:% ls foot.c foot.h foot.o toe.c toe.o % rm * .o rm:.o no su' +
        'ch file '
      'or '
      'directory % ls %'
      ''
      'Concurrent Euclid: You shoot yourself in somebody else'#39's foot.'
      ''
      '370 JCL: You send your foot down to MIS and include a 400-page '
      
        'document explaining exactly how you want it to be shot. Three ye' +
        'ars '
      'later, your foot comes back deep-fried.'
      ''
      
        'Paradox: Not only can you shoot yourself in the foot, your users' +
        ' can, '
      'too.'
      ''
      
        'Access: You try to point the gun at your foot, but it shoots hol' +
        'es in all '
      'your Borland distribution diskettes instead.'
      ''
      
        'Revelation: You'#39're sure you'#39're going to be able to shoot yoursel' +
        'f in '
      'the '
      
        'foot, just as soon as you figure out what all these nifty little' +
        ' bullet-'
      'thingies '
      'are for.'
      ''
      
        'Assembler: You try to shoot yourself in the foot, only to discov' +
        'er you '
      
        'must first invent the gun, the bullet, the trigger, and your foo' +
        't.'
      ''
      
        'Modula2: After realizing that you can'#39't actually accomplish anyt' +
        'hing in '
      'this '
      'language, you shoot yourself in the head.'
      ''
      'Anon')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object lvFonts: TListView
    Left = 12
    Top = 60
    Width = 240
    Height = 189
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Font Name'
        Width = 130
      end
      item
        Alignment = taCenter
        Caption = 'Fixed'
        Width = 40
      end
      item
        Alignment = taCenter
        Caption = 'Truetype'
        Width = 60
      end>
    ColumnClick = False
    GridLines = True
    HideSelection = False
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnChange = lvFontsChange
    OnData = lvFontsData
    OnResize = lvFontsResize
  end
  object gbFontEffects: TGroupBox
    Left = 12
    Top = 262
    Width = 149
    Height = 66
    Anchors = [akLeft, akBottom]
    Caption = 'Font Effects'
    TabOrder = 3
    object cbBold: TCheckBox
      Left = 12
      Top = 18
      Width = 61
      Height = 17
      Caption = '&Bold'
      TabOrder = 0
      OnClick = cbBoldClick
    end
    object cbUnderline: TCheckBox
      Tag = 2
      Left = 77
      Top = 18
      Width = 68
      Height = 17
      Caption = '&Underline'
      TabOrder = 1
      OnClick = cbBoldClick
    end
    object cbStrikeout: TCheckBox
      Tag = 3
      Left = 77
      Top = 42
      Width = 68
      Height = 17
      Caption = '&Strikeout'
      TabOrder = 2
      OnClick = cbBoldClick
    end
    object cbItalic: TCheckBox
      Tag = 1
      Left = 12
      Top = 42
      Width = 61
      Height = 17
      Caption = '&Italic'
      TabOrder = 3
      OnClick = cbBoldClick
    end
  end
  object gbFontColors: TGroupBox
    Left = 168
    Top = 262
    Width = 212
    Height = 66
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Font &Colours:'
    TabOrder = 4
    ExplicitTop = 229
    ExplicitWidth = 209
    DesignSize = (
      212
      66)
    object lblFont: TLabel
      Left = 12
      Top = 43
      Width = 56
      Height = 13
      Caption = 'Foreground'
    end
    object lblBackground: TLabel
      Left = 12
      Top = 19
      Width = 56
      Height = 13
      Caption = '&Background'
    end
    object clrFont: TColorBox
      Left = 80
      Top = 38
      Width = 124
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = clrFontChange
      ExplicitWidth = 121
    end
    object clrBackground: TColorBox
      Left = 80
      Top = 14
      Width = 124
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = clrBackgroundChange
      ExplicitWidth = 121
    end
  end
  object lvSizes: TListView
    Left = 259
    Top = 60
    Width = 114
    Height = 189
    Anchors = [akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Font Size (pts)'
        Width = 90
      end>
    ColumnClick = False
    GridLines = True
    HideSelection = False
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 2
    ViewStyle = vsReport
    OnChange = lvSizesChange
    OnData = lvSizesData
  end
end
