object fmNTAboutBox: TfmNTAboutBox
  Left = 680
  Top = 172
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 328
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 460
    Height = 328
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      DesignSize = (
        452
        300)
      object icoProduct: TImage
        Left = 8
        Top = 8
        Width = 65
        Height = 65
        Center = True
        Transparent = True
      end
      object stProduct: TLabel
        Left = 92
        Top = 14
        Width = 281
        Height = 13
        AutoSize = False
        Caption = 'stProduct'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ShowAccelChar = False
      end
      object stVersion: TLabel
        Left = 92
        Top = 32
        Width = 281
        Height = 13
        AutoSize = False
        Caption = 'stVersion'
        ShowAccelChar = False
      end
      object stCopyright: TLabel
        Left = 92
        Top = 50
        Width = 250
        Height = 13
        Caption = 'Copyright '#169' Colin Wilson 2005.  All Rights Reserved'
        ShowAccelChar = False
      end
      object lblSupport: TLabel
        Left = 92
        Top = 68
        Width = 36
        Height = 13
        Caption = 'Original'
        Visible = False
      end
      object Label1: TLabel
        Left = 92
        Top = 114
        Width = 281
        Height = 13
        AutoSize = False
        Caption = 'This product is licensed to:'
      end
      object stLicense1: TLabel
        Left = 92
        Top = 130
        Width = 281
        Height = 13
        AutoSize = False
        Caption = 'stLicense1'
        ShowAccelChar = False
      end
      object stLicense2: TLabel
        Left = 92
        Top = 146
        Width = 50
        Height = 13
        Caption = 'stLicense2'
        ShowAccelChar = False
      end
      object Bevel1: TBevel
        Left = 92
        Top = 168
        Width = 351
        Height = 4
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object stMemAvail: TLabel
        Left = 92
        Top = 176
        Width = 281
        Height = 13
        AutoSize = False
        Caption = 'Physical Memory Available to Windows:'
      end
      object icoProduct1: TImage
        Left = 8
        Top = 225
        Width = 65
        Height = 65
        Anchors = [akLeft, akBottom]
        AutoSize = True
        Center = True
        Transparent = True
        ExplicitTop = 184
      end
      object hlbSupport: THyperlinkButton
        Left = 142
        Top = 67
        Width = 299
        Height = 16
        Cursor = crHandPoint
        Caption = 'http://www.wilsonc.demon.co.uk/delphi_2006.htm'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ImageIndex = 0
        ParentFont = False
        SelectedFontColor = clBlue
        SelectedFontStyles = [fsUnderline]
        Transparent = True
        Visible = False
        Link = 'http://www.wilsonc.demon.co.uk/delphi_2006.htm'
        AutoLink = True
        InPlace = False
      end
      object lblExtra: TLabel
        Left = 92
        Top = 86
        Width = 30
        Height = 13
        Caption = 'Latest'
        Visible = False
      end
      object hlbExtra: THyperlinkButton
        Left = 142
        Top = 85
        Width = 299
        Height = 16
        Cursor = crHandPoint
        Caption = 'http://xananews.techtips.com.br/'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ImageIndex = 0
        ParentFont = False
        SelectedFontColor = clBlue
        SelectedFontStyles = [fsUnderline]
        Transparent = True
        Visible = False
        Link = 'http://xananews.techtips.com.br/'
        AutoLink = True
        InPlace = False
      end
      object OKBtn: TButton
        Left = 368
        Top = 267
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'OK'
        ModalResult = 1
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Thank You!'
      ImageIndex = 1
      DesignSize = (
        452
        300)
      object stThankYou: TLabel
        Left = 16
        Top = 16
        Width = 425
        Height = 33
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        WordWrap = True
      end
      object lbDonations: TListBox
        Left = 16
        Top = 56
        Width = 426
        Height = 226
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
      end
    end
  end
end
