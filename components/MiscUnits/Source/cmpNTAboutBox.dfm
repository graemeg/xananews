object fmNTAboutBox: TfmNTAboutBox
  Left = 192
  Top = 224
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 287
  ClientWidth = 371
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 371
    Height = 287
    ActivePage = TabSheet3
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object icoProduct: TImage
        Left = 8
        Top = 8
        Width = 65
        Height = 65
        AutoSize = True
        Center = True
        Stretch = True
        Transparent = True
      end
      object stProduct: TLabel
        Left = 80
        Top = 16
        Width = 281
        Height = 13
        AutoSize = False
        Caption = 'stProduct'
        ShowAccelChar = False
      end
      object stVersion: TLabel
        Left = 80
        Top = 32
        Width = 281
        Height = 13
        AutoSize = False
        Caption = 'stVersion'
        ShowAccelChar = False
      end
      object stCopyright: TLabel
        Left = 80
        Top = 48
        Width = 246
        Height = 13
        Caption = 'Copyright '#169' Colin Wilson 2005.  All Rights Reserved'
        ShowAccelChar = False
      end
      object lblSupport: TLabel
        Left = 80
        Top = 64
        Width = 37
        Height = 13
        Caption = 'Support'
        Visible = False
      end
      object Label1: TLabel
        Left = 80
        Top = 96
        Width = 281
        Height = 13
        AutoSize = False
        Caption = 'This product is licensed to:'
      end
      object stLicense1: TLabel
        Left = 80
        Top = 112
        Width = 281
        Height = 13
        AutoSize = False
        Caption = 'stLicense1'
        ShowAccelChar = False
      end
      object stLicense2: TLabel
        Left = 80
        Top = 128
        Width = 51
        Height = 13
        Caption = 'stLicense2'
        ShowAccelChar = False
      end
      object Bevel1: TBevel
        Left = 80
        Top = 152
        Width = 281
        Height = 2
        Shape = bsBottomLine
      end
      object stMemAvail: TLabel
        Left = 80
        Top = 160
        Width = 281
        Height = 13
        AutoSize = False
        Caption = 'Physical Memory Available to Windows:'
      end
      object icoProduct1: TImage
        Left = 8
        Top = 184
        Width = 65
        Height = 65
        AutoSize = True
        Center = True
        Transparent = True
      end
      object hlbSupport: THyperlinkButton
        Left = 136
        Top = 62
        Width = 217
        Height = 16
        Cursor = crHandPoint
        Caption = 'http://www.wilsonc.demon.co.uk/delphi.htm'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ImageIndex = 0
        ParentFont = False
        SelectedFontColor = clBlue
        SelectedFontStyles = [fsUnderline]
        Visible = False
        Link = 'http://www.wilsonc.demon.co.uk/delphi.htm'
        AutoLink = True
        InPlace = False
      end
      object OKBtn: TButton
        Left = 279
        Top = 220
        Width = 75
        Height = 25
        Caption = 'OK'
        ModalResult = 1
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Thank You!'
      ImageIndex = 1
      object stThankYou: TLabel
        Left = 16
        Top = 16
        Width = 337
        Height = 33
        AutoSize = False
        WordWrap = True
      end
      object lbDonations: TListBox
        Left = 16
        Top = 56
        Width = 337
        Height = 185
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'SSL Support'
      ImageIndex = 2
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 363
        Height = 259
        Align = alClient
        Lines.Strings = (
          'SSL support provided by OpenStrSecII, which is:'
          ''
          '  Copyright (c) 2004, Henrick Wibell Hellstr'#246'm, StreamSec'
          '  All rights reserved.'
          ''
          
            '  Redistribution and use in source and binary forms, with or wit' +
            'hout'
          
            '  modification, are permitted provided that the following condit' +
            'ions are met:'
          ''
          
            '    * Redistributions of source code must retain the above copyr' +
            'ight notice,'
          '      this list of conditions and the following disclaimer.'
          
            '    * Redistributions in binary form must reproduce the above co' +
            'pyright notice,'
          
            '      this list of conditions and the following disclaimer in th' +
            'e documentation'
          '      and/or other materials provided with the distribution.'
          
            '    * Neither the name of StreamSec nor the names of its contrib' +
            'utors may be'
          
            '      used to endorse or promote products derived from this soft' +
            'ware without'
          '      specific prior written permission.'
          ''
          
            '  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIB' +
            'UTORS "AS IS"'
          
            '  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMI' +
            'TED TO, THE'
          
            '  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTIC' +
            'ULAR PURPOSE'
          
            '  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTR' +
            'IBUTORS BE'
          
            '  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLAR' +
            'Y, OR'
          
            '  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREM' +
            'ENT OF'
          
            '  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; O' +
            'R BUSINESS'
          
            '  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, W' +
            'HETHER IN'
          
            '  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR O' +
            'THERWISE)'
          
            '  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF AD' +
            'VISED OF THE'
          '  POSSIBILITY OF SUCH DAMAGE.')
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
end
