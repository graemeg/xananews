inherited fmPropertyPageKeywords: TfmPropertyPageKeywords
  Left = 319
  Top = 248
  Caption = 'Keywords'
  ClientWidth = 499
  ExplicitLeft = 319
  ExplicitTop = 248
  ExplicitWidth = 515
  PixelsPerInch = 96
  TextHeight = 13
  object Label18: TLabel [0]
    Left = 12
    Top = 280
    Width = 63
    Height = 13
    Caption = 'Key Phrase 8'
  end
  object Label17: TLabel [1]
    Left = 12
    Top = 256
    Width = 63
    Height = 13
    Caption = 'Key Phrase 7'
  end
  object Label16: TLabel [2]
    Left = 12
    Top = 232
    Width = 63
    Height = 13
    Caption = 'Key Phrase 6'
  end
  object Label15: TLabel [3]
    Left = 12
    Top = 208
    Width = 63
    Height = 13
    Caption = 'Key Phrase 5'
  end
  object Label12: TLabel [4]
    Left = 12
    Top = 184
    Width = 63
    Height = 13
    Caption = 'Key Phrase 4'
  end
  object Label13: TLabel [5]
    Left = 12
    Top = 160
    Width = 63
    Height = 13
    Caption = 'Key Phrase 3'
  end
  object Label11: TLabel [6]
    Left = 12
    Top = 136
    Width = 63
    Height = 13
    Caption = 'Key Phrase 2'
  end
  object Label10: TLabel [7]
    Left = 12
    Top = 112
    Width = 63
    Height = 13
    Caption = 'Key Phrase 1'
  end
  object Label28: TLabel [8]
    Left = 12
    Top = 56
    Width = 478
    Height = 33
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Keyphrase scanning is turned off by default.  Once you'#39've set up' +
      ' keyphrases, go to '#39'Account Properties'#39', and enable it there for' +
      ' each account you want to scan.'
    WordWrap = True
  end
  inherited Panel1: TPanel
    Width = 499
    ExplicitWidth = 499
    inherited Bevel1: TBevel
      Width = 499
      ExplicitWidth = 499
    end
    inherited stSectionDetails: TLabel
      Width = 487
      Caption = 
        'XanaNews will automatically search messages for keyphrases, and ' +
        'put an appropriately coloured marker in the message tree next to' +
        ' the ones it finds.'
      ExplicitWidth = 487
    end
  end
  object edKeyPhrase0: TEdit
    Tag = 1
    Left = 88
    Top = 108
    Width = 371
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = EditChange
  end
  object edKeyPhrase1: TEdit
    Tag = 2
    Left = 88
    Top = 132
    Width = 371
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = EditChange
  end
  object edKeyPhrase2: TEdit
    Tag = 3
    Left = 88
    Top = 156
    Width = 371
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    OnChange = EditChange
  end
  object edKeyPhrase3: TEdit
    Tag = 4
    Left = 88
    Top = 180
    Width = 371
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    OnChange = EditChange
  end
  object edKeyPhrase4: TEdit
    Tag = 5
    Left = 88
    Top = 204
    Width = 371
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    OnChange = EditChange
  end
  object edKeyPhrase5: TEdit
    Tag = 6
    Left = 88
    Top = 228
    Width = 371
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
    OnChange = EditChange
  end
  object edKeyPhrase6: TEdit
    Tag = 7
    Left = 88
    Top = 252
    Width = 371
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
    OnChange = EditChange
  end
  object edKeyPhrase7: TEdit
    Tag = 8
    Left = 88
    Top = 276
    Width = 371
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
    OnChange = EditChange
  end
  object pnlKeyPhrase7: TPanel
    Tag = 8
    Left = 473
    Top = 278
    Width = 17
    Height = 17
    Anchors = [akTop, akRight]
    BevelInner = bvRaised
    Caption = 'X'
    Color = clMaroon
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
    TabStop = True
    OnClick = KeywordColorButtonClick
  end
  object pnlKeyPhrase6: TPanel
    Tag = 7
    Left = 473
    Top = 254
    Width = 17
    Height = 17
    Anchors = [akTop, akRight]
    BevelInner = bvRaised
    Caption = 'X'
    Color = clOlive
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clOlive
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 10
    TabStop = True
    OnClick = KeywordColorButtonClick
  end
  object pnlKeyPhrase4: TPanel
    Tag = 5
    Left = 473
    Top = 206
    Width = 17
    Height = 17
    Anchors = [akTop, akRight]
    BevelInner = bvRaised
    Caption = 'X'
    Color = clFuchsia
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clFuchsia
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 11
    TabStop = True
    OnClick = KeywordColorButtonClick
  end
  object pnlKeyPhrase3: TPanel
    Tag = 4
    Left = 473
    Top = 182
    Width = 17
    Height = 17
    Anchors = [akTop, akRight]
    BevelInner = bvRaised
    Caption = 'X'
    Color = clBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 12
    TabStop = True
    OnClick = KeywordColorButtonClick
  end
  object pnlKeyPhrase2: TPanel
    Tag = 3
    Left = 473
    Top = 158
    Width = 17
    Height = 17
    Anchors = [akTop, akRight]
    BevelInner = bvRaised
    Caption = 'X'
    Color = clYellow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clYellow
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 13
    TabStop = True
    OnClick = KeywordColorButtonClick
  end
  object pnlKeyPhrase1: TPanel
    Tag = 2
    Left = 473
    Top = 134
    Width = 17
    Height = 17
    Anchors = [akTop, akRight]
    BevelInner = bvRaised
    Caption = 'X'
    Color = clLime
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 14
    TabStop = True
    OnClick = KeywordColorButtonClick
  end
  object pnlKeyPhrase0: TPanel
    Tag = 1
    Left = 473
    Top = 110
    Width = 17
    Height = 17
    Anchors = [akTop, akRight]
    BevelInner = bvRaised
    Caption = 'X'
    Color = clRed
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 15
    TabStop = True
    OnClick = KeywordColorButtonClick
  end
  object pnlKeyPhrase5: TPanel
    Tag = 6
    Left = 473
    Top = 230
    Width = 17
    Height = 17
    Anchors = [akTop, akRight]
    BevelInner = bvRaised
    Caption = 'X'
    Color = clAqua
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 16
    TabStop = True
    OnClick = KeywordColorButtonClick
  end
  object ColorDialog1: TColorDialog
    Left = 436
    Top = 208
  end
end
