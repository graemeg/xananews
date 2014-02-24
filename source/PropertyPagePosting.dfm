inherited fmPropertyPagePosting: TfmPropertyPagePosting
  Left = 502
  Top = 131
  HelpType = htKeyword
  HelpKeyword = 'PostingSettings'
  Caption = 'Posting Settings'
  ExplicitLeft = 502
  ExplicitTop = 131
  ExplicitWidth = 471
  ExplicitHeight = 450
  PixelsPerInch = 96
  TextHeight = 13
  object Label39: TLabel [0]
    Left = 12
    Top = 88
    Width = 96
    Height = 13
    Caption = 'Maximum line length'
  end
  object Label37: TLabel [1]
    Left = 181
    Top = 88
    Width = 131
    Height = 13
    Caption = 'characters.  (0 = unlimited)'
  end
  object lblSplitPosts: TLabel [2]
    Left = 12
    Top = 116
    Width = 124
    Height = 13
    Caption = 'Split posts with more than'
  end
  object lblSplitPostsLines: TLabel [3]
    Left = 181
    Top = 116
    Width = 21
    Height = 13
    Caption = 'lines'
  end
  object Label41: TLabel [4]
    Left = 12
    Top = 144
    Width = 105
    Height = 13
    Caption = 'Default Character Set'
  end
  object lblDefaultISpellLanguage: TLabel [5]
    Left = 12
    Top = 172
    Width = 114
    Height = 13
    Caption = 'Default ISpell Language'
  end
  object Label40: TLabel [6]
    Left = 12
    Top = 200
    Width = 57
    Height = 13
    Caption = 'Text format'
  end
  object Label1: TLabel [7]
    Left = 12
    Top = 242
    Width = 54
    Height = 13
    Caption = 'Reply Style'
  end
  inherited Bevel2: TBevel
    ExplicitTop = 287
  end
  object cbUseOutbasket: TCheckBox [10]
    Left = 12
    Top = 60
    Width = 141
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Use Outbasket'
    TabOrder = 1
    OnClick = ControlClick
  end
  object edMaxPostLineLength: TEdit [11]
    Left = 141
    Top = 85
    Width = 37
    Height = 21
    TabOrder = 2
    Text = '72'
    OnChange = edMaxPostLineLengthChange
  end
  object edMaxPostLines: TEdit [12]
    Left = 141
    Top = 113
    Width = 37
    Height = 21
    TabOrder = 3
    Text = '5000'
    OnChange = edMaxPostLineLengthChange
  end
  object cbDefaultCharset: TComboBox [13]
    Left = 141
    Top = 141
    Width = 299
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    OnChange = cbDefaultCharsetChange
  end
  object cbDefaultISpellLanguage: TComboBox [14]
    Left = 141
    Top = 169
    Width = 299
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    OnChange = cbDefaultCharsetChange
  end
  object rbTextPartNNTP: TRadioButton [15]
    Left = 141
    Top = 200
    Width = 52
    Height = 17
    Caption = 'NNTP'
    Checked = True
    TabOrder = 6
    TabStop = True
    OnClick = ControlClick
  end
  object rbTextPartMIME: TRadioButton [16]
    Left = 141
    Top = 220
    Width = 89
    Height = 17
    Caption = 'MIME - Raw'
    TabOrder = 7
    OnClick = ControlClick
  end
  object rbTextPartQuotedPrintable: TRadioButton [17]
    Left = 246
    Top = 200
    Width = 137
    Height = 17
    Caption = 'MIME - Quoted Printable'
    TabOrder = 8
    OnClick = ControlClick
  end
  object rbTextPartFlowed: TRadioButton [18]
    Left = 246
    Top = 220
    Width = 113
    Height = 17
    Caption = 'MIME - Flowed'
    TabOrder = 9
    OnClick = ControlClick
  end
  object cbArchivePostedMessages: TCheckBox [19]
    Left = 12
    Top = 268
    Width = 141
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Archive Posted Messages'
    TabOrder = 11
    OnClick = ControlClick
  end
  object Panel2: TPanel [20]
    Left = 133
    Top = 242
    Width = 287
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 10
    object rbBottomPost: TRadioButton
      Left = 8
      Top = 0
      Width = 97
      Height = 17
      Caption = 'Reply at Bottom'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = ControlClick
    end
    object rbTopPost: TRadioButton
      Left = 113
      Top = 0
      Width = 113
      Height = 17
      Caption = 'Reply at Top'
      TabOrder = 1
      OnClick = ControlClick
    end
  end
  inherited btnReset: TButton
    Top = 384
    TabOrder = 12
    ExplicitTop = 297
  end
end
