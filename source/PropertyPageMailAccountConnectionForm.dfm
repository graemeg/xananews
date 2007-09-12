inherited fmPropertyPageMailAccountConnection: TfmPropertyPageMailAccountConnection
  Left = 373
  Top = 235
  Caption = 'Diallup & Connection'
  ClientHeight = 258
  ClientWidth = 270
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel3: TBevel [0]
    Left = -4
    Top = 136
    Width = 276
    Height = 3
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object Label13: TLabel [1]
    Left = 12
    Top = 148
    Width = 38
    Height = 13
    Caption = 'Timeout'
    Transparent = True
  end
  object Label12: TLabel [2]
    Left = 12
    Top = 172
    Width = 52
    Height = 13
    Caption = 'SMTP Port'
    Transparent = True
  end
  object Bevel6: TBevel [3]
    Left = -5
    Top = 200
    Width = 276
    Height = 3
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object Label15: TLabel [4]
    Left = 26
    Top = 235
    Width = 42
    Height = 13
    Caption = 'SSL Port'
    Transparent = True
  end
  object Label1: TLabel [5]
    Left = 172
    Top = 234
    Width = 90
    Height = 13
    Caption = '(Use 587 for Gmail)'
  end
  inherited Panel1: TPanel
    Width = 270
    inherited Bevel1: TBevel
      Width = 270
    end
    inherited stSectionDetails: TLabel
      Width = 258
      Caption = 
        'Controls how XanaNews connects to the Internet and interacts wit' +
        'h the mail server'
    end
  end
  object rbDialDefault: TRadioButton
    Left = 12
    Top = 60
    Width = 185
    Height = 17
    Caption = 'Dial default Internet connection'
    TabOrder = 1
    OnClick = ControlClick
  end
  object rbDontDial: TRadioButton
    Left = 12
    Top = 84
    Width = 113
    Height = 17
    Caption = 'Don'#39't dial'
    TabOrder = 2
    OnClick = ControlClick
  end
  object rbAlwaysDial: TRadioButton
    Left = 12
    Top = 108
    Width = 81
    Height = 17
    Caption = 'Always dial'
    TabOrder = 3
    OnClick = ControlClick
  end
  object cbRasEntries: TComboBox
    Left = 96
    Top = 104
    Width = 168
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 0
    TabOrder = 4
    OnChange = cbRasEntriesChange
  end
  object edServerTimeout: TEdit
    Left = 136
    Top = 144
    Width = 29
    Height = 21
    TabOrder = 5
    Text = '60'
    OnChange = cbRasEntriesChange
  end
  object edSMTPPort: TEdit
    Left = 136
    Top = 171
    Width = 30
    Height = 21
    TabOrder = 6
    Text = '25'
    OnChange = cbRasEntriesChange
  end
  object cbSSL: TCheckBox
    Left = 10
    Top = 208
    Width = 138
    Height = 17
    Alignment = taLeftJustify
    Caption = 'SSL Required'
    TabOrder = 7
    OnClick = ControlClick
  end
  object edSSLPort: TEdit
    Left = 134
    Top = 231
    Width = 32
    Height = 21
    TabOrder = 8
    Text = '465'
    OnChange = cbRasEntriesChange
  end
end
