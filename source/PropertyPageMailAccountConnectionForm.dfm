inherited fmPropertyPageMailAccountConnection: TfmPropertyPageMailAccountConnection
  Left = 637
  Top = 255
  Caption = 'Diallup & Connection'
  ExplicitLeft = 637
  ExplicitTop = 255
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel3: TBevel [0]
    Left = -4
    Top = 136
    Width = 390
    Height = 3
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
    ExplicitWidth = 276
  end
  object Label13: TLabel [1]
    Left = 12
    Top = 198
    Width = 59
    Height = 13
    Caption = 'Idle Timeout'
    Transparent = True
  end
  object Label12: TLabel [2]
    Left = 12
    Top = 222
    Width = 49
    Height = 13
    Caption = 'SMTP Port'
    Transparent = True
  end
  object Bevel6: TBevel [3]
    Left = -5
    Top = 248
    Width = 276
    Height = 3
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object Label15: TLabel [4]
    Left = 26
    Top = 283
    Width = 40
    Height = 13
    Caption = 'SSL Port'
    Transparent = True
  end
  object Label1: TLabel [5]
    Left = 172
    Top = 283
    Width = 92
    Height = 13
    Caption = '(Use 587 for Gmail)'
  end
  object lbReadTimeout: TLabel [6]
    Left = 12
    Top = 174
    Width = 66
    Height = 13
    Caption = 'Read Timeout'
    Transparent = True
  end
  object lbConnectTimeout: TLabel [7]
    Left = 12
    Top = 150
    Width = 81
    Height = 13
    Caption = 'Connect Timeout'
    Transparent = True
  end
  object Label28: TLabel [8]
    Left = 172
    Top = 150
    Width = 39
    Height = 13
    Caption = 'seconds'
  end
  object Label2: TLabel [9]
    Left = 172
    Top = 174
    Width = 39
    Height = 13
    Caption = 'seconds'
  end
  object Label3: TLabel [10]
    Left = 172
    Top = 198
    Width = 39
    Height = 13
    Caption = 'seconds'
  end
  inherited Panel1: TPanel
    ExplicitWidth = 270
    inherited Bevel1: TBevel
      ExplicitWidth = 270
    end
    inherited stSectionDetails: TLabel
      Caption = 
        'Controls how XanaNews connects to the Internet and interacts wit' +
        'h the mail server'
      ExplicitWidth = 258
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
    TabOrder = 4
    OnChange = cbRasEntriesChange
  end
  object edServerTimeout: TEdit
    Left = 136
    Top = 194
    Width = 30
    Height = 21
    TabOrder = 7
    Text = '60'
    OnChange = cbRasEntriesChange
  end
  object edSMTPPort: TEdit
    Left = 136
    Top = 218
    Width = 32
    Height = 21
    TabOrder = 8
    Text = '25'
    OnChange = cbRasEntriesChange
  end
  object cbSSL: TCheckBox
    Left = 12
    Top = 256
    Width = 136
    Height = 17
    Alignment = taLeftJustify
    Caption = 'SSL Required'
    TabOrder = 9
    OnClick = ControlClick
  end
  object edSSLPort: TEdit
    Left = 134
    Top = 279
    Width = 32
    Height = 21
    TabOrder = 10
    Text = '465'
    OnChange = cbRasEntriesChange
  end
  object edReadTimeout: TEdit
    Left = 136
    Top = 170
    Width = 30
    Height = 21
    TabOrder = 6
    Text = '60'
    OnChange = cbRasEntriesChange
  end
  object edConnectTimeout: TEdit
    Left = 136
    Top = 146
    Width = 30
    Height = 21
    TabOrder = 5
    Text = '60'
    OnChange = cbRasEntriesChange
  end
end
