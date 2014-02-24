inherited fmPropertyPageAccountAdvancedServer: TfmPropertyPageAccountAdvancedServer
  Left = 628
  Top = 212
  HelpType = htKeyword
  HelpKeyword = 'AccountDiallupConnection'
  Caption = 'Diallup & Connection'
  ExplicitLeft = 628
  ExplicitTop = 212
  PixelsPerInch = 96
  TextHeight = 13
  object Label13: TLabel [0]
    Left = 12
    Top = 196
    Width = 59
    Height = 13
    Caption = 'Idle Timeout'
    Transparent = True
  end
  object Label28: TLabel [1]
    Left = 172
    Top = 196
    Width = 39
    Height = 13
    Caption = 'seconds'
  end
  object Label12: TLabel [2]
    Left = 12
    Top = 220
    Width = 49
    Height = 13
    Caption = 'NNTP Port'
    Transparent = True
  end
  object Label15: TLabel [3]
    Left = 28
    Top = 322
    Width = 40
    Height = 13
    Caption = 'SSL Port'
    Transparent = True
  end
  object Label3: TLabel [4]
    Left = 28
    Top = 378
    Width = 91
    Height = 13
    Caption = 'Pipeline Chunk Size'
  end
  object Label1: TLabel [5]
    Left = 176
    Top = 378
    Width = 27
    Height = 13
    Caption = 'bytes'
  end
  object Bevel6: TBevel [6]
    Left = -5
    Top = 288
    Width = 388
    Height = 3
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
    ExplicitWidth = 354
  end
  object Bevel2: TBevel [7]
    Left = -5
    Top = 344
    Width = 388
    Height = 3
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
    ExplicitWidth = 354
  end
  object Bevel3: TBevel [8]
    Left = -4
    Top = 136
    Width = 388
    Height = 3
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
    ExplicitWidth = 354
  end
  object Label2: TLabel [9]
    Left = 12
    Top = 266
    Width = 106
    Height = 13
    Caption = 'Maximum Connections'
  end
  object Label4: TLabel [10]
    Left = 192
    Top = 266
    Width = 62
    Height = 13
    Caption = '0 = unlimited'
  end
  object lbConnectTimeout: TLabel [11]
    Left = 12
    Top = 148
    Width = 81
    Height = 13
    Caption = 'Connect Timeout'
    Transparent = True
  end
  object Label6: TLabel [12]
    Left = 172
    Top = 148
    Width = 39
    Height = 13
    Caption = 'seconds'
  end
  object lbReadTimeout: TLabel [13]
    Left = 12
    Top = 172
    Width = 66
    Height = 13
    Caption = 'Read Timeout'
    Transparent = True
  end
  object Label8: TLabel [14]
    Left = 172
    Top = 172
    Width = 39
    Height = 13
    Caption = 'seconds'
  end
  inherited Panel1: TPanel
    ExplicitWidth = 350
    inherited Bevel1: TBevel
      ExplicitWidth = 350
    end
    inherited stSectionDetails: TLabel
      Caption = 
        'Controls how XanaNews connects to the Internet and interacts wit' +
        'h the server'
      ExplicitWidth = 338
    end
  end
  object edServerTimeout: TEdit
    Left = 136
    Top = 193
    Width = 29
    Height = 21
    TabOrder = 3
    Text = '60'
    OnChange = ControlChange
  end
  object cbUseXOVER: TCheckBox
    Left = 11
    Top = 243
    Width = 138
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Use XOVER'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = ControlClick
  end
  object cbUsePipelining: TCheckBox
    Left = 11
    Top = 355
    Width = 138
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Use Pipelining'
    Checked = True
    State = cbChecked
    TabOrder = 8
    OnClick = ControlClick
  end
  object edNNTPPort: TEdit
    Left = 136
    Top = 217
    Width = 32
    Height = 21
    TabOrder = 4
    Text = '119'
    OnChange = ControlChange
  end
  object cbSSL: TCheckBox
    Left = 10
    Top = 296
    Width = 138
    Height = 17
    Alignment = taLeftJustify
    Caption = 'SSL Required'
    TabOrder = 6
    OnClick = ControlClick
  end
  object edSSLPort: TEdit
    Left = 136
    Top = 318
    Width = 32
    Height = 21
    TabOrder = 7
    Text = '563'
    OnChange = ControlChange
  end
  object edPipelineSize: TEdit
    Left = 136
    Top = 376
    Width = 37
    Height = 21
    TabOrder = 9
    Text = '1024'
    OnChange = ControlChange
  end
  object rbDialDefault: TRadioButton
    Left = 12
    Top = 60
    Width = 185
    Height = 17
    Caption = 'Dial default Internet connection'
    TabOrder = 10
    OnClick = ControlClick
  end
  object rbDontDial: TRadioButton
    Left = 12
    Top = 84
    Width = 113
    Height = 17
    Caption = 'Don'#39't dial'
    TabOrder = 11
    OnClick = ControlClick
  end
  object rbAlwaysDial: TRadioButton
    Left = 12
    Top = 108
    Width = 81
    Height = 17
    Caption = 'Always dial'
    TabOrder = 12
    OnClick = ControlClick
  end
  object cbRasEntries: TComboBox
    Left = 96
    Top = 104
    Width = 280
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 13
    OnChange = cbRasEntriesChange
    ExplicitWidth = 246
  end
  object edMaxConnections: TEdit
    Left = 136
    Top = 264
    Width = 33
    Height = 21
    TabOrder = 14
    Text = '4'
    OnChange = ControlChange
  end
  object udMaxConnections: TUpDown
    Left = 169
    Top = 264
    Width = 15
    Height = 21
    Associate = edMaxConnections
    Max = 256
    Position = 4
    TabOrder = 15
  end
  object edConnectTimeout: TEdit
    Left = 136
    Top = 145
    Width = 29
    Height = 21
    TabOrder = 1
    Text = '60'
    OnChange = ControlChange
  end
  object edReadTimeout: TEdit
    Left = 136
    Top = 169
    Width = 29
    Height = 21
    TabOrder = 2
    Text = '60'
    OnChange = ControlChange
  end
end
