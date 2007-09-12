inherited fmPropertyPageAccountAdvancedServer: TfmPropertyPageAccountAdvancedServer
  Left = 411
  Top = 213
  HelpType = htKeyword
  HelpKeyword = 'AccountDiallupConnection'
  Caption = 'Diallup & Connection'
  ClientHeight = 369
  ClientWidth = 350
  Constraints.MinHeight = 369
  Constraints.MinWidth = 350
  PixelsPerInch = 96
  TextHeight = 13
  object Label13: TLabel [0]
    Left = 12
    Top = 148
    Width = 58
    Height = 13
    Caption = 'Idle Timeout'
    Transparent = True
  end
  object Label28: TLabel [1]
    Left = 172
    Top = 148
    Width = 40
    Height = 13
    Caption = 'seconds'
  end
  object Label12: TLabel [2]
    Left = 12
    Top = 172
    Width = 52
    Height = 13
    Caption = 'NNTP Port'
    Transparent = True
  end
  object Label15: TLabel [3]
    Left = 28
    Top = 274
    Width = 42
    Height = 13
    Caption = 'SSL Port'
    Transparent = True
  end
  object Label3: TLabel [4]
    Left = 28
    Top = 330
    Width = 94
    Height = 13
    Caption = 'Pipeline Chunk Size'
  end
  object Label1: TLabel [5]
    Left = 176
    Top = 330
    Width = 25
    Height = 13
    Caption = 'bytes'
  end
  object Bevel6: TBevel [6]
    Left = -5
    Top = 240
    Width = 354
    Height = 3
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object Bevel2: TBevel [7]
    Left = -5
    Top = 296
    Width = 354
    Height = 3
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object Bevel3: TBevel [8]
    Left = -4
    Top = 136
    Width = 354
    Height = 3
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object Label2: TLabel [9]
    Left = 12
    Top = 218
    Width = 106
    Height = 13
    Caption = 'Maximum Connections'
  end
  object Label4: TLabel [10]
    Left = 192
    Top = 218
    Width = 59
    Height = 13
    Caption = '0 = unlimited'
  end
  inherited Panel1: TPanel
    Width = 350
    inherited Bevel1: TBevel
      Width = 350
    end
    inherited stSectionDetails: TLabel
      Width = 338
      Caption = 
        'Controls how XanaNews connects to the Internet and interacts wit' +
        'h the server'
    end
  end
  object edServerTimeout: TEdit
    Left = 136
    Top = 144
    Width = 29
    Height = 21
    TabOrder = 1
    Text = '60'
    OnChange = ControlChange
  end
  object cbUseXOVER: TCheckBox
    Left = 11
    Top = 195
    Width = 138
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Use XOVER'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = ControlClick
  end
  object cbUsePipelining: TCheckBox
    Left = 11
    Top = 307
    Width = 138
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Use Pipelining'
    Checked = True
    State = cbChecked
    TabOrder = 6
    OnClick = ControlClick
  end
  object edNNTPPort: TEdit
    Left = 136
    Top = 171
    Width = 30
    Height = 21
    TabOrder = 2
    Text = '119'
    OnChange = ControlChange
  end
  object cbSSL: TCheckBox
    Left = 10
    Top = 248
    Width = 138
    Height = 17
    Alignment = taLeftJustify
    Caption = 'SSL Required'
    TabOrder = 4
    OnClick = ControlClick
  end
  object edSSLPort: TEdit
    Left = 136
    Top = 270
    Width = 32
    Height = 21
    TabOrder = 5
    Text = '563'
    OnChange = ControlChange
  end
  object edPipelineSize: TEdit
    Left = 136
    Top = 328
    Width = 37
    Height = 21
    TabOrder = 7
    Text = '1024'
    OnChange = ControlChange
  end
  object rbDialDefault: TRadioButton
    Left = 12
    Top = 60
    Width = 185
    Height = 17
    Caption = 'Dial default Internet connection'
    TabOrder = 8
    OnClick = ControlClick
  end
  object rbDontDial: TRadioButton
    Left = 12
    Top = 84
    Width = 113
    Height = 17
    Caption = 'Don'#39't dial'
    TabOrder = 9
    OnClick = ControlClick
  end
  object rbAlwaysDial: TRadioButton
    Left = 12
    Top = 108
    Width = 81
    Height = 17
    Caption = 'Always dial'
    TabOrder = 10
    OnClick = ControlClick
  end
  object cbRasEntries: TComboBox
    Left = 96
    Top = 104
    Width = 246
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 0
    TabOrder = 11
    OnChange = cbRasEntriesChange
  end
  object edMaxConnections: TEdit
    Left = 136
    Top = 216
    Width = 33
    Height = 21
    TabOrder = 12
    Text = '4'
    OnChange = ControlChange
  end
  object udMaxConnections: TUpDown
    Left = 169
    Top = 216
    Width = 15
    Height = 21
    Associate = edMaxConnections
    Max = 256
    Position = 4
    TabOrder = 13
  end
end
