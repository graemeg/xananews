inherited fmPropertyPagePreferences: TfmPropertyPagePreferences
  HelpType = htKeyword
  HelpKeyword = 'UserPreferences'
  Caption = 'User Preferences'
  ClientWidth = 400
  ExplicitWidth = 416
  PixelsPerInch = 96
  TextHeight = 13
  inherited Bevel2: TBevel
    Width = 400
    ExplicitTop = 350
    ExplicitWidth = 400
  end
  object Label6: TLabel [1]
    Left = 12
    Top = 156
    Width = 105
    Height = 26
    Caption = 'Default Character Set for Message Display'
    WordWrap = True
  end
  object Label5: TLabel [2]
    Left = 12
    Top = 240
    Width = 305
    Height = 13
    Caption = 'Play sound when new replies arrive (When in System Tray only)'
    WordWrap = True
  end
  object Label1: TLabel [3]
    Left = 12
    Top = 124
    Width = 74
    Height = 13
    Caption = 'Purge to Folder'
  end
  object Label2: TLabel [4]
    Left = 12
    Top = 60
    Width = 107
    Height = 13
    Caption = #39'Get Messages'#39' action:'
  end
  object lblDefaultAction: TLabel [5]
    Left = 144
    Top = 60
    Width = 169
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'All new messages - Full'
    WordWrap = True
  end
  object Label4: TLabel [6]
    Left = 12
    Top = 196
    Width = 117
    Height = 33
    AutoSize = False
    Caption = 'Truncate signatures when a line starts with:'
    WordWrap = True
  end
  object Label3: TLabel [7]
    Left = 12
    Top = 88
    Width = 99
    Height = 26
    Caption = 'Perform action when selecting group'
    WordWrap = True
  end
  object Label7: TLabel [8]
    Left = 12
    Top = 286
    Width = 276
    Height = 13
    Caption = 'Signature to use in preference to the one in your Identity'
  end
  inherited Panel1: TPanel
    Width = 400
    ExplicitWidth = 400
    inherited Bevel1: TBevel
      Width = 400
      ExplicitWidth = 400
    end
    inherited stSectionDetails: TLabel
      Width = 388
      ExplicitWidth = 388
    end
  end
  inherited btnReset: TButton
    Left = 289
    TabOrder = 11
    ExplicitLeft = 289
    ExplicitTop = 359
  end
  object cbDefMessageCharset: TComboBox
    Left = 144
    Top = 159
    Width = 252
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
    OnChange = cbDefMessageCharsetChange
  end
  object edPlaySound: TEdit
    Left = 12
    Top = 258
    Width = 340
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
    OnChange = edTruncateFromChange
  end
  object btnPlaySound: TButton
    Left = 363
    Top = 256
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 9
    OnClick = btnPlaySoundClick
  end
  object cbPurgeToFolder: TComboBox
    Left = 144
    Top = 120
    Width = 252
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    OnChange = cbDefMessageCharsetChange
  end
  object btnChangeDefaultAction: TButton
    Left = 319
    Top = 55
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Change'
    TabOrder = 1
    OnClick = btnChangeDefaultActionClick
  end
  object edTruncateFrom: TEdit
    Left = 144
    Top = 200
    Width = 252
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
    OnChange = edTruncateFromChange
  end
  object rbPerformNever: TRadioButton
    Left = 144
    Top = 96
    Width = 57
    Height = 17
    Caption = 'Never'
    Checked = True
    TabOrder = 2
    TabStop = True
    OnClick = rbPerformNeverClick
  end
  object rbPerformOncePerSession: TRadioButton
    Left = 208
    Top = 96
    Width = 105
    Height = 17
    Caption = 'Once per Session'
    TabOrder = 3
    OnClick = rbPerformNeverClick
  end
  object rbPerformAlways: TRadioButton
    Left = 320
    Top = 96
    Width = 69
    Height = 17
    Caption = 'Always'
    TabOrder = 4
    OnClick = rbPerformNeverClick
  end
  object mmoSignature: TMemo
    Left = 12
    Top = 304
    Width = 377
    Height = 61
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 10
    OnChange = mmoSignatureChange
    ExplicitHeight = 37
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'wav'
    Filter = 'Wave Files (*.wav)|*.wav|Any File (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 232
    Top = 360
  end
end
