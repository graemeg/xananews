inherited fmPropertyPageGeneral: TfmPropertyPageGeneral
  Left = 547
  HelpType = htKeyword
  HelpKeyword = 'ProgramSettings'
  ActiveControl = cbShowInSystemTray
  Caption = 'Program Settings'
  ClientHeight = 370
  ClientWidth = 397
  Constraints.MinHeight = 370
  Constraints.MinWidth = 397
  ExplicitLeft = 547
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [0]
    Left = 12
    Top = 144
    Width = 173
    Height = 13
    Caption = 'Check for Latest Version on Internet:'
  end
  object Label2: TLabel [1]
    Left = 16
    Top = 256
    Width = 132
    Height = 13
    Caption = 'Find Message Id on Internet'
  end
  object Label3: TLabel [2]
    Left = 32
    Top = 280
    Width = 50
    Height = 13
    Caption = 'URL Stub:'
  end
  object Label4: TLabel [3]
    Left = 16
    Top = 312
    Width = 98
    Height = 13
    Caption = 'Find Text on Internet'
  end
  object Label5: TLabel [4]
    Left = 32
    Top = 336
    Width = 50
    Height = 13
    Caption = 'URL Stub:'
  end
  inherited Panel1: TPanel
    Width = 397
    ExplicitWidth = 397
    inherited Bevel1: TBevel
      Width = 397
      ExplicitWidth = 397
    end
    inherited stSectionDetails: TLabel
      Width = 385
      Caption = 
        'This section contains settings that define the basic behaviour o' +
        'f XanaNews'
      ExplicitWidth = 385
    end
  end
  object cbShowInSystemTray: TCheckBox
    Left = 12
    Top = 56
    Width = 149
    Height = 17
    Caption = 'Show in &System Tray'
    TabOrder = 1
    OnClick = ControlClick
  end
  object cbShowTooltips: TCheckBox
    Left = 12
    Top = 84
    Width = 133
    Height = 17
    Caption = 'Show Tree &Tooltips'
    TabOrder = 2
    OnClick = ControlClick
  end
  object rbVersionAsk: TRadioButton
    Left = 28
    Top = 192
    Width = 129
    Height = 17
    Caption = '&Ask Before Checking'
    TabOrder = 8
    OnClick = ControlClick
  end
  object rbVersionDont: TRadioButton
    Left = 28
    Top = 168
    Width = 113
    Height = 17
    Caption = '&Don'#39't Check'
    TabOrder = 7
    OnClick = ControlClick
  end
  object rbVersionAlways: TRadioButton
    Left = 28
    Top = 216
    Width = 113
    Height = 17
    Caption = 'Al&ways Check'
    TabOrder = 9
    OnClick = ControlClick
  end
  object cbHideDormantConnections: TCheckBox
    Left = 12
    Top = 112
    Width = 157
    Height = 17
    Caption = 'Hide &Dormant Connections'
    TabOrder = 3
    OnClick = ControlClick
  end
  object cbAutoCrossPostDetect: TCheckBox
    Left = 180
    Top = 56
    Width = 205
    Height = 17
    Caption = 'Mark &Crossposted Messages as Read'
    TabOrder = 4
    OnClick = ControlClick
  end
  object cbQuoteFullText: TCheckBox
    Left = 180
    Top = 84
    Width = 181
    Height = 17
    Caption = '&Quote Full Text if None Selected'
    TabOrder = 5
    OnClick = ControlClick
  end
  object edSearchInternetURLStub: TEdit
    Left = 88
    Top = 276
    Width = 294
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 10
    OnChange = edSearchInternetURLStubChange
  end
  object cbPlainTextPasswords: TCheckBox
    Left = 180
    Top = 112
    Width = 181
    Height = 17
    Caption = 'Show Passwords in Plain Text'
    TabOrder = 6
    OnClick = ControlClick
  end
  object edTextInternetURLStub: TEdit
    Left = 88
    Top = 332
    Width = 294
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 11
    OnChange = edTextInternetURLStubChange
  end
end
