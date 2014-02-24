inherited fmPropertyPageConnection: TfmPropertyPageConnection
  Left = 348
  Top = 282
  HelpType = htKeyword
  HelpKeyword = 'ConnectionOptions'
  ActiveControl = cbAutoDisconnectOnIdle
  Caption = 'Connection Options'
  ExplicitLeft = 348
  ExplicitTop = 282
  PixelsPerInch = 96
  TextHeight = 13
  inherited Panel1: TPanel
    ExplicitWidth = 299
    inherited Bevel1: TBevel
      ExplicitWidth = 299
    end
    inherited stSectionDetails: TLabel
      Caption = 
        'This section controls how XanaNews ends dialup connections.  It ' +
        'only applies to connections it has made.'
      ExplicitWidth = 287
    end
  end
  object cbAutoDisconnectOnIdle: TCheckBox
    Left = 12
    Top = 60
    Width = 145
    Height = 17
    Caption = 'Auto Disconnect on &Idle'
    TabOrder = 1
    OnClick = ControlsClick
  end
  object cbAutoDisconnectOnExit: TCheckBox
    Left = 12
    Top = 88
    Width = 145
    Height = 17
    Caption = 'Auto Disconnect on &Exit'
    TabOrder = 2
    OnClick = ControlsClick
  end
end
