inherited fmPropertyPageConnection: TfmPropertyPageConnection
  Left = 348
  Top = 282
  HelpType = htKeyword
  HelpKeyword = 'ConnectionOptions'
  ActiveControl = cbAutoDisconnectOnIdle
  Caption = 'Connection Options'
  ClientHeight = 211
  ClientWidth = 299
  Constraints.MinHeight = 175
  Constraints.MinWidth = 197
  PixelsPerInch = 96
  TextHeight = 13
  inherited Panel1: TPanel
    Width = 299
    inherited Bevel1: TBevel
      Width = 299
    end
    inherited stSectionDetails: TLabel
      Width = 287
      Caption = 
        'This section controls how XanaNews ends dialup connections.  It ' +
        'only applies to connections it has made.'
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
