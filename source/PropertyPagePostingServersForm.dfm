inherited fmPropertyPagePostingServers: TfmPropertyPagePostingServers
  Left = 445
  Top = 245
  HelpType = htKeyword
  HelpKeyword = 'AccountPostingServer'
  Caption = 'Posting Server Details'
  ExplicitLeft = 445
  ExplicitTop = 245
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel [0]
    Left = 12
    Top = 120
    Width = 157
    Height = 13
    Caption = '&Reply by mail using Mail account:'
  end
  object Label1: TLabel [1]
    Left = 12
    Top = 60
    Width = 144
    Height = 13
    Caption = '&Post messages using account:'
    FocusControl = cbPostAccount
  end
  inherited Panel1: TPanel
    ExplicitWidth = 323
    inherited Bevel1: TBevel
      ExplicitWidth = 323
    end
    inherited stSectionDetails: TLabel
      Caption = 'Specify the servers used for posting News & Mail messages'
      ExplicitWidth = 311
    end
  end
  object cbMailAccount: TComboBox
    Left = 12
    Top = 136
    Width = 366
    Height = 21
    AutoDropDown = True
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = ControlChange
    ExplicitWidth = 305
  end
  object cbPostAccount: TComboBox
    Left = 12
    Top = 76
    Width = 366
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = ControlChange
    ExplicitWidth = 305
  end
end
