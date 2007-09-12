inherited fmPropertyPagePostingServers: TfmPropertyPagePostingServers
  Left = 445
  Top = 245
  HelpType = htKeyword
  HelpKeyword = 'AccountPostingServer'
  Caption = 'Posting Server Details'
  ClientHeight = 181
  ClientWidth = 323
  Constraints.MinHeight = 208
  Constraints.MinWidth = 331
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
    Width = 323
    inherited Bevel1: TBevel
      Width = 323
    end
    inherited stSectionDetails: TLabel
      Width = 311
      Caption = 'Specify the servers used for posting News & Mail messages'
    end
  end
  object cbMailAccount: TComboBox
    Left = 12
    Top = 136
    Width = 305
    Height = 21
    AutoDropDown = True
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 0
    TabOrder = 2
    OnChange = ControlChange
  end
  object cbPostAccount: TComboBox
    Left = 12
    Top = 76
    Width = 305
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 0
    TabOrder = 1
    OnChange = ControlChange
  end
end
