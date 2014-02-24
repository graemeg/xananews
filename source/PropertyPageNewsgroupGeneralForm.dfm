inherited fmPropertyPageNewsgroupGeneral: TfmPropertyPageNewsgroupGeneral
  HelpType = htKeyword
  HelpKeyword = 'BasicNewsgroupSettings'
  Caption = 'Basic Newsgroup Settings'
  ClientWidth = 447
  ExplicitWidth = 463
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel [0]
    Left = 12
    Top = 92
    Width = 423
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'You can enter a Nickname for the group.  This will show in the s' +
      'ubscribed groups tree instead of the original group name'
    WordWrap = True
  end
  inherited Panel1: TPanel
    Width = 447
    ExplicitWidth = 447
    inherited Bevel1: TBevel
      Width = 447
      ExplicitWidth = 447
    end
    inherited stSectionDetails: TLabel
      Width = 435
      Caption = 'Basic Newsgroup Settings'
      ExplicitWidth = 435
    end
  end
  object edNewsgroup: TEdit
    Left = 12
    Top = 60
    Width = 425
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 1
  end
  object cbNickname: TComboBox
    Left = 12
    Top = 120
    Width = 425
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = cbNicknameChange
  end
  object cbSecret: TCheckBox
    Left = 12
    Top = 152
    Width = 423
    Height = 41
    Alignment = taLeftJustify
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'Make this is a Secret Newsgroup.  It will not appear until you s' +
      'elect '#39'Show Secret Accounts && Newsgroups'#39' from the View menu'
    TabOrder = 3
    WordWrap = True
    OnClick = cbSecretClick
  end
end
