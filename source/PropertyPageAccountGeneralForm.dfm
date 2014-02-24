inherited fmPropertyPageAccountGeneral: TfmPropertyPageAccountGeneral
  HelpType = htKeyword
  HelpKeyword = 'BasicAccountSettings'
  ActiveControl = edAccountName
  Caption = 'Basic Account Settings'
  PixelsPerInch = 96
  TextHeight = 13
  object Label7: TLabel [0]
    Left = 12
    Top = 60
    Width = 364
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Type the name by which you would like to refer to this News Acco' +
      'unt.  For example '#39'Demon Internet'#39
    Transparent = True
    WordWrap = True
    ExplicitWidth = 357
  end
  object Bevel6: TBevel [1]
    Left = -5
    Top = 128
    Width = 397
    Height = 3
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
    ExplicitWidth = 390
  end
  inherited Panel1: TPanel
    ExplicitWidth = 377
    inherited Bevel1: TBevel
      ExplicitWidth = 377
    end
    inherited stSectionDetails: TLabel
      Caption = 'The account name, and additional account settings'
      ExplicitWidth = 365
    end
  end
  object edAccountName: TEdit
    Left = 12
    Top = 92
    Width = 363
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = edAccountNameChange
    ExplicitWidth = 356
  end
  object cbMarkOnLeave: TCheckBox
    Left = 12
    Top = 240
    Width = 362
    Height = 41
    Alignment = taLeftJustify
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Mark Messages As Read When Leaving Groups in this account'
    TabOrder = 4
    WordWrap = True
    OnClick = controlClick
    ExplicitWidth = 355
  end
  object cbScanKeyPhrases: TCheckBox
    Left = 12
    Top = 192
    Width = 362
    Height = 41
    Alignment = taLeftJustify
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'Enable Key phrase scanning for this account.  Set up key phrses ' +
      'in the Keywords section of Tools, Options'
    TabOrder = 3
    WordWrap = True
    OnClick = controlClick
    ExplicitWidth = 355
  end
  object cbSecret: TCheckBox
    Left = 12
    Top = 144
    Width = 362
    Height = 41
    Alignment = taLeftJustify
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'Make this is a Secret Account.  It will not appear until you sel' +
      'ect '#39'Show Secret Accounts'#39' from the View menu'
    TabOrder = 2
    WordWrap = True
    OnClick = controlClick
    ExplicitWidth = 355
  end
end
