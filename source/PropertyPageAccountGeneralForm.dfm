inherited fmPropertyPageAccountGeneral: TfmPropertyPageAccountGeneral
  HelpType = htKeyword
  HelpKeyword = 'BasicAccountSettings'
  ActiveControl = edAccountName
  Caption = 'Basic Account Settings'
  ClientHeight = 299
  ClientWidth = 377
  PixelsPerInch = 96
  TextHeight = 13
  object Label7: TLabel [0]
    Left = 12
    Top = 60
    Width = 357
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Type the name by which you would like to refer to this News Acco' +
      'unt.  For example '#39'Demon Internet'#39
    Transparent = True
    WordWrap = True
  end
  object Bevel6: TBevel [1]
    Left = -5
    Top = 128
    Width = 390
    Height = 3
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  inherited Panel1: TPanel
    Width = 377
    inherited Bevel1: TBevel
      Width = 377
    end
    inherited stSectionDetails: TLabel
      Width = 365
      Caption = 'The account name, and additional account settings'
    end
  end
  object edAccountName: TEdit
    Left = 12
    Top = 92
    Width = 356
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = edAccountNameChange
  end
  object cbMarkOnLeave: TCheckBox
    Left = 12
    Top = 240
    Width = 355
    Height = 41
    Alignment = taLeftJustify
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Mark Messages As Read When Leaving Groups in this account'
    TabOrder = 4
    WordWrap = True
    OnClick = controlClick
  end
  object cbScanKeyPhrases: TCheckBox
    Left = 12
    Top = 192
    Width = 355
    Height = 41
    Alignment = taLeftJustify
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'Enable Key phrase scanning for this account.  Set up key phrses ' +
      'in the Keywords section of Tools, Options'
    TabOrder = 3
    WordWrap = True
    OnClick = controlClick
  end
  object cbSecret: TCheckBox
    Left = 12
    Top = 144
    Width = 355
    Height = 41
    Alignment = taLeftJustify
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'Make this is a Secret Account.  It will not appear until you sel' +
      'ect '#39'Show Secret Accounts'#39' from the View menu'
    TabOrder = 2
    WordWrap = True
    OnClick = controlClick
  end
end
