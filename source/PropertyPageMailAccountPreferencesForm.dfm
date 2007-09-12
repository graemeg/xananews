inherited fmPropertyPageMailAccountPreferences: TfmPropertyPageMailAccountPreferences
  Caption = 'User Preferences'
  ClientHeight = 96
  Constraints.MinHeight = 123
  Constraints.MinWidth = 381
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel [0]
    Left = 12
    Top = 66
    Width = 34
    Height = 13
    Caption = 'Identity'
  end
  inherited Panel1: TPanel
    inherited stSectionDetails: TLabel
      Caption = 'Use this identity when sending mail messages'
    end
  end
  object cbIdentities: TComboBox
    Left = 60
    Top = 62
    Width = 217
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 0
    TabOrder = 1
    Text = 'cbIdentities'
    OnChange = cbIdentitiesChange
  end
  object btnNewIdentity: TButton
    Left = 288
    Top = 60
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'New...'
    TabOrder = 2
    OnClick = btnNewIdentityClick
  end
end
