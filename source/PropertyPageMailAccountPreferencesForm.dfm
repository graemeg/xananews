inherited fmPropertyPageMailAccountPreferences: TfmPropertyPageMailAccountPreferences
  Caption = 'User Preferences'
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel [0]
    Left = 12
    Top = 66
    Width = 38
    Height = 13
    Caption = 'Identity'
  end
  inherited Panel1: TPanel
    ExplicitWidth = 384
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
