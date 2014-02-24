inherited fmPropertyPageMailAccountGeneral: TfmPropertyPageMailAccountGeneral
  Left = 540
  Top = 324
  Caption = 'Basic Account Settings'
  ExplicitLeft = 540
  ExplicitTop = 324
  PixelsPerInch = 96
  TextHeight = 13
  object Label7: TLabel [0]
    Left = 12
    Top = 60
    Width = 368
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Type the name by which you would like to refer to this Mail Acco' +
      'unt.  For example '#39'Demon Mail'#39
    Transparent = True
    WordWrap = True
    ExplicitWidth = 242
  end
  inherited Panel1: TPanel
    ExplicitWidth = 258
    inherited Bevel1: TBevel
      ExplicitWidth = 258
    end
    inherited stSectionDetails: TLabel
      Caption = 'The account name, and additional account settings'
      ExplicitWidth = 246
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
    ExplicitWidth = 237
  end
end
