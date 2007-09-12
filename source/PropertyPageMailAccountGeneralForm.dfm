inherited fmPropertyPageMailAccountGeneral: TfmPropertyPageMailAccountGeneral
  Left = 540
  Top = 324
  Caption = 'Basic Account Settings'
  ClientHeight = 391
  ClientWidth = 258
  Constraints.MinHeight = 391
  Constraints.MinWidth = 258
  PixelsPerInch = 96
  TextHeight = 13
  object Label7: TLabel [0]
    Left = 12
    Top = 60
    Width = 242
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Type the name by which you would like to refer to this Mail Acco' +
      'unt.  For example '#39'Demon Mail'#39
    Transparent = True
    WordWrap = True
  end
  inherited Panel1: TPanel
    Width = 258
    inherited Bevel1: TBevel
      Width = 258
    end
    inherited stSectionDetails: TLabel
      Width = 246
      Caption = 'The account name, and additional account settings'
    end
  end
  object edAccountName: TEdit
    Left = 12
    Top = 92
    Width = 237
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = edAccountNameChange
  end
end
