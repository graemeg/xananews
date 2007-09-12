inherited fmPropertyPageDefaults: TfmPropertyPageDefaults
  Left = 325
  Top = 190
  Caption = 'fmPropertyPageDefaults'
  ClientHeight = 309
  ClientWidth = 455
  OnShow = FormShow
  DesignSize = (
    455
    309)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel2: TBevel [0]
    Left = 0
    Top = 269
    Width = 455
    Height = 40
    Align = alBottom
    Shape = bsTopLine
  end
  inherited Panel1: TPanel
    Width = 455
    DesignSize = (
      455
      41)
    inherited Bevel1: TBevel
      Width = 455
    end
    inherited stSectionDetails: TLabel
      Width = 443
    end
  end
  object btnReset: TButton
    Left = 344
    Top = 278
    Width = 103
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Restore Defaults'
    TabOrder = 1
    OnClick = btnResetClick
  end
end
