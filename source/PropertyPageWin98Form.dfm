inherited fmPropertyPageWin98: TfmPropertyPageWin98
  Caption = 'Windows 98 Settings'
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel [0]
    Left = 12
    Top = 64
    Width = 132
    Height = 13
    Caption = 'Text Window Size (K Pixels)'
  end
  inherited Panel1: TPanel
    ExplicitWidth = 378
    inherited Bevel1: TBevel
      ExplicitWidth = 378
    end
    inherited stSectionDetails: TLabel
      Caption = 
        'On Windows 98/ME systems you may need to set Text Window Size to' +
        ' a very low value (eg. 3)'
      ExplicitWidth = 366
    end
  end
  object edTextWindowSize: TEdit
    Left = 156
    Top = 60
    Width = 121
    Height = 21
    TabOrder = 1
    OnChange = edTextWindowSizeChange
  end
end
