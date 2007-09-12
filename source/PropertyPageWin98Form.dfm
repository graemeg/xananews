inherited fmPropertyPageWin98: TfmPropertyPageWin98
  Caption = 'Windows 98 Settings'
  ClientHeight = 163
  ClientWidth = 378
  Constraints.MinHeight = 123
  Constraints.MinWidth = 295
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
    Width = 378
    inherited Bevel1: TBevel
      Width = 378
    end
    inherited stSectionDetails: TLabel
      Width = 366
      Caption = 
        'On Windows 98/ME systems you may need to set Text Window Size to' +
        ' a very low value (eg. 3)'
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
