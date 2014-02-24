inherited fmOptions: TfmOptions
  HelpType = htKeyword
  HelpKeyword = 'Options'
  Caption = 'XanaNews Options'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlButtons: TPanel
    object btnDefaultNewsreader: TButton [0]
      Left = 0
      Top = 8
      Width = 185
      Height = 25
      Caption = ' Set as Default Newsreader'
      TabOrder = 0
      OnClick = btnDefaultNewsreaderClick
    end
    inherited btnOK: TButton
      TabOrder = 1
    end
    inherited btnCancel: TButton
      TabOrder = 2
    end
    inherited btnApply: TButton
      TabOrder = 3
    end
    inherited btnHelp: TButton
      TabOrder = 4
    end
  end
  inherited PersistentPosition1: TPersistentPosition
    SubKey = 'Position\Options'
  end
end
