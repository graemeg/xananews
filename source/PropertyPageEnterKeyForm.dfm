inherited fmPropertyPageEnterKey: TfmPropertyPageEnterKey
  HelpType = htKeyword
  HelpKeyword = 'EnterKeyOptions'
  Caption = 'Enter Key Options'
  PixelsPerInch = 96
  TextHeight = 13
  object Label8: TLabel [0]
    Left = 12
    Top = 60
    Width = 131
    Height = 13
    Caption = 'In Subscribed Groups Tree:'
  end
  object Label9: TLabel [1]
    Left = 12
    Top = 144
    Width = 84
    Height = 13
    Caption = 'In Message Tree:'
  end
  inherited Panel1: TPanel
    ExplicitWidth = 267
    DesignSize = (
      384
      41)
    inherited Bevel1: TBevel
      ExplicitWidth = 267
    end
    inherited stSectionDetails: TLabel
      Caption = 
        'This section controls how XanaNews responds when you press the '#39 +
        'Enter'#39' key.'
      ExplicitWidth = 255
    end
  end
  object rbEnterGetMessages: TRadioButton
    Left = 28
    Top = 80
    Width = 97
    Height = 17
    Caption = 'Get &Messages'
    TabOrder = 1
    OnClick = ControlClick
  end
  object rbEnterGoToMessageTree: TRadioButton
    Left = 28
    Top = 104
    Width = 145
    Height = 17
    Caption = 'Go to Message &Tree'
    TabOrder = 2
    OnClick = ControlClick
  end
  object cbEnterGoToNextGroup: TCheckBox
    Left = 28
    Top = 168
    Width = 249
    Height = 17
    Caption = 'Go to &next group if no more unread messages'
    TabOrder = 3
    OnClick = ControlClick
  end
  object cbEnterLoadsMessage: TCheckBox
    Left = 28
    Top = 192
    Width = 141
    Height = 17
    Caption = '&Retrieve message body'
    TabOrder = 4
    OnClick = ControlClick
  end
end
