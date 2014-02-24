inherited fmPropertyPageMessageTreeActions: TfmPropertyPageMessageTreeActions
  Left = 212
  Top = 180
  HelpType = htKeyword
  HelpKeyword = 'MessageTreeActions'
  Caption = 'Actions in Message Tree'
  ExplicitLeft = 212
  ExplicitTop = 180
  PixelsPerInch = 96
  TextHeight = 13
  object Label14: TLabel [0]
    Left = 22
    Top = 178
    Width = 24
    Height = 13
    Caption = 'after'
  end
  object Label30: TLabel [1]
    Left = 99
    Top = 178
    Width = 61
    Height = 13
    Caption = 'half-seconds'
  end
  inherited Panel1: TPanel
    ExplicitWidth = 332
    inherited Bevel1: TBevel
      ExplicitWidth = 332
    end
    inherited stSectionDetails: TLabel
      Caption = 
        'These options affect how the message tree behaves when messages ' +
        'are clicked'
      ExplicitWidth = 320
    end
  end
  object cbAutoExpandThread: TCheckBox
    Left = 12
    Top = 60
    Width = 201
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Auto &Expand Thread'
    TabOrder = 1
    OnClick = ControlClick
  end
  object cbAutoCentralizeMessage: TCheckBox
    Left = 12
    Top = 84
    Width = 201
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Auto &Centralize Message'
    TabOrder = 2
    OnClick = ControlClick
  end
  object cbAutoExpandAll: TCheckBox
    Left = 12
    Top = 108
    Width = 201
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Auto E&xpand when Changing Groups'
    TabOrder = 3
    OnClick = ControlClick
  end
  object cbAutoDownloadOnClick: TCheckBox
    Left = 12
    Top = 132
    Width = 201
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Auto Download on Clic&k'
    TabOrder = 4
    OnClick = ControlClick
  end
  object cbAutoMarkAsRead: TCheckBox
    Left = 12
    Top = 156
    Width = 201
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Auto &Mark As Read'
    TabOrder = 5
    OnClick = ControlClick
  end
  object edAutoMarkSeconds: TEdit
    Left = 52
    Top = 175
    Width = 41
    Height = 21
    TabOrder = 6
    Text = '0'
    OnChange = ControlClick
  end
end
