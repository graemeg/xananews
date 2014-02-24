inherited fmPropertyPageMessageTreeDisplay: TfmPropertyPageMessageTreeDisplay
  Left = 307
  Top = 232
  HelpType = htKeyword
  HelpKeyword = 'MessageTreeOptions'
  ActiveControl = cbUnreadMessagesBold
  Caption = 'Message Tree'
  ExplicitLeft = 307
  ExplicitTop = 232
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel [0]
    Left = 12
    Top = 60
    Width = 118
    Height = 13
    Caption = '&Show Unread Messages:'
    FocusControl = cbUnreadMessagesBold
  end
  object Label2: TLabel [1]
    Left = 12
    Top = 206
    Width = 125
    Height = 13
    AutoSize = False
    Caption = '&Tree Column'
    FocusControl = cbTreeColumn
  end
  object Label1: TLabel [2]
    Left = 12
    Top = 230
    Width = 64
    Height = 13
    Caption = 'Hide Columns'
  end
  inherited Panel1: TPanel
    ExplicitWidth = 371
    inherited Bevel1: TBevel
      ExplicitWidth = 371
    end
    inherited stSectionDetails: TLabel
      Caption = 'These options affect how the message tree is displayed'
      ExplicitWidth = 359
    end
  end
  object cbUnreadMessagesBold: TCheckBox
    Left = 148
    Top = 59
    Width = 55
    Height = 17
    Caption = 'Bold'
    TabOrder = 1
    OnClick = ControlClick
  end
  object cbUnreadMessagesItalic: TCheckBox
    Left = 220
    Top = 59
    Width = 55
    Height = 17
    Caption = 'Italic'
    TabOrder = 2
    OnClick = ControlClick
  end
  object cbUnreadMessagesUnderline: TCheckBox
    Left = 295
    Top = 59
    Width = 69
    Height = 17
    Caption = 'Underline'
    TabOrder = 3
    OnClick = ControlClick
  end
  object cbFirstLineAsSubject: TCheckBox
    Left = 12
    Top = 84
    Width = 149
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Show &First Line as Subject'
    TabOrder = 4
    OnClick = ControlClick
  end
  object cbHideReadMessages: TCheckBox
    Left = 12
    Top = 132
    Width = 149
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Hide &Read Messages'
    TabOrder = 6
    OnClick = ControlClick
  end
  object cbHideFolderIcons: TCheckBox
    Left = 12
    Top = 108
    Width = 149
    Height = 17
    Alignment = taLeftJustify
    Caption = 'H&ide Folder Icons'
    TabOrder = 5
    OnClick = ControlClick
  end
  object cbDontHighlightXanaNewsUsers: TCheckBox
    Left = 12
    Top = 156
    Width = 297
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Don'#39't Highlight Other XanaNews Users'#39' Messages'
    TabOrder = 8
    OnClick = ControlClick
  end
  object cbTreeColumn: TComboBox
    Left = 148
    Top = 204
    Width = 164
    Height = 21
    TabOrder = 10
    Text = 'Number'
    OnChange = cbTreeColumnChange
    Items.Strings = (
      'Flags'
      'Number'
      'Subject'
      'Author'
      'Date'
      'Lines')
  end
  object cbHideAuthor: TCheckBox
    Left = 220
    Top = 230
    Width = 61
    Height = 17
    Caption = 'Author'
    TabOrder = 12
    OnClick = ControlClick
  end
  object cbHideFlags: TCheckBox
    Left = 148
    Top = 230
    Width = 49
    Height = 17
    Caption = 'Flags'
    TabOrder = 11
    OnClick = ControlClick
  end
  object cbHideNumber: TCheckBox
    Left = 295
    Top = 230
    Width = 66
    Height = 17
    Caption = 'Number'
    TabOrder = 13
    OnClick = ControlClick
  end
  object cbHideDate: TCheckBox
    Left = 220
    Top = 254
    Width = 57
    Height = 17
    Caption = 'Date'
    TabOrder = 15
    OnClick = ControlClick
  end
  object cbHideLines: TCheckBox
    Left = 295
    Top = 254
    Width = 58
    Height = 17
    Caption = 'Lines'
    TabOrder = 16
    OnClick = ControlClick
  end
  object cbHideSubject: TCheckBox
    Left = 148
    Top = 254
    Width = 57
    Height = 17
    Caption = 'Subject'
    TabOrder = 14
    OnClick = ControlClick
  end
  object cbHighlightText: TCheckBox
    Left = 12
    Top = 180
    Width = 297
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Use Highlight Colour for Selected Text'
    TabOrder = 9
    OnClick = ControlClick
  end
  object cbHideIgnoredMessages: TCheckBox
    Left = 174
    Top = 132
    Width = 135
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Hide I&gnored Messages'
    TabOrder = 7
    OnClick = ControlClick
  end
end
