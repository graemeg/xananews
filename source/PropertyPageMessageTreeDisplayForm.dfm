inherited fmPropertyPageMessageTreeDisplay: TfmPropertyPageMessageTreeDisplay
  Left = 307
  Top = 232
  HelpType = htKeyword
  HelpKeyword = 'MessageTreeOptions'
  ActiveControl = cbUnreadMessagesBold
  Caption = 'Message Tree'
  ClientHeight = 308
  ClientWidth = 371
  Constraints.MinHeight = 303
  Constraints.MinWidth = 371
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel [0]
    Left = 15
    Top = 60
    Width = 119
    Height = 13
    Caption = '&Show Unread Messages:'
    FocusControl = cbUnreadMessagesBold
  end
  object Label2: TLabel [1]
    Left = 15
    Top = 216
    Width = 125
    Height = 13
    AutoSize = False
    Caption = '&Tree Column'
    FocusControl = cbTreeColumn
  end
  object Label1: TLabel [2]
    Left = 15
    Top = 248
    Width = 65
    Height = 13
    Caption = 'Hide Columns'
  end
  inherited Panel1: TPanel
    Width = 371
    inherited Bevel1: TBevel
      Width = 371
    end
    inherited stSectionDetails: TLabel
      Width = 359
      Caption = 'These options affect how the message tree is displayed'
    end
  end
  object cbUnreadMessagesBold: TCheckBox
    Left = 148
    Top = 58
    Width = 55
    Height = 17
    Caption = 'Bold'
    TabOrder = 1
    OnClick = ControlClick
  end
  object cbUnreadMessagesItalic: TCheckBox
    Left = 220
    Top = 58
    Width = 55
    Height = 17
    Caption = 'Italic'
    TabOrder = 2
    OnClick = ControlClick
  end
  object cbUnreadMessagesUnderline: TCheckBox
    Left = 295
    Top = 58
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
    Top = 160
    Width = 297
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Don'#39't Highlight Other XanaNews Users'#39' Messages'
    TabOrder = 8
    OnClick = ControlClick
  end
  object cbTreeColumn: TComboBox
    Left = 147
    Top = 214
    Width = 164
    Height = 21
    ItemHeight = 13
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
    Top = 248
    Width = 61
    Height = 17
    Caption = 'Author'
    TabOrder = 12
    OnClick = ControlClick
  end
  object cbHideFlags: TCheckBox
    Left = 148
    Top = 248
    Width = 49
    Height = 17
    Caption = 'Flags'
    TabOrder = 11
    OnClick = ControlClick
  end
  object cbHideNumber: TCheckBox
    Left = 295
    Top = 248
    Width = 66
    Height = 17
    Caption = 'Number'
    TabOrder = 13
    OnClick = ControlClick
  end
  object cbHideDate: TCheckBox
    Left = 220
    Top = 272
    Width = 57
    Height = 17
    Caption = 'Date'
    TabOrder = 15
    OnClick = ControlClick
  end
  object cbHideLines: TCheckBox
    Left = 295
    Top = 272
    Width = 58
    Height = 17
    Caption = 'Lines'
    TabOrder = 16
    OnClick = ControlClick
  end
  object cbHideSubject: TCheckBox
    Left = 148
    Top = 272
    Width = 57
    Height = 17
    Caption = 'Subject'
    TabOrder = 14
    OnClick = ControlClick
  end
  object cbHighlightText: TCheckBox
    Left = 12
    Top = 186
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
