inherited fmPropertyPageSorting: TfmPropertyPageSorting
  Left = 527
  Top = 154
  HelpType = htKeyword
  HelpKeyword = 'SortingThreading'
  Caption = 'Sorting & Threading'
  ClientWidth = 384
  ExplicitLeft = 527
  ExplicitTop = 154
  PixelsPerInch = 96
  TextHeight = 13
  inherited Bevel2: TBevel
    Width = 384
    ExplicitTop = 241
    ExplicitWidth = 307
  end
  object Label10: TLabel [1]
    Left = 12
    Top = 102
    Width = 88
    Height = 13
    Caption = 'Display threads in '
  end
  object Label9: TLabel [2]
    Left = 12
    Top = 60
    Width = 88
    Height = 13
    Caption = 'Display messages:'
  end
  object Label1: TLabel [3]
    Left = 272
    Top = 104
    Width = 28
    Height = 13
    Caption = '&Order'
    FocusControl = cbSortOrder
  end
  inherited Panel1: TPanel
    Width = 384
    ExplicitWidth = 307
    inherited Bevel1: TBevel
      Width = 384
      ExplicitWidth = 307
    end
    inherited stSectionDetails: TLabel
      Width = 372
      ExplicitWidth = 295
    end
  end
  inherited btnReset: TButton
    Left = 273
    TabOrder = 6
    ExplicitLeft = 196
    ExplicitTop = 250
  end
  object cbSortOrder: TComboBox
    Left = 121
    Top = 100
    Width = 144
    Height = 21
    TabOrder = 3
    Text = 'Date'
    OnChange = ControlClick
    Items.Strings = (
      'Date'
      'Subject'
      'Author'
      'Lines'
      'Number'
      'Date of Newest Message')
  end
  object cbGroupSubjects: TCheckBox
    Left = 10
    Top = 168
    Width = 271
    Height = 25
    Caption = '&Group posts using Subjects'
    TabOrder = 5
    OnClick = ControlClick
  end
  object rbThreaded: TRadioButton
    Left = 120
    Top = 60
    Width = 81
    Height = 17
    Caption = '&Threaded'
    TabOrder = 1
    OnClick = ControlClick
  end
  object rbChronological: TRadioButton
    Left = 208
    Top = 60
    Width = 113
    Height = 17
    Caption = '&Chronological'
    TabOrder = 2
    OnClick = ControlClick
  end
  object Panel2: TPanel
    Left = 112
    Top = 128
    Width = 185
    Height = 33
    BevelOuter = bvNone
    TabOrder = 4
    object rbDescending: TRadioButton
      Left = 98
      Top = 8
      Width = 79
      Height = 17
      Caption = '&Descending'
      TabOrder = 1
      OnClick = ControlClick
    end
    object rbAscending: TRadioButton
      Left = 10
      Top = 8
      Width = 73
      Height = 17
      Caption = '&Ascending'
      TabOrder = 0
      OnClick = ControlClick
    end
  end
end
