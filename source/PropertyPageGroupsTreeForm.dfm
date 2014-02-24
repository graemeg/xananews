inherited fmPropertyPageGroupsTree: TfmPropertyPageGroupsTree
  HelpType = htKeyword
  HelpKeyword = 'GroupsTreeOptions'
  Caption = 'Groups Tree'
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [0]
    Left = 12
    Top = 124
    Width = 176
    Height = 13
    Caption = 'Show groups with unread messages:'
  end
  object Label2: TLabel [1]
    Left = 12
    Top = 180
    Width = 91
    Height = 13
    Caption = 'Trim Group Names:'
  end
  object Label3: TLabel [2]
    Left = 12
    Top = 240
    Width = 190
    Height = 13
    Caption = 'Show Markers for Interesting Messages'
  end
  inherited Panel1: TPanel
    ExplicitWidth = 327
    inherited Bevel1: TBevel
      ExplicitWidth = 327
    end
    inherited stSectionDetails: TLabel
      Caption = 
        'This section controls the Subscribed Groups tree is displayed.  ' +
        'Additional font & colour options are in the Colours & Fonts sect' +
        'ion'
      ExplicitWidth = 315
    end
  end
  object cbShowMessageCount: TCheckBox
    Left = 12
    Top = 60
    Width = 141
    Height = 17
    Caption = 'Show Message &Count'
    TabOrder = 1
    OnClick = ControlClick
  end
  object cbAutoExpandGroupTree: TCheckBox
    Left = 12
    Top = 88
    Width = 141
    Height = 17
    Caption = 'Auto E&xpand Tree'
    TabOrder = 2
    OnClick = ControlClick
  end
  object cbUnreadNewsgroupsBold: TCheckBox
    Left = 28
    Top = 146
    Width = 55
    Height = 17
    Caption = '&Bold'
    TabOrder = 4
    OnClick = ControlClick
  end
  object cbUnreadNewsgroupsItalic: TCheckBox
    Left = 116
    Top = 146
    Width = 55
    Height = 17
    Caption = '&Italic'
    TabOrder = 5
    OnClick = ControlClick
  end
  object cbUnreadNewsgroupsUnderline: TCheckBox
    Left = 196
    Top = 146
    Width = 79
    Height = 17
    Caption = '&Underline'
    TabOrder = 6
    OnClick = ControlClick
  end
  object rbTrimNone: TRadioButton
    Left = 28
    Top = 204
    Width = 69
    Height = 17
    Caption = '&Don'#39't Trim'
    TabOrder = 7
    OnClick = ControlClick
  end
  object rbTrimRelaxed: TRadioButton
    Left = 116
    Top = 204
    Width = 61
    Height = 17
    Caption = '&Relaxed'
    TabOrder = 8
    OnClick = ControlClick
  end
  object rbTrimAggressive: TRadioButton
    Left = 196
    Top = 204
    Width = 85
    Height = 17
    Caption = '&Aggressive'
    TabOrder = 9
    OnClick = ControlClick
  end
  object Panel2: TPanel
    Left = 16
    Top = 256
    Width = 289
    Height = 33
    BevelOuter = bvNone
    TabOrder = 10
    object rbMarkersNone: TRadioButton
      Left = 12
      Top = 8
      Width = 79
      Height = 17
      Caption = 'Don'#39'&t Show'
      TabOrder = 0
      OnClick = ControlClick
    end
    object rbMarkersUnread: TRadioButton
      Left = 100
      Top = 8
      Width = 61
      Height = 17
      Caption = 'Unrea&d'
      TabOrder = 1
      OnClick = ControlClick
    end
    object rbMarkersAll: TRadioButton
      Left = 180
      Top = 8
      Width = 45
      Height = 17
      Caption = 'A&ll'
      TabOrder = 2
      OnClick = ControlClick
    end
  end
  object cbAutoContractGroupTree: TCheckBox
    Left = 168
    Top = 88
    Width = 129
    Height = 17
    Caption = 'Auto Co&ntract Tree'
    TabOrder = 3
    OnClick = ControlClick
  end
end
