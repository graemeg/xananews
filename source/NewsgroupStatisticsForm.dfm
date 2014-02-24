object fmNewsgroupStatistics: TfmNewsgroupStatistics
  Left = 469
  Top = 115
  Caption = 'Newsgroup Statistics'
  ClientHeight = 422
  ClientWidth = 445
  Color = clBtnFace
  Constraints.MinHeight = 369
  Constraints.MinWidth = 398
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    445
    422)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 368
    Width = 448
    Height = 3
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsBottomLine
    ExplicitTop = 288
    ExplicitWidth = 393
  end
  object Label6: TLabel
    Left = 12
    Top = 314
    Width = 26
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '&From:'
    ExplicitTop = 333
  end
  object Label7: TLabel
    Left = 182
    Top = 314
    Width = 16
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'To:'
    ExplicitTop = 333
  end
  object lblResult: TLabel
    Left = 12
    Top = 343
    Width = 38
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '&Results:'
    ExplicitTop = 362
  end
  object btnClose: TButton
    Left = 358
    Top = 383
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 0
  end
  object pcSelect: TPageControl
    Left = 0
    Top = 105
    Width = 445
    Height = 193
    ActivePage = tsThreads
    Anchors = [akLeft, akTop, akRight, akBottom]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    object tsThreads: TTabSheet
      Caption = 'Threads'
      object lvThreads: TListView
        Left = 0
        Top = 0
        Width = 437
        Height = 165
        Align = alClient
        Columns = <
          item
            Caption = 'Ranking'
            Width = 60
          end
          item
            Caption = 'Articles'
          end
          item
            Caption = 'Description'
            Width = 300
          end>
        GridLines = True
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = lvThreadsColumnClick
        OnCustomDrawSubItem = lvCustomDrawSubItem
        OnData = lvThreadsData
      end
    end
    object tsPosters: TTabSheet
      Caption = 'Posters'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lvPosters: TListView
        Left = 0
        Top = 0
        Width = 437
        Height = 165
        Align = alClient
        Columns = <
          item
            Caption = 'Ranking'
            Width = 60
          end
          item
            Caption = 'Articles'
          end
          item
            Caption = 'Name'
            Width = 150
          end
          item
            Caption = 'Most Used Newsreader'
            Width = 150
          end>
        GridLines = True
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = lvPostersColumnClick
        OnCustomDrawSubItem = lvCustomDrawSubItem
        OnData = lvPostersData
        OnInfoTip = lvPostersInfoTip
      end
    end
    object tsNewsreaders: TTabSheet
      Caption = 'Newsreaders'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lvNewsreaders: TListView
        Left = 0
        Top = 0
        Width = 437
        Height = 165
        Align = alClient
        Columns = <
          item
            Caption = 'Ranking'
            Width = 60
          end
          item
            Caption = 'Articles'
          end
          item
            Caption = 'Reader'
            Width = 220
          end
          item
            Caption = 'Current Users'
            Width = 80
          end>
        GridLines = True
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = lvNewsreadersColumnClick
        OnCustomDrawSubItem = lvCustomDrawSubItem
        OnData = lvNewsreadersData
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 445
    Height = 105
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 86
      Height = 13
      Caption = 'Number of Articles'
    end
    object Label2: TLabel
      Left = 8
      Top = 32
      Width = 91
      Height = 13
      Caption = 'Number of Threads'
    end
    object Label3: TLabel
      Left = 8
      Top = 56
      Width = 141
      Height = 13
      Caption = 'Number of Unanswered Posts'
    end
    object Label4: TLabel
      Left = 8
      Top = 80
      Width = 89
      Height = 13
      Caption = 'Messagebase Size'
    end
    object stNoArticles: TLabel
      Left = 152
      Top = 8
      Width = 48
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '0'
    end
    object stNoThreads: TLabel
      Left = 152
      Top = 32
      Width = 48
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '0'
    end
    object stNoUnanswered: TLabel
      Left = 152
      Top = 56
      Width = 48
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '0'
    end
    object stMessagebaseSize: TLabel
      Left = 152
      Top = 80
      Width = 48
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '0'
    end
    object Label5: TLabel
      Left = 208
      Top = 80
      Width = 33
      Height = 13
      Caption = 'KBytes'
    end
  end
  object btnCopyToClipboard: TButton
    Left = 16
    Top = 383
    Width = 113
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Report to Clipboard'
    TabOrder = 3
    OnClick = btnCopyToClipboardClick
  end
  object btnStart: TButton
    Left = 358
    Top = 310
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Start'
    TabOrder = 8
    OnClick = btnStartClick
  end
  object dtpFrom: TDateTimePicker
    Left = 62
    Top = 310
    Width = 105
    Height = 21
    Anchors = [akLeft, akBottom]
    Date = 38078.000000000000000000
    Time = 38078.000000000000000000
    TabOrder = 5
  end
  object dtpTo: TDateTimePicker
    Left = 206
    Top = 310
    Width = 105
    Height = 21
    Anchors = [akLeft, akBottom]
    Date = 38078.999988425930000000
    Time = 38078.999988425930000000
    TabOrder = 6
  end
  object btnPostToGroup: TButton
    Left = 144
    Top = 383
    Width = 113
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Post to Group'
    TabOrder = 4
    OnClick = btnPostToGroupClick
  end
  object cbResults: TComboBox
    Left = 62
    Top = 339
    Width = 105
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 7
    Text = '[No limit]'
    OnKeyPress = cbResultsKeyPress
    Items.Strings = (
      '10'
      '20'
      '50'
      '100'
      '[No limit]')
  end
  object PersistentPosition1: TPersistentPosition
    Manufacturer = 'Woozle'
    Product = 'XanaNews'
    SubKey = 'Position\Statistics'
    OnGetSettingsClass = PersistentPosition1GetSettingsClass
    OnGetSettingsFile = PersistentPosition1GetSettingsFile
    Left = 362
    Top = 14
  end
end
