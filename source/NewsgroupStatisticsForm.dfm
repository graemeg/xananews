object fmNewsgroupStatistics: TfmNewsgroupStatistics
  Left = 258
  Top = 115
  AutoScroll = False
  Caption = 'Newsgroup Statistics'
  ClientHeight = 342
  ClientWidth = 390
  Color = clBtnFace
  Constraints.MinHeight = 369
  Constraints.MinWidth = 398
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    390
    342)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 288
    Width = 393
    Height = 3
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsBottomLine
  end
  object Label6: TLabel
    Left = 16
    Top = 260
    Width = 23
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '&From'
  end
  object Label7: TLabel
    Left = 168
    Top = 260
    Width = 16
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'To:'
  end
  object btnClose: TButton
    Left = 303
    Top = 303
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
    Width = 390
    Height = 136
    ActivePage = tsThreads
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object tsThreads: TTabSheet
      Caption = 'Threads'
      object lvThreads: TListView
        Left = 0
        Top = 0
        Width = 382
        Height = 108
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
        OnData = lvThreadsData
      end
    end
    object tsPosters: TTabSheet
      Caption = 'Posters'
      ImageIndex = 1
      object lvPosters: TListView
        Left = 0
        Top = 0
        Width = 478
        Height = 188
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
            Caption = 'Last Reader'
            Width = 150
          end>
        GridLines = True
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = lvPostersColumnClick
        OnData = lvPostersData
      end
    end
    object tsNewsreaders: TTabSheet
      Caption = 'Newsreaders'
      ImageIndex = 2
      object lvNewsreaders: TListView
        Left = 0
        Top = 0
        Width = 478
        Height = 188
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
        OnData = lvNewsreadersData
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 390
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
    Top = 304
    Width = 113
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Report to Clipboard'
    TabOrder = 3
    OnClick = btnCopyToClipboardClick
  end
  object btnStart: TButton
    Left = 304
    Top = 256
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Start'
    TabOrder = 4
    OnClick = btnStartClick
  end
  object dtpFrom: TDateTimePicker
    Left = 48
    Top = 256
    Width = 105
    Height = 21
    Anchors = [akLeft, akBottom]
    Date = 38078.585294594910000000
    Time = 38078.585294594910000000
    TabOrder = 5
  end
  object dtpTo: TDateTimePicker
    Left = 192
    Top = 256
    Width = 105
    Height = 21
    Anchors = [akLeft, akBottom]
    Date = 38078.585294594910000000
    Time = 38078.585294594910000000
    TabOrder = 6
  end
end
