object dlgSearch: TdlgSearch
  Left = 177
  Top = 178
  ActiveControl = cbSubject
  Caption = 'Search Messages'
  ClientHeight = 358
  ClientWidth = 353
  Color = clBtnFace
  Constraints.MinHeight = 385
  Constraints.MinWidth = 361
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    353
    358)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 160
    Top = 302
    Width = 58
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Max Results'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 240
    Width = 333
    Height = 3
    Anchors = [akLeft, akTop, akRight]
    Shape = bsBottomLine
  end
  object OKBtn: TButton
    Left = 268
    Top = 288
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Find Next...'
    Default = True
    TabOrder = 29
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 268
    Top = 320
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 30
    OnClick = CancelBtnClick
  end
  object cbUnreadMessagesOnly: TCheckBox
    Left = 16
    Top = 252
    Width = 137
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '&Unread Messages Only'
    TabOrder = 24
  end
  object cbSearchToBookmark: TCheckBox
    Left = 16
    Top = 300
    Width = 121
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Search to bookmark'
    TabOrder = 26
  end
  object cbCaseSensitive: TCheckBox
    Left = 16
    Top = 325
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Match &Case'
    TabOrder = 27
  end
  object edMaxResults: TEdit
    Left = 224
    Top = 299
    Width = 33
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 28
    Text = '200'
  end
  object cbSubject: TCheckBox
    Left = 12
    Top = 16
    Width = 97
    Height = 17
    Caption = '&Subject'
    TabOrder = 0
    OnClick = cbClick
  end
  object cbCrossposted: TCheckBox
    Tag = 7
    Left = 12
    Top = 212
    Width = 97
    Height = 17
    Caption = 'Crossposted'
    TabOrder = 21
    OnClick = cbClick
  end
  object cbHeaderLines: TCheckBox
    Tag = 6
    Left = 12
    Top = 184
    Width = 97
    Height = 17
    Caption = '&Header Lines'
    TabOrder = 18
    OnClick = cbClick
  end
  object cbMessageID: TCheckBox
    Tag = 5
    Left = 12
    Top = 156
    Width = 97
    Height = 17
    Caption = 'Message &ID'
    TabOrder = 15
    OnClick = cbClick
  end
  object cbMessageBody: TCheckBox
    Tag = 4
    Left = 12
    Top = 128
    Width = 97
    Height = 17
    Caption = '&Message Body'
    TabOrder = 12
    OnClick = cbClick
  end
  object cbLines: TCheckBox
    Tag = 3
    Left = 12
    Top = 100
    Width = 97
    Height = 17
    Caption = '&Lines'
    TabOrder = 9
    OnClick = cbClick
  end
  object cbDate: TCheckBox
    Tag = 2
    Left = 12
    Top = 72
    Width = 97
    Height = 17
    Caption = '&Date'
    TabOrder = 6
    OnClick = cbClick
  end
  object cbAuthor: TCheckBox
    Tag = 1
    Left = 12
    Top = 44
    Width = 97
    Height = 17
    Caption = '&Author'
    TabOrder = 3
    OnClick = cbClick
  end
  object cbxSubject: TComboBox
    Left = 120
    Top = 14
    Width = 100
    Height = 22
    Style = csOwnerDrawFixed
    Enabled = False
    TabOrder = 1
    OnChange = cbxChange
  end
  object cbxAuthor: TComboBox
    Left = 120
    Top = 42
    Width = 100
    Height = 22
    Style = csOwnerDrawFixed
    Enabled = False
    TabOrder = 4
    OnChange = cbxChange
  end
  object cbxDate: TComboBox
    Left = 120
    Top = 70
    Width = 100
    Height = 22
    Style = csOwnerDrawFixed
    Enabled = False
    TabOrder = 7
    OnChange = cbxChange
  end
  object cbxLines: TComboBox
    Left = 120
    Top = 98
    Width = 100
    Height = 22
    Style = csOwnerDrawFixed
    Enabled = False
    TabOrder = 10
    OnChange = cbxChange
  end
  object cbxMessageBody: TComboBox
    Left = 120
    Top = 126
    Width = 100
    Height = 22
    Style = csOwnerDrawFixed
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 13
    OnChange = cbxChange
  end
  object cbxMessageID: TComboBox
    Left = 120
    Top = 154
    Width = 100
    Height = 22
    Style = csOwnerDrawFixed
    Enabled = False
    TabOrder = 16
    OnChange = cbxChange
  end
  object cbxHeaderLines: TComboBox
    Left = 120
    Top = 182
    Width = 100
    Height = 22
    Style = csOwnerDrawFixed
    Enabled = False
    TabOrder = 19
    OnChange = cbxChange
  end
  object cbxCrossposted: TComboBox
    Left = 120
    Top = 210
    Width = 100
    Height = 22
    Style = csOwnerDrawFixed
    Enabled = False
    TabOrder = 22
    OnChange = cbxChange
  end
  object DateTimePicker1: TDateTimePicker
    Left = 224
    Top = 70
    Width = 117
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Date = 38309.000000000000000000
    Time = 38309.000000000000000000
    Enabled = False
    TabOrder = 8
    OnChange = DateTimePicker1Change
  end
  object edSubject: TEdit
    Left = 224
    Top = 14
    Width = 117
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    TabOrder = 2
    OnChange = edChange
  end
  object edAuthor: TEdit
    Left = 224
    Top = 42
    Width = 117
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    TabOrder = 5
    OnChange = edChange
  end
  object edLines: TEdit
    Left = 224
    Top = 98
    Width = 117
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    TabOrder = 11
    OnChange = edChange
  end
  object edMessageBody: TEdit
    Left = 224
    Top = 126
    Width = 117
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    TabOrder = 14
    OnChange = edChange
  end
  object edMessageID: TEdit
    Left = 224
    Top = 154
    Width = 117
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    TabOrder = 17
    OnChange = edChange
  end
  object edHeaderLines: TEdit
    Left = 224
    Top = 182
    Width = 117
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    TabOrder = 20
    OnChange = edChange
  end
  object edCrossposted: TEdit
    Left = 224
    Top = 210
    Width = 117
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    TabOrder = 23
    OnChange = edChange
  end
  object cbInterestingMessagesOnly: TCheckBox
    Left = 16
    Top = 277
    Width = 153
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Interesting Messages Only'
    TabOrder = 25
  end
  object PersistentPosition1: TPersistentPosition
    Manufacturer = 'Woozle'
    Product = 'XanaNews'
    SubKey = 'Position\Search'
    OnGetSettingsClass = PersistentPosition1GetSettingsClass
    OnGetSettingsFile = PersistentPosition1GetSettingsFile
    Left = 312
    Top = 248
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 300
    OnTimer = Timer1Timer
    Left = 184
    Top = 312
  end
end
