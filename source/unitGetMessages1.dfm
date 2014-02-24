object dlgGetMessages1: TdlgGetMessages1
  Left = 130
  Top = 161
  HelpType = htKeyword
  HelpKeyword = 'GetMessages'
  BorderStyle = bsDialog
  Caption = 'Get Messages'
  ClientHeight = 282
  ClientWidth = 381
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object stNextMessages: TLabel
    Left = 138
    Top = 108
    Width = 71
    Height = 13
    Caption = 'New Messages'
  end
  object stLastMessages: TLabel
    Left = 138
    Top = 132
    Width = 97
    Height = 13
    Caption = 'Messages on Server'
  end
  object stTo: TLabel
    Left = 226
    Top = 156
    Width = 12
    Height = 13
    Caption = 'To'
  end
  object Bevel1: TBevel
    Left = 16
    Top = 8
    Width = 353
    Height = 217
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 32
    Top = 208
    Width = 137
    Height = 13
    Caption = '(*) Delete old messages first'
  end
  object rbGetAllNewMessages: TRadioButton
    Left = 29
    Top = 36
    Width = 241
    Height = 13
    Caption = '   Get &All New Messages'
    Checked = True
    TabOrder = 0
    TabStop = True
    OnClick = rbGetAllNewMessagesClick
  end
  object rbGetAllMessages: TRadioButton
    Left = 29
    Top = 60
    Width = 276
    Height = 13
    Caption = '* Get All &Messages'
    TabOrder = 1
    OnClick = rbGetAllMessagesClick
  end
  object rbGetNextMessages: TRadioButton
    Left = 29
    Top = 108
    Width = 68
    Height = 13
    Caption = '   Get &Next'
    TabOrder = 2
    OnClick = rbGetNextMessagesClick
  end
  object edGetNextMessages: TEdit
    Left = 99
    Top = 104
    Width = 33
    Height = 21
    Enabled = False
    TabOrder = 3
    Text = '300'
  end
  object rbGetLastMessages: TRadioButton
    Left = 29
    Top = 132
    Width = 68
    Height = 13
    Caption = '* Get &Last'
    TabOrder = 4
    OnClick = rbGetLastMessagesClick
  end
  object edGetLastMessages: TEdit
    Left = 99
    Top = 128
    Width = 33
    Height = 21
    Enabled = False
    TabOrder = 5
    Text = '300'
  end
  object edAddFrom: TEdit
    Left = 162
    Top = 152
    Width = 55
    Height = 21
    Enabled = False
    TabOrder = 6
    Text = '1'
  end
  object edAddTo: TEdit
    Left = 249
    Top = 151
    Width = 55
    Height = 21
    Enabled = False
    TabOrder = 7
    Text = '1'
  end
  object rbAddFrom: TRadioButton
    Left = 29
    Top = 156
    Width = 124
    Height = 17
    Caption = '   A&dd Messages From '
    TabOrder = 8
    OnClick = rbAddFromClick
  end
  object cbGetHeadersOnly: TCheckBox
    Left = 13
    Top = 252
    Width = 118
    Height = 13
    Caption = 'Get &Headers Only'
    TabOrder = 9
  end
  object rbGetOnlyNewMessages: TRadioButton
    Left = 29
    Top = 84
    Width = 276
    Height = 13
    Caption = '* Get &Only New Messages'
    TabOrder = 10
    OnClick = rbGetAllMessagesClick
  end
  object rbAddSince: TRadioButton
    Left = 29
    Top = 180
    Width = 124
    Height = 17
    Caption = '* Get Messages &Since'
    TabOrder = 11
    OnClick = rbAddSinceClick
  end
  object dtpSince: TDateTimePicker
    Left = 160
    Top = 178
    Width = 153
    Height = 21
    Date = 38285.586842442120000000
    Time = 38285.586842442120000000
    Enabled = False
    TabOrder = 12
  end
  object btnOK: TButton
    Left = 132
    Top = 248
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 13
  end
  object btnCancel: TButton
    Left = 214
    Top = 248
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 14
  end
  object btnHelp: TButton
    Left = 296
    Top = 248
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 15
    OnClick = btnHelpClick
  end
end
