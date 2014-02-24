object dlgGetMessages: TdlgGetMessages
  Left = 65
  Top = 200
  HelpType = htKeyword
  HelpKeyword = 'Download Messages'
  BorderStyle = bsDialog
  Caption = 'Get Messages'
  ClientHeight = 353
  ClientWidth = 330
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    330
    353)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 163
    Top = 311
    Width = 75
    Height = 24
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 242
    Top = 311
    Width = 77
    Height = 24
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 16
    Top = 16
    Width = 301
    Height = 105
    Anchors = [akLeft, akTop, akRight]
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object Panel2: TPanel
      Left = 2
      Top = 2
      Width = 297
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object rbMarkAsRead: TRadioButton
        Left = 16
        Top = 16
        Width = 113
        Height = 17
        Caption = '&Mark as Read'
        TabOrder = 0
      end
      object rbDelete: TRadioButton
        Left = 168
        Top = 16
        Width = 113
        Height = 17
        Caption = '&Delete'
        TabOrder = 1
      end
    end
    object Panel3: TPanel
      Left = 2
      Top = 43
      Width = 297
      Height = 60
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object Bevel1: TBevel
        Left = 0
        Top = 0
        Width = 297
        Height = 3
        Align = alTop
        Shape = bsTopLine
      end
      object stOld: TLabel
        Left = 264
        Top = 33
        Width = 16
        Height = 13
        Caption = 'Old'
      end
      object rbAllMessages: TRadioButton
        Left = 16
        Top = 8
        Width = 113
        Height = 17
        Caption = '&All Messages'
        TabOrder = 0
      end
      object rbMessagesMoreThan: TRadioButton
        Left = 16
        Top = 33
        Width = 121
        Height = 16
        Caption = 'Me&ssages more than'
        TabOrder = 1
      end
      object edMoreThan: TEdit
        Left = 144
        Top = 28
        Width = 33
        Height = 21
        TabOrder = 2
        Text = '1'
      end
      object udMoreThan: TUpDown
        Left = 177
        Top = 28
        Width = 15
        Height = 23
        Associate = edMoreThan
        Min = 1
        Position = 1
        TabOrder = 3
      end
      object cbDaysWeeksMonths: TComboBox
        Left = 200
        Top = 28
        Width = 57
        Height = 22
        Style = csOwnerDrawFixed
        ItemIndex = 0
        TabOrder = 4
        Text = 'Days'
        Items.Strings = (
          'Days'
          'Weeks'
          'Months')
      end
    end
  end
  object Panel4: TPanel
    Left = 16
    Top = 137
    Width = 301
    Height = 169
    Anchors = [akLeft, akTop, akRight]
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 3
    object stNextMessages: TLabel
      Left = 130
      Top = 78
      Width = 71
      Height = 13
      Caption = 'New Messages'
    end
    object stLastMessages: TLabel
      Left = 130
      Top = 106
      Width = 97
      Height = 13
      Caption = 'Messages on Server'
    end
    object rbGetAllNewMessages: TRadioButton
      Left = 21
      Top = 20
      Width = 241
      Height = 13
      Caption = 'Get &All New Messages'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbGetAllNewMessagesClick
    end
    object rbGetNextMessages: TRadioButton
      Left = 21
      Top = 78
      Width = 68
      Height = 13
      Caption = 'Get &Next'
      TabOrder = 1
      OnClick = rbGetNextMessagesClick
    end
    object edGetNextMessages: TEdit
      Left = 91
      Top = 74
      Width = 33
      Height = 21
      Enabled = False
      TabOrder = 2
      Text = '300'
    end
    object rbGetAllMessages: TRadioButton
      Left = 21
      Top = 48
      Width = 276
      Height = 13
      Caption = 'Get All &Messages'
      TabOrder = 3
      OnClick = rbGetAllMessagesClick
    end
    object rbGetLastMessages: TRadioButton
      Left = 21
      Top = 106
      Width = 68
      Height = 13
      Caption = 'Get &Last'
      TabOrder = 4
      OnClick = rbGetLastMessagesClick
    end
    object edGetLastMessages: TEdit
      Left = 91
      Top = 102
      Width = 33
      Height = 21
      Enabled = False
      TabOrder = 5
      Text = '300'
    end
    object cbGetHeadersOnly: TCheckBox
      Left = 13
      Top = 140
      Width = 118
      Height = 13
      Caption = 'Get &Headers Only'
      TabOrder = 6
    end
  end
  object cbNext: TCheckBox
    Left = 24
    Top = 128
    Width = 97
    Height = 17
    Caption = '&Get Messages'
    TabOrder = 4
    OnClick = cbNextClick
  end
  object cbFirst: TCheckBox
    Left = 24
    Top = 8
    Width = 113
    Height = 17
    Caption = '&Tidy old messages'
    TabOrder = 5
    OnClick = cbFirstClick
  end
end
