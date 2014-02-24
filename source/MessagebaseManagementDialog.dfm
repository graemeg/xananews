object dlgMessagebaseManagement: TdlgMessagebaseManagement
  Left = 255
  Top = 173
  BorderStyle = bsDialog
  Caption = 'Messagebase Management'
  ClientHeight = 461
  ClientWidth = 343
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
  object rgManagementAction: TRadioGroup
    Left = 16
    Top = 16
    Width = 313
    Height = 137
    ItemIndex = 1
    Items.Strings = (
      'Mark Articles As Read'
      'Delete Articles'
      'Mark Threads As Read'
      'Delete Threads')
    TabOrder = 0
  end
  object gbWhen: TGroupBox
    Left = 16
    Top = 280
    Width = 313
    Height = 129
    TabOrder = 1
    object rbMoreThanAWeek: TRadioButton
      Left = 8
      Top = 20
      Width = 169
      Height = 17
      Caption = 'More Than A Week Old'
      TabOrder = 0
      OnClick = rbOlderThanClick
    end
    object rbMoreThanAMonth: TRadioButton
      Left = 8
      Top = 48
      Width = 177
      Height = 17
      Caption = 'More Than A Month Old'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = rbOlderThanClick
    end
    object rbOlderThan: TRadioButton
      Left = 8
      Top = 76
      Width = 77
      Height = 17
      Caption = 'Older Than'
      TabOrder = 2
      OnClick = rbOlderThanClick
    end
    object DatePicker: TDateTimePicker
      Left = 96
      Top = 73
      Width = 186
      Height = 21
      Date = 37519.488231331020000000
      Time = 37519.488231331020000000
      Enabled = False
      TabOrder = 3
    end
    object TimePicker: TDateTimePicker
      Left = 96
      Top = 96
      Width = 186
      Height = 21
      Date = 38394.677761863420000000
      Time = 38394.677761863420000000
      ShowCheckbox = True
      DateMode = dmUpDown
      Enabled = False
      Kind = dtkTime
      TabOrder = 4
    end
  end
  object btnOK: TButton
    Left = 168
    Top = 424
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 252
    Top = 424
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 168
    Width = 313
    Height = 105
    TabOrder = 4
    object rbSelectedGroups: TRadioButton
      Left = 8
      Top = 16
      Width = 257
      Height = 17
      Caption = 'Selected Groups'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbAllGroupsInSelectedAccount: TRadioButton
      Left = 8
      Top = 48
      Width = 265
      Height = 17
      Caption = 'All Groups in Selected Account'
      TabOrder = 1
    end
    object rbAllGroups: TRadioButton
      Left = 8
      Top = 80
      Width = 241
      Height = 17
      Caption = 'All Groups'
      TabOrder = 2
    end
  end
end
