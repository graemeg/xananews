object dlgDeleteMessages: TdlgDeleteMessages
  Left = 628
  Top = 213
  BorderStyle = bsDialog
  Caption = 'Delete Messages'
  ClientHeight = 339
  ClientWidth = 281
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 248
    Width = 49
    Height = 13
    Caption = 'Filter Text'
  end
  object OKBtn: TButton
    Left = 110
    Top = 296
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelBtn: TButton
    Left = 188
    Top = 296
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object rgColumn: TRadioGroup
    Left = 16
    Top = 16
    Width = 121
    Height = 217
    ItemIndex = 0
    Items.Strings = (
      'Subject'
      'Author'
      'Date'
      'Lines'
      'Message Body'
      'Message ID'
      'Header'
      'Cross posted'
      'Number')
    TabOrder = 0
    OnClick = rgColumnClick
  end
  object rgOperation: TRadioGroup
    Left = 144
    Top = 16
    Width = 121
    Height = 217
    ItemIndex = 0
    Items.Strings = (
      'Matches'
      'Doesn'#39't match'
      'Is less then'
      'Is greater than'
      'Is equal to'
      'Is not equal to')
    TabOrder = 1
    OnClick = rgOperationClick
  end
  object Button1: TButton
    Left = 16
    Top = 296
    Width = 75
    Height = 25
    Caption = 'Add &Filter...'
    TabOrder = 4
    OnClick = Button1Click
  end
  object edFilter: TEdit
    Left = 16
    Top = 264
    Width = 249
    Height = 21
    TabOrder = 5
    OnChange = edFilterChange
  end
end
