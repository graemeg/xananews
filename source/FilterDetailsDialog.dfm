object dlgFilterDetails: TdlgFilterDetails
  Left = 317
  Top = 214
  BorderStyle = bsDialog
  Caption = 'Filter'
  ClientHeight = 383
  ClientWidth = 300
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object dlgFilterDetails: TLabel
    Left = 16
    Top = 16
    Width = 54
    Height = 13
    Caption = 'Filter Name'
  end
  object Label1: TLabel
    Left = 16
    Top = 272
    Width = 49
    Height = 13
    Caption = 'Filter Text'
  end
  object edFilter: TEdit
    Left = 16
    Top = 30
    Width = 265
    Height = 21
    TabOrder = 0
  end
  object rgColumn: TRadioGroup
    Left = 16
    Top = 63
    Width = 121
    Height = 194
    Items.Strings = (
      'Subject'
      'From'
      'Date'
      'Lines'
      'Message Body'
      'Message ID'
      'Header'
      'Crossposted')
    TabOrder = 1
  end
  object rgOperator: TRadioGroup
    Left = 160
    Top = 63
    Width = 121
    Height = 194
    Items.Strings = (
      'matches'
      'doesn'#39't match'
      'less than'
      'greater or equal'
      'equals'
      'doesn'#39't equal')
    TabOrder = 2
  end
  object edFilterTExt: TEdit
    Left = 16
    Top = 296
    Width = 265
    Height = 21
    TabOrder = 3
  end
  object btnOK: TButton
    Left = 128
    Top = 336
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 208
    Top = 336
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
end
