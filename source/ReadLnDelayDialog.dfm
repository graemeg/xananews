object dlgReadLnDelay: TdlgReadLnDelay
  Left = 0
  Top = 0
  Caption = 'Slow Internet Connection'
  ClientHeight = 78
  ClientWidth = 229
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 129
    Height = 13
    Caption = 'Readln Delay (Milliseconds)'
  end
  object btnOK: TButton
    Left = 72
    Top = 48
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 150
    Top = 48
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edReadLnDelay: TEdit
    Left = 152
    Top = 12
    Width = 73
    Height = 21
    TabOrder = 2
    Text = '0'
  end
end
