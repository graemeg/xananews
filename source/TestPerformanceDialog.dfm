object dlgTestPerformance: TdlgTestPerformance
  Left = 249
  Top = 225
  BorderStyle = bsDialog
  Caption = 'Test '#39'Decode Message'#39' Performance'
  ClientHeight = 103
  ClientWidth = 384
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
    Top = 24
    Width = 84
    Height = 13
    Caption = 'Decode Message '
  end
  object Label2: TLabel
    Left = 160
    Top = 24
    Width = 27
    Height = 13
    Caption = 'Times'
  end
  object lbResult: TLabel
    Left = 16
    Top = 48
    Width = 273
    Height = 13
    AutoSize = False
  end
  object Label3: TLabel
    Left = 16
    Top = 72
    Width = 70
    Height = 13
    Caption = 'Average Time:'
  end
  object lblAverage: TLabel
    Left = 96
    Top = 72
    Width = 49
    Height = 13
    AutoSize = False
    Caption = '0'
  end
  object Label4: TLabel
    Left = 152
    Top = 72
    Width = 39
    Height = 13
    Caption = 'seconds'
  end
  object SpeedButton1: TSpeedButton
    Left = 200
    Top = 68
    Width = 41
    Height = 22
    Caption = 'Reset'
    OnClick = SpeedButton1Click
  end
  object OKBtn: TButton
    Left = 300
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Test'
    Default = True
    TabOrder = 0
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 300
    Top = 70
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 1
  end
  object edDecodeTimes: TEdit
    Left = 112
    Top = 20
    Width = 41
    Height = 21
    TabOrder = 2
    Text = '100'
  end
end
