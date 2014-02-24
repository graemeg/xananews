object dlgCancelArticles: TdlgCancelArticles
  Left = 416
  Top = 342
  BorderStyle = bsDialog
  Caption = 'Cancel Articles'
  ClientHeight = 293
  ClientWidth = 436
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
  object Label1: TLabel
    Left = 72
    Top = 24
    Width = 345
    Height = 33
    AutoSize = False
    Caption = 
      'Cancelling an article completely removes it from the news server' +
      '.  Are you sure you want to cancel the selected articles?'
    WordWrap = True
  end
  object Image1: TImage
    Left = 8
    Top = 16
    Width = 48
    Height = 48
  end
  object stNB: TLabel
    Left = 72
    Top = 72
    Width = 353
    Height = 65
    AutoSize = False
  end
  object stReason: TLabel
    Left = 72
    Top = 144
    Width = 158
    Height = 13
    Caption = 'Reason for cancelling this article:'
  end
  object Label3: TLabel
    Left = 184
    Top = 260
    Width = 67
    Height = 13
    Caption = 'Are you sure?'
  end
  object mmoReason: TMemo
    Left = 72
    Top = 160
    Width = 345
    Height = 73
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 264
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Yes'
    ModalResult = 6
    TabOrder = 1
  end
  object btnNo: TButton
    Left = 344
    Top = 256
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'No'
    Default = True
    ModalResult = 7
    TabOrder = 2
  end
end
