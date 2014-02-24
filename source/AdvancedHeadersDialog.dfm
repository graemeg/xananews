object dlgAdvancedHeaders: TdlgAdvancedHeaders
  Left = 373
  Top = 215
  Caption = 'Advanced Headers'
  ClientHeight = 270
  ClientWidth = 389
  Color = clBtnFace
  Constraints.MinHeight = 233
  Constraints.MinWidth = 397
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    389
    270)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 230
    Height = 13
    Caption = 'Enter a list of extra headers like '#39'X-Name: Value'#39
  end
  object mmoAdvancedHeaders: TMemo
    Left = 16
    Top = 40
    Width = 273
    Height = 209
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
    OnKeyDown = mmoAdvancedHeadersKeyDown
  end
  object btnOK: TButton
    Left = 304
    Top = 48
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 304
    Top = 80
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
