object dlgFindMessageOnInternet: TdlgFindMessageOnInternet
  Left = 0
  Top = 0
  Caption = 'Find Message on Internet'
  ClientHeight = 98
  ClientWidth = 308
  Color = clBtnFace
  Constraints.MaxHeight = 134
  Constraints.MinHeight = 125
  Constraints.MinWidth = 238
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    308
    98)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 56
    Height = 13
    Caption = '&Message ID'
  end
  object edMessageID: TEdit
    Left = 80
    Top = 14
    Width = 211
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 136
    Top = 56
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 216
    Top = 56
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
