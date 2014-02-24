object dlgServerAdminRemoveGroup: TdlgServerAdminRemoveGroup
  Left = 350
  Top = 383
  BorderStyle = bsDialog
  Caption = 'Server Administration - Remove Group'
  ClientHeight = 227
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
    Top = 16
    Width = 59
    Height = 13
    Caption = '&Group Name'
  end
  object Label2: TLabel
    Left = 16
    Top = 48
    Width = 47
    Height = 13
    Caption = '&Approved'
  end
  object Label3: TLabel
    Left = 16
    Top = 80
    Width = 36
    Height = 13
    Caption = '&Reason'
  end
  object OKBtn: TButton
    Left = 212
    Top = 184
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 292
    Top = 184
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edGroupName: TEdit
    Left = 88
    Top = 12
    Width = 281
    Height = 21
    TabOrder = 2
  end
  object edApproved: TEdit
    Left = 88
    Top = 46
    Width = 281
    Height = 21
    TabOrder = 3
  end
  object mmoReason: TMemo
    Left = 88
    Top = 80
    Width = 281
    Height = 89
    ScrollBars = ssVertical
    TabOrder = 4
  end
end
