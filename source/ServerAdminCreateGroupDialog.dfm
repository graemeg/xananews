object dlgServerAdminCreateGroup: TdlgServerAdminCreateGroup
  Left = 266
  Top = 228
  ActiveControl = edGroupName
  BorderStyle = bsDialog
  Caption = 'Server Administration - Create Group'
  ClientHeight = 365
  ClientWidth = 384
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnShow = cbModeratedClick
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
    Top = 80
    Width = 47
    Height = 13
    Caption = '&Approved'
  end
  object Label3: TLabel
    Left = 16
    Top = 48
    Width = 53
    Height = 13
    Caption = '&Description'
  end
  object lbModeratorSubmissionAddress: TLabel
    Left = 32
    Top = 256
    Width = 147
    Height = 13
    Caption = 'Moderator &Submission Address'
  end
  object lbModeratorContactAddress: TLabel
    Left = 32
    Top = 280
    Width = 133
    Height = 13
    Caption = 'Moderator Contact Add&ress'
  end
  object Label6: TLabel
    Left = 16
    Top = 112
    Width = 37
    Height = 13
    Caption = '&Charter'
  end
  object OKBtn: TButton
    Left = 204
    Top = 320
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 7
  end
  object CancelBtn: TButton
    Left = 284
    Top = 320
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object edGroupName: TEdit
    Left = 88
    Top = 13
    Width = 281
    Height = 21
    CharCase = ecLowerCase
    TabOrder = 0
  end
  object cbModerated: TCheckBox
    Left = 16
    Top = 224
    Width = 85
    Height = 17
    Alignment = taLeftJustify
    Caption = '&Moderated'
    TabOrder = 4
    OnClick = cbModeratedClick
  end
  object edApproved: TEdit
    Left = 88
    Top = 76
    Width = 281
    Height = 21
    TabOrder = 2
  end
  object edDescription: TEdit
    Left = 88
    Top = 44
    Width = 281
    Height = 21
    TabOrder = 1
  end
  object edModeratorSubmissionAddress: TEdit
    Left = 192
    Top = 252
    Width = 177
    Height = 21
    TabOrder = 5
  end
  object edModeratorContactAddress: TEdit
    Left = 192
    Top = 276
    Width = 177
    Height = 21
    TabOrder = 6
  end
  object mmoCharter: TMemo
    Left = 88
    Top = 112
    Width = 281
    Height = 97
    ScrollBars = ssVertical
    TabOrder = 3
    WordWrap = False
  end
end
