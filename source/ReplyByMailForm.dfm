object fmReplyByMail: TfmReplyByMail
  Left = 130
  Top = 121
  Caption = 'Reply By Mail'
  ClientHeight = 464
  ClientWidth = 776
  Color = clBtnFace
  Constraints.MinHeight = 293
  Constraints.MinWidth = 545
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    776
    464)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 92
    Width = 36
    Height = 13
    Caption = '&Subject'
  end
  object Label4: TLabel
    Left = 8
    Top = 68
    Width = 21
    Height = 13
    Caption = 'BCC'
  end
  object Label3: TLabel
    Left = 8
    Top = 44
    Width = 14
    Height = 13
    Caption = 'CC'
  end
  object Label2: TLabel
    Left = 8
    Top = 20
    Width = 13
    Height = 13
    Caption = 'To'
  end
  object edSubject: TEdit
    Left = 56
    Top = 88
    Width = 610
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
  object edBCC: TEdit
    Left = 56
    Top = 64
    Width = 610
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object edCC: TEdit
    Left = 56
    Top = 40
    Width = 610
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object edTo: TEdit
    Left = 56
    Top = 16
    Width = 610
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  inline fmePost1: TfmePost
    Left = 0
    Top = 120
    Width = 776
    Height = 344
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 4
    TabStop = True
    ExplicitTop = 120
    ExplicitWidth = 776
    ExplicitHeight = 344
    inherited Label1: TLabel
      Left = 434
      Top = 293
      Width = 65
      ExplicitLeft = 434
      ExplicitTop = 293
      ExplicitWidth = 65
    end
    inherited Label2: TLabel
      Left = 462
      Top = 316
      Width = 34
      ExplicitLeft = 462
      ExplicitTop = 316
      ExplicitWidth = 34
    end
    inherited ScrollBox1: TScrollBox
      Width = 666
      Height = 277
      ExplicitWidth = 666
      ExplicitHeight = 277
      inherited mmoMessage: TExRichEdit
        Font.Height = -15
      end
    end
    inherited cbCheckSpelling: TCheckBox
      Top = 303
      ExplicitTop = 303
    end
    inherited cbCharset: TComboBox
      Left = 505
      Top = 289
      ExplicitLeft = 505
      ExplicitTop = 289
    end
    inherited btnCancel: TButton
      Left = 688
      Top = 308
      ExplicitLeft = 688
      ExplicitTop = 308
    end
    inherited btnOK: TButton
      Left = 688
      Top = 276
      ExplicitLeft = 688
      ExplicitTop = 276
    end
    inherited btnAttachments: TButton
      Left = 688
      ExplicitLeft = 688
    end
    inherited btnAdvanced: TButton
      Left = 688
      ExplicitLeft = 688
    end
    inherited btnSpell: TBitBtn
      Top = 297
      ExplicitTop = 297
    end
    inherited cbIdentity: TComboBox
      Left = 504
      Top = 313
      ExplicitLeft = 504
      ExplicitTop = 313
    end
  end
  object PersistentPosition1: TPersistentPosition
    Manufacturer = 'Woozle'
    Product = 'XanaNews'
    SubKey = 'Position\ReplyByMail'
    OnGetSettingsClass = PersistentPosition1GetSettingsClass
    OnGetSettingsFile = PersistentPosition1GetSettingsFile
    Left = 728
    Top = 24
  end
end
