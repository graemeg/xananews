object fmReplyByMail: TfmReplyByMail
  Left = 653
  Top = 126
  Caption = 'Reply By Mail'
  ClientHeight = 483
  ClientWidth = 638
  Color = clBtnFace
  Constraints.MinHeight = 293
  Constraints.MinWidth = 545
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    638
    483)
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
    Width = 480
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
  object edBCC: TEdit
    Left = 56
    Top = 64
    Width = 480
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object edCC: TEdit
    Left = 56
    Top = 40
    Width = 480
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object edTo: TEdit
    Left = 56
    Top = 16
    Width = 480
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  inline fmePost1: TfmePost
    Left = 0
    Top = 117
    Width = 638
    Height = 366
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 4
    TabStop = True
    ExplicitTop = 117
    ExplicitWidth = 662
    inherited Label1: TLabel
      Left = 296
      Width = 65
      ExplicitLeft = 296
      ExplicitWidth = 65
    end
    inherited Label2: TLabel
      Left = 324
      Width = 34
      ExplicitLeft = 324
      ExplicitWidth = 34
    end
    inherited lbISpellLanguage: TLabel
      Width = 48
      ExplicitWidth = 48
    end
    inherited ScrollBox1: TScrollBox
      Width = 528
      ExplicitWidth = 528
    end
    inherited cbCharset: TComboBox
      Left = 366
      ExplicitLeft = 366
    end
    inherited btnCancel: TButton
      Left = 550
      ExplicitLeft = 550
    end
    inherited btnOK: TButton
      Left = 550
      ExplicitLeft = 550
    end
    inherited btnAttachments: TButton
      Left = 550
      ExplicitLeft = 550
    end
    inherited btnAdvanced: TButton
      Left = 550
      Visible = False
      ExplicitLeft = 550
    end
    inherited cbIdentity: TComboBox
      Left = 366
      ExplicitLeft = 366
    end
  end
  object PersistentPosition1: TPersistentPosition
    Manufacturer = 'Woozle'
    Product = 'XanaNews'
    SubKey = 'Position\ReplyByMail'
    OnGetSettingsClass = PersistentPosition1GetSettingsClass
    OnGetSettingsFile = PersistentPosition1GetSettingsFile
    Left = 578
    Top = 72
  end
end
