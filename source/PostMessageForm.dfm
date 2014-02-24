object fmPostMessage: TfmPostMessage
  Left = 637
  Top = 92
  Caption = 'Post Message'
  ClientHeight = 454
  ClientWidth = 640
  Color = clBtnFace
  Constraints.MinHeight = 259
  Constraints.MinWidth = 549
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    640
    454)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 14
    Width = 29
    Height = 13
    Caption = '&Group'
    FocusControl = cbGroup
  end
  object Label3: TLabel
    Left = 8
    Top = 38
    Width = 63
    Height = 13
    Caption = '&Follow Up To'
    FocusControl = cbFollowUpTo
  end
  object Label1: TLabel
    Left = 8
    Top = 62
    Width = 36
    Height = 13
    Caption = '&Subject'
  end
  object cbGroup: TComboBox
    Left = 79
    Top = 12
    Width = 457
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object cbFollowUpTo: TComboBox
    Left = 79
    Top = 36
    Width = 457
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object edSubject: TExRichEdit
    Left = 79
    Top = 58
    Width = 457
    Height = 23
    RightMargin = 0
    Anchors = [akLeft, akTop, akRight]
    AutoURLDetect = False
    AutoURLExecute = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Style = []
    MultiLine = False
    ParentFont = False
    TabOrder = 2
    WordWrap = False
  end
  object btnCrossPost: TButton
    Left = 552
    Top = 12
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 4
    OnClick = btnCrossPostClick
  end
  object btnFollowUp: TButton
    Left = 552
    Top = 36
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 5
    OnClick = btnFollowUpClick
  end
  inline fmePost1: TfmePost
    Left = 0
    Top = 88
    Width = 640
    Height = 366
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    TabStop = True
    ExplicitTop = 88
    ExplicitWidth = 640
    inherited Label1: TLabel
      Left = 298
      Width = 65
      ExplicitLeft = 297
      ExplicitWidth = 65
    end
    inherited Label2: TLabel
      Left = 326
      Width = 34
      ExplicitLeft = 325
      ExplicitWidth = 34
    end
    inherited lbISpellLanguage: TLabel
      Width = 48
      ExplicitWidth = 48
    end
    inherited ScrollBox1: TScrollBox
      Width = 530
      ExplicitWidth = 529
    end
    inherited cbCharset: TComboBox
      Left = 368
      ExplicitLeft = 367
    end
    inherited btnCancel: TButton
      Left = 552
      ExplicitLeft = 552
    end
    inherited btnOK: TButton
      Left = 552
      OnClick = TfmePost1btnOKClick
      ExplicitLeft = 552
    end
    inherited btnAttachments: TButton
      Left = 552
      ExplicitLeft = 552
    end
    inherited btnAdvanced: TButton
      Left = 552
      OnClick = fmePost1btnAdvancedClick
      ExplicitLeft = 552
    end
    inherited cbIdentity: TComboBox
      Left = 368
      ExplicitLeft = 367
    end
  end
  object PersistentPosition1: TPersistentPosition
    Manufacturer = 'Woozle'
    Product = 'XanaNews'
    SubKey = 'Position\PostMessage'
    OnGetSettingsClass = PersistentPosition1GetSettingsClass
    OnGetSettingsFile = PersistentPosition1GetSettingsFile
    Left = 578
    Top = 44
  end
end
