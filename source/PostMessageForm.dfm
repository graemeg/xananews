object fmPostMessage: TfmPostMessage
  Left = 441
  Top = 218
  Caption = 'Post Message'
  ClientHeight = 454
  ClientWidth = 640
  Color = clBtnFace
  Constraints.MinHeight = 259
  Constraints.MinWidth = 549
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
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
    Font.Name = 'MS Sans Serif'
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
    Width = 639
    Height = 366
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    TabStop = True
    ExplicitTop = 88
    ExplicitWidth = 652
    ExplicitHeight = 320
    inherited Label1: TLabel
      Left = 297
      Width = 65
      ExplicitLeft = 297
      ExplicitWidth = 65
    end
    inherited Label2: TLabel
      Left = 325
      Width = 34
      ExplicitLeft = 325
      ExplicitWidth = 34
    end
    inherited lbISpellLanguage: TLabel
      Width = 48
      ExplicitWidth = 48
    end
    inherited ScrollBox1: TScrollBox
      Width = 529
      ExplicitWidth = 529
    end
    inherited cbCharset: TComboBox
      Left = 367
      ExplicitLeft = 367
    end
    inherited btnCancel: TButton
      Left = 551
      ExplicitLeft = 551
    end
    inherited btnOK: TButton
      Left = 551
      ExplicitLeft = 551
    end
    inherited btnAttachments: TButton
      Left = 551
      ExplicitLeft = 551
    end
    inherited btnAdvanced: TButton
      Left = 551
      OnClick = fmePost1btnAdvancedClick
      ExplicitLeft = 551
    end
    inherited cbIdentity: TComboBox
      Left = 367
      ExplicitLeft = 367
    end
  end
  object PersistentPosition1: TPersistentPosition
    Manufacturer = 'Woozle'
    Product = 'XanaNews'
    SubKey = 'Position\PostMessage'
    OnGetSettingsClass = PersistentPosition1GetSettingsClass
    OnGetSettingsFile = PersistentPosition1GetSettingsFile
    Left = 744
    Top = 48
  end
end
