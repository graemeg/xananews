object fmPostMessage: TfmPostMessage
  Left = 117
  Top = 202
  Caption = 'Post Message'
  ClientHeight = 408
  ClientWidth = 652
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
    652
    408)
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
    Width = 471
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
  end
  object cbFollowUpTo: TComboBox
    Left = 79
    Top = 36
    Width = 471
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 1
  end
  object edSubject: TExRichEdit
    Left = 79
    Top = 58
    Width = 471
    Height = 23
    RightMargin = 0
    Anchors = [akLeft, akTop, akRight]
    AutoURLDetect = False
    AutoURLExecute = False
    MultiLine = False
    TabOrder = 2
    WordWrap = False
  end
  object btnCrossPost: TButton
    Left = 568
    Top = 12
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 4
    OnClick = btnCrossPostClick
  end
  object btnFollowUp: TButton
    Left = 568
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
    Width = 652
    Height = 320
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    TabStop = True
    ExplicitTop = 88
    ExplicitWidth = 652
    ExplicitHeight = 320
    inherited Label1: TLabel
      Left = 310
      Top = 269
      Width = 65
      ExplicitLeft = 310
      ExplicitTop = 269
      ExplicitWidth = 65
    end
    inherited Label2: TLabel
      Left = 338
      Top = 292
      Width = 34
      ExplicitLeft = 338
      ExplicitTop = 292
      ExplicitWidth = 34
    end
    inherited ScrollBox1: TScrollBox
      Width = 542
      Height = 253
      ExplicitWidth = 542
      ExplicitHeight = 253
      inherited mmoMessage: TExRichEdit
        Font.Height = -15
      end
    end
    inherited cbCheckSpelling: TCheckBox
      Top = 279
      ExplicitTop = 279
    end
    inherited cbCharset: TComboBox
      Left = 381
      Top = 265
      Width = 169
      ExplicitLeft = 381
      ExplicitTop = 265
      ExplicitWidth = 169
    end
    inherited btnCancel: TButton
      Left = 564
      Top = 284
      ExplicitLeft = 564
      ExplicitTop = 284
    end
    inherited btnOK: TButton
      Left = 564
      Top = 252
      ExplicitLeft = 564
      ExplicitTop = 252
    end
    inherited btnAttachments: TButton
      Left = 564
      ExplicitLeft = 564
    end
    inherited btnAdvanced: TButton
      Left = 564
      OnClick = fmePost1btnAdvancedClick
      ExplicitLeft = 564
    end
    inherited btnSpell: TBitBtn
      Top = 273
      ExplicitTop = 273
    end
    inherited cbIdentity: TComboBox
      Left = 381
      Top = 289
      ExplicitLeft = 381
      ExplicitTop = 289
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
