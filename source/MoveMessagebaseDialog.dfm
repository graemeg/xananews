object dlgMoveMessagebase: TdlgMoveMessagebase
  Left = 302
  Top = 202
  Caption = 'Move Messagebase'
  ClientHeight = 376
  ClientWidth = 399
  Color = clBtnFace
  Constraints.MinHeight = 403
  Constraints.MinWidth = 407
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    399
    376)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 16
    Top = 16
    Width = 368
    Height = 113
    Anchors = [akLeft, akTop, akRight]
    BevelInner = bvRaised
    BevelOuter = bvLowered
    BorderWidth = 1
    FullRepaint = False
    TabOrder = 0
    DesignSize = (
      368
      113)
    object lbWarning: TLabel
      Left = 12
      Top = 12
      Width = 345
      Height = 89
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      WordWrap = True
    end
  end
  object btnOK: TButton
    Left = 230
    Top = 334
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 310
    Top = 334
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 136
    Width = 368
    Height = 97
    Anchors = [akLeft, akTop, akRight]
    Caption = 'New Location for Messagebase'
    TabOrder = 3
    DesignSize = (
      368
      97)
    object edNewLocation: TEdit
      Left = 8
      Top = 66
      Width = 320
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object btnSelectFolder: TButton
      Left = 335
      Top = 64
      Width = 27
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectFolderClick
    end
    object rbDefaultLocation: TRadioButton
      Left = 8
      Top = 24
      Width = 113
      Height = 17
      Caption = 'Default Location'
      TabOrder = 2
      OnClick = rbDefaultLocationClick
    end
    object rbCustomLocation: TRadioButton
      Left = 8
      Top = 46
      Width = 113
      Height = 17
      Caption = 'Custom Location'
      TabOrder = 3
    end
  end
  object pnlStatus: TPanel
    Left = 16
    Top = 248
    Width = 369
    Height = 73
    Anchors = [akLeft, akTop, akRight]
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 4
    Visible = False
    DesignSize = (
      369
      73)
    object lbCopying: TLabel
      Left = 8
      Top = 8
      Width = 72
      Height = 13
      Caption = 'Analysing files:'
    end
    object lbCurrentFile: TLabel
      Left = 104
      Top = 8
      Width = 257
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
    end
    object ProgressBar1: TProgressBar
      Left = 8
      Top = 48
      Width = 353
      Height = 16
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
  end
  object FileCopier1: TFileCopier
    OnDuplicates = duCopy
    Leeway = 1048576
    CopyMode = cmMove
    OnStartAnalysis = FileCopier1StartAnalysis
    OnStartCopy = FileCopier1StartCopy
    OnEndCopy = FileCopier1EndCopy
    OnStartCopyFile = FileCopier1StartCopyFile
    OnEndCopyFile = FileCopier1EndCopyFile
    OnException = FileCopier1Exception
    Left = 8
    Top = 344
  end
  object PersistentPosition1: TPersistentPosition
    Manufacturer = 'Woozle'
    Product = 'XanaNews'
    SubKey = 'Position\MoveMessagebase'
    OnGetSettingsClass = PersistentPosition1GetSettingsClass
    OnGetSettingsFile = PersistentPosition1GetSettingsFile
    Left = 48
    Top = 344
  end
end
