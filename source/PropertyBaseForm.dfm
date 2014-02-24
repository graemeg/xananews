object fmPropertyBase: TfmPropertyBase
  Left = 374
  Top = 189
  ActiveControl = vstSections
  BorderWidth = 4
  ClientHeight = 436
  ClientWidth = 616
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 169
    Top = 0
    Width = 4
    Height = 401
    Beveled = True
    ExplicitHeight = 323
  end
  object pnlOptions: TPanel
    Left = 173
    Top = 0
    Width = 443
    Height = 401
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    ExplicitWidth = 390
    ExplicitHeight = 323
    object Bevel1: TBevel
      Left = 0
      Top = 398
      Width = 443
      Height = 3
      Align = alBottom
      Shape = bsBottomLine
      ExplicitTop = 320
      ExplicitWidth = 390
    end
  end
  object vstSections: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 169
    Height = 401
    Align = alLeft
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    NodeDataSize = 4
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnFocusChanged = vstSectionsFocusChanged
    OnGetText = vstSectionsGetText
    OnInitChildren = vstSectionsInitChildren
    OnInitNode = vstSectionsInitNode
    ExplicitHeight = 323
    Columns = <>
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 401
    Width = 616
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 323
    ExplicitWidth = 563
    DesignSize = (
      616
      35)
    object btnOK: TButton
      Left = 294
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = btnOKClick
      ExplicitLeft = 241
    end
    object btnCancel: TButton
      Left = 371
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 318
    end
    object btnApply: TButton
      Left = 448
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Apply'
      TabOrder = 2
      OnClick = btnApplyClick
      ExplicitLeft = 395
    end
    object btnHelp: TButton
      Left = 533
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Help'
      TabOrder = 3
      OnClick = btnHelpClick
      ExplicitLeft = 480
    end
  end
  object PersistentPosition1: TPersistentPosition
    Manufacturer = 'Woozle'
    Product = 'XanaNews'
    OnGetSettingsClass = PersistentPosition1GetSettingsClass
    OnGetSettingsFile = PersistentPosition1GetSettingsFile
    Left = 16
    Top = 332
  end
  object PopupMenu1: TPopupMenu
    Left = 56
    Top = 328
    object ExpandAll1: TMenuItem
      Caption = '&Expand All'
      OnClick = ExpandAll1Click
    end
    object CollapseAll1: TMenuItem
      Caption = '&Collapse All'
      OnClick = CollapseAll1Click
    end
  end
end
