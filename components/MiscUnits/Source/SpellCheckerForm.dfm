object fmSpellChecker: TfmSpellChecker
  Left = 638
  Top = 203
  BorderIcons = [biSystemMenu]
  Caption = 'Check Spelling'
  ClientHeight = 285
  ClientWidth = 351
  Color = clBtnFace
  Constraints.MinHeight = 310
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    351
    285)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 73
    Height = 13
    Caption = 'Unknown Word'
  end
  object lblSuggestions: TLabel
    Left = 16
    Top = 96
    Width = 58
    Height = 13
    Caption = 'Suggestions'
  end
  object btnChange: TButton
    Left = 263
    Top = 112
    Width = 75
    Height = 20
    Anchors = [akTop, akRight]
    Caption = '&Change'
    TabOrder = 1
    OnClick = btnChangeClick
    ExplicitLeft = 264
  end
  object btnChangeAll: TButton
    Left = 263
    Top = 136
    Width = 75
    Height = 20
    Anchors = [akTop, akRight]
    Caption = 'Change A&ll'
    TabOrder = 2
    OnClick = btnChangeAllClick
    ExplicitLeft = 264
  end
  object btnSkipAll: TButton
    Left = 263
    Top = 184
    Width = 75
    Height = 20
    Anchors = [akTop, akRight]
    Caption = 'Skip &All'
    TabOrder = 3
    OnClick = btnSkipAllClick
    ExplicitLeft = 264
  end
  object btnSkip: TButton
    Left = 263
    Top = 160
    Width = 75
    Height = 20
    Anchors = [akTop, akRight]
    Caption = '&Skip'
    TabOrder = 4
    OnClick = btnSkipClick
    ExplicitLeft = 264
  end
  object btnAdd: TButton
    Left = 263
    Top = 208
    Width = 75
    Height = 20
    Anchors = [akTop, akRight]
    Caption = '&Add'
    TabOrder = 5
    OnClick = btnAddClick
    ExplicitLeft = 264
  end
  object btnCancel: TButton
    Left = 183
    Top = 246
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
    ExplicitLeft = 184
    ExplicitTop = 240
  end
  object btnFinish: TButton
    Left = 263
    Top = 246
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Finish'
    Default = True
    ModalResult = 1
    TabOrder = 7
    ExplicitLeft = 264
    ExplicitTop = 240
  end
  object reText: TExRichEdit
    Left = 16
    Top = 32
    Width = 320
    Height = 49
    RightMargin = 0
    Anchors = [akLeft, akTop, akRight]
    AutoURLDetect = False
    AutoURLExecute = False
    BevelWidth = 0
    font.Charset = ANSI_CHARSET
    font.Color = clBlack
    font.Height = -11
    font.Name = 'MS Sans Serif'
    font.Style = []
    HideSelection = False
    ParentFont = False
    TabOrder = 0
    WantReturns = False
    WordWrap = False
    ExplicitWidth = 321
  end
  object lvSuggestions: TListView
    Left = 16
    Top = 112
    Width = 232
    Height = 122
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        AutoSize = True
        Caption = 'Suggestions'
      end>
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    TabOrder = 8
    ViewStyle = vsReport
    OnDblClick = lvSuggestionsDblClick
    ExplicitWidth = 233
    ExplicitHeight = 116
  end
  object PersistentPosition1: TPersistentPosition
    Manufacturer = 'Woozle'
    Product = 'XanaNews'
    SubKey = 'Position\SpellChecker'
    OnGetSettingsClass = PersistentPosition1GetSettingsClass
    OnGetSettingsFile = PersistentPosition1GetSettingsFile
    Left = 32
    Top = 240
  end
end
