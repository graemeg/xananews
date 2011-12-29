object fmSpellChecker: TfmSpellChecker
  Left = 638
  Top = 203
  BorderIcons = [biSystemMenu]
  Caption = 'Check Spelling'
  ClientHeight = 365
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 17
  object Label1: TLabel
    Left = 21
    Top = 21
    Width = 97
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Unknown Word'
  end
  object lblSuggestions: TLabel
    Left = 21
    Top = 126
    Width = 74
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Suggestions'
  end
  object btnChange: TButton
    Left = 345
    Top = 146
    Width = 98
    Height = 27
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&Change'
    TabOrder = 1
    OnClick = btnChangeClick
  end
  object btnChangeAll: TButton
    Left = 345
    Top = 178
    Width = 98
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Change A&ll'
    TabOrder = 2
    OnClick = btnChangeAllClick
  end
  object btnSkipAll: TButton
    Left = 345
    Top = 241
    Width = 98
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Skip &All'
    TabOrder = 3
    OnClick = btnSkipAllClick
  end
  object btnSkip: TButton
    Left = 345
    Top = 209
    Width = 98
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&Skip'
    TabOrder = 4
    OnClick = btnSkipClick
  end
  object btnAdd: TButton
    Left = 345
    Top = 272
    Width = 98
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&Add'
    TabOrder = 5
    OnClick = btnAddClick
  end
  object btnCancel: TButton
    Left = 241
    Top = 314
    Width = 98
    Height = 33
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object btnFinish: TButton
    Left = 345
    Top = 314
    Width = 98
    Height = 33
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Finish'
    Default = True
    ModalResult = 1
    TabOrder = 7
  end
  object reText: TExRichEdit
    Left = 21
    Top = 42
    Width = 420
    Height = 64
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    RightMargin = 0
    AutoURLDetect = False
    AutoURLExecute = False
    BevelWidth = 0
    font.Charset = ANSI_CHARSET
    font.Color = clBlack
    font.Height = -15
    font.Name = 'MS Sans Serif'
    font.Style = []
    HideSelection = False
    ParentFont = False
    TabOrder = 0
    WantReturns = False
    WordWrap = False
  end
  object lvSuggestions: TListView
    Left = 21
    Top = 146
    Width = 305
    Height = 152
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
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
  end
  object PersistentPosition1: TPersistentPosition
    Manufacturer = 'Woozle'
    Product = 'XanaNews'
    SubKey = 'Position\SpellChecker'
    Left = 32
    Top = 240
  end
end
