object fmSpellChecker: TfmSpellChecker
  Left = 638
  Top = 203
  BorderStyle = bsToolWindow
  Caption = 'Check Spelling'
  ClientHeight = 279
  ClientWidth = 352
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 75
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
    Left = 264
    Top = 112
    Width = 75
    Height = 20
    Caption = '&Change'
    TabOrder = 1
    OnClick = btnChangeClick
  end
  object btnChangeAll: TButton
    Left = 264
    Top = 136
    Width = 75
    Height = 20
    Caption = 'Change A&ll'
    TabOrder = 2
    OnClick = btnChangeAllClick
  end
  object btnSkipAll: TButton
    Left = 264
    Top = 184
    Width = 75
    Height = 20
    Caption = 'Skip &All'
    TabOrder = 3
    OnClick = btnSkipAllClick
  end
  object btnSkip: TButton
    Left = 264
    Top = 160
    Width = 75
    Height = 20
    Caption = '&Skip'
    TabOrder = 4
    OnClick = btnSkipClick
  end
  object btnAdd: TButton
    Left = 264
    Top = 208
    Width = 75
    Height = 20
    Caption = '&Add'
    TabOrder = 5
    OnClick = btnAddClick
  end
  object btnCancel: TButton
    Left = 184
    Top = 240
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object btnFinish: TButton
    Left = 264
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Finish'
    Default = True
    ModalResult = 1
    TabOrder = 7
  end
  object reText: TExRichEdit
    Left = 16
    Top = 32
    Width = 321
    Height = 49
    RightMargin = 0
    AutoURLDetect = False
    AutoURLExecute = False
    BevelWidth = 0
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    HideSelection = False
    ParentFont = False
    TabOrder = 0
    WantReturns = False
    WordWrap = False
  end
  object lvSuggestions: TListView
    Left = 16
    Top = 112
    Width = 233
    Height = 116
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
