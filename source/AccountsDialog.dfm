object dlgAccounts: TdlgAccounts
  Left = 387
  Top = 345
  Caption = 'News Accounts'
  ClientHeight = 343
  ClientWidth = 485
  Color = clBtnFace
  Constraints.MinHeight = 189
  Constraints.MinWidth = 352
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    485
    343)
  PixelsPerInch = 96
  TextHeight = 13
  object CancelBtn: TButton
    Left = 396
    Top = 303
    Width = 75
    Height = 25
    HelpType = htKeyword
    HelpKeyword = 'News Accounts'
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 0
  end
  object lvAccounts: TListView
    Left = 16
    Top = 16
    Width = 455
    Height = 276
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Account'
        Width = 150
      end
      item
        AutoSize = True
        Caption = 'Connection'
      end>
    ColumnClick = False
    HideSelection = False
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = lvAccountsDblClick
  end
  object btnAdd: TButton
    Left = 15
    Top = 303
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akLeft, akBottom]
    Caption = '&Add...'
    TabOrder = 2
    OnClick = btnAddClick
  end
  object btnRemove: TButton
    Left = 95
    Top = 303
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akLeft, akBottom]
    Caption = '&Remove'
    TabOrder = 3
    OnClick = btnRemoveClick
  end
  object btnProperties: TButton
    Left = 175
    Top = 303
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akLeft, akBottom]
    Caption = '&Properties...'
    TabOrder = 4
    OnClick = btnPropertiesClick
  end
  object btnClone: TButton
    Left = 296
    Top = 303
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akBottom]
    Caption = '&Clone...'
    TabOrder = 5
    OnClick = btnCloneClick
  end
end
