object dlgMailAccounts: TdlgMailAccounts
  Left = 339
  Top = 122
  Caption = 'Mail Accounts'
  ClientHeight = 343
  ClientWidth = 485
  Color = clBtnFace
  Constraints.MinHeight = 233
  Constraints.MinWidth = 433
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    485
    343)
  PixelsPerInch = 96
  TextHeight = 13
  object lvAccounts: TListView
    Left = 16
    Top = 16
    Width = 455
    Height = 276
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
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = lvAccountsDblClick
  end
  object btnAdd: TButton
    Left = 15
    Top = 303
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Add...'
    TabOrder = 1
    OnClick = btnAddClick
  end
  object btnRemove: TButton
    Left = 95
    Top = 303
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Remove'
    TabOrder = 2
    OnClick = btnRemoveClick
  end
  object btnProperties: TButton
    Left = 175
    Top = 303
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Properties...'
    TabOrder = 3
    OnClick = btnPropertiesClick
  end
  object btnClone: TButton
    Left = 296
    Top = 303
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Caption = '&Clone...'
    TabOrder = 4
    OnClick = btnCloneClick
  end
  object CancelBtn: TButton
    Left = 396
    Top = 303
    Width = 75
    Height = 25
    HelpType = htKeyword
    HelpKeyword = 'News Accounts'
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 5
  end
end
