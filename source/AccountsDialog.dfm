object dlgAccounts: TdlgAccounts
  Left = 387
  Top = 345
  AutoScroll = False
  Caption = 'News Accounts'
  ClientHeight = 422
  ClientWidth = 597
  Color = clBtnFace
  Constraints.MinHeight = 233
  Constraints.MinWidth = 433
  ParentFont = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    597
    422)
  PixelsPerInch = 120
  TextHeight = 16
  object CancelBtn: TButton
    Left = 487
    Top = 373
    Width = 93
    Height = 31
    HelpType = htKeyword
    HelpKeyword = 'News Accounts'
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 0
  end
  object lvAccounts: TListView
    Left = 20
    Top = 20
    Width = 560
    Height = 339
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Account'
        Width = 185
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
    Left = 18
    Top = 373
    Width = 93
    Height = 31
    Anchors = [akLeft, akBottom]
    Caption = '&Add...'
    TabOrder = 2
    OnClick = btnAddClick
  end
  object btnRemove: TButton
    Left = 117
    Top = 373
    Width = 92
    Height = 31
    Anchors = [akLeft, akBottom]
    Caption = '&Remove'
    TabOrder = 3
    OnClick = btnRemoveClick
  end
  object btnProperties: TButton
    Left = 215
    Top = 373
    Width = 93
    Height = 31
    Anchors = [akLeft, akBottom]
    Caption = '&Properties...'
    TabOrder = 4
    OnClick = btnPropertiesClick
  end
  object btnClone: TButton
    Left = 364
    Top = 373
    Width = 93
    Height = 31
    Anchors = [akBottom]
    Caption = '&Clone...'
    TabOrder = 5
    OnClick = btnCloneClick
  end
end
