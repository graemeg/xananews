object dlgFilters: TdlgFilters
  Left = 901
  Top = 372
  Caption = 'Filters'
  ClientHeight = 336
  ClientWidth = 485
  Color = clBtnFace
  Constraints.MinHeight = 188
  Constraints.MinWidth = 355
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
    336)
  PixelsPerInch = 96
  TextHeight = 13
  object btnClose: TButton
    Left = 395
    Top = 305
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object lvFilters: TListView
    Left = 16
    Top = 16
    Width = 453
    Height = 278
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        Caption = 'Filter Name'
        Width = 200
      end
      item
        Caption = 'Details'
        Width = 249
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnChange = lvFiltersChange
    OnClick = lvFiltersClick
  end
  object btnAdd: TButton
    Left = 16
    Top = 305
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Add'
    TabOrder = 2
    OnClick = btnAddClick
  end
  object btnUpdate: TButton
    Left = 176
    Top = 305
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Properties...'
    TabOrder = 3
    OnClick = btnUpdateClick
  end
  object btnDelete: TButton
    Left = 96
    Top = 305
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Remove'
    TabOrder = 4
    OnClick = btnDeleteClick
  end
end
