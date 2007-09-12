object dlgBozoBin: TdlgBozoBin
  Left = 442
  Top = 234
  AutoScroll = False
  Caption = 'Bozo Bin Maintenance'
  ClientHeight = 414
  ClientWidth = 447
  Color = clBtnFace
  Constraints.MinHeight = 170
  Constraints.MinWidth = 436
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    447
    414)
  PixelsPerInch = 96
  TextHeight = 13
  object lvBozos: TListView
    Left = 16
    Top = 16
    Width = 414
    Height = 331
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Name'
        Width = 150
      end
      item
        Caption = 'E-Mail Address'
        Width = 150
      end
      item
        Caption = 'Date'
        Width = 100
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
  object btnAdd: TButton
    Left = 16
    Top = 369
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Add'
    TabOrder = 1
    OnClick = btnAddClick
  end
  object btnProperties: TButton
    Left = 96
    Top = 369
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Properties'
    TabOrder = 2
    OnClick = btnPropertiesClick
  end
  object btnDelete: TButton
    Left = 176
    Top = 369
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Delete'
    TabOrder = 3
    OnClick = btnDeleteClick
  end
  object btnOK: TButton
    Left = 274
    Top = 369
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 355
    Top = 369
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
end
