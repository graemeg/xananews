object dlgAttachments: TdlgAttachments
  Left = 388
  Top = 228
  Caption = 'Attachments'
  ClientHeight = 244
  ClientWidth = 466
  Color = clBtnFace
  Constraints.MinHeight = 198
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    466
    244)
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 16
    Top = 16
    Width = 435
    Height = 177
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'File Name'
        Width = 80
      end
      item
        Caption = 'Directory'
        Width = 80
      end
      item
        Caption = 'Date'
        Width = 70
      end
      item
        Caption = 'Size'
        Width = 60
      end>
    ColumnClick = False
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnData = ListView1Data
  end
  object btnAdd: TButton
    Left = 16
    Top = 208
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Add...'
    TabOrder = 1
    OnClick = btnAddClick
  end
  object btnRemove: TButton
    Left = 96
    Top = 208
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Remove'
    TabOrder = 2
    OnClick = btnRemoveClick
  end
  object btnClose: TButton
    Left = 370
    Top = 208
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 'Any File (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 224
    Top = 200
  end
end
