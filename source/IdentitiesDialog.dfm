object dlgIdentities: TdlgIdentities
  Left = 169
  Top = 192
  HelpType = htKeyword
  HelpKeyword = 'Batch Mode'
  AutoScroll = False
  Caption = 'Identities'
  ClientHeight = 343
  ClientWidth = 564
  Color = clBtnFace
  Constraints.MinHeight = 195
  Constraints.MinWidth = 436
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    564
    343)
  PixelsPerInch = 96
  TextHeight = 13
  object lvIdentities: TListView
    Left = 16
    Top = 16
    Width = 534
    Height = 275
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Name'
        Width = 150
      end
      item
        Caption = 'From'
        Width = 150
      end
      item
        Caption = 'E-Mail Address'
        Width = 150
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = btnPropertiesClick
  end
  object btnAdd: TButton
    Left = 16
    Top = 306
    Width = 77
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Add...'
    TabOrder = 1
    OnClick = btnAddClick
  end
  object btnRemove: TButton
    Left = 96
    Top = 306
    Width = 77
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Remo&ve'
    TabOrder = 2
    OnClick = btnRemoveClick
  end
  object btnProperties: TButton
    Left = 176
    Top = 306
    Width = 77
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Properties...'
    TabOrder = 3
    OnClick = btnPropertiesClick
  end
  object btnClose: TButton
    Left = 473
    Top = 306
    Width = 75
    Height = 25
    HelpType = htKeyword
    HelpKeyword = 'News Accounts'
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 4
  end
  object btnSetDefault: TButton
    Left = 349
    Top = 306
    Width = 77
    Height = 25
    Anchors = [akBottom]
    Caption = 'Set Default'
    TabOrder = 5
    OnClick = btnSetDefaultClick
  end
  object btnClone: TButton
    Left = 256
    Top = 306
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Clone...'
    TabOrder = 6
    OnClick = btnCloneClick
  end
end
