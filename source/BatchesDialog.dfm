object dlgBatches: TdlgBatches
  Left = 384
  Top = 381
  HelpType = htKeyword
  HelpKeyword = 'Batch Mode'
  Anchors = [akTop]
  Caption = 'Batches'
  ClientHeight = 343
  ClientWidth = 485
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
    485
    343)
  PixelsPerInch = 96
  TextHeight = 13
  object lvBatches: TListView
    Left = 16
    Top = 16
    Width = 451
    Height = 275
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Batch'
        Width = 150
      end
      item
        Caption = 'Scheduled'
        Width = 100
      end
      item
        Caption = 'Run at Startup'
        Width = 197
      end>
    ColumnClick = False
    Constraints.MinWidth = 393
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
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Add...'
    TabOrder = 1
    OnClick = btnAddClick
  end
  object btnRemove: TButton
    Left = 96
    Top = 306
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Remo&ve'
    TabOrder = 2
    OnClick = btnRemoveClick
  end
  object btnProperties: TButton
    Left = 176
    Top = 306
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Properties...'
    TabOrder = 3
    OnClick = btnPropertiesClick
  end
  object btnRun: TButton
    Left = 296
    Top = 306
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Caption = '&Run'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = btnRunClick
  end
  object btnClose: TButton
    Left = 393
    Top = 306
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
