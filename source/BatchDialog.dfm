object dlgBatch: TdlgBatch
  Left = 227
  Top = 194
  Caption = 'Batch Properties'
  ClientHeight = 434
  ClientWidth = 741
  Color = clBtnFace
  Constraints.MinHeight = 267
  Constraints.MinWidth = 322
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  DesignSize = (
    741
    434)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 57
    Height = 13
    Caption = '&Batch Name'
    FocusControl = btnCancel
  end
  object Label2: TLabel
    Left = 16
    Top = 96
    Width = 35
    Height = 13
    Caption = '&Actions'
  end
  object Label3: TLabel
    Left = 136
    Top = 40
    Width = 37
    Height = 13
    Caption = 'minutes'
  end
  object lvActions: TListView
    Left = 16
    Top = 112
    Width = 711
    Height = 273
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        Caption = 'Account'
        Width = 100
      end
      item
        Caption = 'Newsgroup'
        Width = 255
      end
      item
        Caption = 'Action'
        Width = 352
      end>
    HideSelection = False
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupMenu1
    TabOrder = 4
    ViewStyle = vsReport
    OnColumnClick = lvActionsColumnClick
    OnDblClick = lvActionsDblClick
  end
  object btnOK: TButton
    Left = 570
    Top = 400
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
  object btnCancel: TButton
    Left = 650
    Top = 400
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object edBatchName: TEdit
    Left = 96
    Top = 12
    Width = 209
    Height = 21
    TabOrder = 0
  end
  object btnAction: TButton
    Left = 16
    Top = 400
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Action...'
    TabOrder = 5
    OnClick = btnActionClick
  end
  object cbRunEvery: TCheckBox
    Left = 16
    Top = 40
    Width = 81
    Height = 17
    Caption = '&Run every'
    TabOrder = 1
  end
  object edRunEvery: TEdit
    Left = 96
    Top = 38
    Width = 33
    Height = 21
    Enabled = False
    TabOrder = 2
    Text = '0'
  end
  object cbRunAtStartup: TCheckBox
    Left = 16
    Top = 64
    Width = 97
    Height = 17
    Caption = 'Run at Startup'
    TabOrder = 3
  end
  object PopupMenu1: TPopupMenu
    Left = 512
    Top = 400
    object SelectAll1: TMenuItem
      Caption = 'Check &All'
      OnClick = SelectAll1Click
    end
    object SelectNone1: TMenuItem
      Caption = '&Uncheck All'
      OnClick = SelectNone1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object SelectAllGroupsinAccount1: TMenuItem
      Caption = 'Check All &Groups in Account'
      OnClick = SelectAllGroupsinAccount1Click
    end
    object UncheckAllGroupsinAccount1: TMenuItem
      Caption = 'U&ncheck All Groups in Account'
      OnClick = UncheckAllGroupsinAccount1Click
    end
  end
end
