object dlgExportCompressedMessages: TdlgExportCompressedMessages
  Left = 628
  Top = 528
  AutoScroll = False
  Caption = 'Export Compressed Messages'
  ClientHeight = 460
  ClientWidth = 625
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    625
    460)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 58
    Height = 13
    Caption = 'Export to file'
  end
  object lvActions: TListView
    Left = 16
    Top = 88
    Width = 598
    Height = 323
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        Caption = 'Account'
        Width = 100
      end
      item
        Caption = 'Newsgroup'
        Width = 300
      end>
    ColumnClick = False
    HideSelection = False
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupMenu1
    TabOrder = 0
    ViewStyle = vsReport
  end
  object btnOK: TButton
    Left = 457
    Top = 426
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 537
    Top = 426
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object cbExportToFile: TComboBox
    Left = 108
    Top = 12
    Width = 201
    Height = 21
    ItemHeight = 13
    TabOrder = 3
  end
  object cbExportSettings: TCheckBox
    Left = 16
    Top = 48
    Width = 105
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Export Settings'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object btnSelectFile: TButton
    Left = 320
    Top = 8
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 5
    OnClick = btnSelectFileClick
  end
  object pnlStatus: TPanel
    Left = 16
    Top = 424
    Width = 425
    Height = 28
    Anchors = [akLeft, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 6
    Visible = False
    object Label2: TLabel
      Left = 0
      Top = 8
      Width = 44
      Height = 13
      Caption = 'Exporting'
    end
    object ProgressBar1: TProgressBar
      Left = 56
      Top = 6
      Width = 369
      Height = 17
      TabOrder = 0
    end
  end
  object dlgExportCompressed: TSaveDialog
    DefaultExt = 'XNS'
    Filter = 'XanaNews Compressed Files (*.xns)|*.xns'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 398
    Top = 37
  end
  object PopupMenu1: TPopupMenu
    Left = 336
    Top = 56
    object mnuSelAll: TMenuItem
      Caption = 'Select All Newsgroups'
      OnClick = mnuSelAllClick
    end
    object mnuClearAll: TMenuItem
      Caption = 'Clear selection'
      OnClick = mnuClearAllClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mnuSelAccount: TMenuItem
      Caption = 'Select All Groups in This Account'
      OnClick = mnuSelAccountClick
    end
    object mnuClearAccount: TMenuItem
      Caption = 'Clear Selection for All Groups in This Account'
      OnClick = mnuClearAccountClick
    end
  end
end
