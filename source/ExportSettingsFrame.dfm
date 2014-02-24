object fmeExportSettings: TfmeExportSettings
  Left = 0
  Top = 0
  Width = 417
  Height = 238
  TabOrder = 0
  DesignSize = (
    417
    238)
  object cbExportSettings: TCheckBox
    Left = 0
    Top = 216
    Width = 153
    Height = 17
    Alignment = taLeftJustify
    Anchors = [akLeft, akBottom]
    Caption = 'Export XanaNews Settings'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 417
    Height = 209
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvLowered
    BevelOuter = bvLowered
    Ctl3D = True
    FullRepaint = False
    ParentCtl3D = False
    TabOrder = 1
    object lvActions: TListView
      Left = 2
      Top = 2
      Width = 413
      Height = 205
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Checkboxes = True
      Columns = <
        item
          Caption = 'Account'
          Width = 100
        end
        item
          AutoSize = True
          Caption = 'Newsgroup'
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
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
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
