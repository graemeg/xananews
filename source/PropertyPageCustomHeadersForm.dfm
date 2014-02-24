inherited fmPropertyPageCustomHeaders: TfmPropertyPageCustomHeaders
  HelpType = htKeyword
  HelpKeyword = 'OptionsCustomHeaders'
  Caption = 'Custom Headers Display'
  PixelsPerInch = 96
  TextHeight = 13
  object Label23: TLabel [0]
    Left = 12
    Top = 60
    Width = 108
    Height = 13
    Caption = 'Show Custom Headers'
  end
  inherited Panel1: TPanel
    ExplicitWidth = 262
    inherited Bevel1: TBevel
      ExplicitWidth = 262
    end
    inherited stSectionDetails: TLabel
      Caption = 
        'Select which headers are displayed when you select View, Headers' +
        ', Custom (F12)'
      ExplicitWidth = 250
    end
  end
  object lvShowCustomHeaders: TListView
    Left = 12
    Top = 76
    Width = 356
    Height = 291
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        Caption = 'Header'
        Width = 230
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    TabOrder = 1
    ViewStyle = vsReport
    OnChange = lvShowCustomHeadersChange
    ExplicitWidth = 234
    ExplicitHeight = 133
  end
  object btnAddShowCustomHeader: TButton
    Left = 12
    Top = 374
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Add...'
    TabOrder = 2
    OnClick = btnAddShowCustomHeaderClick
    ExplicitTop = 216
  end
  object btnRemoveShowCustomHeader: TButton
    Left = 94
    Top = 374
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Remove'
    TabOrder = 3
    OnClick = btnRemoveShowCustomHeaderClick
    ExplicitTop = 216
  end
end
