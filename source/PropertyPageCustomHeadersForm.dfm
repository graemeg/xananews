inherited fmPropertyPageCustomHeaders: TfmPropertyPageCustomHeaders
  HelpType = htKeyword
  HelpKeyword = 'OptionsCustomHeaders'
  Caption = 'Custom Headers Display'
  ClientHeight = 256
  ClientWidth = 262
  Constraints.MinHeight = 256
  Constraints.MinWidth = 262
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
    Width = 262
    inherited Bevel1: TBevel
      Width = 262
    end
    inherited stSectionDetails: TLabel
      Width = 250
      Caption = 
        'Select which headers are displayed when you select View, Headers' +
        ', Custom (F12)'
    end
  end
  object lvShowCustomHeaders: TListView
    Left = 12
    Top = 76
    Width = 234
    Height = 133
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        Caption = 'Header'
        Width = -2
        WidthType = (
          -2)
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    TabOrder = 1
    ViewStyle = vsReport
    OnChange = lvShowCustomHeadersChange
  end
  object btnAddShowCustomHeader: TButton
    Left = 12
    Top = 216
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Add...'
    TabOrder = 2
    OnClick = btnAddShowCustomHeaderClick
  end
  object btnRemoveShowCustomHeader: TButton
    Left = 94
    Top = 216
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Remove'
    TabOrder = 3
    OnClick = btnRemoveShowCustomHeaderClick
  end
end
