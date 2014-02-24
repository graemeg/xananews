inherited fmPropertyPageBozos: TfmPropertyPageBozos
  Left = 323
  Top = 135
  Caption = 'Bozo Bin'
  ClientWidth = 436
  ExplicitLeft = 323
  ExplicitTop = 135
  ExplicitWidth = 452
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [0]
    Left = 12
    Top = 128
    Width = 104
    Height = 13
    Caption = 'Bozo Bin Maintenance'
  end
  object Bevel2: TBevel [1]
    Left = 0
    Top = 112
    Width = 437
    Height = 4
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object Label32: TLabel [2]
    Left = 12
    Top = 84
    Width = 111
    Height = 13
    Caption = 'Remove from bin after '
    FocusControl = edRemoveFromBin
  end
  object Label34: TLabel [3]
    Left = 228
    Top = 84
    Width = 23
    Height = 13
    Caption = 'days'
  end
  object Label2: TLabel [4]
    Left = 12
    Top = 54
    Width = 102
    Height = 13
    Caption = 'Action for New Bozos'
  end
  inherited Panel1: TPanel
    Width = 436
    ExplicitWidth = 436
    inherited Bevel1: TBevel
      Width = 436
      ExplicitWidth = 436
    end
    inherited stSectionDetails: TLabel
      Width = 424
      Caption = 
        'Bozos are undesirable posters.  You can prevent their message fr' +
        'om being displayed or downloaded.'
      ExplicitWidth = 424
    end
  end
  object lvBozos: TListView
    Left = 12
    Top = 144
    Width = 415
    Height = 226
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Name'
        Width = 120
      end
      item
        Caption = 'E-Mail Address'
        Width = 120
      end
      item
        Caption = 'Date'
        Width = 80
      end
      item
        Caption = 'Action'
        Width = 78
      end>
    ColumnClick = False
    HideSelection = False
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 3
    ViewStyle = vsReport
    OnData = lvBozosData
  end
  object btnAdd: TButton
    Left = 16
    Top = 376
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Add...'
    TabOrder = 4
    OnClick = btnAddClick
  end
  object btnProperties: TButton
    Left = 96
    Top = 376
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Properties...'
    TabOrder = 5
    OnClick = btnPropertiesClick
  end
  object btnDelete: TButton
    Left = 176
    Top = 376
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Delete'
    TabOrder = 6
    OnClick = btnDeleteClick
  end
  object edRemoveFromBin: TEdit
    Left = 184
    Top = 80
    Width = 33
    Height = 21
    TabOrder = 2
    OnChange = edRemoveFromBinChange
  end
  object cbDefaultBozoAction: TComboBox
    Left = 184
    Top = 52
    Width = 225
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = cbDefaultBozoActionChange
    Items.Strings = (
      'Ignore Messages from this Bozo'
      'Mark as Read Messages from this Bozo'
      'Ignore Threads Started by this Bozo'
      'Mark as Read Threads Started by this Bozo'
      'Don'#39't Download Messages from this Bozo')
  end
end
