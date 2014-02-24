inherited fmPropertyPageFilters: TfmPropertyPageFilters
  HelpType = htKeyword
  HelpKeyword = 'Filters'
  Caption = 'Filters'
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited Bevel2: TBevel
    ExplicitTop = 287
  end
  inherited Panel1: TPanel
    inherited stSectionDetails: TLabel
      Caption = 
        'XanaNews holds a single list of filters, but it allows you to ap' +
        'ply each one globally, per account or per newsgroup'
    end
  end
  object btnUpdate: TButton
    Left = 168
    Top = 343
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Properties...'
    TabOrder = 2
    OnClick = btnUpdateClick
  end
  object btnDelete: TButton
    Left = 88
    Top = 343
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Remove'
    TabOrder = 3
    OnClick = btnDeleteClick
  end
  object btnAdd: TButton
    Left = 12
    Top = 343
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Add...'
    TabOrder = 4
    OnClick = btnAddClick
  end
  object vstFilters: TVirtualStringTree
    Left = 12
    Top = 60
    Width = 433
    Height = 277
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = 0
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoVisible]
    Header.ParentFont = True
    Header.Style = hsFlatButtons
    NodeDataSize = 4
    TabOrder = 5
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnAfterCellPaint = vstFiltersAfterCellPaint
    OnClick = vstFiltersClick
    OnGetText = vstFiltersGetText
    OnInitNode = vstFiltersInitNode
    Columns = <
      item
        Position = 0
        Width = 89
        WideText = 'Filter'
      end
      item
        Position = 1
        Width = 170
        WideText = 'Description'
      end
      item
        Alignment = taCenter
        Position = 2
        Style = vsOwnerDraw
        Width = 90
        WideText = 'Don'#39't Download'
      end
      item
        Alignment = taCenter
        Position = 3
        Style = vsOwnerDraw
        Width = 80
        WideText = 'Don'#39't Display'
      end>
  end
end
