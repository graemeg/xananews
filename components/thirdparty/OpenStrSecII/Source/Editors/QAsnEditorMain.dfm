object frmASN1Editor: TfrmASN1Editor
  Left = 207
  Top = 165
  Width = 759
  Height = 414
  Caption = 'ASN Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 249
    Top = 0
    Width = 8
    Height = 345
  end
  object gbItems: TGroupBox
    Left = 0
    Top = 0
    Width = 249
    Height = 345
    Align = alLeft
    Caption = 'Items'
    TabOrder = 0
    DesignSize = (
      249
      345)
    object TreeView1: TTreeView
      Left = 8
      Top = 16
      Width = 145
      Height = 315
      Anchors = [akLeft, akTop, akRight, akBottom]
      HideSelection = False
      Indent = 19
      RowSelect = True
      TabOrder = 0
      OnChange = TreeView1Change
    end
    object btnNewItem: TButton
      Left = 160
      Top = 16
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'New Item'
      TabOrder = 1
      OnClick = btnNewItemClick
    end
    object btnDelete: TButton
      Left = 160
      Top = 80
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Delete'
      Enabled = False
      TabOrder = 2
      OnClick = btnDeleteClick
    end
    object btnLoad: TButton
      Left = 160
      Top = 112
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Load'
      TabOrder = 3
      OnClick = btnLoadClick
    end
    object btnSave: TButton
      Left = 160
      Top = 144
      Width = 75
      Height = 25
      Caption = 'Save'
      Enabled = False
      TabOrder = 4
      OnClick = btnSaveClick
    end
    object btnNewSubItem: TButton
      Left = 160
      Top = 48
      Width = 75
      Height = 25
      Caption = 'New Subitem'
      Enabled = False
      TabOrder = 5
      OnClick = btnNewSubItemClick
    end
    object btnView: TButton
      Left = 160
      Top = 176
      Width = 75
      Height = 25
      Caption = 'View'
      TabOrder = 6
      OnClick = btnViewClick
    end
    object btnConvert: TButton
      Left = 160
      Top = 208
      Width = 75
      Height = 25
      Hint = 'Convert to a new Object Pascal unit'
      Caption = 'Convert'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = btnConvertClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 345
    Width = 751
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      751
      42)
    object btnCancel: TButton
      Left = 592
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
      OnClick = btnCancelClick
    end
    object btnApply: TButton
      Left = 672
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Apply'
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnOK: TButton
      Left = 512
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
      OnClick = btnOKClick
    end
  end
  object gbItemProps: TGroupBox
    Left = 257
    Top = 0
    Width = 494
    Height = 345
    Align = alClient
    Caption = 'Item Properties'
    TabOrder = 2
    DesignSize = (
      494
      345)
    object Label6: TLabel
      Left = 8
      Top = 296
      Width = 22
      Height = 13
      Caption = 'Path'
    end
    object gbType: TGroupBox
      Left = 8
      Top = 16
      Width = 201
      Height = 129
      Caption = 'Type'
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 16
        Width = 42
        Height = 13
        Caption = 'Tag type'
      end
      object Label2: TLabel
        Left = 8
        Top = 56
        Width = 14
        Height = 13
        Caption = 'Cls'
      end
      object Label3: TLabel
        Left = 120
        Top = 56
        Width = 19
        Height = 13
        Caption = 'Tag'
      end
      object cbDefTypeName: TComboBox
        Left = 8
        Top = 32
        Width = 185
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnClick = cbDefTypeNameClick
      end
      object cbCls: TComboBox
        Left = 8
        Top = 72
        Width = 105
        Height = 21
        ItemHeight = 13
        TabOrder = 1
        Text = 'UNIVERSAL'
        OnChange = cbClsChange
        Items.Strings = (
          'UNIVERSAL'
          'APPLICATION'
          'CONTEXT SPECIFIC'
          'PRIVATE')
      end
      object meTag: TMaskEdit
        Left = 120
        Top = 72
        Width = 70
        Height = 21
        EditMask = '!#99999;1; '
        MaxLength = 6
        TabOrder = 2
        Text = '      '
        OnChange = meTagChange
      end
      object chbImplicit: TCheckBox
        Left = 8
        Top = 100
        Width = 81
        Height = 17
        Caption = 'Implicit'
        TabOrder = 3
        OnClick = chbImplicitClick
      end
      object chbConstructed: TCheckBox
        Left = 96
        Top = 100
        Width = 97
        Height = 17
        Caption = 'Constructed'
        TabOrder = 4
        OnClick = chbConstructedClick
      end
    end
    object gbDecl: TGroupBox
      Left = 8
      Top = 148
      Width = 201
      Height = 145
      Caption = 'Declaration'
      TabOrder = 1
      object Label4: TLabel
        Left = 8
        Top = 16
        Width = 44
        Height = 13
        Caption = 'VarName'
      end
      object Label5: TLabel
        Left = 8
        Top = 56
        Width = 52
        Height = 13
        Caption = 'TypeName'
      end
      object edtVarName: TEdit
        Left = 8
        Top = 32
        Width = 185
        Height = 21
        TabOrder = 0
        OnChange = edtVarNameChange
      end
      object chbOptional: TCheckBox
        Left = 8
        Top = 96
        Width = 97
        Height = 17
        Caption = 'Optional'
        TabOrder = 1
        OnClick = chbOptionalClick
      end
      object chbHasDefaultValue: TCheckBox
        Left = 8
        Top = 120
        Width = 137
        Height = 17
        Caption = 'HasDefaultValue'
        TabOrder = 2
        OnClick = chbHasDefaultValueClick
      end
      object cbTypeName: TComboBox
        Left = 8
        Top = 72
        Width = 185
        Height = 21
        ItemHeight = 13
        TabOrder = 3
        OnDropDown = cbTypeNameDropDown
      end
      object btnApplyType: TButton
        Left = 120
        Top = 104
        Width = 75
        Height = 25
        Caption = 'Apply Type'
        TabOrder = 4
        OnClick = btnApplyTypeClick
      end
    end
    object edtPath: TEdit
      Left = 8
      Top = 312
      Width = 479
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 2
    end
    object PageControl1: TPageControl
      Left = 216
      Top = 16
      Width = 271
      Height = 289
      ActivePage = TabSheet4
      Anchors = [akLeft, akTop, akRight, akBottom]
      Style = tsFlatButtons
      TabHeight = 1
      TabOrder = 3
      object TabSheet1: TTabSheet
        Caption = 'Constructed'
      end
      object TabSheet2: TTabSheet
        Caption = 'Choice'
        DesignSize = (
          263
          278)
        object Label8: TLabel
          Left = 8
          Top = 8
          Width = 38
          Height = 13
          Caption = 'Choices'
        end
        object Label9: TLabel
          Left = 8
          Top = 240
          Width = 107
          Height = 13
          Caption = 'Selected Choice Index'
        end
        object lvChoices: TListView
          Left = 8
          Top = 24
          Width = 255
          Height = 177
          Anchors = [akLeft, akTop, akRight]
          Columns = <
            item
              Caption = 'Index'
            end
            item
              Caption = 'VarName'
              Width = 100
            end
            item
              Caption = 'TypeName'
              Width = 100
            end>
          HideSelection = False
          OwnerData = True
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
          OnData = lvChoicesData
          OnSelectItem = lvChoicesSelectItem
        end
        object btnAddChoice: TButton
          Left = 152
          Top = 208
          Width = 51
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Add'
          TabOrder = 1
          OnClick = btnAddChoiceClick
        end
        object btnDeleteChoice: TButton
          Left = 208
          Top = 208
          Width = 51
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Delete'
          TabOrder = 2
          OnClick = btnDeleteChoiceClick
        end
        object edtSelectedChoice: TEdit
          Left = 8
          Top = 256
          Width = 121
          Height = 21
          TabOrder = 3
          OnChange = edtSelectedChoiceChange
        end
        object btnEditChoice: TButton
          Left = 96
          Top = 208
          Width = 51
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Edit'
          TabOrder = 4
          OnClick = btnEditChoiceClick
        end
        object btnTypeChoice: TButton
          Left = 40
          Top = 208
          Width = 51
          Height = 25
          Caption = 'Type'
          TabOrder = 5
          OnClick = btnTypeChoiceClick
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'TypeIdentified'
        DesignSize = (
          263
          278)
        object Label11: TLabel
          Left = 8
          Top = 8
          Width = 72
          Height = 13
          Caption = 'Type Identifiers'
        end
        object Label13: TLabel
          Left = 8
          Top = 240
          Width = 33
          Height = 13
          Caption = 'IDField'
        end
        object lvTypeIdentifiers: TListView
          Left = 8
          Top = 24
          Width = 255
          Height = 177
          Anchors = [akLeft, akTop, akRight]
          Columns = <
            item
              Caption = 'Index'
            end
            item
              Caption = 'TypeName'
              Width = 100
            end
            item
              Caption = 'IdentifiedBy'
              Width = 100
            end>
          OwnerData = True
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
          OnData = lvTypeIdentifiersData
          OnSelectItem = lvTypeIdentifiersSelectItem
        end
        object btnEditIdentifier: TButton
          Left = 96
          Top = 208
          Width = 51
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Edit'
          TabOrder = 1
          OnClick = btnEditIdentifierClick
        end
        object btnAddIdentifier: TButton
          Left = 152
          Top = 208
          Width = 51
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Add'
          TabOrder = 2
          OnClick = btnAddIdentifierClick
        end
        object btnDeleteIdentifier: TButton
          Left = 208
          Top = 208
          Width = 51
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Delete'
          TabOrder = 3
          OnClick = btnDeleteIdentifierClick
        end
        object cbIDField: TComboBox
          Left = 8
          Top = 256
          Width = 121
          Height = 21
          ItemHeight = 13
          TabOrder = 4
          OnChange = cbIDFieldChange
        end
        object btnTypeIdentifier: TButton
          Left = 40
          Top = 208
          Width = 51
          Height = 25
          Caption = 'Type'
          TabOrder = 5
          OnClick = btnTypeIdentifierClick
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'PrimitiveText'
        TabVisible = False
        DesignSize = (
          263
          278)
        object Label10: TLabel
          Left = 8
          Top = 8
          Width = 37
          Height = 13
          Caption = 'Content'
        end
        object reText: TRichEdit
          Left = 8
          Top = 24
          Width = 249
          Height = 225
          Anchors = [akLeft, akTop, akRight]
          PlainText = True
          ScrollBars = ssVertical
          TabOrder = 0
        end
        object chbViewAsHex: TCheckBox
          Left = 8
          Top = 256
          Width = 97
          Height = 17
          Caption = 'View as hex'
          TabOrder = 1
        end
        object btnApplyText: TButton
          Left = 184
          Top = 256
          Width = 75
          Height = 25
          Caption = 'Apply'
          TabOrder = 2
          OnClick = btnApplyTextClick
        end
      end
      object TabSheet5: TTabSheet
        Caption = 'PrimitiveBoolean'
        object Label7: TLabel
          Left = 8
          Top = 8
          Width = 27
          Height = 13
          Caption = 'Value'
        end
        object cbBooleanValue: TComboBox
          Left = 8
          Top = 24
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = cbBooleanValueChange
          Items.Strings = (
            'Empty'
            'True'
            'False')
        end
      end
      object TabSheet6: TTabSheet
        Caption = 'PrimitiveHex'
        DesignSize = (
          263
          278)
        object Label14: TLabel
          Left = 8
          Top = 8
          Width = 37
          Height = 13
          Caption = 'Content'
        end
        object memHex: TMemo
          Left = 8
          Top = 24
          Width = 257
          Height = 225
          Anchors = [akLeft, akTop, akRight]
          ScrollBars = ssVertical
          TabOrder = 0
        end
        object btnEditAsStruct: TButton
          Left = 112
          Top = 256
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Edit As Struct'
          TabOrder = 1
          OnClick = btnEditAsStructClick
        end
        object btnApplyHex: TButton
          Left = 192
          Top = 256
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Apply'
          TabOrder = 2
          OnClick = btnApplyHexClick
        end
      end
      object TabSheet7: TTabSheet
        Caption = 'PrimitiveInteger'
        DesignSize = (
          263
          278)
        object Label15: TLabel
          Left = 8
          Top = 8
          Width = 27
          Height = 13
          Caption = 'Value'
        end
        object memInteger: TMemo
          Left = 8
          Top = 24
          Width = 257
          Height = 177
          Anchors = [akLeft, akTop, akRight]
          ScrollBars = ssVertical
          TabOrder = 0
        end
        object rgShowInteger: TRadioGroup
          Left = 8
          Top = 208
          Width = 137
          Height = 73
          Caption = 'Show'
          ItemIndex = 0
          Items.Strings = (
            'Hexadecimal'
            'Signed decimal'
            'Unsigned decimal')
          TabOrder = 1
          OnClick = rgShowIntegerClick
        end
        object btnApplyInteger: TButton
          Left = 192
          Top = 208
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Apply'
          TabOrder = 2
          OnClick = btnApplyIntegerClick
        end
      end
      object TabSheet8: TTabSheet
        Caption = 'PrimitiveDateTime'
        object Label16: TLabel
          Left = 8
          Top = 8
          Width = 22
          Height = 13
          Caption = 'Year'
        end
        object Label17: TLabel
          Left = 8
          Top = 48
          Width = 30
          Height = 13
          Caption = 'Month'
        end
        object Label18: TLabel
          Left = 8
          Top = 88
          Width = 63
          Height = 13
          Caption = 'Day of month'
        end
        object Label19: TLabel
          Left = 8
          Top = 128
          Width = 23
          Height = 13
          Caption = 'Hour'
        end
        object Label20: TLabel
          Left = 8
          Top = 168
          Width = 32
          Height = 13
          Caption = 'Minute'
        end
        object Label21: TLabel
          Left = 8
          Top = 208
          Width = 42
          Height = 13
          Caption = 'Seconds'
        end
        object lblMilliseconds: TLabel
          Left = 8
          Top = 248
          Width = 57
          Height = 13
          Caption = 'Milliseconds'
          FocusControl = edtMilliseconds
        end
        object edtYear: TEdit
          Left = 8
          Top = 24
          Width = 121
          Height = 21
          TabOrder = 0
        end
        object edtMonth: TEdit
          Left = 8
          Top = 64
          Width = 121
          Height = 21
          TabOrder = 1
        end
        object edtDay: TEdit
          Left = 8
          Top = 104
          Width = 121
          Height = 21
          TabOrder = 2
        end
        object edtHour: TEdit
          Left = 8
          Top = 144
          Width = 121
          Height = 21
          TabOrder = 3
        end
        object edtMinute: TEdit
          Left = 8
          Top = 184
          Width = 121
          Height = 21
          TabOrder = 4
        end
        object edtSeconds: TEdit
          Left = 8
          Top = 224
          Width = 121
          Height = 21
          TabOrder = 5
        end
        object edtMilliseconds: TEdit
          Left = 8
          Top = 264
          Width = 121
          Height = 21
          TabOrder = 6
        end
        object btnApplyDate: TButton
          Left = 184
          Top = 256
          Width = 75
          Height = 25
          Caption = 'Apply'
          TabOrder = 7
          OnClick = btnApplyDateClick
        end
      end
      object TabSheet9: TTabSheet
        Caption = 'PrimitiveReal'
        DesignSize = (
          263
          278)
        object Label23: TLabel
          Left = 8
          Top = 8
          Width = 27
          Height = 13
          Caption = 'Value'
        end
        object edtRealContent: TEdit
          Left = 8
          Top = 24
          Width = 257
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
        object btnApplyReal: TButton
          Left = 192
          Top = 56
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Apply'
          TabOrder = 1
          OnClick = btnApplyRealClick
        end
      end
      object TabSheet10: TTabSheet
        Caption = 'ConstructedOF'
        object btnEditTemplate: TButton
          Left = 8
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Edit Template'
          TabOrder = 0
          OnClick = btnEditTemplateClick
        end
      end
      object TabSheet11: TTabSheet
        Caption = 'PrimitiveObject'
        DesignSize = (
          263
          278)
        object Label12: TLabel
          Left = 8
          Top = 8
          Width = 30
          Height = 13
          Caption = 'Value:'
        end
        object Label22: TLabel
          Left = 8
          Top = 48
          Width = 45
          Height = 13
          Caption = 'Identifies:'
        end
        object cbObject: TComboBox
          Left = 8
          Top = 24
          Width = 257
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          TabOrder = 0
          OnChange = cbObjectChange
        end
        object lbObjectIdentifies: TListBox
          Left = 8
          Top = 64
          Width = 257
          Height = 234
          Anchors = [akLeft, akTop, akRight, akBottom]
          ItemHeight = 13
          TabOrder = 1
        end
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'ASN Module (*.asn)|*.asn|DER encoded file (*.der)|*.der|DOM file' +
      ' (*.dom)|*.dom|Any file (*.*)|*.*'
    Left = 136
    Top = 112
  end
  object SaveDialog1: TSaveDialog
    Filter = 
      'ASN Module (*.asn)|*.asn|DER encoded file (*.der)|*.der|DOM file' +
      ' (*.dom)|*.dom|Any file (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofNoReadOnlyReturn, ofEnableSizing]
    Left = 136
    Top = 144
  end
end
