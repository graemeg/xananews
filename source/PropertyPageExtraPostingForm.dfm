inherited fmPropertyPageExtraPosting: TfmPropertyPageExtraPosting
  Left = 338
  Top = 214
  HelpType = htKeyword
  HelpKeyword = 'PostedMessageHeaders'
  Caption = 'Posted Message Headers'
  ClientWidth = 384
  ExplicitLeft = 338
  ExplicitTop = 214
  ExplicitWidth = 400
  ExplicitHeight = 450
  PixelsPerInch = 96
  TextHeight = 13
  inherited Bevel2: TBevel
    Width = 384
    ExplicitTop = 357
    ExplicitWidth = 315
  end
  object Label1: TLabel [1]
    Left = 28
    Top = 184
    Width = 81
    Height = 13
    Caption = 'Message ID Stub'
  end
  object Label2: TLabel [2]
    Left = 28
    Top = 208
    Width = 35
    Height = 13
    Caption = 'Domain'
  end
  object Label5: TLabel [3]
    Left = 12
    Top = 232
    Width = 6
    Height = 13
    Caption = '*'
  end
  object Label3: TLabel [4]
    Left = 28
    Top = 232
    Width = 347
    Height = 43
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'nb.  XanaNews will always generate message IDs if '#39'Archive Poste' +
      'd Messages'#39' is selected on the Posting Settings tab'
    WordWrap = True
    ExplicitWidth = 278
  end
  object Label38: TLabel [5]
    Left = 12
    Top = 280
    Width = 69
    Height = 13
    Caption = 'Extra Headers'
  end
  object Label4: TLabel [6]
    Left = 12
    Top = 60
    Width = 38
    Height = 13
    Caption = 'Identity'
  end
  object Label6: TLabel [7]
    Left = 176
    Top = 136
    Width = 125
    Height = 13
    Caption = 'when cancelling messages'
  end
  inherited Panel1: TPanel
    Width = 384
    ExplicitWidth = 315
    inherited Bevel1: TBevel
      Width = 384
      ExplicitWidth = 315
    end
    inherited stSectionDetails: TLabel
      Width = 372
      ExplicitWidth = 303
    end
  end
  object cbNoArchive: TCheckBox [9]
    Left = 12
    Top = 86
    Width = 157
    Height = 17
    Alignment = taLeftJustify
    Caption = 'No Archive'
    TabOrder = 1
    OnClick = controlClick
  end
  object cbGenerateDateHeaders: TCheckBox [10]
    Left = 12
    Top = 110
    Width = 157
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Generate Date Headers'
    TabOrder = 2
    OnClick = controlClick
  end
  object cbGenerateMessageIDs: TCheckBox [11]
    Left = 12
    Top = 158
    Width = 157
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Generate Message IDs'
    TabOrder = 3
    OnClick = controlClick
  end
  object edMessageIDStub: TEdit [12]
    Left = 156
    Top = 180
    Width = 83
    Height = 21
    TabOrder = 4
    OnChange = edMessageIDStubChange
  end
  object edDomain: TEdit [13]
    Left = 156
    Top = 204
    Width = 83
    Height = 21
    TabOrder = 5
    OnChange = edMessageIDStubChange
  end
  object lbXHeaders: TListBox [14]
    Left = 12
    Top = 296
    Width = 283
    Height = 71
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 6
  end
  object btnModifyXHeaders: TButton [15]
    Left = 301
    Top = 296
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Modify...'
    TabOrder = 7
    OnClick = btnModifyXHeadersClick
  end
  inherited btnReset: TButton
    Left = 273
    Top = 384
    TabOrder = 8
    ExplicitLeft = 204
    ExplicitTop = 367
  end
  object cbIdentities: TComboBox
    Left = 60
    Top = 56
    Width = 226
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 9
    OnChange = cbIdentitiesChange
  end
  object btnNewIdentity: TButton
    Left = 297
    Top = 54
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'New...'
    TabOrder = 10
    OnClick = btnNewIdentityClick
  end
  object cbGenerateApprovedHeaders: TCheckBox
    Left = 12
    Top = 134
    Width = 157
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Generate Approved Headers'
    TabOrder = 11
    OnClick = controlClick
  end
end
