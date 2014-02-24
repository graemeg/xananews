object fmePost: TfmePost
  Left = 0
  Top = 0
  Width = 663
  Height = 366
  TabOrder = 0
  TabStop = True
  DesignSize = (
    663
    366)
  object Label1: TLabel
    Left = 321
    Top = 315
    Width = 67
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Character Set'
  end
  object Label2: TLabel
    Left = 349
    Top = 338
    Width = 38
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Identity'
  end
  object lbISpellLanguage: TLabel
    Left = 9
    Top = 338
    Width = 47
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Language'
  end
  object ScrollBox1: TScrollBox
    Left = 8
    Top = 0
    Width = 553
    Height = 299
    HorzScrollBar.Tracking = True
    VertScrollBar.Visible = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoScroll = False
    TabOrder = 0
    object Ruler1: TXNRuler
      Left = 0
      Top = 0
      Width = 661
      Height = 10
      BevelInner = bvNone
      BevelOuter = bvNone
      SmallTickSpacing = 5
    end
    object mmoMessage: TExRichEdit
      Left = 0
      Top = 12
      Width = 661
      Height = 280
      RightMargin = 0
      AutoURLDetect = True
      AutoURLExecute = True
      BevelInner = bvNone
      BevelOuter = bvNone
      BevelWidth = 0
      BorderStyle = bsNone
      font.Charset = ANSI_CHARSET
      font.Color = clWindowText
      font.Height = -13
      font.Name = 'Lucida Console'
      font.Style = []
      HideSelection = False
      HideScrollBars = False
      ParentFont = False
      PopupMenu = PopupMenu1
      ScrollBars = ssVertical
      TabOrder = 0
      WantTabs = True
      OnFontChange = mmoMessageFontChange
      OnKeyDown = mmoMessageKeyDown
    end
  end
  object cbCheckSpelling: TCheckBox
    Left = 8
    Top = 311
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Check Spelling'
    TabOrder = 3
  end
  object cbCharset: TComboBox
    Left = 391
    Top = 311
    Width = 169
    Height = 21
    Style = csDropDownList
    Anchors = [akRight, akBottom]
    TabOrder = 5
    OnChange = cbCharsetChange
  end
  object btnCancel: TButton
    Left = 575
    Top = 330
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
    OnClick = btnCancelClick
  end
  object btnOK: TButton
    Left = 575
    Top = 298
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 8
    OnClick = btnOKClick
  end
  object btnAttachments: TButton
    Left = 575
    Top = 32
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Attach...'
    TabOrder = 2
    OnClick = btnAttachmentsClick
  end
  object btnAdvanced: TButton
    Left = 575
    Top = 0
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'A&dvanced...'
    TabOrder = 1
  end
  object btnSpell: TBitBtn
    Left = 112
    Top = 307
    Width = 90
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Check Now'
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF005200005100FF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FF004F00007900006600FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF005201038608019103007500004A
      00FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF02
      55040D9A21099517037706028705005A01FF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FF035A0818AC3D16AC37025D05004B0005840E0374
      07FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0D
      862206680EFF00FFFF00FF036608098F18005201FF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0A87
      1A056D0DFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FF04630A0A801AFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FF076F1204600AFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF035905FF00FF811E00FF00FF
      FF00FF811E00FF00FF811E00811E00811E00FF00FFFF00FFFF00FF811E00811E
      00FF00FF004C00004C00811E00FF00FFFF00FF811E00FF00FF811E00FF00FFFF
      00FF811E00FF00FF811E00FF00FFFF00FF811E00FF00FF004B00811E00811E00
      811E00811E00FF00FF811E00811E00811E00811E00FF00FF811E00FF00FFFF00
      FFFF00FFFF00FFFF00FF811E00FF00FFFF00FF811E00FF00FF811E00FF00FFFF
      00FF811E00FF00FF811E00FF00FFFF00FF811E00FF00FFFF00FFFF00FF811E00
      811E00FF00FFFF00FF811E00811E00811E00FF00FFFF00FFFF00FF811E00811E
      00FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
    TabOrder = 4
    OnClick = btnSpellClick
  end
  object cbIdentity: TComboBox
    Left = 391
    Top = 335
    Width = 169
    Height = 21
    Style = csDropDownList
    Anchors = [akRight, akBottom]
    TabOrder = 6
    OnChange = cbIdentityChange
  end
  object cbISpellLanguage: TComboBox
    Left = 68
    Top = 335
    Width = 133
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 9
  end
  object PopupMenu1: TPopupMenu
    Left = 156
    Top = 32
    object mnuUndo: TMenuItem
      Caption = 'Undo'
      ShortCut = 16474
      OnClick = mnuUndoClick
    end
    object mnuRedo: TMenuItem
      Caption = 'Redo'
      ShortCut = 24666
      OnClick = mnuRedoClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mnuCut: TMenuItem
      Caption = 'Cut'
      ShortCut = 16472
      OnClick = mnuCutClick
    end
    object mnuCopy: TMenuItem
      Caption = 'Copy'
      ShortCut = 16451
      OnClick = mnuCopyClick
    end
    object mnuPaste: TMenuItem
      Caption = 'Paste'
      ShortCut = 16470
      OnClick = mnuPasteClick
    end
    object mnuPasteQuote: TMenuItem
      Caption = 'Paste Quote'
      ShortCut = 24662
      OnClick = mnuPasteQuoteClick
    end
    object mnuPasteSelected: TMenuItem
      Caption = 'Paste Selected Message Pane Text'
      ShortCut = 16468
      OnClick = mnuPasteSelectedClick
    end
    object mnuDelete: TMenuItem
      Caption = 'Delete'
      ShortCut = 16430
    end
    object ROT13SelectedText1: TMenuItem
      Caption = 'ROT-13 Selected Text'
      OnClick = ROT13SelectedText1Click
    end
    object ReverseSelectedText1: TMenuItem
      Caption = 'Reverse Selected Text'
      OnClick = ReverseSelectedText1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object mnuSelectAll: TMenuItem
      Caption = 'Select All'
      ShortCut = 16449
      OnClick = mnuSelectAllClick
    end
  end
  object CWSpellChecker1: TCWSpellChecker
    ExRichEdit = mmoMessage
    Left = 54
    Top = 32
  end
end
