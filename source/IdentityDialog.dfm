object dlgIdentity: TdlgIdentity
  Left = 673
  Top = 166
  ActiveControl = edName
  ClientHeight = 464
  ClientWidth = 607
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 379
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    607
    464)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 18
    Width = 68
    Height = 13
    Caption = '&Identity Name'
  end
  object Label2: TLabel
    Left = 16
    Top = 42
    Width = 52
    Height = 13
    Caption = '&Your Name'
  end
  object Label3: TLabel
    Left = 16
    Top = 66
    Width = 61
    Height = 13
    Caption = '&Organization'
  end
  object Label4: TLabel
    Left = 16
    Top = 90
    Width = 70
    Height = 13
    Caption = 'E-Mail Address'
  end
  object Label5: TLabel
    Left = 16
    Top = 114
    Width = 84
    Height = 13
    Caption = 'Reply To Address'
  end
  object Label14: TLabel
    Left = 16
    Top = 139
    Width = 46
    Height = 13
    Caption = 'Signature'
    Transparent = True
  end
  object Label37: TLabel
    Left = 16
    Top = 380
    Width = 33
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'X-Face'
  end
  object Label6: TLabel
    Left = 208
    Top = 364
    Width = 383
    Height = 49
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 
      'An X-Face is a small monochrome picture that is sent along with ' +
      'your messages.  '
    WordWrap = True
  end
  object Label7: TLabel
    Left = 16
    Top = 254
    Width = 247
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'You can use the following macros in your signature:'
  end
  object Label8: TLabel
    Left = 24
    Top = 270
    Width = 453
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 
      '%author% is replaced with the Author, %datetime% is replaced wit' +
      'h the current date && time,'
  end
  object Label9: TLabel
    Left = 24
    Top = 286
    Width = 225
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '%ver% is replaced with the XanaNews version'
  end
  object Label10: TLabel
    Left = 16
    Top = 332
    Width = 65
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Signature File'
  end
  object Label11: TLabel
    Left = 24
    Top = 302
    Width = 405
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 
      '%sigfile% is replaced with a  (random) signature/quote from the ' +
      'below signature file'
  end
  object btnOK: TButton
    Left = 438
    Top = 430
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 11
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 518
    Top = 430
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 12
  end
  object edName: TEdit
    Left = 120
    Top = 14
    Width = 471
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'edName'
  end
  object edUserName: TEdit
    Left = 120
    Top = 38
    Width = 471
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'edUserName'
  end
  object edOrganization: TEdit
    Left = 120
    Top = 62
    Width = 471
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Text = 'Edit2'
  end
  object edEMailAddress: TEdit
    Left = 120
    Top = 86
    Width = 471
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = 'Edit2'
  end
  object edReplyAddress: TEdit
    Left = 120
    Top = 110
    Width = 471
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    Text = 'Edit2'
    OnEnter = edReplyAddressEnter
  end
  object mmoSignature: TMemo
    Left = 16
    Top = 155
    Width = 575
    Height = 91
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object Panel1: TPanel
    Left = 56
    Top = 364
    Width = 50
    Height = 50
    Anchors = [akLeft, akBottom]
    AutoSize = True
    BevelOuter = bvLowered
    TabOrder = 8
    object imgXFace: TImage
      Left = 1
      Top = 1
      Width = 48
      Height = 48
      PopupMenu = pomXFace
    end
  end
  object btnLoadXFace: TButton
    Left = 112
    Top = 362
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Load...'
    TabOrder = 9
    OnClick = btnLoadXFaceClick
  end
  object btnClearXFace: TButton
    Left = 112
    Top = 392
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Clear'
    TabOrder = 10
    OnClick = btnClearXFaceClick
  end
  object edSigFile: TEdit
    Left = 120
    Top = 328
    Width = 447
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 6
  end
  object btnSigFile: TButton
    Left = 574
    Top = 326
    Width = 25
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '...'
    TabOrder = 7
    OnClick = btnSigFileClick
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 130
    Top = 406
  end
  object pomXFace: TPopupMenu
    Left = 94
    Top = 406
    object actXFaceCopy: TMenuItem
      Caption = 'Copy'
      ShortCut = 16451
      OnClick = actXFaceCopyClick
    end
    object actXFaceCopyAsText: TMenuItem
      Caption = 'Copy as Text'
      ShortCut = 49219
      OnClick = actXFaceCopyAsTextClick
    end
    object actXFacePaste: TMenuItem
      Caption = 'Paste'
      ShortCut = 16470
      OnClick = actXFacePasteClick
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'txt'
    Filter = 
      'Text Files (*.txt)|*.txt|Signature files (*.sig)|*.sig|Any File ' +
      '(*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 166
    Top = 406
  end
end
