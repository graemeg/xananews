object dlgBozoDetails: TdlgBozoDetails
  Left = 447
  Top = 361
  BorderStyle = bsDialog
  Caption = 'Bozo Details'
  ClientHeight = 324
  ClientWidth = 368
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    368
    324)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 27
    Height = 13
    Caption = '&Name'
  end
  object Label2: TLabel
    Left = 16
    Top = 48
    Width = 70
    Height = 13
    Caption = '&E-Mail Address'
  end
  object Label3: TLabel
    Left = 16
    Top = 254
    Width = 78
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Date Registered'
  end
  object Label4: TLabel
    Left = 16
    Top = 80
    Width = 54
    Height = 13
    Caption = 'Key Phrase'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 236
    Width = 347
    Height = 3
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsBottomLine
  end
  object Label5: TLabel
    Left = 16
    Top = 112
    Width = 30
    Height = 13
    Caption = 'Action'
  end
  object edName: TEdit
    Left = 104
    Top = 12
    Width = 227
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object edEMail: TEdit
    Left = 104
    Top = 44
    Width = 227
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object dpDate: TDateTimePicker
    Left = 104
    Top = 250
    Width = 121
    Height = 21
    Anchors = [akLeft, akBottom]
    Date = 37748.955889849540000000
    Time = 37748.955889849540000000
    TabOrder = 6
  end
  object btnOK: TButton
    Left = 199
    Top = 283
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 7
  end
  object btnCancel: TButton
    Left = 279
    Top = 283
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object cbUseName: TCheckBox
    Left = 339
    Top = 12
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object cbUseEmail: TCheckBox
    Left = 339
    Top = 44
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object cbUseKeyphrase: TCheckBox
    Left = 339
    Top = 76
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 5
  end
  object edKeyphrase: TEdit
    Left = 104
    Top = 78
    Width = 227
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
  end
  object rbIgnore: TRadioButton
    Left = 104
    Top = 112
    Width = 249
    Height = 17
    Caption = '&Ignore Messages from this Bozo'
    Checked = True
    TabOrder = 9
    TabStop = True
  end
  object rbMarkAsRead: TRadioButton
    Left = 104
    Top = 136
    Width = 249
    Height = 17
    Caption = '&Mark as Read Messages from this Bozo'
    TabOrder = 10
  end
  object rbIgnoreThread: TRadioButton
    Left = 104
    Top = 160
    Width = 249
    Height = 17
    Caption = 'I&gnore Branches Started by this Bozo'
    TabOrder = 11
  end
  object rbMarkAsReadThread: TRadioButton
    Left = 104
    Top = 184
    Width = 249
    Height = 17
    Caption = 'Mar&k as Read Branches Started by this Bozo'
    TabOrder = 12
  end
  object rbDontDownload: TRadioButton
    Left = 104
    Top = 208
    Width = 249
    Height = 17
    Caption = '&Don'#39't Download Messages from this Bozo'
    TabOrder = 13
  end
end
