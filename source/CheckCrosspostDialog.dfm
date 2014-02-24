object dlgCheckCrosspost: TdlgCheckCrosspost
  Left = 230
  Top = 227
  BorderStyle = bsDialog
  Caption = 'Crosspost Warning'
  ClientHeight = 166
  ClientWidth = 440
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 301
    Height = 13
    Caption = 'You are attempting to crosspost to three or more newsgroups.'
  end
  object rbGoAhead: TRadioButton
    Left = 32
    Top = 48
    Width = 393
    Height = 17
    Caption = 'Go ahead.  Post to the groups I'#39've specified'
    Checked = True
    TabOrder = 0
    TabStop = True
  end
  object rbFollowup: TRadioButton
    Left = 32
    Top = 72
    Width = 385
    Height = 17
    Caption = 
      'Post to the groups I'#39've specified, but set followups to just the' +
      ' first group'
    TabOrder = 1
  end
  object rbFirstGroupOnly: TRadioButton
    Left = 32
    Top = 96
    Width = 385
    Height = 17
    Caption = 'Oops!  Just post to the first group in the list.'
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 272
    Top = 128
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 352
    Top = 128
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
