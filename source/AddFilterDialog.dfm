object dlgAddFilter: TdlgAddFilter
  Left = 372
  Top = 303
  ActiveControl = edFilterName
  BorderStyle = bsDialog
  Caption = 'Add Filter'
  ClientHeight = 214
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object stBlurb: TLabel
    Left = 16
    Top = 16
    Width = 281
    Height = 73
    AutoSize = False
    Caption = 
      'This will add the filter '#39'%s'#39' to the list of XanaNews filters.  ' +
      'The filter will not become active until you enable it - either f' +
      'or an individual news group or an entire news server.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 16
    Top = 112
    Width = 58
    Height = 13
    Caption = 'Filter Name:'
  end
  object OKBtn: TButton
    Left = 143
    Top = 172
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 223
    Top = 172
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edFilterName: TEdit
    Left = 16
    Top = 128
    Width = 289
    Height = 21
    TabOrder = 2
    OnChange = edFilterNameChange
  end
end
