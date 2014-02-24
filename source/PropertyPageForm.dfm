object fmPropertyPage: TfmPropertyPage
  Left = 335
  Top = 221
  BorderStyle = bsNone
  Caption = 'fmPropertyPage'
  ClientHeight = 414
  ClientWidth = 384
  Color = clBtnFace
  Constraints.MinHeight = 450
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 384
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 373
    DesignSize = (
      384
      41)
    object Bevel1: TBevel
      Left = 0
      Top = 38
      Width = 384
      Height = 3
      Align = alBottom
      Shape = bsBottomLine
      ExplicitWidth = 373
    end
    object stSectionDetails: TLabel
      Left = 8
      Top = 6
      Width = 372
      Height = 29
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      ShowAccelChar = False
      WordWrap = True
      ExplicitWidth = 361
    end
  end
end
