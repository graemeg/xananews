inherited fmPropertyPageMessagePane: TfmPropertyPageMessagePane
  Left = 516
  Top = 323
  HelpType = htKeyword
  HelpKeyword = 'MessagePaneOptions'
  Caption = 'Message Pane'
  ExplicitLeft = 516
  ExplicitTop = 323
  PixelsPerInch = 96
  TextHeight = 13
  object Label22: TLabel [0]
    Left = 12
    Top = 193
    Width = 63
    Height = 13
    Caption = 'Wrap lines at'
  end
  object Label21: TLabel [1]
    Left = 120
    Top = 193
    Width = 161
    Height = 13
    Caption = 'characters.  (0 = wrap at margin)'
  end
  inherited Panel1: TPanel
    ExplicitWidth = 310
    inherited Bevel1: TBevel
      ExplicitWidth = 310
    end
    inherited stSectionDetails: TLabel
      Caption = 'Options to control how the message pane displays messages'
      ExplicitWidth = 298
    end
  end
  object cbShowDetailsBar: TCheckBox
    Left = 12
    Top = 64
    Width = 193
    Height = 17
    Caption = 'Display Header Details Bar'
    TabOrder = 1
    OnClick = ControlClick
  end
  object cbCheckSpelling: TCheckBox
    Left = 12
    Top = 88
    Width = 97
    Height = 17
    Caption = 'Check Spelling'
    TabOrder = 2
    OnClick = ControlClick
  end
  object cbNOXFaces: TCheckBox
    Left = 12
    Top = 112
    Width = 97
    Height = 17
    Caption = 'No X-Faces'
    TabOrder = 3
    OnClick = ControlClick
  end
  object cbNoHTML: TCheckBox
    Left = 12
    Top = 136
    Width = 129
    Height = 17
    Caption = 'No HTML Decoding'
    TabOrder = 4
    OnClick = ControlClick
  end
  object edWrapLines: TEdit
    Left = 84
    Top = 190
    Width = 33
    Height = 21
    TabOrder = 6
    Text = '0'
    OnChange = edWrapLinesChange
  end
  object cbStrictSigSep: TCheckBox
    Left = 12
    Top = 160
    Width = 157
    Height = 17
    Caption = 'Strict Signature Separators'
    TabOrder = 5
    OnClick = ControlClick
  end
end
