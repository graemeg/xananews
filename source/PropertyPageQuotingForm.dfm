inherited fmPropertyPageQuoting: TfmPropertyPageQuoting
  Left = 617
  Top = 120
  HelpType = htKeyword
  HelpKeyword = 'QuotingSettings'
  Caption = 'Quoting Settings'
  ClientWidth = 402
  ExplicitLeft = 617
  ExplicitTop = 120
  ExplicitWidth = 418
  PixelsPerInch = 96
  TextHeight = 13
  inherited Bevel2: TBevel
    Width = 402
    ExplicitTop = 327
    ExplicitWidth = 402
  end
  object Label16: TLabel [1]
    Left = 5
    Top = 60
    Width = 87
    Height = 13
    Caption = '* Start quote with'
  end
  object Label17: TLabel [2]
    Left = 12
    Top = 88
    Width = 129
    Height = 13
    Caption = 'Start each quoted line with'
  end
  object Label18: TLabel [3]
    Left = 5
    Top = 116
    Width = 81
    Height = 13
    Caption = '* End quote with'
  end
  object Label3: TLabel [4]
    Left = 5
    Top = 178
    Width = 357
    Height = 13
    Caption = 
      '* You can use the following macros in the Start quote and End qu' +
      'ote lines:'
  end
  object Label4: TLabel [5]
    Left = 22
    Top = 197
    Width = 54
    Height = 13
    Caption = '%author%'
  end
  object Label5: TLabel [6]
    Left = 22
    Top = 213
    Width = 44
    Height = 13
    Caption = '%date%'
  end
  object Label6: TLabel [7]
    Left = 22
    Top = 229
    Width = 42
    Height = 13
    Caption = '%time%'
  end
  object Label7: TLabel [8]
    Left = 22
    Top = 245
    Width = 72
    Height = 13
    Caption = '%messageid%'
  end
  object Label8: TLabel [9]
    Left = 22
    Top = 261
    Width = 40
    Height = 13
    Caption = '%mail%'
  end
  object Label9: TLabel [10]
    Left = 22
    Top = 277
    Width = 50
    Height = 13
    Caption = '%group%'
  end
  object Label10: TLabel [11]
    Left = 110
    Top = 197
    Width = 174
    Height = 13
    Caption = '- The author of the quoted message'
  end
  object Label11: TLabel [12]
    Left = 110
    Top = 213
    Width = 164
    Height = 13
    Caption = '- The date of the quoted message'
  end
  object Label12: TLabel [13]
    Left = 110
    Top = 229
    Width = 156
    Height = 13
    Caption = '- The time of the quoted messge'
  end
  object Label13: TLabel [14]
    Left = 110
    Top = 245
    Width = 199
    Height = 13
    Caption = '- The '#39'message id'#39' of the quoted message'
  end
  object Label1: TLabel [15]
    Left = 110
    Top = 261
    Width = 253
    Height = 13
    Caption = '- The e-mail address of the quoted message'#39's author'
  end
  object Label14: TLabel [16]
    Left = 110
    Top = 277
    Width = 271
    Height = 13
    Caption = '- The group(s) to which the quoted message was posted'
  end
  object Label15: TLabel [17]
    Left = 5
    Top = 144
    Width = 57
    Height = 13
    Caption = '* Salutation'
  end
  object Label2: TLabel [18]
    Left = 22
    Top = 293
    Width = 68
    Height = 13
    Caption = '%forename%'
  end
  object Label19: TLabel [19]
    Left = 110
    Top = 293
    Width = 198
    Height = 13
    Caption = '- The quoted message author'#39's forename'
  end
  object Label20: TLabel [20]
    Left = 22
    Top = 309
    Width = 58
    Height = 13
    Caption = '%newline%'
  end
  object Label21: TLabel [21]
    Left = 110
    Top = 309
    Width = 100
    Height = 13
    Caption = '- Inserts a line-break'
  end
  inherited Panel1: TPanel
    Width = 402
    ExplicitWidth = 402
    inherited Bevel1: TBevel
      Width = 402
      ExplicitWidth = 402
    end
    inherited stSectionDetails: TLabel
      Width = 390
      ExplicitWidth = 390
    end
  end
  inherited btnReset: TButton
    Left = 291
    Top = 384
    TabOrder = 5
    ExplicitLeft = 291
    ExplicitTop = 337
  end
  object edQuoteHeader: TEdit
    Left = 148
    Top = 56
    Width = 238
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = '%author% wrote:'
    OnChange = edQuoteHeaderChange
  end
  object edQuoteLineMarker: TEdit
    Left = 148
    Top = 84
    Width = 238
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Text = '> '
    OnChange = edQuoteHeaderChange
  end
  object edQuoteFooter: TEdit
    Left = 148
    Top = 112
    Width = 238
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    OnChange = edQuoteHeaderChange
  end
  object edSalutation: TEdit
    Left = 148
    Top = 140
    Width = 238
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    OnChange = edQuoteHeaderChange
  end
end
