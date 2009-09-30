inherited fmPropertyPageQuoting: TfmPropertyPageQuoting
  Left = 617
  Top = 120
  HelpType = htKeyword
  HelpKeyword = 'QuotingSettings'
  Caption = 'Quoting Settings'
  ClientHeight = 367
  ClientWidth = 402
  Constraints.MinHeight = 367
  Constraints.MinWidth = 402
  ExplicitLeft = 617
  ExplicitTop = 120
  ExplicitWidth = 418
  ExplicitHeight = 403
  PixelsPerInch = 96
  TextHeight = 13
  inherited Bevel2: TBevel
    Top = 327
    Width = 402
    ExplicitTop = 327
    ExplicitWidth = 402
  end
  object Label16: TLabel [1]
    Left = 5
    Top = 60
    Width = 81
    Height = 13
    Caption = '* Start quote with'
  end
  object Label17: TLabel [2]
    Left = 12
    Top = 88
    Width = 126
    Height = 13
    Caption = 'Start each quoted line with'
  end
  object Label18: TLabel [3]
    Left = 5
    Top = 116
    Width = 78
    Height = 13
    Caption = '* End quote with'
  end
  object Label3: TLabel [4]
    Left = 5
    Top = 178
    Width = 350
    Height = 13
    Caption = 
      '* You can use the following macros in the Start quote and End qu' +
      'ote lines:'
  end
  object Label4: TLabel [5]
    Left = 24
    Top = 202
    Width = 46
    Height = 13
    Caption = '%author%'
  end
  object Label5: TLabel [6]
    Left = 24
    Top = 218
    Width = 37
    Height = 13
    Caption = '%date%'
  end
  object Label6: TLabel [7]
    Left = 24
    Top = 234
    Width = 35
    Height = 13
    Caption = '%time%'
  end
  object Label7: TLabel [8]
    Left = 24
    Top = 250
    Width = 66
    Height = 13
    Caption = '%messageid%'
  end
  object Label8: TLabel [9]
    Left = 24
    Top = 266
    Width = 34
    Height = 13
    Caption = '%mail%'
  end
  object Label9: TLabel [10]
    Left = 24
    Top = 282
    Width = 43
    Height = 13
    Caption = '%group%'
  end
  object Label10: TLabel [11]
    Left = 112
    Top = 202
    Width = 169
    Height = 13
    Caption = '- The author of the quoted message'
  end
  object Label11: TLabel [12]
    Left = 112
    Top = 218
    Width = 160
    Height = 13
    Caption = '- The date of the quoted message'
  end
  object Label12: TLabel [13]
    Left = 112
    Top = 234
    Width = 152
    Height = 13
    Caption = '- The time of the quoted messge'
  end
  object Label13: TLabel [14]
    Left = 112
    Top = 250
    Width = 196
    Height = 13
    Caption = '- The '#39'message id'#39' of the quoted message'
  end
  object Label1: TLabel [15]
    Left = 112
    Top = 266
    Width = 246
    Height = 13
    Caption = '- The e-mail address of the quoted message'#39's author'
  end
  object Label14: TLabel [16]
    Left = 112
    Top = 282
    Width = 265
    Height = 13
    Caption = '- The group(s) to which the quoted message was posted'
  end
  object Label15: TLabel [17]
    Left = 5
    Top = 144
    Width = 54
    Height = 13
    Caption = '* Salutation'
  end
  object Label2: TLabel [18]
    Left = 24
    Top = 298
    Width = 60
    Height = 13
    Caption = '%forename%'
  end
  object Label19: TLabel [19]
    Left = 112
    Top = 298
    Width = 193
    Height = 13
    Caption = '- The quoted message author'#39's forename'
  end
  object Label20: TLabel [20]
    Left = 24
    Top = 312
    Width = 52
    Height = 13
    Caption = '%newline%'
  end
  object Label21: TLabel [21]
    Left = 112
    Top = 312
    Width = 95
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
    Top = 337
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
