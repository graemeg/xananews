inherited fmPropertyPageMailAccountServer: TfmPropertyPageMailAccountServer
  Caption = 'Server Settings'
  ClientHeight = 223
  ClientWidth = 337
  Constraints.MinHeight = 223
  Constraints.MinWidth = 337
  ExplicitWidth = 345
  ExplicitHeight = 257
  PixelsPerInch = 96
  TextHeight = 13
  object stServerAccountName: TLabel [0]
    Left = 28
    Top = 136
    Width = 71
    Height = 13
    Caption = 'Account Name'
    Transparent = True
  end
  object stServerAccountPassword: TLabel [1]
    Left = 28
    Top = 160
    Width = 46
    Height = 13
    Caption = 'Password'
    Transparent = True
  end
  object Label9: TLabel [2]
    Left = 12
    Top = 60
    Width = 188
    Height = 13
    Caption = 'Server Name (Host name or IP Address)'
    Transparent = True
  end
  object stRetypePassword: TLabel [3]
    Left = 28
    Top = 182
    Width = 83
    Height = 13
    Caption = 'Retype Password'
    Transparent = True
  end
  object stPasswordError: TLabel [4]
    Left = 120
    Top = 200
    Width = 108
    Height = 13
    Caption = 'Passwords must match'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  inherited Panel1: TPanel
    Width = 337
    ExplicitWidth = 337
    inherited Bevel1: TBevel
      Width = 337
      ExplicitWidth = 337
    end
    inherited stSectionDetails: TLabel
      Width = 325
      Caption = 
        'This section contains the mail server name and server account se' +
        'ttings'
      ExplicitWidth = 325
    end
  end
  object edServerAccountName: TEdit
    Left = 120
    Top = 128
    Width = 197
    Height = 21
    TabOrder = 2
    OnChange = edServerNameChange
  end
  object edServerAccountPassword: TEdit
    Left = 120
    Top = 152
    Width = 197
    Height = 21
    TabOrder = 3
    OnChange = edServerAccountPasswordChange
  end
  object edServerName: TEdit
    Left = 12
    Top = 76
    Width = 313
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = edServerNameChange
  end
  object edRetypePassword: TEdit
    Left = 120
    Top = 178
    Width = 197
    Height = 21
    PasswordChar = '*'
    TabOrder = 4
    OnChange = edServerAccountPasswordChange
  end
end
