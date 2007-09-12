inherited fmPropertyPageAccountServer: TfmPropertyPageAccountServer
  HelpType = htKeyword
  HelpKeyword = 'AccountServerSettings'
  Caption = 'Server Settings'
  ClientHeight = 358
  ClientWidth = 340
  Constraints.MinHeight = 358
  Constraints.MinWidth = 340
  DesignSize = (
    340
    358)
  PixelsPerInch = 96
  TextHeight = 13
  object Label9: TLabel [0]
    Left = 12
    Top = 60
    Width = 188
    Height = 13
    Caption = 'Server Name (Host name or IP Address)'
    Transparent = True
  end
  object Label2: TLabel [1]
    Left = 12
    Top = 268
    Width = 163
    Height = 13
    Caption = 'Last greeting received from server:'
  end
  object stServerAccountName: TLabel [2]
    Left = 28
    Top = 147
    Width = 53
    Height = 13
    Caption = 'User Name'
    Transparent = True
  end
  object stServerAccountPassword: TLabel [3]
    Left = 28
    Top = 171
    Width = 46
    Height = 13
    Caption = 'Password'
    Transparent = True
  end
  object stRetypePassword: TLabel [4]
    Left = 28
    Top = 195
    Width = 83
    Height = 13
    Caption = 'Retype Password'
    Transparent = True
  end
  object stPasswordError: TLabel [5]
    Left = 120
    Top = 216
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
    Width = 340
    DesignSize = (
      340
      41)
    inherited Bevel1: TBevel
      Width = 340
    end
    inherited stSectionDetails: TLabel
      Width = 328
      Caption = 
        'This section contains the server name and server account setting' +
        's'
    end
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
  object cbLogonRequired: TCheckBox
    Left = 12
    Top = 116
    Width = 305
    Height = 17
    Caption = 'This server requires me to log on'
    TabOrder = 2
    OnClick = cbLogonRequiredClick
  end
  object edServerAccountName: TEdit
    Left = 120
    Top = 144
    Width = 197
    Height = 21
    TabOrder = 3
    OnChange = edServerNameChange
  end
  object edServerAccountPassword: TEdit
    Left = 120
    Top = 168
    Width = 197
    Height = 21
    TabOrder = 4
    OnChange = edServerAccountPasswordChange
  end
  object cbAlwaysAuthenticate: TCheckBox
    Left = 28
    Top = 239
    Width = 137
    Height = 17
    Caption = 'Always Authenticate'
    TabOrder = 6
    OnClick = cbLogonRequiredClick
  end
  object stGreeting: TMemo
    Left = 12
    Top = 284
    Width = 305
    Height = 63
    TabStop = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvNone
    BevelKind = bkTile
    BorderStyle = bsNone
    ParentColor = True
    ReadOnly = True
    TabOrder = 7
  end
  object edRetypePassword: TEdit
    Left = 120
    Top = 192
    Width = 197
    Height = 21
    PasswordChar = '*'
    TabOrder = 5
    OnChange = edServerAccountPasswordChange
  end
end
