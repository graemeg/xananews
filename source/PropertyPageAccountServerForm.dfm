inherited fmPropertyPageAccountServer: TfmPropertyPageAccountServer
  HelpType = htKeyword
  HelpKeyword = 'AccountServerSettings'
  Caption = 'Server Settings'
  DesignSize = (
    384
    414)
  PixelsPerInch = 96
  TextHeight = 13
  object Label9: TLabel [0]
    Left = 12
    Top = 60
    Width = 192
    Height = 13
    Caption = 'Server Name (Host name or IP Address)'
    Transparent = True
  end
  object Label2: TLabel [1]
    Left = 12
    Top = 268
    Width = 170
    Height = 13
    Caption = 'Last greeting received from server:'
  end
  object stServerAccountName: TLabel [2]
    Left = 28
    Top = 147
    Width = 52
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
    Width = 84
    Height = 13
    Caption = 'Retype Password'
    Transparent = True
  end
  object stPasswordError: TLabel [5]
    Left = 120
    Top = 216
    Width = 109
    Height = 13
    Caption = 'Passwords must match'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  inherited Panel1: TPanel
    ExplicitWidth = 340
    DesignSize = (
      384
      41)
    inherited Bevel1: TBevel
      ExplicitWidth = 340
    end
    inherited stSectionDetails: TLabel
      Caption = 
        'This section contains the server name and server account setting' +
        's'
      ExplicitWidth = 328
    end
  end
  object edServerName: TEdit
    Left = 12
    Top = 76
    Width = 357
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = edServerNameChange
    ExplicitWidth = 313
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
    Width = 349
    Height = 119
    TabStop = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvNone
    BevelKind = bkTile
    BorderStyle = bsNone
    ParentColor = True
    ReadOnly = True
    TabOrder = 7
    ExplicitWidth = 305
    ExplicitHeight = 63
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
