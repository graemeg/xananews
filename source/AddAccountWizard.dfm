object fmAddAccountWizard: TfmAddAccountWizard
  Left = 0
  Top = 0
  Caption = 'Create New Account'
  ClientHeight = 298
  ClientWidth = 439
  Color = clBtnFace
  Constraints.MinHeight = 325
  Constraints.MinWidth = 405
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    439
    298)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 81
    Height = 81
  end
  object btnBack: TButton
    Left = 186
    Top = 265
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Back'
    TabOrder = 0
    OnClick = btnBackClick
  end
  object btnNext: TButton
    Left = 266
    Top = 265
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Next'
    TabOrder = 1
    OnClick = btnNextClick
  end
  object btnCancel: TButton
    Left = 346
    Top = 265
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object PageControl1: TPageControl
    Left = 96
    Top = 8
    Width = 336
    Height = 247
    ActivePage = TabSheet3
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    TabPosition = tpBottom
    OnChange = PageControl1Change
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        328
        239)
      object Label1: TLabel
        Left = 8
        Top = 16
        Width = 106
        Height = 18
        Caption = 'Account Name'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label2: TLabel
        Left = 8
        Top = 48
        Width = 309
        Height = 33
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'The Account Name is the descriptive name that appears in XanaNew' +
          's in the Accounts and Groups tree.'
        WordWrap = True
      end
      object Label3: TLabel
        Left = 8
        Top = 88
        Width = 309
        Height = 33
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Eg. '#39'Demon Internet'#39' or  '#39'Microsoft Technical Groups'#39
        WordWrap = True
      end
      object Label4: TLabel
        Left = 8
        Top = 128
        Width = 121
        Height = 13
        Caption = 'Enter the Account Name:'
      end
      object edAccountName: TEdit
        Left = 8
        Top = 144
        Width = 304
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        328
        239)
      object Label5: TLabel
        Left = 8
        Top = 16
        Width = 98
        Height = 18
        Caption = 'Server Name'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label6: TLabel
        Left = 8
        Top = 48
        Width = 309
        Height = 33
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'The Server is the Internet Name or IP Address of the account'#39's s' +
          'erver.'
        WordWrap = True
      end
      object Label7: TLabel
        Left = 8
        Top = 88
        Width = 309
        Height = 33
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Eg. news.demon.net, msnws.microsoft.com or 10.192.63.42'
        WordWrap = True
      end
      object Label8: TLabel
        Left = 8
        Top = 128
        Width = 114
        Height = 13
        Caption = 'Enter the Server Name:'
      end
      object edServerName: TEdit
        Left = 8
        Top = 144
        Width = 304
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'TabSheet3'
      ImageIndex = 2
      TabVisible = False
      DesignSize = (
        328
        239)
      object Label9: TLabel
        Left = 8
        Top = 16
        Width = 120
        Height = 18
        Caption = 'Security Details'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label10: TLabel
        Left = 8
        Top = 48
        Width = 309
        Height = 33
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Some servers require you to log on with a User Name and Password'
        WordWrap = True
      end
      object stPasswordError: TLabel
        Left = 120
        Top = 216
        Width = 109
        Height = 13
        Caption = 'Passwords must match'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentFont = False
        Visible = False
      end
      object cbLogonRequired: TCheckBox
        Left = 6
        Top = 88
        Width = 177
        Height = 17
        Alignment = taLeftJustify
        Caption = 'The server requires me to log on'
        TabOrder = 0
      end
      object pnlLogOnDetails: TPanel
        Left = 0
        Top = 112
        Width = 353
        Height = 97
        BevelOuter = bvNone
        TabOrder = 1
        Visible = False
        object Label11: TLabel
          Left = 24
          Top = 16
          Width = 52
          Height = 13
          Caption = 'User Name'
        end
        object Label12: TLabel
          Left = 24
          Top = 48
          Width = 46
          Height = 13
          Caption = 'Password'
        end
        object stRetypePassword: TLabel
          Left = 24
          Top = 80
          Width = 84
          Height = 13
          Caption = 'Retype Password'
        end
        object edUserName: TEdit
          Left = 120
          Top = 12
          Width = 145
          Height = 21
          TabOrder = 0
        end
        object edPassword: TEdit
          Left = 120
          Top = 44
          Width = 145
          Height = 21
          TabOrder = 1
        end
        object edRetypePassword: TEdit
          Left = 120
          Top = 76
          Width = 145
          Height = 21
          PasswordChar = '*'
          TabOrder = 2
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'TabSheet4'
      ImageIndex = 3
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        328
        239)
      object Label13: TLabel
        Left = 8
        Top = 16
        Width = 68
        Height = 18
        Caption = 'Finished!'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label14: TLabel
        Left = 8
        Top = 48
        Width = 309
        Height = 49
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'XanaNews has enough information to create your new account.  If ' +
          'you need to alter the details, or change more advanced settings,' +
          ' select Account Properties'
        WordWrap = True
      end
    end
  end
end
