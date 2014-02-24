object frmNewUserWizard: TfrmNewUserWizard
  Left = 414
  Top = 177
  BorderStyle = bsDialog
  Caption = 'Welcome to XanaNews'
  ClientHeight = 388
  ClientWidth = 605
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    605
    388)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 16
    Top = 12
    Width = 105
    Height = 105
  end
  object PageControl1: TPageControl
    Left = 128
    Top = 8
    Width = 467
    Height = 337
    ActivePage = tsConnection
    Anchors = [akLeft, akTop, akRight, akBottom]
    Style = tsFlatButtons
    TabOrder = 0
    OnChange = PageControl1Change
    object tsUserDetails: TTabSheet
      Caption = 'tsUserDetails'
      TabVisible = False
      object Label2: TLabel
        Left = 24
        Top = 108
        Width = 53
        Height = 13
        Caption = 'Your Name'
      end
      object Label3: TLabel
        Left = 24
        Top = 164
        Width = 95
        Height = 13
        Caption = 'Your E-Mail Address'
      end
      object Label1: TLabel
        Left = 8
        Top = 24
        Width = 225
        Height = 24
        Caption = 'Welcome to XanaNews!'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label4: TLabel
        Left = 8
        Top = 68
        Width = 229
        Height = 13
        Caption = 'Please enter the following details about yourself:-'
      end
      object Label9: TLabel
        Left = 144
        Top = 128
        Width = 87
        Height = 13
        Caption = 'Type in your name'
      end
      object Label10: TLabel
        Left = 144
        Top = 184
        Width = 200
        Height = 13
        Caption = 'Type in the E-Mail address you usually use'
      end
      object edYourName: TEdit
        Left = 144
        Top = 104
        Width = 305
        Height = 21
        TabOrder = 0
      end
      object edYourEMail: TEdit
        Left = 144
        Top = 160
        Width = 305
        Height = 21
        TabOrder = 1
      end
    end
    object tsServerDetails: TTabSheet
      Caption = 'tsServerDetails'
      ImageIndex = 1
      TabVisible = False
      object Label5: TLabel
        Left = 24
        Top = 164
        Width = 62
        Height = 13
        Caption = 'Server Name'
      end
      object Label6: TLabel
        Left = 24
        Top = 108
        Width = 105
        Height = 13
        Caption = 'Server Account Name'
      end
      object Label7: TLabel
        Left = 8
        Top = 68
        Width = 380
        Height = 13
        Caption = 
          'Please enter the following details about the news server you wan' +
          't to connect to:-'
      end
      object Label8: TLabel
        Left = 8
        Top = 24
        Width = 225
        Height = 24
        Caption = 'Welcome to XanaNews!'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label11: TLabel
        Left = 144
        Top = 128
        Width = 308
        Height = 13
        Caption = 
          'Type in a friendly name for the server - eg. '#39'Demon Internet New' +
          's'#39
      end
      object Label12: TLabel
        Left = 144
        Top = 184
        Width = 292
        Height = 13
        Caption = 'Type in the Internet name of the server - eg. '#39'news.demon.net'#39
      end
      object edAccountName: TEdit
        Left = 144
        Top = 104
        Width = 305
        Height = 21
        TabOrder = 0
      end
      object edServerName: TEdit
        Left = 144
        Top = 160
        Width = 305
        Height = 21
        TabOrder = 1
      end
    end
    object tsServerLogon: TTabSheet
      Caption = 'tsServerLogon'
      ImageIndex = 2
      TabVisible = False
      object Label13: TLabel
        Left = 8
        Top = 24
        Width = 225
        Height = 24
        Caption = 'Welcome to XanaNews!'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label14: TLabel
        Left = 8
        Top = 68
        Width = 421
        Height = 13
        Caption = 
          'Some servers require a user name and password.  Please enter the' +
          ' details here if required'
      end
      object sb1: TLabel
        Left = 40
        Top = 140
        Width = 102
        Height = 13
        Caption = 'User Name for Server'
        Enabled = False
      end
      object sb2: TLabel
        Left = 168
        Top = 160
        Width = 281
        Height = 33
        AutoSize = False
        Caption = 
          'Type in the user name required when connecting to your news serv' +
          'er'
        Enabled = False
        WordWrap = True
      end
      object sb3: TLabel
        Left = 40
        Top = 212
        Width = 95
        Height = 13
        Caption = 'Password for Server'
        Enabled = False
      end
      object sb4: TLabel
        Left = 168
        Top = 232
        Width = 281
        Height = 33
        AutoSize = False
        Caption = 
          'Type in the password required when connecting to your news serve' +
          'r'
        Enabled = False
        WordWrap = True
      end
      object edServerUserName: TEdit
        Left = 168
        Top = 136
        Width = 281
        Height = 21
        Enabled = False
        TabOrder = 1
      end
      object edServerPassword: TEdit
        Left = 168
        Top = 208
        Width = 281
        Height = 21
        Enabled = False
        TabOrder = 2
      end
      object cbServerLogon: TCheckBox
        Left = 24
        Top = 104
        Width = 281
        Height = 17
        Alignment = taLeftJustify
        Caption = 'My news server requires a user name and password'
        TabOrder = 0
        OnClick = cbServerLogonClick
      end
    end
    object tsConnection: TTabSheet
      Caption = 'tsConnection'
      ImageIndex = 3
      TabVisible = False
      object Label19: TLabel
        Left = 8
        Top = 24
        Width = 225
        Height = 24
        Caption = 'Welcome to XanaNews!'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label20: TLabel
        Left = 8
        Top = 68
        Width = 383
        Height = 26
        Caption = 
          'XanaNews will try to connect to the server using your default In' +
          'ternet connection unless you specify a differnt one here:'
        WordWrap = True
      end
      object cbAlwaysConnectUsing: TCheckBox
        Left = 24
        Top = 112
        Width = 313
        Height = 17
        Caption = 'Always connect to this server using:'
        TabOrder = 0
        OnClick = cbAlwaysConnectUsingClick
      end
      object cbRasEntries: TComboBox
        Left = 24
        Top = 144
        Width = 369
        Height = 21
        Style = csDropDownList
        Enabled = False
        TabOrder = 1
      end
    end
    object tsFinished: TTabSheet
      Caption = 'tsFinished'
      ImageIndex = 4
      TabVisible = False
      DesignSize = (
        459
        327)
      object Label21: TLabel
        Left = 8
        Top = 24
        Width = 225
        Height = 24
        Caption = 'Welcome to XanaNews!'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label22: TLabel
        Left = 8
        Top = 72
        Width = 433
        Height = 57
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'You have successfully entered all the information required to st' +
          'art using XanaNews.  XanaNews will now connect to the internet t' +
          'o obtain the lists of newsgroups available on your news server.'
        WordWrap = True
      end
    end
  end
  object btnBack: TButton
    Left = 344
    Top = 352
    Width = 75
    Height = 25
    Caption = 'Back'
    TabOrder = 1
    OnClick = btnBackClick
  end
  object btnNext: TButton
    Left = 432
    Top = 352
    Width = 75
    Height = 25
    Caption = 'Next'
    TabOrder = 2
    OnClick = btnNextClick
  end
  object btnCancel: TButton
    Left = 520
    Top = 352
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
