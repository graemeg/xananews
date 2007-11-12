object frmNewCertDlg: TfrmNewCertDlg
  Left = 224
  Top = 295
  BorderStyle = bsDialog
  Caption = 'New Certificate Dialog'
  ClientHeight = 397
  ClientWidth = 653
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    653
    397)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 653
    Height = 397
    ActivePage = tabCreate
    Align = alClient
    TabOrder = 0
    object tabCreate: TTabSheet
      Caption = 'Create'
      object rgCreate: TRadioGroup
        Left = 16
        Top = 24
        Width = 617
        Height = 145
        Caption = 'Create:'
        Items.Strings = (
          'Create a personal Root Certificate'
          
            'Create a personal Certificate, signed by a personal Root Certifi' +
            'cate'
          'Create a Certificate Request'
          'Create a Certificate from a Certificate Request')
        TabOrder = 0
        OnClick = rgCreateClick
      end
      object rgWizard: TRadioGroup
        Left = 16
        Top = 176
        Width = 617
        Height = 145
        Caption = 'Wizard:'
        ItemIndex = 0
        Items.Strings = (
          'Advanced (all options available)'
          'CA Certificate'
          'Server Certificate'
          'Client Certificate'
          'Code Signing Certificate')
        TabOrder = 1
      end
    end
    object tabIssuer: TTabSheet
      Caption = 'Issuer'
      object lvIssuer: TListView
        Left = 0
        Top = 0
        Width = 645
        Height = 281
        Align = alTop
        Columns = <
          item
            Caption = 'Common name'
            Width = 200
          end
          item
            Caption = 'Organization'
            Width = 200
          end
          item
            Caption = 'Organizational unit'
            Width = 200
          end>
        HideSelection = False
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnData = lvIssuerData
        OnSelectItem = lvIssuerSelectItem
      end
    end
    object tabSubject: TTabSheet
      Caption = 'Subject Name'
      object Label1: TLabel
        Left = 16
        Top = 8
        Width = 73
        Height = 13
        Caption = 'Common name:'
      end
      object Label2: TLabel
        Left = 16
        Top = 48
        Width = 45
        Height = 13
        Caption = 'Surname:'
      end
      object Label3: TLabel
        Left = 16
        Top = 128
        Width = 39
        Height = 13
        Caption = 'Country:'
      end
      object Label4: TLabel
        Left = 16
        Top = 208
        Width = 62
        Height = 13
        Caption = 'Organization:'
      end
      object Label5: TLabel
        Left = 16
        Top = 248
        Width = 92
        Height = 13
        Caption = 'Organizational Unit:'
      end
      object Label6: TLabel
        Left = 248
        Top = 48
        Width = 60
        Height = 13
        Caption = 'Given name:'
      end
      object Label7: TLabel
        Left = 16
        Top = 88
        Width = 23
        Height = 13
        Caption = 'Title:'
      end
      object Label8: TLabel
        Left = 248
        Top = 88
        Width = 32
        Height = 13
        Caption = 'Initials:'
      end
      object Label9: TLabel
        Left = 320
        Top = 88
        Width = 96
        Height = 13
        Caption = 'Generation Qualifier:'
      end
      object Label10: TLabel
        Left = 88
        Top = 128
        Width = 85
        Height = 13
        Caption = 'State or Province:'
      end
      object Label11: TLabel
        Left = 16
        Top = 168
        Width = 39
        Height = 13
        Caption = 'Locality:'
      end
      object Label12: TLabel
        Left = 16
        Top = 288
        Width = 60
        Height = 13
        Caption = 'DN Qualifier:'
      end
      object edtCommonName: TEdit
        Left = 16
        Top = 24
        Width = 481
        Height = 21
        TabOrder = 0
      end
      object edtSurname: TEdit
        Left = 16
        Top = 64
        Width = 217
        Height = 21
        TabOrder = 1
      end
      object edtCountryName: TEdit
        Left = 16
        Top = 144
        Width = 57
        Height = 21
        TabOrder = 6
      end
      object edtOrganization: TEdit
        Left = 16
        Top = 224
        Width = 481
        Height = 21
        TabOrder = 9
      end
      object edtOrganizationalUnit: TEdit
        Left = 16
        Top = 264
        Width = 481
        Height = 21
        TabOrder = 10
      end
      object edtGivenName: TEdit
        Left = 248
        Top = 64
        Width = 249
        Height = 21
        TabOrder = 2
      end
      object edtTitle: TEdit
        Left = 16
        Top = 104
        Width = 217
        Height = 21
        TabOrder = 3
      end
      object edtInitials: TEdit
        Left = 248
        Top = 104
        Width = 57
        Height = 21
        TabOrder = 4
      end
      object edtGenerationQualifier: TEdit
        Left = 320
        Top = 104
        Width = 57
        Height = 21
        TabOrder = 5
      end
      object edtStateOrProvince: TEdit
        Left = 88
        Top = 144
        Width = 409
        Height = 21
        TabOrder = 7
      end
      object edtLocality: TEdit
        Left = 16
        Top = 184
        Width = 481
        Height = 21
        TabOrder = 8
      end
      object edtDNQualifier: TEdit
        Left = 16
        Top = 304
        Width = 481
        Height = 21
        TabOrder = 11
      end
    end
    object tabSubjectAltName: TTabSheet
      Caption = 'Subject Alt Name'
      object Label13: TLabel
        Left = 16
        Top = 8
        Width = 74
        Height = 13
        Caption = 'RFC 822 name:'
      end
      object Label14: TLabel
        Left = 24
        Top = 48
        Width = 101
        Height = 26
        Caption = 'Example: '#13#10'john.doe@acme.com'
      end
      object Label15: TLabel
        Left = 16
        Top = 88
        Width = 55
        Height = 13
        Caption = 'DNS name:'
      end
      object Label16: TLabel
        Left = 24
        Top = 128
        Width = 139
        Height = 26
        Caption = 'Example: '#13#10'johndoescomputer.acme.com'
      end
      object Label17: TLabel
        Left = 16
        Top = 168
        Width = 131
        Height = 13
        Caption = 'Uniform Resource Identifier:'
      end
      object Label18: TLabel
        Left = 24
        Top = 208
        Width = 155
        Height = 26
        Caption = 'Example: '#13#10'http://www.acme.com/johndoe/'
      end
      object Label19: TLabel
        Left = 16
        Top = 248
        Width = 54
        Height = 13
        Caption = 'IP Address:'
      end
      object Label20: TLabel
        Left = 24
        Top = 288
        Width = 57
        Height = 26
        Caption = 'Example: '#13#10'192.168.0.1'
      end
      object edtRFC822Name: TEdit
        Left = 16
        Top = 24
        Width = 481
        Height = 21
        TabOrder = 0
      end
      object edtDNSName: TEdit
        Left = 16
        Top = 104
        Width = 481
        Height = 21
        TabOrder = 1
      end
      object edtURI: TEdit
        Left = 16
        Top = 184
        Width = 481
        Height = 21
        TabOrder = 2
      end
      object edtIPAddress: TEdit
        Left = 16
        Top = 264
        Width = 481
        Height = 21
        TabOrder = 3
      end
    end
    object tabValidity: TTabSheet
      Caption = 'Validity'
      object Label21: TLabel
        Left = 16
        Top = 8
        Width = 54
        Height = 13
        Caption = 'Not Before:'
      end
      object Label22: TLabel
        Left = 16
        Top = 56
        Width = 45
        Height = 13
        Caption = 'Not After:'
      end
      object dtpNotBefore: TDateTimePicker
        Left = 16
        Top = 24
        Width = 186
        Height = 21
        Date = 37509.881513726850000000
        Time = 37509.881513726850000000
        DateFormat = dfLong
        TabOrder = 0
      end
      object dtpNotAfter: TDateTimePicker
        Left = 16
        Top = 72
        Width = 186
        Height = 21
        Date = 37509.881513726850000000
        Time = 37509.881513726850000000
        DateFormat = dfLong
        TabOrder = 1
      end
    end
    object tabSubjectPublicKey: TTabSheet
      Caption = 'Subject Public Key'
      object Label23: TLabel
        Left = 16
        Top = 8
        Width = 46
        Height = 13
        Caption = 'Algorithm:'
      end
      object Label24: TLabel
        Left = 16
        Top = 56
        Width = 44
        Height = 13
        Caption = 'Key Size:'
      end
      object Label47: TLabel
        Left = 16
        Top = 104
        Width = 242
        Height = 13
        Caption = 'Signature Digest Algorithm (stored with private key):'
      end
      object cbSPKIAlgorithm: TComboBox
        Left = 16
        Top = 24
        Width = 201
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbSPKIAlgorithmSelect
        Items.Strings = (
          'rsaEncryption'
          'id-dsa'
          'dhPublicNumber'
          'id-ecPublicKey')
      end
      object cbKeySize: TComboBox
        Left = 16
        Top = 72
        Width = 201
        Height = 21
        ItemHeight = 0
        TabOrder = 1
        Text = '0'
      end
      object cbSignatureAlgorithm: TComboBox
        Left = 16
        Top = 120
        Width = 201
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        Items.Strings = (
          'sha-1'
          'md5'
          'sha-256'
          'sha-384'
          'sha-512')
      end
    end
    object tabKeyUsage: TTabSheet
      Caption = 'Key Usage'
      object Label26: TLabel
        Left = 16
        Top = 8
        Width = 55
        Height = 13
        Caption = 'Key Usage:'
      end
      object clbKeyUsage: TCheckListBox
        Left = 16
        Top = 24
        Width = 201
        Height = 241
        ItemHeight = 13
        Items.Strings = (
          'digitalSignature'
          'nonRepudiation'
          'keyEncipherment'
          'dataEncipherment'
          'keyAgreement'
          'keyCertSign'
          'cRLSign'
          'encipherOnly'
          'decipherOnly')
        TabOrder = 0
      end
    end
    object tabExtKeyUsage: TTabSheet
      Caption = 'Extended Key Usage'
      ImageIndex = 15
      object Label48: TLabel
        Left = 16
        Top = 8
        Width = 103
        Height = 13
        Caption = 'Extended Key Usage:'
      end
      object clbExtKeyUsage: TCheckListBox
        Left = 16
        Top = 24
        Width = 201
        Height = 105
        ItemHeight = 13
        Items.Strings = (
          'Server Authentication'
          'Client Authentication'
          'Code Signing'
          'Email Protection'
          'Time Stamping')
        TabOrder = 0
      end
      object chbExtKeyUsageCritical: TCheckBox
        Left = 16
        Top = 136
        Width = 97
        Height = 17
        Caption = 'Critical'
        TabOrder = 1
      end
    end
    object tabBasicConstraints: TTabSheet
      Caption = 'Basic Constraints'
      object Label25: TLabel
        Left = 16
        Top = 8
        Width = 111
        Height = 13
        Caption = 'Path Length Constraint:'
      end
      object edtPathLenConstraint: TEdit
        Left = 16
        Top = 24
        Width = 121
        Height = 21
        TabOrder = 0
        Text = '0'
      end
    end
    object tabCRLDistributionPoints: TTabSheet
      Caption = 'CRL Distribution Points'
      OnShow = tabCRLDistributionPointsShow
      object Label27: TLabel
        Left = 248
        Top = 8
        Width = 131
        Height = 13
        Caption = 'Uniform Resource Identifier:'
      end
      object Label28: TLabel
        Left = 248
        Top = 48
        Width = 54
        Height = 13
        Caption = 'IP Address:'
      end
      object Label29: TLabel
        Left = 248
        Top = 96
        Width = 69
        Height = 13
        Caption = 'CRL Reasons:'
      end
      object Label30: TLabel
        Left = 8
        Top = 8
        Width = 110
        Height = 13
        Caption = 'CRL Distribution points:'
      end
      object edtCRLDistPointURI: TEdit
        Left = 248
        Top = 24
        Width = 393
        Height = 21
        TabOrder = 0
      end
      object edtCRLDistPointIP: TEdit
        Left = 248
        Top = 64
        Width = 393
        Height = 21
        TabOrder = 1
      end
      object clbCRLReason: TCheckListBox
        Left = 248
        Top = 112
        Width = 249
        Height = 121
        ItemHeight = 13
        Items.Strings = (
          'keyCompromise'
          'cACompromise'
          'affiliationChanged'
          'superseded'
          'cessationOfOperation'
          'certificateHold'
          'removeFromCRL')
        TabOrder = 2
      end
      object lbCRLDistPoints: TListBox
        Left = 8
        Top = 24
        Width = 225
        Height = 249
        ItemHeight = 13
        TabOrder = 3
        OnClick = lbCRLDistPointsClick
      end
      object btnDeleteCRLDistPoint: TButton
        Left = 80
        Top = 280
        Width = 75
        Height = 25
        Caption = 'Delete'
        TabOrder = 4
        OnClick = btnDeleteCRLDistPointClick
      end
      object btnAddCRLDistPoint: TButton
        Left = 160
        Top = 280
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 5
        OnClick = btnAddCRLDistPointClick
      end
      object btnUpdateCRLDistPoint: TButton
        Left = 560
        Top = 280
        Width = 75
        Height = 25
        Caption = 'Update'
        TabOrder = 6
        OnClick = btnUpdateCRLDistPointClick
      end
    end
    object tabNameConstraints: TTabSheet
      Caption = 'Name Constraints'
      OnShow = tabNameConstraintsShow
      object Label31: TLabel
        Left = 320
        Top = 168
        Width = 74
        Height = 13
        Caption = 'RFC 822 name:'
      end
      object Label32: TLabel
        Left = 512
        Top = 168
        Width = 101
        Height = 39
        Caption = 'Examples: '#13#10'john.doe@acme.com'#13#10'acme.com'
      end
      object Label33: TLabel
        Left = 320
        Top = 216
        Width = 55
        Height = 13
        Caption = 'DNS name:'
      end
      object Label34: TLabel
        Left = 512
        Top = 216
        Width = 49
        Height = 26
        Caption = 'Example: '#13#10'acme.com'
      end
      object Label35: TLabel
        Left = 320
        Top = 248
        Width = 131
        Height = 13
        Caption = 'Uniform Resource Identifier:'
      end
      object Label36: TLabel
        Left = 512
        Top = 248
        Width = 52
        Height = 39
        Caption = 'Examples: '#13#10'acme.com'#13#10'.acme.com'
      end
      object Label37: TLabel
        Left = 320
        Top = 296
        Width = 54
        Height = 13
        Caption = 'IP Address:'
      end
      object Label38: TLabel
        Left = 512
        Top = 296
        Width = 119
        Height = 26
        Caption = 'Example: '#13#10'192.168.0.0/255.255.0.0'
      end
      object Label39: TLabel
        Left = 8
        Top = 8
        Width = 95
        Height = 13
        Caption = 'Permitted Sub trees:'
      end
      object Label40: TLabel
        Left = 8
        Top = 128
        Width = 95
        Height = 13
        Caption = 'Excluded Sub trees:'
      end
      object Label41: TLabel
        Left = 296
        Top = 8
        Width = 90
        Height = 13
        Caption = 'Sub Tree Identifier:'
      end
      object edtSubTreeId: TEdit
        Left = 296
        Top = 24
        Width = 345
        Height = 21
        TabOrder = 0
      end
      object lbPermittedSubTrees: TListBox
        Left = 8
        Top = 24
        Width = 273
        Height = 97
        ItemHeight = 13
        TabOrder = 1
        OnClick = lbPermittedSubTreesClick
      end
      object lbExcludedSubTrees: TListBox
        Left = 8
        Top = 144
        Width = 273
        Height = 97
        ItemHeight = 13
        TabOrder = 2
        OnClick = lbExcludedSubTreesClick
      end
      object rgSubTreeType: TRadioGroup
        Left = 296
        Top = 48
        Width = 185
        Height = 105
        Caption = 'Type:'
        Items.Strings = (
          'URI'
          'RFC 822 Name'
          'DNS Name'
          'IP Address')
        TabOrder = 3
      end
      object btnSubTreeUpdate: TButton
        Left = 560
        Top = 128
        Width = 75
        Height = 25
        Caption = 'Update'
        TabOrder = 4
        OnClick = btnSubTreeUpdateClick
      end
      object btnExclSubTreeAdd: TButton
        Left = 208
        Top = 248
        Width = 75
        Height = 25
        Caption = 'Add Excl'
        TabOrder = 5
        OnClick = btnExclSubTreeAddClick
      end
      object btnSubTreeDelete: TButton
        Left = 48
        Top = 248
        Width = 75
        Height = 25
        Caption = 'Delete'
        TabOrder = 6
        OnClick = btnSubTreeDeleteClick
      end
      object btnPermSubTreeAdd: TButton
        Left = 128
        Top = 248
        Width = 75
        Height = 25
        Caption = 'Add Perm'
        TabOrder = 7
        OnClick = btnPermSubTreeAddClick
      end
    end
    object tabPolicyConstraints: TTabSheet
      Caption = 'Policy Constraints'
      object Label42: TLabel
        Left = 16
        Top = 8
        Width = 106
        Height = 13
        Caption = 'Inhibit Policy Mapping:'
      end
      object Label43: TLabel
        Left = 16
        Top = 56
        Width = 107
        Height = 13
        Caption = 'Require Explicit Policy:'
      end
      object edtIPM: TEdit
        Left = 16
        Top = 24
        Width = 121
        Height = 21
        TabOrder = 0
      end
      object edtREP: TEdit
        Left = 16
        Top = 72
        Width = 121
        Height = 21
        TabOrder = 1
      end
    end
    object tabPolicyMappings: TTabSheet
      Caption = 'Policy Mappings'
      object Label44: TLabel
        Left = 16
        Top = 8
        Width = 80
        Height = 13
        Caption = 'Policy Mappings:'
      end
      object Label45: TLabel
        Left = 392
        Top = 24
        Width = 194
        Height = 78
        Caption = 
          'Syntax:'#13#10'  <id>=<id>'#13#10'where each <id> is a dot separated OID '#13#10'w' +
          'ith decimal sub identifiers. '#13#10'The left <id> is the subject doma' +
          'in policy.'#13#10'The right <id> is the issuer domain policy.'
      end
      object memPolicyMappings: TMemo
        Left = 16
        Top = 24
        Width = 369
        Height = 257
        TabOrder = 0
      end
    end
    object tabCertificatePolicies: TTabSheet
      Caption = 'Certificate Policies'
      OnShow = tabCertificatePoliciesShow
      object Label46: TLabel
        Left = 16
        Top = 8
        Width = 108
        Height = 13
        Caption = 'Certificate Policy OIDs:'
      end
      object Label50: TLabel
        Left = 328
        Top = 24
        Width = 31
        Height = 13
        Caption = 'Policy:'
      end
      object Label51: TLabel
        Left = 328
        Top = 64
        Width = 151
        Height = 13
        Caption = 'Certification Practice Statement:'
      end
      object cbPolicy: TComboBox
        Left = 328
        Top = 40
        Width = 313
        Height = 21
        ItemHeight = 0
        TabOrder = 0
      end
      object edtCPS: TEdit
        Left = 328
        Top = 80
        Width = 313
        Height = 21
        TabOrder = 1
      end
      object btnDeletePolicy: TButton
        Left = 160
        Top = 280
        Width = 75
        Height = 25
        Caption = 'Delete'
        TabOrder = 2
        OnClick = btnDeletePolicyClick
      end
      object btnAddPolicy: TButton
        Left = 240
        Top = 280
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 3
        OnClick = btnAddPolicyClick
      end
      object Button3: TButton
        Left = 568
        Top = 112
        Width = 75
        Height = 25
        Caption = 'Update'
        TabOrder = 4
        OnClick = Button3Click
      end
      object lbCertificatePolicies: TListBox
        Left = 16
        Top = 24
        Width = 297
        Height = 249
        ItemHeight = 13
        TabOrder = 5
        OnClick = lbCertificatePoliciesClick
      end
    end
    object tabFinished: TTabSheet
      Caption = 'Finished'
      ImageIndex = 15
      OnShow = tabFinishedShow
      object Label49: TLabel
        Left = 16
        Top = 8
        Width = 39
        Height = 13
        Caption = 'Finished'
      end
      object lblFinishedMsg: TLabel
        Left = 16
        Top = 32
        Width = 69
        Height = 13
        Caption = 'lblFinishedMsg'
      end
    end
  end
  object btnOK: TButton
    Left = 482
    Top = 360
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 562
    Top = 360
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object btnNext: TButton
    Left = 402
    Top = 360
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Next'
    TabOrder = 3
    OnClick = btnNextClick
  end
  object btnBack: TButton
    Left = 322
    Top = 360
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Back'
    TabOrder = 4
    OnClick = btnBackClick
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'p10'
    Filter = 
      'PKCS#10 Request (*.p10)|*.p10|Certificate Request Message (*.crm' +
      ')|*.crm'
    Title = 'Open Certificate Request'
    Left = 168
    Top = 120
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'p10'
    Filter = 'PKCS#10 Request (*.p10)|*.p10'
    Title = 'Save Certificate Request'
    Left = 200
    Top = 120
  end
  object SaveDialog2: TSaveDialog
    DefaultExt = 'cer'
    Filter = 'Certificate (*.cer)|*.cer'
    Title = 'Save Certificate'
    Left = 232
    Top = 120
  end
  object Issuers: TX509TrustedCertificates
    CertList = <>
    HoursOffsetFromGMT = 1.000000000000000000
    LeastKeyBitSize = 0
    Left = 284
    Top = 120
  end
end
