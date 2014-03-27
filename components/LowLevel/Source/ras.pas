(*======================================================================*
 | RAS import unit                                                      |
 |                                                                      |
 | RAS API definitions in Delphi                                        |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2002  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 10.0     08/03/2006  CPWW  Tidied up for BDS 2006                    |
 *======================================================================*)
unit Ras;

{$I CompVers.inc}

interface

uses windows;

const
  RASBASE = 600;
  SUCCESS = 0;

  UNLEN = 256;                 // Maximum user name length
  PWLEN = 256;                 // Maximum password length
  CNLEN = 15;                  // Computer name length
  DNLEN = CNLEN;               // Maximum domain name length
  NETBIOS_NAME_LEN = 16;       // NetBIOS net name (bytes)

  PENDING                              = RASBASE+0;
  ERROR_INVALID_PORT_HANDLE            = RASBASE+1;
  ERROR_PORT_ALREADY_OPEN              = RASBASE+2;
  ERROR_BUFFER_TOO_SMALL               = RASBASE+3;
  ERROR_WRONG_INFO_SPECIFIED           = RASBASE+4;
  ERROR_CANNOT_SET_PORT_INFO           = RASBASE+5;
  ERROR_PORT_NOT_CONNECTED             = RASBASE+6;
  ERROR_EVENT_INVALID                  = RASBASE+7;
  ERROR_DEVICE_DOES_NOT_EXIST          = RASBASE+8;
  ERROR_DEVICETYPE_DOES_NOT_EXIST      = RASBASE+9;
  ERROR_BUFFER_INVALID                 = RASBASE+10;
  ERROR_ROUTE_NOT_AVAILABLE            = RASBASE+11;
  ERROR_ROUTE_NOT_ALLOCATED            = RASBASE+12;
  ERROR_INVALID_COMPRESSION_SPECIFIED  = RASBASE+13;
  ERROR_OUT_OF_BUFFERS                 = RASBASE+14;
  ERROR_PORT_NOT_FOUND                 = RASBASE+15;
  ERROR_ASYNC_REQUEST_PENDING          = RASBASE+16;
  ERROR_ALREADY_DISCONNECTING          = RASBASE+17;
  ERROR_PORT_NOT_OPEN                  = RASBASE+18;
  ERROR_PORT_DISCONNECTED              = RASBASE+19;
  ERROR_NO_ENDPOINTS                   = RASBASE+20;
  ERROR_CANNOT_OPEN_PHONEBOOK          = RASBASE+21;
  ERROR_CANNOT_LOAD_PHONEBOOK          = RASBASE+22;
  ERROR_CANNOT_FIND_PHONEBOOK_ENTRY    = RASBASE+23;
  ERROR_CANNOT_WRITE_PHONEBOOK         = RASBASE+24;
  ERROR_CORRUPT_PHONEBOOK              = RASBASE+25;
  ERROR_CANNOT_LOAD_STRING             = RASBASE+26;
  ERROR_KEY_NOT_FOUND                  = RASBASE+27;
  ERROR_DISCONNECTION                  = RASBASE+28;
  ERROR_REMOTE_DISCONNECTION           = RASBASE+29;
  ERROR_HARDWARE_FAILURE               = RASBASE+30;
  ERROR_USER_DISCONNECTION             = RASBASE+31;
  ERROR_INVALID_SIZE                   = RASBASE+32;
  ERROR_PORT_NOT_AVAILABLE             = RASBASE+33;
  ERROR_CANNOT_PROJECT_CLIENT          = RASBASE+34;
  ERROR_UNKNOWN                        = RASBASE+35;
  ERROR_WRONG_DEVICE_ATTACHED          = RASBASE+36;
  ERROR_BAD_STRING                     = RASBASE+37;
  ERROR_REQUEST_TIMEOUT                = RASBASE+38;
  ERROR_CANNOT_GET_LANA                = RASBASE+39;
  ERROR_NETBIOS_ERROR                  = RASBASE+40;
  ERROR_SERVER_OUT_OF_RESOURCES        = RASBASE+41;
  ERROR_NAME_EXISTS_ON_NET             = RASBASE+42;
  ERROR_SERVER_GENERAL_NET_FAILURE     = RASBASE+43;
  WARNING_MSG_ALIAS_NOT_ADDED          = RASBASE+44;
  ERROR_AUTH_INTERNAL                  = RASBASE+45;
  ERROR_RESTRICTED_LOGON_HOURS         = RASBASE+46;
  ERROR_ACCT_DISABLED                  = RASBASE+47;
  ERROR_PASSWD_EXPIRED                 = RASBASE+48;
  ERROR_NO_DIALIN_PERMISSION           = RASBASE+49;
  ERROR_SERVER_NOT_RESPONDING          = RASBASE+50;
  ERROR_FROM_DEVICE                    = RASBASE+51;
  ERROR_UNRECOGNIZED_RESPONSE          = RASBASE+52;
  ERROR_MACRO_NOT_FOUND                = RASBASE+53;
  ERROR_MACRO_NOT_DEFINED              = RASBASE+54;
  ERROR_MESSAGE_MACRO_NOT_FOUND        = RASBASE+55;
  ERROR_DEFAULTOFF_MACRO_NOT_FOUND     = RASBASE+56;
  ERROR_FILE_COULD_NOT_BE_OPENED       = RASBASE+57;
  ERROR_DEVICENAME_TOO_LONG            = RASBASE+58;
  ERROR_DEVICENAME_NOT_FOUND           = RASBASE+59;
  ERROR_NO_RESPONSES                   = RASBASE+60;
  ERROR_NO_COMMAND_FOUND               = RASBASE+61;
  ERROR_WRONG_KEY_SPECIFIED            = RASBASE+62;
  ERROR_UNKNOWN_DEVICE_TYPE            = RASBASE+63;
  ERROR_ALLOCATING_MEMORY              = RASBASE+64;
  ERROR_PORT_NOT_CONFIGURED            = RASBASE+65;
  ERROR_DEVICE_NOT_READY               = RASBASE+66;
  ERROR_READING_INI_FILE               = RASBASE+67;
  ERROR_NO_CONNECTION                  = RASBASE+68;
  ERROR_BAD_USAGE_IN_INI_FILE          = RASBASE+69;
  ERROR_READING_SECTIONNAME            = RASBASE+70;
  ERROR_READING_DEVICETYPE             = RASBASE+71;
  ERROR_READING_DEVICENAME             = RASBASE+72;
  ERROR_READING_USAGE                  = RASBASE+73;
  ERROR_READING_MAXCONNECTBPS          = RASBASE+74;
  ERROR_READING_MAXCARRIERBPS          = RASBASE+75;
  ERROR_LINE_BUSY                      = RASBASE+76;
  ERROR_VOICE_ANSWER                   = RASBASE+77;
  ERROR_NO_ANSWER                      = RASBASE+78;
  ERROR_NO_CARRIER                     = RASBASE+79;
  ERROR_NO_DIALTONE                    = RASBASE+80;
  ERROR_IN_COMMAND                     = RASBASE+81;
  ERROR_WRITING_SECTIONNAME            = RASBASE+82;
  ERROR_WRITING_DEVICETYPE             = RASBASE+83;
  ERROR_WRITING_DEVICENAME             = RASBASE+84;
  ERROR_WRITING_MAXCONNECTBPS          = RASBASE+85;
  ERROR_WRITING_MAXCARRIERBPS          = RASBASE+86;
  ERROR_WRITING_USAGE                  = RASBASE+87;
  ERROR_WRITING_DEFAULTOFF             = RASBASE+88;
  ERROR_READING_DEFAULTOFF             = RASBASE+89;
  ERROR_EMPTY_INI_FILE                 = RASBASE+90;
  ERROR_AUTHENTICATION_FAILURE         = RASBASE+91;
  ERROR_PORT_OR_DEVICE                 = RASBASE+92;
  ERROR_NOT_BINARY_MACRO               = RASBASE+93;
  ERROR_DCB_NOT_FOUND                  = RASBASE+94;
  ERROR_STATE_MACHINES_NOT_STARTED     = RASBASE+95;
  ERROR_STATE_MACHINES_ALREADY_STARTED = RASBASE+96;
  ERROR_PARTIAL_RESPONSE_LOOPING       = RASBASE+97;
  ERROR_UNKNOWN_RESPONSE_KEY           = RASBASE+98;
  ERROR_RECV_BUF_FULL                  = RASBASE+99;
  ERROR_CMD_TOO_LONG                   = RASBASE+100;
  ERROR_UNSUPPORTED_BPS                = RASBASE+101;
  ERROR_UNEXPECTED_RESPONSE            = RASBASE+102;
  ERROR_INTERACTIVE_MODE               = RASBASE+103;
  ERROR_BAD_CALLBACK_NUMBER            = RASBASE+104;
  ERROR_INVALID_AUTH_STATE             = RASBASE+105;
  ERROR_WRITING_INITBPS                = RASBASE+106;
  ERROR_X25_DIAGNOSTIC                 = RASBASE+107;
  ERROR_ACCT_EXPIRED                   = RASBASE+108;
  ERROR_CHANGING_PASSWORD              = RASBASE+109;
  ERROR_OVERRUN                        = RASBASE+110;
  ERROR_RASMAN_CANNOT_INITIALIZE	     = RASBASE+111;
  ERROR_BIPLEX_PORT_NOT_AVAILABLE      = RASBASE+112;
  ERROR_NO_ACTIVE_ISDN_LINES           = RASBASE+113;
  ERROR_NO_ISDN_CHANNELS_AVAILABLE     = RASBASE+114;
  ERROR_TOO_MANY_LINE_ERRORS           = RASBASE+115;
  ERROR_IP_CONFIGURATION               = RASBASE+116;
  ERROR_NO_IP_ADDRESSES                = RASBASE+117;
  ERROR_PPP_TIMEOUT                    = RASBASE+118;
  ERROR_PPP_REMOTE_TERMINATED          = RASBASE+119;
  ERROR_PPP_NO_PROTOCOLS_CONFIGURED    = RASBASE+120;
  ERROR_PPP_NO_RESPONSE                = RASBASE+121;
  ERROR_PPP_INVALID_PACKET             = RASBASE+122;
  ERROR_PHONE_NUMBER_TOO_LONG          = RASBASE+123;
  ERROR_IPXCP_NO_DIALOUT_CONFIGURED    = RASBASE+124;
  ERROR_IPXCP_NO_DIALIN_CONFIGURED     = RASBASE+125;
  ERROR_IPXCP_DIALOUT_ALREADY_ACTIVE   = RASBASE+126;
  ERROR_ACCESSING_TCPCFGDLL            = RASBASE+127;
  ERROR_NO_IP_RAS_ADAPTER              = RASBASE+128;
  ERROR_SLIP_REQUIRES_IP               = RASBASE+129;
  ERROR_PROJECTION_NOT_COMPLETE        = RASBASE+130;
  ERROR_PROTOCOL_NOT_CONFIGURED        = RASBASE+131;
  ERROR_PPP_NOT_CONVERGING             = RASBASE+132;
  ERROR_PPP_CP_REJECTED                = RASBASE+133;
  ERROR_PPP_LCP_TERMINATED             = RASBASE+134;
  ERROR_PPP_REQUIRED_ADDRESS_REJECTED  = RASBASE+135;
  ERROR_PPP_NCP_TERMINATED             = RASBASE+136;
  ERROR_PPP_LOOPBACK_DETECTED          = RASBASE+137;
  ERROR_PPP_NO_ADDRESS_ASSIGNED        = RASBASE+138;
  ERROR_CANNOT_USE_LOGON_CREDENTIALS   = RASBASE+139;
  ERROR_TAPI_CONFIGURATION             = RASBASE+140;
  ERROR_NO_LOCAL_ENCRYPTION            = RASBASE+141;
  ERROR_NO_REMOTE_ENCRYPTION           = RASBASE+142;
  ERROR_REMOTE_REQUIRES_ENCRYPTION     = RASBASE+143;
  ERROR_IPXCP_NET_NUMBER_CONFLICT      = RASBASE+144;
  ERROR_INVALID_SMM                    = RASBASE+145;
  ERROR_SMM_UNINITIALIZED              = RASBASE+146;
  ERROR_NO_MAC_FOR_PORT                = RASBASE+147;
  ERROR_SMM_TIMEOUT                    = RASBASE+148;
  ERROR_BAD_PHONE_NUMBER               = RASBASE+149;
  ERROR_WRONG_MODULE                   = RASBASE+150;
  ERROR_INVALID_CALLBACK_NUMBER        = RASBASE+151;
  ERROR_SCRIPT_SYNTAX                  = RASBASE+152;
  RASBASEEND                           = RASBASE+152;

  RAS_MaxDeviceType     = 16;
  RAS_MaxPhoneNumber    = 128;
  RAS_MaxIpAddress      = 15;
  RAS_MaxIpxAddress     = 21;

  RAS_MaxEntryName      = 256;
  RAS_MaxDeviceName     = 128;
  RAS_MaxCallbackNumber = RAS_MaxPhoneNumber;

  RAS_MaxAreaCode       = 10;
  RAS_MaxPadType        = 32;
  RAS_MaxX25Address     = 200;
  RAS_MaxFacilities     = 200;
  RAS_MaxUserData       = 200;

(* Enumerates intermediate states to a connection.  (See RasDial)
*)
  RASCS_PAUSED = $1000;
  RASCS_DONE = $2000;

  RASCS_OpenPort = 0;
  RASCS_PortOpened = 1;
  RASCS_ConnectDevice = 2;
  RASCS_DeviceConnected = 3;
  RASCS_AllDevicesConnected = 4;
  RASCS_Authenticate = 5;
  RASCS_AuthNotify = 6;
  RASCS_AuthRetry = 7;
  RASCS_AuthCallback = 8;
  RASCS_AuthChangePassword = 9;
  RASCS_AuthProject = 10;
  RASCS_AuthLinkSpeed = 11;
  RASCS_AuthAck = 12;
  RASCS_ReAuthenticate = 13;
  RASCS_Authenticated = 14;
  RASCS_PrepareForCallback = 15;
  RASCS_WaitForModemReset = 16;
  RASCS_WaitForCallback = 17;
  RASCS_Projected = 18;

  RASCS_StartAuthentication = 19;
  RASCS_CallbackComplete = 20;
  RASCS_LogonNetwork = 21;
  RASCS_SubEntryConnected = 22;
  RASCS_SubEntryDisconnected = 23;

  RASCS_Interactive = RASCS_PAUSED;
  RASCS_RetryAuthentication = RASCS_PAUSED + 1;
  RASCS_CallbackSetByCaller = RASCS_PAUSED + 2;
  RASCS_PasswordExpired = RASCS_PAUSED + 3;

  RASCS_Connected = RASCS_DONE;
  RASCS_Disconnected = RASCS_DONE + 1;

(* 'dwfOptions' bit flags.
*)
  RDEOPT_UsePrefixSuffix           = $00000001;
  RDEOPT_PausedStates              = $00000002;
  RDEOPT_IgnoreModemSpeaker        = $00000004;
  RDEOPT_SetModemSpeaker           = $00000008;
  RDEOPT_IgnoreSoftwareCompression = $00000010;
  RDEOPT_SetSoftwareCompression    = $00000020;
  RDEOPT_DisableConnectedUI        = $00000040;
  RDEOPT_DisableReconnectUI        = $00000080;
  RDEOPT_DisableReconnect          = $00000100;
  RDEOPT_NoUser                    = $00000200;
  RDEOPT_PauseOnScript             = $00000400;

  RASP_Amb = $10000;
  RASP_PppNbf = $803F;
  RASP_PppIpx = $802B;
  RASP_PppIp = $8021;
  RASP_PppLcp = $C021;
  RASP_Slip = $20000;

(* If using RasDial message notifications, get the notification message code
** by passing this string to the RegisterWindowMessageA() API.
** WM_RASDIALEVENT is used only if a unique message cannot be registered.
*)

  RASDIALEVENT    = 'RasDialEvent';
  WM_RASDIALEVENT = $CCCD;

(* RASENTRY 'dwfOptions' bit flags.
*)
  RASEO_UseCountryAndAreaCodes    = $00000001;
  RASEO_SpecificIpAddr            = $00000002;
  RASEO_SpecificNameServers       = $00000004;
  RASEO_IpHeaderCompression       = $00000008;
  RASEO_RemoteDefaultGateway      = $00000010;
  RASEO_DisableLcpExtensions      = $00000020;
  RASEO_TerminalBeforeDial        = $00000040;
  RASEO_TerminalAfterDial         = $00000080;
  RASEO_ModemLights               = $00000100;
  RASEO_SwCompression             = $00000200;
  RASEO_RequireEncryptedPw        = $00000400;
  RASEO_RequireMsEncryptedPw      = $00000800;
  RASEO_RequireDataEncryption     = $00001000;
  RASEO_NetworkLogon              = $00002000;
  RASEO_UseLogonCredentials       = $00004000;
  RASEO_PromoteAlternates         = $00008000;
  RASEO_SecureLocalFiles          = $00010000;
  RASEO_RequireEAP                = $00020000;
  RASEO_RequirePAP                = $00040000;
  RASEO_RequireSPAP               = $00080000;
  RASEO_Custom                    = $00100000;

  RASEO_PreviewPhoneNumber        = $00200000;
  RASEO_SharedPhoneNumbers        = $00800000;
  RASEO_PreviewUserPw             = $01000000;
  RASEO_PreviewDomain             = $02000000;
  RASEO_ShowDialingProgress       = $04000000;
  RASEO_RequireCHAP               = $08000000;
  RASEO_RequireMsCHAP             = $10000000;
  RASEO_RequireMsCHAP2            = $20000000;
  RASEO_RequireW95MSCHAP          = $40000000;
  RASEO_CustomScript              = $80000000;

(* RASENTRY 'dwProtocols' bit flags.
*)
  RASNP_NetBEUI                   = $00000001;
  RASNP_Ipx                       = $00000002;
  RASNP_Ip                        = $00000004;

(* RASENTRY 'dwFramingProtocols' bit flags.
*)
  RASFP_Ppp                       = $00000001;
  RASFP_Slip                      = $00000002;
  RASFP_Ras                       = $00000004;

(* RASENTRY 'szDeviceType' default strings.
*)
  RASDT_Modem                     = 'modem';
  RASDT_Isdn                      = 'isdn';
  RASDT_X25                       = 'x25';
  RASDT_Vpn                       = 'vpn';
  RASDT_Pad                       = 'pad';
  RASDT_Generic                   = 'GENERIC';
  RASDT_Serial        		  = 'SERIAL';
  RASDT_FrameRelay                = 'FRAMERELAY';
  RASDT_Atm                       = 'ATM';
  RASDT_Sonet                     = 'SONET';
  RASDT_SW56                      = 'SW56';
  RASDT_Irda                      = 'IRDA';
  RASDT_Parallel                  = 'PARALLEL';

  RASET_Phone    = 1;  // Phone lines: modem, ISDN, X.25, etc
  RASET_Vpn      = 2;  // Virtual private network
  RASET_Direct   = 3;  // Direct connect: serial, parallel
  RASET_Internet = 4;  // BaseCamp internet

(* Flags for RasConnectionNotification().
*)
  RASCN_Connection        = $00000001;
  RASCN_Disconnection     = $00000002;
  RASCN_BandwidthAdded    = $00000004;
  RASCN_BandwidthRemoved  = $00000008;

(* RASENTRY 'dwDialMode' values.
*)
  RASEDM_DialAll                  = 1;
  RASEDM_DialAsNeeded             = 2;

(* RASENTRY 'dwIdleDisconnectSeconds' constants.
*)
  RASIDS_Disabled                 = $ffffffff;
  RASIDS_UseGlobalValue           = 0;

(* AutoDial DLL function parameter block 'dwFlags.'
*)
  RASADFLG_PositionDlg            = $00000001;

(* RASCREDENTIALS 'dwMask' values.
*)
  RASCM_UserName       = $00000001;
  RASCM_Password       = $00000002;
  RASCM_Domain         = $00000004;


(* AutoDial control parameter values for
** Ras{Get,Set}AutodialParam.
*)
  RASADP_DisableConnectionQuery           = 0;
  RASADP_LoginSessionDisable              = 1;
  RASADP_SavedAddressesLimit              = 2;
  RASADP_FailedConnectionTimeout          = 3;
  RASADP_ConnectionQueryTimeout           = 4;


type
  HRASCon = THandle;
  PHRASCon = ^HRASCon;

(* Identifies an active RAS connection.  (See RasEnumConnections)
*)

  TRASConnW = record
    dwSize: DWORD;
    hrasconn: HRasCon;
    szEntryName: array[0..RAS_MaxEntryName] of WideChar;
    szDeviceType: array[0..RAS_MaxDeviceType] of WideChar;
    szDeviceName: array[0..RAS_MaxDeviceName] of WideChar;
    szPhonebook: array[0..MAX_PATH - 1] of WideChar;
    dwSubEntry: DWORD;
  end;

  TRasConnA = record
    dwSize: DWORD;
    hrasconn: HRasCon;
    szEntryName: array[0..RAS_MaxEntryName] of AnsiChar;
    szDeviceType: array[0..RAS_MaxDeviceType] of AnsiChar;
    szDeviceName: array[0..RAS_MaxDeviceName] of AnsiChar;
    szPhonebook: array[0..MAX_PATH - 1] of AnsiChar;
    dwSubEntry: DWORD;
  end;

{$ifdef COMPILER12_UP}
  TRasConn = TRasConnW;
{$else}
  TRasConn = TRasConnA;
{$endif}

  PRASCONNW = ^TRasConnw;
  PRASCONNA = ^TRasConnA;
  PRasConn = ^TRasConn;

  TRASConnState = Integer;
  PRasConnState = ^TRASConnState;


(* Describes the status of a RAS connection.  (See RasConnectionStatus)
*)
  TRASConnStatusW = record
    dwSize: DWORD;
    rasconnstate: TRASConnState;
    dwError: DWORD;
    szDeviceType: array[0..RAS_MaxDeviceType] of WideChar;
    szDeviceName: array[0..RAS_MaxDeviceName] of WideChar;
    szPhoneNumber: array[0..RAS_MaxPhoneNumber] of WideChar;
  end;

  TRASConnStatusA = record
    dwSize: DWORD;
    rasconnstate: TRASConnState;
    dwError: DWORD;
    szDeviceType: array[0..RAS_MaxDeviceType] of AnsiChar;
    szDeviceName: array[0..RAS_MaxDeviceName] of AnsiChar;
    szPhoneNumber: array[0..RAS_MaxPhoneNumber] of AnsiChar;
  end;

{$ifdef COMPILER12_UP}
  TRASConnStatus = TRASConnStatusW;
{$else}
  TRASConnStatus = TRASConnStatusA;
{$endif}

  PRASConnStatusW = ^TRASConnStatusW;
  PRASConnStatusA = ^TRASConnStatusA;
  PRASConnStatus = ^TRASConnStatus;

(* Describes connection establishment parameters.  (See RasDial)
*)
  TRASDialParamsW = record
    dwSize: DWORD;
    szEntryName: array[0..RAS_MaxEntryName] of WideChar;
    szPhoneNumber: array[0..RAS_MaxPhoneNumber] of WideChar;
    szCallbackNumber: array[0..RAS_MaxCallbackNumber] of WideChar;
    szUserName: array[0..UNLEN] of WideChar;
    szPassword: array[0..PWLEN] of WideChar;
    szDomain: array[0..DNLEN] of WideChar;
    dwSubEntry: DWORD;
    dwCallbackId: DWORD;
  end;

  TRASDialParamsA = record
    dwSize: DWORD;
    szEntryName: array[0..RAS_MaxEntryName] of AnsiChar;
    szPhoneNumber: array[0..RAS_MaxPhoneNumber] of AnsiChar;
    szCallbackNumber: array[0..RAS_MaxCallbackNumber] of AnsiChar;
    szUserName: array[0..UNLEN] of AnsiChar;
    szPassword: array[0..PWLEN] of AnsiChar;
    szDomain: array[0..DNLEN] of AnsiChar;
    dwSubEntry: DWORD;
    dwCallbackId: DWORD;
  end;

{$ifdef COMPILER12_UP}
  TRASDialParams = TRASDialParamsW;
{$else}
  TRASDialParams = TRASDialParamsA;
{$endif}


  PRASDialParamsW = ^TRASDialParamsW;
  PRASDialParamsA = ^TRASDialParamsA;
  PRASDialParams = ^TRASDialParams;


(* Describes extended connection establishment options.  (See RasDial)
*)

  TRASDialExtensions = record
    dwSize: DWORD;
    dwfOptions: DWORD;
    hwndParent: HWND;
    reserved: DWORD;
  end;
  PRASDialExtensions = ^TRASDialExtensions;


(* Describes an enumerated RAS phone book entry name.  (See RasEntryEnum)
*)
  TRASEntryNameW = record
    dwSize: DWORD;
    szEntryName: array[0..RAS_MaxEntryName] of WideChar;
  end;

  TRASEntryNameA = record
    dwSize: DWORD;
    szEntryName: array[0..RAS_MaxEntryName] of AnsiChar;
  end;

{$ifdef COMPILER12_UP}
  TRASEntryName = TRASEntryNameW;
{$else}
  TRASEntryName = TRASEntryNameA;
{$endif}

  PRASEntryNameW = ^TRASEntryNameW;
  PRASEntryNameA = ^TRASEntryNameA;
  PRASEntryName = ^TRASEntryName;

(* Protocol code to projection data structure mapping.
*)

  TRASProjection = DWORD;
  PRASProjection = ^TRASProjection;

(* Describes the Result of a RAS AMB (Authentication Message Block)
** projection.  This protocol is used with NT 3.1 and OS/2 1.3 downlevel
** RAS servers.
*)
  TRASAMBW = record
    dwSize: DWORD;
    dwError: DWORD;
    szNetBiosError: array[0..NETBIOS_NAME_LEN] of WideChar;
    bLana: BYTE;
  end;

  TRASAMBA = record
    dwSize: DWORD;
    dwError: DWORD;
    szNetBiosError: array[0..NETBIOS_NAME_LEN] of AnsiChar;
    bLana: BYTE;
  end;

{$ifdef COMPILER12_UP}
  TRASAMB = TRASAMBW;
{$else}
  TRASAMB = TRASAMBA;
{$endif}

  PRASAMBW = ^TRASAMBW;
  PRASAMBA = ^TRASAMBA;
  PRASAMB = ^TRASAMB;

(* Describes the Result of a PPP NBF (NetBEUI) projection.
*)

  TRASPPPNBFW = record
    dwSize: DWORD;
    dwError: DWORD;
    dwNetBiosError: DWORD;
    szNetBiosError: array[0..NETBIOS_NAME_LEN] of WideChar;
    szWorkstationName: array[0..NETBIOS_NAME_LEN] of WideChar;
    bLana: byte;
  end;

  TRASPPPNBFA = record
    dwSize: DWORD;
    dwError: DWORD;
    dwNetBiosError: DWORD;
    szNetBiosError: array[0..NETBIOS_NAME_LEN] of AnsiChar;
    szWorkstationName: array[0..NETBIOS_NAME_LEN] of AnsiChar;
    bLana: byte;
  end;

{$ifdef COMPILER12_UP}
  TRASPPPNBF = TRASPPPNBFW;
{$else}
  TRASPPPNBF = TRASPPPNBFA;
{$endif}

  PRASPPPNBFW = ^TRASPPPNBFW;
  PRASPPPNBFA = ^TRASPPPNBFA;
  PRASPPPNBF = ^TRASPPPNBF;

(* Describes the Results of a PPP IPX (Internetwork Packet Exchange)
** projection.
*)

  TRASPPPIPXW = record
    dwSize: DWORD;
    dwError: DWORD;
    szIpxAddress: array[0..RAS_MaxIpxAddress] of WideChar;
  end;

  TRASPPPIPXA = record
    dwSize: DWORD;
    dwError: DWORD;
    szIpxAddress: array[0..RAS_MaxIpxAddress] of AnsiChar;
  end;

{$ifdef COMPILER12_UP}
  TRASPPPIPX = TRASPPPIPXW;
{$else}
  TRASPPPIPX = TRASPPPIPXA;
{$endif}

  PRASPPPIPXW = ^TRASPPPIPXW;
  PRASPPPIPXA = ^TRASPPPIPXA;
  PRASPPPIPX = ^TRASPPPIPX;


(* Describes the Results of a PPP IP (Internet) projection.
*)

  TRASPPPIPW = record
    dwSize: DWORD;
    dwError: DWORD;
    szIpAddress: array[0..RAS_MaxIpAddress] of WideChar;
    szServerIpAddress: array[0..RAS_MaxIpAddress] of WideChar;
  end;

  TRASPPPIPA = record
    dwSize: DWORD;
    dwError: DWORD;
    szIpAddress: array[0..RAS_MaxIpAddress] of AnsiChar;
    szServerIpAddress: array[0..RAS_MaxIpAddress] of AnsiChar;
  end;

{$ifdef COMPILER12_UP}
  TRASPPPIP = TRASPPPIPW;
{$else}
  TRASPPPIP = TRASPPPIPA;
{$endif}

  PRASPPPIPW = ^TRASPPPIPW;
  PRASPPPIPA = ^TRASPPPIPA;
  PRASPPPIP = ^TRASPPPIP;



(* Describes the Results of a PPP LCP/multi-link negotiation.
*)
  TRASPPPLCP = record
    dwSize: DWORD;
    fBundled: BOOL;
  end;

  PRASPPPLCP = ^TRASPPPLCP;


(* Describes the Results of a SLIP (Serial Line IP) projection.
*)
  TRASSLIPW = record
    dwSize: DWORD;
    dwError: DWORD;
    szIpAddress: array[0..RAS_MaxIpAddress] of WideChar;
  end;

  TRASSLIPA = record
    dwSize: DWORD;
    dwError: DWORD;
    szIpAddress: array[0..RAS_MaxIpAddress] of AnsiChar;
  end;

{$ifdef COMPILER12_UP}
  TRASSLIP = TRASSLIPW;
{$else}
  TRASSLIP = TRASSLIPA;
{$endif}

  PRASSLIPW = ^TRASSLIPW;
  PRASSLIPA = ^TRASSLIPA;
  PRASSLIP = ^TRASSLIP;

(* Prototypes for caller's RasDial callback handler.  Arguments are the
** message ID (currently always WM_RASDIALEVENT), the current RASCONNSTATE and
** the error that has occurred (or 0 if none).  Extended arguments are the
** handle of the RAS connection and an extended error code.
**
** For RASDIALFUNC2, subsequent callback notifications for all
** subentries can be cancelled by returning FALSE.
*)

TRasDialFunc = procedure(unMsg: UINT; RASConnState: TRASConnState; dwError: DWORD); stdcall;
TRasDialFunc1 = procedure(HRASConn: HRASCON; unMsg: UINT; rascs: TRASConnState; dwError, dwExtendedError: DWORD); stdcall;
TRasDialFunc2 = function(dwCallbackID, dwSubEntry: DWORD; HRASConn: HRASCON; unMsg: UINT; rascs: TRASConnState; dwError, dwExtendedError: DWORD): DWORD; stdcall;

(* Information describing a RAS-capable device.
*)

  TRASDevInfoW = record
    dwSize: DWORD;
    szDeviceType: array[0..RAS_MaxDeviceType] of WideChar;
    szDeviceName: array[0..RAS_MaxDeviceName] of WideChar;
  end;

  TRASDevInfoA = record
    dwSize: DWORD;
    szDeviceType: array[0..RAS_MaxDeviceType] of AnsiChar;
    szDeviceName: array[0..RAS_MaxDeviceName] of AnsiChar;
  end;

{$ifdef COMPILER12_UP}
  TRASDevInfo = TRASDevInfoW;
{$else}
  TRASDevInfo = TRASDevInfoA;
{$endif}

  PRASDevInfoW = ^TRASDevInfoW;
  PRASDevInfoA = ^TRASDevInfoA;
  PRASDevInfo = ^TRASDevInfo;

(* RAS country information(currently retrieved from TAPI).
*)

  TRASCtryInfo = record
    dwSize: DWORD;
    dwCountryID: DWORD;
    dwNextCountryID: DWORD;
    dwCountryCode: DWORD;
    dwCountryNameOffset: DWORD;
  end;

(* There is currently no difference between
** RASCTRYINFOA and RASCTRYINFOW.  This may
** change in the future.
*)
  TRASCtryInfoW =  TRASCtryInfo;
  TRASCtryInfoA =  TRASCtryInfo;

  PRASCtryInfoW = ^TRASCtryInfoW;
  PRASCtryInfoA = ^TRASCtryInfoA;
  PRASCtryInfo = ^TRASCtryInfo;

(* A RAS IP address.
*)

  TRASIPAddr = record
    a: BYTE;
    b: BYTE;
    c: BYTE;
    d: BYTE;
  end;

const
  ET_40Bit        = 1;
  ET_128Bit       = 2;

  ET_None         = 0;  // No encryption
  ET_Require      = 1;  // Require Encryption
  ET_RequireMax   = 2;  // Require max encryption
  ET_Optional     = 3;  // Do encryption if possible. None Ok.

  VS_Default      = 0;   // default (PPTP for now)
  VS_PptpOnly     = 1;	// Only PPTP is attempted.
  VS_PptpFirst	  = 2;   // PPTP is tried first.
  VS_L2tpOnly 	  = 3;	// Only L2TP is attempted.
  VS_L2tpFirst	  = 4;	// L2TP is tried first.

type

(* A RAS phone book entry.
*)

  TRASEntryA = record
    dwSize: DWORD;
    dwfOptions: DWORD;
    //
    // Location/phone number.
    //
    dwCountryID: DWORD;
    dwCountryCode: DWORD;
    szAreaCode: array[0..RAS_MaxAreaCode] of AnsiChar;
    szLocalPhoneNumber: array[0..RAS_MaxPhoneNumber] of AnsiChar;
    dwAlternateOffset: DWORD;
    //
    // PPP/Ip
    //
    ipaddr: TRASIPAddr;
    ipaddrDns: TRASIPAddr;
    ipaddrDnsAlt: TRASIPAddr;
    ipaddrWins: TRASIPAddr;
    ipaddrWinsAlt: TRASIPAddr;
    //
    // Framing
    //
    dwFrameSize: DWORD;
    dwfNetProtocols: DWORD;
    dwFramingProtocol: DWORD;
    //
    // Scripting
    //
    szScript: array[0..MAX_PATH-1] of AnsiChar;
    //
    // AutoDial
    //
    szAutodialDll: array[0..MAX_PATH-1] of AnsiChar;
    szAutodialFunc: array[0.. MAX_PATH-1] of AnsiChar;
    //
    // Device
    //
    szDeviceType: array[0..RAS_MaxDeviceType] of AnsiChar;
    szDeviceName: array[0..RAS_MaxDeviceName] of AnsiChar;
    //
    // X.25
    //
    szX25PadType: array[0..RAS_MaxPadType] of AnsiChar;
    szX25Address: array[0..RAS_MaxX25Address] of AnsiChar;
    szX25Facilities: array[0..RAS_MaxFacilities] of AnsiChar;
    szX25UserData: array[0..RAS_MaxUserData] of AnsiChar;
    dwChannels: DWORD;
    //
    // Reserved
    //
    dwReserved1 : DWORD;
    dwReserved2: DWORD;

    //
    // Multilink
    //
    dwSubEntries: DWORD;
    dwDialMode: DWORD;
    dwDialExtraPercent: DWORD;
    dwDialExtraSampleSeconds: DWORD;
    dwHangUpExtraPercent: DWORD;
    dwHangUpExtraSampleSeconds: DWORD;
    //
    // Idle timeout
    //
    dwIdleDisconnectSeconds: DWORD;
  end;


  TRASEntryW = record
    dwSize: DWORD;
    dwfOptions: DWORD;
    //
    // Location/phone number.
    //
    dwCountryID: DWORD;
    dwCountryCode: DWORD;
    szAreaCode: array[0..RAS_MaxAreaCode] of WideChar;
    szLocalPhoneNumber: array[0..RAS_MaxPhoneNumber] of WideChar;
    dwAlternateOffset: DWORD;
    //
    // PPP/Ip
    //
    ipaddr: TRASIPAddr;
    ipaddrDns: TRASIPAddr;
    ipaddrDnsAlt: TRASIPAddr;
    ipaddrWins: TRASIPAddr;
    ipaddrWinsAlt: TRASIPAddr;
    //
    // Framing
    //
    dwFrameSize: DWORD;
    dwfNetProtocols: DWORD;
    dwFramingProtocol: DWORD;
    //
    // Scripting
    //
    szScript: array[0..MAX_PATH-1] of WideChar;
    //
    // AutoDial
    //
    szAutodialDll: array[0..MAX_PATH-1] of WideChar;
    szAutodialFunc: array[0.. MAX_PATH-1] of WideChar;
    //
    // Device
    //
    szDeviceType: array[0..RAS_MaxDeviceType] of WideChar;
    szDeviceName: array[0..RAS_MaxDeviceName] of WideChar;
    //
    // X.25
    //
    szX25PadType: array[0..RAS_MaxPadType] of WideChar;
    szX25Address: array[0..RAS_MaxX25Address] of WideChar;
    szX25Facilities: array[0..RAS_MaxFacilities] of WideChar;
    szX25UserData: array[0..RAS_MaxUserData] of WideChar;
    dwChannels: DWORD;
    //
    // Reserved
    //
    dwReserved1 : DWORD;
    dwReserved2: DWORD;

    //
    // Multilink
    //
    dwSubEntries: DWORD;
    dwDialMode: DWORD;
    dwDialExtraPercent: DWORD;
    dwDialExtraSampleSeconds: DWORD;
    dwHangUpExtraPercent: DWORD;
    dwHangUpExtraSampleSeconds: DWORD;
    //
    // Idle timeout
    //
    dwIdleDisconnectSeconds: DWORD;
  end;

{$ifdef COMPILER12_UP}
  TRASEntry = TRASEntryW;
{$else}
  TRASEntry = TRASEntryA;
{$endif}

  PRASEntryW = ^TRASEntryW;
  PRASEntryA = ^TRASEntryA;
  PRASEntry = ^TRASEntry;


(* Old AutoDial DLL function prototype.
**
** This prototype is documented for backward-compatibility
** purposes only.  It is superceded by the RASADFUNCA
** and RASADFUNCW definitions below.  DO NOT USE THIS
** PROTOTYPE IN NEW CODE.  SUPPORT FOR IT MAY BE REMOVED
** IN FUTURE VERSIONS OF RAS.
*)

ORASADFUNC = function(hwndOwner: HWND; lpszEntry: PChar; dwFlags: DWORD; var dwRetCode: DWORD): BOOL; stdcall;

(* AutoDial DLL function parameter block.
*)

  TRASADParams = record
    dwSize: DWORD;
    hwndOwner: HWND;
    dwFlags: DWORD;
    xDlg: LongInt;
    yDlg: LongInt;
  end;

  PRASADParams = ^TRASADParams;


(* Prototype AutoDial DLL function.
*)
  RASADFuncA = function(lpszPhoneBook, lpszEntry: PCHAR; const AutoDialParams: TRASADParams; var dwRetCode: PDWORD): BOOL; stdcall;
  RASADFuncW = function(lpszPhoneBook, lpszEntry: PWideChar; const AutoDialParams: TRASADParams; var dwRetCode: PDWORD): BOOL; stdcall;

  RASADFunc = RASADFuncA;

(* A RAS phone book multilinked sub-entry.
*)
  TRASSubEntryA = record
    dwSize: DWORD;
    dwfFlags: DWORD;
    //
    // Device
    //
    szDeviceType: array[0..RAS_MaxDeviceType] of AnsiChar;
    szDeviceName: array[0..RAS_MaxDeviceName] of AnsiChar;
    //
    // Phone numbers
    //
    szLocalPhoneNumber: array[0..RAS_MaxPhoneNumber] of AnsiChar;
    dwAlternateOffset: DWORD;
  end;

  TRASSubEntryW = record
    dwSize: DWORD;
    dwfFlags: DWORD;
    //
    // Device
    //
    szDeviceType: array[0..RAS_MaxDeviceType] of WideChar;
    szDeviceName: array[0..RAS_MaxDeviceName] of WideChar;
    //
    // Phone numbers
    //
    szLocalPhoneNumber: array[0..RAS_MaxPhoneNumber] of WideChar;
    dwAlternateOffset: DWORD;
  end;

{$ifdef COMPILER12_UP}
  TRASSubEntry = TRASSubEntryW;
{$else}
  TRASSubEntry = TRASSubEntryA;
{$endif}

  PRASSubEntryW = ^TRASSubEntryW;
  PRASSubEntryA = ^TRASSubEntryA;
  PRASSubEntry = ^TRASSubEntry;


(* Ras{Get,Set}Credentials structure.  These calls
** supercede Ras{Get,Set}EntryDialParams.
*)
  TRASCredentialsA = record
    dwSize :DWORD;
    dwMask: DWORD;
    szUserName: array[0..UNLEN] of AnsiChar;
    szPassword: array[0..PWLEN] of AnsiChar;
    szDomain: array[0..DNLEN] of AnsiChar;
  end;

  TRASCredentialsW = record
    dwSize :DWORD;
    dwMask: DWORD;
    szUserName: array[0..UNLEN] of WideChar;
    szPassword: array[0..PWLEN] of WideChar;
    szDomain: array[0..DNLEN] of WideChar;
  end;

{$ifdef COMPILER12_UP}
  TRASCredentials = TRASCredentialsW;
{$else}
  TRASCredentials = TRASCredentialsA;
{$endif}

  PRASCredentialsW = ^TRASCredentialsW;
  PRASCredentialsA = ^TRASCredentialsA;
  PRASCredentials = ^TRASCredentials;

(* AutoDial address properties.
*)

  TRASAutoDialEntryA = record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwDialingLocation: DWORD;
    szEntry: array[0..RAS_MaxEntryName] of AnsiChar;
  end;

  TRASAutoDialEntryW = record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwDialingLocation: DWORD;
    szEntry: array[0..RAS_MaxEntryName] of WideChar;
  end;

{$ifdef COMPILER12_UP}
  TRASAutoDialEntry = TRASAutoDialEntryW;
{$else}
  TRASAutoDialEntry = TRASAutoDialEntryA;
{$endif}

  PRASAutoDialEntryW = TRASAutoDialEntryW;
  PRASAutoDialEntryA = TRASAutoDialEntryA;
  PRASAutoDialEntry = TRASAutoDialEntry;


(* External RAS API function prototypes.
*)

{$ifdef COMPILER12_UP}
  PPWideChar = ^PWideChar;
  PPChar = PPWideChar;
{$else}
  PPChar = ^PChar;
  PPWideChar = ^PWideChar;
{$endif}


TfnRasDialA = function(
                 RASDialExtension: PRASDialExtensions;
                 lpszPhonebook: PChar;
                 const RASDialParams: TRASDialParamsA;
                 dwNotifierType: DWORD;
                 pNotifier: Pointer;
                 var hRasConn: hRasCon): DWORD; stdcall;

TfnRasDialW = function(
                 RASDialExtension: PRASDialExtensions;
                 lpszPhonebook: PWideChar;
                 const RASDialParams: TRASDialParamsW;
                 dwNotifierType: DWORD;
                 pNotifier: Pointer;
                 var hRasConn: hRasCon): DWORD; stdcall;

TfnRasDial = function(
                 RASDialExtension: PRASDialExtensions;
                 lpszPhonebook: PChar;
                 const RASDialParams: TRASDialParams;
                 dwNotifierType: DWORD;
                 pNotifier: Pointer;
                 var hRasConn: hRasCon): DWORD; stdcall;


TfnRasEnumConnectionsA = function(connections: PRASConnA; var cb, cConnections: DWORD): DWORD; stdcall;
TfnRasEnumConnectionsW = function(connections: PRASConnW; var cb, cConnections: DWORD): DWORD; stdcall;
TfnRasEnumConnections = function(connections: PRASConn; var cb, cConnections: DWORD): DWORD; stdcall;

TfnRasEnumEntriesA = function(
                 lpszReserved, lpszPhonebook: PChar;
                 pEntryname: PRASENTRYNAMEA;
                 var cb, cEntries: DWORD): DWORD; stdcall;

TfnRasEnumEntriesW = function(
                 lpszReserved, lpszPhonebook: PWideChar;
                 pEntryname: PRASENTRYNAMEW;
                 var cb, cEntries: DWORD): DWORD; stdcall;

TfnRasEnumEntries = function(
                 lpszReserved, lpszPhonebook: PChar;
                 pEntryname: PRASENTRYNAME;
                 var cb, cEntries: DWORD): DWORD; stdcall;

TfnRasGetConnectStatusA = function(hRasConn: HRASCON; var status: TRASConnStatusA): DWORD; stdcall;
TfnRasGetConnectStatusW = function(hRasConn: HRASCON; var status: TRASConnStatusW): DWORD; stdcall;
TfnRasGetConnectStatus = function(hRasConn: HRASCON; var status: TRASConnStatus): DWORD; stdcall;

TfnRasGetErrorStringA = function(error: UINT; errorString: PChar; cb: DWORD ): DWORD; stdcall;
TfnRasGetErrorStringW = function(error: UINT; errorString: PWideChar; cb: DWORD ): DWORD; stdcall;
TfnRasGetErrorString = function(error: UINT; errorString: PChar; cb: DWORD ): DWORD; stdcall;

TfnRASHangUpA = function(hRasConn: HRASCON): DWORD; stdcall;
TfnRASHangUpW = function(hRasConn: HRASCON): DWORD; stdcall;
TfnRASHangUp = function(hRasConn: HRASCON): DWORD; stdcall;

TfnRasGetProjectionInfoA = function(
                 hRasConn: HRASCON;
                 projection: TRasProjection;
                 lpProjection: Pointer;
                 var cb: DWORD): DWORD; stdcall;

TfnRasGetProjectionInfoW = function(
                 hRasConn: HRASCON;
                 projection: TRasProjection;
                 lpProjection: Pointer;
                 var cb: DWORD): DWORD; stdcall;

TfnRasGetProjectionInfo = function(
                 hRasConn: HRASCON;
                 projection: TRasProjection;
                 lpProjection: Pointer;
                 var cb: DWORD): DWORD; stdcall;

TfnRasCreatePhonebookEntryA = function(HWND: hWnd; lpszPhonebook: PChar): DWORD; stdcall;
TfnRasCreatePhonebookEntryW = function(HWND: hWnd; lpszPhonebook: PWideChar): DWORD; stdcall;
TfnRasCreatePhonebookEntry = function(HWND: hWnd; lpszPhonebook: PChar): DWORD; stdcall;

TfnRasEditPhonebookEntryA = function(HWND: hWnd; lpszPhonebook, lpszEntryname: PChar): DWORD; stdcall;
TfnRasEditPhonebookEntryW = function(HWND: hWnd; lpszPhonebook, lpszEntryname: PWideChar): DWORD; stdcall;
TfnRasEditPhonebookEntry = function(HWND: hWnd; lpszPhonebook, lpszEntryname: PChar): DWORD; stdcall;

TfnRasSetEntryDialParamsA = function(lpszPhonebook: PChar; const rasDialParams: TRasDialParamsA; fRemovePassword: BOOL): DWORD; stdcall;
TfnRasSetEntryDialParamsW = function(lpszPhonebook: PWideChar; const rasDialParams: TRasDialParamsW; fRemovePassword: BOOL): DWORD; stdcall;
TfnRasSetEntryDialParams = function(lpszPhonebook: PChar; const rasDialParams: TRasDialParams; fRemovePassword: BOOL): DWORD; stdcall;

TfnRasGetEntryDialParamsA = function(lpszPhonebook: PChar; var rasDialParams: TRasDialParamsA; var fRemovePassword: BOOL): DWORD; stdcall;
TfnRasGetEntryDialParamsW = function(lpszPhonebook: PWideChar; var rasDialParams: TRasDialParamsW; var fRemovePassword: BOOL): DWORD; stdcall;
TfnRasGetEntryDialParams = function(lpszPhonebook: PChar; var rasDialParams: TRasDialParams; var fRemovePassword: BOOL): DWORD; stdcall;

TfnRasEnumDevicesA = function(devices: PRASDevInfoA; var cb, cDevices: DWORD): DWORD; stdcall;
TfnRasEnumDevicesW = function(devices: PRASDevInfoW; var cb, cDevices: DWORD): DWORD; stdcall;
TfnRasEnumDevices = function(devices: PRASDevInfo; var cb, cDevices: DWORD): DWORD; stdcall;

TfnRasGetCountryInfoA = function(countryInfo: PRasCtryInfoA; var cb: DWORD): DWORD; stdcall;
TfnRasGetCountryInfoW = function(countryInfo: PRasCtryInfoW; var cb: DWORD): DWORD; stdcall;
TfnRasGetCountryInfo = function(countryInfo: PRasCtryInfo; var cb: DWORD): DWORD; stdcall;

TfnRasGetEntryPropertiesA = function(
                  lpszPhonebook, lpszEntry: PChar;
                  lpRasEntry: PRasEntryA;
                  EntrySize: PDWORD;
                  DeviceInfo: PBYTE;
                  DeviceInfoSize: PDWORD): DWORD; stdcall;

TfnRasGetEntryPropertiesW = function(
                  lpszPhonebook, lpszEntry: PWideChar;
                  lpRasEntry: PRasEntryW;
                  EntrySize: PDWORD;
                  DeviceInfo: PBYTE;
                  DeviceInfoSize: PDWORD): DWORD; stdcall;

TfnRasGetEntryProperties = function(
                  lpszPhonebook, lpszEntry: PChar;
                  lpRasEntry: PRasEntry;
                  EntrySize: PDWORD;
                  DeviceInfo: PBYTE;
                  DeviceInfoSize: PDWORD): DWORD; stdcall;

TfnRasSetEntryPropertiesA = function(
                  lpszPhonebook, lpszEntry: PChar;
                  const RasEntry: TRasEntryA;
                  EntrySize: DWORD;
                  DeviceInfo: PBYTE;
                  DeviceInfoSize: DWORD): DWORD; stdcall;

TfnRasSetEntryPropertiesW = function(
                  lpszPhonebook, lpszEntry: PWideChar;
                  const RasEntry: TRasEntryW;
                  EntrySize: DWORD;
                  DeviceInfo: PBYTE;
                  DeviceInfoSize: DWORD): DWORD; stdcall;

TfnRasSetEntryProperties = function(
                  lpszPhonebook, lpszEntry: PChar;
                  const RasEntry: TRasEntry;
                  EntrySize: DWORD;
                  DeviceInfo: PBYTE;
                  DeviceInfoSize: DWORD): DWORD; stdcall;

TfnRasRenameEntryA = function(lpszPhonebook, lpszOldEntry, lpszNewEntry: PChar): DWORD; stdcall;
TfnRasRenameEntryW = function(lpszPhonebook, lpszOldEntry, lpszNewEntry: PWideChar): DWORD; stdcall;
TfnRasRenameEntry = function(lpszPhonebook, lpszOldEntry, lpszNewEntry: PChar): DWORD; stdcall;

TfnRasDeleteEntryA = function(lpszPhonebook, lpszEntry: PChar): DWORD; stdcall;
TfnRasDeleteEntryW = function(lpszPhonebook, lpszEntry: PWideChar): DWORD; stdcall;
TfnRasDeleteEntry = function(lpszPhonebook, lpszEntry: PChar): DWORD; stdcall;

TfnRasValidateEntryNameA = function(lpszPhonebook, lpszEntry: PChar): DWORD; stdcall;
TfnRasValidateEntryNameW = function(lpszPhonebook, lpszEntry: PWideChar): DWORD; stdcall;
TfnRasValidateEntryName = function(lpszPhonebook, lpszEntry: PChar): DWORD; stdcall;

TfnRasGetSubEntryHandleA = function(hRasConn: HRASCON; dwSubEntry: DWORD; var handle: HRASCON): DWORD; stdcall;
TfnRasGetSubEntryHandleW = function(hRasConn: HRASCON; dwSubEntry: DWORD; var handle: HRASCON): DWORD; stdcall;
TfnRasGetSubEntryHandle = function(hRasConn: HRASCON; dwSubEntry: DWORD; var handle: HRASCON): DWORD; stdcall;
TfnRasGetCredentialsA = function(lpszPhonebook, lpszEntry: PChar; var Credentials: TRASCredentialsA): DWORD; stdcall;
TfnRasGetCredentialsW = function(lpszPhonebook, lpszEntry: PChar; var Credentials: TRASCredentialsW): DWORD; stdcall;
TfnRasGetCredentials = function(lpszPhonebook, lpszEntry: PChar; var Credentials: TRASCredentials): DWORD; stdcall;
TfnRasSetCredentialsA = function(lpszPhonebook, lpszEntry: PChar; const Credentials: TRASCredentialsA; fClearConnections: Boolean): DWORD; stdcall;
TfnRasSetCredentialsW = function(lpszPhonebook, lpszEntry: PChar; const Credentials: TRASCredentialsW; fClearConnections: Boolean): DWORD; stdcall;
TfnRasSetCredentials = function(lpszPhonebook, lpszEntry: PChar; const Credentials: TRASCredentials; fClearConnections: Boolean): DWORD; stdcall;
TfnRasConnectionNotificationA = function(hRasConn: HRASCON; hEvent: THandle; dwFlags: DWORD ): DWORD; stdcall;
TfnRasConnectionNotificationW = function(hRasConn: HRASCON; hEvent: THandle; dwFlags: DWORD ): DWORD; stdcall;
TfnRasConnectionNotification = function(hRasConn: HRASCON; hEvent: THandle; dwFlags: DWORD ): DWORD; stdcall;
TfnRasGetSubEntryPropertiesA = function(lpszPhonebook, lpszEntry: PChar; dwSubEntry: DWORD; subEntry: PRasSubEntryA; pcb: PDWORD): DWORD; stdcall;
TfnRasGetSubEntryPropertiesW = function(lpszPhonebook, lpszEntry: PWideChar; dwSubEntry: DWORD; subEntry: PRasSubEntryW; pcb: PDWORD): DWORD; stdcall;
TfnRasGetSubEntryProperties = function(lpszPhonebook, lpszEntry: PChar; dwSubEntry: DWORD; subEntry: PRasSubEntry; pcb: PDWORD): DWORD; stdcall;
TfnRasSetSubEntryPropertiesA = function(lpszPhonebook, lpszEntry: PChar; dwSubEntry: DWORD; subEntry: PRasSubEntryA; cb: DWORD): DWORD; stdcall;
TfnRasSetSubEntryPropertiesW = function(lpszPhonebook, lpszEntry: PWideChar; dwSubEntry: DWORD; subEntry: PRasSubEntryW; cb: DWORD): DWORD; stdcall;
TfnRasSetSubEntryProperties = function(lpszPhonebook, lpszEntry: PChar; dwSubEntry: DWORD; subEntry: PRasSubEntry; cb: DWORD): DWORD; stdcall;
TfnRasGetAutodialAddressA = function(lpszAddress: PChar; lpdwReserved: PDWORD; entries: PRASAutoDialEntryA; var cb, cEntries: DWORD): DWORD; stdcall;
TfnRasGetAutodialAddressW = function(lpszAddress: PWideChar; lpdwReserved: PDWORD; entries: PRASAutoDialEntryW; var cb, cEntries: DWORD): DWORD; stdcall;
TfnRasGetAutodialAddress = function(lpszAddress: PChar; lpdwReserved: PDWORD; entries: PRASAutoDialEntry; var cb, cEntries: DWORD): DWORD; stdcall;
TfnRasSetAutodialAddressA = function(lpszAddress: PChar; dwReserved: DWORD; entries: PRASAutoDialEntry; cb, cEntries: DWORD): DWORD; stdcall;
TfnRasSetAutodialAddressW = function(lpszAddress: PWideChar; dwReserved: DWORD; entries: PRASAutoDialEntryW; cb, cEntries: DWORD): DWORD; stdcall;
TfnRasSetAutodialAddress = function(lpszAddress: PChar; dwReserved: DWORD; entries: PRASAutoDialEntry; cb, cEntries: DWORD): DWORD; stdcall;
TfnRasEnumAutodialAddressesA = function(addresses: PPChar; var cbAddresses, cAddresses: DWORD): DWORD; stdcall;
TfnRasEnumAutodialAddressesW = function(addresses: PPWideChar; var cbAddresses, cAddresses: DWORD): DWORD; stdcall;
TfnRasEnumAutodialAddresses = function(addresses: PPChar; var cbAddresses, cAddresses: DWORD): DWORD; stdcall;
TfnRasGetAutodialEnableA = function(dwDiallingLocation: DWORD; var enabled: BOOL): DWORD; stdcall;
TfnRasGetAutodialEnableW = function(dwDiallingLocation: DWORD; var enabled: BOOL): DWORD; stdcall;
TfnRasGetAutodialEnable = function(dwDiallingLocation: DWORD; var enabled: BOOL): DWORD; stdcall;
TfnRasSetAutodialEnableA = function(dwDiallingLocation: DWORD; enabled: BOOL): DWORD; stdcall;
TfnRasSetAutodialEnableW = function(dwDiallingLocation: DWORD; enabled: BOOL): DWORD; stdcall;
TfnRasSetAutodialEnable = function(dwDiallingLocation: DWORD; enabled: BOOL): DWORD; stdcall;
TfnRasGetAutodialParamA = function(dwDiallingLocation: DWORD; param: Pointer; var cb: DWORD): DWORD; stdcall;
TfnRasGetAutodialParamW = function(dwDiallingLocation: DWORD; param: Pointer; var cb: DWORD): DWORD; stdcall;
TfnRasGetAutodialParam = function(dwDiallingLocation: DWORD; param: Pointer; var cb: DWORD): DWORD; stdcall;
TfnRasSetAutodialParamA = function(dwDiallingLocation: DWORD; param: Pointer; cb: DWORD): DWORD; stdcall;
TfnRasSetAutodialParamW = function(dwDiallingLocation: DWORD; param: Pointer; cb: DWORD): DWORD; stdcall;
TfnRasSetAutodialParam = function(dwDiallingLocation: DWORD; param: Pointer; cb: DWORD): DWORD; stdcall;

var
RasDialA: TfnRasDialA = nil;
RasDialW: TfnRasDialW = nil;
RasDial : TfnRasDial  = nil;

RasEnumConnectionsA: TfnRasEnumConnectionsA = nil;
RasEnumConnectionsW: TfnRasEnumConnectionsW = nil;
RasEnumConnections: TfnRasEnumConnections = nil;

RasEnumEntriesA: TfnRasEnumEntriesA = nil;
RasEnumEntriesW: TfnRasEnumEntriesW = nil;
RasEnumEntries: TfnRasEnumEntries = nil;

RasGetConnectStatusA: TfnRasGetConnectStatusA = nil;
RasGetConnectStatusW: TfnRasGetConnectStatusW = nil;
RasGetConnectStatus: TfnRasGetConnectStatus = nil;

RasGetErrorStringA: TfnRasGetErrorStringA = nil;
RasGetErrorStringW: TfnRasGetErrorStringW = nil;
RasGetErrorString: TfnRasGetErrorString = nil;

RASHangUpA: TfnRASHangUpA = nil;
RASHangUpW: TfnRASHangUpW = nil;
RASHangUp: TfnRASHangUp = nil;

RasGetProjectionInfoA: TfnRasGetProjectionInfoA = nil;
RasGetProjectionInfoW: TfnRasGetProjectionInfoW = nil;
RasGetProjectionInfo: TfnRasGetProjectionInfo = nil;
RasCreatePhonebookEntryA: TfnRasCreatePhonebookEntryA = nil;
RasCreatePhonebookEntryW: TfnRasCreatePhonebookEntryW = nil;
RasCreatePhonebookEntry: TfnRasCreatePhonebookEntry = nil;

RasEditPhonebookEntryA: TfnRasEditPhonebookEntryA = nil;
RasEditPhonebookEntryW: TfnRasEditPhonebookEntryW = nil;
RasEditPhonebookEntry: TfnRasEditPhonebookEntry = nil;

RasSetEntryDialParamsA: TfnRasSetEntryDialParamsA = nil;
RasSetEntryDialParamsW: TfnRasSetEntryDialParamsW = nil;
RasSetEntryDialParams: TfnRasSetEntryDialParams = nil;

RasGetEntryDialParamsA: TfnRasGetEntryDialParamsA = nil;
RasGetEntryDialParamsW: TfnRasGetEntryDialParamsW = nil;
RasGetEntryDialParams: TfnRasGetEntryDialParams = nil;

RasEnumDevicesA: TfnRasEnumDevicesA = nil;
RasEnumDevicesW: TfnRasEnumDevicesW = nil;
RasEnumDevices: TfnRasEnumDevices = nil;

RasGetCountryInfoA: TfnRasGetCountryInfoA = nil;
RasGetCountryInfoW: TfnRasGetCountryInfoW = nil;
RasGetCountryInfo: TfnRasGetCountryInfo = nil;

RasGetEntryPropertiesA: TfnRasGetEntryPropertiesA = nil;
RasGetEntryPropertiesW: TfnRasGetEntryPropertiesW = nil;
RasGetEntryProperties: TfnRasGetEntryProperties = nil;
RasSetEntryPropertiesA: TfnRasSetEntryPropertiesA = nil;
RasSetEntryPropertiesW: TfnRasSetEntryPropertiesW = nil;
RasSetEntryProperties: TfnRasSetEntryProperties = nil;
RasRenameEntryA: TfnRasRenameEntryA = nil;
RasRenameEntryW: TfnRasRenameEntryW = nil;
RasRenameEntry: TfnRasRenameEntry = nil;
RasDeleteEntryA: TfnRasDeleteEntryA = nil;
RasDeleteEntryW: TfnRasDeleteEntryW = nil;
RasDeleteEntry: TfnRasDeleteEntry = nil;

RasValidateEntryNameA: TfnRasValidateEntryNameA = nil;
RasValidateEntryNameW: TfnRasValidateEntryNameW = nil;
RasValidateEntryName: TfnRasValidateEntryName = nil;

RasGetSubEntryHandleA: TfnRasGetSubEntryHandleA = nil;
RasGetSubEntryHandleW: TfnRasGetSubEntryHandleW = nil;
RasGetSubEntryHandle: TfnRasGetSubEntryHandle = nil;
RasGetCredentialsA: TfnRasGetCredentialsA = nil;
RasGetCredentialsW: TfnRasGetCredentialsW = nil;
RasGetCredentials: TfnRasGetCredentials = nil;
RasSetCredentialsA: TfnRasSetCredentialsA = nil;
RasSetCredentialsW: TfnRasSetCredentialsW = nil;
RasSetCredentials: TfnRasSetCredentials = nil;
RasConnectionNotificationA: TfnRasConnectionNotificationA = nil;
RasConnectionNotificationW: TfnRasConnectionNotificationW = nil;
RasConnectionNotification: TfnRasConnectionNotification = nil;
RasGetSubEntryPropertiesA: TfnRasGetSubEntryPropertiesA = nil;
RasGetSubEntryPropertiesW: TfnRasGetSubEntryPropertiesW = nil;
RasGetSubEntryProperties: TfnRasGetSubEntryProperties = nil;
RasSetSubEntryPropertiesA: TfnRasSetSubEntryPropertiesA = nil;
RasSetSubEntryPropertiesW: TfnRasSetSubEntryPropertiesW = nil;
RasSetSubEntryProperties: TfnRasSetSubEntryProperties = nil;
RasGetAutodialAddressA: TfnRasGetAutodialAddressA = nil;
RasGetAutodialAddressW: TfnRasGetAutodialAddressW = nil;
RasGetAutodialAddress: TfnRasGetAutodialAddress = nil;
RasSetAutodialAddressA: TfnRasSetAutodialAddressA = nil;
RasSetAutodialAddressW: TfnRasSetAutodialAddressW = nil;
RasSetAutodialAddress: TfnRasSetAutodialAddress = nil;
RasEnumAutodialAddressesA: TfnRasEnumAutodialAddressesA = nil;
RasEnumAutodialAddressesW: TfnRasEnumAutodialAddressesW = nil;
RasEnumAutodialAddresses: TfnRasEnumAutodialAddresses = nil;
RasGetAutodialEnableA: TfnRasGetAutodialEnableA = nil;
RasGetAutodialEnableW: TfnRasGetAutodialEnableW = nil;
RasGetAutodialEnable: TfnRasGetAutodialEnable = nil;
RasSetAutodialEnableA: TfnRasSetAutodialEnableA = nil;
RasSetAutodialEnableW: TfnRasSetAutodialEnableW = nil;
RasSetAutodialEnable: TfnRasSetAutodialEnable = nil;
RasGetAutodialParamA: TfnRasGetAutodialParamA = nil;
RasGetAutodialParamW: TfnRasGetAutodialParamW = nil;
RasGetAutodialParam: TfnRasGetAutodialParam = nil;
RasSetAutodialParamA: TfnRasSetAutodialParamA = nil;
RasSetAutodialParamW: TfnRasSetAutodialParamW = nil;
RasSetAutodialParam: TfnRasSetAutodialParam = nil;

function RasGetStatusString(status: Integer): string;
function InitRasLibrary: Boolean;
procedure FreeRASLibrary;

implementation

uses syncobjs, sysutils;

const
  rasdll = 'rasapi32.dll';

var
  RASLibrary: THandle = 0;
  ReferenceCount: Integer = 0;
  Lock: TCriticalSection;
  RasVer401: Boolean = False;

function RasGetStatusString(status: Integer): string;
begin
  case status of
    RASCS_OpenPort            : Result := 'Opening Port';
    RASCS_PortOpened          : Result := 'Port Opened';
    RASCS_ConnectDevice       : Result := 'Connecting to Device';
    RASCS_DeviceConnected     : Result := 'Device Connected';
    RASCS_AllDevicesConnected : Result := 'All Devices Connected';
    RASCS_Authenticate        : Result := 'Authenticating';
    RASCS_AuthNotify          : Result := 'Authenticating - Notify';
    RASCS_AuthRetry           : Result := 'Authenticating - Retry';
    RASCS_AuthCallback        : Result := 'Authenticating - Callback';
    RASCS_AuthChangePassword  : Result := 'Authenticating - Change Password';
    RASCS_AuthProject         : Result := 'Authenticating - Project';
    RASCS_AuthLinkSpeed       : Result := 'Authenticating - Link Speed';
    RASCS_AuthAck             : Result := 'Authenticating - Ack';
    RASCS_ReAuthenticate      : Result := 'Re-Authenticating';
    RASCS_Authenticated       : Result := 'Authenticated';
    RASCS_PrepareForCallback  : Result := 'Preparing Callback';
    RASCS_WaitForModemReset   : Result := 'Waiting for Modem Reset';
    RASCS_WaitForCallback     : Result := 'Waiting for Callback';
    RASCS_Projected           : Result := 'Projected';

    RASCS_StartAuthentication : Result := 'Starting Authentication';
    RASCS_CallbackComplete    : Result := 'Callback Completed';
    RASCS_LogonNetwork        : Result := 'Network Logon';
    RASCS_SubEntryConnected   : Result := 'Sub-Entry Connected';
    RASCS_SubEntryDisconnected: Result := 'Syb-Entry Disconnected';

    RASCS_Interactive         : Result := 'Paused - Interactive';
    RASCS_RetryAuthentication : Result := 'Retry Authentication';
    RASCS_CallbackSetByCaller : Result := 'Callback set by Caller';
    RASCS_PasswordExpired     : Result := 'Password Expired';

    RASCS_Connected           : Result := 'Connected';
    RASCS_Disconnected        : Result := 'Disconnected';
  else
    Result := 'Unknown status ' + IntToStr(status);
  end
end;

function InitRasLibrary: Boolean;
begin
  Lock.Enter;
  try
    Inc(ReferenceCount);

    if (ReferenceCount = 1) and (RASLibrary = 0) then
    begin
      RASLibrary := LoadLibrary(rasdll);
      if RASLibrary > 0 then
      begin
        RasDialA := GetProcAddress(RASLibrary, 'RasDialA');
        RasDialW := GetProcAddress(RASLibrary, 'RasDialW');
{$ifdef COMPILER12_UP}
        RasDial := RasDialW;
{$else}
        RasDial := RasDialA;
{$endif}

        RasEnumConnectionsA := GetProcAddress(RASLibrary, 'RasEnumConnectionsA');
        RasEnumConnectionsW := GetProcAddress(RASLibrary, 'RasEnumConnectionsW');
{$ifdef COMPILER12_UP}
        RasEnumConnections := TfnRasEnumConnections(RasEnumConnectionsW);
{$else}
        RasEnumConnections := TfnRasEnumConnections(RasEnumConnectionsA);
{$endif}

        RasEnumEntriesA := GetProcAddress(RASLibrary, 'RasEnumEntriesA');
        RasEnumEntriesW := GetProcAddress(RASLibrary, 'RasEnumEntriesW');
{$ifdef COMPILER12_UP}
        RasEnumEntries := TfnRasEnumEntries(RasEnumEntriesW);
{$else}
        RasEnumEntries := TfnRasEnumEntries(RasEnumEntriesA);
{$endif}

        RasGetConnectStatusA := GetProcAddress(RASLibrary, 'RasGetConnectStatusA');
        RasGetConnectStatusW := GetProcAddress(RASLibrary, 'RasGetConnectStatusW');
{$ifdef COMPILER12_UP}
        RasGetConnectStatus := RasGetConnectstatusW;
{$else}
        RasGetConnectStatus := RasGetConnectstatusA;
{$endif}

        RasGetErrorStringA := GetProcAddress(RASLibrary, 'RasGetErrorStringA');
        RasGetErrorStringW := GetProcAddress(RASLibrary, 'RasGetErrorStringW');
{$ifdef COMPILER12_UP}
        RasGetErrorString := RasGetErrorStringW;
{$else}
        RasGetErrorString := RasGetErrorStringA;
{$endif}

        RasHangUpA := GetProcAddress(RASLibrary, 'RasHangUpA');
        RasHangUpW := GetProcAddress(RASLibrary, 'RasHangUpW');
{$ifdef COMPILER12_UP}
        RasHangUp := RasHangUpW;
{$else}
        RasHangUp := RasHangUpA;
{$endif}

        RasGetProjectionInfoA := GetProcAddress(RASLibrary, 'RasGetProjectionInfoA');
        RasGetProjectionInfoW := GetProcAddress(RASLibrary, 'RasGetProjectionInfoW');
{$ifdef COMPILER12_UP}
        RasGetProjectionInfo := RasGetProjectionInfoW;
{$else}
        RasGetProjectionInfo := RasGetProjectionInfoA;
{$endif}

        RasCreatePhonebookEntryA := GetProcAddress(RASLibrary, 'RasCreatePhonebookEntryA');
        RasCreatePhonebookEntryW := GetProcAddress(RASLibrary, 'RasCreatePhonebookEntryW');
{$ifdef COMPILER12_UP}
        RasCreatePhonebookEntry := RasCreatePhonebookEntryW;
{$else}
        RasCreatePhonebookEntry := RasCreatePhonebookEntryA;
{$endif}

        RasEditPhonebookEntryA := GetProcAddress(RASLibrary, 'RasEditPhonebookEntryA');
        RasEditPhonebookEntryW := GetProcAddress(RASLibrary, 'RasEditPhonebookEntryW');
{$ifdef COMPILER12_UP}
        RasEditPhonebookEntry := RasEditPhonebookEntryW;
{$else}
        RasEditPhonebookEntry := RasEditPhonebookEntryA;
{$endif}

        RasSetEntryDialParamsA := GetProcAddress(RASLibrary, 'RasSetEntryDialParamsA');
        RasSetEntryDialParamsW := GetProcAddress(RASLibrary, 'RasSetEntryDialParamsW');
{$ifdef COMPILER12_UP}
        RasSetEntryDialParams := RasSetEntryDialParamsW;
{$else}
        RasSetEntryDialParams := RasSetEntryDialParamsA;
{$endif}

        RasGetEntryDialParamsA := GetProcAddress(RASLibrary, 'RasGetEntryDialParamsA');
        RasGetEntryDialParamsW := GetProcAddress(RASLibrary, 'RasGetEntryDialParamsW');
{$ifdef COMPILER12_UP}
        RasGetEntryDialParams := RasGetEntryDialParamsW;
{$else}
        RasGetEntryDialParams := RasGetEntryDialParamsA;
{$endif}

        RasEnumDevicesA := GetProcAddress(RASLibrary, 'RasEnumDevicesA');
        RasEnumDevicesW := GetProcAddress(RASLibrary, 'RasEnumDevicesW');
{$ifdef COMPILER12_UP}
        RasEnumDevices := TfnRasEnumDevices(RasEnumDevicesW);
{$else}
        RasEnumDevices := TfnRasEnumDevices(RasEnumDevicesA);
{$endif}

        RasGetCountryInfoA := GetProcAddress(RASLibrary, 'RasGetCountryInfoA');
        RasGetCountryInfoW := GetProcAddress(RASLibrary, 'RasGetCountryInfoW');
{$ifdef COMPILER12_UP}
        RasGetCountryInfo := TfnRasGetCountryInfo(RasGetCountryInfoW);
{$else}
        RasGetCountryInfo := TfnRasGetCountryInfo(RasGetCountryInfoA);
{$endif}

        RasGetEntryPropertiesA := GetProcAddress(RASLibrary, 'RasGetEntryPropertiesA');
        RasGetEntryPropertiesW := GetProcAddress(RASLibrary, 'RasGetEntryPropertiesW');
{$ifdef COMPILER12_UP}
        RasGetEntryProperties := TfnRasGetEntryProperties(RasGetEntryPropertiesW);
{$else}
        RasGetEntryProperties := TfnRasGetEntryProperties(RasGetEntryPropertiesA);
{$endif}

        RasSetEntryPropertiesA := GetProcAddress(RASLibrary, 'RasSetEntryPropertiesA');
        RasSetEntryPropertiesW := GetProcAddress(RASLibrary, 'RasSetEntryPropertiesW');
{$ifdef COMPILER12_UP}
        RasSetEntryProperties := RasSetEntryPropertiesW;
{$else}
        RasSetEntryProperties := RasSetEntryPropertiesA;
{$endif}

        RasRenameEntryA := GetProcAddress(RASLibrary, 'RasRenameEntryA');
        RasRenameEntryW := GetProcAddress(RASLibrary, 'RasRenameEntryW');
{$ifdef COMPILER12_UP}
        RasRenameEntry := RasRenameEntryW;
{$else}
        RasRenameEntry := RasRenameEntryA;
{$endif}

        RasDeleteEntryA := GetProcAddress(RASLibrary, 'RasDeleteEntryA');
        RasDeleteEntryW := GetProcAddress(RASLibrary, 'RasDeleteEntryW');
{$ifdef COMPILER12_UP}
        RasDeleteEntry := RasDeleteEntryW;
{$else}
        RasDeleteEntry := RasDeleteEntryA;
{$endif}

        RasValidateEntryNameA := GetProcAddress(RASLibrary, 'RasValidateEntryNameA');
        RasValidateEntryNameW := GetProcAddress(RASLibrary, 'RasValidateEntryNameW');
{$ifdef COMPILER12_UP}
        RasValidateEntryName := RasValidateEntryNameW;
{$else}
        RasValidateEntryName := RasValidateEntryNameA;
{$endif}

        RasGetSubEntryHandleA := GetProcAddress(RASLibrary, 'RasGetSubEntryHandleA');

        if Assigned(RasGetSubEntryHandleA) then
        begin
          RasVer401 := True;
          RasGetSubEntryHandleW := GetProcAddress(RASLibrary, 'RasGetSubEntryHandleW');
{$ifdef COMPILER12_UP}
          RasGetSubEntryHandle := RasGetSubEntryHandleW;
{$else}
          RasGetSubEntryHandle := RasGetSubEntryHandleA;
{$endif}

          RasGetCredentialsA := GetProcAddress(RASLibrary, 'RasGetCredentialsA');
          RasGetCredentialsW := GetProcAddress(RASLibrary, 'RasGetCredentialsW');
{$ifdef COMPILER12_UP}
          RasGetCredentials := RasGetCredentialsW;
{$else}
          RasGetCredentials := RasGetCredentialsA;
{$endif}

          RasSetCredentialsA := GetProcAddress(RASLibrary, 'RasSetCredentialsA');
          RasSetCredentialsW := GetProcAddress(RASLibrary, 'RasSetCredentialsW');
{$ifdef COMPILER12_UP}
          RasSetCredentials := RasSetCredentialsW;
{$else}
          RasSetCredentials := RasSetCredentialsA;
{$endif}

          RasConnectionNotificationA := GetProcAddress(RASLibrary, 'RasConnectionNotificationA');
          RasConnectionNotificationW := GetProcAddress(RASLibrary, 'RasConnectionNotificationW');
{$ifdef COMPILER12_UP}
          RasConnectionNotification := RasConnectionNotificationW;
{$else}
          RasConnectionNotification := RasConnectionNotificationA;
{$endif}

          RasGetSubEntryPropertiesA := GetProcAddress(RASLibrary, 'RasGetSubEntryPropertiesA');
          RasGetSubEntryPropertiesW := GetProcAddress(RASLibrary, 'RasGetSubEntryPropertiesW');
{$ifdef COMPILER12_UP}
          RasGetSubEntryProperties := TfnRasGetSubEntryProperties(RasGetSubEntryPropertiesW);
{$else}
          RasGetSubEntryProperties := TfnRasGetSubEntryProperties(RasGetSubEntryPropertiesA);
{$endif}

          RasSetSubEntryPropertiesA := GetProcAddress(RASLibrary, 'RasSetSubEntryPropertiesA');
          RasSetSubEntryPropertiesW := GetProcAddress(RASLibrary, 'RasSetSubEntryPropertiesW');
{$ifdef COMPILER12_UP}
          RasSetSubEntryProperties := TfnRasSetSubEntryProperties(RasSetSubEntryPropertiesW);
{$else}
          RasSetSubEntryProperties := TfnRasSetSubEntryProperties(RasSetSubEntryPropertiesA);
{$endif}

          RasGetAutodialAddressA := GetProcAddress(RASLibrary, 'RasGetAutodialAddressA');
          RasGetAutodialAddressW := GetProcAddress(RASLibrary, 'RasGetAutodialAddressW');
{$ifdef COMPILER12_UP}
          RasGetAutodialAddress := RasGetAutodialAddressW;
{$else}
          RasGetAutodialAddress := RasGetAutodialAddressA;
{$endif}

          RasSetAutodialAddressA := GetProcAddress(RASLibrary, 'RasSetAutodialAddressA');
          RasSetAutodialAddressW := GetProcAddress(RASLibrary, 'RasSetAutodialAddressW');
{$ifdef COMPILER12_UP}
          RasSetAutodialAddress := RasSetAutodialAddressW;
{$else}
          RasSetAutodialAddress := RasSetAutodialAddressA;
{$endif}

          RasEnumAutodialAddressesA := GetProcAddress(RASLibrary, 'RasEnumAutodialAddressesA');
          RasEnumAutodialAddressesW := GetProcAddress(RASLibrary, 'RasEnumAutodialAddressesW');
{$ifdef COMPILER12_UP}
          RasEnumAutodialAddresses := RasEnumAutodialAddressesW;
{$else}
          RasEnumAutodialAddresses := RasEnumAutodialAddressesA;
{$endif}

          RasGetAutodialEnableA := GetProcAddress(RASLibrary, 'RasGetAutodialEnableA');
          RasGetAutodialEnableW := GetProcAddress(RASLibrary, 'RasGetAutodialEnableW');
{$ifdef COMPILER12_UP}
          RasGetAutodialEnable := RasGetAutodialEnableW;
{$else}
          RasGetAutodialEnable := RasGetAutodialEnableA;
{$endif}

          RasSetAutodialEnableA := GetProcAddress(RASLibrary, 'RasSetAutodialEnableA');
          RasSetAutodialEnableW := GetProcAddress(RASLibrary, 'RasSetAutodialEnableW');
{$ifdef COMPILER12_UP}
          RasSetAutodialEnable := RasSetAutodialEnableW;
{$else}
          RasSetAutodialEnable := RasSetAutodialEnableA;
{$endif}

          RasGetAutodialParamA := GetProcAddress(RASLibrary, 'RasGetAutodialParamA');
          RasGetAutodialParamW := GetProcAddress(RASLibrary, 'RasGetAutodialParamW');
{$ifdef COMPILER12_UP}
          RasGetAutodialParam := RasGetAutodialParamW;
{$else}
          RasGetAutodialParam := RasGetAutodialParamA;
{$endif}

          RasSetAutodialParamA := GetProcAddress(RASLibrary, 'RasSetAutodialParamA');
          RasSetAutodialParamW := GetProcAddress(RASLibrary, 'RasSetAutodialParamW');
{$ifdef COMPILER12_UP}
          RasSetAutodialParam := RasSetAutodialParamW;
{$else}
          RasSetAutodialParam := RasSetAutodialParamA;
{$endif}
        end
      end;
    end;
    Result := RasLibrary > 0;
  finally
    Lock.Leave;
  end;
end;

procedure FreeRASLibrary;
begin
  Lock.Enter;
  try
    if ReferenceCount > 0 then
      Dec(ReferenceCount);

    if (RasLibrary <> 0) and (ReferenceCount = 0) then
    begin
      FreeLibrary(RasLibrary);
      RasLibrary := 0;

      RasDialA := nil;
      RasDialA := nil;
      RasDialW := nil;
      RasEnumConnectionsA := nil;
      RasEnumConnectionsW := nil;
      RasEnumEntriesA := nil;
      RasEnumEntriesW := nil;
      RasGetConnectStatusA := nil;
      RasGetConnectStatusW := nil;
      RasGetErrorStringA := nil;
      RasGetErrorStringW := nil;
      RasHangUpA := nil;
      RasHangUpW := nil;
      RasGetProjectionInfoA := nil;
      RasGetProjectionInfoW := nil;
      RasCreatePhonebookEntryA := nil;
      RasCreatePhonebookEntryW := nil;
      RasEditPhonebookEntryA := nil;
      RasEditPhonebookEntryW := nil;
      RasSetEntryDialParamsA := nil;
      RasSetEntryDialParamsW := nil;
      RasGetEntryDialParamsA := nil;
      RasGetEntryDialParamsW := nil;
      RasEnumDevicesA := nil;
      RasEnumDevicesW := nil;
      RasGetCountryInfoA := nil;
      RasGetCountryInfoW := nil;
      RasGetEntryPropertiesA := nil;
      RasGetEntryPropertiesW := nil;
      RasSetEntryPropertiesA := nil;
      RasSetEntryPropertiesW := nil;
      RasRenameEntryA := nil;
      RasRenameEntryW := nil;
      RasDeleteEntryA := nil;
      RasDeleteEntryW := nil;
      RasValidateEntryNameA := nil;
      RasValidateEntryNameW := nil;
      RasDial := nil;
      RasEnumConnections := nil;
      RasEnumEntries := nil;
      RasGetConnectStatus := nil;
      RasGetErrorString := nil;
      RasHangUp := nil;
      RasGetProjectionInfo := nil;
      RasCreatePhonebookEntry := nil;
      RasEditPhonebookEntry := nil;
      RasSetEntryDialParams := nil;
      RasGetEntryDialParams := nil;
      RasEnumDevices := nil;
      RasGetCountryInfo := nil;
      RasGetEntryProperties := nil;
      RasSetEntryProperties := nil;
      RasRenameEntry := nil;
      RasDeleteEntry := nil;
      RasValidateEntryName := nil;


      RasGetSubEntryHandleA := nil;
      RasGetSubEntryHandleW := nil;
      RasGetCredentialsA := nil;
      RasGetCredentialsW := nil;
      RasSetCredentialsA := nil;
      RasSetCredentialsW := nil;
      RasConnectionNotificationA := nil;
      RasConnectionNotificationW := nil;
      RasGetSubEntryPropertiesA := nil;
      RasGetSubEntryPropertiesW := nil;
      RasSetSubEntryPropertiesA := nil;
      RasSetSubEntryPropertiesW := nil;
      RasGetAutodialAddressA := nil;
      RasGetAutodialAddressW := nil;
      RasSetAutodialAddressA := nil;
      RasSetAutodialAddressW := nil;
      RasEnumAutodialAddressesA := nil;
      RasEnumAutodialAddressesW := nil;
      RasGetAutodialEnableA := nil;
      RasGetAutodialEnableW := nil;
      RasSetAutodialEnableA := nil;
      RasSetAutodialEnableW := nil;
      RasGetAutodialParamA := nil;
      RasGetAutodialParamW := nil;
      RasSetAutodialParamA := nil;
      RasSetAutodialParamW := nil;
      RasGetSubEntryHandle := nil;
      RasGetCredentials := nil;
      RasSetCredentials := nil;
      RasConnectionNotification := nil;
      RasGetSubEntryProperties := nil;
      RasSetSubEntryProperties := nil;
      RasGetAutodialAddress := nil;
      RasSetAutodialAddress := nil;
      RasEnumAutodialAddresses := nil;
      RasGetAutodialEnable := nil;
      RasSetAutodialEnable := nil;
      RasGetAutodialParam := nil;
      RasSetAutodialParam := nil;
    end;
  finally
    Lock.Leave;
  end;
end;

initialization
  Lock := TCriticalSection.Create;
finalization
  while ReferenceCount > 0 do
    FreeRasLibrary;
  Lock.Free;
end.
