//////////////////////////////////////////////////////////////////////////////////
//     This source code is provided 'as-is', without any express or implied     //
//     warranty. In no event will Infintuary be held liable for any damages     //
//     arising from the use of this software.                                   //
//                                                                              //
//     Infintuary does not warrant, that the source code will be free from      //
//     defects in design or workmanship or that operation of the source code    //         
//     will be error-free. No implied or statutory warranty of merchantability  //
//     or fitness for a particular purpose shall apply. The entire risk of      //
//     quality and performance is with the user of this source code.            //
//                                                                              //
//     Permission is granted to anyone to use this software for any purpose,    //
//     including commercial applications, and to alter it and redistribute it   //
//     freely, subject to the following restrictions:                           //
//                                                                              //
//     1. The origin of this source code must not be misrepresented; you must   //
//        not claim that you wrote the original source code.                    //
//                                                                              //
//     2. Altered source versions must be plainly marked as such, and must not  //
//        be misrepresented as being the original source code.                  //
//                                                                              //
//     3. This notice may not be removed or altered from any source             //
//        distribution.                                                         //
//////////////////////////////////////////////////////////////////////////////////

unit MD_PCSCDef;
{$IFDEF FPC}                              
{$MODE delphi}
{$ELSE}
{$DEFINE WINDOWS}
{$ENDIF}

interface

type
  TPcscProtocol = (prNC, prT0, prT1, prRaw, prDirect);
  TPcscProtocols = set of TPcscProtocol;
  TPcscCardInState = (stDirect, stExclusive, stShared, stAvailable, stNoCard, stBadCard, stUnknown, stGeneral);

  (* **************************************************************************** *)
  TSCardContext = THandle;
  TSCardHandle = THandle;

  TSCardReaderStateA = record
    szReader: PChar;
    pvUserData: Pointer;
    dwCurrentState, dwEventState, cbAtr: Cardinal;
    rgbAtr: array [0 .. 36 - 1] of Byte;
  end;
  PSCardReaderStateA = ^TSCardReaderStateA;

  TRSArray = array [0 .. $FFFF] of TSCardReaderStateA;
  PRSArray = ^TRSArray;

  TSCardReaderStateW = record
    szReader: PWideChar;
    pvUserData: Pointer;
    dwCurrentState, dwEventState, cbAtr: Cardinal;
    rgbAtr: array [0 .. 36 - 1] of Byte;
  end;
  PSCardReaderStateW = ^TSCardReaderStateW;

  TSCardIoRequest = packed record
    Protocol, Size: Cardinal;
  end;
  PSCardIoRequest = ^TSCardIoRequest;

const
  FILE_DEVICE_SMARTCARD = $00000031;
  FILE_DEVICE_UNKNOWN = $00000022;
  METHOD_BUFFERED = 0;
  METHOD_IN_DIRECT = 1;
  METHOD_OUT_DIRECT = 2;
  METHOD_NEITHER = 3;
  FILE_ANY_ACCESS = 0;

  SCARD_AUTOALLOCATE = -1;

  SC_DLG_MINIMAL_UI = $01;
  SC_DLG_NO_UI = $02;
  SC_DLG_FORCE_UI = $04;

  SCARD_SCOPE_USER = 0;
  SCARD_SCOPE_TERMINAL = 1;
  SCARD_SCOPE_SYSTEM = 2;

  SCARD_SHARE_EXCLUSIVE = 1;
  SCARD_SHARE_SHARED = 2;
  SCARD_SHARE_DIRECT = 3;

  SCARD_LEAVE_CARD = 0;
  SCARD_RESET_CARD = 1;
  SCARD_UNPOWER_CARD = 2;
  SCARD_EJECT_CARD = 3;

  SCARD_POWER_DOWN = 0;
  SCARD_COLD_RESET = 1;
  SCARD_WARM_RESET = 2;

  // Driver attribute flags
  SCARD_READER_SWALLOWS = $00000001; // Reader has a card swallowing
  SCARD_READER_EJECTS = $00000002; // Reader has a card ejection
  SCARD_READER_CONFISCATES = $00000004; // Reader has a card capture

  // protocol flag definitions
  SCARD_PROTOCOL_UNDEFINED = $00000000; // There is no active protocol.
  SCARD_PROTOCOL_T0 = $00000001; // T=0 is the active protocol.
  SCARD_PROTOCOL_T1 = $00000002; // T=1 is the active protocol.
  SCARD_PROTOCOL_RAW = $00010000; // Raw is the active protocol.
  SCARD_PROTOCOL_DEFAULT = $80000000; // Use implicit PTS.
  SCARD_PROTOCOL_OPTIMAL = $00000000; // Use PTS to optimize protocol.
  SCARD_PROTOCOL_Tx = (SCARD_PROTOCOL_T0 or SCARD_PROTOCOL_T1);
  SCARD_PROTOCOL_ALL = SCARD_PROTOCOL_Tx or SCARD_PROTOCOL_RAW;

  // Reader states
  SCARD_UNKNOWN = 0; // This value implies the driver is unaware
  // of the current state of the reader.
  SCARD_ABSENT = 1; // This value implies there is no card in
  // the reader.
  SCARD_PRESENT = 2; // This value implies there is a card is
  // present in the reader, but that it has
  // not been moved into position for use.
  SCARD_SWALLOWED = 3; // This value implies there is a card in the
  // reader in position for use.  The card is
  // not powered.
  SCARD_POWERED = 4; // This value implies there is power is
  // being provided to the card, but the
  // Reader Driver is unaware of the mode of
  // the card.
  SCARD_NEGOTIABLE = 5; // This value implies the card has been
  // reset and is awaiting PTS negotiation.
  SCARD_SPECIFIC = 6; // This value implies the card has been
  // reset and specific communication
  // protocols have been established.

  SCARD_CLASS_VENDOR_INFO = $00010000; // Vendor information definitions
  SCARD_CLASS_COMMUNICATIONS = $00020000; // Communication definitions
  SCARD_CLASS_PROTOCOL = $00030000; // Protocol definitions
  SCARD_CLASS_POWER_MGMT = $00040000; // Power Management definitions
  SCARD_CLASS_SECURITY = $00050000; // Security Assurance definitions
  SCARD_CLASS_MECHANICAL = $00060000; // Mechanical characteristic defs
  SCARD_CLASS_VENDOR_DEFINED = $00070000; // Vendor specific definitions
  SCARD_CLASS_IFD_PROTOCOL = $00080000; // Interface Device Protocol options
  SCARD_CLASS_ICC_STATE = $00090000; // ICC State specific definitions
  SCARD_CLASS_SYSTEM = $7FFF0000; // System-specific definitions

  SCARD_ATTR_VENDOR_NAME = SCARD_CLASS_VENDOR_INFO + $0100;
  SCARD_ATTR_VENDOR_IFD_TYPE = SCARD_CLASS_VENDOR_INFO + $0101;
  SCARD_ATTR_VENDOR_IFD_VERSION = SCARD_CLASS_VENDOR_INFO + $0102;
  SCARD_ATTR_VENDOR_IFD_SERIAL_NO = SCARD_CLASS_VENDOR_INFO + $0103;
  SCARD_ATTR_CHANNEL_ID = SCARD_CLASS_COMMUNICATIONS + $0110;

  SCARD_ATTR_PROTOCOL_TYPES = SCARD_CLASS_PROTOCOL + $0120;
  // SCARD_ATTR_ASYNC_PROTOCOL_TYPES     = SCARD_CLASS_PROTOCOL + $0120;
  // seems Microsoft changed this....
  SCARD_ATTR_DEFAULT_CLK = SCARD_CLASS_PROTOCOL + $0121;
  SCARD_ATTR_MAX_CLK = SCARD_CLASS_PROTOCOL + $0122;
  SCARD_ATTR_DEFAULT_DATA_RATE = SCARD_CLASS_PROTOCOL + $0123;
  SCARD_ATTR_MAX_DATA_RATE = SCARD_CLASS_PROTOCOL + $0124;
  SCARD_ATTR_MAX_IFSD = SCARD_CLASS_PROTOCOL + $0125;
  SCARD_ATTR_SYNC_PROTOCOL_TYPES = SCARD_CLASS_PROTOCOL + $0126;
  SCARD_ATTR_POWER_MGMT_SUPPORT = SCARD_CLASS_POWER_MGMT + $0131;
  SCARD_ATTR_USER_TO_CARD_AUTH_DEVICE = SCARD_CLASS_SECURITY + $0140;
  SCARD_ATTR_USER_AUTH_INPUT_DEVICE = SCARD_CLASS_SECURITY + $0142;
  SCARD_ATTR_CHARACTERISTICS = SCARD_CLASS_MECHANICAL + $0150;

  SCARD_ATTR_CURRENT_PROTOCOL_TYPE = SCARD_CLASS_IFD_PROTOCOL + $0201;
  SCARD_ATTR_CURRENT_CLK = SCARD_CLASS_IFD_PROTOCOL + $0202;
  SCARD_ATTR_CURRENT_F = SCARD_CLASS_IFD_PROTOCOL + $0203;
  SCARD_ATTR_CURRENT_D = SCARD_CLASS_IFD_PROTOCOL + $0204;
  SCARD_ATTR_CURRENT_N = SCARD_CLASS_IFD_PROTOCOL + $0205;
  SCARD_ATTR_CURRENT_W = SCARD_CLASS_IFD_PROTOCOL + $0206;
  SCARD_ATTR_CURRENT_IFSC = SCARD_CLASS_IFD_PROTOCOL + $0207;
  SCARD_ATTR_CURRENT_IFSD = SCARD_CLASS_IFD_PROTOCOL + $0208;
  SCARD_ATTR_CURRENT_BWT = SCARD_CLASS_IFD_PROTOCOL + $0209;
  SCARD_ATTR_CURRENT_CWT = SCARD_CLASS_IFD_PROTOCOL + $020A;
  SCARD_ATTR_CURRENT_EBC_ENCODING = SCARD_CLASS_IFD_PROTOCOL + $020B;
  SCARD_ATTR_EXTENDED_BWT = SCARD_CLASS_IFD_PROTOCOL + $020C;

  SCARD_ATTR_ICC_PRESENCE = SCARD_CLASS_ICC_STATE + $0300;
  SCARD_ATTR_ICC_INTERFACE_STATUS = SCARD_CLASS_ICC_STATE + $0301;
  SCARD_ATTR_CURRENT_IO_STATE = SCARD_CLASS_ICC_STATE + $0302;
  SCARD_ATTR_ATR_STRING = SCARD_CLASS_ICC_STATE + $0303;
  SCARD_ATTR_ICC_TYPE_PER_ATR = SCARD_CLASS_ICC_STATE + $0304;

  SCARD_ATTR_ESC_RESET = SCARD_CLASS_VENDOR_DEFINED + $A000;
  SCARD_ATTR_ESC_CANCEL = SCARD_CLASS_VENDOR_DEFINED + $A003;
  SCARD_ATTR_ESC_AUTHREQUEST = SCARD_CLASS_VENDOR_DEFINED + $A005;
  SCARD_ATTR_MAXINPUT = SCARD_CLASS_VENDOR_DEFINED + $A007;

  SCARD_ATTR_DEVICE_UNIT = SCARD_CLASS_SYSTEM + $0001;
  SCARD_ATTR_DEVICE_IN_USE = SCARD_CLASS_SYSTEM + $0002;
  SCARD_ATTR_DEVICE_FRIENDLY_NAME_A = SCARD_CLASS_SYSTEM + $0003;
  SCARD_ATTR_DEVICE_SYSTEM_NAME_A = SCARD_CLASS_SYSTEM + $0004;
  SCARD_ATTR_DEVICE_FRIENDLY_NAME_W = SCARD_CLASS_SYSTEM + $0005;
  SCARD_ATTR_DEVICE_SYSTEM_NAME_W = SCARD_CLASS_SYSTEM + $0006;
  SCARD_ATTR_SUPRESS_T1_IFS_REQUEST = SCARD_CLASS_SYSTEM + $0007;

  SCARD_S_SUCCESS = $00000000;

  SCARD_F_COMM_ERROR = $80100013;
  SCARD_F_INTERNAL_ERROR = $80100001;
  SCARD_F_UNKNOWN_ERROR = $80100014;
  SCARD_F_WAITED_TOO_LONG = $80100007;

  SCARD_E_CANCELLED = $80100002;
  SCARD_E_INSUFFICIENT_BUFFER = $80100008;
  SCARD_E_INVALID_HANDLE = $80100003;
  SCARD_E_INVALID_PARAMETER = $80100004;
  SCARD_E_INVALID_TARGET = $80100005;
  SCARD_E_NO_MEMORY = $80100006;
  SCARD_E_UNKNOWN_READER = $80100009;
  SCARD_E_TIMEOUT = $8010000A;
  SCARD_E_SHARING_VIOLATION = $8010000B;
  SCARD_E_NO_SMARTCARD = $8010000C;
  SCARD_E_UNKNOWN_CARD = $8010000D;
  SCARD_E_CANT_DISPOSE = $8010000E;
  SCARD_E_PROTO_MISMATCH = $8010000F;
  SCARD_E_NOT_READY = $80100010;
  SCARD_E_INVALID_VALUE = $80100011;
  SCARD_E_SYSTEM_CANCELLED = $80100012;
  SCARD_E_INVALID_ATR = $80100015;
  SCARD_E_NOT_TRANSACTED = $80100016;
  SCARD_E_READER_UNAVAILABLE = $80100017;
  SCARD_P_SHUTDOWN = $80100018;
  SCARD_E_PCI_TOO_SMALL = $80100019;
  SCARD_E_READER_UNSUPPORTED = $8010001A;
  SCARD_E_DUPLICATE_READER = $8010001B;
  SCARD_E_CARD_UNSUPPORTED = $8010001C;
  SCARD_E_NO_SERVICE = $8010001D;
  SCARD_E_SERVICE_STOPPED = $8010001E;
  SCARD_E_UNEXPECTED = $8010001F;
  SCARD_E_ICC_INSTALLATION = $80100020;
  SCARD_E_ICC_CREATEORDER = $80100021;
  SCARD_E_UNSUPPORTED_FEATURE = $80100022;
  SCARD_E_DIR_NOT_FOUND = $80100023;
  SCARD_E_FILE_NOT_FOUND = $80100024;
  SCARD_E_NO_DIR = $80100025;
  SCARD_E_NO_FILE = $80100026;
  SCARD_E_NO_ACCESS = $80100027;
  SCARD_E_WRITE_TOO_MANY = $80100028;
  SCARD_E_BAD_SEEK = $80100029;
  SCARD_E_INVALID_CHV = $8010002A;
  SCARD_E_UNKNOWN_RES_MNG = $8010002B;
  SCARD_E_NO_SUCH_CERTIFICATE = $8010002C;
  SCARD_E_CERTIFICATE_UNAVAILABLE = $8010002D;
  SCARD_E_NO_READERS_AVAILABLE = $8010002E;
  SCARD_E_COMM_DATA_LOST = $8010002F;
  SCARD_E_NO_KEY_CONTAINER = $80100030;
  SCARD_E_SERVER_TOO_BUSY = $80100031;
  SCARD_W_UNSUPPORTED_CARD = $80100065;
  SCARD_W_UNRESPONSIVE_CARD = $80100066;
  SCARD_W_UNPOWERED_CARD = $80100067;
  SCARD_W_RESET_CARD = $80100068;
  SCARD_W_REMOVED_CARD = $80100069;
  SCARD_W_SECURITY_VIOLATION = $8010006A;
  SCARD_W_WRONG_CHV = $8010006B;
  SCARD_W_CHV_BLOCKED = $8010006C;
  SCARD_W_EOF = $8010006D;
  SCARD_W_CANCELLED_BY_USER = $8010006E;
  SCARD_W_CARD_NOT_AUTHENTICATED = $8010006F;

  SCARD_STATE_UNAWARE = $00000000;
  SCARD_STATE_IGNORE = $00000001;
  SCARD_STATE_CHANGED = $00000002;
  SCARD_STATE_UNKNOWN = $00000004;
  SCARD_STATE_UNAVAILABLE = $00000008;
  SCARD_STATE_EMPTY = $00000010;
  SCARD_STATE_PRESENT = $00000020;
  SCARD_STATE_ATRMATCH = $00000040;
  SCARD_STATE_EXCLUSIVE = $00000080;
  SCARD_STATE_INUSE = $00000100;
  SCARD_STATE_MUTE = $00000200;
  SCARD_STATE_UNPOWERED = $00000400;

  // Type of reader
  SCARD_READER_TYPE_SERIAL = $01;
  SCARD_READER_TYPE_PARALELL = $02;
  SCARD_READER_TYPE_KEYBOARD = $04;
  SCARD_READER_TYPE_SCSI = $08;
  SCARD_READER_TYPE_IDE = $10;
  SCARD_READER_TYPE_USB = $20;
  SCARD_READER_TYPE_PCMCIA = $40;
  SCARD_READER_TYPE_VENDOR = $F0;

  IOCTL_SMARTCARD_POWER = (FILE_DEVICE_SMARTCARD shl 16) + (01 shl 2);
  IOCTL_SMARTCARD_GET_ATTRIBUTE = (FILE_DEVICE_SMARTCARD shl 16) + (02 shl 2);
  IOCTL_SMARTCARD_SET_ATTRIBUTE = (FILE_DEVICE_SMARTCARD shl 16) + (03 shl 2);
  IOCTL_SMARTCARD_CONFISCATE = (FILE_DEVICE_SMARTCARD shl 16) + (04 shl 2);
  IOCTL_SMARTCARD_TRANSMIT = (FILE_DEVICE_SMARTCARD shl 16) + (05 shl 2);
  IOCTL_SMARTCARD_EJECT = (FILE_DEVICE_SMARTCARD shl 16) + (06 shl 2);
  IOCTL_SMARTCARD_SWALLOW = (FILE_DEVICE_SMARTCARD shl 16) + (07 shl 2);
  // IOCTL_SMARTCARD_READ        = (FILE_DEVICE_SMARTCARD shl 16)+(08 shl 2);
  // IOCTL_SMARTCARD_WRITE       = (FILE_DEVICE_SMARTCARD shl 16)+(09 shl 2);
  IOCTL_SMARTCARD_IS_PRESENT = (FILE_DEVICE_SMARTCARD shl 16) + (10 shl 2);
  IOCTL_SMARTCARD_IS_ABSENT = (FILE_DEVICE_SMARTCARD shl 16) + (11 shl 2);
  IOCTL_SMARTCARD_SET_PROTOCOL = (FILE_DEVICE_SMARTCARD shl 16) + (12 shl 2);
  IOCTL_SMARTCARD_GET_STATE = (FILE_DEVICE_SMARTCARD shl 16) + (14 shl 2);
  IOCTL_SMARTCARD_GET_LAST_ERROR = (FILE_DEVICE_SMARTCARD shl 16) + (15 shl 2);
  IOCTL_SMARTCARD_GET_PERF_CNTR = (FILE_DEVICE_SMARTCARD shl 16) + (16 shl 2);

  SCardPciT1: TSCardIoRequest = (Protocol: SCARD_PROTOCOL_T1;
    Size: SizeOf(TSCardIoRequest));

  SCardPciT0: TSCardIoRequest = (Protocol: SCARD_PROTOCOL_T0;
    Size: SizeOf(TSCardIoRequest));

  SCardPciRaw: TSCardIoRequest = (Protocol: SCARD_PROTOCOL_RAW;
    Size: SizeOf(TSCardIoRequest));

const
{$IFDEF WINDOWS}
  IOCTL_CCID_ESCAPE = $0DAC;
{$ELSE}
  IOCTL_CCID_ESCAPE = $1;
{$ENDIF}
  IOCTL_GET_VERSIONS = $0801;

implementation

end.
