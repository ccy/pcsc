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

unit MD_Tools;
{$IFDEF FPC}
{$MODE delphi}                                    
{$ELSE}
{$DEFINE WINDOWS}
{$ENDIF}

interface

uses
  SysUtils, Classes, MD_PCSCDef;

{$IFNDEF UNICODE}
type
  TCharSet = set of char;
{$ENDIF}

{$IFNDEF UNICODE}
function CharInSet(c: char; TheSet: TCharSet): boolean;
{$ENDIF}

function BufferToHexString(Buffer: TBytes): string;
function HexStringToBuffer(HexString: string): TBytes;

function PCSCErrorToString(ErrorCode: Cardinal): string;
function CardErrorToString(ErrorCode: Word): string;
function WindowsErrorToString(ErrorCode: Cardinal): string;

implementation

{$IFNDEF UNICODE}
function CharInSet(c: char; TheSet: TCharSet): boolean;
begin
  result := (c in TheSet);
end;
{$ENDIF}

function BufferToHexString(Buffer: TBytes): string;
var
  i: integer;
begin
  result := '';
  for i := 0 to length(Buffer) - 1 do result := result + IntToHex(Buffer[i], 2) + ' ';
  result := trim(result);
end;

function HexChrVal(HexChr: char): byte;
begin
  HexChr := UpCase(HexChr);
  case HexChr of
    '0' .. '9': result := byte(HexChr) - byte('0');
    'A' .. 'F': result := byte(HexChr) - byte('A') + 10;
  else result := $FF;
  end;
end;

function HexStringToBuffer(HexString: string): TBytes;
var
  i: integer;
  s, s2, s3: string;
const
  HexChars = ['0' .. '9', 'A' .. 'F'];
begin
  s3 := '';
  s := UpperCase(HexString);
  for i := 1 to length(s) do
    if CharInSet(s[i], HexChars) then s3 := s3 + s[i];
  s := s3;
  if length(s) mod 2 <> 0 then s := '0' + s;
  SetLength(result, (length(s) div 2));
  for i := 0 to length(result) - 1 do begin
    s2 := copy(s, 2 * i + 1, 2);
    result[i] := (HexChrVal(s2[1]) * 16) + HexChrVal(s2[2]);
  end;
end;

function PCSCErrorToString(ErrorCode: Cardinal): string;
begin
  case ErrorCode of
    SCARD_S_SUCCESS: result := 'OK';
    SCARD_F_COMM_ERROR: result := 'An internal communications error has been detected.';
    SCARD_F_INTERNAL_ERROR: result := 'An internal consistency check failed.'; // 80100001;
    SCARD_F_UNKNOWN_ERROR: result := 'An internal error has been detected, but the source is unknown.'; // 80100014;
    SCARD_F_WAITED_TOO_LONG: result := 'An internal consistency timer has expired.'; // 80100007;
    SCARD_E_CANCELLED: result := 'The action was cancelled by an SCardCancel request.'; // 80100002;
    SCARD_E_INSUFFICIENT_BUFFER: result := 'The data buffer to receive returned data is too small for the returned data.'; // 80100008;
    SCARD_E_INVALID_HANDLE: result := 'The supplied handle was invalid.'; // 80100003;
    SCARD_E_INVALID_PARAMETER: result := 'One or more of the supplied parameters could not be properly interpreted.'; // 80100004;
    SCARD_E_INVALID_TARGET: result := 'Registry startup information is missing or invalid.'; // 80100005;
    SCARD_E_NO_MEMORY: result := 'Not enough memory available to complete this command.'; // 80100006;
    SCARD_E_UNKNOWN_READER: result := 'The specified reader name is not recognized.'; // 80100009;
    SCARD_E_TIMEOUT: result := 'The user-specified timeout value has expired.'; // 8010000A;
    SCARD_E_SHARING_VIOLATION: result := 'The smart card cannot be accessed because of other connections outstanding.'; // 8010000B;
    SCARD_E_NO_SMARTCARD: result := 'The operation requires a Smart Card, but no Smart Card is currently in the device.'; // 8010000C;
    SCARD_E_UNKNOWN_CARD: result := 'The specified smart card name is not recognized.'; // 8010000D;
    SCARD_E_CANT_DISPOSE: result := 'The system could not dispose of the media in the requested manner.'; // 8010000E;
    SCARD_E_PROTO_MISMATCH: result := 'The requested protocols are incompatible with the protocol currently in use with the smart card.'; // 8010000F;
    SCARD_E_NOT_READY: result := 'The reader or smart card is not ready to accept commands.'; // 80100010;
    SCARD_E_INVALID_VALUE: result := 'One or more of the supplied parameters values could not be properly interpreted.'; // 80100011;
    SCARD_E_SYSTEM_CANCELLED: result := 'The action was cancelled by the system, presumably to log off or shut down.'; // 80100012;
    SCARD_E_INVALID_ATR: result := 'An ATR obtained from the registry is not a valid ATR string.'; // 80100015;
    SCARD_E_NOT_TRANSACTED: result := 'An attempt was made to end a non-existent transaction.'; // 80100016;
    SCARD_E_READER_UNAVAILABLE: result := 'The specified reader is not currently available for use.'; // 80100017;
    SCARD_P_SHUTDOWN: result := 'The operation has been aborted to allow the server application to exit.'; // 80100018;
    SCARD_E_PCI_TOO_SMALL: result := 'The PCI Receive buffer was too small.'; // 80100019;
    SCARD_E_READER_UNSUPPORTED: result := 'The reader driver does not meet minimal requirements for support.'; // 8010001A;
    SCARD_E_DUPLICATE_READER: result := 'The reader driver did not produce a unique reader name.'; // 8010001B;
    SCARD_E_CARD_UNSUPPORTED: result := 'The smart card does not meet minimal requirements for support.'; // 8010001C;
    SCARD_E_NO_SERVICE: result := 'The Smart card resource manager is not running.'; // 8010001D;
    SCARD_E_SERVICE_STOPPED: result := 'The Smart card resource manager has shut down.'; // 8010001E;
    SCARD_E_UNEXPECTED: result := 'An unexpected card error has occurred.'; // 8010001F;
    SCARD_E_ICC_INSTALLATION: result := 'No Primary Provider can be found for the smart card.'; // 80100020;
    SCARD_E_ICC_CREATEORDER: result := 'The requested order of object creation is not supported.'; // 80100021;
    SCARD_E_UNSUPPORTED_FEATURE: result := 'This smart card does not support the requested feature.'; // 80100022;
    SCARD_E_DIR_NOT_FOUND: result := 'The identified directory does not exist in the smart card.'; // 80100023;
    SCARD_E_FILE_NOT_FOUND: result := 'The identified file does not exist in the smart card.'; // 80100024;
    SCARD_E_NO_DIR: result := 'The supplied path does not represent a smart card directory.'; // 80100025;
    SCARD_E_NO_FILE: result := 'The supplied path does not represent a smart card file.'; // 80100026;
    SCARD_E_NO_ACCESS: result := 'Access is denied to this file.'; // 80100027;
    SCARD_E_WRITE_TOO_MANY: result := 'The smartcard does not have enough memory to store the information.'; // 80100028;
    SCARD_E_BAD_SEEK: result := 'There was an error trying to set the smart card file object pointer.'; // 80100029;
    SCARD_E_INVALID_CHV: result := 'The supplied PIN is incorrect.'; // 8010002A;
    SCARD_E_UNKNOWN_RES_MNG: result := 'An unrecognized error code was returned from a layered component.'; // 8010002B;
    SCARD_E_NO_SUCH_CERTIFICATE: result := 'The requested certificate does not exist.'; // 8010002C;
    SCARD_E_CERTIFICATE_UNAVAILABLE: result := 'The requested certificate could not be obtained.'; // 8010002D;
    SCARD_E_NO_READERS_AVAILABLE: result := 'Cannot find a smart card reader.'; // 8010002E;
    SCARD_E_COMM_DATA_LOST: result := 'A communications error with the smart card has been detected. Retry the operation.'; // 8010002F;
    SCARD_E_NO_KEY_CONTAINER: result := 'The requested key container does not exist on the smart card.'; // 80100030;
    SCARD_E_SERVER_TOO_BUSY: result := 'The Smart card resource manager is too busy to complete this operation.'; // 80100031;
    SCARD_W_UNSUPPORTED_CARD: result := 'The reader cannot communicate with the card, due to ATR string configuration conflicts.'; // 80100065;
    SCARD_W_UNRESPONSIVE_CARD: result := 'The smart card is not responding to a reset.'; // 80100066;
    SCARD_W_UNPOWERED_CARD: result := 'Power has been removed from the smart card, so that further communication is not possible.'; // 80100067;
    SCARD_W_RESET_CARD: result := 'The smart card has been reset, so any shared state information is invalid.'; // 80100068;
    SCARD_W_REMOVED_CARD: result := 'The smart card has been removed, so further communication is not possible.'; // 80100069;
    SCARD_W_SECURITY_VIOLATION: result := 'Access was denied because of a security violation.'; // 8010006A;
    SCARD_W_WRONG_CHV: result := 'The card cannot be accessed because the wrong PIN was presented.'; // 8010006B;
    SCARD_W_CHV_BLOCKED: result := 'The card cannot be accessed because the maximum number of PIN entry attempts has been reached.'; // 8010006C;
    SCARD_W_EOF: result := 'The end of the smart card file has been reached.'; // 8010006D;
    SCARD_W_CANCELLED_BY_USER: result := 'The action was canceled by the user.'; // 8010006E;
    SCARD_W_CARD_NOT_AUTHENTICATED: result := 'No PIN was presented to the Smart card.'; // 8010006F;
  else result := 'Unknown error code.';
  end;
end;

function CardErrorToString(ErrorCode: Word): string;
begin
  case ErrorCode of
    $9000: result := 'OK';
    $9100: result := 'OK';
    $6200: result := 'State of non-volatile memory unchanged';
    $6281: result := 'State of non-volatile memory unchanged. Part of returned data may be corrupted.';
    $6282: result := 'State of non-volatile memory unchanged. End of file/record reached before reading Le bytes.';
    $6283: result := 'State of non-volatile memory unchanged. Selected file invalidated.';
    $6284: result := 'State of non-volatile memory unchanged. FCI not formatted.';

    $6300: result := 'State of non-volatile memory changed.';
    $6381: result := 'State of non-volatile memory changed. File filled up by the last write.';

    $6400: result := 'State of non-volatile memory unchanged.';
    $6500: result := 'State of non-volatile memory changed.';
    $6581: result := 'State of non-volatile memory changed. Memory Failure.';

    $6700: result := 'Wrong length.';
    $6800: result := 'Functions in CLA not supported.';
    $6881: result := 'Functions in CLA not supported. Logical channel not supported.';
    $6882: result := 'Functions in CLA not supported. Secure messaging not supported.';

    $6900: result := 'Command not allowed. ';
    $6981: result := 'Command not allowed. Command incompatible with file structure.';
    $6982: result := 'Command not allowed. Security status not satisfied.';
    $6983: result := 'Command not allowed. Authentication method blocked.';
    $6984: result := 'Command not allowed. Referenced data invalidated.';
    $6985: result := 'Command not allowed. Conditions of use not satisfied.';
    $6986: result := 'Command not allowed. Command not allowed (no current EF).';
    $6987: result := 'Command not allowed. Expected SM data objects missing.';
    $6988: result := 'Command not allowed. SM data objects incorrect.';

    $6A00: result := 'Wrong parameters P1 or P2.';
    $6A80: result := 'Wrong parameter. Incorrect parameters in the data field.';
    $6A81: result := 'Wrong parameter. Function not supported.';
    $6A82: result := 'Wrong parameter. File not found.';
    $6A83: result := 'Wrong parameter. Record not found.';
    $6A84: result := 'Wrong parameter. Not enough memory space in the file.';
    $6A85: result := 'Wrong parameter. Lc inconsistent with TLV structure.';
    $6A86: result := 'Wrong parameters P1 or P2.';
    $6A87: result := 'Wrong parameter. Lc inconsistent with P1-P2.';
    $6A88: result := 'Wrong parameter. Referenced data not found.';

    $6B00: result := 'Wrong parameters P1 or P2.';
    $6D00: result := 'Instruction code not supported or invalid.';
    $6E00: result := 'Class byte not supported.';
    $6F00: result := 'Unknown error.';
  else begin
      case (ErrorCode shr 8) and $FF of
        $61: result := IntToStr(ErrorCode and $FF) + ' response bytes are still available.';
        $63:
          begin
            result := 'State of non-volatile memory changed.';
            if (ErrorCode and $F0) = $C0 then result := result + ' Counter = ' + IntToStr(ErrorCode and $0F);
          end;
        $64: result := 'State of non-volatile memory unchanged.';
        $6C: result := 'Wrong length Le: ' + IntToStr((ErrorCode and $FF)) + ' expected.';
        $90: result := 'OK. ' + IntToStr((ErrorCode and $FF)) + ' response byte(s) available';
        $91: result := 'DESFire specific error code: ' { + DESFireStatusToString(ErrorCode and $FF) };
        $9F: result := 'OK. ' + IntToStr((ErrorCode and $FF)) + ' response byte(s) available';
      else result := 'Unknown error code.';
      end;
    end;
  end;
end;

function WindowsErrorToString(ErrorCode: Cardinal): string;
begin
  result := trim(SysErrorMessage(ErrorCode));
  if result = '' then result := 'Unknown.'
  else if result[length(result)] <> '.' then result := result + '.';
end;

end.
