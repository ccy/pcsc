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

unit MD_PCSC;
{$IFDEF FPC}
{$MODE delphi}
{$ELSE}
{$DEFINE WINDOWS}
{$ENDIF}

interface

uses
{$IFDEF WINDOWS}Windows, {$ENDIF}
{$IFDEF FPC} LCLType,{$ENDIF}
{$IFDEF LINUX}Forms, {$ENDIF}
  SysUtils, Classes,
  MD_PCSCRaw, MD_PCSCDef, MD_Events, MD_Tools;

type
  TPCSC = class;
  TPCSCReader = class;

  TOnReaderListChanged = procedure of object;
  TOnTimer = procedure of object;
  TOnTerminated = procedure of object;
  TOnReaderEvent = procedure(Sender: TObject; ReaderName: string) of object;
  TOnCardInsertEvent = procedure(Sender: TObject; ReaderName: string; ATR: TBytes) of object;
  TOnPCSCReaderEvent = procedure(PCSCReader: TPCSCReader) of object;

  TCardState = (csUnknown = 0, csExclusive, csShared, csAvailable, csBadCard, csNoCard);

  TReaderListThread = class(TThread)
  private
    FPCSCRaw: TPCSCRaw;
    FPCSCDeviceContext: THandle;
{$IFDEF WINDOWS}
    FReaderState: TSCardReaderStateA;
{$ELSE}
    FLastReaderSize: Cardinal;
    FOnTerminated: TOnTerminated;
{$ENDIF}
    FOnTimer: TOnTimer;
    FOnReaderListChanged: TOnReaderListChanged;
  public
    constructor Create(PCSCRaw: TPCSCRaw);
    procedure Execute; override;

    property OnReaderListChanged: TOnReaderListChanged read FOnReaderListChanged write FOnReaderListChanged;
    property OnTimer: TOnTimer read FOnTimer write FOnTimer;
{$IFDEF UNIX}
    property OnTerminated: TOnTerminated read FOnTerminated write FOnTerminated;
{$ENDIF}
  end;

  TPCSCReader = class
  private
    FOnCardStateChanged: TOnPCSCReaderEvent;
    FPCSCRaw: TPCSCRaw;

    FReaderName: string;
    FCardState: TCardState;

    FCardHandle: THandle;
    FProtocolType: TPcscProtocol;
    FPCSCDeviceContext: THandle;
    FVendorName: string;
    FDisplayName: string;

    FVendorIFDVersion: Cardinal;
    FIFDMajor, FIFDMinor: integer;
    FDrvMajor, FDrvMinor: integer;
    FSerial: string;

    FValidCard: boolean;
    FATR: TBytes;
    FDirectMode: boolean;

    function ReadSCardAttrDW(AttrCode: Cardinal; out Attrib: Cardinal): boolean;
    function ReadSCardAttrS(AttrCode: Cardinal; out Attrib: string): boolean;
    procedure CollectReaderData;
    function GetVendorName: boolean;
    function GetDisplayName: boolean;
    function GetDriverVersion: boolean;
    function GetFirmwareVersion: boolean;
    function GetDeviceSerial: boolean;
    function GetDeviceSerialStd: boolean;
{$IFDEF UNIX}
    function GetDeviceSerialLinux: boolean;
{$ENDIF}
    procedure ClearCardInfo;

    function GetAtrAsString: string;
    function SCSendAPDU(InBuffer: Pointer; InSize: Cardinal; OutBuffer: Pointer; var OutSize: Cardinal; out SW12: Word): Cardinal;
    function SCIOCTL(IOCtlCode: Cardinal; InBuffer: Pointer; InSize: Cardinal; OutBuffer: Pointer; var OutSize: Cardinal): Cardinal;
  public
    constructor Create(AReaderName: string; PCSCRaw: TPCSCRaw);
    destructor Destroy; override;

    function Connect(ShareMode: Cardinal = SCARD_SHARE_SHARED): Cardinal;
    function Disconnect(Disposition: Cardinal = SCARD_UNPOWER_CARD): Cardinal;
    function ResetCard(WarmReset: boolean): Cardinal;

    function BeginTransaction: Cardinal;
    function EndTransaction: Cardinal;

    function TransmitSW(DataIn: TBytes; out DataOut: TBytes; out SW12: Word): Cardinal;
    function Transmit(DataIn: TBytes; out DataOut: TBytes): Cardinal;
    function TransmitAsString(DataIn: string; out DataOut: TBytes): Cardinal;
    function TransmitAsStringSW(DataIn: string; out DataOut: TBytes; out SW12: Word): Cardinal;
    function IOCTL(IOCtlCode: Cardinal; DataIn: TBytes; out DataOut: TBytes): Cardinal;
    function IOCTLString(IOCtlCode: Cardinal; DataIn: string; out DataOut: TBytes): Cardinal;

    procedure CheckCardState;

    property ReaderName: string read FReaderName;
    property CardState: TCardState read FCardState;
    property ATR: TBytes read FATR;
    property ATRasString: string read GetAtrAsString;
    property Protocol: TPcscProtocol read FProtocolType;
    property CardHandle: THandle read FCardHandle;

    property OnCardStateChanged: TOnPCSCReaderEvent read FOnCardStateChanged write FOnCardStateChanged;

    property VendorName: string read FVendorName;
    property DisplayName: string read FDisplayName;
    property DrvMajor: integer read FDrvMajor;
    property DrvMinor: integer read FDrvMinor;
    property FirmwareMajor: integer read FIFDMajor;
    property FirmwareMinor: integer read FIFDMinor;
  end;

  TPCSC = class
  private
    FOnReaderFoundAsync: TOnReaderEvent;
    FOnReaderRemovedAsync: TOnReaderEvent;
    FOnCardInsertedAsync: TOnCardInsertEvent;
    FOnCardRemovedAsync: TOnReaderEvent;
    FOnCardErrorAsync: TOnReaderEvent;
    FOnReaderListChanged: TNotifyEvent;
    FOnCardStateChanged: TOnReaderEvent;

    FOnReaderFound: TOnReaderEvent;
    FOnReaderRemoved: TOnReaderEvent;
    FOnCardInserted: TOnCardInsertEvent;
    FOnCardRemoved: TOnReaderEvent;
    FOnCardError: TOnReaderEvent;

    FValid: boolean;
    FReaderList: TStringList;
    FPCSCDeviceContext: THandle;
    FPCSCRaw: TPCSCRaw;
    FReaderListThread: TReaderListThread;
    FMCardSupport: boolean;
    FGetReaderDetails: boolean;

    FPCSCEventList: TPCSCEventList;
{$IFDEF UNIX}
    FReaderListThreadRunning: boolean;
{$ENDIF}

    function InitPCSC: Cardinal;
    procedure CheckCardStates;
    procedure ReaderListChanged;
    procedure UpdatePCSCReaderList;
    procedure GetPCSCReaderList(ReaderList: TStringList);
    procedure CardStateChanged(PCSCReader: TPCSCReader);
{$IFDEF UNIX}
    procedure ReaderListThreadTerminated;
{$ENDIF}

    procedure CardInsertSync(ReaderName, ATR: string);
    procedure CardRemoveSync(ReaderName: string);
    procedure CardErrorSync(ReaderName: string);
    procedure ReaderFoundSync(ReaderName: string);
    procedure ReaderRemovedSync(ReaderName: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure ProcessEvent;
    function GetPCSCReader(ReaderName: string): TPCSCReader;

    property Valid: boolean read FValid;
    property ReaderList: TStringList read FReaderList;
    property MCardSupport: boolean read FMCardSupport write FMCardSupport;
    property GetReaderDetails: boolean read FGetReaderDetails write FGetReaderDetails;

    property OnReaderFound: TOnReaderEvent read FOnReaderFound write FOnReaderFound;
    property OnReaderRemoved: TOnReaderEvent read FOnReaderRemoved write FOnReaderRemoved;
    property OnCardStateChanged: TOnReaderEvent read FOnCardStateChanged write FOnCardStateChanged;
    property OnCardInserted: TOnCardInsertEvent read FOnCardInserted write FOnCardInserted;
    property OnCardRemoved: TOnReaderEvent read FOnCardRemoved write FOnCardRemoved;
    property OnCardError: TOnReaderEvent read FOnCardError write FOnCardError;
  end;

implementation

constructor TReaderListThread.Create(PCSCRaw: TPCSCRaw);
begin
  inherited Create(true);
  FPCSCRaw := PCSCRaw;
  FPCSCDeviceContext := 0;
  FOnReaderListChanged := nil;
{$IFDEF UNIX}
  FLastReaderSize := high(Cardinal);
  FOnTerminated := nil;
{$ENDIF}
  FOnTimer := nil;
  FreeOnTerminate := true;

  FPCSCRaw.SCardEstablishContext(SCARD_SCOPE_SYSTEM, nil, nil, FPCSCDeviceContext);
end;

procedure TReaderListThread.Execute;
var
  PCSCResult: Cardinal;
{$IFDEF UNIX}
  SizeReaders: Cardinal;
{$ENDIF}
begin
{$IFDEF WINDOWS}
  FReaderState.cbAtr := 0;
  FReaderState.dwEventState := SCARD_STATE_UNAWARE;
  FReaderState.dwCurrentState := SCARD_STATE_UNAWARE;
  FReaderState.szReader := '\\?PNP?\Notification';
  FReaderState.pvUserData := nil;
{$ENDIF}

  while not Terminated do begin
{$IFDEF WINDOWS}
    PCSCResult := FPCSCRaw.SCardGetStatusChange(FPCSCDeviceContext, 0, @FReaderState, 1);
    if PCSCResult = SCARD_E_CANCELLED then break;
    if PCSCResult = SCARD_S_SUCCESS then begin
      FReaderState.dwCurrentState := FReaderState.dwEventState;
      if Assigned(FOnReaderListChanged) then Synchronize(FOnReaderListChanged);
    end;
{$ELSE}
    PCSCResult := FPCSCRaw.SCardListReaders(FPCSCDeviceContext, nil, nil, SizeReaders);
    if PCSCResult = SCARD_E_CANCELLED then break;
    if (PCSCResult = SCARD_S_SUCCESS) or (PCSCResult = SCARD_E_NO_READERS_AVAILABLE) then begin
      if FLastReaderSize<>SizeReaders then begin
        FLastReaderSize:=SizeReaders;
        if Assigned(FOnReaderListChanged) then Synchronize(FOnReaderListChanged);
      end;
    end
    else if (PCSCResult = ERROR_BROKEN_PIPE) or (PCSCResult = ERROR_INVALID_HANDLE) or (PCSCResult = SCARD_E_INVALID_HANDLE) or (PCSCResult = SCARD_E_SERVICE_STOPPED) then begin
      FPCSCRaw.SCardReleaseContext(FPCSCDeviceContext);
      PCSCResult := FPCSCRaw.SCardEstablishContext(SCARD_SCOPE_SYSTEM, nil, nil, FPCSCDeviceContext);
      if PCSCResult <> SCARD_S_SUCCESS then FPCSCRaw.SCardEstablishContext(SCARD_SCOPE_USER, nil, nil, FPCSCDeviceContext);
    end;
{$ENDIF}
    if not Terminated then begin
      if Assigned(FOnTimer) then Synchronize(FOnTimer);
      Sleep(100);
    end;
  end;
  if FPCSCDeviceContext <> 0 then FPCSCRaw.SCardReleaseContext(FPCSCDeviceContext);
{$IFDEF UNIX}
  if Assigned(FOnTerminated) then Synchronize(FOnTerminated);
{$ENDIF}
end;

constructor TPCSCReader.Create(AReaderName: string; PCSCRaw: TPCSCRaw);
begin
  FReaderName := AReaderName;
  FOnCardStateChanged := nil;
  FPCSCRaw := PCSCRaw;

  FCardState := csUnknown;
  FCardHandle := INVALID_HANDLE_VALUE;
  FDirectMode := false;

  FVendorName := '';
  FDisplayName := FReaderName;
  FVendorIFDVersion := 0;
  FIFDMajor := 0;
  FIFDMinor := 0;
  FDrvMajor := 0;
  FDrvMinor := 0;
  FSerial := '';

  FPCSCDeviceContext := 0;
  if FPCSCRaw.SCardEstablishContext(SCARD_SCOPE_SYSTEM, nil, nil, FPCSCDeviceContext) <> SCARD_S_SUCCESS then
      FPCSCRaw.SCardEstablishContext(SCARD_SCOPE_USER, nil, nil, FPCSCDeviceContext);

  ClearCardInfo;
end;

destructor TPCSCReader.Destroy;
begin
  if FPCSCDeviceContext <> 0 then FPCSCRaw.SCardReleaseContext(FPCSCDeviceContext);
  inherited;
end;

function TPCSCReader.Connect(ShareMode: Cardinal = SCARD_SHARE_SHARED): Cardinal;
var
  Protocol: Cardinal;
begin
  if FCardHandle <> INVALID_HANDLE_VALUE then begin
    result := SCARD_S_SUCCESS;
    exit;
  end;
  if ShareMode = SCARD_SHARE_DIRECT then result := FPCSCRaw.SCardConnect(FPCSCDeviceContext, PChar(FReaderName), SCARD_SHARE_DIRECT, 0, FCardHandle, Protocol)
  else result := FPCSCRaw.SCardConnect(FPCSCDeviceContext, PChar(FReaderName), ShareMode, SCARD_PROTOCOL_Tx, FCardHandle, Protocol);
  if result = SCARD_S_SUCCESS then begin
    FDirectMode := (ShareMode = SCARD_SHARE_DIRECT);
    if Protocol = SCARD_PROTOCOL_T0 then FProtocolType := prT0
    else if Protocol = SCARD_PROTOCOL_T1 then FProtocolType := prT1
    else if Protocol = SCARD_PROTOCOL_RAW then FProtocolType := prRaw
    else begin
      if ShareMode = SCARD_SHARE_DIRECT then FProtocolType := prRaw
      else FProtocolType := prNC;
    end;
  end
  else FCardHandle := INVALID_HANDLE_VALUE;
end;

function TPCSCReader.Disconnect(Disposition: Cardinal = SCARD_UNPOWER_CARD): Cardinal;
begin
  result := SCARD_S_SUCCESS;
  if FCardHandle = INVALID_HANDLE_VALUE then exit;
  result := FPCSCRaw.SCardDisconnect(FCardHandle, Disposition);
  FProtocolType := prNC;
  FCardHandle := INVALID_HANDLE_VALUE;
end;

function TPCSCReader.ResetCard(WarmReset: boolean): Cardinal;
var
  ReqProt, ActProt, IMode: Cardinal;
begin
  if FProtocolType = prT0 then ReqProt := SCARD_PROTOCOL_T0
  else if FProtocolType = prT1 then ReqProt := SCARD_PROTOCOL_T1
  else if FProtocolType = prRaw then ReqProt := SCARD_PROTOCOL_RAW
  else begin
    result := SCARD_E_INVALID_PARAMETER;
    exit;
  end;

  if WarmReset then IMode := SCARD_RESET_CARD
  else IMode := SCARD_UNPOWER_CARD;

  // Try to reconnect
  result := FPCSCRaw.SCardReconnect(FCardHandle, SCARD_SHARE_SHARED, ReqProt, IMode, ActProt);
  if result = SCARD_S_SUCCESS then begin
    // Actions after reconnection
    if ActProt = SCARD_PROTOCOL_T0 then FProtocolType := prT0
    else if ActProt = SCARD_PROTOCOL_T1 then FProtocolType := prT1
    else if ActProt = SCARD_PROTOCOL_RAW then FProtocolType := prRaw;
  end;
end;

function TPCSCReader.BeginTransaction: Cardinal;
begin
  result := FPCSCRaw.SCardBeginTransaction(FCardHandle);
end;

function TPCSCReader.EndTransaction: Cardinal;
begin
  result := FPCSCRaw.SCardEndTransaction(FCardHandle, SCARD_LEAVE_CARD);
end;

function TPCSCReader.TransmitSW(DataIn: TBytes; out DataOut: TBytes; out SW12: Word): Cardinal;
var
  OutBufLen: Cardinal;
begin
{$IFDEF WINDOWS}
  OutBufLen := 66000;
{$ELSE}
  OutBufLen := 264;
{$ENDIF}
  SetLength(DataOut, OutBufLen);
  result := SCSendAPDU(DataIn, length(DataIn), DataOut, OutBufLen, SW12);
  if result = SCARD_S_SUCCESS then begin
    if OutBufLen < 2 then SetLength(DataOut, 0)
    else SetLength(DataOut, OutBufLen - 2);
  end
  else SetLength(DataOut, 0);
end;

function TPCSCReader.Transmit(DataIn: TBytes; out DataOut: TBytes): Cardinal;
var
  SW12: Word;
  OutBufLen: Cardinal;
begin
{$IFDEF WINDOWS}
  OutBufLen := 66000;
{$ELSE}
  OutBufLen := 264;
{$ENDIF}
  SetLength(DataOut, OutBufLen);
  result := SCSendAPDU(DataIn, length(DataIn), DataOut, OutBufLen, SW12);
  if result = SCARD_S_SUCCESS then begin
    SetLength(DataOut, OutBufLen);
  end
  else SetLength(DataOut, 0);
end;

function TPCSCReader.TransmitAsString(DataIn: string; out DataOut: TBytes): Cardinal;
var
  InBuffer: TBytes;
begin
  InBuffer := HexStringToBuffer(DataIn);
  result := Transmit(InBuffer, DataOut);
  SetLength(InBuffer, 0);
end;

function TPCSCReader.TransmitAsStringSW(DataIn: string; out DataOut: TBytes; out SW12: Word): Cardinal;
var
  InBuffer: TBytes;
begin
  InBuffer := HexStringToBuffer(DataIn);
  result := TransmitSW(InBuffer, DataOut, SW12);
  SetLength(InBuffer, 0);
end;

function TPCSCReader.SCSendAPDU(InBuffer: Pointer; InSize: Cardinal; OutBuffer: Pointer; var OutSize: Cardinal; out SW12: Word): Cardinal;
var
  pioSendPCI, pioRecvPCI: pSCardIORequest;
begin
  SW12 := 0;
  case FProtocolType of
    prT0: begin
        pioSendPCI := @SCARDPCIT0;
        pioRecvPCI := nil;
      end;
    prT1: begin
        pioSendPCI := @SCARDPCIT1;
        pioRecvPCI := nil;
      end;
  else begin
      result := SCARD_E_INVALID_PARAMETER;
      exit;
    end;
  end;
  result := FPCSCRaw.SCardTransmit(FCardHandle, pioSendPCI, InBuffer, InSize, pioRecvPCI, OutBuffer, OutSize);
  if result = SCARD_S_SUCCESS then begin
    if OutSize >= 2 then begin
      SW12 := ((PByteArray(OutBuffer)^[OutSize - 2]) shl 8) or PByteArray(OutBuffer)^[OutSize - 1];
    end;
  end
end;

function TPCSCReader.SCIOCTL(IOCtlCode: Cardinal; InBuffer: Pointer; InSize: Cardinal; OutBuffer: Pointer; var OutSize: Cardinal): Cardinal;
var
  dwIoctlCode: Cardinal;
begin
  dwIoctlCode := FPCSCRaw.SCardCTLCode(IOCtlCode);
  result := FPCSCRaw.ScardControl(FCardHandle, dwIoctlCode, InBuffer, InSize, OutBuffer, OutSize, OutSize);
end;

function TPCSCReader.IOCTL(IOCtlCode: Cardinal; DataIn: TBytes; out DataOut: TBytes): Cardinal;
var
  OutBufLen: Cardinal;
begin
  OutBufLen := 264;
  SetLength(DataOut, OutBufLen);

  result := SCIOCTL(IOCtlCode, DataIn, length(DataIn), DataOut, OutBufLen);
  if result = SCARD_S_SUCCESS then SetLength(DataOut, OutBufLen)
  else SetLength(DataOut, 0);
end;

function TPCSCReader.IOCTLString(IOCtlCode: Cardinal; DataIn: string; out DataOut: TBytes): Cardinal;
var
  InBuffer: TBytes;
begin
  InBuffer := HexStringToBuffer(DataIn);
  result := IOCTL(IOCtlCode, InBuffer, DataOut);
  SetLength(InBuffer, 0);
end;

procedure TPCSCReader.ClearCardInfo;
begin
  SetLength(FATR, 0);
  FValidCard := false;
end;

function TPCSCReader.GetAtrAsString: string;
var
  i: integer;
begin
  result := '';
  for i := 0 to length(FATR) - 1 do result := result + IntToHex(FATR[i], 2);
end;

procedure TPCSCReader.CheckCardState;
var
  CS: Cardinal;
  tmpATR: TBytes;
  i: integer;
  PCSCResult: Cardinal;
  NewCardState: TCardState;
  FReaderStateArray: array [0 .. 0] of TSCardReaderStateA;
begin
  FReaderStateArray[0].cbAtr := 0;
  FReaderStateArray[0].dwEventState := SCARD_STATE_UNAWARE;
  FReaderStateArray[0].dwCurrentState := SCARD_STATE_UNAWARE;
  FReaderStateArray[0].szReader := @FReaderName[1];
  FReaderStateArray[0].pvUserData := nil;

  PCSCResult := FPCSCRaw.SCardGetStatusChange(FPCSCDeviceContext, 1, @FReaderStateArray, 1);
  if PCSCResult = SCARD_E_CANCELLED then exit;
  if PCSCResult = SCARD_S_SUCCESS then begin
    FReaderStateArray[0].dwCurrentState := FReaderStateArray[0].dwEventState;

    NewCardState := FCardState;
    CS := FReaderStateArray[0].dwEventState;
    if (CS and SCARD_STATE_UNAVAILABLE = 0) then begin
      if (CS and SCARD_STATE_PRESENT <> 0) then begin
        if (CS and SCARD_STATE_EXCLUSIVE <> 0) then NewCardState := csExclusive
        else if (CS and SCARD_STATE_INUSE <> 0) then NewCardState := csShared
        else if (CS and SCARD_STATE_MUTE <> 0) then NewCardState := csBadCard
        else begin
          NewCardState := csAvailable;
          SetLength(tmpATR, FReaderStateArray[0].cbAtr);
          for i := 0 to FReaderStateArray[0].cbAtr - 1 do tmpATR[i] := FReaderStateArray[0].rgbAtr[i];
        end;
      end
      else if (CS and SCARD_STATE_EMPTY) <> 0 then NewCardState := csNoCard
      else if FReaderStateArray[0].cbAtr = 0 then NewCardState := csBadCard
      else NewCardState := csUnknown;
    end;

    if FCardState <> NewCardState then begin
      FCardState := NewCardState;
      if (NewCardState <> csShared) and (NewCardState <> csExclusive) then begin
        ClearCardInfo;
        if (FCardState = csNoCard) then begin
          Disconnect(SCARD_UNPOWER_CARD);
        end
        else if FCardState = csBadCard then begin
          Disconnect(SCARD_UNPOWER_CARD);
        end
        else if FCardState = csAvailable then begin
          FATR := copy(tmpATR);
          FValidCard := true;
        end;
        if Assigned(FOnCardStateChanged) then FOnCardStateChanged(self);
      end;
    end;
  end;
end;

function TPCSCReader.ReadSCardAttrDW(AttrCode: Cardinal; out Attrib: Cardinal): boolean;
var
  len: Cardinal;
begin
  result := false;
  Attrib := 0;
  if FCardHandle = INVALID_HANDLE_VALUE then exit;
  len := sizeof(Cardinal);
  result := FPCSCRaw.SCardGetAttrib(FCardHandle, AttrCode, @Attrib, len) = SCARD_S_SUCCESS;
end;

function TPCSCReader.ReadSCardAttrS(AttrCode: Cardinal; out Attrib: string): boolean;
var
  len: Cardinal;
  cstr: array [0 .. 255] of char;
begin
  result := false;
  Attrib := '';
  if FCardHandle = INVALID_HANDLE_VALUE then exit;
  len := length(cstr);
{$IFDEF FPC}
{$PUSH}
{$HINTS OFF}
  FillByte(cstr, len * sizeof(char), 0);
{$POP}
{$ELSE}
  FillChar(cstr, len * sizeof(char), 0);
{$ENDIF}
  result := FPCSCRaw.SCardGetAttrib(FCardHandle, AttrCode, @cstr[0], len) = SCARD_S_SUCCESS;
  if result then Attrib := string(cstr);
end;

procedure TPCSCReader.CollectReaderData;
begin
  if Connect(SCARD_SHARE_DIRECT) <> SCARD_S_SUCCESS then exit;
  try
    GetVendorName;
    GetDisplayName;
    GetDriverVersion;
    GetFirmwareVersion;
    GetDeviceSerial;
  finally
    Disconnect(SCARD_LEAVE_CARD);
  end;
end;

function TPCSCReader.GetVendorName: boolean;
begin
  result := ReadSCardAttrS(SCARD_ATTR_VENDOR_NAME, FVendorName);
end;

function TPCSCReader.GetDeviceSerial: boolean;
begin
  FSerial := '';
  result := GetDeviceSerialStd;
{$IFDEF UNIX}
  if FSerial = '' then result := GetDeviceSerialLinux;
{$ENDIF}
end;

function TPCSCReader.GetDeviceSerialStd: boolean;
begin
  result := ReadSCardAttrS(SCARD_ATTR_VENDOR_IFD_SERIAL_NO, FSerial);
end;

{$IFDEF UNIX}

function TPCSCReader.GetDeviceSerialLinux: boolean;
var
  p1, p2: integer;
begin
  FSerial := '';
  p1 := pos('(', FReaderName);
  p2 := pos(')', FReaderName);
  if (p1 > 10) and (p2 > p1 + 5) then FSerial := copy(FReaderName, p1 + 1, p2 - p1 - 1);
  result := FSerial <> '';
end;
{$ENDIF}

function TPCSCReader.GetDisplayName: boolean;
{$IFDEF UNIX}
var
  p1, p2: integer;
  tmpName: string;
{$ENDIF}
begin
  result := ReadSCardAttrS(SCARD_ATTR_DEVICE_FRIENDLY_NAME_A, FDisplayName);
  if FDisplayName = '' then FDisplayName := FReaderName;
{$IFDEF UNIX}
  if FDisplayName = '' then exit;
  p1 := pos('[', FDisplayName);
  p2 := pos(']', FDisplayName);
  if (p1 > 10) and (p2 > p1 + 10) then tmpName := trim(copy(FDisplayName, p1 + 1, p2 - p1 - 1));
  if lowercase(tmpName) = 'ccid interface' then FDisplayName := trim(copy(FDisplayName, 1, p1 - 1))
  else FDisplayName := tmpName;
{$ENDIF}
end;

type
  TVersionControl = record
    SmclibVersion: Cardinal;
    DriverMajor: byte;
    DriverMinor: byte;
    FirmwareMajor: byte;
    FirmwareMinor: byte;
    UpdateKey: byte;
  end;

function TPCSCReader.GetDriverVersion: boolean;
var
  Version: TVersionControl;
  ReturnLength: Cardinal;
begin
  ReturnLength := sizeof(TVersionControl);

  result := SCIOCTL(IOCTL_GET_VERSIONS, nil, 0, @Version, ReturnLength) = SCARD_S_SUCCESS;
  if not result then exit;

  FDrvMajor := Version.DriverMajor;
  FDrvMinor := Version.DriverMinor;
  FIFDMajor := Version.FirmwareMajor;
  FIFDMinor := Version.FirmwareMinor;
end;

function TPCSCReader.GetFirmwareVersion: boolean;
var
  x: integer;
begin
  FVendorIFDVersion := 0;
  result := ReadSCardAttrDW(SCARD_ATTR_VENDOR_IFD_VERSION, FVendorIFDVersion);
  if not result then exit;

  x := (FVendorIFDVersion shr 24) and $FF;
  FIFDMajor := ((x shr 4) and $F) * 10 + x and $F;
  x := (FVendorIFDVersion shr 16) and $FF;
  FIFDMinor := ((x shr 4) and $F) * 10 + x and $F;
end;

constructor TPCSC.Create;
begin
  inherited;
  FPCSCDeviceContext := INVALID_HANDLE_VALUE;
  FMCardSupport := true;
  FGetReaderDetails := true;

  FReaderList := TStringList.Create;
  FPCSCRaw := TPCSCRaw.Create;
  FValid := (InitPCSC = SCARD_S_SUCCESS);

  FReaderListThread := TReaderListThread.Create(FPCSCRaw);
  FReaderListThread.OnReaderListChanged := ReaderListChanged;
  FReaderListThread.OnTimer := CheckCardStates;
{$IFDEF UNIX}
  FReaderListThread.OnTerminated := ReaderListThreadTerminated;
  FReaderListThreadRunning := false;
{$ENDIF}

  FPCSCEventList := TPCSCEventList.Create;

  FPCSCEventList.OnCardInsert := CardInsertSync;
  FPCSCEventList.OnCardRemove := CardRemoveSync;
  FPCSCEventList.OnCardError := CardErrorSync;
  FPCSCEventList.OnReaderFound := ReaderFoundSync;
  FPCSCEventList.OnReaderRemoved := ReaderRemovedSync;

  FOnReaderListChanged := nil;
  FOnCardStateChanged := nil;
  FOnReaderFound := nil;
  FOnReaderRemoved := nil;
  FOnCardInserted := nil;
  FOnCardRemoved := nil;
  FOnCardError := nil;

  FOnReaderFoundAsync := FPCSCEventList.ReaderFoundAsync;
  FOnReaderRemovedAsync := FPCSCEventList.ReaderRemovedAsync;
  FOnCardInsertedAsync := FPCSCEventList.CardInsertedAsync;
  FOnCardRemovedAsync := FPCSCEventList.CardRemovedAsync;
  FOnCardErrorAsync := FPCSCEventList.CardErrorAsync;
end;

destructor TPCSC.Destroy;
var
  i: integer;
begin
  FReaderListThread.Terminate;
  // Be sure that thread has been terminated before freeing it
{$IFDEF WINDOWS}
  WaitForSingleObject(FReaderListThread.Handle, 1000);
{$ELSE}
  // WaitForSingleObject is not implemented in Linux
  while FReaderListThreadRunning do Application.ProcessMessages;
{$ENDIF}
  for i := 0 to FReaderList.Count - 1 do TPCSCReader(FReaderList.Objects[i]).Free;
  FReaderList.Free;
  FPCSCEventList.Free;

  if FPCSCDeviceContext <> 0 then begin
    FPCSCRaw.SCardCancel(FPCSCDeviceContext);
    FPCSCRaw.SCardReleaseContext(FPCSCDeviceContext);
  end;

  FPCSCRaw.Shutdown;
  FPCSCRaw.Free;
  FPCSCRaw := nil;

  inherited;
end;

function TPCSC.InitPCSC: Cardinal;
begin
  result := SCARD_F_INTERNAL_ERROR;
  if not FPCSCRaw.Initialize then exit;
  result := FPCSCRaw.SCardEstablishContext(SCARD_SCOPE_SYSTEM, nil, nil, FPCSCDeviceContext);
  if result <> SCARD_S_SUCCESS then result := FPCSCRaw.SCardEstablishContext(SCARD_SCOPE_USER, nil, nil, FPCSCDeviceContext);
end;

procedure TPCSC.CheckCardStates;
var
  i: integer;
begin
  for i := 0 to FReaderList.Count - 1 do begin
    TPCSCReader(FReaderList.Objects[i]).CheckCardState;
  end;
end;

procedure TPCSC.ReaderListChanged;
begin
  if Assigned(FOnReaderListChanged) then FOnReaderListChanged(self);
  UpdatePCSCReaderList;
end;

procedure TPCSC.UpdatePCSCReaderList;
var
  i, j: integer;
  Found: boolean;
  ReaderName: string;
  ReaderList: TStringList;
  PCSCReader: TPCSCReader;
begin
  ReaderList := TStringList.Create;
  try
    GetPCSCReaderList(ReaderList);
    for i := FReaderList.Count - 1 downto 0 do begin
      ReaderName := FReaderList[i];
      Found := false;
      for j := 0 to ReaderList.Count - 1 do begin
        if ReaderName = ReaderList[j] then begin
          Found := true;
          break;
        end;
      end;
      if not Found then begin
        if Assigned(FOnReaderRemovedAsync) then FOnReaderRemovedAsync(self, ReaderName);
        TPCSCReader(FReaderList.Objects[i]).Free;
        FReaderList.Delete(i);
      end;
    end;

    for i := 0 to ReaderList.Count - 1 do begin
      Found := false;
      for j := 0 to FReaderList.Count - 1 do begin
        ReaderName := FReaderList[j];
        if ReaderName = ReaderList[i] then begin
          Found := true;
          break;
        end;
      end;
      if not Found then begin
        ReaderName := ReaderList[i];
        PCSCReader := TPCSCReader.Create(ReaderName, FPCSCRaw);
        PCSCReader.OnCardStateChanged := CardStateChanged;
        FReaderList.AddObject(ReaderName, PCSCReader);
        if FGetReaderDetails then PCSCReader.CollectReaderData;
        if Assigned(FOnReaderFoundAsync) then FOnReaderFoundAsync(self, ReaderName);
      end;
    end;
  finally
    ReaderList.Free;
  end;
end;

procedure TPCSC.GetPCSCReaderList(ReaderList: TStringList);
var
  pReaders: PChar;
  PCSCResult: Cardinal;
  SizeReaders: Cardinal;
  Retried:boolean; // Workaround for Windows 8.1
begin
  ReaderList.Clear;

  Retried:=false; // Workaround for Windows 8.1
  while true do begin // Workaround for Windows 8.1
    PCSCResult := FPCSCRaw.SCardListReaders(FPCSCDeviceContext, nil, nil, SizeReaders);
    if PCSCResult = SCARD_S_SUCCESS then begin
{$IFDEF UNIX} if SizeReaders < 50 then SizeReaders := 50; {$ENDIF} // workaround for Linux, where GetMem fails, if the requested amount of memory is too small
      GetMem(pReaders, SizeReaders * 2 + 2);
      try
        PCSCResult := FPCSCRaw.SCardListReaders(FPCSCDeviceContext, nil, pReaders, SizeReaders);
        if PCSCResult = SCARD_S_SUCCESS then begin
          MultiStrToStringList(pReaders, SizeReaders, ReaderList);
          exit;
        end;
      finally
        if pReaders <> nil then FreeMem(pReaders);
      end;
    end
    else begin
      if Retried then exit; // Workaround for Windows 8.1
      Retried:=true; // Workaround for Windows 8.1
      if {$IFDEF WINDOWS}(PCSCResult = ERROR_BROKEN_PIPE) or (PCSCResult = ERROR_INVALID_HANDLE) or {$ENDIF} (PCSCResult = SCARD_E_SERVICE_STOPPED) then begin
        FPCSCDeviceContext := 0;
        PCSCResult := FPCSCRaw.SCardEstablishContext(SCARD_SCOPE_SYSTEM, nil, nil, FPCSCDeviceContext);
        if PCSCResult <> SCARD_S_SUCCESS then FPCSCRaw.SCardEstablishContext(SCARD_SCOPE_USER, nil, nil, FPCSCDeviceContext);
      end;
    end;
  end; // Workaround for Windows 8.1
end;

procedure TPCSC.CardStateChanged(PCSCReader: TPCSCReader);
var
  ReaderName: string;
  CardState: TCardState;
begin
  CardState := PCSCReader.CardState;
  ReaderName := PCSCReader.ReaderName;
  if Assigned(FOnCardStateChanged) then FOnCardStateChanged(self, ReaderName);
  if CardState = csAvailable then begin
    if Assigned(FOnCardInsertedAsync) then FOnCardInsertedAsync(self, ReaderName, PCSCReader.ATR);
  end
  else if CardState = csNoCard then begin
    if Assigned(FOnCardRemovedAsync) then FOnCardRemovedAsync(self, ReaderName);
  end
  else if CardState = csBadCard then begin
    if Assigned(FOnCardErrorAsync) then FOnCardErrorAsync(self, ReaderName);
  end;
end;

{$IFDEF UNIX}
procedure TPCSC.ReaderListThreadTerminated;
begin
  FReaderListThreadRunning := false;
end;
{$ENDIF}

procedure TPCSC.Start;
begin
{$IFDEF FPC}
  FReaderListThread.Start;
{$ELSE}
{$IF CompilerVersion >= 20}
  FReaderListThread.Start;
{$ELSE}
  FReaderListThread.Resume;
{$IFEND}
{$ENDIF}
{$IFDEF UNIX}
  FReaderListThreadRunning := true;
{$ENDIF}
end;

procedure TPCSC.ProcessEvent;
begin
  if FPCSCEventList = nil then exit;
  FPCSCEventList.ProcessEvent;
end;

function TPCSC.GetPCSCReader(ReaderName: string): TPCSCReader;
var
  i: integer;
begin
  result := nil;
  for i := 0 to FReaderList.Count - 1 do begin
    if FReaderList[i] = ReaderName then begin
      result := TPCSCReader(FReaderList.Objects[i]);
      exit;
    end;
  end;
end;

procedure TPCSC.CardInsertSync(ReaderName, ATR: string);
begin
  if Assigned(FOnCardInserted) then FOnCardInserted(self, ReaderName, HexStringToBuffer(ATR));
end;

procedure TPCSC.CardRemoveSync(ReaderName: string);
begin
  if Assigned(FOnCardRemoved) then FOnCardRemoved(self, ReaderName);
end;

procedure TPCSC.CardErrorSync(ReaderName: string);
begin
  if Assigned(FOnCardError) then FOnCardError(self, ReaderName);
end;

procedure TPCSC.ReaderFoundSync(ReaderName: string);
begin
  if Assigned(FOnReaderFound) then FOnReaderFound(self, ReaderName);
end;

procedure TPCSC.ReaderRemovedSync(ReaderName: string);
begin
  if Assigned(FOnReaderRemoved) then FOnReaderRemoved(self, ReaderName);
end;

end.