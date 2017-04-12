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

unit MD_PCSCRaw;
{$IFDEF FPC}                              
{$MODE delphi}
{$ELSE}
{$DEFINE WINDOWS}
{$ENDIF}

interface

uses
{$IFDEF WINDOWS}Windows, {$ENDIF}
{$IFDEF UNIX}DynLibs, {$ENDIF}
  SysUtils, Classes, MD_PcscDef;

type
  TSCardBeginTransaction = function(hContext: THandle): Cardinal; stdcall;
  TSCardCancel = function(hContext: THandle): Cardinal; stdcall;
  TSCardConnect = function(hContext: THandle; pReader: PChar; ShareMode: Cardinal; PreferredProtocols: Cardinal; out CardHandle: THandle; out ActiveProtocol: Cardinal): Cardinal; stdcall;
  TSCardControl = function(hContext: THandle; IoCtl: Cardinal; pInBuffer: Pointer; SizeInBuffer: Cardinal; pOutBuffer: Pointer; SizeOutBuffer: Cardinal; var BytesRet: Cardinal): Cardinal; stdcall;
  TSCardDisconnect = function(hContext: THandle; Disposition: Cardinal): Cardinal; stdcall;
  TSCardEndTransaction = function(hContext: THandle; Disposition: Cardinal): Cardinal; stdcall;
  TSCardEstablishContext = function(Scope: Cardinal; pReserved1, pReserved2: Pointer; out hContext: THandle): Cardinal; stdcall;
  TSCardFreeMemory = function(hContext: THandle; pMem: Pointer): Cardinal; stdcall;
  TSCardGetAttrib = function(hCard: THandle; AttrId: Cardinal; pAttr: Pointer; var SizeAttr: Cardinal): Cardinal; stdcall;
  TSCardGetProviderId = function(hContext: THandle; pCard: PChar; out GuidProviderId: TGUID): Cardinal; stdcall;
  TSCardGetStatusChange = function(hContext: THandle; Timeout: Cardinal; pReaderStates: PSCardReaderStateA; ReaderStatesCount: Cardinal): Cardinal; stdcall;
  TSCardListReaders = function(hContext: THandle; pGroups, pReaders: PChar; out SizeReaders: Cardinal): Cardinal; stdcall;
  TSCardListCards = function(hContext: THandle; pAtr: Pointer; pGuidInterfaces: PGUID; GuidInterfacesCount: Cardinal; pCards: PChar; var SizeCards: Cardinal): Cardinal; stdcall;
  TSCardListInterfaces = function(hContext: THandle; pCard: PChar; pGuidInterfaces: PGUID; var GuidInterfacesCount: Cardinal): Cardinal; stdcall;
  TSCardListReaderGroups = function(hContext: THandle; pGroups: PChar; var SizeGroups: Cardinal): Cardinal; stdcall;
  TSCardLocateCards = function(hContext: THandle; pCards: PChar; pReaderStates: PSCardReaderStateA; ReaderStatesCount: Cardinal): Cardinal; stdcall;
  TSCardReconnect = function(hCard: THandle; ShareMode, PreferredProtocols, dwInitialization: Cardinal; out ActiveProtocol: Cardinal): Cardinal; stdcall;
  TSCardReleaseContext = function(hContext: THandle): Cardinal; stdcall;
  TSCardTransmit = function(hCard: THandle; PioSendPci: PSCardIoRequest; pSendBuffer: Pointer; SizeSendBuffer: Cardinal; PioRecvPci: PSCardIoRequest; pRecvBuffer: Pointer; var SizeRecvBuffer: Cardinal): Cardinal; stdcall;
  TSCardSetAttrib = function(hCard: THandle; AttrId: Cardinal; pAttr: Pointer; AttrLen: Cardinal): Cardinal; stdcall;
  TSCardStatus = function(hCard: THandle; pReaderNames: PChar; var SizeReaderNames: Cardinal; out State, Protocol: Cardinal; pAtr: Pointer; var AtrLen: Cardinal): Cardinal; stdcall;
  TSCardIsValidContext = function(hContext: THandle): Cardinal; stdcall;

  TPCSCRaw = class(TObject)
  private
    FValid: boolean;
    FhWinSCard: THandle;

    FSCardBeginTransaction: TSCardBeginTransaction;
    FSCardCancel: TSCardCancel;
    FSCardConnect: TSCardConnect;
    FSCardControl: TSCardControl;
    FSCardDisconnect: TSCardDisconnect;
    FSCardEndTransaction: TSCardEndTransaction;
    FSCardEstablishContext: TSCardEstablishContext;
    FSCardFreeMemory: TSCardFreeMemory;
    FSCardGetAttrib: TSCardGetAttrib;
    FSCardGetProviderId: TSCardGetProviderId;
    FSCardGetStatusChange: TSCardGetStatusChange;
    FSCardListReaders: TSCardListReaders;
    FSCardListCards: TSCardListCards;
    FSCardListInterfaces: TSCardListInterfaces;
    FSCardListReaderGroups: TSCardListReaderGroups;
    FSCardLocateCards: TSCardLocateCards;
    FSCardReconnect: TSCardReconnect;
    FSCardReleaseContext: TSCardReleaseContext;
    FSCardTransmit: TSCardTransmit;
    FSCardSetAttrib: TSCardSetAttrib;
    FSCardStatus: TSCardStatus;
    FSCardIsValidContext: TSCardIsValidContext;
    function CTLCode(DeviceType, _Function, Method, Access: Cardinal): Cardinal;
  public
    constructor Create;
    destructor Destroy; override;

    function Initialize: boolean;
    function Shutdown: boolean;
    function SCardBeginTransaction(hContext: THandle): Cardinal;
    function SCardCancel(hContext: THandle): Cardinal;
    function SCardConnect(hContext: THandle; pReader: PChar; ShareMode, PreferredProtocols: Cardinal; out CardHandle: THandle; out ActiveProtocol: Cardinal): Cardinal;
    function SCardControl(hContext: THandle; IoCtl: Cardinal; pInBuffer: Pointer; SizeInBuffer: Cardinal; pOutBuffer: Pointer; SizeOutBuffer: Cardinal; var BytesRet: Cardinal): Cardinal;
    function SCardDisconnect(hContext: THandle; Disposition: Cardinal): Cardinal;
    function SCardEndTransaction(hContext: THandle; Disposition: Cardinal): Cardinal;
    function SCardEstablishContext(Scope: Cardinal; pReserved1, pReserved2: Pointer; out hContext: THandle): Cardinal;
    function SCardFreeMemory(hContext: THandle; pMem: Pointer): Cardinal;
    function SCardGetAttrib(hCard: THandle; AttrId: Cardinal; pAttr: Pointer; var SizeAttr: Cardinal): Cardinal;
    function SCardGetProviderId(hContext: THandle; pCard: PChar; out GuidProviderId: TGUID): Cardinal;
    function SCardGetStatusChange(hContext: THandle; Timeout: Cardinal; pReaderStates: PSCardReaderStateA; ReadersStatesCount: Cardinal): Cardinal;
    function SCardListReaders(hContext: THandle; pGroups, pReaders: PChar; out SizeReaders: Cardinal): Cardinal;
    function SCardListCards(hContext: THandle; pAtr: Pointer; pGuidInterfaces: PGUID; GuidInterfacesCount: Cardinal; pCards: PChar; var SizeCards: Cardinal): Cardinal;
    function SCardListInterfaces(hContext: THandle; pCard: PChar; pGuidInterfaces: PGUID; var GuidInterfacesCount: Cardinal): Cardinal;
    function SCardListReaderGroups(hContext: THandle; pGroups: PChar; var SizeGroups: Cardinal): Cardinal;
    function SCardLocateCards(hContext: THandle; pCards: PChar; pReaderStates: PSCardReaderStateA; ReaderStatesCount: Cardinal): Cardinal;
    function SCardReconnect(hCard: THandle; ShareMode, PreferredProtocols, dwInitialization: Cardinal; out ActiveProtocol: Cardinal): Cardinal;
    function SCardReleaseContext(hContext: THandle): Cardinal;
    function SCardTransmit(hCard: THandle; PioSendPci: PSCardIoRequest; SendBuffer: Pointer; SizeSendBuffer: Cardinal; PioRecvPci: PSCardIoRequest; RecvBuffer: Pointer; var SizeRecvBuffer: Cardinal): Cardinal;
    function SCardSetAttrib(hCard: THandle; AttrId: Cardinal; pAttr: Pointer; AttrLen: Cardinal): Cardinal;
    function SCardStatus(hCard: THandle; pReaderNames: PChar; var SizeReaderNames: Cardinal; out State, Protocol: Cardinal; pAtr: Pointer; var AtrLen: Cardinal): Cardinal;
    function SCardIsValidContext(hContext: THandle): Cardinal;

    function SCardCTLCode(Code: Cardinal): Cardinal;

    property hWinSCard: THandle read FhWinSCard;
    property Valid: boolean read FValid;
  end;

function MultiStrToStringList(pBuffer: PChar; SizeStr: LongInt; StrList: TStringList): boolean;

implementation

// MultiString:  str1#0str2#0str3#0.....strN#0#0
function MultiStrToStringList(pBuffer: PChar; SizeStr: LongInt; StrList: TStringList): boolean;
var
  i: integer;
  s: string;
  c: char;
begin
  Result := false;
  if StrList = nil then exit;
  StrList.Clear;
  if ((pBuffer = nil) or (SizeStr = 0)) then Result := true
  else if (SizeStr = 1) then begin
    if (pBuffer^ = #0) then Result := true;
  end
  else if ((pBuffer + SizeStr - 1)^ <> #0) or ((pBuffer + SizeStr - 2)^ <> #0) then exit
  else begin
    s := '';
    for i := 0 to SizeStr - 1 do begin
      c := (pBuffer + i)^;
      if c <> #0 then s := s + c
      else begin
        if s <> '' then StrList.Add(s);
        s := '';
      end;
    end;
    Result := true;
  end;
end;

constructor TPCSCRaw.Create;
begin
  inherited Create;
  FhWinSCard := 0;
  FValid := false;
end;

destructor TPCSCRaw.Destroy;
begin
  if FValid then Shutdown;
  inherited Destroy;
end;

function TPCSCRaw.SCardBeginTransaction(hContext: THandle): Cardinal;
begin
  if FValid and Assigned(FSCardBeginTransaction) then Result := FSCardBeginTransaction(hContext)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardCancel(hContext: THandle): Cardinal;
begin
  if FValid and Assigned(FSCardCancel) then Result := FSCardCancel(hContext)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardConnect(hContext: THandle; pReader: PChar; ShareMode: Cardinal; PreferredProtocols: Cardinal; out CardHandle: THandle; out ActiveProtocol: Cardinal): Cardinal;
begin
  if FValid and Assigned(FSCardConnect) then Result := FSCardConnect(hContext, pReader, ShareMode, PreferredProtocols, CardHandle, ActiveProtocol)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardControl(hContext: THandle; IoCtl: Cardinal; pInBuffer: Pointer; SizeInBuffer: Cardinal; pOutBuffer: Pointer; SizeOutBuffer: Cardinal; var BytesRet: Cardinal): Cardinal;
begin
  if FValid and Assigned(FSCardControl) then Result := FSCardControl(hContext, IoCtl, pInBuffer, SizeInBuffer, pOutBuffer, SizeOutBuffer, BytesRet)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardDisconnect(hContext: THandle; Disposition: Cardinal): Cardinal;
begin
  if FValid and Assigned(FSCardDisconnect) then Result := FSCardDisconnect(hContext, Disposition)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardEndTransaction(hContext: THandle; Disposition: Cardinal): Cardinal;
begin
  if FValid and Assigned(FSCardEndTransaction) then Result := FSCardEndTransaction(hContext, Disposition)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardEstablishContext(Scope: Cardinal; pReserved1, pReserved2: Pointer; out hContext: THandle): Cardinal;
begin
  if FValid and Assigned(FSCardEstablishContext) then Result := FSCardEstablishContext(Scope, pReserved1, pReserved2, hContext)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardFreeMemory(hContext: THandle; pMem: Pointer): Cardinal;
begin
  if FValid and Assigned(FSCardFreeMemory) then Result := FSCardFreeMemory(hContext, pMem)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardGetAttrib(hCard: THandle; AttrId: Cardinal; pAttr: Pointer; var SizeAttr: Cardinal): Cardinal;
begin
  if FValid and Assigned(FSCardGetAttrib) then Result := FSCardGetAttrib(hCard, AttrId, pAttr, SizeAttr)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardGetProviderId(hContext: THandle; pCard: PChar; out GuidProviderId: TGUID): Cardinal;
begin
  if FValid and Assigned(FSCardGetProviderId) then Result := FSCardGetProviderId(hContext, pCard, GuidProviderId)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardGetStatusChange(hContext: THandle; Timeout: Cardinal; pReaderStates: PSCardReaderStateA; ReadersStatesCount: Cardinal): Cardinal;
begin
  if FValid and Assigned(FSCardGetStatusChange) then Result := FSCardGetStatusChange(hContext, Timeout, pReaderStates, ReadersStatesCount)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardListReaders(hContext: THandle; pGroups, pReaders: PChar; out SizeReaders: Cardinal): Cardinal;
begin
  if FValid and Assigned(FSCardListReaders) then Result := FSCardListReaders(hContext, pGroups, pReaders, SizeReaders)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardListCards(hContext: THandle; pAtr: Pointer; pGuidInterfaces: PGUID; GuidInterfacesCount: Cardinal; pCards: PChar; var SizeCards: Cardinal): Cardinal;
begin
  if FValid and Assigned(FSCardListCards) then Result := FSCardListCards(hContext, pAtr, pGuidInterfaces, GuidInterfacesCount, pCards, SizeCards)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardListInterfaces(hContext: THandle; pCard: PChar; pGuidInterfaces: PGUID; var GuidInterfacesCount: Cardinal): Cardinal;
begin
  if FValid and Assigned(FSCardListInterfaces) then Result := FSCardListInterfaces(hContext, pCard, pGuidInterfaces, GuidInterfacesCount)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardListReaderGroups(hContext: THandle; pGroups: PChar; var SizeGroups: Cardinal): Cardinal;
begin
  if FValid and Assigned(FSCardListReaderGroups) then Result := FSCardListReaderGroups(hContext, pGroups, SizeGroups)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardLocateCards(hContext: THandle; pCards: PChar; pReaderStates: PSCardReaderStateA; ReaderStatesCount: Cardinal): Cardinal;
begin
  if FValid and Assigned(FSCardLocateCards) then Result := FSCardLocateCards(hContext, pCards, pReaderStates, ReaderStatesCount)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardReconnect(hCard: THandle; ShareMode, PreferredProtocols, dwInitialization: Cardinal; out ActiveProtocol: Cardinal): Cardinal;
begin
  if FValid and Assigned(FSCardReconnect) then Result := FSCardReconnect(hCard, ShareMode, PreferredProtocols, dwInitialization, ActiveProtocol)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardReleaseContext(hContext: THandle): Cardinal;
begin
  if FValid and Assigned(FSCardReleaseContext) then Result := FSCardReleaseContext(hContext)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardTransmit(hCard: THandle; PioSendPci: PSCardIoRequest; SendBuffer: Pointer; SizeSendBuffer: Cardinal; PioRecvPci: PSCardIoRequest; RecvBuffer: Pointer;
  var SizeRecvBuffer: Cardinal): Cardinal;
begin
  if FValid and Assigned(FSCardTransmit) then Result := FSCardTransmit(hCard, PioSendPci, SendBuffer, SizeSendBuffer, PioRecvPci, RecvBuffer, SizeRecvBuffer)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardSetAttrib(hCard: THandle; AttrId: Cardinal; pAttr: Pointer; AttrLen: Cardinal): Cardinal;
begin
  if FValid and Assigned(FSCardSetAttrib) then Result := FSCardSetAttrib(hCard, AttrId, pAttr, AttrLen)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardStatus(hCard: THandle; pReaderNames: PChar; var SizeReaderNames: Cardinal; out State, Protocol: Cardinal; pAtr: Pointer; var AtrLen: Cardinal): Cardinal;
begin
  if FValid and Assigned(FSCardStatus) then Result := FSCardStatus(hCard, pReaderNames, SizeReaderNames, State, Protocol, pAtr, AtrLen)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.SCardIsValidContext(hContext: THandle): Cardinal;
begin
  if FValid and Assigned(FSCardIsValidContext) then Result := FSCardIsValidContext(hContext)
  else Result := SCARD_E_UNSUPPORTED_FEATURE;
end;

function TPCSCRaw.CTLCode(DeviceType, _Function, Method, Access: Cardinal): Cardinal;
begin
  Result := (DeviceType shl 16) or (Access shl 14) or (_Function shl 2) or Method;
end;

function TPCSCRaw.SCardCTLCode(Code: Cardinal): Cardinal;
begin
{$IFDEF WINDOWS}
  Result := CTLCode(FILE_DEVICE_SMARTCARD, Code, METHOD_BUFFERED, FILE_ANY_ACCESS);
{$ELSE}
  Result := $42000000 or Code;
{$ENDIF}
end;

function TPCSCRaw.Initialize: boolean;
const
{$IFDEF WINDOWS}
  _LIB_NAME = 'winscard.dll';
{$ELSE}
  {$IFDEF DARWIN}
  _LIB_NAME = '/System/Library/Frameworks/PCSC.framework/PCSC';
  {$ELSE}
  _LIB_NAME = 'libpcsclite.so';
  {$ENDIF}
{$ENDIF}
var
  tmpDLLH: THandle;
begin
  Result := false;
  tmpDLLH := LoadLibrary(_LIB_NAME);
  if (tmpDLLH = 0) then exit;

  FSCardBeginTransaction := TSCardBeginTransaction(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardBeginTransaction'))));
  FSCardCancel := TSCardCancel(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardCancel'))));
{$IFDEF WINDOWS}
{$IFNDEF UNICODE}
  FSCardConnect := TSCardConnect(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardConnectA'))));
  FSCardGetStatusChange := TSCardGetStatusChange(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardGetStatusChangeA'))));
  FSCardListReaders := TSCardListReaders(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardListReadersA'))));
  FSCardListCards := TSCardListCards(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardListCardsA'))));
  FSCardListInterfaces := TSCardListInterfaces(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardListInterfacesA'))));
  FSCardGetProviderId := TSCardGetProviderId(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardGetProviderIdA'))));
  FSCardListReaderGroups := TSCardListReaderGroups(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardListReaderGroupsA'))));
  FSCardLocateCards := TSCardLocateCards(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardLocateCardsA'))));
  FSCardStatus := TSCardStatus(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardStatusA'))));
  FSCardIsValidContext := nil;
{$ELSE}
  FSCardConnect := TSCardConnect(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardConnectW'))));
  FSCardGetStatusChange := TSCardGetStatusChange(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardGetStatusChangeW'))));
  FSCardListReaders := TSCardListReaders(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardListReadersW'))));
  FSCardListCards := TSCardListCards(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardListCardsW'))));
  FSCardListInterfaces := TSCardListInterfaces(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardListInterfacesW'))));
  FSCardGetProviderId := TSCardGetProviderId(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardGetProviderIdW'))));
  FSCardListReaderGroups := TSCardListReaderGroups(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardListReaderGroupsW'))));
  FSCardLocateCards := TSCardLocateCards(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardLocateCardsW'))));
  FSCardStatus := TSCardStatus(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardStatusW'))));
  FSCardIsValidContext := nil;
{$ENDIF}
{$ELSE}
  FSCardConnect := TSCardConnect(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardConnect'))));
  FSCardGetStatusChange := TSCardGetStatusChange(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardGetStatusChange'))));
  FSCardListReaders := TSCardListReaders(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardListReaders'))));
  FSCardListCards := TSCardListCards(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardListCards'))));
  FSCardListInterfaces := TSCardListInterfaces(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardListInterfaces'))));
  FSCardGetProviderId := TSCardGetProviderId(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardGetProviderId'))));
  FSCardListReaderGroups := TSCardListReaderGroups(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardListReaderGroups'))));
  FSCardLocateCards := TSCardLocateCards(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardLocateCards'))));
  FSCardStatus := TSCardStatus(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardStatus'))));
  FSCardIsValidContext := TSCardIsValidContext(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardIsValidContext'))));
{$ENDIF}
{$IFDEF DARWIN}
  FSCardControl := TSCardControl(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardControl132'))));
{$ELSE}
  FSCardControl := TSCardControl(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardControl'))));
{$ENDIF}
  FSCardDisconnect := TSCardDisconnect(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardDisconnect'))));
  FSCardEndTransaction := TSCardEndTransaction(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardEndTransaction'))));
  FSCardEstablishContext := TSCardEstablishContext(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardEstablishContext'))));
  FSCardFreeMemory := TSCardFreeMemory(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardFreeMemory'))));
  FSCardGetAttrib := TSCardGetAttrib(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardGetAttrib'))));
  FSCardReconnect := TSCardReconnect(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardReconnect'))));
  FSCardReleaseContext := TSCardReleaseContext(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardReleaseContext'))));
  FSCardTransmit := TSCardTransmit(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardTransmit'))));
  FSCardSetAttrib := TSCardSetAttrib(GetProcAddress(tmpDLLH, PAnsiChar(AnsiString('SCardSetAttrib'))));

  // check for minimum set of function
  FValid :=
    Assigned(FSCardConnect) and
    Assigned(FSCardControl) and
    Assigned(FSCardDisconnect) and
    Assigned(FSCardEstablishContext) and
    Assigned(FSCardGetAttrib) and
    Assigned(FSCardListReaders) and
    Assigned(FSCardReconnect) and
    Assigned(FSCardReleaseContext) and
    Assigned(FSCardTransmit) and
    Assigned(FSCardStatus);

  if FValid then begin
    FValid := true;
    Result := true;
    FhWinSCard := tmpDLLH;
  end;
end;

function TPCSCRaw.Shutdown: boolean;
begin
  if FValid then begin
    Result := true;
    FValid := false;
    FreeLibrary(FhWinSCard);
    FhWinSCard := 0;
  end
  else Result := false;
end;

end.
