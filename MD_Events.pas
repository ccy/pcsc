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

unit MD_Events;
{$ifdef FPC}                                              
  {$mode delphi}
{$else}
  {$define WINDOWS}
{$endif}

interface

uses
  SysUtils, Classes, MD_Tools;

type
  TEventType = (evNone, evCardInsert, evCardRemove, evCardError, evReaderFound, evReaderRemoved);

  TPCSCCardInsertProc = procedure(ReaderName: string; ATR: string) of object;
  TPCSCCardRemoveProc = procedure(ReaderName: string) of object;
  TPCSCCardErrorProc = procedure(ReaderName: string) of object;
  TPCSCReaderFoundProc = procedure(ReaderName: string) of object;
  TPCSCReaderRemovedProc = procedure(ReaderName: string) of object;

  TPCSCEvent = class
  protected
    FEventType: TEventType;
    FReaderName: string;
    FATR: string;
  public
    constructor Create;
    destructor Destroy; override;

    property EventType: TEventType read FEventType;
    property ReaderName: string read FReaderName;
    property ATR: string read FATR;
  end;

  TPCSCCardInsertEvent = class(TPCSCEvent)
  public
    constructor Create(AReaderName: string; AATR: string);
    destructor Destroy; override;
  end;

  TPCSCCardRemoveEvent = class(TPCSCEvent)
  public
    constructor Create(AReaderName: string);
    destructor Destroy; override;
  end;

  TPCSCCardErrorEvent = class(TPCSCEvent)
  public
    constructor Create(AReaderName: string);
    destructor Destroy; override;
  end;

  TPCSCReaderFoundEvent = class(TPCSCEvent)
  public
    constructor Create(AReaderName: string);
    destructor Destroy; override;
  end;

  TPCSCReaderRemovedEvent = class(TPCSCEvent)
  public
    constructor Create(AReaderName: string);
    destructor Destroy; override;
  end;

  TPCSCEventList = class
  private
    FEventList: TList;
    FOnCardInsert: TPCSCCardInsertProc;
    FOnCardRemove: TPCSCCardRemoveProc;
    FOnCardError: TPCSCCardErrorProc;
    FOnReaderFound: TPCSCReaderFoundProc;
    FOnReaderRemoved: TPCSCReaderRemovedProc;
    FProcessingEvent: boolean;
    function ExistsCardInsertEvent(ReaderName: string): boolean;
    function ExistsCardRemoveEvent(ReaderName: string): boolean;
    function ExistsCardErrorEvent(ReaderName: string): boolean;
    function ExistsReaderFoundEvent(ReaderName: string): boolean;
    function ExistsReaderRemovedEvent(ReaderName: string): boolean;
  protected
    procedure CardInsert(ReaderName: string; ATR: string);
    procedure CardRemove(ReaderName: string);
    procedure CardError(ReaderName: string);
    procedure ReaderFound(ReaderName: string);
    procedure ReaderRemoved(ReaderName: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ProcessEvent;
    procedure ProcessAllEvents;

    procedure CardInsertedAsync(Sender: TObject; ReaderName: string; ATR: TBytes);
    procedure CardRemovedAsync(Sender: TObject; ReaderName: string);
    procedure CardErrorAsync(Sender: TObject; ReaderName: string);
    procedure ReaderFoundAsync(Sender: TObject; ReaderName: string);
    procedure ReaderRemovedAsync(Sender: TObject; ReaderName: string);

    property OnCardInsert: TPCSCCardInsertProc read FOnCardInsert write FOnCardInsert;
    property OnCardRemove: TPCSCCardRemoveProc read FOnCardRemove write FOnCardRemove;
    property OnCardError: TPCSCCardErrorProc read FOnCardError write FOnCardError;
    property OnReaderFound: TPCSCReaderFoundProc read FOnReaderFound write FOnReaderFound;
    property OnReaderRemoved: TPCSCReaderRemovedProc read FOnReaderRemoved write FOnReaderRemoved;
  end;

implementation

constructor TPCSCEvent.Create;
begin
  inherited Create;
  FEventType := evNone;
  FReaderName := '';
end;

destructor TPCSCEvent.Destroy;
begin
  inherited Destroy;
end;

constructor TPCSCEventList.Create;
begin
  inherited Create;
  FEventList := TList.Create;
  FOnCardInsert := nil;
  FOnCardRemove := nil;
  FOnCardError := nil;
  FProcessingEvent := false;
  FEventList.Clear;
end;

destructor TPCSCEventList.Destroy;
var
  i: integer;
begin
  for i := 0 to FEventList.Count - 1 do TPCSCEvent(FEventList[i]).Free;
  FEventList.Free;
  inherited Destroy;
end;

procedure TPCSCEventList.CardInsert(ReaderName: string; ATR: string);
begin
  if Assigned(FOnCardInsert) then FOnCardInsert(ReaderName, ATR);
end;

procedure TPCSCEventList.CardRemove(ReaderName: string);
begin
  if Assigned(FOnCardRemove) then FOnCardRemove(ReaderName);
end;

procedure TPCSCEventList.CardError(ReaderName: string);
begin
  if Assigned(FOnCardError) then FOnCardError(ReaderName);
end;

procedure TPCSCEventList.ReaderFound(ReaderName: string);
begin
  if Assigned(FOnReaderFound) then FOnReaderFound(ReaderName);
end;

procedure TPCSCEventList.ReaderRemoved(ReaderName: string);
begin
  if Assigned(FOnReaderRemoved) then FOnReaderRemoved(ReaderName);
end;

procedure TPCSCEventList.ProcessAllEvents;
begin
  while FEventList.Count > 0 do ProcessEvent;
end;

procedure TPCSCEventList.ProcessEvent;
var
  Event: TPCSCEvent;
begin
  if FEventList.Count = 0 then exit;
  try
    Event := TPCSCEvent(FEventList[0]);
    FEventList.Delete(0);
    if Event = nil then exit;
    try
      case Event.EventType of
        evCardInsert:
          begin
            if not ExistsCardInsertEvent(Event.ReaderName) and not FProcessingEvent then begin
              FProcessingEvent := true;
              try
                CardInsert(Event.ReaderName, Event.ATR);
              finally
                FProcessingEvent := false;
              end;
            end;
          end;
        evCardRemove:
          begin
            if not ExistsCardRemoveEvent(Event.ReaderName) then CardRemove(Event.ReaderName);
          end;
        evCardError:
          begin
            if not ExistsCardErrorEvent(Event.ReaderName) and not FProcessingEvent then begin
              FProcessingEvent := true;
              try
                CardError(Event.ReaderName);
              finally
                FProcessingEvent := false;
              end;
            end;
          end;
        evReaderFound:
          begin
            if not ExistsReaderFoundEvent(Event.ReaderName) and not FProcessingEvent then begin
              FProcessingEvent := true;
              try
                ReaderFound(Event.ReaderName);
              finally
                FProcessingEvent := false;
              end;
            end;
          end;
        evReaderRemoved:
          begin
            if not ExistsReaderRemovedEvent(Event.ReaderName) and not FProcessingEvent then begin
              FProcessingEvent := true;
              try
                ReaderRemoved(Event.ReaderName);
              finally
                FProcessingEvent := false;
              end;
            end;
          end;
      else begin
        end;
      end;
    finally
      if Event <> nil then Event.Free;
    end;
  finally
  end;
end;

procedure TPCSCEventList.CardInsertedAsync(Sender: TObject; ReaderName: string; ATR: TBytes);
begin
  FEventList.Add(TPCSCCardInsertEvent.Create(ReaderName, BufferToHexString(ATR)));
end;

procedure TPCSCEventList.CardRemovedAsync(Sender: TObject; ReaderName: string);
begin
  FEventList.Add(TPCSCCardRemoveEvent.Create(ReaderName));
end;

procedure TPCSCEventList.CardErrorAsync(Sender: TObject; ReaderName: string);
begin
  FEventList.Add(TPCSCCardErrorEvent.Create(ReaderName));
end;

procedure TPCSCEventList.ReaderFoundAsync(Sender: TObject; ReaderName: string);
begin
  FEventList.Add(TPCSCReaderFoundEvent.Create(ReaderName));
end;

procedure TPCSCEventList.ReaderRemovedAsync(Sender: TObject; ReaderName: string);
begin
  FEventList.Add(TPCSCReaderRemovedEvent.Create(ReaderName));
end;

function TPCSCEventList.ExistsCardInsertEvent(ReaderName: string): boolean;
var
  i: integer;
  Event: TPCSCEvent;
begin
  result := false;
  for i := 0 to FEventList.Count - 1 do begin
    Event := TPCSCEvent(FEventList[i]);
    if Event = nil then begin
      FEventList.Delete(i);
      exit;
    end;
    if (Event.EventType = evCardInsert) then begin
      if Event.ReaderName = ReaderName then begin
        result := true;
        exit;
      end;
    end;
  end;
end;

function TPCSCEventList.ExistsCardRemoveEvent(ReaderName: string): boolean;
var
  i: integer;
  Event: TPCSCEvent;
begin
  result := false;
  for i := 0 to FEventList.Count - 1 do begin
    Event := TPCSCEvent(FEventList[i]);
    if Event = nil then begin
      FEventList.Delete(i);
      exit;
    end;
    if (Event.EventType = evCardRemove) then begin
      if Event.ReaderName = ReaderName then begin
        result := true;
        exit;
      end;
    end;
  end;
end;

function TPCSCEventList.ExistsCardErrorEvent(ReaderName: string): boolean;
var
  i: integer;
  Event: TPCSCEvent;
begin
  result := false;
  for i := 0 to FEventList.Count - 1 do begin
    Event := TPCSCEvent(FEventList[i]);
    if Event = nil then begin
      FEventList.Delete(i);
      exit;
    end;
    if (Event.EventType = evCardError) then begin
      if Event.ReaderName = ReaderName then begin
        result := true;
        exit;
      end;
    end;
  end;
end;

function TPCSCEventList.ExistsReaderFoundEvent(ReaderName: string): boolean;
var
  i: integer;
  Event: TPCSCEvent;
begin
  result := false;
  for i := 0 to FEventList.Count - 1 do begin
    Event := TPCSCEvent(FEventList[i]);
    if Event = nil then begin
      FEventList.Delete(i);
      exit;
    end;
    if (Event.EventType = evReaderFound) then begin
      if Event.ReaderName = ReaderName then begin
        result := true;
        exit;
      end;
    end;
  end;
end;

function TPCSCEventList.ExistsReaderRemovedEvent(ReaderName: string): boolean;
var
  i: integer;
  Event: TPCSCEvent;
begin
  result := false;
  for i := 0 to FEventList.Count - 1 do begin
    Event := TPCSCEvent(FEventList[i]);
    if Event = nil then begin
      FEventList.Delete(i);
      exit;
    end;
    if (Event.EventType = evReaderRemoved) then begin
      if Event.ReaderName = ReaderName then begin
        result := true;
        exit;
      end;
    end;
  end;
end;

constructor TPCSCCardInsertEvent.Create(AReaderName: string; AATR: string);
begin
  inherited Create;
  FEventType := evCardInsert;
  FReaderName := AReaderName;
  FATR := AATR;
end;

destructor TPCSCCardInsertEvent.Destroy;
begin
  inherited Destroy;
end;

constructor TPCSCCardRemoveEvent.Create(AReaderName: string);
begin
  inherited Create;
  FEventType := evCardRemove;
  FReaderName := AReaderName;
end;

destructor TPCSCCardRemoveEvent.Destroy;
begin
  inherited Destroy;
end;

constructor TPCSCCardErrorEvent.Create(AReaderName: string);
begin
  inherited Create;
  FEventType := evCardError;
  FReaderName := AReaderName;
end;

destructor TPCSCCardErrorEvent.Destroy;
begin
  inherited Destroy;
end;

constructor TPCSCReaderFoundEvent.Create(AReaderName: string);
begin
  inherited Create;
  FEventType := evReaderFound;
  FReaderName := AReaderName;
end;

destructor TPCSCReaderFoundEvent.Destroy;
begin
  inherited Destroy;
end;

constructor TPCSCReaderRemovedEvent.Create(AReaderName: string);
begin
  inherited Create;
  FEventType := evReaderRemoved;
  FReaderName := AReaderName;
end;

destructor TPCSCReaderRemovedEvent.Destroy;
begin
  inherited Destroy;
end;

end.
