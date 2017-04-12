//////////////////////////////////////////////////////////////////////////////////
//     This source code is provided 'as-is', without any express or implied     //
//     warranty. In no event will Infintuary be held liable for any damages     //
//     arising from the use of this software.                                   //
//                                                                              //
//     Infintuary does not warrant, that the source code will be free from      //
//     defects in design or workmanship or that operation of the source code    //
//     will be error-free. No implied or statutory warranty of merchantability  //
//     or fitness for a particulat purpose shall apply. The entire risk of      //
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

unit Main;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Forms, Controls, ComCtrls, StdCtrls,
  ExtCtrls, Graphics,
  MD_PCSC, MD_PCSCDef, MD_Tools;

type

  { TMainForm }

  TMainForm = class(TForm)
    TopPanel: TPanel;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Label2: TLabel;
    Panel3: TPanel;
    Label1: TLabel;
    ReaderListBox: TListBox;
    StatusBar1: TStatusBar;
    ConnectSharedButton: TButton;
    ConnectExclusiveButton: TButton;
    ConnectDirectButton: TButton;
    DisconnectButton: TButton;
    Label3: TLabel;
    CommandComboBox: TComboBox;
    TransmitButton: TButton;
    Bevel1: TBevel;
    LogMemo: TMemo;
    procedure ConnectDirectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ReaderListBoxDrawItem(Control: TWinControl; Index: Integer; RC: TRect; State: TOwnerDrawState);
    procedure ConnectSharedButtonClick(Sender: TObject);
    procedure ConnectExclusiveButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure TransmitButtonClick(Sender: TObject);
    procedure ReaderListBoxClick(Sender: TObject);
    procedure CommandComboBoxChange(Sender: TObject);
    procedure CommandComboBoxEnter(Sender: TObject);
    procedure CommandComboBoxKeyPress(Sender: TObject; var Key: Char);
    procedure CommandComboBoxKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure CommandComboBoxExit(Sender: TObject);
  private
    FPCSC: TPCSC;
    Initialized: boolean;
    procedure UpdatePCSCReaderList;
    procedure CardStateChanged(Sender: TObject; ReaderName: string);
    procedure UpdateButtons;
    procedure ProcessEvents(Sender: TObject; var Done: boolean);

    procedure ReaderFound(Sender: TObject; ReaderName: string);
    procedure ReaderRemoved(Sender: TObject; ReaderName: string);
    procedure CardInserted(Sender: TObject; ReaderName: string; ATR: TBytes);
    procedure CardRemoved(Sender: TObject; ReaderName: string);
    procedure CardError(Sender: TObject; ReaderName: string);

    procedure AddLogMemo(Msg: string);
    function ErrorToString(ErrorCode: DWORD): string;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if not Initialized then begin
    Initialized := true;

    if not(FPCSC.Valid) then AddLogMemo('SCardEstablishContext failed.')
    else begin
      AddLogMemo('SCardEstablishContext succeeded.');
      FPCSC.OnReaderFound := ReaderFound;
      FPCSC.OnReaderRemoved := ReaderRemoved;
      FPCSC.OnCardStateChanged := CardStateChanged;
      FPCSC.OnCardInserted := CardInserted;
      FPCSC.OnCardRemoved := CardRemoved;
      FPCSC.OnCardError := CardError;

      FPCSC.Start;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := 'PC/SC Sample Application V1.3';
  Application.Title := Caption;
  Application.OnIdle := ProcessEvents;
  LogMemo.Clear;
  Initialized := false;
  FPCSC := TPCSC.Create;

{$IFDEF LINUX}
  CommandComboBox.Top := 40;
{$ENDIF}
{$IFDEF WINDOWS}
  CommandComboBox.Top := 43;
{$ENDIF}
end;

procedure TMainForm.ConnectDirectButtonClick(Sender: TObject);
var
  PCSCResult: DWORD;
  PCSCReader: TPCSCReader;
begin
  try
    if ReaderListBox.ItemIndex < 0 then exit;
    PCSCReader := FPCSC.GetPCSCReader(ReaderListBox.Items[ReaderListBox.ItemIndex]);
    if PCSCReader = nil then exit;

    PCSCResult := PCSCReader.Connect(SCARD_SHARE_DIRECT);
    if PCSCResult = SCARD_S_SUCCESS then AddLogMemo('SCardConnect (direct) succeeded.')
    else AddLogMemo(Format('SCardConnect (direct) failed with error code %s (%s)', [IntToHex(PCSCResult, 8), ErrorToString(PCSCResult)]));
  finally
    UpdateButtons;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FPCSC.Free;
end;

procedure TMainForm.CardStateChanged(Sender: TObject; ReaderName: string);
var
  CardState: TCardState;
  PCSCReader: TPCSCReader;
  sState: string;
begin
  PCSCReader := TPCSC(Sender).GetPCSCReader(ReaderName);
  if PCSCReader = nil then exit;

  CardState := PCSCReader.CardState;
  ReaderName := PCSCReader.ReaderName;
  case CardState of
    csExclusive: sState := 'exclusive';
    csShared: sState := 'shared';
    csAvailable: sState := 'available';
    csBadCard: sState := 'bad card';
    csNoCard: sState := 'no card';
  else sState := 'unknown';
  end;
  AddLogMemo('Card State changed in ' + ReaderName + ' to ' + sState);
  ReaderListBox.Repaint;
  UpdateButtons;
end;

procedure TMainForm.UpdatePCSCReaderList;
var
  i, j: Integer;
  Found: boolean;
  ReaderName: string;
begin
  for i := ReaderListBox.Items.Count - 1 downto 0 do begin
    ReaderName := ReaderListBox.Items[i];
    Found := false;
    for j := 0 to FPCSC.ReaderList.Count - 1 do begin
      if ReaderName = FPCSC.ReaderList[j] then begin
        Found := true;
        break;
      end;
    end;
    if not Found then ReaderListBox.Items.Delete(i);
  end;

  for i := 0 to FPCSC.ReaderList.Count - 1 do begin
    Found := false;
    for j := 0 to ReaderListBox.Items.Count - 1 do begin
      ReaderName := ReaderListBox.Items[j];
      if ReaderName = FPCSC.ReaderList[i] then begin
        Found := true;
        break;
      end;
    end;
    if not Found then begin
      ReaderName := FPCSC.ReaderList[i];
      ReaderListBox.Items.Add(ReaderName);
    end;
  end;
end;

procedure TMainForm.ReaderListBoxDrawItem(Control: TWinControl; Index: Integer; RC: TRect; State: TOwnerDrawState);
var
  sState: string;
  PCSCReader: TPCSCReader;
begin
  with TListBox(Control).Canvas do begin
    PCSCReader := FPCSC.GetPCSCReader(TListBox(Control).Items[Index]);
    if PCSCReader = nil then exit;

    if odSelected in State then begin
      Brush.Color := $00CEFF;
      Font.Color := clBlack;
    end
    else begin
      Brush.Color := clWhite;
      Font.Color := clBlack;
    end;
    Pen.Color := Brush.Color;

    Rectangle(RC.Left, RC.Top, RC.Right, RC.Bottom);
    TextOut(RC.Left + 4, RC.Top + 2, TListBox(Control).Items[Index]);

    if PCSCReader <> nil then begin
      case PCSCReader.CardState of
        csExclusive:
          begin
            Font.Color := clGreen;
            sState := 'Card state = exclusive, ATR = ' + PCSCReader.ATRasString;
          end;
        csShared:
          begin
            Font.Color := clGreen;
            sState := 'Card state = shared, ATR = ' + PCSCReader.ATRasString;
          end;
        csAvailable:
          begin
            Font.Color := clGreen;
            sState := 'Card state = available, ATR = ' + PCSCReader.ATRasString;
          end;
        csBadCard:
          begin
            Font.Color := $808080;
            sState := 'Card state = bad card';
          end;
        csNoCard:
          begin
            Font.Color := clGray;
            sState := 'Card state = no card';
          end;
      else begin
          Font.Color := clGray;
          sState := 'Card state = unknown';
        end;
      end;
      TextOut(RC.Left + 20, RC.Top + 15, sState);
    end;
  end;
end;

procedure TMainForm.UpdateButtons;
var
  PCSCReader: TPCSCReader;
begin
  ConnectSharedButton.Enabled := false;
  ConnectExclusiveButton.Enabled := false;
  ConnectDirectButton.Enabled := false;
  DisconnectButton.Enabled := false;
  CommandComboBox.Enabled := false;
  TransmitButton.Enabled := false;

  if ReaderListBox.ItemIndex < 0 then exit;
  PCSCReader := FPCSC.GetPCSCReader(ReaderListBox.Items[ReaderListBox.ItemIndex]);
  if PCSCReader = nil then exit;

  if (PCSCReader.CardState = csAvailable) or (PCSCReader.CardState = csExclusive) or (PCSCReader.CardState = csShared) then begin
    if PCSCReader.CardHandle = INVALID_HANDLE_VALUE then begin
      ConnectSharedButton.Enabled := true;
      ConnectExclusiveButton.Enabled := true;
    end;
  end;
  if PCSCReader.CardHandle <> INVALID_HANDLE_VALUE then begin
    DisconnectButton.Enabled := true;
    CommandComboBox.Enabled := true;
    TransmitButton.Enabled := length(trim(CommandComboBox.Text)) > 1;
  end
  else ConnectDirectButton.Enabled := true;
end;

procedure TMainForm.ConnectSharedButtonClick(Sender: TObject);
var
  PCSCResult: DWORD;
  PCSCReader: TPCSCReader;
begin
  try
    if ReaderListBox.ItemIndex < 0 then exit;
    PCSCReader := FPCSC.GetPCSCReader(ReaderListBox.Items[ReaderListBox.ItemIndex]);
    if PCSCReader = nil then exit;

    PCSCResult := PCSCReader.Connect(SCARD_SHARE_SHARED);
    if PCSCResult = SCARD_S_SUCCESS then AddLogMemo('SCardConnect (shared) succeeded.')
    else AddLogMemo(Format('SCardConnect (shared) failed with error code %s (%s)', [IntToHex(PCSCResult, 8), ErrorToString(PCSCResult)]));
  finally
    UpdateButtons;
  end;
end;

procedure TMainForm.ConnectExclusiveButtonClick(Sender: TObject);
var
  PCSCResult: DWORD;
  PCSCReader: TPCSCReader;
begin
  try
    if ReaderListBox.ItemIndex < 0 then exit;
    PCSCReader := FPCSC.GetPCSCReader(ReaderListBox.Items[ReaderListBox.ItemIndex]);
    if PCSCReader = nil then exit;

    PCSCResult := PCSCReader.Connect(SCARD_SHARE_EXCLUSIVE);
    if PCSCResult = SCARD_S_SUCCESS then AddLogMemo('SCardConnect (exclusive) succeeded.')
    else AddLogMemo(Format('SCardConnect (exclusive) failed with error code %s (%s)', [IntToHex(PCSCResult, 8), ErrorToString(PCSCResult)]));
  finally
    UpdateButtons;
  end;
end;

procedure TMainForm.DisconnectButtonClick(Sender: TObject);
var
  PCSCResult: DWORD;
  PCSCReader: TPCSCReader;
begin
  try
    if ReaderListBox.ItemIndex < 0 then exit;
    PCSCReader := FPCSC.GetPCSCReader(ReaderListBox.Items[ReaderListBox.ItemIndex]);
    if PCSCReader = nil then exit;

    PCSCResult := PCSCReader.Disconnect();
    if PCSCResult = SCARD_S_SUCCESS then AddLogMemo('SCardDisconnect succeeded.')
    else AddLogMemo(Format('SCardDisconnect failed with error code %s (%s)', [IntToHex(PCSCResult, 8), ErrorToString(PCSCResult)]));
  finally
    UpdateButtons;
  end;
end;

procedure TMainForm.TransmitButtonClick(Sender: TObject);
var
  PCSCResult: DWORD;
  PCSCReader: TPCSCReader;
  DataIn: TBytes;
  DataOut: TBytes;
  SW12: Word;
begin
  try
    if ReaderListBox.ItemIndex < 0 then exit;
    PCSCReader := FPCSC.GetPCSCReader(ReaderListBox.Items[ReaderListBox.ItemIndex]);
    if PCSCReader = nil then exit;

    if CommandComboBox.Items.IndexOf(lowercase(trim(CommandComboBox.Text))) < 0 then CommandComboBox.Items.Insert(0, CommandComboBox.Text);
    DataIn := HexStringToBuffer(CommandComboBox.Text);

    if PCSCReader.Protocol = prRaw then begin
      AddLogMemo('Sending Escape command to reader: ' + BufferToHexString(DataIn));
      PCSCResult := PCSCReader.IOCTL(IOCTL_CCID_ESCAPE, DataIn, DataOut);
      if PCSCResult = SCARD_S_SUCCESS then begin
        AddLogMemo('SCardControl succeeded.');
        if length(DataOut) > 0 then AddLogMemo('Response data: ' + BufferToHexString(DataOut));
      end
      else AddLogMemo(Format('SCardControl failed with error code %s (%s)', [IntToHex(PCSCResult, 8), ErrorToString(PCSCResult)]));
    end
    else begin
      AddLogMemo('Sending APDU to card: ' + BufferToHexString(DataIn));
      PCSCResult := PCSCReader.TransmitSW(DataIn, DataOut, SW12);
      if PCSCResult = SCARD_S_SUCCESS then begin
        AddLogMemo('SCardTransmit succeeded.');
        AddLogMemo('Card response status word: ' + IntToHex(SW12, 4) + ' (' + CardErrorToString(SW12) + ')');
        if length(DataOut) > 0 then AddLogMemo('Card response data: ' + BufferToHexString(DataOut));
      end
      else AddLogMemo(Format('SCardTransmit failed with error code %s (%s)', [IntToHex(PCSCResult, 8), ErrorToString(PCSCResult)]));
    end;
  finally
    UpdateButtons;
  end;
end;

function TMainForm.ErrorToString(ErrorCode: DWORD): string;
begin
  if ErrorCode >= $80000000 then result := PCSCErrorToString(ErrorCode)
  else result := WindowsErrorToString(ErrorCode);
end;

procedure TMainForm.ReaderListBoxClick(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TMainForm.CommandComboBoxChange(Sender: TObject);
var
  i: Integer;
  s: string;
  Changed: boolean;
  SelStart: Integer;
const
  AllowedChars = ['A' .. 'F', '0' .. '9', ' '];
begin
  Changed := false;
  SelStart := CommandComboBox.SelStart;
  s := '';
  for i := 1 to length(CommandComboBox.Text) do begin
    if CharInSet(UpCase(CommandComboBox.Text[i]), AllowedChars) then begin
      s := s + UpCase(CommandComboBox.Text[i]);
    end
    else Changed := true;
  end;
  if Changed then begin
    CommandComboBox.Text := s;
    CommandComboBox.SelStart := SelStart;
    CommandComboBox.SelLength := 0;
  end;
  TransmitButton.Enabled := length(trim(CommandComboBox.Text)) > 1;
end;

procedure TMainForm.CommandComboBoxEnter(Sender: TObject);
begin
  CommandComboBox.SelStart := length(TEdit(Sender).Text);
  CommandComboBox.SelLength := 0;
end;

procedure TMainForm.CommandComboBoxExit(Sender: TObject);
begin
  CommandComboBox.Text := BufferToHexString(HexStringToBuffer(CommandComboBox.Text));
end;

procedure TMainForm.CommandComboBoxKeyPress(Sender: TObject; var Key: Char);
begin
  if ((Key >= '0') and (Key <= '9')) or (Key <= #32) or ((UpCase(Key) >= 'A') and (UpCase(Key) <= 'F')) then Key := UpCase(Key)
  else Key := #0;
end;

procedure TMainForm.CommandComboBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    if TransmitButton.Enabled then TransmitButton.Click;
  end;
end;

procedure TMainForm.ReaderFound(Sender: TObject; ReaderName: string);
begin
  AddLogMemo('New reader found: ' + ReaderName);
  UpdatePCSCReaderList;
end;

procedure TMainForm.ReaderRemoved(Sender: TObject; ReaderName: string);
begin
  AddLogMemo('Reader removed: ' + ReaderName);
  UpdatePCSCReaderList;
end;

procedure TMainForm.CardInserted(Sender: TObject; ReaderName: string; ATR: TBytes);
var
  PCSCReader: TPCSCReader;
begin
  AddLogMemo('Card inserted in ' + ReaderName);
  PCSCReader := TPCSCReader(FPCSC.GetPCSCReader(ReaderName));
  if PCSCReader <> nil then begin
    if length(ATR) > 0 then AddLogMemo('ATR = ' + BufferToHexString(ATR));
  end;
end;

procedure TMainForm.CardRemoved(Sender: TObject; ReaderName: string);
begin
  AddLogMemo('Card removed from ' + ReaderName);
end;

procedure TMainForm.CardError(Sender: TObject; ReaderName: string);
begin
  AddLogMemo('Card error in ' + ReaderName);
end;

procedure TMainForm.ProcessEvents(Sender: TObject; var Done: boolean);
begin
  if FPCSC.Valid then FPCSC.ProcessEvent;
  Done := true;
end;

procedure TMainForm.AddLogMemo(Msg: string);
begin
  LogMemo.Lines.Add(Msg);
end;

end.
