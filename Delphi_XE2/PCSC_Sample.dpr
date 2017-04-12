program PCSC_Sample;

uses
{$IF CompilerVersion >= 22}
  Vcl.Forms,
{$ELSE}
  Forms,
{$IFEND}
  Main in 'Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
