// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program SimplexIec104ClientVCL;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  ConnectSettings in 'ConnectSettings.pas' {frmConnectSettings},
  Client.Common in '..\Client.Common.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
