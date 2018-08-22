// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program SimplexIec104ClientFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {frmMain},
  ConnectSettings in 'ConnectSettings.pas' {frmConnectSettings},
  Client.Common in '..\Client.Common.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
