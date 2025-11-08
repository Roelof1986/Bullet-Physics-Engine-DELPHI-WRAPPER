program Bullet_MinimalTorso_Test_MULTITHREAD;

uses
  System.StartUpCopy,
  System.SyncObjs,
  FMX.Forms,
  Unit2_Featherstone_MinimalTorso_Final in 'Unit2_Featherstone_MinimalTorso_Final.pas',
  Unit1_Physics_Thread_MinimalTorso in 'Unit1_Physics_Thread_MinimalTorso.pas';

{$R *.res}
{$APPTYPE CONSOLE}
var
  CS       : TCriticalSection;
  BasePose : TPose;
  LinkPose : TPose;
  Phys     : TBulletThread;
begin
  Application.Initialize;
  // 1) shared state
  CS := TCriticalSection.Create;
  FillChar(BasePose, SizeOf(BasePose), 0);
  FillChar(LinkPose, SizeOf(LinkPose), 0);
  // 2) physics-thread ALS EERSTE laten draaien
  Phys := TBulletThread.Create(CS, BasePose, LinkPose);
  // 3) form zoals jij wilt
  Form1 := TForm1.CreateNew(nil);
  Application.MainForm := Form1;
  // 4) FMX exact als Minimal Torso (alleen viz)
  Form1.InitScene;
  // 5) UI koppelt zich aan de thread-state (alleen lezen/tekenen)
  Form1.AttachSharedState(CS, BasePose, LinkPose);
  // 6) run
  Form1.Show;
  Application.Run;
  // 7) netjes afsluiten
  try
    if Assigned(Phys) then begin Phys.Terminate; Phys.WaitFor; Phys.Free; end;
  finally
    CS.Free;
  end;
  Form1.Free;
end.
