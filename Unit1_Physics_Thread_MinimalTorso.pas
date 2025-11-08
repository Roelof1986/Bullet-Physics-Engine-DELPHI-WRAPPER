unit Unit1_Physics_Thread_MinimalTorso;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Diagnostics, System.Math,
  BulletWrapper; // BW_MB_* declaraties → jouw nieuwe DLL

type
  TPose = record
    x, y, z, qx, qy, qz, qw: Double;
  end;

  // Alle Bullet-calls gebeuren HIER (geen FMX in deze thread)
  TBulletThread = class(TThread)
  private
    FCS       : TCriticalSection;
    FBasePose : ^TPose;
    FLinkPose : ^TPose;

    // Featherstone handles (thread-only)
    W, MB, Motor : Pointer;
    Link         : Integer;

    // Joint-limit handle
    FLimit       : Pointer;

    SW           : TStopwatch;

    procedure InitPhysics;
    procedure StepPhysics;
    procedure ReadPoses(out BP, LP: TPose);
  protected
    procedure Execute; override;
  public
    constructor Create(const ACS: TCriticalSection; var ABase, ALink: TPose);

    // Optioneel: limieten runtime bijstellen (in graden)
    procedure SetJointLimitsDegrees(const LoDeg, HiDeg: Double);
  end;

implementation

{ TBulletThread }

constructor TBulletThread.Create(const ACS: TCriticalSection; var ABase, ALink: TPose);
begin
  inherited Create(False);           // start direct
  FreeOnTerminate := False;
  FCS := ACS;
  FBasePose := @ABase;
  FLinkPose := @ALink;
end;

procedure TBulletThread.InitPhysics;
var
  k: Integer;
begin
  // Wereld + gravity + vloer
  W := BW_MB_CreateWorld;
  BW_MB_SetGravity(W, 0, -9.81, 0);
  BW_MB_CreateGroundBox(W, 20.0, 0.2, 20.0, 0, -0.10, 0);

  // Torso (viz 0.4³ → half-extents = 0.2), op (0,3,0), ***vrij*** (fixedBase=0)
  MB := BW_MB_CreateBaseBox(W, 0.2, 0.2, 0.2, 3.0, 0.0, 3.0, 0.0, 0);

  // Eén pootje aan de zijkant (Minimal Torso vectors):
  // link half-extents = (2.5, 0.05, 0.05)  (viz 5.0×0.1×0.1)
  // hinge-as Z, baseCOM→pivot = (-0.20,0,0), pivot→childCOM = (+2.50,0,0) ***flush***
  Link := BW_MB_AddRevoluteLinkBox(W, MB, -1,
            2.5, 0.05, 0.05, 3.0,
            0, 0, 1,                 // Z-as
            -0.20, 0.0, 0.0,         // baseCOM → pivot
            +2.50, 0.0, 0.0);        // pivot  → childCOM (flush aan poot)

  // finalize + add-to-world
  BW_MB_FinalizeAndAdd(W, MB);

  // Colliders (torso + poot) - zodat beide met de vloer kunnen botsen
  BW_MB_AddBaseColliderBox(W, MB, 0.2, 0.2, 0.2);        // torso 0.4³
  BW_MB_AddLinkColliderBox(W, MB, Link, 2.5, 0.05, 0.05);// poot  5×0.1×0.1

  // === Joint limits ===  [-30°, +45°]
  FLimit := BW_MB_AddLimit(W, MB, Link, DegToRad(-30.0), DegToRad(45.0));

  // Velocity-motor (target wordt elke iteratie gezet)
  Motor := BW_MB_AddMotor(W, MB, Link, 0.0, 300.0);

  // kleine init
  for k := 1 to 32 do
    BW_MB_Step(W, 1/240.0, 1);
end;

procedure TBulletThread.StepPhysics;
var
  t, wTarget: Double;
  i: Integer;
begin
  t := SW.Elapsed.TotalSeconds;

  // 18° @ 1.5 Hz * 2 (zelfde als Minimal Torso)
  wTarget := DegToRad(18.0) * (2*Pi*1.5) * Cos(2*Pi*1.5*t) * 2.0;
  BW_MB_SetVelocityMotor(Motor, wTarget, 300.0, 0.2);

  // lichte microsteps per iteratie (UI blijft soepel)
  for i := 1 to 4 do
    BW_MB_Step(W, 1/240.0, 1);
end;

procedure TBulletThread.ReadPoses(out BP, LP: TPose);
begin
  BW_MB_GetBaseTransform(MB, BP.x,BP.y,BP.z, BP.qx,BP.qy,BP.qz,BP.qw);
  BW_MB_GetLinkTransform(MB, Link, LP.x,LP.y,LP.z, LP.qx,LP.qy,LP.qz,LP.qw);
end;

procedure TBulletThread.Execute;
var
  BP, LP: TPose;
begin
  try
    SW := TStopwatch.StartNew;
    InitPhysics;

    // eerste pose wegschrijven
    ReadPoses(BP, LP);
    FCS.Enter; try FBasePose^ := BP; FLinkPose^ := LP; finally FCS.Leave; end;

    while not Terminated do
    begin
      StepPhysics;
      ReadPoses(BP, LP);
      FCS.Enter;
      try
        FBasePose^ := BP;
        FLinkPose^ := LP;
      finally
        FCS.Leave;
      end;
      Sleep(1); // ademruimte voor UI
    end;

  finally
    // limits netjes verwijderen
    if (W <> nil) and (FLimit <> nil) then
      try BW_MB_RemoveConstraint(W, FLimit); except end;

    if W <> nil then
      try BW_MB_DestroyWorld(W); except end;

    W := nil; MB := nil; Motor := nil; FLimit := nil;
  end;
end;

procedure TBulletThread.SetJointLimitsDegrees(const LoDeg, HiDeg: Double);
begin
  // veilig: alleen uitvoeren als wereld en MB bestaan
  if (W=nil) or (MB=nil) then Exit;
  FLimit := BW_MB_ReplaceLimit(W, MB, FLimit, Link, DegToRad(LoDeg), DegToRad(HiDeg));
end;

end.
