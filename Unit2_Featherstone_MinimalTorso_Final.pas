unit Unit2_Featherstone_MinimalTorso_Final;

interface

uses
  System.SysUtils, System.Classes, System.Math, System.Math.Vectors, System.SyncObjs,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.Viewport3D, FMX.Controls3D, FMX.Objects3D, FMX.Types3D, FMX.MaterialSources,
  Unit1_Physics_Thread_MinimalTorso;  // TPose + TBulletThread

type
  TForm1 = class(TForm)
  private
    // === FMX (EXACT als Minimal Torso) ===
    View     : TViewport3D;
    Cam      : TCamera;
    Light    : TLight;
    Floor    : TCube;
    MatFloor : TColorMaterialSource;
    MatTorso : TColorMaterialSource;
    MatLeg   : TColorMaterialSource;
    CubeTorso: TCube;
    CubeLeg  : TCube;

    // === gedeelde state (alleen lezen!) ===
    CS       : TCriticalSection;
    PBase    : ^TPose;
    PLink    : ^TPose;

    // === UI timer (alleen viz) ===
    UITimer  : TTimer;

    // helpers
    procedure MapYFlipToCube(const P: TPose; C: TCube);
    procedure OnUITick(Sender: TObject);
  public
    procedure InitScene;  // bouwt alleen FMX op (zoals jouw Minimal Torso)
    procedure AttachSharedState(ACS: TCriticalSection; var ABase, ALink: TPose);
  end;

var
  Form1: TForm1;

implementation

//{$R *.res}

procedure TForm1.InitScene;
begin
  Caption := 'Minimal Torso (Featherstone – threaded)';
  Width := 1280; Height := 800; Position := TFormPosition.poScreenCenter;

  // Viewport
  View := TViewport3D.Create(Self);
  View.Parent := Self; View.Align := TAlignLayout.Contents;
  View.Color := $FF87CEEB; View.UsingDesignCamera := False;

  // Camera (Y-flip viz)
  Cam := TCamera.Create(Self); Cam.Parent := View;
  Cam.Position.Point := Point3D(0, {0.9}-2.0, {3.8}-6.8);
  Cam.RotationAngle.X := -12;
  View.Camera := Cam;

  // Licht
  Light := TLight.Create(Self); Light.Parent := View;
  Light.LightType := TLightType.Point;
  Light.Position.Point := Point3D(1.5, 2.0, 1.5);

  // Vloer (top ~ 0.20)
  MatFloor := TColorMaterialSource.Create(Self); MatFloor.Color := $FF2FAF2F;
  Floor := TCube.Create(Self); Floor.Parent := View; Floor.MaterialSource := MatFloor;
  Floor.Width := 20.0; Floor.Depth := 20.0; Floor.Height := 0.2; Floor.Position.Y := 0.20;

  // Torso/Leg viz (0.4³ en 5×0.1×0.1)
  MatTorso := TColorMaterialSource.Create(Self); MatTorso.Color := $FF4060FF;
  MatLeg   := TColorMaterialSource.Create(Self); MatLeg.Color   := $FFFFFF00;

  CubeTorso := TCube.Create(Self); CubeTorso.Parent := View; CubeTorso.MaterialSource := MatTorso;
  CubeTorso.Width := 0.4; CubeTorso.Height := 0.4; CubeTorso.Depth := 0.4;

  CubeLeg := TCube.Create(Self); CubeLeg.Parent := View; CubeLeg.MaterialSource := MatLeg;
  CubeLeg.Width := 5.0; CubeLeg.Height := 0.1; CubeLeg.Depth := 0.1;

  // UI-timer
  UITimer := TTimer.Create(Self);
  UITimer.Interval := 16;     // ~60 Hz
  UITimer.OnTimer  := OnUITick;
  UITimer.Enabled  := True;
end;

procedure TForm1.AttachSharedState(ACS: TCriticalSection; var ABase, ALink: TPose);
begin
  CS    := ACS;
  PBase := @ABase;
  PLink := @ALink;
end;

procedure TForm1.MapYFlipToCube(const P: TPose; C: TCube);
var sr,cr,roll, sp,pitch, sy,cy,yaw: Double;
begin
  C.Position.Point := Point3D(P.x, -P.y, P.z);

  sr := 2*(P.qw*P.qx + P.qy*P.qz);
  cr := 1 - 2*(P.qx*P.qx + P.qy*P.qy);
  roll  := ArcTan2(sr, cr);

  sp := 2*(P.qw*P.qy - P.qz*P.qx);
  if Abs(sp) >= 1 then pitch := Sign(sp)*Pi/2 else pitch := ArcSin(sp);

  sy := 2*(P.qw*P.qz + P.qx*P.qy);
  cy := 1 - 2*(P.qy*P.qy + P.qz*P.qz);
  yaw := ArcTan2(sy, cy);

  C.RotationAngle.X := RadToDeg(-roll);
  C.RotationAngle.Y := RadToDeg( pitch);
  C.RotationAngle.Z := RadToDeg(-yaw);
end;

procedure TForm1.OnUITick(Sender: TObject);
var bp, lp: TPose;
begin
  if (CS=nil) or (PBase=nil) or (PLink=nil) then Exit;

  CS.Enter;
  try
    bp := PBase^;  lp := PLink^;
  finally
    CS.Leave;
  end;

  MapYFlipToCube(bp, CubeTorso);
  MapYFlipToCube(lp, CubeLeg);

  // optioneel: View.Repaint;
end;

end.
