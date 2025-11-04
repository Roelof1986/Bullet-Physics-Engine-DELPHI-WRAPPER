unit Unit1;

interface
uses
  System.SysUtils, System.Types, System.Math, System.Math.Vectors, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Colors, FMX.Dialogs,
  FMX.Viewport3D, FMX.Controls3D, FMX.Objects3D, FMX.Types3D, FMX.MaterialSources,
  BulletWrapper;
type
  TForm1 = class(TForm)
  private
    // 3D scene
    Viewport3D : TViewport3D;
    Camera     : TCamera;
    Light      : TLight;
    FloorCube  : TCube;
    BoxCube    : TCube;
    Timer      : TTimer;
    MatGrass   : TColorMaterialSource;
    MatBox     : TColorMaterialSource;
    // Bullet handles
    W          : TBWWorld;
    Ground     : TBWBody;
    Box        : TBWBody;
    procedure OnTimer(Sender: TObject);
    procedure ApplyPose(const B: TBWBody; Cube: TCube);
  public
    procedure InitializeScene;      // roepen na CreateNew(nil) of vanuit FormCreate
    destructor Destroy; override;
  end;
var
  Form1: TForm1;
implementation
{ $R *.fmx } // laat staan als je een .fmx gebruikt; anders mag je deze regel uitcommenten
// --------- Scene opbouwen ----------
procedure TForm1.InitializeScene;
begin
  // Venster basis
  Caption := 'Bullet 3D – gras, lucht en kubus';
  Width := 1024; Height := 720;
  Position := TFormPosition.poScreenCenter;
  // Viewport + "lucht" (achtergrond)
  Viewport3D := TViewport3D.Create(Self);
  Viewport3D.Parent := Self;
  Viewport3D.Align := TAlignLayout.Contents;
  Viewport3D.Color := {TAlphaColorRec.Skyblue}$FF87CEEB;   // lucht-blauw
  Viewport3D.UsingDesignCamera := False;
  // Camera en licht
  Camera := TCamera.Create(Self);
  Camera.Parent := Viewport3D;
  Camera.Position.Point := Point3D(1, 1.6, 4.0); // iets hoger/achter
  Camera.RotationAngle.X := -40;                 // kijk iets naar beneden
  Light := TLight.Create(Self);
  Light.Parent := Viewport3D;
  Light.LightType := TLightType.Point;
  Light.Position.Point := Point3D(1.5, 2.0, 1.5);
  // Materialen
  MatGrass := TColorMaterialSource.Create(Self);
  MatGrass.Color := $FF2FAF2F; // fris groen
  MatBox := TColorMaterialSource.Create(Self);
  MatBox.Color := $FFCC3333;   // roodachtige kubus
  // Grasveld (groot vlak)
  FloorCube := TCube.Create(Self);
  FloorCube.Parent := Viewport3D;
  FloorCube.MaterialSource := MatGrass;
  FloorCube.Width  := 20.0;   // 20×20 meter
  FloorCube.Depth  := 20.0;
  FloorCube.Height := 0.2;    // dun
//  FloorCube.Position.Y := {0.12}0.20; // bovenzijde ~ y=0 (kleine offset)
  FloorCube.Position.Point := Point3D(0.0, 0.20, 0.0); // bovenzijde ~ y=0 (kleine offset)
  // Kubus (0.5 m) – staat op de grond
  BoxCube := TCube.Create(Self);
  BoxCube.Parent := Viewport3D;
  BoxCube.MaterialSource := MatBox;
  BoxCube.Width := 0.8; BoxCube.Height := 0.8; BoxCube.Depth := 0.8;
  // --------- Bullet wereld ----------
  W := BW_CreateWorld;
  BW_SetGravity(W, 0, -9.81, 0);
  // Statische grond (massa 0) – precies gelijk aan FloorCube
  Ground := BW_CreateBox(W, 20.0, 0.2, 20.0, 0.0, 0, -0.00, 0);
  // Dynamische kubus – zet hem net boven het vlak: y = halfhoogte = 0.25
  Box := BW_CreateBox(W, 0.5, 0.5, 0.5, 1.0, 10{=X!}, 10.25{=Y!}, 0);
  // Timer ~60 FPS → 4× substep 1/240 s
  Timer := TTimer.Create(Self);
  Timer.Interval := 16;
  Timer.OnTimer := OnTimer;
  Timer.Enabled := True;
end;
destructor TForm1.Destroy;
begin
  if Assigned(Timer) then Timer.Enabled := False;
  if W <> nil then
    BW_DestroyWorld(W);
  inherited;
end;
// --------- Sim loop ----------
procedure TForm1.OnTimer(Sender: TObject);
var
  i: Integer;
begin
  for i := 1 to 4 do
    BW_Step(W, 1/240, 1);
  ApplyPose(Box, BoxCube);
  // ground is statisch; visuele FloorCube staat al vast
end;
// --------- Pose toepassen ----------
(*procedure TForm1.ApplyPose(const B: TBWBody; Cube: TCube);
var
  x,y,z,qx,qy,qz,qw: Double;
  sinr_cosp, cosr_cosp, roll : Double;
  sinp,       pitch          : Double;
  siny_cosp, cosy_cosp, yaw  : Double;
begin
  BW_GetBodyTransform(B, x,y,z, qx,qy,qz,qw);
  Cube.Position.Point := Point3D(x, y, z);
  // Quaternion -> Euler (roll/pitch/yaw)
  // roll (X)
  sinr_cosp := 2 * (qw*qx + qy*qz);
  cosr_cosp := 1 - 2 * (qx*qx + qy*qy);
  roll := ArcTan2(sinr_cosp, cosr_cosp);
  // pitch (Y)
  sinp := 2 * (qw*qy - qz*qx);
  if Abs(sinp) >= 1 then pitch := Sign(sinp) * Pi/2 else pitch := ArcSin(sinp);
  // yaw (Z)
  siny_cosp := 2 * (qw*qz + qx*qy);
  cosy_cosp := 1 - 2 * (qy*qy + qz*qz);
  yaw := ArcTan2(siny_cosp, cosy_cosp);
  Cube.RotationAngle.X := RadToDeg(roll);
  Cube.RotationAngle.Y := RadToDeg(pitch);
  Cube.RotationAngle.Z := RadToDeg(yaw);
end; *)
procedure TForm1.ApplyPose(const B: TBWBody; Cube: TCube);
var
  x,y,z,qx,qy,qz,qw: Double;
  // Euler (roll, pitch, yaw)
  sinr_cosp, cosr_cosp, roll : Double;
  sinp,       pitch          : Double;
  siny_cosp, cosy_cosp, yaw  : Double;
begin
  // 1) Transform ophalen uit Bullet
  BW_GetBodyTransform(B, x,y,z, qx,qy,qz,qw);
  // 2) Bullet → FMX conversie: Y-as omdraaien (Bullet +Y omhoog, FMX getekend omgekeerd)
  Cube.Position.Point := Point3D(x, -y, z);
  // 3) Quaternion → Euler (roll/pitch/yaw)
  // roll (X)
  sinr_cosp := 2 * (qw*qx + qy*qz);
  cosr_cosp := 1 - 2 * (qx*qx + qy*qy);
  roll := ArcTan2(sinr_cosp, cosr_cosp);
  // pitch (Y)
  sinp := 2 * (qw*qy - qz*qx);
  if Abs(sinp) >= 1
    then pitch := Sign(sinp) * Pi/2
    else pitch := ArcSin(sinp);
  // yaw (Z)
  siny_cosp := 2 * (qw*qz + qx*qy);
  cosy_cosp := 1 - 2 * (qy*qy + qz*qz);
  yaw := ArcTan2(siny_cosp, cosy_cosp);
  // 4) Rotatie toepassen – X en Z spiegelen omdat we Y hebben omgedraaid
  Cube.RotationAngle.X := RadToDeg(-roll);
  Cube.RotationAngle.Y := RadToDeg(pitch);
  Cube.RotationAngle.Z := RadToDeg(-yaw);
end;
end.
