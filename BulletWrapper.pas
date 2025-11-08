unit BulletWrapper;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

const
  // Zet hier de naam van jouw DLL neer
  DLL_NAME = 'BulletWrapper.dll';

type
  { Klassieke Bullet 2 (rigid-body) }
  TBWWorld = Pointer;
  TBWBody  = Pointer;
  TBWHinge = Pointer;

  { Bullet 2 MultiBody (Featherstone) }
  TBWMBWorld = Pointer;
  TBWMBBody  = Pointer;
  TBWMBCons  = Pointer;

{==============================================================================
  KLASSIEK: BW_*  – btDiscreteDynamicsWorld
==============================================================================}
{ Wereldbeheer }
function  BW_CreateWorld: TBWWorld; cdecl; external DLL_NAME;
procedure BW_DestroyWorld(W: TBWWorld); cdecl; external DLL_NAME;
procedure BW_SetGravity(W: TBWWorld; gx, gy, gz: Double); cdecl; external DLL_NAME;
procedure BW_Step(W: TBWWorld; dt: Double; substeps: LongInt); cdecl; external DLL_NAME;

{ Solver tuning (gevraagd) }
procedure BW_TuneSolver(W: TBWWorld; iters: LongInt; splitImpulse: LongInt;
  splitThresh, erp: Double); cdecl; external DLL_NAME;

{ Bodies / ground }
function  BW_CreateBox(W: TBWWorld;
  sx, sy, sz, mass, px, py, pz: Double): TBWBody; cdecl; external DLL_NAME;
function  BW_CreateGroundPlane(W: TBWWorld; planeY: Double): TBWBody; cdecl; external DLL_NAME;

{ Transforms }
procedure BW_GetBodyTransform(B: TBWBody;
  var x, y, z, qx, qy, qz, qw: Double); cdecl; external DLL_NAME;

{ Hinges / constraints }
function  BW_CreateHingeAtWorldPivot(W: TBWWorld; bodyA, bodyB: TBWBody;
  px, py, pz, ax, ay, az, low, high: Double;
  enableMotor: LongInt; targetSpeed, maxImpulse: Double): TBWHinge; cdecl; external DLL_NAME;

function  BW_CreateHingeLocal(W: TBWWorld; bodyA, bodyB: TBWBody;
  axA, ayA, azA, axB, ayB, azB: Double;
  axisWx, axisWy, axisWz: Double;
  limLow, limHigh: Double;
  enableMotor: LongInt; targetSpeed, maxTorque: Double): TBWHinge; cdecl; external DLL_NAME;

procedure BW_SetHingeMotor(H: TBWHinge; enable: LongInt; speed, maxTorque: Double); cdecl; external DLL_NAME;
procedure BW_SetHingeMotorSpeed(H: TBWHinge; speed: Double); cdecl; external DLL_NAME;
procedure BW_SetHingeMaxTorque(H: TBWHinge; maxTorque: Double); cdecl; external DLL_NAME;

procedure BW_SetHingeLimit(H: TBWHinge;
  lower, upper, softness, bias, relaxation: Double); cdecl; external DLL_NAME;

procedure BW_SetHingeLimitParams(H: TBWHinge;
  softness, bias, relaxation: Double); cdecl; external DLL_NAME;

{ Helpers – rigid body properties/motion }
procedure BW_SetDamping(B: TBWBody; lin, ang: Double); cdecl; external DLL_NAME;
procedure BW_SetFriction(B: TBWBody; mu: Double); cdecl; external DLL_NAME;
procedure BW_SetRestitution(B: TBWBody; e: Double); cdecl; external DLL_NAME;

procedure BW_SetLinearVelocity(B: TBWBody; vx, vy, vz: Double); cdecl; external DLL_NAME;
procedure BW_SetAngularVelocity(B: TBWBody; wx, wy, wz: Double); cdecl; external DLL_NAME;

procedure BW_ApplyCentralImpulse(B: TBWBody; ix, iy, iz: Double); cdecl; external DLL_NAME;
procedure BW_ApplyTorqueImpulse(B: TBWBody; tx, ty, tz: Double); cdecl; external DLL_NAME;

procedure BW_SetActivation(B: TBWBody; state: LongInt); cdecl; external DLL_NAME;
procedure BW_SetSleepingThresholds(B: TBWBody; lin, ang: Double); cdecl; external DLL_NAME;
procedure BW_SetKinematic(B: TBWBody; enable: LongInt); cdecl; external DLL_NAME;

{==============================================================================
  FEATHERSTONE: BW_MB_*  – MultiBody (btMultiBody / btMultiBodyDynamicsWorld)
==============================================================================}
{ Wereldbeheer }
function  BW_MB_CreateWorld: TBWMBWorld; cdecl; external DLL_NAME;
procedure BW_MB_DestroyWorld(W: TBWMBWorld); cdecl; external DLL_NAME;
procedure BW_MB_SetGravity(W: TBWMBWorld; gx, gy, gz: Double); cdecl; external DLL_NAME;
procedure BW_MB_Step(W: TBWMBWorld; dt: Double; substeps: LongInt); cdecl; external DLL_NAME;

{ Floor / collision helper }
function  BW_MB_CreateGroundBox(W: TBWMBWorld;
  sx, sy, sz, px, py, pz: Double): Pointer; cdecl; external DLL_NAME;

{ Base (torso) – standaard 1 link-capaciteit in de C++ implementatie }
function  BW_MB_CreateBaseBox(W: TBWMBWorld;
  hx, hy, hz, mass, px, py, pz: Double; fixedBase: LongInt): TBWMBBody; cdecl; external DLL_NAME;

{ Varianten (optioneel – alleen gebruiken als ze in jouw DLL zitten) }
function  BW_MB_CreateBaseBox_NoWorld(W: TBWMBWorld;
  hx, hy, hz, mass, px, py, pz: Double; fixedBase: LongInt): TBWMBBody; cdecl; external DLL_NAME;

function  BW_MB_CreateBaseBox_WithLinks(W: TBWMBWorld;
  hx, hy, hz, mass, px, py, pz: Double; fixedBase: LongInt; linkCount: LongInt): TBWMBBody; cdecl; external DLL_NAME;

procedure BW_MB_FinalizeAndAdd(W, MB: TBWMBWorld); cdecl; external DLL_NAME;
procedure BW_MB_AddMultiBodyToWorld(W, MB: TBWMBWorld); cdecl; external DLL_NAME;

procedure BW_MB_AddBaseColliderBox(W: Pointer; MB: Pointer;
  hx, hy, hz: Double); cdecl; external 'BulletWrapper.dll';

procedure BW_MB_AddLinkColliderBox(W: Pointer; MB: Pointer; linkIndex: LongInt;
  hx, hy, hz: Double); cdecl; external 'BulletWrapper.dll';

{ Links (hinges) }
function  BW_MB_AddRevoluteLinkBox(W: TBWMBWorld; MB: TBWMBBody; parent: LongInt;
  hx, hy, hz, mass: Double;
  jx, jy, jz: Double;
  rpx, rpy, rpz: Double;
  rcx, rcy, rcz: Double): LongInt; cdecl; external DLL_NAME;

// Alternatief: directe index-variant (als je die export in je DLL hebt)
function  BW_MB_SetupRevoluteLinkAt(W, MB: TBWMBWorld;
  linkIndex, parentIndex: LongInt;
  hx, hy, hz, mass: Double;
  jx, jy, jz: Double;
  rpx, rpy, rpz: Double;
  rcx, rcy, rcz: Double): LongInt; cdecl; external DLL_NAME;

function  BW_MB_AddLimit(W, MB: Pointer; linkIndex: LongInt;
  lo, hi: Double): Pointer; cdecl; external 'BulletWrapper.dll';

function  BW_MB_ReplaceLimit(W, MB, oldLimit: Pointer; linkIndex: LongInt;
  lo, hi: Double): Pointer; cdecl; external 'BulletWrapper.dll';

procedure BW_MB_RemoveConstraint(W, constraint: Pointer); cdecl; external 'BulletWrapper.dll';

{ Motors }
function  BW_MB_AddMotor(W: TBWMBWorld; MB: TBWMBBody; linkIndex: LongInt;
  targetVel, maxImpulse: Double): TBWMBCons; cdecl; external DLL_NAME;

procedure BW_MB_SetVelocityMotor(motor: TBWMBCons;
  targetVel, maxImpulse, erp: Double); cdecl; external DLL_NAME;

{ (optioneel) Position motor – alleen opnemen als jouw DLL deze exports heeft }
function  BW_MB_AddPositionMotor(W: TBWMBWorld; MB: TBWMBBody; linkIndex: LongInt;
  posTarget, kp, kd, maxImpulse: Double): TBWMBCons; cdecl; external DLL_NAME;

procedure BW_MB_SetPositionMotor(motor: TBWMBCons;
  posTarget, kp, kd, maxImpulse: Double); cdecl; external DLL_NAME;

{ Limits / constraints – alleen gebruiken als je ze in de DLL hebt }
//function  BW_MB_AddLimit(W: TBWMBWorld; MB: TBWMBBody; linkIndex: LongInt;
//  lo, hi: Double): TBWMBCons; cdecl; external DLL_NAME;

procedure BW_MB_SetLimit(limitConstraint: TBWMBCons; lo, hi: Double); cdecl; external DLL_NAME;

//function  BW_MB_ReplaceLimit(W, MB, oldLimit: Pointer; linkIndex: LongInt;
//  lo, hi: Double): TBWMBCons; cdecl; external DLL_NAME;

//procedure BW_MB_RemoveConstraint(W: TBWMBWorld; constraint: TBWMBCons); cdecl; external DLL_NAME;

{ State / pose }
procedure BW_MB_GetBaseTransform(MB: TBWMBBody;
  var x, y, z, qx, qy, qz, qw: Double); cdecl; external DLL_NAME;

procedure BW_MB_GetLinkTransform(MB: TBWMBBody; linkIndex: LongInt;
  var x, y, z, qx, qy, qz, qw: Double); cdecl; external DLL_NAME;

procedure BW_MB_GetJointState(MB: TBWMBBody; linkIndex: LongInt;
  var angle, velocity: Double); cdecl; external DLL_NAME;

implementation
end.
