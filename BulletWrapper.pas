unit BulletWrapper;

interface

{$IFDEF MSWINDOWS}
  {$IFDEF WIN64}
    {$DEFINE BW_X64}
  {$ELSE}
    {$MESSAGE WARN 'Deze unit is bedoeld voor Win64.'}
  {$ENDIF}
{$ENDIF}

type
  TBWWorld = Pointer;
  TBWBody  = Pointer;
  TBWHinge = Pointer;

{ Wereld }
function BW_CreateWorld: TBWWorld; cdecl; external 'BulletWrapper.dll';
procedure BW_DestroyWorld(w: TBWWorld); cdecl; external 'BulletWrapper.dll';
procedure BW_SetGravity(w: TBWWorld; gx, gy, gz: Double); cdecl; external 'BulletWrapper.dll';
procedure BW_Step(w: TBWWorld; dt: Double; substeps: LongInt); cdecl; external 'BulletWrapper.dll';

{ Bodies }
function BW_CreateBox(w: TBWWorld;
  sx, sy, sz: Double;              // afmetingen (meter)
  mass: Double;                    // massa (kg)
  px, py, pz: Double               // beginpositie (meter)
): TBWBody; cdecl; external 'BulletWrapper.dll';

procedure BW_GetBodyTransform(b: TBWBody;
  var x, y, z: Double;             // positie
  var qx, qy, qz, qw: Double       // quaternion (x,y,z,w)
); cdecl; external 'BulletWrapper.dll';

{ Hinges (scharnieren) }
function BW_CreateHinge(w: TBWWorld; aBody, bBody: TBWBody;
  pax, pay, paz: Double;           // pivot A (wereld)
  pbx, pby, pbz: Double;           // pivot B (wereld)
  ax,  ay,  az:  Double;           // as (wereld, genormaliseerd)
  limLow, limHigh: Double;         // limieten (radianen)
  enableMotor: LongInt;            // 0/1
  targetSpeed, maxTorque: Double   // rad/s, Nm
): TBWHinge; cdecl; external 'BulletWrapper.dll';

procedure BW_SetHingeMotor(h: TBWHinge; enable: LongInt; speed, maxTorque: Double);
  cdecl; external 'BulletWrapper.dll';

implementation
end.