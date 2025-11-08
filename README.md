# 🧠 BulletWrapper.cpp — Delphi Interface Layer for Bullet Physics (Featherstone)

This project provides a C++ → Delphi interface (DLL) for the **Bullet 3 Physics Engine**,
including Featherstone multibody dynamics, motors, constraints, and colliders.  
It allows native **Delphi / FreePascal applications** (FMX or VCL) to run full Bullet-based
physics simulations in real time.

---

## 🚀 MinimalTorso_Test (Multithreaded Featherstone Demo)

**MinimalTorso_Test** demonstrates a complete multibody setup using this interface layer.  
It runs a physics thread in parallel with an FMX visualization at 60 FPS.

### Features
- 🧩 **Featherstone multibody** (Bullet 3)
- 🧠 **Separate physics thread** (`TBulletThread`)
- 🏗️ **Free base (non-fixed)** with active gravity and collisions
- 🔄 **Revolute joint** for a single leg (Z-axis hinge)
- ⚙️ **Motor control** via velocity target (`SetVelocityMotor`)
- 🎯 **Joint limits** `[-30°, +45°]` using `btMultiBodyJointLimitConstraint`
- 🟦 **Independent colliders** for base and link
- 🪄 **FMX visualization** with Y-flip correction and shared `TPose`
- 🧵 **Smooth multithread synchronization** via `TCriticalSection`

---

## 🧱 Project Structure

/src_cpp/ ├─ BulletWrapper.cpp          ← C++ interface layer (DLL source) ├─ CMakeLists.txt             ← build script for Visual Studio / CMake /pascal/ ├─ BulletWrapper.pas          ← Pascal header (DLL import) ├─ Unit1_Physics_Thread_MinimalTorso.pas ├─ Unit2_Featherstone_MinimalTorso_Final.pas └─ Bullet_MinimalTorso_Test_MULTITHREAD.dpr /dll/ └─ BulletWrapper.dll          ← prebuilt 64-bit DLL

---

## 🧩 How It Works

1. **BulletWrapper.dll**  
   The DLL exports a minimal, flat API:
   ```pascal
   function BW_MB_CreateWorld: Pointer; cdecl;
   procedure BW_MB_SetGravity(W: Pointer; gx, gy, gz: Double); cdecl;
   function BW_MB_CreateBaseBox(W: Pointer; ...): Pointer; cdecl;
   function BW_MB_AddRevoluteLinkBox(W, MB: Pointer; ...): Integer; cdecl;
   procedure BW_MB_FinalizeAndAdd(W, MB: Pointer); cdecl;

Written in C++, compiled with CMake + MSVC.

2. Delphi / Pascal Front-end
The physics thread (TBulletThread) runs Bullet simulation steps and updates shared poses (TPose records) under a mutex.


3. FMX Visualization
The main form reads the poses at ~60 Hz and updates two cubes:

Blue = torso

Yellow = leg
Both Y-flipped for Bullet → FMX coordinate conversion.





---

🧰 Requirements

Delphi 10+ or FreePascal/Lazarus (FMX compatible)

Visual Studio 2022 (for DLL build via CMake)

Bullet Physics 3.x (Featherstone multibody modules)

Windows 64-bit



---

🧪 Building the DLL

1. Open a Developer Command Prompt for VS.


2. Navigate to /src_cpp/.


3. Run:

cmake -S . -B build -G "Visual Studio 17 2022" -A x64

cmake --build build --config Release


4. Copy the generated BulletWrapper.dll to /dll/ or next to your .exe.




---

🧵 Running the Demo

Open Bullet_MinimalTorso_Test_MULTITHREAD.dproj in Delphi.

Build & run → a blue torso with a yellow leg appears.

The leg swings within joint limits and the torso falls and collides with the floor.



---

⚖️ License

MIT License

Copyright (c) 2025 Roelof P. Emmerink

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction...

(see LICENSE file for full text)


---

💬 Credits

Roelof P. Emmerink — main developer

Bullet Physics by Erwin Coumans — core physics engine



---

🌟 Next Steps

Add second joint (knee)

Implement torque-based motor control

Integrate Hexapod-VR demo using this engine
