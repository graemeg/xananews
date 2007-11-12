{$IFDEF RIJNDAEL}
  push EDX

  mov ECX,EAX
  lea EBP,dword ptr [ECX].TRijndael_ECB.rk

  mov EAX,dword [EDX]
  mov EBX,dword [EDX+4]
  mov EDI,dword [EDX+8]
  mov ESI,dword [EDX+12]
  xor EAX,[EBP]        
  xor EBX,[EBP+4]
  xor EDI,[EBP+8]
  xor ESI,[EBP+12]

{$IFNDEF USE_MMX}
  sub ESP,8
{$ENDIF}

  mov ECX,dword [ECX].TRijndael_ECB.NumRounds
//  dec ECX
  push ECX
  cmp ECX,14
  je @@NR14b
  sub EBP,64
  cmp ECX,12
  je @@NR12b
  sub EBP,64
  jmp @@NR10b

@@NR14b:
{$I RijndaelEncRound0.inc}
{$I RijndaelEncRound1.inc}
@@NR12b:
{$I RijndaelEncRound2.inc}
{$I RijndaelEncRound3.inc}
@@NR10b:
{$I RijndaelEncRound4.inc}
{$I RijndaelEncRound5.inc}
{$I RijndaelEncRound6.inc}
{$I RijndaelEncRound7.inc}
{$I RijndaelEncRound8.inc}
{$I RijndaelEncRound9.inc}
{$I RijndaelEncRound10.inc}
{$I RijndaelEncRound11.inc}
{$I RijndaelEncRound12.inc}
// ----
  mov ECX,EAX
{$IFDEF USE_MMX}
  movd mm0,EBX
  movd mm1,ESI
{$ELSE}
  mov dword [ESP+4],EBX
  mov dword [ESP+8],ESI
{$ENDIF}

  mov EAX,dword [EBP + 448]
  movzx EDX,CL
  mov ESI,dword [EBP + 448 + 12]
  xor EAX,dword [4*EDX + S0]
  mov EBX,dword [EBP + 448 + 4]
  movzx EDX,CH
  shr ECX,16
  xor ESI,dword [4*EDX + S0 + 1024]
  movzx EDX,CL
  movzx ECX,CH
  xor EBX,dword [4*ECX + S0 + 3072]
  mov ECX,EDI
  mov EDI,dword [EBP + 448 + 8]
  xor EDI,dword [4*EDX + S0 + 2048]

  movzx EDX,CL
  xor EDI,dword [EDX*4 + S0]
  movzx EDX,CH
  shr ECX,16
  xor EBX,dword [EDX*4 + S0 + 1024]
  movzx EDX,CL
  movzx ECX,CH
  xor EAX,dword [EDX*4 + S0 + 2048]
  xor ESI,dword [ECX*4 + S0 + 3072]

{$IFDEF USE_MMX}
  movd ECX,mm0
{$ELSE}
  mov ECX,dword [ESP+4]
{$ENDIF}

  movzx EDX,CL
  xor EBX,dword [EDX*4 + S0]
  movzx EDX,CH
  shr ECX,16
  xor EAX,dword [EDX*4 + S0 + 1024]
  movzx EDX,CL
  movzx ECX,CH
  xor ESI,dword [EDX*4 + S0 + 2048]
  xor EDI,dword [ECX*4 + S0 + 3072]

{$IFDEF USE_MMX}
  movd ECX,mm1
{$ELSE}
  mov ECX,dword [ESP+8]
{$ENDIF}

  movzx EDX,CL
  xor ESI,dword [EDX*4 + S0]
  movzx EDX,CH
  shr ECX,16
  xor EDI,dword [EDX*4 + S0 + 1024]
  movzx EDX,CL
  movzx ECX,CH
  xor EBX,dword [EDX*4 + S0 + 2048]
  xor EAX,dword [ECX*4 + S0 + 3072]

// ----

{$IFDEF USE_MMX}
  mov EBP,dword [ESP+4]
{$ELSE}
  mov EBP,dword [ESP+12]
{$ENDIF}
  mov dword [EBP],EAX
  mov dword [EBP+4],EBX
  mov dword [EBP+8],EDI
  mov dword [EBP+12],ESI

{$IFDEF USE_MMX}
  add ESP,8
{$ELSE}
  mov dword [ESP+4],0
  mov dword [ESP+8],0
  add ESP,16
{$ENDIF}
{$IFDEF USE_EMMX}
  emms
{$ENDIF}
{$ELSE}
{$ENDIF}
