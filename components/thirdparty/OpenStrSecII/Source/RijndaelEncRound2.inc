  mov ECX,EAX
{$IFDEF USE_MMX}
  movd mm0,EBX
  movd mm1,ESI
{$ELSE}
  mov dword [ESP+4],EBX
  mov dword [ESP+8],ESI
{$ENDIF}

  mov EAX,dword [EBP + 96]
  movzx EDX,CL
  mov ESI,dword [EBP + 96 + 12]
  xor EAX,dword [4*EDX + T0]
  mov EBX,dword [EBP + 96 + 4]
  movzx EDX,CH
  shr ECX,16
  xor ESI,dword [4*EDX + T0 + 1024]
  movzx EDX,CL
  movzx ECX,CH
  xor EBX,dword [4*ECX + T0 + 3072]
  mov ECX,EDI
  mov EDI,dword [EBP + 96 + 8]
  xor EDI,dword [4*EDX + T0 + 2048]

  movzx EDX,CL
  xor EDI,dword [EDX*4 + T0]
  movzx EDX,CH
  shr ECX,16
  xor EBX,dword [EDX*4 + T0 + 1024]
  movzx EDX,CL
  movzx ECX,CH
  xor EAX,dword [EDX*4 + T0 + 2048]
  xor ESI,dword [ECX*4 + T0 + 3072]

{$IFDEF USE_MMX}
  movd ECX,mm0
{$ELSE}
  mov ECX,dword [ESP+4]
{$ENDIF}

  movzx EDX,CL
  xor EBX,dword [EDX*4 + T0]
  movzx EDX,CH
  shr ECX,16
  xor EAX,dword [EDX*4 + T0 + 1024]
  movzx EDX,CL
  movzx ECX,CH
  xor ESI,dword [EDX*4 + T0 + 2048]
  xor EDI,dword [ECX*4 + T0 + 3072]

{$IFDEF USE_MMX}
  movd ECX,mm1
{$ELSE}
  mov ECX,dword [ESP+8]
{$ENDIF}

  movzx EDX,CL
  xor ESI,dword [EDX*4 + T0]
  movzx EDX,CH
  shr ECX,16
  xor EDI,dword [EDX*4 + T0 + 1024]
  movzx EDX,CL
  movzx ECX,CH
  xor EBX,dword [EDX*4 + T0 + 2048]
  xor EAX,dword [ECX*4 + T0 + 3072]
