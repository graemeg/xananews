{$IFDEF RIJNDAEL}
  mov EDI,EDX

  mov ESI,[EDI]
  mov EBX,[EDI+4]
  mov ECX,[EDI+8]
  mov EDX,[EDI+12]
  push EDI
  push EBX
  push ESI
  mov EDI,ECX

  lea ESI,dword ptr [EAX].TRijndael_ECB.rk
  push ESI

  mov EAX,[EAX].TRijndael_ECB.NumRounds
  dec EAX
  push EAX

@@Main:
  mov EAX,[ESP+8]
  mov EBX,[ESP+12]
  mov ECX,EDI

  xor EAX,[ESI]
  xor EBX,[ESI+4]
  xor ECX,[ESI+8]
  xor EDX,[ESI+12]

  ror ECX,16
  ror EDX,16
  movzx EBP,AL
  mov EDI,dword [Offset @T0 + EBP*4]
  movzx EBP,BH
  xor EDI,dword [Offset @T1 + EBP*4]
  movzx EBP,CL
  xor EDI,dword [Offset @T2 + EBP*4]
  movzx EBP,DH
  xor EDI,dword [Offset @T3 + EBP*4]
  mov [ESP+8],EDI

  ror EAX,16
  ror ECX,16
  movzx EBP,BL
  mov EDI,dword [Offset @T0 + EBP*4]
  movzx EBP,CH
  xor EDI,dword [Offset @T1 + EBP*4]
  movzx EBP,DL
  xor EDI,dword [Offset @T2 + EBP*4]
  movzx EBP,AH
  xor EDI,dword [Offset @T3 + EBP*4]
  mov [ESP+12],EDI

  shr EBX,16
  shr EDX,16
  movzx EBP,CL
  mov EDI,dword [Offset @T0 + EBP*4]
  movzx EBP,DH
  xor EDI,dword [Offset @T1 + EBP*4]
  movzx EBP,AL
  xor EDI,dword [Offset @T2 + EBP*4]
  movzx EBP,BH
  xor EDI,dword [Offset @T3 + EBP*4]

  shr ECX,16
  shr EAX,16
  movzx EBP,DL
  mov EDX,dword [Offset @T0 + EBP*4]
  movzx EBP,AH
  xor EDX,dword [Offset @T1 + EBP*4]
  movzx EBP,BL
  xor EDX,dword [Offset @T2 + EBP*4]
  movzx EBP,CH
  xor EDX,dword [Offset @T3 + EBP*4]

  add ESI,32
  dec dword [ESP]
  jnz @@Main

  mov [ESP+4],ESI

  mov EAX,[ESP+8]
  mov EBX,[ESP+12]
  mov ECX,EDI

  xor EAX,[ESI]
  xor EBX,[ESI+4]
  xor ECX,[ESI+8]
  xor EDX,[ESI+12]

  mov EDI,EAX
  ror ECX,16
  ror EDX,16
  ror EAX,16
  movzx EBP,CL
  mov AL,byte [Offset @S0 + EBP*4]
  movzx EBP,DH
  mov AH,byte [Offset @S0 + EBP*4]
  ror EAX,16
  movzx EBP,AL
  mov AL,byte [Offset @S0 + EBP*4]
  movzx EBP,BH
  mov AH,byte [Offset @S0 + EBP*4]
  mov [ESP+8],EAX
  mov EAX,EDI

  mov EDI,EDX
  ror EBX,16
  movzx EBP,BL
  mov DL,byte [Offset @S0 + EBP*4]
  movzx EBP,CH
  mov DH,byte [Offset @S0 + EBP*4]
  ror EDX,16
  movzx EBP,DL
  mov DL,byte [Offset @S0 + EBP*4]
  movzx EBP,AH
  mov DH,byte [Offset @S0 + EBP*4]
  mov [ESP+12],EDX
  mov EDX,EDI

  mov EDI,ECX
  shr EAX,16
  ror EDX,16
  movzx EBP,AL
  mov CL,byte [Offset @S0 + EBP*4]
  movzx EBP,BH
  mov CH,byte [Offset @S0 + EBP*4]
  ror ECX,16
  movzx EBP,CL
  mov CL,byte [Offset @S0 + EBP*4]
  movzx EBP,DH
  mov CH,byte [Offset @S0 + EBP*4]
  xchg EDI,ECX

  shr ECX,16
  shr EDX,16
  movzx EBP,DL
  mov BL,byte [Offset @S0 + EBP*4]
  movzx EBP,AH
  mov BH,byte [Offset @S0 + EBP*4]
  ror EBX,16
  movzx EBP,BL
  mov BL,byte [Offset @S0 + EBP*4]
  movzx EBP,CH
  mov BH,byte [Offset @S0 + EBP*4]

  mov EAX,[ESP+8]
  mov EDX,[ESP+12]
  mov ECX,EDI

  xor EAX,[ESI+32]
  xor EBX,[ESI+36]
  xor ECX,[ESI+40]
  xor EDX,[ESI+44]
  mov EDI,[ESP+16]
  mov [EDI],EAX
  mov [EDI+4],EBX
  mov [EDI+8],ECX
  mov [EDI+12],EDX
             
  mov dword [ESP+8],0
  mov dword [ESP+12],0
  add ESP,20
{$ELSE}
{$ENDIF}

