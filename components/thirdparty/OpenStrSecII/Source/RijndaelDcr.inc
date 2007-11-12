{$IFDEF RIJNDAEL}
  mov EDI,EDX

  push EDI
  mov ESI,dword [EDI]
  mov ECX,dword [EDI+4]
  push ECX
  push ESI
  mov ECX,dword [EDI+8]
  mov EDX,dword [EDI+12]
  mov EDI,ECX

  lea ESI,dword ptr [EAX].TRijndael_ECB.drk
  push ESI

  mov EAX,dword [EAX].TRijndael_ECB.NumRounds
  dec EAX
  push EAX

@@Main:
  mov EAX,dword [ESP+8]
  mov EBX,dword [ESP+12]
  mov ECX,EDI

  xor EAX,dword [ESI]
  xor EBX,dword [ESI+4]
  xor ECX,dword [ESI+8]
  xor EDX,dword [ESI+12]

  ror ECX,16
  ror EBX,16
  movzx EBP,AL
  mov EDI,dword [Offset @T4 + EBP*4]
  movzx EBP,DH
  xor EDI,dword [Offset @T5 + EBP*4]
  movzx EBP,CL
  xor EDI,dword [Offset @T6 + EBP*4]
  movzx EBP,BH
  xor EDI,dword [Offset @T7 + EBP*4]
  mov dword [ESP+8],EDI

  ror EDX,16
  ror EBX,16
  movzx EBP,BL
  mov EDI,dword [Offset @T4 + EBP*4]
  movzx EBP,AH
  xor EDI,dword [Offset @T5 + EBP*4]
  movzx EBP,DL
  xor EDI,dword [Offset @T6 + EBP*4]
  movzx EBP,CH
  xor EDI,dword [Offset @T7 + EBP*4]
  mov dword [ESP+12],EDI

  shr ECX,16
  shr EAX,16
  movzx EBP,CL
  mov EDI,dword [Offset @T4 + EBP*4]
  movzx EBP,BH
  xor EDI,dword [Offset @T5 + EBP*4]
  movzx EBP,AL
  xor EDI,dword [Offset @T6 + EBP*4]
  movzx EBP,DH
  xor EDI,dword [Offset @T7 + EBP*4]

  shr EDX,16
  shr EBX,16
  movzx EBP,DL
  mov EDX,dword [Offset @T4 + EBP*4]
  movzx EBP,CH
  xor EDX,dword [Offset @T5 + EBP*4]
  movzx EBP,BL
  xor EDX,dword [Offset @T6 + EBP*4]
  movzx EBP,AH
  xor EDX,dword [Offset @T7 + EBP*4]

  add ESI,32
  dec dword [ESP]
  jnz @@Main

  mov dword [ESP+4],ESI

  mov EAX,dword [ESP+8]
  mov EBX,dword [ESP+12]
  mov ECX,EDI

  xor EAX,dword [ESI]
  xor EBX,dword [ESI+4]
  xor ECX,dword [ESI+8]
  xor EDX,dword [ESI+12]

  mov EDI,EAX
  ror ECX,16
  ror EBX,16
  ror EAX,16
  movzx EBP,CL
  mov AL,byte [Offset @S4 + EBP*4]
  movzx EBP,BH
  mov AH,byte [Offset @S4 + EBP*4]
  ror EAX,16
  movzx EBP,AL
  mov AL,byte [Offset @S4 + EBP*4]
  movzx EBP,DH
  mov AH,byte [Offset @S4 + EBP*4]
  mov [ESP+8],EAX
  mov EAX,EDI

  mov EDI,EBX
  ror EDX,16
  movzx EBP,DL
  mov BL,byte [Offset @S4 + EBP*4]
  movzx EBP,CH
  mov BH,byte [Offset @S4 + EBP*4]
  ror EBX,16
  movzx EBP,BL
  mov BL,byte [Offset @S4 + EBP*4]
  movzx EBP,AH
  mov BH,byte [Offset @S4 + EBP*4]
  mov dword [ESP+12],EBX
  mov EBX,EDI

  mov EDI,ECX
  ror EBX,16
  ror EAX,16
  movzx EBP,AL
  mov CL,byte [Offset @S4 + EBP*4]
  movzx EBP,DH
  mov CH,byte [Offset @S4 + EBP*4]
  ror ECX,16
  movzx EBP,CL
  mov CL,byte [Offset @S4 + EBP*4]
  movzx EBP,BH
  mov CH,byte [Offset @S4 + EBP*4]
  xchg EDI,ECX

  ror ECX,16
  ror EBX,16
  movzx EBP,BL
  mov DL,byte [Offset @S4 + EBP*4]
  movzx EBP,AH
  mov DH,byte [Offset @S4 + EBP*4]
  ror EDX,16
  movzx EBP,DL
  mov DL,byte [Offset @S4 + EBP*4]
  movzx EBP,CH
  mov DH,byte [Offset @S4 + EBP*4]

  mov EAX,dword [ESP+8]
  mov EBX,dword [ESP+12]
  mov ECX,EDI

  xor EAX,dword [ESI+32]
  xor EBX,dword [ESI+36]
  xor ECX,dword [ESI+40]
  xor EDX,dword [ESI+44]
  mov EDI,dword [ESP+16]
  mov dword [EDI],EAX
  mov dword [EDI+4],EBX
  mov dword [EDI+8],ECX
  mov dword [EDI+12],EDX

  mov dword [ESP+8],0
  mov dword [ESP+12],0
  add ESP,20
{$ELSE}
{$ENDIF}
