{$IFDEF TWOFISH}
//  Data := @Buf;
//  Box := @FBox;
//  S := @FSubKey;
  mov EDI,EDX
  lea EBP,dword ptr [EAX].TTwoFish_ECB.FBox
  lea ESI,dword ptr [EAX].TTwoFish_ECB.FSubKey

//  M.A.L := PIntArray(Data)[0] xor S[0];
//  M.B.L := PIntArray(Data)[1] xor S[1];
//  M.C.L := PIntArray(Data)[2] xor S[2];
//  M.D.L := PIntArray(Data)[3] xor S[3];
  mov EBX,[ESI]
  xor [EDI],EBX
  mov EBX,[ESI+4]
  xor [EDI+4],EBX
  mov EBX,[ESI+8]
  xor [EDI+8],EBX
  mov EBX,[ESI+12]
  xor [EDI+12],EBX

  mov EAX,[EDI]
  mov EBX,[EDI + 4]

//  S   := @FSubKey[8];
  lea ESI,[ESI + 32]
//  for I := 0 to 7 do begin
  mov ECX,8
  push ECX
@@TwoFishEncMain2:
//  X := Box[0, M.A.A] xor Box[1, M.A.B] xor Box[2, M.A.C] xor Box[3, M.A.D];
//  mov EAX,[EDI]
  movzx EDX,AL
  mov ECX,[EBP        + EDX*4]
  movzx EDX,AH
  xor ECX,[EBP + 1024 + EDX*4]
  shr EAX,16
  movzx EDX,AL
  xor ECX,[EBP + 2048 + EDX*4]
  movzx EDX,AH
  xor ECX,[EBP + 3072 + EDX*4]
//  Y := Box[1, M.B.A] xor Box[2, M.B.B] xor Box[3, M.B.C] xor Box[0, M.B.D];
//  mov EBX,[EDI + 4]
  movzx EAX,BL
  mov EDX,[EBP + 1024 + EAX*4]
  movzx EAX,BH
  xor EDX,[EBP + 2048 + EAX*4]
  shr EBX,16
  movzx EAX,BL
  xor EDX,[EBP + 3072 + EAX*4]
  movzx EAX,BH
  xor EDX,[EBP        + EAX*4]
//  M.C.L := M.C.L xor (X + Y       + S[0]);
  mov EAX,[ESI]
  add ECX,EDX
  add EAX,ECX
  xor EAX,[EDI + 8]
//  asm ROR  M.C.L,1 end;
  ror EAX,1
  mov [EDI + 8],EAX
//  asm ROL  M.D.L,1 end;
  mov EBX,[EDI + 12]
  rol EBX,1
//  M.D.L := M.D.L xor (X + Y shl 1 + S[1]);
  add ECX,EDX
  add ECX,[ESI + 4]
  xor EBX,ECX
  mov [EDI + 12],EBX

//  X := Box[0, M.C.A] xor Box[1, M.C.B] xor Box[2, M.C.C] xor Box[3, M.C.D];
//  mov EAX,[EDI + 8]
  movzx EDX,AL
  mov ECX,[EBP        + EDX*4]
  movzx EDX,AH
  xor ECX,[EBP + 1024 + EDX*4]
  shr EAX,16
  movzx EDX,AL
  xor ECX,[EBP + 2048 + EDX*4]
  movzx EDX,AH
  xor ECX,[EBP + 3072 + EDX*4]
//  Y := Box[1, M.D.A] xor Box[2, M.D.B] xor Box[3, M.D.C] xor Box[0, M.D.D];
//  mov EBX,[EDI + 12]
  movzx EAX,BL
  mov EDX,[EBP + 1024 + EAX*4]
  movzx EAX,BH
  xor EDX,[EBP + 2048 + EAX*4]
  shr EBX,16
  movzx EAX,BL
  xor EDX,[EBP + 3072 + EAX*4]
  movzx EAX,BH
  xor EDX,[EBP        + EAX*4]
//  M.A.L := M.A.L xor (X + Y       + S[2]);
  mov EAX,[ESI + 8]
  add ECX,EDX
  add EAX,ECX
  xor EAX,[EDI]
//  asm ROR  M.A.L,1 end;
  ror EAX,1
  mov [EDI],EAX
//  asm ROL  M.B.L,1 end;
  mov EBX,[EDI + 4]
  rol EBX,1
//  M.B.L := M.B.L xor (X + Y shl 1 + S[3]);
  add ECX,EDX
  add ECX,[ESI + 12]
  xor EBX,ECX
  mov [EDI + 4],EBX

//  S := Ptr(LongInt(S) + BlockSize);
  lea ESI,[ESI + 16]
  sub dword [ESP],1
  jnz @@TwoFishEncMain2
  add ESP,4

//  S := @FSubKey;
  lea ESI,[ESI - 10*16]
//  PIntArray(Data)[0] := M.C.L xor S[4];
//  PIntArray(Data)[1] := M.D.L xor S[5];
//  PIntArray(Data)[2] := M.A.L xor S[6];
//  PIntArray(Data)[3] := M.B.L xor S[7];
//  mov EAX,[EDI]
//  mov EBX,[EDI + 4]
  mov ECX,[EDI + 8]
  mov EDX,[EDI + 12]
  xor ECX,[ESI + 16]
  mov [EDI],ECX
  xor EDX,[ESI + 20]
  mov [EDI + 4],EDX
  xor EAX,[ESI + 24]
  mov [EDI + 8],EAX
  xor EBX,[ESI + 28]
  mov [EDI + 12],EBX
{$ELSE}
{$ENDIF}

