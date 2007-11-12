{$Q-,R-}
{$I ver.inc}
{.$DEFINE OLDBUG}

{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     TwoFish Unit                                      }
{
  LICENSE

  Copyright (c) 2004, Henrick Wibell Hellström, StreamSec
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the name of StreamSec nor the names of its contributors may be
      used to endorse or promote products derived from this software without
      specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
{*******************************************************}
unit SsTwoFish;

interface

uses
  SecUtils;
    
type
  PTwoFishBox = ^TTwoFishBox;
  TTwofishBox = array[0..3, 0..255] of Longword;
  
const
  Twofish_Data: TTwoFishBox = (
  ($BCBC3275,$ECEC21F3,$202043C6,$B3B3C9F4,$DADA03DB,$02028B7B,$E2E22BFB,$9E9EFAC8,
   $C9C9EC4A,$D4D409D3,$18186BE6,$1E1E9F6B,$98980E45,$B2B2387D,$A6A6D2E8,$2626B74B,
   $3C3C57D6,$93938A32,$8282EED8,$525298FD,$7B7BD437,$BBBB3771,$5B5B97F1,$474783E1,
   $24243C30,$5151E20F,$BABAC6F8,$4A4AF31B,$BFBF4887,$0D0D70FA,$B0B0B306,$7575DE3F,
   $D2D2FD5E,$7D7D20BA,$666631AE,$3A3AA35B,$59591C8A,$00000000,$CDCD93BC,$1A1AE09D,
   $AEAE2C6D,$7F7FABC1,$2B2BC7B1,$BEBEB90E,$E0E0A080,$8A8A105D,$3B3B52D2,$6464BAD5,
   $D8D888A0,$E7E7A584,$5F5FE807,$1B1B1114,$2C2CC2B5,$FCFCB490,$3131272C,$808065A3,
   $73732AB2,$0C0C8173,$79795F4C,$6B6B4154,$4B4B0292,$53536974,$94948F36,$83831F51,
   $2A2A3638,$C4C49CB0,$2222C8BD,$D5D5F85A,$BDBDC3FC,$48487860,$FFFFCE62,$4C4C0796,
   $4141776C,$C7C7E642,$EBEB24F7,$1C1C1410,$5D5D637C,$36362228,$6767C027,$E9E9AF8C,
   $4444F913,$1414EA95,$F5F5BB9C,$CFCF18C7,$3F3F2D24,$C0C0E346,$7272DB3B,$54546C70,
   $29294CCA,$F0F035E3,$0808FE85,$C6C617CB,$F3F34F11,$8C8CE4D0,$A4A45993,$CACA96B8,
   $68683BA6,$B8B84D83,$38382820,$E5E52EFF,$ADAD569F,$0B0B8477,$C8C81DC3,$9999FFCC,
   $5858ED03,$19199A6F,$0E0E0A08,$95957EBF,$70705040,$F7F730E7,$6E6ECF2B,$1F1F6EE2,
   $B5B53D79,$09090F0C,$616134AA,$57571682,$9F9F0B41,$9D9D803A,$111164EA,$2525CDB9,
   $AFAFDDE4,$4545089A,$DFDF8DA4,$A3A35C97,$EAEAD57E,$353558DA,$EDEDD07A,$4343FC17,
   $F8F8CB66,$FBFBB194,$3737D3A1,$FAFA401D,$C2C2683D,$B4B4CCF0,$32325DDE,$9C9C71B3,
   $5656E70B,$E3E3DA72,$878760A7,$15151B1C,$F9F93AEF,$6363BFD1,$3434A953,$9A9A853E,
   $B1B1428F,$7C7CD133,$88889B26,$3D3DA65F,$A1A1D7EC,$E4E4DF76,$8181942A,$91910149,
   $0F0FFB81,$EEEEAA88,$161661EE,$D7D77321,$9797F5C4,$A5A5A81A,$FEFE3FEB,$6D6DB5D9,
   $7878AEC5,$C5C56D39,$1D1DE599,$7676A4CD,$3E3EDCAD,$CBCB6731,$B6B6478B,$EFEF5B01,
   $12121E18,$6060C523,$6A6AB0DD,$4D4DF61F,$CECEE94E,$DEDE7C2D,$55559DF9,$7E7E5A48,
   $2121B24F,$03037AF2,$A0A02665,$5E5E198E,$5A5A6678,$65654B5C,$62624E58,$FDFD4519,
   $0606F48D,$404086E5,$F2F2BE98,$3333AC57,$17179067,$05058E7F,$E8E85E05,$4F4F7D64,
   $89896AAF,$10109563,$74742FB6,$0A0A75FE,$5C5C92F5,$9B9B74B7,$2D2D333C,$3030D6A5,
   $2E2E49CE,$494989E9,$46467268,$77775544,$A8A8D8E0,$9696044D,$2828BD43,$A9A92969,
   $D9D97929,$8686912E,$D1D187AC,$F4F44A15,$8D8D1559,$D6D682A8,$B9B9BC0A,$42420D9E,
   $F6F6C16E,$2F2FB847,$DDDD06DF,$23233934,$CCCC6235,$F1F1C46A,$C1C112CF,$8585EBDC,
   $8F8F9E22,$7171A1C9,$9090F0C0,$AAAA539B,$0101F189,$8B8BE1D4,$4E4E8CED,$8E8E6FAB,
   $ABABA212,$6F6F3EA2,$E6E6540D,$DBDBF252,$92927BBB,$B7B7B602,$6969CA2F,$3939D9A9,
   $D3D30CD7,$A7A72361,$A2A2AD1E,$C3C399B4,$6C6C4450,$07070504,$04047FF6,$272746C2,
   $ACACA716,$D0D07625,$50501386,$DCDCF756,$84841A55,$E1E15109,$7A7A25BE,$1313EF91),
  ($A9D93939,$67901717,$B3719C9C,$E8D2A6A6,$04050707,$FD985252,$A3658080,$76DFE4E4,
   $9A084545,$92024B4B,$80A0E0E0,$78665A5A,$E4DDAFAF,$DDB06A6A,$D1BF6363,$38362A2A,
   $0D54E6E6,$C6432020,$3562CCCC,$98BEF2F2,$181E1212,$F724EBEB,$ECD7A1A1,$6C774141,
   $43BD2828,$7532BCBC,$37D47B7B,$269B8888,$FA700D0D,$13F94444,$94B1FBFB,$485A7E7E,
   $F27A0303,$D0E48C8C,$8B47B6B6,$303C2424,$84A5E7E7,$54416B6B,$DF06DDDD,$23C56060,
   $1945FDFD,$5BA33A3A,$3D68C2C2,$59158D8D,$F321ECEC,$AE316666,$A23E6F6F,$82165757,
   $63951010,$015BEFEF,$834DB8B8,$2E918686,$D9B56D6D,$511F8383,$9B53AAAA,$7C635D5D,
   $A63B6868,$EB3FFEFE,$A5D63030,$BE257A7A,$16A7ACAC,$0C0F0909,$E335F0F0,$6123A7A7,
   $C0F09090,$8CAFE9E9,$3A809D9D,$F5925C5C,$73810C0C,$2C273131,$2576D0D0,$0BE75656,
   $BB7B9292,$4EE9CECE,$89F10101,$6B9F1E1E,$53A93434,$6AC4F1F1,$B499C3C3,$F1975B5B,
   $E1834747,$E66B1818,$BDC82222,$450E9898,$E26E1F1F,$F4C9B3B3,$B62F7474,$66CBF8F8,
   $CCFF9999,$95EA1414,$03ED5858,$56F7DCDC,$D4E18B8B,$1C1B1515,$1EADA2A2,$D70CD3D3,
   $FB2BE2E2,$C31DC8C8,$8E195E5E,$B5C22C2C,$E9894949,$CF12C1C1,$BF7E9595,$BA207D7D,
   $EA641111,$77840B0B,$396DC5C5,$AF6A8989,$33D17C7C,$C9A17171,$62CEFFFF,$7137BBBB,
   $81FB0F0F,$793DB5B5,$0951E1E1,$ADDC3E3E,$242D3F3F,$CDA47676,$F99D5555,$D8EE8282,
   $E5864040,$C5AE7878,$B9CD2525,$4D049696,$44557777,$080A0E0E,$86135050,$E730F7F7,
   $A1D33737,$1D40FAFA,$AA346161,$ED8C4E4E,$06B3B0B0,$706C5454,$B22A7373,$D2523B3B,
   $410B9F9F,$7B8B0202,$A088D8D8,$114FF3F3,$3167CBCB,$C2462727,$27C06767,$90B4FCFC,
   $20283838,$F67F0404,$60784848,$FF2EE5E5,$96074C4C,$5C4B6565,$B1C72B2B,$AB6F8E8E,
   $9E0D4242,$9CBBF5F5,$52F2DBDB,$1BF34A4A,$5FA63D3D,$9359A4A4,$0ABCB9B9,$EF3AF9F9,
   $91EF1313,$85FE0808,$49019191,$EE611616,$2D7CDEDE,$4FB22121,$8F42B1B1,$3BDB7272,
   $47B82F2F,$8748BFBF,$6D2CAEAE,$46E3C0C0,$D6573C3C,$3E859A9A,$6929A9A9,$647D4F4F,
   $2A948181,$CE492E2E,$CB17C6C6,$2FCA6969,$FCC3BDBD,$975CA3A3,$055EE8E8,$7AD0EDED,
   $AC87D1D1,$7F8E0505,$D5BA6464,$1AA8A5A5,$4BB72626,$0EB9BEBE,$A7608787,$5AF8D5D5,
   $28223636,$14111B1B,$3FDE7575,$2979D9D9,$88AAEEEE,$3C332D2D,$4C5F7979,$02B6B7B7,
   $B896CACA,$DA583535,$B09CC4C4,$17FC4343,$551A8484,$1FF64D4D,$8A1C5959,$7D38B2B2,
   $57AC3333,$C718CFCF,$8DF40606,$74695353,$B7749B9B,$C4F59797,$9F56ADAD,$72DAE3E3,
   $7ED5EAEA,$154AF4F4,$229E8F8F,$12A2ABAB,$584E6262,$07E85F5F,$99E51D1D,$34392323,
   $6EC1F6F6,$50446C6C,$DE5D3232,$68724646,$6526A0A0,$BC93CDCD,$DB03DADA,$F8C6BABA,
   $C8FA9E9E,$A882D6D6,$2BCF6E6E,$40507070,$DCEB8585,$FE750A0A,$328A9393,$A48DDFDF,
   $CA4C2929,$10141C1C,$2173D7D7,$F0CCB4B4,$D309D4D4,$5D108A8A,$0FE25151,$00000000,
   $6F9A1919,$9DE01A1A,$368F9494,$42E6C7C7,$4AECC9C9,$5EFDD2D2,$C1AB7F7F,$E0D8A8A8),
  ($BC75BC32,$ECF3EC21,$20C62043,$B3F4B3C9,$DADBDA03,$027B028B,$E2FBE22B,$9EC89EFA,
   $C94AC9EC,$D4D3D409,$18E6186B,$1E6B1E9F,$9845980E,$B27DB238,$A6E8A6D2,$264B26B7,
   $3CD63C57,$9332938A,$82D882EE,$52FD5298,$7B377BD4,$BB71BB37,$5BF15B97,$47E14783,
   $2430243C,$510F51E2,$BAF8BAC6,$4A1B4AF3,$BF87BF48,$0DFA0D70,$B006B0B3,$753F75DE,
   $D25ED2FD,$7DBA7D20,$66AE6631,$3A5B3AA3,$598A591C,$00000000,$CDBCCD93,$1A9D1AE0,
   $AE6DAE2C,$7FC17FAB,$2BB12BC7,$BE0EBEB9,$E080E0A0,$8A5D8A10,$3BD23B52,$64D564BA,
   $D8A0D888,$E784E7A5,$5F075FE8,$1B141B11,$2CB52CC2,$FC90FCB4,$312C3127,$80A38065,
   $73B2732A,$0C730C81,$794C795F,$6B546B41,$4B924B02,$53745369,$9436948F,$8351831F,
   $2A382A36,$C4B0C49C,$22BD22C8,$D55AD5F8,$BDFCBDC3,$48604878,$FF62FFCE,$4C964C07,
   $416C4177,$C742C7E6,$EBF7EB24,$1C101C14,$5D7C5D63,$36283622,$672767C0,$E98CE9AF,
   $441344F9,$149514EA,$F59CF5BB,$CFC7CF18,$3F243F2D,$C046C0E3,$723B72DB,$5470546C,
   $29CA294C,$F0E3F035,$088508FE,$C6CBC617,$F311F34F,$8CD08CE4,$A493A459,$CAB8CA96,
   $68A6683B,$B883B84D,$38203828,$E5FFE52E,$AD9FAD56,$0B770B84,$C8C3C81D,$99CC99FF,
   $580358ED,$196F199A,$0E080E0A,$95BF957E,$70407050,$F7E7F730,$6E2B6ECF,$1FE21F6E,
   $B579B53D,$090C090F,$61AA6134,$57825716,$9F419F0B,$9D3A9D80,$11EA1164,$25B925CD,
   $AFE4AFDD,$459A4508,$DFA4DF8D,$A397A35C,$EA7EEAD5,$35DA3558,$ED7AEDD0,$431743FC,
   $F866F8CB,$FB94FBB1,$37A137D3,$FA1DFA40,$C23DC268,$B4F0B4CC,$32DE325D,$9CB39C71,
   $560B56E7,$E372E3DA,$87A78760,$151C151B,$F9EFF93A,$63D163BF,$345334A9,$9A3E9A85,
   $B18FB142,$7C337CD1,$8826889B,$3D5F3DA6,$A1ECA1D7,$E476E4DF,$812A8194,$91499101,
   $0F810FFB,$EE88EEAA,$16EE1661,$D721D773,$97C497F5,$A51AA5A8,$FEEBFE3F,$6DD96DB5,
   $78C578AE,$C539C56D,$1D991DE5,$76CD76A4,$3EAD3EDC,$CB31CB67,$B68BB647,$EF01EF5B,
   $1218121E,$602360C5,$6ADD6AB0,$4D1F4DF6,$CE4ECEE9,$DE2DDE7C,$55F9559D,$7E487E5A,
   $214F21B2,$03F2037A,$A065A026,$5E8E5E19,$5A785A66,$655C654B,$6258624E,$FD19FD45,
   $068D06F4,$40E54086,$F298F2BE,$335733AC,$17671790,$057F058E,$E805E85E,$4F644F7D,
   $89AF896A,$10631095,$74B6742F,$0AFE0A75,$5CF55C92,$9BB79B74,$2D3C2D33,$30A530D6,
   $2ECE2E49,$49E94989,$46684672,$77447755,$A8E0A8D8,$964D9604,$284328BD,$A969A929,
   $D929D979,$862E8691,$D1ACD187,$F415F44A,$8D598D15,$D6A8D682,$B90AB9BC,$429E420D,
   $F66EF6C1,$2F472FB8,$DDDFDD06,$23342339,$CC35CC62,$F16AF1C4,$C1CFC112,$85DC85EB,
   $8F228F9E,$71C971A1,$90C090F0,$AA9BAA53,$018901F1,$8BD48BE1,$4EED4E8C,$8EAB8E6F,
   $AB12ABA2,$6FA26F3E,$E60DE654,$DB52DBF2,$92BB927B,$B702B7B6,$692F69CA,$39A939D9,
   $D3D7D30C,$A761A723,$A21EA2AD,$C3B4C399,$6C506C44,$07040705,$04F6047F,$27C22746,
   $AC16ACA7,$D025D076,$50865013,$DC56DCF7,$8455841A,$E109E151,$7ABE7A25,$139113EF),
  ($D939A9D9,$90176790,$719CB371,$D2A6E8D2,$05070405,$9852FD98,$6580A365,$DFE476DF,
   $08459A08,$024B9202,$A0E080A0,$665A7866,$DDAFE4DD,$B06ADDB0,$BF63D1BF,$362A3836,
   $54E60D54,$4320C643,$62CC3562,$BEF298BE,$1E12181E,$24EBF724,$D7A1ECD7,$77416C77,
   $BD2843BD,$32BC7532,$D47B37D4,$9B88269B,$700DFA70,$F94413F9,$B1FB94B1,$5A7E485A,
   $7A03F27A,$E48CD0E4,$47B68B47,$3C24303C,$A5E784A5,$416B5441,$06DDDF06,$C56023C5,
   $45FD1945,$A33A5BA3,$68C23D68,$158D5915,$21ECF321,$3166AE31,$3E6FA23E,$16578216,
   $95106395,$5BEF015B,$4DB8834D,$91862E91,$B56DD9B5,$1F83511F,$53AA9B53,$635D7C63,
   $3B68A63B,$3FFEEB3F,$D630A5D6,$257ABE25,$A7AC16A7,$0F090C0F,$35F0E335,$23A76123,
   $F090C0F0,$AFE98CAF,$809D3A80,$925CF592,$810C7381,$27312C27,$76D02576,$E7560BE7,
   $7B92BB7B,$E9CE4EE9,$F10189F1,$9F1E6B9F,$A93453A9,$C4F16AC4,$99C3B499,$975BF197,
   $8347E183,$6B18E66B,$C822BDC8,$0E98450E,$6E1FE26E,$C9B3F4C9,$2F74B62F,$CBF866CB,
   $FF99CCFF,$EA1495EA,$ED5803ED,$F7DC56F7,$E18BD4E1,$1B151C1B,$ADA21EAD,$0CD3D70C,
   $2BE2FB2B,$1DC8C31D,$195E8E19,$C22CB5C2,$8949E989,$12C1CF12,$7E95BF7E,$207DBA20,
   $6411EA64,$840B7784,$6DC5396D,$6A89AF6A,$D17C33D1,$A171C9A1,$CEFF62CE,$37BB7137,
   $FB0F81FB,$3DB5793D,$51E10951,$DC3EADDC,$2D3F242D,$A476CDA4,$9D55F99D,$EE82D8EE,
   $8640E586,$AE78C5AE,$CD25B9CD,$04964D04,$55774455,$0A0E080A,$13508613,$30F7E730,
   $D337A1D3,$40FA1D40,$3461AA34,$8C4EED8C,$B3B006B3,$6C54706C,$2A73B22A,$523BD252,
   $0B9F410B,$8B027B8B,$88D8A088,$4FF3114F,$67CB3167,$4627C246,$C06727C0,$B4FC90B4,
   $28382028,$7F04F67F,$78486078,$2EE5FF2E,$074C9607,$4B655C4B,$C72BB1C7,$6F8EAB6F,
   $0D429E0D,$BBF59CBB,$F2DB52F2,$F34A1BF3,$A63D5FA6,$59A49359,$BCB90ABC,$3AF9EF3A,
   $EF1391EF,$FE0885FE,$01914901,$6116EE61,$7CDE2D7C,$B2214FB2,$42B18F42,$DB723BDB,
   $B82F47B8,$48BF8748,$2CAE6D2C,$E3C046E3,$573CD657,$859A3E85,$29A96929,$7D4F647D,
   $94812A94,$492ECE49,$17C6CB17,$CA692FCA,$C3BDFCC3,$5CA3975C,$5EE8055E,$D0ED7AD0,
   $87D1AC87,$8E057F8E,$BA64D5BA,$A8A51AA8,$B7264BB7,$B9BE0EB9,$6087A760,$F8D55AF8,
   $22362822,$111B1411,$DE753FDE,$79D92979,$AAEE88AA,$332D3C33,$5F794C5F,$B6B702B6,
   $96CAB896,$5835DA58,$9CC4B09C,$FC4317FC,$1A84551A,$F64D1FF6,$1C598A1C,$38B27D38,
   $AC3357AC,$18CFC718,$F4068DF4,$69537469,$749BB774,$F597C4F5,$56AD9F56,$DAE372DA,
   $D5EA7ED5,$4AF4154A,$9E8F229E,$A2AB12A2,$4E62584E,$E85F07E8,$E51D99E5,$39233439,
   $C1F66EC1,$446C5044,$5D32DE5D,$72466872,$26A06526,$93CDBC93,$03DADB03,$C6BAF8C6,
   $FA9EC8FA,$82D6A882,$CF6E2BCF,$50704050,$EB85DCEB,$750AFE75,$8A93328A,$8DDFA48D,
   $4C29CA4C,$141C1014,$73D72173,$CCB4F0CC,$09D4D309,$108A5D10,$E2510FE2,$00000000,
   $9A196F9A,$E01A9DE0,$8F94368F,$E6C742E6,$ECC94AEC,$FDD25EFD,$AB7FC1AB,$D8A8E0D8));

type
  TLongRec = packed record
               case Integer of
                 0: (L: Longword);
                 1: (A,B,C,D: Byte);
             end;
  PBlock = ^TBlock;
  TBlock = packed record
             A,B,C,D: TLongRec;
           end;

  PIntArray = ^IntArray;
  IntArray = array [0..(MaxInt div 4)-1] of LongWord;

  TTwoFish_ECB = class(TBlockCipher)
  protected
    FSubKey: array [0..39] of LongWord;
    FBox: TTwoFishBox;            
    IV: array [0..47] of Byte;
    procedure CleanUp; override;
    procedure DecryptBlock(var Buf); override;
    procedure DecryptBlockToDst(var Dst; const Src); override;
    procedure EncryptBlock(var Buf); override;
    procedure EncryptBlockToDst(var Dst; const Src); override;
  public
    constructor Create(const AKey; Count, VectorSize: Integer); override;
    constructor CreateClone(ACipher: TCipher); override;
    class function BlockSize: Integer; override;
    class function Mode: TCipherMode; override;  
    class function Algorithm: TCipherAlg; override;
    class function AlgorithmName: PChar; override;
    class function BlockVectorSize: Integer; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;
    class function MaxKeySize: Integer; override;
    class function MinKeySize: Integer; override;
    procedure SetUp(const AKey; Count, VectorSize: Integer); override;
  end;

  TTwoFish_CFB = class(TTwoFish_ECB)
  public
    constructor Create(const AKey; Count, VectorSize: Integer); override;
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;
    property ModeRatio;
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;

  TTwoFish_OFB = class(TTwoFish_CFB)
  public
    class function Mode: TCipherMode; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;        
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;

  TTwoFish_PCFB = class(TTwoFish_CFB)
  public
    constructor Create(const AKey; Count, VectorSize: Integer); override;
    class function Mode: TCipherMode; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;        
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;

  TTwoFish_PipedPCFB = class(TTwoFish_ECB)
  public
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;

  TTwoFish_ABC = class(TTwoFish_ECB)
  public
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;

  TTwoFish_CTR = class(TTwoFish_ECB)
  public
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;        
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;

  TTwoFish_CBC = class(TTwoFish_ECB)
  public
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;

implementation

uses
  SysUtils;

{ TTwoFish }

const
  Twofish_8x8: array[0..1, 0..255] of Byte = (
   ($A9,$67,$B3,$E8,$04,$FD,$A3,$76,$9A,$92,$80,$78,$E4,$DD,$D1,$38,
    $0D,$C6,$35,$98,$18,$F7,$EC,$6C,$43,$75,$37,$26,$FA,$13,$94,$48,
    $F2,$D0,$8B,$30,$84,$54,$DF,$23,$19,$5B,$3D,$59,$F3,$AE,$A2,$82,
    $63,$01,$83,$2E,$D9,$51,$9B,$7C,$A6,$EB,$A5,$BE,$16,$0C,$E3,$61,
    $C0,$8C,$3A,$F5,$73,$2C,$25,$0B,$BB,$4E,$89,$6B,$53,$6A,$B4,$F1,
    $E1,$E6,$BD,$45,$E2,$F4,$B6,$66,$CC,$95,$03,$56,$D4,$1C,$1E,$D7,
    $FB,$C3,$8E,$B5,$E9,$CF,$BF,$BA,$EA,$77,$39,$AF,$33,$C9,$62,$71,
    $81,$79,$09,$AD,$24,$CD,$F9,$D8,$E5,$C5,$B9,$4D,$44,$08,$86,$E7,
    $A1,$1D,$AA,$ED,$06,$70,$B2,$D2,$41,$7B,$A0,$11,$31,$C2,$27,$90,
    $20,$F6,$60,$FF,$96,$5C,$B1,$AB,$9E,$9C,$52,$1B,$5F,$93,$0A,$EF,
    $91,$85,$49,$EE,$2D,$4F,$8F,$3B,$47,$87,$6D,$46,$D6,$3E,$69,$64,
    $2A,$CE,$CB,$2F,$FC,$97,$05,$7A,$AC,$7F,$D5,$1A,$4B,$0E,$A7,$5A,
    $28,$14,$3F,$29,$88,$3C,$4C,$02,$B8,$DA,$B0,$17,$55,$1F,$8A,$7D,
    $57,$C7,$8D,$74,$B7,$C4,$9F,$72,$7E,$15,$22,$12,$58,$07,$99,$34,
    $6E,$50,$DE,$68,$65,$BC,$DB,$F8,$C8,$A8,$2B,$40,$DC,$FE,$32,$A4,
    $CA,$10,$21,$F0,$D3,$5D,$0F,$00,$6F,$9D,$36,$42,$4A,$5E,$C1,$E0),
   ($75,$F3,$C6,$F4,$DB,$7B,$FB,$C8,$4A,$D3,$E6,$6B,$45,$7D,$E8,$4B,
    $D6,$32,$D8,$FD,$37,$71,$F1,$E1,$30,$0F,$F8,$1B,$87,$FA,$06,$3F,
    $5E,$BA,$AE,$5B,$8A,$00,$BC,$9D,$6D,$C1,$B1,$0E,$80,$5D,$D2,$D5,
    $A0,$84,$07,$14,$B5,$90,$2C,$A3,$B2,$73,$4C,$54,$92,$74,$36,$51,
    $38,$B0,$BD,$5A,$FC,$60,$62,$96,$6C,$42,$F7,$10,$7C,$28,$27,$8C,
    $13,$95,$9C,$C7,$24,$46,$3B,$70,$CA,$E3,$85,$CB,$11,$D0,$93,$B8,
    $A6,$83,$20,$FF,$9F,$77,$C3,$CC,$03,$6F,$08,$BF,$40,$E7,$2B,$E2,
    $79,$0C,$AA,$82,$41,$3A,$EA,$B9,$E4,$9A,$A4,$97,$7E,$DA,$7A,$17,
    $66,$94,$A1,$1D,$3D,$F0,$DE,$B3,$0B,$72,$A7,$1C,$EF,$D1,$53,$3E,
    $8F,$33,$26,$5F,$EC,$76,$2A,$49,$81,$88,$EE,$21,$C4,$1A,$EB,$D9,
    $C5,$39,$99,$CD,$AD,$31,$8B,$01,$18,$23,$DD,$1F,$4E,$2D,$F9,$48,
    $4F,$F2,$65,$8E,$78,$5C,$58,$19,$8D,$E5,$98,$57,$67,$7F,$05,$64,
    $AF,$63,$B6,$FE,$F5,$B7,$3C,$A5,$CE,$E9,$68,$44,$E0,$4D,$43,$69,
    $29,$2E,$AC,$15,$59,$A8,$0A,$9E,$6E,$47,$DF,$34,$35,$6A,$CF,$DC,
    $22,$C9,$C0,$9B,$89,$D4,$ED,$AB,$12,$A2,$0D,$52,$BB,$02,$2F,$A9,
    $D7,$61,$1E,$B4,$50,$04,$F6,$C2,$16,$25,$86,$56,$55,$09,$BE,$91));


function ROL(Value: LongWord; Shift: Integer): LongWord; assembler;
asm
       MOV   ECX,EDX
       ROL   EAX,CL
end;

class function TTwoFish_ECB.Algorithm: TCipherAlg;
begin
  Result := caTwoFish;
end;

class function TTwoFish_ECB.AlgorithmName: PChar;
begin
  Result := 'TwoFish';
end;

class function TTwoFish_ECB.BlockSize: Integer;
begin
  Result := 16;
end;

class function TTwoFish_ECB.BlockVectorSize: Integer;
begin
  Result := 0;
end;

procedure TTwoFish_ECB.CleanUp;
begin
  ProtectClear(FBox,SizeOf(FBox));
  ProtectClear(FSubKey,SizeOf(FSubKey));
  inherited;
end;

constructor TTwoFish_ECB.Create(const AKey; Count, VectorSize: Integer);
begin
  FIV := @IV;
  inherited;
end;   

constructor TTwoFish_ECB.CreateClone(ACipher: TCipher);
begin         
  VirtualLock;
  if ACipher is TTwoFish_ECB then begin
    FIV := @IV;
    FSubKey := TTwoFish_ECB(ACipher).FSubKey;
    FBox := TTwoFish_ECB(ACipher).FBox;
    FFBIndex := TTwoFish_ECB(ACipher).FFBIndex;
    IV := TTwoFish_ECB(ACipher).IV;
    ModeRatio := TTwoFish_ECB(ACipher).ModeRatio;
  end else
    inherited;
end;

                                                        
{$IFDEF ECB}
procedure TTwoFish_ECB.Decrypt(var Buf; Count: Integer);
asm
  push EBX
  push ESI
  push EDI
  push EBP

  push EAX
  push EDX
  shr ECX,4
  push ECX
  test ECX,ECX
  jz @@Exit
@@DcrMain:
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  {$I TwoFishDcr.inc}
  add dword [ESP+4],16
  dec dword [ESP]
  jnz @@DcrMain

@@Exit:
  add ESP,12

  pop EBP
  pop EDI
  pop ESI
  pop EBX
{$ELSE}
procedure TTwoFish_ECB.Decrypt(var Buf; Count: Integer);
begin
  Assert(False);
{$ENDIF}
end;

procedure TTwoFish_ECB.DecryptBlock(var Buf);
asm
  push EBX
  push EDI
  push ESI
  push EBP
  {$I TwoFishDcr.inc}
  pop EBP
  pop ESI
  pop EDI
  pop EBX
end;
{var
  Data: Pointer;
  S: PIntArray;
  Box: PTwofishBox;
  I,X,Y: LongWord;
  M: TBlock;
begin
  Data := @Buf;
  S := @FSubKey;
  Box := @FBox;
  M.C.L := PIntArray(Data)[0] xor S[4];
  M.D.L := PIntArray(Data)[1] xor S[5];
  M.A.L := PIntArray(Data)[2] xor S[6];
  M.B.L := PIntArray(Data)[3] xor S[7];

  S := @FSubKey[36];
  for I := 0 to 7 do begin
    X := Box[0, M.C.A] xor Box[1, M.C.B] xor Box[2, M.C.C] xor Box[3, M.C.D];
    Y := Box[0, M.D.D] xor Box[1, M.D.A] xor Box[2, M.D.B] xor Box[3, M.D.C];
    asm ROL  M.A.L,1 end;
    M.B.L := M.B.L xor (X + Y shl 1 + S[3]);
    M.A.L := M.A.L xor (X + Y       + S[2]);
    asm ROR  M.B.L,1 end;

    X := Box[0, M.A.A] xor Box[1, M.A.B] xor Box[2, M.A.C] xor Box[3, M.A.D];
    Y := Box[0, M.B.D] xor Box[1, M.B.A] xor Box[2, M.B.B] xor Box[3, M.B.C];
    asm ROL  M.C.L,1 end;
    M.D.L := M.D.L xor (X + Y shl 1 + S[1]);
    M.C.L := M.C.L xor (X + Y       + S[0]);
    asm ROR  M.D.L,1 end;
    S := Ptr(LongInt(S) - BlockSize);
  end;
  S := @FSubKey;
  PIntArray(Data)[0] := M.A.L xor S[0];
  PIntArray(Data)[1] := M.B.L xor S[1];
  PIntArray(Data)[2] := M.C.L xor S[2];
  PIntArray(Data)[3] := M.D.L xor S[3];
end;}

procedure TTwoFish_ECB.DecryptBlockToDst(var Dst; const Src);
asm
  push EBX
  push EDI
  push ESI
  push EBP
  {$I TwoFishDcr2Dst.inc}
  pop EBP
  pop ESI
  pop EDI
  pop EBX
end;

{$IFDEF ECB}
procedure TTwoFish_ECB.Encrypt(var Buf; Count: Integer);
asm
  push EBX
  push ESI
  push EDI
  push EBP

  push EAX
  push EDX
  shr ECX,4
  push ECX
  test ECX,ECX
  jz @@Exit
@@EncMain:
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  {$I TwoFishEnc.inc}
  add dword [ESP+4],16
  dec dword [ESP]
  jnz @@EncMain

@@Exit:
  add ESP,12

  pop EBP
  pop EDI
  pop ESI
  pop EBX
{$ELSE}
procedure TTwoFish_ECB.Encrypt(var Buf; Count: Integer);
begin
  Assert(False);
{$ENDIF}
end;

procedure TTwoFish_ECB.EncryptBlock(var Buf);
asm
  push EBX
  push EDI
  push ESI
  push EBP
  {$I TwoFishEnc.inc}
  pop EBP
  pop ESI
  pop EDI
  pop EBX
end;
{var
  Data: Pointer;
  S: PIntArray;
  Box: PTwofishBox;
  I,X,Y: LongWord;
  M: TBlock;
begin
  Data := @Buf;
  Box := @FBox;
  S := @FSubKey;
  M.A.L := PIntArray(Data)[0] xor S[0];
  M.B.L := PIntArray(Data)[1] xor S[1];
  M.C.L := PIntArray(Data)[2] xor S[2];
  M.D.L := PIntArray(Data)[3] xor S[3];

  S   := @FSubKey[8];
  for I := 0 to 7 do begin
    X := Box[0, M.A.A] xor Box[1, M.A.B] xor Box[2, M.A.C] xor Box[3, M.A.D];
    Y := Box[1, M.B.A] xor Box[2, M.B.B] xor Box[3, M.B.C] xor Box[0, M.B.D];
    asm ROL  M.D.L,1 end;
    M.C.L := M.C.L xor (X + Y       + S[0]);
    M.D.L := M.D.L xor (X + Y shl 1 + S[1]);
    asm ROR  M.C.L,1 end;

    X := Box[0, M.C.A] xor Box[1, M.C.B] xor Box[2, M.C.C] xor Box[3, M.C.D];
    Y := Box[1, M.D.A] xor Box[2, M.D.B] xor Box[3, M.D.C] xor Box[0, M.D.D];
    asm ROL  M.B.L,1 end;
    M.A.L := M.A.L xor (X + Y       + S[2]);
    M.B.L := M.B.L xor (X + Y shl 1 + S[3]);
    asm ROR  M.A.L,1 end;
    S := Ptr(LongInt(S) + BlockSize);
  end;
  S := @FSubKey;
  PIntArray(Data)[0] := M.C.L xor S[4];
  PIntArray(Data)[1] := M.D.L xor S[5];
  PIntArray(Data)[2] := M.A.L xor S[6];
  PIntArray(Data)[3] := M.B.L xor S[7];
end;}

procedure TTwoFish_ECB.EncryptBlockToDst(var Dst; const Src);
asm
  push EBX
  push EDI
  push ESI
  push EBP
  {$I TwoFishEnc2Dst.inc}
  pop EBP
  pop ESI
  pop EDI
  pop EBX
end;

class function TTwoFish_ECB.MaxKeySize: Integer;
begin
  Result := 32;
end;

class function TTwoFish_ECB.MinKeySize: Integer;
begin
  Result := 16;
end;

class function TTwoFish_ECB.Mode: TCipherMode;
begin
  Result := cmECB;
end;

procedure XorByByte(var Dst; const Src; Count: Integer; Value: Byte);
var
  I: Integer;
  X: LongWord;
begin
  X := Value * $01010101;
  for I := 0 to (Count shr 2) - 1 do
    PIntArray(@Dst)[I] := PIntArray(@Src)[I] xor X;
end;

procedure TTwoFish_ECB.SetUp(const AKey; Count, VectorSize: Integer);
var
  BoxKey: array[0..3] of TLongRec;
  SubKey: PIntArray;
  Box: PTwofishBox;

  procedure SetupKey;

    function Encode(K0, K1: Integer): Integer;
    var
      R, I, J, G2, G3: Integer;
      B: byte;
    begin
      R := 0;
      for I := 0 to 1 do begin
        if I <> 0 then
          R := R xor K0
        else
          R := R xor K1;
        for J := 0 to 3 do begin
          B := R shr 24;
          if B and $80 <> 0 then
            G2 := (B shl 1 xor $014D) and $FF
          else
            G2 := B shl 1 and $FF;
          if B and 1 <> 0 then
            G3 := (B shr 1 and $7F) xor $014D shr 1 xor G2
          else
            G3 := (B shr 1 and $7F) xor G2;
          R := R shl 8 xor G3 shl 24 xor G2 shl 16 xor G3 shl 8 xor B;
        end;
      end;
      Result := R;
    end;

    function F32(X: Integer; K: array of Integer): Integer;
    var
      A, B, C, D: Integer;
    begin
      A := X and $FF;
      B := X shr  8 and $FF;
      C := X shr 16 and $FF;
      D := X shr 24;
      if Count = 32 then begin
        A := Twofish_8x8[1, A] xor K[3] and $FF;
        B := Twofish_8x8[0, B] xor K[3] shr  8 and $FF;
        C := Twofish_8x8[0, C] xor K[3] shr 16 and $FF;
        D := Twofish_8x8[1, D] xor K[3] shr 24;
      end;
      if Count >= 24 then begin
        A := Twofish_8x8[1, A] xor K[2] and $FF;
        B := Twofish_8x8[1, B] xor K[2] shr  8 and $FF;
        C := Twofish_8x8[0, C] xor K[2] shr 16 and $FF;
        D := Twofish_8x8[0, D] xor K[2] shr 24;
      end;
      A := Twofish_8x8[0, A] xor K[1] and $FF;
      B := Twofish_8x8[1, B] xor K[1] shr  8 and $FF;
      C := Twofish_8x8[0, C] xor K[1] shr 16 and $FF;
      D := Twofish_8x8[1, D] xor K[1] shr 24;

      A := Twofish_8x8[0, A] xor K[0] and $FF;
      B := Twofish_8x8[0, B] xor K[0] shr  8 and $FF;
      C := Twofish_8x8[1, C] xor K[0] shr 16 and $FF;
      D := Twofish_8x8[1, D] xor K[0] shr 24;

      Result := Twofish_Data[0, A] xor Twofish_Data[1, B] xor
                Twofish_Data[2, C] xor Twofish_Data[3, D];
    end;

  var
    I,J,A,B: Integer;
    E,O: array[0..3] of Integer;
    K: array[0..7] of Integer;
  begin
    FillChar(K, SizeOf(K), 0);
    if Count > 32 then Count := 32;
    Move(AKey, K, Count);
    if Count <= 16 then
      Count := 16
    else if Count <= 24 then
      Count := 24
    else
      Count := 32;
    J := Count shr 3 - 1;
    for I := 0 to J do begin
      E[I] := K[I shl 1];
      O[I] := K[I shl 1 + 1];
      BoxKey[J].L := Encode(E[I], O[I]);
      Dec(J);
    end;
    J := 0;
    for I := 0 to 19 do begin
      A := F32(J, E);
      B := ROL(F32(J + $01010101, O), 8);
      SubKey[I shl 1] := A + B;
      {$IFDEF OLDBUG}
      B := A + B shr 1;
      {$ELSE}
      B := A + B shl 1;
      {$ENDIF}
      SubKey[I shl 1 + 1] := ROL(B, 9);
      Inc(J, $02020202);
    end;
    ProtectClear(E,SizeOf(E));
    ProtectClear(O,SizeOf(O));
    ProtectClear(K,SizeOf(K));
  end;

  procedure SetupBox128;
  var
    L: array[0..255] of Byte;
    A,I: Integer;
  begin
    XorByByte(L, Twofish_8x8[0], 256, BoxKey[1].L);
    A := BoxKey[0].A;
    for I := 0 to 255 do
      Box[0, I] := Twofish_Data[0, Twofish_8x8[0, L[I]] xor A];
    XorByByte(L, Twofish_8x8[1], 256, BoxKey[1].L shr 8);
    A := BoxKey[0].B;
    for I := 0 to 255 do
      Box[1, I] := Twofish_Data[1, Twofish_8x8[0, L[I]] xor A];
    XorByByte(L, Twofish_8x8[0], 256, BoxKey[1].L shr 16);
    A := BoxKey[0].C;
    for I := 0 to 255 do
      Box[2, I] := Twofish_Data[2, Twofish_8x8[1, L[I]] xor A];
    XorByByte(L, Twofish_8x8[1], 256, BoxKey[1].L shr 24);
    A := BoxKey[0].D;
    for I := 0 to 255 do
      Box[3, I] := Twofish_Data[3, Twofish_8x8[1, L[I]] xor A];
    ProtectClear(L,SizeOf(L));
  end;

  procedure SetupBox192;
  var
    L: array[0..255] of Byte;
    A,B,I: Integer;
  begin
    XorByByte(L, Twofish_8x8[1], 256, BoxKey[2].L);
    A := BoxKey[0].A;
    B := BoxKey[1].A;
    for I := 0 to 255 do
      Box[0, I] := Twofish_Data[0, Twofish_8x8[0, Twofish_8x8[0, L[I]] xor B] xor A];
    XorByByte(L, Twofish_8x8[1], 256, BoxKey[2].L shr 8);
    A := BoxKey[0].B;
    B := BoxKey[1].B;
    for I := 0 to 255 do
      Box[1, I] := Twofish_Data[1, Twofish_8x8[0, Twofish_8x8[1, L[I]] xor B] xor A];
    XorByByte(L, Twofish_8x8[0], 256, BoxKey[2].L shr 16);
    A := BoxKey[0].C;
    B := BoxKey[1].C;
    for I := 0 to 255 do
      Box[2, I] := Twofish_Data[2, Twofish_8x8[1, Twofish_8x8[0, L[I]] xor B] xor A];
    XorByByte(L ,Twofish_8x8[0], 256, BoxKey[2].L shr 24);
    A := BoxKey[0].D;
    B := BoxKey[1].D;
    for I := 0 to 255 do
      Box[3, I] := Twofish_Data[3, Twofish_8x8[1, Twofish_8x8[1, L[I]] xor B] xor A];
    ProtectClear(L,SizeOf(L));
  end;

  procedure SetupBox256;
  var
    L: array[0..255] of Byte;
    K: array[0..255] of Byte;
    A,B,I: Integer;
  begin
    XorByByte(K, Twofish_8x8[1], 256, BoxKey[3].L);
    for I := 0 to 255 do
      L[I] := Twofish_8x8[1, K[I]];
    XorByByte(L, L, 256, BoxKey[2].L);
    A := BoxKey[0].A;
    B := BoxKey[1].A;
    for I := 0 to 255 do
      Box[0, I] := Twofish_Data[0, Twofish_8x8[0, Twofish_8x8[0, L[I]] xor B] xor A];
    XorByByte(K, Twofish_8x8[0], 256, BoxKey[3].L shr 8);
    for I := 0 to 255 do
      L[I] := Twofish_8x8[1, K[I]];
    XorByByte(L, L, 256, BoxKey[2].L shr 8);
    A := BoxKey[0].B;
    B := BoxKey[1].B;
    for I := 0 to 255 do
      Box[1, I] := Twofish_Data[1, Twofish_8x8[0, Twofish_8x8[1, L[I]] xor B] xor A];
    XorByByte(K, Twofish_8x8[0], 256, BoxKey[3].L shr 16);
    for I := 0 to 255 do
      L[I] := Twofish_8x8[0, K[I]];
    XorByByte(L, L, 256, BoxKey[2].L shr 16);
    A := BoxKey[0].C;
    B := BoxKey[1].C;
    for I := 0 to 255 do
      Box[2, I] := Twofish_Data[2, Twofish_8x8[1, Twofish_8x8[0, L[I]] xor B] xor A];
    XorByByte(K, Twofish_8x8[1], 256, BoxKey[3].L shr 24);
    for I := 0 to 255 do
      L[I] := Twofish_8x8[0, K[I]];
    XorByByte(L, L, 256, BoxKey[2].L shr 24);
    A := BoxKey[0].D;
    B := BoxKey[1].D;
    for I := 0 to 255 do
      Box[3, I] := Twofish_Data[3, Twofish_8x8[1, Twofish_8x8[1, L[I]] xor B] xor A];
    ProtectClear(L,SizeOf(L));
    ProtectClear(K,SizeOf(K));
  end;

begin
  Nag(Count);
  SubKey := @FSubKey;
  Box    := @FBox;
  SetupKey;
  if Count = 16 then
    SetupBox128
  else if Count = 24 then
    SetupBox192
  else
    SetupBox256;
end;

{ TTwoFish_PCFB }

constructor TTwoFish_PCFB.Create(const AKey; Count, VectorSize: Integer);
begin
  ModeRatio := 1;
  inherited;
end;

procedure TTwoFish_PCFB.Decrypt(var Buf; Count: Integer);
var
  i, j, c: longint;
  CT: array[0..15] of byte;
begin
  c := Count div ModeRatio;
  for i:= 0 to c-1 do begin 
    Move(PByteArray(@Buf)^[i*ModeRatio],CT,ModeRatio);
    EncryptBlock(IV);
    for j := 0 to ModeRatio - 1 do
      PByteArray(@Buf)^[i*ModeRatio + j]:=
        PByteArray(@Buf)^[i*ModeRatio + j] xor IV[j];
    Move(IV[ModeRatio],IV[0],16 - ModeRatio);
    Move(CT,IV[16 - ModeRatio],ModeRatio);
  end;    
  if (Count mod ModeRatio) <> 0 then begin
    Move(PByteArray(@Buf)^[c*ModeRatio],CT,Count mod ModeRatio);
    EncryptBlock(IV);
    for j := 0 to (Count mod ModeRatio) - 1 do
      PByteArray(@Buf)^[c*ModeRatio + j]:=
        PByteArray(@Buf)^[c*ModeRatio + j] xor IV[j];
    Move(IV[Count mod ModeRatio],IV[0],16 - (Count mod ModeRatio));
    Move(CT,IV[16 - (Count mod ModeRatio)],(Count mod ModeRatio));
  end;
end;

procedure TTwoFish_PCFB.Encrypt(var Buf; Count: Integer);
var
  i, j, c: longint;
begin                      
  c := Count div ModeRatio;
  for i:= 0 to c-1 do begin
    EncryptBlock(IV);
    for j := 0 to ModeRatio - 1 do
      PByteArray(@Buf)^[i*ModeRatio + j]:=
        PByteArray(@Buf)^[i*ModeRatio + j] xor IV[j];
    Move(IV[ModeRatio],IV[0],16 - ModeRatio);
    Move(PByteArray(@Buf)^[i*ModeRatio],IV[16 - ModeRatio],ModeRatio);
  end;      
  if (Count mod ModeRatio) <> 0 then begin
    EncryptBlock(IV);
    for j := 0 to (Count mod ModeRatio) - 1 do
      PByteArray(@Buf)^[c*ModeRatio + j]:=
        PByteArray(@Buf)^[c*ModeRatio + j] xor IV[j];
    Move(IV[Count mod ModeRatio],IV[0],16 - (Count mod ModeRatio));
    Move(PByteArray(@Buf)^[c*ModeRatio],IV[16 - (Count mod ModeRatio)],Count mod ModeRatio);
  end;
end;

class function TTwoFish_PCFB.KeyedIVOID(KeySize: Integer): PChar;
begin
  case KeySize of
    16: Result := '1.3.6.1.4.1.13085.1.37';
    24: Result := '1.3.6.1.4.1.13085.1.38';
    32: Result := '1.3.6.1.4.1.13085.1.39';
  else
    Result := nil;
  end;
end;

class function TTwoFish_PCFB.Mode: TCipherMode;
begin
  Result := cmPCFB;
end;

class function TTwoFish_PCFB.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

{ TTwoFish_ABC }

class function TTwoFish_ABC.BlockVectorSize: Integer;
begin
  Result := 2;
end;

procedure TTwoFish_ABC.Decrypt(var Buf; Count: Integer);
{$IFDEF ABC}
asm
  push EBX
  push ESI
  push EDI
  push EBP

  push ECX
  push EAX
  push EDX
  shr ECX,4
  push ECX
  test ECX,ECX
  jz @@LastBlock
@@DcrMain:
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TTwoFish_ABC.IV
  // B[i] := C[i] xor H[i-1]
  mov EBX,[ESI]
  mov [ESI+32],EBX
  xor EBX,[EDX]
  mov [ESI],EBX
  mov EBX,[ESI+4]
  mov [ESI+36],EBX
  xor EBX,[EDX+4]
  mov [ESI+4],EBX
  mov EBX,[ESI+8]
  mov [ESI+40],EBX
  xor EBX,[EDX+8]
  mov [ESI+8],EBX
  mov EBX,[ESI+12]
  mov [ESI+44],EBX
  xor EBX,[EDX+12]
  mov [ESI+12],EBX
  // A[i] := D(B[i]);
  mov EDX,ESI
  {$I TwoFishDcr.inc}
  // H[i] := A[i] xor C[i - 1]
  // P[i] := H[i] xor H[i-1]
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TTwoFish_ABC.IV
  mov EBX,[ESI+16]
  mov ECX,[EDX]
  mov [ESI+16],ECX
  xor [ESI],EBX   
  mov EBX,[ESI]
  xor EBX,[ESI+32]
  mov [EDX],EBX

  mov EBX,[ESI+20]
  mov ECX,[EDX+4]
  mov [ESI+20],ECX
  xor [ESI+4],EBX
  mov EBX,[ESI+4]
  xor EBX,[ESI+36]
  mov [EDX+4],EBX
  mov EBX,[ESI+24]
  mov ECX,[EDX+8]
  mov [ESI+24],ECX
  xor [ESI+8],EBX
  mov EBX,[ESI+8]
  xor EBX,[ESI+40]
  mov [EDX+8],EBX
  mov EBX,[ESI+28]
  mov ECX,[EDX+12]
  mov [ESI+28],ECX
  xor [ESI+12],EBX
  mov EBX,[ESI+12]
  xor EBX,[ESI+44]
  mov [EDX+12],EBX


  add dword [ESP+4],16
  dec dword [ESP]
  jnz @@DcrMain

@@LastBlock:
  mov ECX,[ESP+12]
  and ECX,15
  jz @@Exit
  // XORBlock128(IV[0],IV[16]);
  lea ESI,dword ptr [EAX].TTwoFish_ABC.IV
  mov EBX,[ESI+16]
  xor [ESI],EBX
  mov EBX,[ESI+20]
  xor [ESI+4],EBX
  mov EBX,[ESI+24]
  xor [ESI+8],EBX
  mov EBX,[ESI+28]
  xor [ESI+12],EBX
  // EncryptBlock(IV[0]);
  mov EDX,ESI
  {$I TwoFishEnc2.inc}
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TTwoFish_ABC.IV
  mov ECX,[ESP+12]
  and ECX,15
@@LastLoop:
  mov AL,[ESI]
  xor [EDX],AL
  inc ESI
  inc EDX
  loop @@LastLoop

@@Exit:
  add ESP,16

  pop EBP
  pop EDI
  pop ESI
  pop EBX  
{$ELSE}
begin
  Assert(False);
{$ENDIF}
end;
{var
  BlockCount, I: longint;
begin
  BlockCount := Count div 16;
  for I := 0 to BlockCount - 1 do begin
    Move(IV[0],IV[32],16);
    // H[i] := D(C[i] xor H[i-1]) xor C[i-1]
    XORBlock128(IV[0],Ptr(LongInt(@Buf) + I*16)^);
    DecryptBlock(IV[0]);
    XORBlock128(IV[0],IV[16]);

    Move(Ptr(LongInt(@Buf) + I*16)^,IV[16],16);
    // P[i] := H[i] xor H[i-1]
    Move(IV[0],Ptr(LongInt(@Buf) + I*16)^,16);
    XORBlock128(Ptr(LongInt(@Buf) + I*16)^,IV[32]);
  end;

  if BlockCount * 16 < Count then begin
    XORBlock128(IV[0],IV[16]);
    EncryptBlock(IV[0]);
    for I := 0 to Count - BlockCount * 16 - 1 do
      Byte(Ptr(LongInt(@Buf) + I + BlockCount*16)^) :=
        Byte(Ptr(LongInt(@Buf) + I + BlockCount*16)^) xor IV[I];
  end;
end;}

procedure TTwoFish_ABC.Encrypt(var Buf; Count: Integer);
{$IFDEF ABC}
asm
  push EBX
  push ESI
  push EDI
  push EBP

  push ECX
  push EAX
  push EDX
  shr ECX,4
  push ECX
  test ECX,ECX
  jz @@LastBlock
@@EncMain:
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TTwoFish_ABC.IV
  // H[i] := P[i] xor H[i-1];
  // A[i] := H[i] xor C[i-1];
  mov EBX,[ESI]
  mov [ESI+32],EBX
  xor EBX,[EDX]
  mov [ESI],EBX
  xor EBX,[ESI+16]
  mov [EDX],EBX
  mov EBX,[ESI+4]
  mov [ESI+36],EBX
  xor EBX,[EDX+4]
  mov [ESI+4],EBX
  xor EBX,[ESI+20]
  mov [EDX+4],EBX
  mov EBX,[ESI+8]
  mov [ESI+40],EBX
  xor EBX,[EDX+8]
  mov [ESI+8],EBX
  xor EBX,[ESI+24]
  mov [EDX+8],EBX
  mov EBX,[ESI+12]
  mov [ESI+44],EBX
  xor EBX,[EDX+12]
  mov [ESI+12],EBX
  xor EBX,[ESI+28]
  mov [EDX+12],EBX
  // B[i] := E(A[i]);
  {$I TwoFishEnc.inc}
  // C[i] := B[i] xor H[i-1]
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TTwoFish_ABC.IV
  mov EBX,[EDX]
  xor EBX,[ESI+32]
  mov [EDX],EBX
  mov [ESI+16],EBX
  mov EBX,[EDX+4]
  xor EBX,[ESI+36]
  mov [EDX+4],EBX
  mov [ESI+20],EBX
  mov EBX,[EDX+8]
  xor EBX,[ESI+40]
  mov [EDX+8],EBX
  mov [ESI+24],EBX
  mov EBX,[EDX+12]
  xor EBX,[ESI+44]
  mov [EDX+12],EBX
  mov [ESI+28],EBX

  add dword [ESP+4],16
  dec dword [ESP]
  jnz @@EncMain

@@LastBlock:
  mov ECX,[ESP+12]
  and ECX,15
  jz @@Exit
  // XORBlock128(IV[0],IV[16]);
  lea ESI,dword ptr [EAX].TTwoFish_ABC.IV
  mov EBX,[ESI+16]
  xor [ESI],EBX
  mov EBX,[ESI+20]
  xor [ESI+4],EBX
  mov EBX,[ESI+24]
  xor [ESI+8],EBX
  mov EBX,[ESI+28]
  xor [ESI+12],EBX
  // EncryptBlock(IV[0]);
  mov EDX,ESI
  {$I TwoFishEnc2.inc}
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TTwoFish_ABC.IV
  mov ECX,[ESP+12]
  and ECX,15
@@LastLoop:
  mov AL,[ESI]
  xor [EDX],AL
  inc ESI
  inc EDX
  loop @@LastLoop

@@Exit:
  add ESP,16

  pop EBP
  pop EDI
  pop ESI
  pop EBX  
{$ELSE}
begin
  Assert(False);
{$ENDIF}
end;
{var
  BlockCount, I: longint;
begin
  BlockCount := Count div 16;
  for I := 0 to BlockCount - 1 do begin
    Move(IV[0],IV[32],16);
    // H[i] := P[i] xor H[i-1]
    XORBlock128(IV[0],Ptr(LongInt(@Buf) + I*16)^);
    Move(IV[0],Ptr(LongInt(@Buf) + I*16)^,16);
    // C[i] := E(C[i-1] xor H[i]) xor H[i-1]
    XORBlock128(Ptr(LongInt(@Buf) + I*16)^,IV[16]);
    EncryptBlock(Ptr(LongInt(@Buf) + I*16)^);
    XORBlock128(Ptr(LongInt(@Buf) + I*16)^,IV[32]);

    Move(Ptr(LongInt(@Buf) + I*16)^,IV[16],16);
  end;

  if BlockCount * 16 < Count then begin
    XORBlock128(IV[0],IV[16]);
    EncryptBlock(IV[0]);
    for I := 0 to Count - BlockCount * 16 - 1 do
      Byte(Ptr(LongInt(@Buf) + I + BlockCount*16)^) :=
        Byte(Ptr(LongInt(@Buf) + I + BlockCount*16)^) xor IV[I];
  end;
end;}

class function TTwoFish_ABC.KeyedIVOID(KeySize: Integer): PChar;
begin
  case KeySize of
    16: Result := '1.3.6.1.4.1.13085.1.34';
    24: Result := '1.3.6.1.4.1.13085.1.35';
    32: Result := '1.3.6.1.4.1.13085.1.36';
  else
    Result := nil;
  end;
end;

class function TTwoFish_ABC.Mode: TCipherMode;
begin
  Result := cmABC;
end;

class function TTwoFish_ABC.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

{ TTwoFish_PipedPCFB }

class function TTwoFish_PipedPCFB.BlockVectorSize: Integer;
begin
  Result := 2;
end;

procedure TTwoFish_PipedPCFB.Decrypt(var Buf; Count: Integer);
var
  BlockCount, I: longint;
begin
  BlockCount := Count div 16;
  for I := 0 to BlockCount - 1 do begin 
    Move(IV[0],IV[32],16);
    // V[i] := E(C[i-1] + V[i-1]);
    AddBlock128(IV[32],IV[16]);
    EncryptBlock(IV[32]);

    Move(Ptr(LongInt(@Buf) + I*16)^,IV[16],16);
    // P[i] := C[i] xor E(V[i] + V[i-1]);
    AddBlock128(IV[0],IV[32]);
    EncryptBlock(IV[0]);
    XORBlock128(Ptr(LongInt(@Buf) + I*16)^,IV[0]);

    Move(IV[32],IV[0],16);
  end;

  if BlockCount * 16 < Count then begin
    Move(IV[0],IV[32],16);
    // V[i] := E(C[i-1] + V[i-1]);
    AddBlock128(IV[32],IV[16]);
    EncryptBlock(IV[32]);
    // P[i] := C[i] xor E(V[i] + V[i-1]);
    AddBlock128(IV[0],IV[32]);
    EncryptBlock(IV[0]);
    for I := 0 to Count - BlockCount * 16 - 1 do
      Byte(Ptr(LongInt(@Buf) + I + BlockCount*16)^) :=
        Byte(Ptr(LongInt(@Buf) + I + BlockCount*16)^) xor IV[I];
  end;
end;

procedure TTwoFish_PipedPCFB.Encrypt(var Buf; Count: Integer);
var
  BlockCount, I: longint;
begin
  BlockCount := Count div 16;
  for I := 0 to BlockCount - 1 do begin
    Move(IV[0],IV[32],16);
    // V[i] := E(C[i-1] + V[i-1]);
    AddBlock128(IV[32],IV[16]);
    EncryptBlock(IV[32]);

    // C[i] := P[i] xor E(V[i] + V[i-1]);
    AddBlock128(IV[0],IV[32]);
    EncryptBlock(IV[0]);
    XORBlock128(Ptr(LongInt(@Buf) + I*16)^,IV[0]);

    Move(Ptr(LongInt(@Buf) + I*16)^,IV[16],16);
    Move(IV[32],IV[0],16);
  end;

  if BlockCount * 16 < Count then begin
    Move(IV[0],IV[32],16);
    // V[i] := E(C[i-1] + V[i-1]);
    AddBlock128(IV[32],IV[16]);
    EncryptBlock(IV[32]);
    // P[i] := C[i] xor E(V[i] + V[i-1]);
    AddBlock128(IV[0],IV[32]);  
    EncryptBlock(IV[0]);
    for I := 0 to Count - BlockCount * 16 - 1 do
      Byte(Ptr(LongInt(@Buf) + I + BlockCount*16)^) :=
        Byte(Ptr(LongInt(@Buf) + I + BlockCount*16)^) xor IV[I];
  end;
end;

class function TTwoFish_PipedPCFB.KeyedIVOID(KeySize: Integer): PChar;
begin
  case KeySize of
    16: Result := '1.3.6.1.4.1.13085.1.40';
    24: Result := '1.3.6.1.4.1.13085.1.41';
    32: Result := '1.3.6.1.4.1.13085.1.42';
  else
    Result := nil;
  end;
end;

class function TTwoFish_PipedPCFB.Mode: TCipherMode;
begin
  Result := cmPipedPCFB;
end;

class function TTwoFish_PipedPCFB.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

{ TTwoFish_CTR }

class function TTwoFish_CTR.BlockVectorSize: Integer;
begin
  Result := 1;
end;

procedure TTwoFish_CTR.Decrypt(var Buf; Count: Integer);
{$IFDEF CTR}
asm
  push EBX
  push EDI
  push ESI
  push EBP

  push ECX
  push EAX
  push EDX

  lea ESI,dword ptr [EAX].TTwoFish_ECB.IV

  add ESI,16
  mov ECX,EDX
  mov EDI,16
  mov EBX,dword [ESP+8]
  {$I FBEncFirst.inc}
  mov dword [ESP+8],EBX    
  mov dword [ESP],EDX
  shr EBX,4
  push EBX
  sub ESI,16
  mov EAX,dword [ESP+8]
  mov EDX,dword [ESP+4]

  mov ECX,dword [ESP]
  test ECX,ECX
  jz @@LastBlock
@@EncMain:
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TTwoFish_CTR.IV

  // IV := IV + 1;
  mov EBX,[ESI + 12]
  bswap EBX
  add EBX,1
  bswap EBX
  mov [ESI + 12],EBX
  mov [ESI + 28],EBX
  mov EBX,[ESI + 8]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 8],EBX
  mov [ESI + 24],EBX
  mov EBX,[ESI + 4]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 4],EBX
  mov [ESI + 20],EBX
  mov EBX,[ESI + 0]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 0],EBX
  mov [ESI + 16],EBX
  // EncryptBlock(IV);
  lea EDX,[ESI+16]
  {$I TwoFishEnc.inc}
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TTwoFish_CTR.IV
  mov EBX,[ESI+16]
  xor [EDX],EBX
  mov EBX,[ESI+20]
  xor [EDX+4],EBX
  mov EBX,[ESI+24]
  xor [EDX+8],EBX
  mov EBX,[ESI+28]
  xor [EDX+12],EBX


  add dword [ESP+4],16
  dec dword [ESP]
  jnz @@EncMain

@@LastBlock:
  mov ECX,[ESP+12]
  and ECX,15
  jz @@Exit

  // IV := IV + 1;
  lea ESI,dword ptr [EAX].TTwoFish_CTR.IV
  mov EBX,[ESI + 12]
  bswap EBX
  add EBX,1
  bswap EBX
  mov [ESI + 12],EBX
  mov [ESI + 28],EBX
  mov EBX,[ESI + 8]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 8],EBX
  mov [ESI + 24],EBX
  mov EBX,[ESI + 4]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 4],EBX
  mov [ESI + 20],EBX
  mov EBX,[ESI + 0]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 0],EBX
  mov [ESI + 16],EBX
  // EncryptBlock(IV);
  lea EDX,[ESI+16]
  {$I TwoFishEnc2.inc}
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TTwoFish_CTR.IV
  add ESI,16
  mov ECX,[ESP+12]
  and ECX,15       
  mov dword [EAX].TBlockCipher.FFBIndex,ECX
@@LastLoop:
  mov AL,[ESI]
  xor [EDX],AL
  inc ESI
  inc EDX
  loop @@LastLoop

@@Exit:
  add ESP,16

  pop EBP
  pop ESI
  pop EDI
  pop EBX
{$ELSE}
begin
  Assert(False);
{$ENDIF}
end;

procedure TTwoFish_CTR.Encrypt(var Buf; Count: Integer);
{$IFDEF CTR}
asm
  push EBX
  push EDI
  push ESI
  push EBP

  push ECX
  push EAX
  push EDX

  lea ESI,dword ptr [EAX].TTwoFish_ECB.IV

  add ESI,16
  mov ECX,EDX
  mov EDI,16
  mov EBX,dword [ESP+8]
  {$I FBEncFirst.inc}
  mov dword [ESP+8],EBX     
  mov dword [ESP],EDX
  shr EBX,4
  push EBX
  sub ESI,16
  mov EAX,dword [ESP+8]
  mov EDX,dword [ESP+4]

  mov ECX,dword [ESP]
  test ECX,ECX
  jz @@LastBlock
@@EncMain:
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TTwoFish_CTR.IV

  // IV := IV + 1;
  mov EBX,[ESI + 12]
  bswap EBX
  add EBX,1
  bswap EBX
  mov [ESI + 12],EBX
  mov [ESI + 28],EBX
  mov EBX,[ESI + 8]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 8],EBX 
  mov [ESI + 24],EBX
  mov EBX,[ESI + 4]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 4],EBX
  mov [ESI + 20],EBX
  mov EBX,[ESI + 0]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 0],EBX
  mov [ESI + 16],EBX
  // EncryptBlockToDst(IV[16],IV);
  lea EDX,[ESI+16]
  {$I TwoFishEnc.inc}
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TTwoFish_CTR.IV
  mov EBX,[ESI+16]
  xor [EDX],EBX
  mov EBX,[ESI+20]
  xor [EDX+4],EBX
  mov EBX,[ESI+24]
  xor [EDX+8],EBX
  mov EBX,[ESI+28]
  xor [EDX+12],EBX


  add dword [ESP+4],16
  dec dword [ESP]
  jnz @@EncMain

@@LastBlock:
  mov ECX,[ESP+12]
  and ECX,15
  jz @@Exit

  lea ESI,dword ptr [EAX].TTwoFish_CTR.IV
  // IV := IV + 1;
  mov EBX,[ESI + 12]
  bswap EBX
  add EBX,1
  bswap EBX
  mov [ESI + 12],EBX
  mov [ESI + 28],EBX
  mov EBX,[ESI + 8]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 8],EBX
  mov [ESI + 24],EBX
  mov EBX,[ESI + 4]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 4],EBX
  mov [ESI + 20],EBX
  mov EBX,[ESI + 0]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 0],EBX
  mov [ESI + 16],EBX
  // EncryptBlock(IV);
  lea EDX,[ESI+16]
  {$I TwoFishEnc2.inc}
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TTwoFish_CTR.IV
  add ESI,16
  mov ECX,[ESP+12]
  and ECX,15          
  mov dword [EAX].TBlockCipher.FFBIndex,ECX
@@LastLoop:
  mov AL,[ESI]
  xor [EDX],AL
  inc ESI
  inc EDX
  loop @@LastLoop

@@Exit:
  add ESP,16

  pop EBP
  pop ESI
  pop EDI
  pop EBX  
{$ELSE}
begin
  Assert(False);
{$ENDIF}
end;

class function TTwoFish_CTR.KeyedIVOID(KeySize: Integer): PChar;
begin
  case KeySize of
    16: Result := '1.3.6.1.4.1.13085.1.31';
    24: Result := '1.3.6.1.4.1.13085.1.32';
    32: Result := '1.3.6.1.4.1.13085.1.33';
  else
    Result := nil;
  end;
end;

class function TTwoFish_CTR.Mode: TCipherMode;
begin
  Result := cmCTR;
end;

class function TTwoFish_CTR.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

{ TTwoFish_CBC }

class function TTwoFish_CBC.BlockVectorSize: Integer;
begin
  Result := 1;
end;

procedure TTwoFish_CBC.Decrypt(var Buf; Count: Integer);
{$IFDEF CBC}
asm
  push EBX
  push ESI
  push EDI
  push EBP

  push ECX
  push EAX
  push EDX
  shr ECX,4
  push ECX
  test ECX,ECX
  jz @@LastBlock
@@EncMain:
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TTwoFish_ECB.IV
  // P[i] := D(C[i]) xor C[i-1];
  mov EBX,[EDX]
  mov [ESI+16],EBX
  mov EBX,[EDX+4]
  mov [ESI+20],EBX
  mov EBX,[EDX+8]
  mov [ESI+24],EBX
  mov EBX,[EDX+12]
  mov [ESI+28],EBX
  {$I TwoFishDcr.inc}
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TTwoFish_ECB.IV
  mov EBX,[ESI+16]
  xchg EBX,[ESI]
  xor [EDX],EBX
  mov EBX,[ESI+20]
  xchg EBX,[ESI+4]
  xor [EDX+4],EBX
  mov EBX,[ESI+24]
  xchg EBX,[ESI+8]
  xor [EDX+8],EBX
  mov EBX,[ESI+28]
  xchg EBX,[ESI+12]
  xor [EDX+12],EBX

  add dword [ESP+4],16
  dec dword [ESP]
  jnz @@EncMain

@@LastBlock:
  mov ECX,[ESP+12]
  and ECX,15
  jz @@Exit
  // EncryptBlock(IV[0]);
  lea ESI,dword ptr [EAX].TTwoFish_CBC.IV
  mov EDX,ESI
  {$I TwoFishEnc2.inc}
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TTwoFish_ECB.IV
  mov ECX,[ESP+12]
  and ECX,15
@@LastLoop:
  mov AL,[ESI]
  xor [EDX],AL
  inc ESI
  inc EDX
  loop @@LastLoop

@@Exit:
  add ESP,16

  pop EBP
  pop EDI
  pop ESI
  pop EBX           
{$ELSE}
begin
  Assert(False);
{$ENDIF}
end;

procedure TTwoFish_CBC.Encrypt(var Buf; Count: Integer);
{$IFDEF CBC}
asm
  push EBX
  push ESI
  push EDI
  push EBP

  push ECX
  push EAX
  push EDX
  shr ECX,4
  push ECX
  test ECX,ECX
  jz @@LastBlock
@@EncMain:
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TTwoFish_ECB.IV
  // C[i] := E(P[i] xor C[i-1]);
  mov EBX,[ESI]
  xor [EDX],EBX
  mov EBX,[ESI+4]
  xor [EDX+4],EBX
  mov EBX,[ESI+8]
  xor [EDX+8],EBX
  mov EBX,[ESI+12]
  xor [EDX+12],EBX
  {$I TwoFishEnc.inc}
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TTwoFish_ECB.IV
  mov EBX,[EDX]
  mov [ESI],EBX
  mov EBX,[EDX+4]
  mov [ESI+4],EBX
  mov EBX,[EDX+8]
  mov [ESI+8],EBX
  mov EBX,[EDX+12]
  mov [ESI+12],EBX


  add dword [ESP+4],16
  dec dword [ESP]
  jnz @@EncMain

@@LastBlock:
  mov ECX,[ESP+12]
  and ECX,15
  jz @@Exit
  // EncryptBlock(IV[0]);
  lea ESI,dword ptr [EAX].TTwoFish_CBC.IV
  mov EDX,ESI
  {$I TwoFishEnc2.inc}
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TTwoFish_ECB.IV
  mov ECX,[ESP+12]
  and ECX,15
@@LastLoop:
  mov AL,[ESI]
  xor [EDX],AL
  inc ESI
  inc EDX
  loop @@LastLoop

@@Exit:
  add ESP,16

  pop EBP
  pop EDI
  pop ESI
  pop EBX  
{$ELSE}
begin
  Assert(False);
{$ENDIF}
end;

class function TTwoFish_CBC.KeyedIVOID(KeySize: Integer): PChar;
begin
  case KeySize of
    16: Result := '1.3.6.1.4.1.13085.1.46';
    24: Result := '1.3.6.1.4.1.13085.1.47';
    32: Result := '1.3.6.1.4.1.13085.1.48';
  else
    Result := nil;
  end;
end;

class function TTwoFish_CBC.Mode: TCipherMode;
begin
  Result := cmCBC;
end;

class function TTwoFish_CBC.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

{ TTwoFish_CFB }

class function TTwoFish_CFB.BlockVectorSize: Integer;
begin
  Result := 1;
end;

constructor TTwoFish_CFB.Create(const AKey; Count, VectorSize: Integer);
begin
  ModeRatio := 16;
  inherited;
end;

procedure TTwoFish_CFB.Decrypt(var Buf; Count: Integer);
begin
  Decrypt_CFB(Buf,Count);
end;

procedure TTwoFish_CFB.Encrypt(var Buf; Count: Integer);
begin
  Encrypt_CFB(Buf,Count);
end;

class function TTwoFish_CFB.KeyedIVOID(KeySize: Integer): PChar;
begin
  case KeySize of
    16: Result := '1.3.6.1.4.1.13085.1.52';
    24: Result := '1.3.6.1.4.1.13085.1.53';
    32: Result := '1.3.6.1.4.1.13085.1.54';
  else
    Result := nil;
  end;
end;

class function TTwoFish_CFB.Mode: TCipherMode;
begin
  Result := cmCFB;
end;

class function TTwoFish_CFB.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

{ TTwoFish_OFB }

procedure TTwoFish_OFB.Decrypt(var Buf; Count: Integer);
begin
  Encrypt_OFB(Buf,Count);
end;

procedure TTwoFish_OFB.Encrypt(var Buf; Count: Integer);
begin
  Encrypt_OFB(Buf,Count);
end;

class function TTwoFish_OFB.KeyedIVOID(KeySize: Integer): PChar;
begin
  case KeySize of
    16: Result := '1.3.6.1.4.1.13085.1.49';
    24: Result := '1.3.6.1.4.1.13085.1.50';
    32: Result := '1.3.6.1.4.1.13085.1.51';
  else
    Result := nil;
  end;
end;

class function TTwoFish_OFB.Mode: TCipherMode;
begin
  Result := cmOFB;
end;

class function TTwoFish_OFB.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

{$IFDEF TWOFISH}
initialization
  {$IFDEF ECB}       RegisterCipherClass( TTwoFish_ECB );       {$ENDIF}
  {$IFDEF ABC}       RegisterCipherClass( TTwoFish_ABC );       {$ENDIF}
  {$IFDEF CBC}       RegisterCipherClass( TTwoFish_CBC );       {$ENDIF}
  {$IFDEF CFB}       RegisterCipherClass( TTwoFish_CFB );       {$ENDIF}
  {$IFDEF CTR}       RegisterCipherClass( TTwoFish_CTR );       {$ENDIF}
  {$IFDEF OFB}       RegisterCipherClass( TTwoFish_OFB );       {$ENDIF}
  {$IFDEF PCFB}      RegisterCipherClass( TTwoFish_PCFB );      {$ENDIF}
  {$IFDEF PIPEDPCFB} RegisterCipherClass( TTwoFish_PipedPCFB ); {$ENDIF}
{$ENDIF}
end.
