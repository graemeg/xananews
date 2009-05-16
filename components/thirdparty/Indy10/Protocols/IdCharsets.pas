{
  $Project$
  $Workfile$
  $Revision$
  $DateUTC$
  $Id$

  This file is part of the Indy (Internet Direct) project, and is offered
  under the dual-licensing agreement described on the Indy website.
  (http://www.indyproject.org/)

  Copyright:
   (c) 1993-2005, Chad Z. Hower and the Indy Pit Crew. All rights reserved.
}
{
  $Log$
}
{
  Rev 1.3    10/16/2003 10:49:18 PM  DSiders
  Added localization comments.

  Rev 1.2    10/8/2003 9:49:02 PM  GGrieve
  merge all TIdCharset to here

  Rev 1.1    10/3/2003 5:39:26 PM  GGrieve
  dotnet work

  Rev 1.0    11/14/2002 02:14:14 PM  JPMugaas
}

unit IdCharsets;

{
  This file is automatically created from
  http://www.iana.org/assignments/character-sets

  All character set constants are prefixed with "idcs", this could lead
  to having a constant named idcscs... because some IANA names are actually
  cs...
  All constants have been renamed to fit Delphi's naming scheme,
  '-', '.', ':' and '+' are converted to '_'
  If a collision occurs, a '_' is appended to the name.
  Care is taken to
    a) put the preferred charset first in a list of identical ones
    b) not append a '_' to the preferred charset

  Two functions can be found here:
  1)
  * function FindPreferredCharset(const Charset: TIdCharSet): TIdCharSet;
  is provided to find the preferred identical charset from an arbitrary
  charset given.

  2)
  * function FindCharset(const s: string): TIdCharset;
  can be used to find a charset from a given string
  (if not found idcsINVALID is returned)

  For references and people see the end of the file (copied from above location)

  Johannes Berg - 2002-08-22

 -- header of the original file follows --

  ===================================================================
  CHARACTER SETS

  (last updated 2007-05-14)

  These are the official names for character sets that may be used in
  the Internet and may be referred to in Internet documentation.  These
  names are expressed in ANSI_X3.4-1968 which is commonly called
  US-ASCII or simply ASCII.  The character set most commonly use in the
  Internet and used especially in protocol standards is US-ASCII, this
  is strongly encouraged.  The use of the name US-ASCII is also
  encouraged.

  The character set names may be up to 40 characters taken from the
  printable characters of US-ASCII.  However, no distinction is made
  between use of upper and lower case letters.

  The MIBenum value is a unique value for use in MIBs to identify coded
  character sets.

  The value space for MIBenum values has been divided into three
  regions. The first region (3-999) consists of coded character sets
  that have been standardized by some standard setting organization.
  This region is intended for standards that do not have subset
  implementations. The second region (1000-1999) is for the Unicode and
  ISO/IEC 10646 coded character sets together with a specification of a
  (set of) sub-repertoires that may occur.  The third region (>1999) is
  intended for vendor specific coded character sets.

	Assigned MIB enum Numbers
	-------------------------
	0-2		Reserved
	3-999		Set By Standards Organizations
	1000-1999	Unicode / 10646
	2000-2999	Vendor

  The aliases that start with "cs" have been added for use with the
  IANA-CHARSET-MIB as originally defined in RFC3808, and as currently
  maintained by IANA at http://www.iana.org/assignments/ianacharset-mib.
  Note that the ianacharset-mib needs to be kept in sync with this
  registry.  These aliases that start with "cs" contain the standard 
  numbers along with suggestive names in order to facilitate applications 
  that want to display the names in user interfaces.  The "cs" stands 
  for character set and is provided for applications that need a lower 
  case first letter but want to use mixed case thereafter that cannot 
  contain any special characters, such as underbar ("_") and dash ("-").  

  If the character set is from an ISO standard, its cs alias is the ISO
  standard number or name.  If the character set is not from an ISO
  standard, but is registered with ISO (IPSJ/ITSCJ is the current ISO
  Registration Authority), the ISO Registry number is specified as
  ISOnnn followed by letters suggestive of the name or standards number
  of the code set.  When a national or international standard is
  revised, the year of revision is added to the cs alias of the new
  character set entry in the IANA Registry in order to distinguish the
  revised character set from the original character set.


  Character Set                                               Reference
}

interface

{$i IdCompilerDefines.inc}

// once upon a time Indy had 3 different declarations of TIdCharSet
// now all use this one. For reference, one of the more widely used
// enums and the equivalents in the full enum is listed here:
//
//  csGB2312         idcsGB2312 *
//  csBig5           idcsBig5 *
//  csIso2022jp      idcsISO_2022_JP *
//  csEucKR          idcsEUC_KR *
//  csIso88591       idcsISO_8859_1 *
//  csWindows1251    idcswindows_1251 *
//  csKOI8r          idcsKOI8_R *
//  csKOI8u          idcsKOI8_U *
//  csUnicode        idcsUNICODE_1_1
//
//
//  Classic UTF-8 is idcsUTF_8

type
  TIdCharSet = (
    idcsINVALID, { signifies an invalid character was found when searching
                   for a charset by name }
    idcsUS_ASCII,
    idcsANSI_X3_4_1968, {Alias of idcsUS_ASCII}
    idcsiso_ir_6, {Alias of idcsUS_ASCII}
    idcsANSI_X3_4_1986, {Alias of idcsUS_ASCII}
    idcsISO_646_irv_1991, {Alias of idcsUS_ASCII}
    idcsASCII, {Alias of idcsUS_ASCII}
    idcsISO646_US, {Alias of idcsUS_ASCII}
    idcsus, {Alias of idcsUS_ASCII}
    idcsIBM367, {Alias of idcsUS_ASCII}
    idcscp367, {Alias of idcsUS_ASCII}
    idcscsASCII, {Alias of idcsUS_ASCII}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsISO_10646_UTF_1,
    idcscsISO10646UTF1, {Alias of idcsISO_10646_UTF_1}
    { Source:
      Universal Transfer Format (1), this is the multibyte encoding, that
      subsets ASCII-7. It does not have byte ordering issues.  }

    idcsISO_646_basic_1983,
    idcsref, {Alias of idcsISO_646_basic_1983}
    idcscsISO646basic1983, {Alias of idcsISO_646_basic_1983}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsINVARIANT,
    idcscsINVARIANT, {Alias of idcsINVARIANT}
    { References: RFC1345,KXS2  }

    idcsISO_646_irv_1983,
    idcsiso_ir_2, {Alias of idcsISO_646_irv_1983}
    idcsirv, {Alias of idcsISO_646_irv_1983}
    idcscsISO2IntlRefVersion, {Alias of idcsISO_646_irv_1983}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsBS_4730,
    idcsiso_ir_4, {Alias of idcsBS_4730}
    idcsISO646_GB, {Alias of idcsBS_4730}
    idcsgb, {Alias of idcsBS_4730}
    idcsuk, {Alias of idcsBS_4730}
    idcscsISO4UnitedKingdom, {Alias of idcsBS_4730}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsNATS_SEFI,
    idcsiso_ir_8_1, {Alias of idcsNATS_SEFI}
    idcscsNATSSEFI, {Alias of idcsNATS_SEFI}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsNATS_SEFI_ADD,
    idcsiso_ir_8_2, {Alias of idcsNATS_SEFI_ADD}
    idcscsNATSSEFIADD, {Alias of idcsNATS_SEFI_ADD}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsNATS_DANO,
    idcsiso_ir_9_1, {Alias of idcsNATS_DANO}
    idcscsNATSDANO, {Alias of idcsNATS_DANO}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsNATS_DANO_ADD,
    idcsiso_ir_9_2, {Alias of idcsNATS_DANO_ADD}
    idcscsNATSDANOADD, {Alias of idcsNATS_DANO_ADD}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsSEN_850200_B,
    idcsiso_ir_10, {Alias of idcsSEN_850200_B}
    idcsFI, {Alias of idcsSEN_850200_B}
    idcsISO646_FI, {Alias of idcsSEN_850200_B}
    idcsISO646_SE, {Alias of idcsSEN_850200_B}
    idcsse, {Alias of idcsSEN_850200_B}
    idcscsISO10Swedish, {Alias of idcsSEN_850200_B}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsSEN_850200_C,
    idcsiso_ir_11, {Alias of idcsSEN_850200_C}
    idcsISO646_SE2, {Alias of idcsSEN_850200_C}
    idcsse2, {Alias of idcsSEN_850200_C}
    idcscsISO11SwedishForNames, {Alias of idcsSEN_850200_C}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsKS_C_5601_1987,
    idcsiso_ir_149, {Alias of idcsKS_C_5601_1987}
    idcsKS_C_5601_1989, {Alias of idcsKS_C_5601_1987}
    idcsKSC_5601, {Alias of idcsKS_C_5601_1987}
    idcskorean, {Alias of idcsKS_C_5601_1987}
    idcscsKSC56011987, {Alias of idcsKS_C_5601_1987}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcscsISO2022KR,
    idcsISO_2022_KR, {Alias of idcscsISO2022KR}
    { References: RFC1557,Choi  }
    { Source:
      RFC-1557 (see also KS_C_5601-1987)  }

    idcscsEUCKR,
    idcsEUC_KR, {Alias of idcscsEUCKR}
    { References: RFC1557,Choi  }
    { Source:
      RFC-1557 (see also KS_C_5861-1992)  }

    idcscsISO2022JP,
    idcsISO_2022_JP, {Alias of idcscsISO2022JP}
    { References: RFC1468,Murai  }
    { Source:
      RFC-1468 (see also RFC-2237)  }

    idcscsISO2022JP2,
    idcsISO_2022_JP_2, {Alias of idcscsISO2022JP2}
    { References: RFC1554,Ohta  }
    { Source:
      RFC-1554  }

    idcsISO_2022_CN,
    { References: RFC1922  }
    { Source:
      RFC-1922  }

    idcsISO_2022_CN_EXT,
    { References: RFC1922  }
    { Source:
      RFC-1922  }

    idcsJIS_C6220_1969_jp,
    idcsJIS_C6220_1969, {Alias of idcsJIS_C6220_1969_jp}
    idcsiso_ir_13, {Alias of idcsJIS_C6220_1969_jp}
    idcskatakana, {Alias of idcsJIS_C6220_1969_jp}
    idcsx0201_7, {Alias of idcsJIS_C6220_1969_jp}
    idcscsISO13JISC6220jp, {Alias of idcsJIS_C6220_1969_jp}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsJIS_C6220_1969_ro,
    idcsiso_ir_14, {Alias of idcsJIS_C6220_1969_ro}
    idcsjp, {Alias of idcsJIS_C6220_1969_ro}
    idcsISO646_JP, {Alias of idcsJIS_C6220_1969_ro}
    idcscsISO14JISC6220ro, {Alias of idcsJIS_C6220_1969_ro}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsIT,
    idcsiso_ir_15, {Alias of idcsIT}
    idcsISO646_IT, {Alias of idcsIT}
    idcscsISO15Italian, {Alias of idcsIT}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsPT,
    idcsiso_ir_16, {Alias of idcsPT}
    idcsISO646_PT, {Alias of idcsPT}
    idcscsISO16Portuguese, {Alias of idcsPT}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsES,
    idcsiso_ir_17, {Alias of idcsES}
    idcsISO646_ES, {Alias of idcsES}
    idcscsISO17Spanish, {Alias of idcsES}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsgreek7_old,
    idcsiso_ir_18, {Alias of idcsgreek7_old}
    idcscsISO18Greek7Old, {Alias of idcsgreek7_old}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcslatin_greek,
    idcsiso_ir_19, {Alias of idcslatin_greek}
    idcscsISO19LatinGreek, {Alias of idcslatin_greek}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsDIN_66003,
    idcsiso_ir_21, {Alias of idcsDIN_66003}
    idcsde, {Alias of idcsDIN_66003}
    idcsISO646_DE, {Alias of idcsDIN_66003}
    idcscsISO21German, {Alias of idcsDIN_66003}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcscsISO25French,
    idcsNF_Z_62_010_, {Alias of idcscsISO25French}
    idcsiso_ir_25, {Alias of idcscsISO25French}
    idcsISO646_FR1, {Alias of idcscsISO25French}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsLatin_greek_1,
    idcsiso_ir_27, {Alias of idcsLatin_greek_1}
    idcscsISO27LatinGreek1, {Alias of idcsLatin_greek_1}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsISO_5427,
    idcsiso_ir_37, {Alias of idcsISO_5427}
    idcscsISO5427Cyrillic, {Alias of idcsISO_5427}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsJIS_C6226_1978,
    idcsiso_ir_42, {Alias of idcsJIS_C6226_1978}
    idcscsISO42JISC62261978, {Alias of idcsJIS_C6226_1978}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsBS_viewdata,
    idcsiso_ir_47, {Alias of idcsBS_viewdata}
    idcscsISO47BSViewdata, {Alias of idcsBS_viewdata}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsINIS,
    idcsiso_ir_49, {Alias of idcsINIS}
    idcscsISO49INIS, {Alias of idcsINIS}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsINIS_8,
    idcsiso_ir_50, {Alias of idcsINIS_8}
    idcscsISO50INIS8, {Alias of idcsINIS_8}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsINIS_cyrillic,
    idcsiso_ir_51, {Alias of idcsINIS_cyrillic}
    idcscsISO51INISCyrillic, {Alias of idcsINIS_cyrillic}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsISO_5427_1981,
    idcsiso_ir_54, {Alias of idcsISO_5427_1981}
    idcsISO5427Cyrillic1981, {Alias of idcsISO_5427_1981}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsISO_5428_1980,
    idcsiso_ir_55, {Alias of idcsISO_5428_1980}
    idcscsISO5428Greek, {Alias of idcsISO_5428_1980}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsGB_1988_80,
    idcsiso_ir_57, {Alias of idcsGB_1988_80}
    idcscn, {Alias of idcsGB_1988_80}
    idcsISO646_CN, {Alias of idcsGB_1988_80}
    idcscsISO57GB1988, {Alias of idcsGB_1988_80}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsGB_2312_80,
    idcsiso_ir_58, {Alias of idcsGB_2312_80}
    idcschinese, {Alias of idcsGB_2312_80}
    idcscsISO58GB231280, {Alias of idcsGB_2312_80}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsNS_4551_1,
    idcsiso_ir_60, {Alias of idcsNS_4551_1}
    idcsISO646_NO, {Alias of idcsNS_4551_1}
    idcsno, {Alias of idcsNS_4551_1}
    idcscsISO60DanishNorwegian, {Alias of idcsNS_4551_1}
    idcscsISO60Norwegian1, {Alias of idcsNS_4551_1}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsNS_4551_2,
    idcsISO646_NO2, {Alias of idcsNS_4551_2}
    idcsiso_ir_61, {Alias of idcsNS_4551_2}
    idcsno2, {Alias of idcsNS_4551_2}
    idcscsISO61Norwegian2, {Alias of idcsNS_4551_2}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsNF_Z_62_010,
    idcsiso_ir_69, {Alias of idcsNF_Z_62_010}
    idcsISO646_FR, {Alias of idcsNF_Z_62_010}
    idcsfr, {Alias of idcsNF_Z_62_010}
    idcscsISO69French, {Alias of idcsNF_Z_62_010}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsvideotex_suppl,
    idcsiso_ir_70, {Alias of idcsvideotex_suppl}
    idcscsISO70VideotexSupp1, {Alias of idcsvideotex_suppl}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsPT2,
    idcsiso_ir_84, {Alias of idcsPT2}
    idcsISO646_PT2, {Alias of idcsPT2}
    idcscsISO84Portuguese2, {Alias of idcsPT2}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsES2,
    idcsiso_ir_85, {Alias of idcsES2}
    idcsISO646_ES2, {Alias of idcsES2}
    idcscsISO85Spanish2, {Alias of idcsES2}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsMSZ_7795_3,
    idcsiso_ir_86, {Alias of idcsMSZ_7795_3}
    idcsISO646_HU, {Alias of idcsMSZ_7795_3}
    idcshu, {Alias of idcsMSZ_7795_3}
    idcscsISO86Hungarian, {Alias of idcsMSZ_7795_3}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsJIS_C6226_1983,
    idcsiso_ir_87, {Alias of idcsJIS_C6226_1983}
    idcsx0208, {Alias of idcsJIS_C6226_1983}
    idcsJIS_X0208_1983, {Alias of idcsJIS_C6226_1983}
    idcscsISO87JISX0208, {Alias of idcsJIS_C6226_1983}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsgreek7,
    idcsiso_ir_88, {Alias of idcsgreek7}
    idcscsISO88Greek7, {Alias of idcsgreek7}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsASMO_449,
    idcsISO_9036, {Alias of idcsASMO_449}
    idcsarabic7, {Alias of idcsASMO_449}
    idcsiso_ir_89, {Alias of idcsASMO_449}
    idcscsISO89ASMO449, {Alias of idcsASMO_449}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsiso_ir_90,
    idcscsISO90, {Alias of idcsiso_ir_90}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsJIS_C6229_1984_a,
    idcsiso_ir_91, {Alias of idcsJIS_C6229_1984_a}
    idcsjp_ocr_a, {Alias of idcsJIS_C6229_1984_a}
    idcscsISO91JISC62291984a, {Alias of idcsJIS_C6229_1984_a}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsJIS_C6229_1984_b,
    idcsiso_ir_92, {Alias of idcsJIS_C6229_1984_b}
    idcsISO646_JP_OCR_B, {Alias of idcsJIS_C6229_1984_b}
    idcsjp_ocr_b, {Alias of idcsJIS_C6229_1984_b}
    idcscsISO92JISC62991984b, {Alias of idcsJIS_C6229_1984_b}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsJIS_C6229_1984_b_add,
    idcsiso_ir_93, {Alias of idcsJIS_C6229_1984_b_add}
    idcsjp_ocr_b_add, {Alias of idcsJIS_C6229_1984_b_add}
    idcscsISO93JIS62291984badd, {Alias of idcsJIS_C6229_1984_b_add}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsJIS_C6229_1984_hand,
    idcsiso_ir_94, {Alias of idcsJIS_C6229_1984_hand}
    idcsjp_ocr_hand, {Alias of idcsJIS_C6229_1984_hand}
    idcscsISO94JIS62291984hand, {Alias of idcsJIS_C6229_1984_hand}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsJIS_C6229_1984_hand_add,
    idcsiso_ir_95, {Alias of idcsJIS_C6229_1984_hand_add}
    idcsjp_ocr_hand_add, {Alias of idcsJIS_C6229_1984_hand_add}
    idcscsISO95JIS62291984handadd, {Alias of idcsJIS_C6229_1984_hand_add}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsJIS_C6229_1984_kana,
    idcsiso_ir_96, {Alias of idcsJIS_C6229_1984_kana}
    idcscsISO96JISC62291984kana, {Alias of idcsJIS_C6229_1984_kana}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsISO_2033_1983,
    idcsiso_ir_98, {Alias of idcsISO_2033_1983}
    idcse13b, {Alias of idcsISO_2033_1983}
    idcscsISO2033, {Alias of idcsISO_2033_1983}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsANSI_X3_110_1983,
    idcsiso_ir_99, {Alias of idcsANSI_X3_110_1983}
    idcsCSA_T500_1983, {Alias of idcsANSI_X3_110_1983}
    idcsNAPLPS, {Alias of idcsANSI_X3_110_1983}
    idcscsISO99NAPLPS, {Alias of idcsANSI_X3_110_1983}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsISO_8859_1,
    idcsISO_8859_1_1987, {Alias of idcsISO_8859_1}
    idcsiso_ir_100, {Alias of idcsISO_8859_1}
    idcsISO_8859_1_, {Alias of idcsISO_8859_1}
    idcslatin1, {Alias of idcsISO_8859_1}
    idcsl1, {Alias of idcsISO_8859_1}
    idcsIBM819, {Alias of idcsISO_8859_1}
    idcsCP819, {Alias of idcsISO_8859_1}
    idcscsISOLatin1, {Alias of idcsISO_8859_1}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsISO_8859_2,
    idcsISO_8859_2_1987, {Alias of idcsISO_8859_2}
    idcsiso_ir_101, {Alias of idcsISO_8859_2}
    idcsISO_8859_2_, {Alias of idcsISO_8859_2}
    idcslatin2, {Alias of idcsISO_8859_2}
    idcsl2, {Alias of idcsISO_8859_2}
    idcscsISOLatin2, {Alias of idcsISO_8859_2}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsT_61_7bit,
    idcsiso_ir_102, {Alias of idcsT_61_7bit}
    idcscsISO102T617bit, {Alias of idcsT_61_7bit}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsT_61_8bit,
    idcsT_61, {Alias of idcsT_61_8bit}
    idcsiso_ir_103, {Alias of idcsT_61_8bit}
    idcscsISO103T618bit, {Alias of idcsT_61_8bit}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsISO_8859_3,
    idcsISO_8859_3_1988, {Alias of idcsISO_8859_3}
    idcsiso_ir_109, {Alias of idcsISO_8859_3}
    idcsISO_8859_3_, {Alias of idcsISO_8859_3}
    idcslatin3, {Alias of idcsISO_8859_3}
    idcsl3, {Alias of idcsISO_8859_3}
    idcscsISOLatin3, {Alias of idcsISO_8859_3}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsISO_8859_4,
    idcsISO_8859_4_1988, {Alias of idcsISO_8859_4}
    idcsiso_ir_110, {Alias of idcsISO_8859_4}
    idcsISO_8859_4_, {Alias of idcsISO_8859_4}
    idcslatin4, {Alias of idcsISO_8859_4}
    idcsl4, {Alias of idcsISO_8859_4}
    idcscsISOLatin4, {Alias of idcsISO_8859_4}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsECMA_cyrillic,
    idcsiso_ir_111, {Alias of idcsECMA_cyrillic}
    idcscsISO111ECMACyrillic, {Alias of idcsECMA_cyrillic}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsCSA_Z243_4_1985_1,
    idcsiso_ir_121, {Alias of idcsCSA_Z243_4_1985_1}
    idcsISO646_CA, {Alias of idcsCSA_Z243_4_1985_1}
    idcscsa7_1, {Alias of idcsCSA_Z243_4_1985_1}
    idcsca, {Alias of idcsCSA_Z243_4_1985_1}
    idcscsISO121Canadian1, {Alias of idcsCSA_Z243_4_1985_1}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsCSA_Z243_4_1985_2,
    idcsiso_ir_122, {Alias of idcsCSA_Z243_4_1985_2}
    idcsISO646_CA2, {Alias of idcsCSA_Z243_4_1985_2}
    idcscsa7_2, {Alias of idcsCSA_Z243_4_1985_2}
    idcscsISO122Canadian2, {Alias of idcsCSA_Z243_4_1985_2}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsCSA_Z243_4_1985_gr,
    idcsiso_ir_123, {Alias of idcsCSA_Z243_4_1985_gr}
    idcscsISO123CSAZ24341985gr, {Alias of idcsCSA_Z243_4_1985_gr}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsISO_8859_6,
    idcsISO_8859_6_1987, {Alias of idcsISO_8859_6}
    idcsiso_ir_127, {Alias of idcsISO_8859_6}
    idcsISO_8859_6_, {Alias of idcsISO_8859_6}
    idcsECMA_114, {Alias of idcsISO_8859_6}
    idcsASMO_708, {Alias of idcsISO_8859_6}
    idcsarabic, {Alias of idcsISO_8859_6}
    idcscsISOLatinArabic, {Alias of idcsISO_8859_6}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsISO_8859_6_E,
    idcsISO_8859_6_E_, {Alias of idcsISO_8859_6_E}
    idcscsISO88596E, {Alias of idcsISO_8859_6_E}
    { References: RFC1556,IANA  }
    { Source:
      RFC1556  }

    idcsISO_8859_6_I,
    idcsISO_8859_6_I_, {Alias of idcsISO_8859_6_I}
    idcscsISO88596I, {Alias of idcsISO_8859_6_I}
    { References: RFC1556,IANA  }
    { Source:
      RFC1556  }

    idcsISO_8859_7,
    idcsISO_8859_7_1987, {Alias of idcsISO_8859_7}
    idcsiso_ir_126, {Alias of idcsISO_8859_7}
    idcsISO_8859_7_, {Alias of idcsISO_8859_7}
    idcsELOT_928, {Alias of idcsISO_8859_7}
    idcsECMA_118, {Alias of idcsISO_8859_7}
    idcsgreek, {Alias of idcsISO_8859_7}
    idcsgreek8, {Alias of idcsISO_8859_7}
    idcscsISOLatinGreek, {Alias of idcsISO_8859_7}
    { References: RFC1947,RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsT_101_G2,
    idcsiso_ir_128, {Alias of idcsT_101_G2}
    idcscsISO128T101G2, {Alias of idcsT_101_G2}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsISO_8859_8,
    idcsISO_8859_8_1988, {Alias of idcsISO_8859_8}
    idcsiso_ir_138, {Alias of idcsISO_8859_8}
    idcsISO_8859_8_, {Alias of idcsISO_8859_8}
    idcshebrew, {Alias of idcsISO_8859_8}
    idcscsISOLatinHebrew, {Alias of idcsISO_8859_8}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsISO_8859_8_E,
    idcsISO_8859_8_E_, {Alias of idcsISO_8859_8_E}
    idcscsISO88598E, {Alias of idcsISO_8859_8_E}
    { References: RFC1556,Nussbacher  }
    { Source:
      RFC1556  }

    idcsISO_8859_8_I,
    idcsISO_8859_8_I_, {Alias of idcsISO_8859_8_I}
    idcscsISO88598I, {Alias of idcsISO_8859_8_I}
    { References: RFC1556,Nussbacher  }
    { Source:
      RFC1556  }

    idcsCSN_369103,
    idcsiso_ir_139, {Alias of idcsCSN_369103}
    idcscsISO139CSN369103, {Alias of idcsCSN_369103}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsJUS_I_B1_002,
    idcsiso_ir_141, {Alias of idcsJUS_I_B1_002}
    idcsISO646_YU, {Alias of idcsJUS_I_B1_002}
    idcsjs, {Alias of idcsJUS_I_B1_002}
    idcsyu, {Alias of idcsJUS_I_B1_002}
    idcscsISO141JUSIB1002, {Alias of idcsJUS_I_B1_002}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsISO_6937_2_add,
    idcsiso_ir_142, {Alias of idcsISO_6937_2_add}
    idcscsISOTextComm, {Alias of idcsISO_6937_2_add}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry and ISO 6937-2:1983  }

    idcsIEC_P27_1,
    idcsiso_ir_143, {Alias of idcsIEC_P27_1}
    idcscsISO143IECP271, {Alias of idcsIEC_P27_1}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsISO_8859_5,
    idcsISO_8859_5_1988, {Alias of idcsISO_8859_5}
    idcsiso_ir_144, {Alias of idcsISO_8859_5}
    idcsISO_8859_5_, {Alias of idcsISO_8859_5}
    idcscyrillic, {Alias of idcsISO_8859_5}
    idcscsISOLatinCyrillic, {Alias of idcsISO_8859_5}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsJUS_I_B1_003_serb,
    idcsiso_ir_146, {Alias of idcsJUS_I_B1_003_serb}
    idcsserbian, {Alias of idcsJUS_I_B1_003_serb}
    idcscsISO146Serbian, {Alias of idcsJUS_I_B1_003_serb}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsJUS_I_B1_003_mac,
    idcsmacedonian, {Alias of idcsJUS_I_B1_003_mac}
    idcsiso_ir_147, {Alias of idcsJUS_I_B1_003_mac}
    idcscsISO147Macedonian, {Alias of idcsJUS_I_B1_003_mac}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsISO_8859_9,
    idcsISO_8859_9_1989, {Alias of idcsISO_8859_9}
    idcsiso_ir_148, {Alias of idcsISO_8859_9}
    idcsISO_8859_9_, {Alias of idcsISO_8859_9}
    idcslatin5, {Alias of idcsISO_8859_9}
    idcsl5, {Alias of idcsISO_8859_9}
    idcscsISOLatin5, {Alias of idcsISO_8859_9}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsgreek_ccitt,
    idcsiso_ir_150, {Alias of idcsgreek_ccitt}
    idcscsISO150, {Alias of idcsgreek_ccitt}
    idcscsISO150GreekCCITT, {Alias of idcsgreek_ccitt}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsNC_NC00_10_81,
    idcscuba, {Alias of idcsNC_NC00_10_81}
    idcsiso_ir_151, {Alias of idcsNC_NC00_10_81}
    idcsISO646_CU, {Alias of idcsNC_NC00_10_81}
    idcscsISO151Cuba, {Alias of idcsNC_NC00_10_81}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsISO_6937_2_25,
    idcsiso_ir_152, {Alias of idcsISO_6937_2_25}
    idcscsISO6937Add, {Alias of idcsISO_6937_2_25}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsGOST_19768_74,
    idcsST_SEV_358_88, {Alias of idcsGOST_19768_74}
    idcsiso_ir_153, {Alias of idcsGOST_19768_74}
    idcscsISO153GOST1976874, {Alias of idcsGOST_19768_74}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsISO_8859_supp,
    idcsiso_ir_154, {Alias of idcsISO_8859_supp}
    idcslatin1_2_5, {Alias of idcsISO_8859_supp}
    idcscsISO8859Supp, {Alias of idcsISO_8859_supp}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsISO_10367_box,
    idcsiso_ir_155, {Alias of idcsISO_10367_box}
    idcscsISO10367Box, {Alias of idcsISO_10367_box}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcslatin6,
    idcsISO_8859_10, {Alias of idcslatin6}
    idcsiso_ir_157, {Alias of idcslatin6}
    idcsl6, {Alias of idcslatin6}
    idcsISO_8859_10_1992, {Alias of idcslatin6}
    idcscsISOLatin6, {Alias of idcslatin6}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcslatin_lap,
    idcslap, {Alias of idcslatin_lap}
    idcsiso_ir_158, {Alias of idcslatin_lap}
    idcscsISO158Lap, {Alias of idcslatin_lap}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsJIS_X0212_1990,
    idcsx0212, {Alias of idcsJIS_X0212_1990}
    idcsiso_ir_159, {Alias of idcsJIS_X0212_1990}
    idcscsISO159JISX02121990, {Alias of idcsJIS_X0212_1990}
    { References: RFC1345,KXS2  }
    { Source:
      ECMA registry  }

    idcsDS_2089,
    idcsDS2089, {Alias of idcsDS_2089}
    idcsISO646_DK, {Alias of idcsDS_2089}
    idcsdk, {Alias of idcsDS_2089}
    idcscsISO646Danish, {Alias of idcsDS_2089}
    { References: RFC1345,KXS2  }
    { Source:
      Danish Standard, DS 2089, February 1974  }

    idcsus_dk,
    idcscsUSDK, {Alias of idcsus_dk}
    { References: RFC1345,KXS2  }

    idcsdk_us,
    idcscsDKUS, {Alias of idcsdk_us}
    { References: RFC1345,KXS2  }

    idcsJIS_X0201,
    idcsX0201, {Alias of idcsJIS_X0201}
    idcscsHalfWidthKatakana, {Alias of idcsJIS_X0201}
    { References: RFC1345,KXS2  }
    { Source:
      JIS X 0201-1976.   One byte only, this is equivalent to JIS/Roman
      (similar to ASCII) plus eight-bit half-width Katakana  }

    idcsKSC5636,
    idcsISO646_KR, {Alias of idcsKSC5636}
    idcscsKSC5636, {Alias of idcsKSC5636}
    { References: RFC1345,KXS2  }

    idcsISO_10646_UCS_2,
    idcscsUnicode, {Alias of idcsISO_10646_UCS_2}
    { Source:
      the 2-octet Basic Multilingual Plane, aka Unicode this needs to specify
      network byte order: the standard does not specify (it is a 16-bit
      integer space)  }

    idcsISO_10646_UCS_4,
    idcscsUCS4, {Alias of idcsISO_10646_UCS_4}
    { Source:
      the full code space. (same comment about byte order, these are 31-bit
      numbers.  }

    idcsDEC_MCS,
    idcsdec, {Alias of idcsDEC_MCS}
    idcscsDECMCS, {Alias of idcsDEC_MCS}
    { References: RFC1345,KXS2  }
    { Source:
      VAX/VMS User's Manual, Order Number: AI-Y517A-TE, April 1986.  }

    idcshp_roman8,
    idcsroman8, {Alias of idcshp_roman8}
    idcsr8, {Alias of idcshp_roman8}
    idcscsHPRoman8, {Alias of idcshp_roman8}
    { References: HP-PCL5,RFC1345,KXS2  }
    { Source:
      LaserJet IIP Printer User's Manual, HP part no 33471-90901,
      Hewlet-Packard, June 1989.  }

    idcsmacintosh,
    idcsmac, {Alias of idcsmacintosh}
    idcscsMacintosh, {Alias of idcsmacintosh}
    { References: RFC1345,KXS2  }
    { Source:
      The Unicode Standard ver1.0, ISBN 0-201-56788-1, Oct 1991  }

    idcsIBM037,
    idcscp037, {Alias of idcsIBM037}
    idcsebcdic_cp_us, {Alias of idcsIBM037}
    idcsebcdic_cp_ca, {Alias of idcsIBM037}
    idcsebcdic_cp_wt, {Alias of idcsIBM037}
    idcsebcdic_cp_nl, {Alias of idcsIBM037}
    idcscsIBM037, {Alias of idcsIBM037}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM038,
    idcsEBCDIC_INT, {Alias of idcsIBM038}
    idcscp038, {Alias of idcsIBM038}
    idcscsIBM038, {Alias of idcsIBM038}
    { References: RFC1345,KXS2  }
    { Source:
      IBM 3174 Character Set Ref, GA27-3831-02, March 1990  }

    idcsIBM273,
    idcsCP273, {Alias of idcsIBM273}
    idcscsIBM273, {Alias of idcsIBM273}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM274,
    idcsEBCDIC_BE, {Alias of idcsIBM274}
    idcsCP274, {Alias of idcsIBM274}
    idcscsIBM274, {Alias of idcsIBM274}
    { References: RFC1345,KXS2  }
    { Source:
      IBM 3174 Character Set Ref, GA27-3831-02, March 1990  }

    idcsIBM275,
    idcsEBCDIC_BR, {Alias of idcsIBM275}
    idcscp275, {Alias of idcsIBM275}
    idcscsIBM275, {Alias of idcsIBM275}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM277,
    idcsEBCDIC_CP_DK, {Alias of idcsIBM277}
    idcsEBCDIC_CP_NO, {Alias of idcsIBM277}
    idcscsIBM277, {Alias of idcsIBM277}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM278,
    idcsCP278, {Alias of idcsIBM278}
    idcsebcdic_cp_fi, {Alias of idcsIBM278}
    idcsebcdic_cp_se, {Alias of idcsIBM278}
    idcscsIBM278, {Alias of idcsIBM278}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM280,
    idcsCP280, {Alias of idcsIBM280}
    idcsebcdic_cp_it, {Alias of idcsIBM280}
    idcscsIBM280, {Alias of idcsIBM280}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM281,
    idcsEBCDIC_JP_E, {Alias of idcsIBM281}
    idcscp281, {Alias of idcsIBM281}
    idcscsIBM281, {Alias of idcsIBM281}
    { References: RFC1345,KXS2  }
    { Source:
      IBM 3174 Character Set Ref, GA27-3831-02, March 1990  }

    idcsIBM284,
    idcsCP284, {Alias of idcsIBM284}
    idcsebcdic_cp_es, {Alias of idcsIBM284}
    idcscsIBM284, {Alias of idcsIBM284}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM285,
    idcsCP285, {Alias of idcsIBM285}
    idcsebcdic_cp_gb, {Alias of idcsIBM285}
    idcscsIBM285, {Alias of idcsIBM285}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM290,
    idcscp290, {Alias of idcsIBM290}
    idcsEBCDIC_JP_kana, {Alias of idcsIBM290}
    idcscsIBM290, {Alias of idcsIBM290}
    { References: RFC1345,KXS2  }
    { Source:
      IBM 3174 Character Set Ref, GA27-3831-02, March 1990  }

    idcsIBM297,
    idcscp297, {Alias of idcsIBM297}
    idcsebcdic_cp_fr, {Alias of idcsIBM297}
    idcscsIBM297, {Alias of idcsIBM297}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM420,
    idcscp420, {Alias of idcsIBM420}
    idcsebcdic_cp_ar1, {Alias of idcsIBM420}
    idcscsIBM420, {Alias of idcsIBM420}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990, IBM NLS RM p 11-11  }

    idcsIBM423,
    idcscp423, {Alias of idcsIBM423}
    idcsebcdic_cp_gr, {Alias of idcsIBM423}
    idcscsIBM423, {Alias of idcsIBM423}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM424,
    idcscp424, {Alias of idcsIBM424}
    idcsebcdic_cp_he, {Alias of idcsIBM424}
    idcscsIBM424, {Alias of idcsIBM424}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM437,
    idcscp437, {Alias of idcsIBM437}
    idcs437, {Alias of idcsIBM437}
    idcscsPC8CodePage437, {Alias of idcsIBM437}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM500,
    idcsCP500, {Alias of idcsIBM500}
    idcsebcdic_cp_be, {Alias of idcsIBM500}
    idcsebcdic_cp_ch, {Alias of idcsIBM500}
    idcscsIBM500, {Alias of idcsIBM500}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM775,
    idcscp775, {Alias of idcsIBM775}
    idcscsPC775Baltic, {Alias of idcsIBM775}
    { References: HP-PCL5  }
    { Source:
      HP PCL 5 Comparison Guide (P/N 5021-0329) pp B-13, 1996  }

    idcsIBM850,
    idcscp850, {Alias of idcsIBM850}
    idcs850, {Alias of idcsIBM850}
    idcscsPC850Multilingual, {Alias of idcsIBM850}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM851,
    idcscp851, {Alias of idcsIBM851}
    idcs851, {Alias of idcsIBM851}
    idcscsIBM851, {Alias of idcsIBM851}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM852,
    idcscp852, {Alias of idcsIBM852}
    idcs852, {Alias of idcsIBM852}
    idcscsPCp852, {Alias of idcsIBM852}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM855,
    idcscp855, {Alias of idcsIBM855}
    idcs855, {Alias of idcsIBM855}
    idcscsIBM855, {Alias of idcsIBM855}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM857,
    idcscp857, {Alias of idcsIBM857}
    idcs857, {Alias of idcsIBM857}
    idcscsIBM857, {Alias of idcsIBM857}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM860,
    idcscp860, {Alias of idcsIBM860}
    idcs860, {Alias of idcsIBM860}
    idcscsIBM860, {Alias of idcsIBM860}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM861,
    idcscp861, {Alias of idcsIBM861}
    idcs861, {Alias of idcsIBM861}
    idcscp_is, {Alias of idcsIBM861}
    idcscsIBM861, {Alias of idcsIBM861}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM862,
    idcscp862, {Alias of idcsIBM862}
    idcs862, {Alias of idcsIBM862}
    idcscsPC862LatinHebrew, {Alias of idcsIBM862}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM863,
    idcscp863, {Alias of idcsIBM863}
    idcs863, {Alias of idcsIBM863}
    idcscsIBM863, {Alias of idcsIBM863}
    { References: RFC1345,KXS2  }
    { Source:
      IBM Keyboard layouts and code pages, PN 07G4586 June 1991  }

    idcsIBM864,
    idcscp864, {Alias of idcsIBM864}
    idcscsIBM864, {Alias of idcsIBM864}
    { References: RFC1345,KXS2  }
    { Source:
      IBM Keyboard layouts and code pages, PN 07G4586 June 1991  }

    idcsIBM865,
    idcscp865, {Alias of idcsIBM865}
    idcs865, {Alias of idcsIBM865}
    idcscsIBM865, {Alias of idcsIBM865}
    { References: RFC1345,KXS2  }
    { Source:
      IBM DOS 3.3 Ref (Abridged), 94X9575 (Feb 1987)  }

    idcsIBM866,
    idcscp866, {Alias of idcsIBM866}
    idcs866, {Alias of idcsIBM866}
    idcscsIBM866, {Alias of idcsIBM866}
    { References: Pond  }
    { Source:
      IBM NLDG Volume 2 (SE09-8002-03) August 1994  }

    idcsIBM868,
    idcsCP868, {Alias of idcsIBM868}
    idcscp_ar, {Alias of idcsIBM868}
    idcscsIBM868, {Alias of idcsIBM868}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM869,
    idcscp869, {Alias of idcsIBM869}
    idcs869, {Alias of idcsIBM869}
    idcscp_gr, {Alias of idcsIBM869}
    idcscsIBM869, {Alias of idcsIBM869}
    { References: RFC1345,KXS2  }
    { Source:
      IBM Keyboard layouts and code pages, PN 07G4586 June 1991  }

    idcsIBM870,
    idcsCP870, {Alias of idcsIBM870}
    idcsebcdic_cp_roece, {Alias of idcsIBM870}
    idcsebcdic_cp_yu, {Alias of idcsIBM870}
    idcscsIBM870, {Alias of idcsIBM870}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM871,
    idcsCP871, {Alias of idcsIBM871}
    idcsebcdic_cp_is, {Alias of idcsIBM871}
    idcscsIBM871, {Alias of idcsIBM871}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM880,
    idcscp880, {Alias of idcsIBM880}
    idcsEBCDIC_Cyrillic, {Alias of idcsIBM880}
    idcscsIBM880, {Alias of idcsIBM880}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM891,
    idcscp891, {Alias of idcsIBM891}
    idcscsIBM891, {Alias of idcsIBM891}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM903,
    idcscp903, {Alias of idcsIBM903}
    idcscsIBM903, {Alias of idcsIBM903}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM904,
    idcscp904, {Alias of idcsIBM904}
    idcs904, {Alias of idcsIBM904}
    idcscsIBBM904, {Alias of idcsIBM904}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM905,
    idcsCP905, {Alias of idcsIBM905}
    idcsebcdic_cp_tr, {Alias of idcsIBM905}
    idcscsIBM905, {Alias of idcsIBM905}
    { References: RFC1345,KXS2  }
    { Source:
      IBM 3174 Character Set Ref, GA27-3831-02, March 1990  }

    idcsIBM918,
    idcsCP918, {Alias of idcsIBM918}
    idcsebcdic_cp_ar2, {Alias of idcsIBM918}
    idcscsIBM918, {Alias of idcsIBM918}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsIBM1026,
    idcsCP1026, {Alias of idcsIBM1026}
    idcscsIBM1026, {Alias of idcsIBM1026}
    { References: RFC1345,KXS2  }
    { Source:
      IBM NLS RM Vol2 SE09-8002-01, March 1990  }

    idcsEBCDIC_AT_DE,
    idcscsIBMEBCDICATDE, {Alias of idcsEBCDIC_AT_DE}
    { References: RFC1345,KXS2  }
    { Source:
      IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987  }

    idcsEBCDIC_AT_DE_A,
    idcscsEBCDICATDEA, {Alias of idcsEBCDIC_AT_DE_A}
    { References: RFC1345,KXS2  }
    { Source:
      IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987  }

    idcsEBCDIC_CA_FR,
    idcscsEBCDICCAFR, {Alias of idcsEBCDIC_CA_FR}
    { References: RFC1345,KXS2  }
    { Source:
      IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987  }

    idcsEBCDIC_DK_NO,
    idcscsEBCDICDKNO, {Alias of idcsEBCDIC_DK_NO}
    { References: RFC1345,KXS2  }
    { Source:
      IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987  }

    idcsEBCDIC_DK_NO_A,
    idcscsEBCDICDKNOA, {Alias of idcsEBCDIC_DK_NO_A}
    { References: RFC1345,KXS2  }
    { Source:
      IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987  }

    idcsEBCDIC_FI_SE,
    idcscsEBCDICFISE, {Alias of idcsEBCDIC_FI_SE}
    { References: RFC1345,KXS2  }
    { Source:
      IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987  }

    idcsEBCDIC_FI_SE_A,
    idcscsEBCDICFISEA, {Alias of idcsEBCDIC_FI_SE_A}
    { References: RFC1345,KXS2  }
    { Source:
      IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987  }

    idcsEBCDIC_FR,
    idcscsEBCDICFR, {Alias of idcsEBCDIC_FR}
    { References: RFC1345,KXS2  }
    { Source:
      IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987  }

    idcsEBCDIC_IT,
    idcscsEBCDICIT, {Alias of idcsEBCDIC_IT}
    { References: RFC1345,KXS2  }
    { Source:
      IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987  }

    idcsEBCDIC_PT,
    idcscsEBCDICPT, {Alias of idcsEBCDIC_PT}
    { References: RFC1345,KXS2  }
    { Source:
      IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987  }

    idcsEBCDIC_ES,
    idcscsEBCDICES, {Alias of idcsEBCDIC_ES}
    { References: RFC1345,KXS2  }
    { Source:
      IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987  }

    idcsEBCDIC_ES_A,
    idcscsEBCDICESA, {Alias of idcsEBCDIC_ES_A}
    { References: RFC1345,KXS2  }
    { Source:
      IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987  }

    idcsEBCDIC_ES_S,
    idcscsEBCDICESS, {Alias of idcsEBCDIC_ES_S}
    { References: RFC1345,KXS2  }
    { Source:
      IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987  }

    idcsEBCDIC_UK,
    idcscsEBCDICUK, {Alias of idcsEBCDIC_UK}
    { References: RFC1345,KXS2  }
    { Source:
      IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987  }

    idcsEBCDIC_US,
    idcscsEBCDICUS, {Alias of idcsEBCDIC_US}
    { References: RFC1345,KXS2  }
    { Source:
      IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987  }

    idcsUNKNOWN_8BIT,
    idcscsUnknown8BiT, {Alias of idcsUNKNOWN_8BIT}
    { References: RFC1428  }

    idcsMNEMONIC,
    idcscsMnemonic, {Alias of idcsMNEMONIC}
    { References: RFC1345,KXS2  }
    { Source:
      RFC 1345, also known as "mnemonic+ascii+38"  }

    idcsMNEM,
    idcscsMnem, {Alias of idcsMNEM}
    { References: RFC1345,KXS2  }
    { Source:
      RFC 1345, also known as "mnemonic+ascii+8200"  }

    idcsVISCII,
    idcscsVISCII, {Alias of idcsVISCII}
    { References: RFC1456  }
    { Source:
      RFC 1456  }

    idcsVIQR,
    idcscsVIQR, {Alias of idcsVIQR}
    { References: RFC1456  }
    { Source:
      RFC 1456  }

    idcscsKOI8R,
    idcsKOI8_R, {Alias of idcscsKOI8R}
    { References: RFC1489  }
    { Source:
      RFC 1489, based on GOST-19768-74, ISO-6937/8, INIS-Cyrillic, ISO-5427.  }

    idcsKOI8_U,
    { References: RFC2319  }
    { Source:
      RFC 2319  }

    idcsIBM00858,
    idcsCCSID00858, {Alias of idcsIBM00858}
    idcsCP00858, {Alias of idcsIBM00858}
    idcsPC_Multilingual_850_euro, {Alias of idcsIBM00858}
    { Source:
      IBM See (http://www.iana.org/assignments/charset-reg/IBM00858)    [Mahdi]  }

    idcsIBM00924,
    idcsCCSID00924, {Alias of idcsIBM00924}
    idcsCP00924, {Alias of idcsIBM00924}
    idcsebcdic_Latin9__euro, {Alias of idcsIBM00924}
    { Source:
      IBM See (http://www.iana.org/assignments/charset-reg/IBM00924)    [Mahdi]  }

    idcsIBM01140,
    idcsCCSID01140, {Alias of idcsIBM01140}
    idcsCP01140, {Alias of idcsIBM01140}
    idcsebcdic_us_37_euro, {Alias of idcsIBM01140}
    { Source:
      IBM See (http://www.iana.org/assignments/charset-reg/IBM01140)    [Mahdi]  }

    idcsIBM01141,
    idcsCCSID01141, {Alias of idcsIBM01141}
    idcsCP01141, {Alias of idcsIBM01141}
    idcsebcdic_de_273_euro, {Alias of idcsIBM01141}
    { Source:
      IBM See (http://www.iana.org/assignments/charset-reg/IBM01141)    [Mahdi]  }

    idcsIBM01142,
    idcsCCSID01142, {Alias of idcsIBM01142}
    idcsCP01142, {Alias of idcsIBM01142}
    idcsebcdic_dk_277_euro, {Alias of idcsIBM01142}
    idcsebcdic_no_277_euro, {Alias of idcsIBM01142}
    { Source:
      IBM See (http://www.iana.org/assignments/charset-reg/IBM01142)    [Mahdi]  }

    idcsIBM01143,
    idcsCCSID01143, {Alias of idcsIBM01143}
    idcsCP01143, {Alias of idcsIBM01143}
    idcsebcdic_fi_278_euro, {Alias of idcsIBM01143}
    idcsebcdic_se_278_euro, {Alias of idcsIBM01143}
    { Source:
      IBM See (http://www.iana.org/assignments/charset-reg/IBM01143)    [Mahdi]  }

    idcsIBM01144,
    idcsCCSID01144, {Alias of idcsIBM01144}
    idcsCP01144, {Alias of idcsIBM01144}
    idcsebcdic_it_280_euro, {Alias of idcsIBM01144}
    { Source:
      IBM See (http://www.iana.org/assignments/charset-reg/IBM01144)    [Mahdi]  }

    idcsIBM01145,
    idcsCCSID01145, {Alias of idcsIBM01145}
    idcsCP01145, {Alias of idcsIBM01145}
    idcsebcdic_es_284_euro, {Alias of idcsIBM01145}
    { Source:
      IBM See (http://www.iana.org/assignments/charset-reg/IBM01145)    [Mahdi]  }

    idcsIBM01146,
    idcsCCSID01146, {Alias of idcsIBM01146}
    idcsCP01146, {Alias of idcsIBM01146}
    idcsebcdic_gb_285_euro, {Alias of idcsIBM01146}
    { Source:
      IBM See (http://www.iana.org/assignments/charset-reg/IBM01146)    [Mahdi]  }

    idcsIBM01147,
    idcsCCSID01147, {Alias of idcsIBM01147}
    idcsCP01147, {Alias of idcsIBM01147}
    idcsebcdic_fr_297_euro, {Alias of idcsIBM01147}
    { Source:
      IBM See (http://www.iana.org/assignments/charset-reg/IBM01147)    [Mahdi]  }

    idcsIBM01148,
    idcsCCSID01148, {Alias of idcsIBM01148}
    idcsCP01148, {Alias of idcsIBM01148}
    idcsebcdic_international_500_euro, {Alias of idcsIBM01148}
    { Source:
      IBM See (http://www.iana.org/assignments/charset-reg/IBM01148)    [Mahdi]  }

    idcsIBM01149,
    idcsCCSID01149, {Alias of idcsIBM01149}
    idcsCP01149, {Alias of idcsIBM01149}
    idcsebcdic_is_871_euro, {Alias of idcsIBM01149}
    { Source:
      IBM See (http://www.iana.org/assignments/charset-reg/IBM01149)    [Mahdi]  }

    idcsBig5_HKSCS,
    { Source:
      See (http://www.iana.org/assignments/charset-reg/Big5-HKSCS)     [Yick]  }

    idcsUNICODE_1_1,
    idcscsUnicode11, {Alias of idcsUNICODE_1_1}
    { References: RFC1641  }
    { Source:
      RFC 1641  }

    idcsSCSU,
    { Source:
      SCSU See (http://www.iana.org/assignments/charset-reg/SCSU)     [Scherer]  }

    idcsUTF_7,
    { References: RFC2152  }
    { Source:
      RFC 2152  }

    idcsUTF_16BE,
    { References: RFC2781  }
    { Source:
      RFC 2781  }

    idcsUTF_16LE,
    { References: RFC2781  }
    { Source:
      RFC 2781  }

    idcsUTF_16,
    { References: RFC2781  }
    { Source:
      RFC 2781  }

    idcsCESU_8,
    idcscsCESU_8, {Alias of idcsCESU_8}
    { References: Phipps  }
    { Source:
      <http://www.unicode.org/unicode/reports/tr26>  }

    idcsUTF_32,
    { References: Davis  }
    { Source:
      <http://www.unicode.org/unicode/reports/tr19/>  }

    idcsUTF_32BE,
    { References: Davis  }
    { Source:
      <http://www.unicode.org/unicode/reports/tr19/>  }

    idcsUTF_32LE,
    { References: Davis  }
    { Source:
      <http://www.unicode.org/unicode/reports/tr19/>  }

    idcsUNICODE_1_1_UTF_7,
    idcscsUnicode11UTF7, {Alias of idcsUNICODE_1_1_UTF_7}
    { References: RFC1642  }
    { Source:
      RFC 1642  }

    idcsUTF_8,
    { References: RFC2279  }
    { Source:
      RFC 2279  }

    idcsISO_8859_13,
    { Source:
      ISO See
      (http://www.iana.org/assignments/charset-reg/iso-8859-13)[Tumasonis]  }

    idcsISO_8859_14,
    idcsiso_ir_199, {Alias of idcsISO_8859_14}
    idcsISO_8859_14_1998, {Alias of idcsISO_8859_14}
    idcsISO_8859_14_, {Alias of idcsISO_8859_14}
    idcslatin8, {Alias of idcsISO_8859_14}
    idcsiso_celtic, {Alias of idcsISO_8859_14}
    idcsl8, {Alias of idcsISO_8859_14}
    { Source:
      ISO See (http://www.iana.org/assignments/charset-reg/iso-8859-14)
      [Simonsen]  }

    idcsISO_8859_15,
    idcsISO_8859_15_, {Alias of idcsISO_8859_15}
    { Source:
      ISO  }

    idcsISO_8859_16,
    { Source:
      ISO  }

    idcsGBK,
    idcsCP936, {Alias of idcsGBK}
    idcsMS936, {Alias of idcsGBK}
    idcswindows_936, {Alias of idcsGBK}
    { Source:
      Chinese IT Standardization Technical Committee Please see:
      <http://www.iana.org/assignments/charset-reg/GBK>  }

    idcsGB18030,
    { Source:
      Chinese IT Standardization Technical Committee Please see:
      <http://www.iana.org/assignments/charset-reg/GB18030>  }

    idcsJIS_Encoding,
    idcscsJISEncoding, {Alias of idcsJIS_Encoding}
    { Source:
      JIS X 0202-1991.  Uses ISO 2022 escape sequences to shift code sets as
      documented in JIS X 0202-1991.  }

    idcscsShiftJIS,
    idcsShift_JIS, {Alias of idcscsShiftJIS}
    idcsMS_Kanji, {Alias of idcscsShiftJIS}
    { Source:
      This charset is an extension of csHalfWidthKatakana by adding graphic
      characters in JIS X 0208.  The CCS's are JIS X0201:1997 and JIS
      X0208:1997.  The complete definition is shown in Appendix 1 of JIS
      X0208:1997. This charset can be used for the top-level media type "text".  }

    idcsEUC_JP,
    idcsExtended_UNIX_Code_Packed_Format_for_Japanese, {Alias of idcsEUC_JP}
    idcscsEUCPkdFmtJapanese, {Alias of idcsEUC_JP}
    { Source:
      Standardized by OSF, UNIX International, and UNIX Systems Laboratories
      Pacific.  Uses ISO 2022 rules to select code set 0: US-ASCII (a single
      7-bit byte set) code set 1: JIS X0208-1990 (a double 8-bit byte set)
      restricted to A0-FF in both bytes code set 2: Half Width Katakana (a
      single 7-bit byte set) requiring SS2 as the character prefix code set
      3: JIS X0212-1990 (a double 7-bit byte set) restricted to A0-FF in both
      bytes requiring SS3 as the character prefix  }

    idcsExtended_UNIX_Code_Fixed_Width_for_Japanese,
    idcscsEUCFixWidJapanese, {Alias of idcsExtended_UNIX_Code_Fixed_Width_for_Japanese}
    { Source:
      Used in Japan.  Each character is 2 octets. code set 0: US-ASCII (a
      single 7-bit byte set) 1st byte = 00 2nd byte = 20-7E code set 1: JIS
      X0208-1990 (a double 7-bit byte set) restricted  to A0-FF in both bytes
      code set 2: Half Width Katakana (a single 7-bit byte set) 1st byte = 00
      2nd byte = A0-FF code set 3: JIS X0212-1990 (a double 7-bit byte set)
      restricted to A0-FF in the first byte and 21-7E in the second byte  }

    idcsISO_10646_UCS_Basic,
    idcscsUnicodeASCII, {Alias of idcsISO_10646_UCS_Basic}
    { Source:
      ASCII subset of Unicode.  Basic Latin = collection 1 See ISO 10646,
      Appendix A  }

    idcsISO_10646_Unicode_Latin1,
    idcscsUnicodeLatin1, {Alias of idcsISO_10646_Unicode_Latin1}
    idcsISO_10646, {Alias of idcsISO_10646_Unicode_Latin1}
    { Source:
      ISO Latin-1 subset of Unicode. Basic Latin and Latin-1 Supplement  =
      collections 1 and 2.  See ISO 10646, Appendix A.  See RFC 1815.  }

    idcsISO_10646_J_1,
    { Source:
      ISO 10646 Japanese, see RFC 1815.  }

    idcsISO_Unicode_IBM_1261,
    idcscsUnicodeIBM1261, {Alias of idcsISO_Unicode_IBM_1261}
    { Source:
      IBM Latin-2, -3, -5, Extended Presentation Set, GCSGID: 1261  }

    idcsISO_Unicode_IBM_1268,
    idcscsUnicodeIBM1268, {Alias of idcsISO_Unicode_IBM_1268}
    { Source:
      IBM Latin-4 Extended Presentation Set, GCSGID: 1268  }

    idcsISO_Unicode_IBM_1276,
    idcscsUnicodeIBM1276, {Alias of idcsISO_Unicode_IBM_1276}
    { Source:
      IBM Cyrillic Greek Extended Presentation Set, GCSGID: 1276  }

    idcsISO_Unicode_IBM_1264,
    idcscsUnicodeIBM1264, {Alias of idcsISO_Unicode_IBM_1264}
    { Source:
      IBM Arabic Presentation Set, GCSGID: 1264  }

    idcsISO_Unicode_IBM_1265,
    idcscsUnicodeIBM1265, {Alias of idcsISO_Unicode_IBM_1265}
    { Source:
      IBM Hebrew Presentation Set, GCSGID: 1265  }

    idcsISO_8859_1_Windows_3_0_Latin_1,
    idcscsWindows30Latin1, {Alias of idcsISO_8859_1_Windows_3_0_Latin_1}
    { References: HP-PCL5  }
    { Source:
      Extended ISO 8859-1 Latin-1 for Windows 3.0. PCL Symbol Set id: 9U  }

    idcsISO_8859_1_Windows_3_1_Latin_1,
    idcscsWindows31Latin1, {Alias of idcsISO_8859_1_Windows_3_1_Latin_1}
    { References: HP-PCL5  }
    { Source:
      Extended ISO 8859-1 Latin-1 for Windows 3.1. PCL Symbol Set id: 19U  }

    idcsISO_8859_2_Windows_Latin_2,
    idcscsWindows31Latin2, {Alias of idcsISO_8859_2_Windows_Latin_2}
    { References: HP-PCL5  }
    { Source:
      Extended ISO 8859-2.  Latin-2 for Windows 3.1. PCL Symbol Set id: 9E  }

    idcsISO_8859_9_Windows_Latin_5,
    idcscsWindows31Latin5, {Alias of idcsISO_8859_9_Windows_Latin_5}
    { References: HP-PCL5  }
    { Source:
      Extended ISO 8859-9.  Latin-5 for Windows 3.1 PCL Symbol Set id: 5T  }

    idcsAdobe_Standard_Encoding,
    idcscsAdobeStandardEncoding, {Alias of idcsAdobe_Standard_Encoding}
    { References: Adobe  }
    { Source:
      PostScript Language Reference Manual PCL Symbol Set id: 10J  }

    idcsVentura_US,
    idcscsVenturaUS, {Alias of idcsVentura_US}
    { References: HP-PCL5  }
    { Source:
      Ventura US.  ASCII plus characters typically used in publishing, like
      pilcrow, copyright, registered, trade mark, section, dagger, and double
      dagger in the range A0 (hex) to FF (hex). PCL Symbol Set id: 14J  }

    idcsVentura_International,
    idcscsVenturaInternational, {Alias of idcsVentura_International}
    { References: HP-PCL5  }
    { Source:
      Ventura International.  ASCII plus coded characters similar to Roman8.
      PCL Symbol Set id: 13J  }

    idcsPC8_Danish_Norwegian,
    idcscsPC8DanishNorwegian, {Alias of idcsPC8_Danish_Norwegian}
    { References: HP-PCL5  }
    { Source:
      PC Danish Norwegian 8-bit PC set for Danish Norwegian PCL Symbol Set
      id: 11U  }

    idcsPC8_Turkish,
    idcscsPC8Turkish, {Alias of idcsPC8_Turkish}
    { References: HP-PCL5  }
    { Source:
      PC Latin Turkish.  PCL Symbol Set id: 9T  }

    idcsIBM_Symbols,
    idcscsIBMSymbols, {Alias of idcsIBM_Symbols}
    { References: IBM-CIDT  }
    { Source:
      Presentation Set, CPGID: 259  }

    idcsIBM_Thai,
    idcscsIBMThai, {Alias of idcsIBM_Thai}
    { References: IBM-CIDT  }
    { Source:
      Presentation Set, CPGID: 838  }

    idcsHP_Legal,
    idcscsHPLegal, {Alias of idcsHP_Legal}
    { References: HP-PCL5  }
    { Source:
      PCL 5 Comparison Guide, Hewlett-Packard, HP part number 5961-0510,
      October 1992 PCL Symbol Set id: 1U  }

    idcsHP_Pi_font,
    idcscsHPPiFont, {Alias of idcsHP_Pi_font}
    { References: HP-PCL5  }
    { Source:
      PCL 5 Comparison Guide, Hewlett-Packard, HP part number 5961-0510,
      October 1992 PCL Symbol Set id: 15U  }

    idcsHP_Math8,
    idcscsHPMath8, {Alias of idcsHP_Math8}
    { References: HP-PCL5  }
    { Source:
      PCL 5 Comparison Guide, Hewlett-Packard, HP part number 5961-0510,
      October 1992 PCL Symbol Set id: 8M  }

    idcsAdobe_Symbol_Encoding,
    idcscsHPPSMath, {Alias of idcsAdobe_Symbol_Encoding}
    { References: Adobe  }
    { Source:
      PostScript Language Reference Manual PCL Symbol Set id: 5M  }

    idcsHP_DeskTop,
    idcscsHPDesktop, {Alias of idcsHP_DeskTop}
    { References: HP-PCL5  }
    { Source:
      PCL 5 Comparison Guide, Hewlett-Packard, HP part number 5961-0510,
      October 1992 PCL Symbol Set id: 7J  }

    idcsVentura_Math,
    idcscsVenturaMath, {Alias of idcsVentura_Math}
    { References: HP-PCL5  }
    { Source:
      PCL 5 Comparison Guide, Hewlett-Packard, HP part number 5961-0510,
      October 1992 PCL Symbol Set id: 6M  }

    idcsMicrosoft_Publishing,
    idcscsMicrosoftPublishing, {Alias of idcsMicrosoft_Publishing}
    { References: HP-PCL5  }
    { Source:
      PCL 5 Comparison Guide, Hewlett-Packard, HP part number 5961-0510,
      October 1992 PCL Symbol Set id: 6J  }

    idcsWindows_31J,
    idcscsWindows31J, {Alias of idcsWindows_31J}
    { Source:
      Windows Japanese.  A further extension of Shift_JIS to include NEC
      special characters (Row 13), NEC selection of IBM extensions (Rows 89
      to 92), and IBM extensions (Rows 115 to 119).  The CCS's are JIS
      X0201:1997, JIS X0208:1997, and these extensions. This charset can be
      used for the top-level media type "text", but it is of limited or
      specialized use (see RFC2278). PCL Symbol Set id: 19K  }

    idcscsGB2312,
    idcsGB2312, {Alias of idcscsGB2312}
    { Source:
      Chinese for People's Republic of China (PRC) mixed one byte, two byte
      set: 20-7E = one byte ASCII A1-FE = two byte PRC Kanji See GB 2312-80
      PCL Symbol Set Id: 18C  }

    idcscsBig5,
    idcsBig5, {Alias of idcscsBig5}
    { Source:
      Chinese for Taiwan Multi-byte set. PCL Symbol Set Id: 18T  }

    idcswindows_1250,
    { Source:
      Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1250)
      [Lazhintseva]  }

    idcswindows_1251,
    { Source:
      Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1251)
      [Lazhintseva]  }

    idcswindows_1252,
    { Source:
      Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1252)
          [Wendt]  }

    idcswindows_1253,
    { Source:
      Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1253)
      [Lazhintseva]  }

    idcswindows_1254,
    { Source:
      Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1254)
      [Lazhintseva]  }

    idcswindows_1255,
    { Source:
      Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1255)
      [Lazhintseva]  }

    idcswindows_1256,
    { Source:
      Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1256)
      [Lazhintseva]  }

    idcswindows_1257,
    { Source:
      Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1257)
      [Lazhintseva]  }

    idcswindows_1258,

    idcsTIS_620,
    { Source:
      Thai Industrial Standards Institute (TISI)	     [Tantsetthi]  }

    idcsHZ_GB_2312
    { Source:
      RFC 1842, RFC 1843                              [RFC1842, RFC1843]  }


  );


const
  IdCharsetNames : array[Low(TIdCharSet)..High(TIdCharSet)] of string = (
    '',                         {invalid is empty}
    'US-ASCII',                 {do not localize}
    'ANSI_X3.4-1968',           {do not localize}
    'iso-ir-6',                 {do not localize}
    'ANSI_X3.4-1986',           {do not localize}
    'ISO_646.irv:1991',         {do not localize}
    'ASCII',                    {do not localize}
    'ISO646-US',                {do not localize}
    'us',                       {do not localize}
    'IBM367',                   {do not localize}
    'cp367',                    {do not localize}
    'csASCII',                  {do not localize}
    'ISO-10646-UTF-1',          {do not localize}
    'csISO10646UTF1',           {do not localize}
    'ISO_646.basic:1983',       {do not localize}
    'ref',                      {do not localize}
    'csISO646basic1983',        {do not localize}
    'INVARIANT',                {do not localize}
    'csINVARIANT',              {do not localize}
    'ISO_646.irv:1983',         {do not localize}
    'iso-ir-2',                 {do not localize}
    'irv',                      {do not localize}
    'csISO2IntlRefVersion',     {do not localize}
    'BS_4730',                  {do not localize}
    'iso-ir-4',                 {do not localize}
    'ISO646-GB',                {do not localize}
    'gb',                       {do not localize}
    'uk',                       {do not localize}
    'csISO4UnitedKingdom',      {do not localize}
    'NATS-SEFI',                {do not localize}
    'iso-ir-8-1',               {do not localize}
    'csNATSSEFI',               {do not localize}
    'NATS-SEFI-ADD',            {do not localize}
    'iso-ir-8-2',               {do not localize}
    'csNATSSEFIADD',            {do not localize}
    'NATS-DANO',                {do not localize}
    'iso-ir-9-1',               {do not localize}
    'csNATSDANO',               {do not localize}
    'NATS-DANO-ADD',            {do not localize}
    'iso-ir-9-2',               {do not localize}
    'csNATSDANOADD',            {do not localize}
    'SEN_850200_B',             {do not localize}
    'iso-ir-10',                {do not localize}
    'FI',                       {do not localize}
    'ISO646-FI',                {do not localize}
    'ISO646-SE',                {do not localize}
    'se',                       {do not localize}
    'csISO10Swedish',           {do not localize}
    'SEN_850200_C',             {do not localize}
    'iso-ir-11',                {do not localize}
    'ISO646-SE2',               {do not localize}
    'se2',                      {do not localize}
    'csISO11SwedishForNames',   {do not localize}
    'KS_C_5601-1987',           {do not localize}
    'iso-ir-149',               {do not localize}
    'KS_C_5601-1989',           {do not localize}
    'KSC_5601',                 {do not localize}
    'korean',                   {do not localize}
    'csKSC56011987',            {do not localize}
    'csISO2022KR',              {do not localize}
    'ISO-2022-KR',              {do not localize}
    'csEUCKR',                  {do not localize}
    'EUC-KR',                   {do not localize}
    'csISO2022JP',              {do not localize}
    'ISO-2022-JP',              {do not localize}
    'csISO2022JP2',             {do not localize}
    'ISO-2022-JP-2',            {do not localize}
    'ISO-2022-CN',              {do not localize}
    'ISO-2022-CN-EXT',          {do not localize}
    'JIS_C6220-1969-jp',        {do not localize}
    'JIS_C6220-1969',           {do not localize}
    'iso-ir-13',                {do not localize}
    'katakana',                 {do not localize}
    'x0201-7',                  {do not localize}
    'csISO13JISC6220jp',        {do not localize}
    'JIS_C6220-1969-ro',        {do not localize}
    'iso-ir-14',                {do not localize}
    'jp',                       {do not localize}
    'ISO646-JP',                {do not localize}
    'csISO14JISC6220ro',        {do not localize}
    'IT',                       {do not localize}
    'iso-ir-15',                {do not localize}
    'ISO646-IT',                {do not localize}
    'csISO15Italian',           {do not localize}
    'PT',                       {do not localize}
    'iso-ir-16',                {do not localize}
    'ISO646-PT',                {do not localize}
    'csISO16Portuguese',        {do not localize}
    'ES',                       {do not localize}
    'iso-ir-17',                {do not localize}
    'ISO646-ES',                {do not localize}
    'csISO17Spanish',           {do not localize}
    'greek7-old',               {do not localize}
    'iso-ir-18',                {do not localize}
    'csISO18Greek7Old',         {do not localize}
    'latin-greek',              {do not localize}
    'iso-ir-19',                {do not localize}
    'csISO19LatinGreek',        {do not localize}
    'DIN_66003',                {do not localize}
    'iso-ir-21',                {do not localize}
    'de',                       {do not localize}
    'ISO646-DE',                {do not localize}
    'csISO21German',            {do not localize}
    'csISO25French',            {do not localize}
    'NF_Z_62-010_',             {do not localize}
    'iso-ir-25',                {do not localize}
    'ISO646-FR1',               {do not localize}
    'Latin-greek-1',            {do not localize}
    'iso-ir-27',                {do not localize}
    'csISO27LatinGreek1',       {do not localize}
    'ISO_5427',                 {do not localize}
    'iso-ir-37',                {do not localize}
    'csISO5427Cyrillic',        {do not localize}
    'JIS_C6226-1978',           {do not localize}
    'iso-ir-42',                {do not localize}
    'csISO42JISC62261978',      {do not localize}
    'BS_viewdata',              {do not localize}
    'iso-ir-47',                {do not localize}
    'csISO47BSViewdata',        {do not localize}
    'INIS',                     {do not localize}
    'iso-ir-49',                {do not localize}
    'csISO49INIS',              {do not localize}
    'INIS-8',                   {do not localize}
    'iso-ir-50',                {do not localize}
    'csISO50INIS8',             {do not localize}
    'INIS-cyrillic',            {do not localize}
    'iso-ir-51',                {do not localize}
    'csISO51INISCyrillic',      {do not localize}
    'ISO_5427:1981',            {do not localize}
    'iso-ir-54',                {do not localize}
    'ISO5427Cyrillic1981',      {do not localize}
    'ISO_5428:1980',            {do not localize}
    'iso-ir-55',                {do not localize}
    'csISO5428Greek',           {do not localize}
    'GB_1988-80',               {do not localize}
    'iso-ir-57',                {do not localize}
    'cn',                       {do not localize}
    'ISO646-CN',                {do not localize}
    'csISO57GB1988',            {do not localize}
    'GB_2312-80',               {do not localize}
    'iso-ir-58',                {do not localize}
    'chinese',                  {do not localize}
    'csISO58GB231280',          {do not localize}
    'NS_4551-1',                {do not localize}
    'iso-ir-60',                {do not localize}
    'ISO646-NO',                {do not localize}
    'no',                       {do not localize}
    'csISO60DanishNorwegian',   {do not localize}
    'csISO60Norwegian1',        {do not localize}
    'NS_4551-2',                {do not localize}
    'ISO646-NO2',               {do not localize}
    'iso-ir-61',                {do not localize}
    'no2',                      {do not localize}
    'csISO61Norwegian2',        {do not localize}
    'NF_Z_62-010',              {do not localize}
    'iso-ir-69',                {do not localize}
    'ISO646-FR',                {do not localize}
    'fr',                       {do not localize}
    'csISO69French',            {do not localize}
    'videotex-suppl',           {do not localize}
    'iso-ir-70',                {do not localize}
    'csISO70VideotexSupp1',     {do not localize}
    'PT2',                      {do not localize}
    'iso-ir-84',                {do not localize}
    'ISO646-PT2',               {do not localize}
    'csISO84Portuguese2',       {do not localize}
    'ES2',                      {do not localize}
    'iso-ir-85',                {do not localize}
    'ISO646-ES2',               {do not localize}
    'csISO85Spanish2',          {do not localize}
    'MSZ_7795.3',               {do not localize}
    'iso-ir-86',                {do not localize}
    'ISO646-HU',                {do not localize}
    'hu',                       {do not localize}
    'csISO86Hungarian',         {do not localize}
    'JIS_C6226-1983',           {do not localize}
    'iso-ir-87',                {do not localize}
    'x0208',                    {do not localize}
    'JIS_X0208-1983',           {do not localize}
    'csISO87JISX0208',          {do not localize}
    'greek7',                   {do not localize}
    'iso-ir-88',                {do not localize}
    'csISO88Greek7',            {do not localize}
    'ASMO_449',                 {do not localize}
    'ISO_9036',                 {do not localize}
    'arabic7',                  {do not localize}
    'iso-ir-89',                {do not localize}
    'csISO89ASMO449',           {do not localize}
    'iso-ir-90',                {do not localize}
    'csISO90',                  {do not localize}
    'JIS_C6229-1984-a',         {do not localize}
    'iso-ir-91',                {do not localize}
    'jp-ocr-a',                 {do not localize}
    'csISO91JISC62291984a',     {do not localize}
    'JIS_C6229-1984-b',         {do not localize}
    'iso-ir-92',                {do not localize}
    'ISO646-JP-OCR-B',          {do not localize}
    'jp-ocr-b',                 {do not localize}
    'csISO92JISC62991984b',     {do not localize}
    'JIS_C6229-1984-b-add',     {do not localize}
    'iso-ir-93',                {do not localize}
    'jp-ocr-b-add',             {do not localize}
    'csISO93JIS62291984badd',   {do not localize}
    'JIS_C6229-1984-hand',      {do not localize}
    'iso-ir-94',                {do not localize}
    'jp-ocr-hand',              {do not localize}
    'csISO94JIS62291984hand',   {do not localize}
    'JIS_C6229-1984-hand-add',  {do not localize}
    'iso-ir-95',                {do not localize}
    'jp-ocr-hand-add',          {do not localize}
    'csISO95JIS62291984handadd',{do not localize}
    'JIS_C6229-1984-kana',      {do not localize}
    'iso-ir-96',                {do not localize}
    'csISO96JISC62291984kana',  {do not localize}
    'ISO_2033-1983',            {do not localize}
    'iso-ir-98',                {do not localize}
    'e13b',                     {do not localize}
    'csISO2033',                {do not localize}
    'ANSI_X3.110-1983',         {do not localize}
    'iso-ir-99',                {do not localize}
    'CSA_T500-1983',            {do not localize}
    'NAPLPS',                   {do not localize}
    'csISO99NAPLPS',            {do not localize}
    'ISO-8859-1',               {do not localize}
    'ISO_8859-1:1987',          {do not localize}
    'iso-ir-100',               {do not localize}
    'ISO_8859-1',               {do not localize}
    'latin1',                   {do not localize}
    'l1',                       {do not localize}
    'IBM819',                   {do not localize}
    'CP819',                    {do not localize}
    'csISOLatin1',              {do not localize}
    'ISO-8859-2',               {do not localize}
    'ISO_8859-2:1987',          {do not localize}
    'iso-ir-101',               {do not localize}
    'ISO_8859-2',               {do not localize}
    'latin2',                   {do not localize}
    'l2',                       {do not localize}
    'csISOLatin2',              {do not localize}
    'T.61-7bit',                {do not localize}
    'iso-ir-102',               {do not localize}
    'csISO102T617bit',          {do not localize}
    'T.61-8bit',                {do not localize}
    'T.61',                     {do not localize}
    'iso-ir-103',               {do not localize}
    'csISO103T618bit',          {do not localize}
    'ISO-8859-3',               {do not localize}
    'ISO_8859-3:1988',          {do not localize}
    'iso-ir-109',               {do not localize}
    'ISO_8859-3',               {do not localize}
    'latin3',                   {do not localize}
    'l3',                       {do not localize}
    'csISOLatin3',              {do not localize}
    'ISO-8859-4',               {do not localize}
    'ISO_8859-4:1988',          {do not localize}
    'iso-ir-110',               {do not localize}
    'ISO_8859-4',               {do not localize}
    'latin4',                   {do not localize}
    'l4',                       {do not localize}
    'csISOLatin4',              {do not localize}
    'ECMA-cyrillic',            {do not localize}
    'iso-ir-111',               {do not localize}
    'csISO111ECMACyrillic',     {do not localize}
    'CSA_Z243.4-1985-1',        {do not localize}
    'iso-ir-121',               {do not localize}
    'ISO646-CA',                {do not localize}
    'csa7-1',                   {do not localize}
    'ca',                       {do not localize}
    'csISO121Canadian1',        {do not localize}
    'CSA_Z243.4-1985-2',        {do not localize}
    'iso-ir-122',               {do not localize}
    'ISO646-CA2',               {do not localize}
    'csa7-2',                   {do not localize}
    'csISO122Canadian2',        {do not localize}
    'CSA_Z243.4-1985-gr',       {do not localize}
    'iso-ir-123',               {do not localize}
    'csISO123CSAZ24341985gr',   {do not localize}
    'ISO-8859-6',               {do not localize}
    'ISO_8859-6:1987',          {do not localize}
    'iso-ir-127',               {do not localize}
    'ISO_8859-6',               {do not localize}
    'ECMA-114',                 {do not localize}
    'ASMO-708',                 {do not localize}
    'arabic',                   {do not localize}
    'csISOLatinArabic',         {do not localize}
    'ISO-8859-6-E',             {do not localize}
    'ISO_8859-6-E',             {do not localize}
    'csISO88596E',              {do not localize}
    'ISO-8859-6-I',             {do not localize}
    'ISO_8859-6-I',             {do not localize}
    'csISO88596I',              {do not localize}
    'ISO-8859-7',               {do not localize}
    'ISO_8859-7:1987',          {do not localize}
    'iso-ir-126',               {do not localize}
    'ISO_8859-7',               {do not localize}
    'ELOT_928',                 {do not localize}
    'ECMA-118',                 {do not localize}
    'greek',                    {do not localize}
    'greek8',                   {do not localize}
    'csISOLatinGreek',          {do not localize}
    'T.101-G2',                 {do not localize}
    'iso-ir-128',               {do not localize}
    'csISO128T101G2',           {do not localize}
    'ISO-8859-8',               {do not localize}
    'ISO_8859-8:1988',          {do not localize}
    'iso-ir-138',               {do not localize}
    'ISO_8859-8',               {do not localize}
    'hebrew',                   {do not localize}
    'csISOLatinHebrew',         {do not localize}
    'ISO-8859-8-E',             {do not localize}
    'ISO_8859-8-E',             {do not localize}
    'csISO88598E',              {do not localize}
    'ISO-8859-8-I',             {do not localize}
    'ISO_8859-8-I',             {do not localize}
    'csISO88598I',              {do not localize}
    'CSN_369103',               {do not localize}
    'iso-ir-139',               {do not localize}
    'csISO139CSN369103',        {do not localize}
    'JUS_I.B1.002',             {do not localize}
    'iso-ir-141',               {do not localize}
    'ISO646-YU',                {do not localize}
    'js',                       {do not localize}
    'yu',                       {do not localize}
    'csISO141JUSIB1002',        {do not localize}
    'ISO_6937-2-add',           {do not localize}
    'iso-ir-142',               {do not localize}
    'csISOTextComm',            {do not localize}
    'IEC_P27-1',                {do not localize}
    'iso-ir-143',               {do not localize}
    'csISO143IECP271',          {do not localize}
    'ISO-8859-5',               {do not localize}
    'ISO_8859-5:1988',          {do not localize}
    'iso-ir-144',               {do not localize}
    'ISO_8859-5',               {do not localize}
    'cyrillic',                 {do not localize}
    'csISOLatinCyrillic',       {do not localize}
    'JUS_I.B1.003-serb',        {do not localize}
    'iso-ir-146',               {do not localize}
    'serbian',                  {do not localize}
    'csISO146Serbian',          {do not localize}
    'JUS_I.B1.003-mac',         {do not localize}
    'macedonian',               {do not localize}
    'iso-ir-147',               {do not localize}
    'csISO147Macedonian',       {do not localize}
    'ISO-8859-9',               {do not localize}
    'ISO_8859-9:1989',          {do not localize}
    'iso-ir-148',               {do not localize}
    'ISO_8859-9',               {do not localize}
    'latin5',                   {do not localize}
    'l5',                       {do not localize}
    'csISOLatin5',              {do not localize}
    'greek-ccitt',              {do not localize}
    'iso-ir-150',               {do not localize}
    'csISO150',                 {do not localize}
    'csISO150GreekCCITT',       {do not localize}
    'NC_NC00-10:81',            {do not localize}
    'cuba',                     {do not localize}
    'iso-ir-151',               {do not localize}
    'ISO646-CU',                {do not localize}
    'csISO151Cuba',             {do not localize}
    'ISO_6937-2-25',            {do not localize}
    'iso-ir-152',               {do not localize}
    'csISO6937Add',             {do not localize}
    'GOST_19768-74',            {do not localize}
    'ST_SEV_358-88',            {do not localize}
    'iso-ir-153',               {do not localize}
    'csISO153GOST1976874',      {do not localize}
    'ISO_8859-supp',            {do not localize}
    'iso-ir-154',               {do not localize}
    'latin1-2-5',               {do not localize}
    'csISO8859Supp',            {do not localize}
    'ISO_10367-box',            {do not localize}
    'iso-ir-155',               {do not localize}
    'csISO10367Box',            {do not localize}
    'latin6',                   {do not localize}
    'ISO-8859-10',              {do not localize}
    'iso-ir-157',               {do not localize}
    'l6',                       {do not localize}
    'ISO_8859-10:1992',         {do not localize}
    'csISOLatin6',              {do not localize}
    'latin-lap',                {do not localize}
    'lap',                      {do not localize}
    'iso-ir-158',               {do not localize}
    'csISO158Lap',              {do not localize}
    'JIS_X0212-1990',           {do not localize}
    'x0212',                    {do not localize}
    'iso-ir-159',               {do not localize}
    'csISO159JISX02121990',     {do not localize}
    'DS_2089',                  {do not localize}
    'DS2089',                   {do not localize}
    'ISO646-DK',                {do not localize}
    'dk',                       {do not localize}
    'csISO646Danish',           {do not localize}
    'us-dk',                    {do not localize}
    'csUSDK',                   {do not localize}
    'dk-us',                    {do not localize}
    'csDKUS',                   {do not localize}
    'JIS_X0201',                {do not localize}
    'X0201',                    {do not localize}
    'csHalfWidthKatakana',      {do not localize}
    'KSC5636',                  {do not localize}
    'ISO646-KR',                {do not localize}
    'csKSC5636',                {do not localize}
    'ISO-10646-UCS-2',          {do not localize}
    'csUnicode',                {do not localize}
    'ISO-10646-UCS-4',          {do not localize}
    'csUCS4',                   {do not localize}
    'DEC-MCS',                  {do not localize}
    'dec',                      {do not localize}
    'csDECMCS',                 {do not localize}
    'hp-roman8',                {do not localize}
    'roman8',                   {do not localize}
    'r8',                       {do not localize}
    'csHPRoman8',               {do not localize}
    'macintosh',                {do not localize}
    'mac',                      {do not localize}
    'csMacintosh',              {do not localize}
    'IBM037',                   {do not localize}
    'cp037',                    {do not localize}
    'ebcdic-cp-us',             {do not localize}
    'ebcdic-cp-ca',             {do not localize}
    'ebcdic-cp-wt',             {do not localize}
    'ebcdic-cp-nl',             {do not localize}
    'csIBM037',                 {do not localize}
    'IBM038',                   {do not localize}
    'EBCDIC-INT',               {do not localize}
    'cp038',                    {do not localize}
    'csIBM038',                 {do not localize}
    'IBM273',                   {do not localize}
    'CP273',                    {do not localize}
    'csIBM273',                 {do not localize}
    'IBM274',                   {do not localize}
    'EBCDIC-BE',                {do not localize}
    'CP274',                    {do not localize}
    'csIBM274',                 {do not localize}
    'IBM275',                   {do not localize}
    'EBCDIC-BR',                {do not localize}
    'cp275',                    {do not localize}
    'csIBM275',                 {do not localize}
    'IBM277',                   {do not localize}
    'EBCDIC-CP-DK',             {do not localize}
    'EBCDIC-CP-NO',             {do not localize}
    'csIBM277',                 {do not localize}
    'IBM278',                   {do not localize}
    'CP278',                    {do not localize}
    'ebcdic-cp-fi',             {do not localize}
    'ebcdic-cp-se',             {do not localize}
    'csIBM278',                 {do not localize}
    'IBM280',                   {do not localize}
    'CP280',                    {do not localize}
    'ebcdic-cp-it',             {do not localize}
    'csIBM280',                 {do not localize}
    'IBM281',                   {do not localize}
    'EBCDIC-JP-E',              {do not localize}
    'cp281',                    {do not localize}
    'csIBM281',                 {do not localize}
    'IBM284',                   {do not localize}
    'CP284',                    {do not localize}
    'ebcdic-cp-es',             {do not localize}
    'csIBM284',                 {do not localize}
    'IBM285',                   {do not localize}
    'CP285',                    {do not localize}
    'ebcdic-cp-gb',             {do not localize}
    'csIBM285',                 {do not localize}
    'IBM290',                   {do not localize}
    'cp290',                    {do not localize}
    'EBCDIC-JP-kana',           {do not localize}
    'csIBM290',                 {do not localize}
    'IBM297',                   {do not localize}
    'cp297',                    {do not localize}
    'ebcdic-cp-fr',             {do not localize}
    'csIBM297',                 {do not localize}
    'IBM420',                   {do not localize}
    'cp420',                    {do not localize}
    'ebcdic-cp-ar1',            {do not localize}
    'csIBM420',                 {do not localize}
    'IBM423',                   {do not localize}
    'cp423',                    {do not localize}
    'ebcdic-cp-gr',             {do not localize}
    'csIBM423',                 {do not localize}
    'IBM424',                   {do not localize}
    'cp424',                    {do not localize}
    'ebcdic-cp-he',             {do not localize}
    'csIBM424',                 {do not localize}
    'IBM437',                   {do not localize}
    'cp437',                    {do not localize}
    '437',                      {do not localize}
    'csPC8CodePage437',         {do not localize}
    'IBM500',                   {do not localize}
    'CP500',                    {do not localize}
    'ebcdic-cp-be',             {do not localize}
    'ebcdic-cp-ch',             {do not localize}
    'csIBM500',                 {do not localize}
    'IBM775',                   {do not localize}
    'cp775',                    {do not localize}
    'csPC775Baltic',            {do not localize}
    'IBM850',                   {do not localize}
    'cp850',                    {do not localize}
    '850',                      {do not localize}
    'csPC850Multilingual',      {do not localize}
    'IBM851',                   {do not localize}
    'cp851',                    {do not localize}
    '851',                      {do not localize}
    'csIBM851',                 {do not localize}
    'IBM852',                   {do not localize}
    'cp852',                    {do not localize}
    '852',                      {do not localize}
    'csPCp852',                 {do not localize}
    'IBM855',                   {do not localize}
    'cp855',                    {do not localize}
    '855',                      {do not localize}
    'csIBM855',                 {do not localize}
    'IBM857',                   {do not localize}
    'cp857',                    {do not localize}
    '857',                      {do not localize}
    'csIBM857',                 {do not localize}
    'IBM860',                   {do not localize}
    'cp860',                    {do not localize}
    '860',                      {do not localize}
    'csIBM860',                 {do not localize}
    'IBM861',                   {do not localize}
    'cp861',                    {do not localize}
    '861',                      {do not localize}
    'cp-is',                    {do not localize}
    'csIBM861',                 {do not localize}
    'IBM862',                   {do not localize}
    'cp862',                    {do not localize}
    '862',                      {do not localize}
    'csPC862LatinHebrew',       {do not localize}
    'IBM863',                   {do not localize}
    'cp863',                    {do not localize}
    '863',                      {do not localize}
    'csIBM863',                 {do not localize}
    'IBM864',                   {do not localize}
    'cp864',                    {do not localize}
    'csIBM864',                 {do not localize}
    'IBM865',                   {do not localize}
    'cp865',                    {do not localize}
    '865',                      {do not localize}
    'csIBM865',                 {do not localize}
    'IBM866',                   {do not localize}
    'cp866',                    {do not localize}
    '866',                      {do not localize}
    'csIBM866',                 {do not localize}
    'IBM868',                   {do not localize}
    'CP868',                    {do not localize}
    'cp-ar',                    {do not localize}
    'csIBM868',                 {do not localize}
    'IBM869',                   {do not localize}
    'cp869',                    {do not localize}
    '869',                      {do not localize}
    'cp-gr',                    {do not localize}
    'csIBM869',                 {do not localize}
    'IBM870',                   {do not localize}
    'CP870',                    {do not localize}
    'ebcdic-cp-roece',          {do not localize}
    'ebcdic-cp-yu',             {do not localize}
    'csIBM870',                 {do not localize}
    'IBM871',                   {do not localize}
    'CP871',                    {do not localize}
    'ebcdic-cp-is',             {do not localize}
    'csIBM871',                 {do not localize}
    'IBM880',                   {do not localize}
    'cp880',                    {do not localize}
    'EBCDIC-Cyrillic',          {do not localize}
    'csIBM880',                 {do not localize}
    'IBM891',                   {do not localize}
    'cp891',                    {do not localize}
    'csIBM891',                 {do not localize}
    'IBM903',                   {do not localize}
    'cp903',                    {do not localize}
    'csIBM903',                 {do not localize}
    'IBM904',                   {do not localize}
    'cp904',                    {do not localize}
    '904',                      {do not localize}
    'csIBBM904',                {do not localize}
    'IBM905',                   {do not localize}
    'CP905',                    {do not localize}
    'ebcdic-cp-tr',             {do not localize}
    'csIBM905',                 {do not localize}
    'IBM918',                   {do not localize}
    'CP918',                    {do not localize}
    'ebcdic-cp-ar2',            {do not localize}
    'csIBM918',                 {do not localize}
    'IBM1026',                  {do not localize}
    'CP1026',                   {do not localize}
    'csIBM1026',                {do not localize}
    'EBCDIC-AT-DE',             {do not localize}
    'csIBMEBCDICATDE',          {do not localize}
    'EBCDIC-AT-DE-A',           {do not localize}
    'csEBCDICATDEA',            {do not localize}
    'EBCDIC-CA-FR',             {do not localize}
    'csEBCDICCAFR',             {do not localize}
    'EBCDIC-DK-NO',             {do not localize}
    'csEBCDICDKNO',             {do not localize}
    'EBCDIC-DK-NO-A',           {do not localize}
    'csEBCDICDKNOA',            {do not localize}
    'EBCDIC-FI-SE',             {do not localize}
    'csEBCDICFISE',             {do not localize}
    'EBCDIC-FI-SE-A',           {do not localize}
    'csEBCDICFISEA',            {do not localize}
    'EBCDIC-FR',                {do not localize}
    'csEBCDICFR',               {do not localize}
    'EBCDIC-IT',                {do not localize}
    'csEBCDICIT',               {do not localize}
    'EBCDIC-PT',                {do not localize}
    'csEBCDICPT',               {do not localize}
    'EBCDIC-ES',                {do not localize}
    'csEBCDICES',               {do not localize}
    'EBCDIC-ES-A',              {do not localize}
    'csEBCDICESA',              {do not localize}
    'EBCDIC-ES-S',              {do not localize}
    'csEBCDICESS',              {do not localize}
    'EBCDIC-UK',                {do not localize}
    'csEBCDICUK',               {do not localize}
    'EBCDIC-US',                {do not localize}
    'csEBCDICUS',               {do not localize}
    'UNKNOWN-8BIT',             {do not localize}
    'csUnknown8BiT',            {do not localize}
    'MNEMONIC',                 {do not localize}
    'csMnemonic',               {do not localize}
    'MNEM',                     {do not localize}
    'csMnem',                   {do not localize}
    'VISCII',                   {do not localize}
    'csVISCII',                 {do not localize}
    'VIQR',                     {do not localize}
    'csVIQR',                   {do not localize}
    'csKOI8R',                  {do not localize}
    'KOI8-R',                   {do not localize}
    'KOI8-U',                   {do not localize}
    'IBM00858',                 {do not localize}
    'CCSID00858',               {do not localize}
    'CP00858',                  {do not localize}
    'PC-Multilingual-850+euro', {do not localize}
    'IBM00924',                 {do not localize}
    'CCSID00924',               {do not localize}
    'CP00924',                  {do not localize}
    'ebcdic-Latin9--euro',      {do not localize}
    'IBM01140',                 {do not localize}
    'CCSID01140',               {do not localize}
    'CP01140',                  {do not localize}
    'ebcdic-us-37+euro',        {do not localize}
    'IBM01141',                 {do not localize}
    'CCSID01141',               {do not localize}
    'CP01141',                  {do not localize}
    'ebcdic-de-273+euro',       {do not localize}
    'IBM01142',                 {do not localize}
    'CCSID01142',               {do not localize}
    'CP01142',                  {do not localize}
    'ebcdic-dk-277+euro',       {do not localize}
    'ebcdic-no-277+euro',       {do not localize}
    'IBM01143',                 {do not localize}
    'CCSID01143',               {do not localize}
    'CP01143',                  {do not localize}
    'ebcdic-fi-278+euro',       {do not localize}
    'ebcdic-se-278+euro',       {do not localize}
    'IBM01144',                 {do not localize}
    'CCSID01144',               {do not localize}
    'CP01144',                  {do not localize}
    'ebcdic-it-280+euro',       {do not localize}
    'IBM01145',                 {do not localize}
    'CCSID01145',               {do not localize}
    'CP01145',                  {do not localize}
    'ebcdic-es-284+euro',       {do not localize}
    'IBM01146',                 {do not localize}
    'CCSID01146',               {do not localize}
    'CP01146',                  {do not localize}
    'ebcdic-gb-285+euro',       {do not localize}
    'IBM01147',                 {do not localize}
    'CCSID01147',               {do not localize}
    'CP01147',                  {do not localize}
    'ebcdic-fr-297+euro',       {do not localize}
    'IBM01148',                 {do not localize}
    'CCSID01148',                    {do not localize}
    'CP01148',                       {do not localize}
    'ebcdic-international-500+euro', {do not localize}
    'IBM01149',                      {do not localize}
    'CCSID01149',                    {do not localize}
    'CP01149',                       {do not localize}
    'ebcdic-is-871+euro',            {do not localize}
    'Big5-HKSCS',                    {do not localize}
    'UNICODE-1-1',                   {do not localize}
    'csUnicode11',                   {do not localize}
    'SCSU',                     {do not localize}
    'UTF-7',                    {do not localize}
    'UTF-16BE',                 {do not localize}
    'UTF-16LE',                 {do not localize}
    'UTF-16',                   {do not localize}
    'CESU-8',                   {do not localize}
    'csCESU-8',                 {do not localize}
    'UTF-32',                   {do not localize}
    'UTF-32BE',                 {do not localize}
    'UTF-32LE',                 {do not localize}
    'UNICODE-1-1-UTF-7',        {do not localize}
    'csUnicode11UTF7',          {do not localize}
    'UTF-8',                    {do not localize}
    'ISO-8859-13',              {do not localize}
    'ISO-8859-14',              {do not localize}
    'iso-ir-199',               {do not localize}
    'ISO_8859-14:1998',         {do not localize}
    'ISO_8859-14',              {do not localize}
    'latin8',                   {do not localize}
    'iso-celtic',               {do not localize}
    'l8',                       {do not localize}
    'ISO-8859-15',              {do not localize}
    'ISO_8859-15',              {do not localize}
    'ISO-8859-16',              {do not localize}
    'GBK',                      {do not localize}
    'CP936',                    {do not localize}
    'MS936',                    {do not localize}
    'windows-936',              {do not localize}
    'GB18030',                  {do not localize}
    'JIS_Encoding',             {do not localize}
    'csJISEncoding',                                   {do not localize}
    'csShiftJIS',                                      {do not localize}
    'Shift_JIS',                                       {do not localize}
    'MS_Kanji',                                        {do not localize}
    'EUC-JP',                                          {do not localize}
    'Extended_UNIX_Code_Packed_Format_for_Japanese',   {do not localize}
    'csEUCPkdFmtJapanese',                             {do not localize}
    'Extended_UNIX_Code_Fixed_Width_for_Japanese',     {do not localize}
    'csEUCFixWidJapanese',                             {do not localize}
    'ISO-10646-UCS-Basic',                             {do not localize}
    'csUnicodeASCII',           {do not localize}
    'ISO-10646-Unicode-Latin1', {do not localize}
    'csUnicodeLatin1',          {do not localize}
    'ISO-10646',                {do not localize}
    'ISO-10646-J-1',            {do not localize}
    'ISO-Unicode-IBM-1261',     {do not localize}
    'csUnicodeIBM1261',         {do not localize}
    'ISO-Unicode-IBM-1268',     {do not localize}
    'csUnicodeIBM1268',         {do not localize}
    'ISO-Unicode-IBM-1276',     {do not localize}
    'csUnicodeIBM1276',                 {do not localize}
    'ISO-Unicode-IBM-1264',             {do not localize}
    'csUnicodeIBM1264',                 {do not localize}
    'ISO-Unicode-IBM-1265',             {do not localize}
    'csUnicodeIBM1265',                 {do not localize}
    'ISO-8859-1-Windows-3.0-Latin-1',   {do not localize}
    'csWindows30Latin1',                {do not localize}
    'ISO-8859-1-Windows-3.1-Latin-1',   {do not localize}
    'csWindows31Latin1',                {do not localize}
    'ISO-8859-2-Windows-Latin-2',       {do not localize}
    'csWindows31Latin2',                {do not localize}
    'ISO-8859-9-Windows-Latin-5',       {do not localize}
    'csWindows31Latin5',                {do not localize}
    'Adobe-Standard-Encoding',          {do not localize}
    'csAdobeStandardEncoding',          {do not localize}
    'Ventura-US',                       {do not localize}
    'csVenturaUS',                      {do not localize}
    'Ventura-International',            {do not localize}
    'csVenturaInternational',           {do not localize}
    'PC8-Danish-Norwegian',             {do not localize}
    'csPC8DanishNorwegian',             {do not localize}
    'PC8-Turkish',                      {do not localize}
    'csPC8Turkish',                     {do not localize}
    'IBM-Symbols',                      {do not localize}
    'csIBMSymbols',                     {do not localize}
    'IBM-Thai',                         {do not localize}
    'csIBMThai',                        {do not localize}
    'HP-Legal',                         {do not localize}
    'csHPLegal',                        {do not localize}
    'HP-Pi-font',                       {do not localize}
    'csHPPiFont',                       {do not localize}
    'HP-Math8',                         {do not localize}
    'csHPMath8',                        {do not localize}
    'Adobe-Symbol-Encoding',            {do not localize}
    'csHPPSMath',                       {do not localize}
    'HP-DeskTop',                       {do not localize}
    'csHPDesktop',                      {do not localize}
    'Ventura-Math',                     {do not localize}
    'csVenturaMath',                    {do not localize}
    'Microsoft-Publishing',             {do not localize}
    'csMicrosoftPublishing',            {do not localize}
    'Windows-31J',                      {do not localize}
    'csWindows31J',                     {do not localize}
    'csGB2312',                         {do not localize}
    'GB2312',                           {do not localize}
    'csBig5',                           {do not localize}
    'Big5',                             {do not localize}
    'windows-1250',                     {do not localize}
    'windows-1251',                     {do not localize}
    'windows-1252',                     {do not localize}
    'windows-1253',                     {do not localize}
    'windows-1254',                     {do not localize}
    'windows-1255',                     {do not localize}
    'windows-1256',                     {do not localize}
    'windows-1257',                     {do not localize}
    'windows-1258',                     {do not localize}
    'TIS-620',                          {do not localize}
    'HZ-GB-2312'                        {do not localize}

  );


function FindPreferredCharset(const ACharSet: TIdCharSet): TIdCharSet;
function FindCharset(const ACharSet: string): TIdCharset;
function CharsetToCodePage(const ACharSet: TIdCharSet): Word; overload;
function CharsetToCodePage(const ACharSet: String): Word; overload;

implementation

uses
  IdGlobal,
  SysUtils;

function FindPreferredCharset(const ACharSet: TIdCharSet): TIdCharSet;
begin
  case ACharSet of
      idcsANSI_X3_4_1968,
      idcsiso_ir_6,
      idcsANSI_X3_4_1986,
      idcsISO_646_irv_1991,
      idcsASCII,
      idcsISO646_US,
      idcsus,
      idcsIBM367,
      idcscp367,
      idcscsASCII: Result := idcsUS_ASCII;

      idcscsISO10646UTF1: Result := idcsISO_10646_UTF_1;

      idcsref,
      idcscsISO646basic1983: Result := idcsISO_646_basic_1983;

      idcscsINVARIANT: Result := idcsINVARIANT;

      idcsiso_ir_2,
      idcsirv,
      idcscsISO2IntlRefVersion: Result := idcsISO_646_irv_1983;

      idcsiso_ir_4,
      idcsISO646_GB,
      idcsgb,
      idcsuk,
      idcscsISO4UnitedKingdom: Result := idcsBS_4730;

      idcsiso_ir_8_1,
      idcscsNATSSEFI: Result := idcsNATS_SEFI;

      idcsiso_ir_8_2,
      idcscsNATSSEFIADD: Result := idcsNATS_SEFI_ADD;

      idcsiso_ir_9_1,
      idcscsNATSDANO: Result := idcsNATS_DANO;

      idcsiso_ir_9_2,
      idcscsNATSDANOADD: Result := idcsNATS_DANO_ADD;

      idcsiso_ir_10,
      idcsFI,
      idcsISO646_FI,
      idcsISO646_SE,
      idcsse,
      idcscsISO10Swedish: Result := idcsSEN_850200_B;

      idcsiso_ir_11,
      idcsISO646_SE2,
      idcsse2,
      idcscsISO11SwedishForNames: Result := idcsSEN_850200_C;

      idcsiso_ir_149,
      idcsKS_C_5601_1989,
      idcsKSC_5601,
      idcskorean,
      idcscsKSC56011987: Result := idcsKS_C_5601_1987;

      idcsISO_2022_KR: Result := idcscsISO2022KR;

      idcsEUC_KR: Result := idcscsEUCKR;

      idcsISO_2022_JP: Result := idcscsISO2022JP;

      idcsISO_2022_JP_2: Result := idcscsISO2022JP2;

      idcsJIS_C6220_1969,
      idcsiso_ir_13,
      idcskatakana,
      idcsx0201_7,
      idcscsISO13JISC6220jp: Result := idcsJIS_C6220_1969_jp;

      idcsiso_ir_14,
      idcsjp,
      idcsISO646_JP,
      idcscsISO14JISC6220ro: Result := idcsJIS_C6220_1969_ro;

      idcsiso_ir_15,
      idcsISO646_IT,
      idcscsISO15Italian: Result := idcsIT;

      idcsiso_ir_16,
      idcsISO646_PT,
      idcscsISO16Portuguese: Result := idcsPT;

      idcsiso_ir_17,
      idcsISO646_ES,
      idcscsISO17Spanish: Result := idcsES;

      idcsiso_ir_18,
      idcscsISO18Greek7Old: Result := idcsgreek7_old;

      idcsiso_ir_19,
      idcscsISO19LatinGreek: Result := idcslatin_greek;

      idcsiso_ir_21,
      idcsde,
      idcsISO646_DE,
      idcscsISO21German: Result := idcsDIN_66003;

      idcsNF_Z_62_010_,
      idcsiso_ir_25,
      idcsISO646_FR1: Result := idcscsISO25French;

      idcsiso_ir_27,
      idcscsISO27LatinGreek1: Result := idcsLatin_greek_1;

      idcsiso_ir_37,
      idcscsISO5427Cyrillic: Result := idcsISO_5427;

      idcsiso_ir_42,
      idcscsISO42JISC62261978: Result := idcsJIS_C6226_1978;

      idcsiso_ir_47,
      idcscsISO47BSViewdata: Result := idcsBS_viewdata;

      idcsiso_ir_49,
      idcscsISO49INIS: Result := idcsINIS;

      idcsiso_ir_50,
      idcscsISO50INIS8: Result := idcsINIS_8;

      idcsiso_ir_51,
      idcscsISO51INISCyrillic: Result := idcsINIS_cyrillic;

      idcsiso_ir_54,
      idcsISO5427Cyrillic1981: Result := idcsISO_5427_1981;

      idcsiso_ir_55,
      idcscsISO5428Greek: Result := idcsISO_5428_1980;

      idcsiso_ir_57,
      idcscn,
      idcsISO646_CN,
      idcscsISO57GB1988: Result := idcsGB_1988_80;

      idcsiso_ir_58,
      idcschinese,
      idcscsISO58GB231280: Result := idcsGB_2312_80;

      idcsiso_ir_60,
      idcsISO646_NO,
      idcsno,
      idcscsISO60DanishNorwegian,
      idcscsISO60Norwegian1: Result := idcsNS_4551_1;

      idcsISO646_NO2,
      idcsiso_ir_61,
      idcsno2,
      idcscsISO61Norwegian2: Result := idcsNS_4551_2;

      idcsiso_ir_69,
      idcsISO646_FR,
      idcsfr,
      idcscsISO69French: Result := idcsNF_Z_62_010;

      idcsiso_ir_70,
      idcscsISO70VideotexSupp1: Result := idcsvideotex_suppl;

      idcsiso_ir_84,
      idcsISO646_PT2,
      idcscsISO84Portuguese2: Result := idcsPT2;

      idcsiso_ir_85,
      idcsISO646_ES2,
      idcscsISO85Spanish2: Result := idcsES2;

      idcsiso_ir_86,
      idcsISO646_HU,
      idcshu,
      idcscsISO86Hungarian: Result := idcsMSZ_7795_3;

      idcsiso_ir_87,
      idcsx0208,
      idcsJIS_X0208_1983,
      idcscsISO87JISX0208: Result := idcsJIS_C6226_1983;

      idcsiso_ir_88,
      idcscsISO88Greek7: Result := idcsgreek7;

      idcsISO_9036,
      idcsarabic7,
      idcsiso_ir_89,
      idcscsISO89ASMO449: Result := idcsASMO_449;

      idcscsISO90: Result := idcsiso_ir_90;

      idcsiso_ir_91,
      idcsjp_ocr_a,
      idcscsISO91JISC62291984a: Result := idcsJIS_C6229_1984_a;

      idcsiso_ir_92,
      idcsISO646_JP_OCR_B,
      idcsjp_ocr_b,
      idcscsISO92JISC62991984b: Result := idcsJIS_C6229_1984_b;

      idcsiso_ir_93,
      idcsjp_ocr_b_add,
      idcscsISO93JIS62291984badd: Result := idcsJIS_C6229_1984_b_add;

      idcsiso_ir_94,
      idcsjp_ocr_hand,
      idcscsISO94JIS62291984hand: Result := idcsJIS_C6229_1984_hand;

      idcsiso_ir_95,
      idcsjp_ocr_hand_add,
      idcscsISO95JIS62291984handadd: Result := idcsJIS_C6229_1984_hand_add;

      idcsiso_ir_96,
      idcscsISO96JISC62291984kana: Result := idcsJIS_C6229_1984_kana;

      idcsiso_ir_98,
      idcse13b,
      idcscsISO2033: Result := idcsISO_2033_1983;

      idcsiso_ir_99,
      idcsCSA_T500_1983,
      idcsNAPLPS,
      idcscsISO99NAPLPS: Result := idcsANSI_X3_110_1983;

      idcsISO_8859_1_1987,
      idcsiso_ir_100,
      idcsISO_8859_1_,
      idcslatin1,
      idcsl1,
      idcsIBM819,
      idcsCP819,
      idcscsISOLatin1: Result := idcsISO_8859_1;

      idcsISO_8859_2_1987,
      idcsiso_ir_101,
      idcsISO_8859_2_,
      idcslatin2,
      idcsl2,
      idcscsISOLatin2: Result := idcsISO_8859_2;

      idcsiso_ir_102,
      idcscsISO102T617bit: Result := idcsT_61_7bit;

      idcsT_61,
      idcsiso_ir_103,
      idcscsISO103T618bit: Result := idcsT_61_8bit;

      idcsISO_8859_3_1988,
      idcsiso_ir_109,
      idcsISO_8859_3_,
      idcslatin3,
      idcsl3,
      idcscsISOLatin3: Result := idcsISO_8859_3;

      idcsISO_8859_4_1988,
      idcsiso_ir_110,
      idcsISO_8859_4_,
      idcslatin4,
      idcsl4,
      idcscsISOLatin4: Result := idcsISO_8859_4;

      idcsiso_ir_111,
      idcscsISO111ECMACyrillic: Result := idcsECMA_cyrillic;

      idcsiso_ir_121,
      idcsISO646_CA,
      idcscsa7_1,
      idcsca,
      idcscsISO121Canadian1: Result := idcsCSA_Z243_4_1985_1;

      idcsiso_ir_122,
      idcsISO646_CA2,
      idcscsa7_2,
      idcscsISO122Canadian2: Result := idcsCSA_Z243_4_1985_2;

      idcsiso_ir_123,
      idcscsISO123CSAZ24341985gr: Result := idcsCSA_Z243_4_1985_gr;

      idcsISO_8859_6_1987,
      idcsiso_ir_127,
      idcsISO_8859_6_,
      idcsECMA_114,
      idcsASMO_708,
      idcsarabic,
      idcscsISOLatinArabic: Result := idcsISO_8859_6;

      idcsISO_8859_6_E_,
      idcscsISO88596E: Result := idcsISO_8859_6_E;

      idcsISO_8859_6_I_,
      idcscsISO88596I: Result := idcsISO_8859_6_I;

      idcsISO_8859_7_1987,
      idcsiso_ir_126,
      idcsISO_8859_7_,
      idcsELOT_928,
      idcsECMA_118,
      idcsgreek,
      idcsgreek8,
      idcscsISOLatinGreek: Result := idcsISO_8859_7;

      idcsiso_ir_128,
      idcscsISO128T101G2: Result := idcsT_101_G2;

      idcsISO_8859_8_1988,
      idcsiso_ir_138,
      idcsISO_8859_8_,
      idcshebrew,
      idcscsISOLatinHebrew: Result := idcsISO_8859_8;

      idcsISO_8859_8_E_,
      idcscsISO88598E: Result := idcsISO_8859_8_E;

      idcsISO_8859_8_I_,
      idcscsISO88598I: Result := idcsISO_8859_8_I;

      idcsiso_ir_139,
      idcscsISO139CSN369103: Result := idcsCSN_369103;

      idcsiso_ir_141,
      idcsISO646_YU,
      idcsjs,
      idcsyu,
      idcscsISO141JUSIB1002: Result := idcsJUS_I_B1_002;

      idcsiso_ir_142,
      idcscsISOTextComm: Result := idcsISO_6937_2_add;

      idcsiso_ir_143,
      idcscsISO143IECP271: Result := idcsIEC_P27_1;

      idcsISO_8859_5_1988,
      idcsiso_ir_144,
      idcsISO_8859_5_,
      idcscyrillic,
      idcscsISOLatinCyrillic: Result := idcsISO_8859_5;

      idcsiso_ir_146,
      idcsserbian,
      idcscsISO146Serbian: Result := idcsJUS_I_B1_003_serb;

      idcsmacedonian,
      idcsiso_ir_147,
      idcscsISO147Macedonian: Result := idcsJUS_I_B1_003_mac;

      idcsISO_8859_9_1989,
      idcsiso_ir_148,
      idcsISO_8859_9_,
      idcslatin5,
      idcsl5,
      idcscsISOLatin5: Result := idcsISO_8859_9;

      idcsiso_ir_150,
      idcscsISO150,
      idcscsISO150GreekCCITT: Result := idcsgreek_ccitt;

      idcscuba,
      idcsiso_ir_151,
      idcsISO646_CU,
      idcscsISO151Cuba: Result := idcsNC_NC00_10_81;

      idcsiso_ir_152,
      idcscsISO6937Add: Result := idcsISO_6937_2_25;

      idcsST_SEV_358_88,
      idcsiso_ir_153,
      idcscsISO153GOST1976874: Result := idcsGOST_19768_74;

      idcsiso_ir_154,
      idcslatin1_2_5,
      idcscsISO8859Supp: Result := idcsISO_8859_supp;

      idcsiso_ir_155,
      idcscsISO10367Box: Result := idcsISO_10367_box;

      idcsISO_8859_10,
      idcsiso_ir_157,
      idcsl6,
      idcsISO_8859_10_1992,
      idcscsISOLatin6: Result := idcslatin6;

      idcslap,
      idcsiso_ir_158,
      idcscsISO158Lap: Result := idcslatin_lap;

      idcsx0212,
      idcsiso_ir_159,
      idcscsISO159JISX02121990: Result := idcsJIS_X0212_1990;

      idcsDS2089,
      idcsISO646_DK,
      idcsdk,
      idcscsISO646Danish: Result := idcsDS_2089;

      idcscsUSDK: Result := idcsus_dk;

      idcscsDKUS: Result := idcsdk_us;

      idcsX0201,
      idcscsHalfWidthKatakana: Result := idcsJIS_X0201;

      idcsISO646_KR,
      idcscsKSC5636: Result := idcsKSC5636;

      idcscsUnicode: Result := idcsISO_10646_UCS_2;

      idcscsUCS4: Result := idcsISO_10646_UCS_4;

      idcsdec,
      idcscsDECMCS: Result := idcsDEC_MCS;

      idcsroman8,
      idcsr8,
      idcscsHPRoman8: Result := idcshp_roman8;

      idcsmac,
      idcscsMacintosh: Result := idcsmacintosh;

      idcscp037,
      idcsebcdic_cp_us,
      idcsebcdic_cp_ca,
      idcsebcdic_cp_wt,
      idcsebcdic_cp_nl,
      idcscsIBM037: Result := idcsIBM037;

      idcsEBCDIC_INT,
      idcscp038,
      idcscsIBM038: Result := idcsIBM038;

      idcsCP273,
      idcscsIBM273: Result := idcsIBM273;

      idcsEBCDIC_BE,
      idcsCP274,
      idcscsIBM274: Result := idcsIBM274;

      idcsEBCDIC_BR,
      idcscp275,
      idcscsIBM275: Result := idcsIBM275;

      idcsEBCDIC_CP_DK,
      idcsEBCDIC_CP_NO,
      idcscsIBM277: Result := idcsIBM277;

      idcsCP278,
      idcsebcdic_cp_fi,
      idcsebcdic_cp_se,
      idcscsIBM278: Result := idcsIBM278;

      idcsCP280,
      idcsebcdic_cp_it,
      idcscsIBM280: Result := idcsIBM280;

      idcsEBCDIC_JP_E,
      idcscp281,
      idcscsIBM281: Result := idcsIBM281;

      idcsCP284,
      idcsebcdic_cp_es,
      idcscsIBM284: Result := idcsIBM284;

      idcsCP285,
      idcsebcdic_cp_gb,
      idcscsIBM285: Result := idcsIBM285;

      idcscp290,
      idcsEBCDIC_JP_kana,
      idcscsIBM290: Result := idcsIBM290;

      idcscp297,
      idcsebcdic_cp_fr,
      idcscsIBM297: Result := idcsIBM297;

      idcscp420,
      idcsebcdic_cp_ar1,
      idcscsIBM420: Result := idcsIBM420;

      idcscp423,
      idcsebcdic_cp_gr,
      idcscsIBM423: Result := idcsIBM423;

      idcscp424,
      idcsebcdic_cp_he,
      idcscsIBM424: Result := idcsIBM424;

      idcscp437,
      idcs437,
      idcscsPC8CodePage437: Result := idcsIBM437;

      idcsCP500,
      idcsebcdic_cp_be,
      idcsebcdic_cp_ch,
      idcscsIBM500: Result := idcsIBM500;

      idcscp775,
      idcscsPC775Baltic: Result := idcsIBM775;

      idcscp850,
      idcs850,
      idcscsPC850Multilingual: Result := idcsIBM850;

      idcscp851,
      idcs851,
      idcscsIBM851: Result := idcsIBM851;

      idcscp852,
      idcs852,
      idcscsPCp852: Result := idcsIBM852;

      idcscp855,
      idcs855,
      idcscsIBM855: Result := idcsIBM855;

      idcscp857,
      idcs857,
      idcscsIBM857: Result := idcsIBM857;

      idcscp860,
      idcs860,
      idcscsIBM860: Result := idcsIBM860;

      idcscp861,
      idcs861,
      idcscp_is,
      idcscsIBM861: Result := idcsIBM861;

      idcscp862,
      idcs862,
      idcscsPC862LatinHebrew: Result := idcsIBM862;

      idcscp863,
      idcs863,
      idcscsIBM863: Result := idcsIBM863;

      idcscp864,
      idcscsIBM864: Result := idcsIBM864;

      idcscp865,
      idcs865,
      idcscsIBM865: Result := idcsIBM865;

      idcscp866,
      idcs866,
      idcscsIBM866: Result := idcsIBM866;

      idcsCP868,
      idcscp_ar,
      idcscsIBM868: Result := idcsIBM868;

      idcscp869,
      idcs869,
      idcscp_gr,
      idcscsIBM869: Result := idcsIBM869;

      idcsCP870,
      idcsebcdic_cp_roece,
      idcsebcdic_cp_yu,
      idcscsIBM870: Result := idcsIBM870;

      idcsCP871,
      idcsebcdic_cp_is,
      idcscsIBM871: Result := idcsIBM871;

      idcscp880,
      idcsEBCDIC_Cyrillic,
      idcscsIBM880: Result := idcsIBM880;

      idcscp891,
      idcscsIBM891: Result := idcsIBM891;

      idcscp903,
      idcscsIBM903: Result := idcsIBM903;

      idcscp904,
      idcs904,
      idcscsIBBM904: Result := idcsIBM904;

      idcsCP905,
      idcsebcdic_cp_tr,
      idcscsIBM905: Result := idcsIBM905;

      idcsCP918,
      idcsebcdic_cp_ar2,
      idcscsIBM918: Result := idcsIBM918;

      idcsCP1026,
      idcscsIBM1026: Result := idcsIBM1026;

      idcscsIBMEBCDICATDE: Result := idcsEBCDIC_AT_DE;

      idcscsEBCDICATDEA: Result := idcsEBCDIC_AT_DE_A;

      idcscsEBCDICCAFR: Result := idcsEBCDIC_CA_FR;

      idcscsEBCDICDKNO: Result := idcsEBCDIC_DK_NO;

      idcscsEBCDICDKNOA: Result := idcsEBCDIC_DK_NO_A;

      idcscsEBCDICFISE: Result := idcsEBCDIC_FI_SE;

      idcscsEBCDICFISEA: Result := idcsEBCDIC_FI_SE_A;

      idcscsEBCDICFR: Result := idcsEBCDIC_FR;

      idcscsEBCDICIT: Result := idcsEBCDIC_IT;

      idcscsEBCDICPT: Result := idcsEBCDIC_PT;

      idcscsEBCDICES: Result := idcsEBCDIC_ES;

      idcscsEBCDICESA: Result := idcsEBCDIC_ES_A;

      idcscsEBCDICESS: Result := idcsEBCDIC_ES_S;

      idcscsEBCDICUK: Result := idcsEBCDIC_UK;

      idcscsEBCDICUS: Result := idcsEBCDIC_US;

      idcscsUnknown8BiT: Result := idcsUNKNOWN_8BIT;

      idcscsMnemonic: Result := idcsMNEMONIC;

      idcscsMnem: Result := idcsMNEM;

      idcscsVISCII: Result := idcsVISCII;

      idcscsVIQR: Result := idcsVIQR;

      idcsKOI8_R: Result := idcscsKOI8R;

      idcsCCSID00858,
      idcsCP00858,
      idcsPC_Multilingual_850_euro: Result := idcsIBM00858;

      idcsCCSID00924,
      idcsCP00924,
      idcsebcdic_Latin9__euro: Result := idcsIBM00924;

      idcsCCSID01140,
      idcsCP01140,
      idcsebcdic_us_37_euro: Result := idcsIBM01140;

      idcsCCSID01141,
      idcsCP01141,
      idcsebcdic_de_273_euro: Result := idcsIBM01141;

      idcsCCSID01142,
      idcsCP01142,
      idcsebcdic_dk_277_euro,
      idcsebcdic_no_277_euro: Result := idcsIBM01142;

      idcsCCSID01143,
      idcsCP01143,
      idcsebcdic_fi_278_euro,
      idcsebcdic_se_278_euro: Result := idcsIBM01143;

      idcsCCSID01144,
      idcsCP01144,
      idcsebcdic_it_280_euro: Result := idcsIBM01144;

      idcsCCSID01145,
      idcsCP01145,
      idcsebcdic_es_284_euro: Result := idcsIBM01145;

      idcsCCSID01146,
      idcsCP01146,
      idcsebcdic_gb_285_euro: Result := idcsIBM01146;

      idcsCCSID01147,
      idcsCP01147,
      idcsebcdic_fr_297_euro: Result := idcsIBM01147;

      idcsCCSID01148,
      idcsCP01148,
      idcsebcdic_international_500_euro: Result := idcsIBM01148;

      idcsCCSID01149,
      idcsCP01149,
      idcsebcdic_is_871_euro: Result := idcsIBM01149;

      idcscsUnicode11: Result := idcsUNICODE_1_1;

      idcscsCESU_8: Result := idcsCESU_8;

      idcscsUnicode11UTF7: Result := idcsUNICODE_1_1_UTF_7;

      idcsiso_ir_199,
      idcsISO_8859_14_1998,
      idcsISO_8859_14_,
      idcslatin8,
      idcsiso_celtic,
      idcsl8: Result := idcsISO_8859_14;

      idcsISO_8859_15_: Result := idcsISO_8859_15;

      idcsCP936,
      idcsMS936,
      idcswindows_936: Result := idcsGBK;

      idcscsJISEncoding: Result := idcsJIS_Encoding;

      idcsShift_JIS,
      idcsMS_Kanji: Result := idcscsShiftJIS;

      idcsExtended_UNIX_Code_Packed_Format_for_Japanese,
      idcscsEUCPkdFmtJapanese: Result := idcsEUC_JP;

      idcscsEUCFixWidJapanese: Result := idcsExtended_UNIX_Code_Fixed_Width_for_Japanese;

      idcscsUnicodeASCII: Result := idcsISO_10646_UCS_Basic;

      idcscsUnicodeLatin1,
      idcsISO_10646: Result := idcsISO_10646_Unicode_Latin1;

      idcscsUnicodeIBM1261: Result := idcsISO_Unicode_IBM_1261;

      idcscsUnicodeIBM1268: Result := idcsISO_Unicode_IBM_1268;

      idcscsUnicodeIBM1276: Result := idcsISO_Unicode_IBM_1276;

      idcscsUnicodeIBM1264: Result := idcsISO_Unicode_IBM_1264;

      idcscsUnicodeIBM1265: Result := idcsISO_Unicode_IBM_1265;

      idcscsWindows30Latin1: Result := idcsISO_8859_1_Windows_3_0_Latin_1;

      idcscsWindows31Latin1: Result := idcsISO_8859_1_Windows_3_1_Latin_1;

      idcscsWindows31Latin2: Result := idcsISO_8859_2_Windows_Latin_2;

      idcscsWindows31Latin5: Result := idcsISO_8859_9_Windows_Latin_5;

      idcscsAdobeStandardEncoding: Result := idcsAdobe_Standard_Encoding;

      idcscsVenturaUS: Result := idcsVentura_US;

      idcscsVenturaInternational: Result := idcsVentura_International;

      idcscsPC8DanishNorwegian: Result := idcsPC8_Danish_Norwegian;

      idcscsPC8Turkish: Result := idcsPC8_Turkish;

      idcscsIBMSymbols: Result := idcsIBM_Symbols;

      idcscsIBMThai: Result := idcsIBM_Thai;

      idcscsHPLegal: Result := idcsHP_Legal;

      idcscsHPPiFont: Result := idcsHP_Pi_font;

      idcscsHPMath8: Result := idcsHP_Math8;

      idcscsHPPSMath: Result := idcsAdobe_Symbol_Encoding;

      idcscsHPDesktop: Result := idcsHP_DeskTop;

      idcscsVenturaMath: Result := idcsVentura_Math;

      idcscsMicrosoftPublishing: Result := idcsMicrosoft_Publishing;

      idcscsWindows31J: Result := idcsWindows_31J;

      idcsGB2312: Result := idcscsGB2312;

      idcsBig5: Result := idcscsBig5;


    else Result := ACharSet;
  end;
end;


{
  REFERENCES

  [RFC1345]  Simonsen, K., "Character Mnemonics & Character Sets",
             RFC 1345, Rationel Almen Planlaegning, Rationel Almen
             Planlaegning, June 1992.

  [RFC1428]  Vaudreuil, G., "Transition of Internet Mail from
             Just-Send-8 to 8bit-SMTP/MIME", RFC1428, CNRI, February
             1993.

  [RFC1456]  Vietnamese Standardization Working Group, "Conventions for
             Encoding the Vietnamese Language VISCII: VIetnamese
             Standard Code for Information Interchange VIQR: VIetnamese
             Quoted-Readable Specification Revision 1.1", RFC 1456, May
             1993.

  [RFC1468]  Murai, J., Crispin, M., and E. van der Poel, "Japanese
             Character Encoding for Internet Messages", RFC 1468,
             Keio University, Panda Programming, June 1993.

  [RFC1489]  Chernov, A., "Registration of a Cyrillic Character Set",
             RFC1489, RELCOM Development Team, July 1993.

  [RFC1554]  Ohta, M., and K. Handa, "ISO-2022-JP-2: Multilingual
             Extension of ISO-2022-JP", RFC1554, Tokyo Institute of
             Technology, ETL, December 1993.

  [RFC1556]  Nussbacher, H., "Handling of Bi-directional Texts in MIME",
             RFC1556, Israeli Inter-University, December 1993.

  [RFC1557]  Choi, U., Chon, K., and H. Park, "Korean Character Encoding
             for Internet Messages", KAIST, Solvit Chosun Media,
             December 1993.

  [RFC1641]  Goldsmith, D., and M. Davis, "Using Unicode with MIME",
             RFC1641, Taligent, Inc., July 1994.

  [RFC1642]  Goldsmith, D., and M. Davis, "UTF-7", RFC1642, Taligent,
             Inc., July 1994.

  [RFC1815]  Ohta, M., "Character Sets ISO-10646 and ISO-10646-J-1",
             RFC 1815, Tokyo Institute of Technology, July 1995.


  [Adobe]    Adobe Systems Incorporated, PostScript Language Reference
             Manual, second edition, Addison-Wesley Publishing Company,
             Inc., 1990.

  [HP-PCL5]  Hewlett-Packard Company, "HP PCL 5 Comparison Guide",
             (P/N 5021-0329) pp B-13, 1996.

  [IBM-CIDT] IBM Corporation, "ABOUT TYPE: IBM's Technical Reference
             for Core Interchange Digitized Type", Publication number
             S544-3708-01

  [RFC1842]  Wei, Y., J. Li, and Y. Jiang, "ASCII Printable
             Characters-Based Chinese Character Encoding for Internet
             Messages", RFC 1842, Harvard University, Rice University,
             University of Maryland, August 1995.

  [RFC1843]  Lee, F., "HZ - A Data Format for Exchanging Files of
             Arbitrarily Mixed Chinese and ASCII Characters", RFC 1843,
             Stanford University, August 1995.

  [RFC2152]  Goldsmith, D., M. Davis, "UTF-7: A Mail-Safe Transformation
             Format of Unicode", RFC 2152, Apple Computer, Inc.,
             Taligent Inc., May 1997.

  [RFC2279]  Yergeau, F., "UTF-8, A Transformation Format of ISO 10646",
             RFC 2279, Alis Technologies, January, 1998.

  [RFC2781]  Hoffman, P., Yergeau, F., "UTF-16, an encoding of ISO 10646",
             RFC 2781, February 2000.


  PEOPLE

  [KXS2] Keld Simonsen <Keld.Simonsen@dkuug.dk>

  [Choi] Woohyong Choi <whchoi@cosmos.kaist.ac.kr>

  [Davis] Mark Davis, <mark@unicode.org>, April 2002.

  [Lazhintseva] Katya Lazhintseva, <katyal@MICROSOFT.com>, May 1996.

  [Mahdi] Tamer Mahdi, <tamer@ca.ibm.com>, August 2000.

  [Murai] Jun Murai <jun@wide.ad.jp>

  [Nussbacher] Hank Nussbacher, <hank@vm.tau.ac.il>

  [Ohta] Masataka Ohta, <mohta@cc.titech.ac.jp>, July 1995.

  [Phipps] Toby Phipps, <tphipps@peoplesoft.com>, March 2002.

  [Pond] Rick Pond, <rickpond@vnet.ibm.com> March 1997.

  [Scherer] Markus Scherer, <markus.scherer@jtcsv.com>, August 2000.

  [Simonsen] Keld Simonsen, <Keld.Simonsen@rap.dk>, August 2000.
}

{
  this is for searching a charset from a string, it must be case-
  insensitive so we build a lowercase table at startup
}

function FindCharset(const ACharSet: string): TIdCharset;
var
  Lcset: TIdCharset;
begin
  Result := idcsINVALID;
  for Lcset := Low(TIdCharSet) to High(TIdCharSet) do begin
    if TextIsSame(IdCharsetNames[Lcset], ACharSet) then begin
      Result := Lcset;
      Break;
    end;
  end;
end;

// RLebeau: this table was generated by scanning my PC's Windows Registry key:
// "HKEY_CLASSES_ROOT\Mime\Database\Charset"
// and then filling in missing values using various online resources.
// This may be incomplete or not entirely accurate...

{ RLebeau: TODO

720
DOS-720 "Arabic (DOS)"

737
IBM737 "Greek (DOS)"

874
windows-874 "Thai (Windows)"

875
cp875 "IBM EBCDIC (Greek Modern)"

1047
IBM01047 "IBM Latin-1"

1361
Johab "Korean (Johab)"

10001 
x-mac-japanese "Japanese (Mac)"
 
10002 
x-mac-chinesetrad "Chinese Traditional (Mac)"
 
10003 
x-mac-korean "Korean (Mac)"

10004 
x-mac-arabic "Arabic (Mac)"

10005 
x-mac-hebrew "Hebrew (Mac)"

10006 
x-mac-greek "Greek (Mac)"

10007 
x-mac-cyrillic "Cyrillic (Mac)"

10008 
x-mac-chinesesimp "Chinese Simplified (Mac)"

10010 
x-mac-romanian "Romanian (Mac)"

10017 
x-mac-ukrainian "Ukrainian (Mac)"

10021 
x-mac-thai "Thai (Mac)"

10029 
x-mac-ce "Central European (Mac)"

10079 
x-mac-icelandic "Icelandic (Mac)"

10081 
x-mac-turkish "Turkish (Mac)"

10082 
x-mac-croatian "Croatian (Mac)"

20000 
 x-Chinese-CNS 
 Chinese Traditional (CNS) 
 
 
20001 
 x-cp20001 
 TCA Taiwan 
 
 
20002 
 x-Chinese-Eten 
 Chinese Traditional (Eten) 
 
 
20003 
 x-cp20003 
 IBM5550 Taiwan 
 
 
20004 
 x-cp20004 
 TeleText Taiwan 
 
 
20005 
 x-cp20005 
 Wang Taiwan 
 
 
20105 
 x-IA5 
 Western European (IA5) 
 
 
20106 
 x-IA5-German 
 German (IA5) 
 
 
20107 
 x-IA5-Swedish 
 Swedish (IA5) 
 
 
20108 
 x-IA5-Norwegian 
 Norwegian (IA5) 
 
 
20127 
 us-ascii 
 US-ASCII 
 
20261 
 x-cp20261 
 T.61 
 
 
20269 
 x-cp20269 
 ISO-6937 
 
 
20273 
 IBM273 
 IBM EBCDIC (Germany) 
 
 
20277 
 IBM277 
 IBM EBCDIC (Denmark-Norway) 
 
 
20278 
 IBM278 
 IBM EBCDIC (Finland-Sweden) 
 
 
20280 
 IBM280 
 IBM EBCDIC (Italy) 
 
 
20284 
 IBM284 
 IBM EBCDIC (Spain) 
 
 
20285 
 IBM285 
 IBM EBCDIC (UK) 
 
 
20290 
 IBM290 
 IBM EBCDIC (Japanese katakana) 
 
 
20297 
 IBM297 
 IBM EBCDIC (France) 
 
 
20420 
 IBM420 
 IBM EBCDIC (Arabic) 
 
 
20423 
 IBM423 
 IBM EBCDIC (Greek) 
 
 
20424 
 IBM424 
 IBM EBCDIC (Hebrew) 
 
 
20833 
 x-EBCDIC-KoreanExtended 
 IBM EBCDIC (Korean Extended) 
 
 
20838 
 IBM-Thai 
 IBM EBCDIC (Thai) 
 
 
20866 
 koi8-r 
 Cyrillic (KOI8-R) 
 
 
20871 
 IBM871 
 IBM EBCDIC (Icelandic) 
 
 
20880 
 IBM880 
 IBM EBCDIC (Cyrillic Russian) 
 
 
20905 
 IBM905 
 IBM EBCDIC (Turkish) 
 
 
20924 
 IBM00924 
 IBM Latin-1 
 
 
20932 
 EUC-JP 
 Japanese (JIS 0208-1990 and 0212-1990) 
 
 
20936 
 x-cp20936 
 Chinese Simplified (GB2312-80) 
 
20949 
 x-cp20949 
 Korean Wansung 
 
21025 
 cp1025 
 IBM EBCDIC (Cyrillic Serbian-Bulgarian) 
 
 
21866 
 koi8-u 
 Cyrillic (KOI8-U) 
 
 
28591 
 iso-8859-1 
 Western European (ISO) 
 
28592 
 iso-8859-2 
 Central European (ISO) 
 
 
28593 
 iso-8859-3 
 Latin 3 (ISO) 
 
 
28594 
 iso-8859-4 
 Baltic (ISO) 
 
 
28595 
 iso-8859-5 
 Cyrillic (ISO) 
 
 
28596 
 iso-8859-6 
 Arabic (ISO) 
 
 
28597 
 iso-8859-7 
 Greek (ISO) 
 
 
28598 
 iso-8859-8 
 Hebrew (ISO-Visual) 
 
28599 
 iso-8859-9 
 Turkish (ISO) 
 
 
28603 
 iso-8859-13 
 Estonian (ISO) 
 
 
28605 
 iso-8859-15 
 Latin 9 (ISO) 
 
 
29001 
 x-Europa 
 Europa 
 
 
38598 
 iso-8859-8-i 
 Hebrew (ISO-Logical) 
 
50220 
 iso-2022-jp 
 Japanese (JIS) 
 
50221 
 csISO2022JP 
 Japanese (JIS-Allow 1 byte Kana) 
 
50222 
 iso-2022-jp 
 Japanese (JIS-Allow 1 byte Kana - SO/SI) 
 
50225 
 iso-2022-kr 
 Korean (ISO) 
 
50227 
 x-cp50227 
 Chinese Simplified (ISO-2022) 
 
51932 
 euc-jp 
 Japanese (EUC) 
 
51936 
 EUC-CN 
 Chinese Simplified (EUC) 
 
51949 
 euc-kr 
 Korean (EUC) 
 
52936 
 hz-gb-2312 
 Chinese Simplified (HZ) 
 
54936 
 GB18030 
 Chinese Simplified (GB18030) 
 
57002 
 x-iscii-de 
 ISCII Devanagari 
 
57003 
 x-iscii-be 
 ISCII Bengali 
 
57004 
 x-iscii-ta 
 ISCII Tamil 
 
57005 
 x-iscii-te 
 ISCII Telugu 
 
57006 
 x-iscii-as 
 ISCII Assamese 
 
57007 
 x-iscii-or 
 ISCII Oriya 
 
57008 
 x-iscii-ka 
 ISCII Kannada 
 
57009 
 x-iscii-ma 
 ISCII Malayalam 
 
57010 
 x-iscii-gu 
 ISCII Gujarati 
 
57011 
 x-iscii-pa 
 ISCII Punjabi 
}

const
  IdCharsetCodePages : array[Low(TIdCharSet)..High(TIdCharSet)] of Word = (
    0,      // Unknown
    1252,   // US-ASCII (use 20127 instead?)
    1252,   // ANSI_X3.4-1968
    1252,   // iso-ir-6
    1252,   // ANSI_X3.4-1986
    1252,   // ISO_646.irv:1991
    1252,   // ASCII
    1252,   // ISO646-US
    1252,   // us
    1252,   // IBM367
    1252,   // cp367
    1252,   // csASCII
    0,      // ISO-10646-UTF-1
    0,      // csISO10646UTF1
    0,      // ISO_646.basic:1983
    0,      // ref
    0,      // csISO646basic1983
    0,      // INVARIANT
    0,      // csINVARIANT
    0,      // ISO_646.irv:1983
    0,      // iso-ir-2
    0,      // irv
    0,      // csISO2IntlRefVersion
    0,      // BS_4730
    0,      // iso-ir-4
    0,      // ISO646-GB
    0,      // gb
    0,      // uk
    0,      // csISO4UnitedKingdom
    0,      // NATS-SEFI
    0,      // iso-ir-8-1
    0,      // csNATSSEFI
    0,      // NATS-SEFI-ADD
    0,      // iso-ir-8-2
    0,      // csNATSSEFIADD
    0,      // NATS-DANO
    0,      // iso-ir-9-1
    0,      // csNATSDANO
    0,      // NATS-DANO-ADD
    0,      // iso-ir-9-2
    0,      // csNATSDANOADD
    0,      // SEN_850200_B
    0,      // iso-ir-10
    0,      // FI
    0,      // ISO646-FI
    0,      // ISO646-SE
    0,      // se
    0,      // csISO10Swedish
    0,      // SEN_850200_C
    0,      // iso-ir-11
    0,      // ISO646-SE2
    0,      // se2
    0,      // csISO11SwedishForNames
    949,    // KS_C_5601-1987
    949,    // iso-ir-149
    949,    // KS_C_5601-1989
    949,    // KSC_5601
    949,    // korean
    949,    // csKSC56011987
    50225,  // csISO2022KR
    50225,  // ISO-2022-KR
    949,    // csEUCKR
    949,    // EUC-KR
    50221,  // csISO2022JP
    50220,  // ISO-2022-JP
    0,      // csISO2022JP2
    0,      // ISO-2022-JP-2
    0,      // ISO-2022-CN
    0,      // ISO-2022-CN-EXT
    0,      // JIS_C6220-1969-jp
    0,      // JIS_C6220-1969
    0,      // iso-ir-13
    0,      // katakana
    0,      // x0201-7
    0,      // csISO13JISC6220jp
    0,      // JIS_C6220-1969-ro
    0,      // iso-ir-14
    0,      // jp
    0,      // ISO646-JP
    0,      // csISO14JISC6220ro
    0,      // IT
    0,      // iso-ir-15
    0,      // ISO646-IT
    0,      // csISO15Italian
    0,      // PT
    0,      // iso-ir-16
    0,      // ISO646-PT
    0,      // csISO16Portuguese
    0,      // ES
    0,      // iso-ir-17
    0,      // ISO646-ES
    0,      // csISO17Spanish
    0,      // greek7-old
    0,      // iso-ir-18
    0,      // csISO18Greek7Old
    0,      // latin-greek
    0,      // iso-ir-19
    0,      // csISO19LatinGreek
    0,      // DIN_66003
    0,      // iso-ir-21
    0,      // de
    0,      // ISO646-DE
    0,      // csISO21German
    0,      // csISO25French
    0,      // NF_Z_62-010_
    0,      // iso-ir-25
    0,      // ISO646-FR1
    0,      // Latin-greek-1
    0,      // iso-ir-27
    0,      // csISO27LatinGreek1
    0,      // ISO_5427
    0,      // iso-ir-37
    0,      // csISO5427Cyrillic
    0,      // JIS_C6226-1978
    0,      // iso-ir-42
    0,      // csISO42JISC62261978
    0,      // BS_viewdata
    0,      // iso-ir-47
    0,      // csISO47BSViewdata
    0,      // INIS
    0,      // iso-ir-49
    0,      // csISO49INIS
    0,      // INIS-8
    0,      // iso-ir-50
    0,      // csISO50INIS8
    0,      // INIS-cyrillic
    0,      // iso-ir-51
    0,      // csISO51INISCyrillic
    0,      // ISO_5427:1981
    0,      // iso-ir-54
    0,      // ISO5427Cyrillic1981
    0,      // ISO_5428:1980
    0,      // iso-ir-55
    0,      // csISO5428Greek
    0,      // GB_1988-80
    0,      // iso-ir-57
    0,      // cn
    0,      // ISO646-CN
    0,      // csISO57GB1988
    936,    // GB_2312-80
    936,    // iso-ir-58
    936,    // chinese
    936,    // csISO58GB231280
    0,      // NS_4551-1
    0,      // iso-ir-60
    0,      // ISO646-NO
    0,      // no
    0,      // csISO60DanishNorwegian
    0,      // csISO60Norwegian1
    0,      // NS_4551-2
    0,      // ISO646-NO2
    0,      // iso-ir-61
    0,      // no2
    0,      // csISO61Norwegian2
    0,      // NF_Z_62-010
    0,      // iso-ir-69
    0,      // ISO646-FR
    0,      // fr
    0,      // csISO69French
    0,      // videotex-suppl
    0,      // iso-ir-70
    0,      // csISO70VideotexSupp1
    0,      // PT2
    0,      // iso-ir-84
    0,      // ISO646-PT2
    0,      // csISO84Portuguese2
    0,      // ES2
    0,      // iso-ir-85
    0,      // ISO646-ES2
    0,      // csISO85Spanish2
    0,      // MSZ_7795.3
    0,      // iso-ir-86
    0,      // ISO646-HU
    0,      // hu
    0,      // csISO86Hungarian
    0,      // JIS_C6226-1983
    0,      // iso-ir-87
    0,      // x0208
    0,      // JIS_X0208-1983
    0,      // csISO87JISX0208
    0,      // greek7
    0,      // iso-ir-88
    0,      // csISO88Greek7
    0,      // ASMO_449
    0,      // ISO_9036
    0,      // arabic7
    0,      // iso-ir-89
    0,      // csISO89ASMO449
    0,      // iso-ir-90
    0,      // csISO90
    0,      // JIS_C6229-1984-a
    0,      // iso-ir-91
    0,      // jp-ocr-a
    0,      // csISO91JISC62291984a
    0,      // JIS_C6229-1984-b
    0,      // iso-ir-92
    0,      // ISO646-JP-OCR-B
    0,      // jp-ocr-b
    0,      // csISO92JISC62991984b
    0,      // JIS_C6229-1984-b-add
    0,      // iso-ir-93
    0,      // jp-ocr-b-add
    0,      // csISO93JIS62291984badd
    0,      // JIS_C6229-1984-hand
    0,      // iso-ir-94
    0,      // jp-ocr-hand
    0,      // csISO94JIS62291984hand
    0,      // JIS_C6229-1984-hand-add
    0,      // iso-ir-95
    0,      // jp-ocr-hand-add
    0,      // csISO95JIS62291984handadd
    0,      // JIS_C6229-1984-kana
    0,      // iso-ir-96
    0,      // csISO96JISC62291984kana
    0,      // ISO_2033-1983
    0,      // iso-ir-98
    0,      // e13b
    0,      // csISO2033
    0,      // ANSI_X3.110-1983
    0,      // iso-ir-99
    0,      // CSA_T500-1983
    0,      // NAPLPS
    0,      // csISO99NAPLPS

    // RLebeau: normally, Windows maps these to codepage 1252,
    // but they are actually codepage 28591 instead...
    28591,  // ISO-8859-1
    28591,  // ISO_8859-1:1987
    28591,  // iso-ir-100
    28591,  // ISO_8859-1
    28591,  // latin1
    28591,  // l1
    28591,  // IBM819
    28591,  // CP819
    28591,  // csISOLatin1

    28592,  // ISO-8859-2
    28592,  // ISO_8859-2:1987
    28592,  // iso-ir-101
    28592,  // ISO_8859-2
    28592,  // latin2
    28592,  // l2
    28592,  // csISOLatin2
    0,      // T.61-7bit
    0,      // iso-ir-102
    0,      // csISO102T617bit
    0,      // T.61-8bit
    0,      // T.61
    0,      // iso-ir-103
    0,      // csISO103T618bit
    28593,  // ISO-8859-3
    0,      // ISO_8859-3:1988
    0,      // iso-ir-109
    0,      // ISO_8859-3
    0,      // latin3
    0,      // l3
    0,      // csISOLatin3
    28594,  // ISO-8859-4
    28594,  // ISO_8859-4:1988
    28594,  // iso-ir-110
    28594,  // ISO_8859-4
    28594,  // latin4
    28594,  // l4
    28594,  // csISOLatin4
    0,      // ECMA-cyrillic
    28594,  // iso-ir-111
    0,      // csISO111ECMACyrillic
    0,      // CSA_Z243.4-1985-1
    0,      // iso-ir-121
    0,      // ISO646-CA
    0,      // csa7-1
    0,      // ca
    0,      // csISO121Canadian1
    0,      // CSA_Z243.4-1985-2
    0,      // iso-ir-122
    0,      // ISO646-CA2
    0,      // csa7-2
    0,      // csISO122Canadian2
    0,      // CSA_Z243.4-1985-gr
    0,      // iso-ir-123
    0,      // csISO123CSAZ24341985gr
    28596,  // ISO-8859-6
    28596,  // ISO_8859-6:1987
    28596,  // iso-ir-127
    28596,  // ISO_8859-6
    28596,  // ECMA-114
    708,    // ASMO-708
    28596,  // arabic
    28596,  // csISOLatinArabic
    0,      // ISO-8859-6-E
    0,      // ISO_8859-6-E
    0,      // csISO88596E
    0,      // ISO-8859-6-I
    0,      // ISO_8859-6-I
    0,      // csISO88596I
    28597,  // ISO-8859-7
    28597,  // ISO_8859-7:1987
    28597,  // iso-ir-126
    28597,  // ISO_8859-7
    28597,  // ELOT_928
    28597,  // ECMA-118
    28597,  // greek
    28597,  // greek8
    28597,  // csISOLatinGreek
    0,      // T.101-G2
    0,      // iso-ir-128
    0,      // csISO128T101G2
    28598,  // ISO-8859-8
    28598,  // ISO_8859-8:1988
    28598,  // iso-ir-138
    28598,  // ISO_8859-8
    28598,  // hebrew
    28598,  // csISOLatinHebrew
    0,      // ISO-8859-8-E
    0,      // ISO_8859-8-E
    0,      // csISO88598E
    38598,  // ISO-8859-8-I
    0,      // ISO_8859-8-I
    0,      // csISO88598I
    0,      // CSN_369103
    0,      // iso-ir-139
    0,      // csISO139CSN369103
    0,      // JUS_I.B1.002
    0,      // iso-ir-141
    0,      // ISO646-YU
    0,      // js
    0,      // yu
    0,      // csISO141JUSIB1002
    0,      // ISO_6937-2-add
    0,      // iso-ir-142
    0,      // csISOTextComm
    0,      // IEC_P27-1
    0,      // iso-ir-143
    0,      // csISO143IECP271
    28595,  // ISO-8859-5
    28595,  // ISO_8859-5:1988
    28595,  // iso-ir-144
    28595,  // ISO_8859-5
    28595,  // cyrillic
    28595,  // csISOLatinCyrillic
    0,      // JUS_I.B1.003-serb
    0,      // iso-ir-146
    0,      // serbian
    0,      // csISO146Serbian
    0,      // JUS_I.B1.003-mac
    0,      // macedonian
    0,      // iso-ir-147
    0,      // csISO147Macedonian
    1254,   // ISO-8859-9
    1254,   // ISO_8859-9:1989
    1254,   // iso-ir-148
    1254,   // ISO_8859-9
    1254,   // latin5
    1254,   // l5
    1254,   // csISOLatin5
    0,      // greek-ccitt
    0,      // iso-ir-150
    0,      // csISO150
    0,      // csISO150GreekCCITT
    0,      // NC_NC00-10:81
    0,      // cuba
    0,      // iso-ir-151
    0,      // ISO646-CU
    0,      // csISO151Cuba
    0,      // ISO_6937-2-25
    0,      // iso-ir-152
    0,      // csISO6937Add
    0,      // GOST_19768-74
    0,      // ST_SEV_358-88
    0,      // iso-ir-153
    0,      // csISO153GOST1976874
    0,      // ISO_8859-supp
    0,      // iso-ir-154
    0,      // latin1-2-5
    0,      // csISO8859Supp
    0,      // ISO_10367-box
    0,      // iso-ir-155
    0,      // csISO10367Box
    0,      // latin6
    0,      // ISO-8859-10
    0,      // iso-ir-157
    0,      // l6
    0,      // ISO_8859-10:1992
    0,      // csISOLatin6
    0,      // latin-lap
    0,      // lap
    0,      // iso-ir-158
    0,      // csISO158Lap
    0,      // JIS_X0212-1990
    0,      // x0212
    0,      // iso-ir-159
    0,      // csISO159JISX02121990
    0,      // DS_2089
    0,      // DS2089
    0,      // ISO646-DK
    0,      // dk
    0,      // csISO646Danish
    0,      // us-dk
    0,      // csUSDK
    0,      // dk-us
    0,      // csDKUS
    0,      // JIS_X0201
    0,      // X0201
    0,      // csHalfWidthKatakana
    0,      // KSC5636
    0,      // ISO646-KR
    0,      // csKSC5636
    0,      // ISO-10646-UCS-2
    0,      // csUnicode
    0,      // ISO-10646-UCS-4
    0,      // csUCS4
    0,      // DEC-MCS
    0,      // dec
    0,      // csDECMCS
    0,      // hp-roman8
    0,      // roman8
    0,      // r8
    0,      // csHPRoman8
    10000,  // macintosh
    0,      // mac
    0,      // csMacintosh
    37,     // IBM037
    0,      // cp037
    0,      // ebcdic-cp-us
    0,      // ebcdic-cp-ca
    0,      // ebcdic-cp-wt
    0,      // ebcdic-cp-nl
    0,      // csIBM037
    0,      // IBM038
    0,      // EBCDIC-INT
    0,      // cp038
    0,      // csIBM038
    0,      // IBM273
    0,      // CP273
    0,      // csIBM273
    0,      // IBM274
    0,      // EBCDIC-BE
    0,      // CP274
    0,      // csIBM274
    0,      // IBM275
    0,      // EBCDIC-BR
    0,      // cp275
    0,      // csIBM275
    0,      // IBM277
    0,      // EBCDIC-CP-DK
    0,      // EBCDIC-CP-NO
    0,      // csIBM277
    0,      // IBM278
    0,      // CP278
    0,      // ebcdic-cp-fi
    0,      // ebcdic-cp-se
    0,      // csIBM278
    0,      // IBM280
    0,      // CP280
    0,      // ebcdic-cp-it
    0,      // csIBM280
    0,      // IBM281
    0,      // EBCDIC-JP-E
    0,      // cp281
    0,      // csIBM281
    0,      // IBM284
    0,      // CP284
    0,      // ebcdic-cp-es
    0,      // csIBM284
    0,      // IBM285
    0,      // CP285
    0,      // ebcdic-cp-gb
    0,      // csIBM285
    0,      // IBM290
    0,      // cp290
    0,      // EBCDIC-JP-kana
    0,      // csIBM290
    0,      // IBM297
    0,      // cp297
    0,      // ebcdic-cp-fr
    0,      // csIBM297
    0,      // IBM420
    0,      // cp420
    0,      // ebcdic-cp-ar1
    0,      // csIBM420
    0,      // IBM423
    0,      // cp423
    0,      // ebcdic-cp-gr
    0,      // csIBM423
    0,      // IBM424
    0,      // cp424
    0,      // ebcdic-cp-he
    0,      // csIBM424
    437,    // IBM437
    0,      // cp437
    0,      // 437
    0,      // csPC8CodePage437
    500,    // IBM500
    0,      // CP500
    0,      // ebcdic-cp-be
    0,      // ebcdic-cp-ch
    0,      // csIBM500
    775,    // IBM775
    0,      // cp775
    0,      // csPC775Baltic
    850,    // IBM850
    0,      // cp850
    0,      // 850
    0,      // csPC850Multilingual
    0,      // IBM851
    0,      // cp851
    0,      // 851
    0,      // csIBM851
    852,    // IBM852
    852,    // cp852
    0,      // 852
    0,      // csPCp852
    855,    // IBM855
    0,      // cp855
    0,      // 855
    0,      // csIBM855
    857,    // IBM857
    0,      // cp857
    0,      // 857
    0,      // csIBM857
    860,    // IBM860
    0,      // cp860
    0,      // 860
    0,      // csIBM860
    861,    // IBM861
    0,      // cp861
    0,      // 861
    0,      // cp-is
    0,      // csIBM861
    0,      // IBM862
    0,      // cp862
    0,      // 862
    0,      // csPC862LatinHebrew
    863,    // IBM863
    0,      // cp863
    0,      // 863
    0,      // csIBM863
    864,    // IBM864
    0,      // cp864
    0,      // csIBM864
    865,    // IBM865
    0,      // cp865
    0,      // 865
    0,      // csIBM865
    866,    // IBM866
    866,    // cp866
    0,      // 866
    0,      // csIBM866
    0,      // IBM868
    0,      // CP868
    0,      // cp-ar
    0,      // csIBM868
    869,    // IBM869
    0,      // cp869
    0,      // 869
    0,      // cp-gr
    0,      // csIBM869
    870,    // IBM870
    0,      // CP870
    0,      // ebcdic-cp-roece
    0,      // ebcdic-cp-yu
    0,      // csIBM870
    0,      // IBM871
    0,      // CP871
    0,      // ebcdic-cp-is
    0,      // csIBM871
    0,      // IBM880
    0,      // cp880
    0,      // EBCDIC-Cyrillic
    0,      // csIBM880
    0,      // IBM891
    0,      // cp891
    0,      // csIBM891
    0,      // IBM903
    0,      // cp903
    0,      // csIBM903
    0,      // IBM904
    0,      // cp904
    0,      // 904
    0,      // csIBBM904
    0,      // IBM905
    0,      // CP905
    0,      // ebcdic-cp-tr
    0,      // csIBM905
    0,      // IBM918
    0,      // CP918
    0,      // ebcdic-cp-ar2
    0,      // csIBM918
    1026,   // IBM1026
    0,      // CP1026
    0,      // csIBM1026
    0,      // EBCDIC-AT-DE
    0,      // csIBMEBCDICATDE
    0,      // EBCDIC-AT-DE-A
    0,      // csEBCDICATDEA
    0,      // EBCDIC-CA-FR
    0,      // csEBCDICCAFR
    0,      // EBCDIC-DK-NO
    0,      // csEBCDICDKNO
    0,      // EBCDIC-DK-NO-A
    0,      // csEBCDICDKNOA
    0,      // EBCDIC-FI-SE
    0,      // csEBCDICFISE
    0,      // EBCDIC-FI-SE-A
    0,      // csEBCDICFISEA
    0,      // EBCDIC-FR
    0,      // csEBCDICFR
    0,      // EBCDIC-IT
    0,      // csEBCDICIT
    0,      // EBCDIC-PT
    0,      // csEBCDICPT
    0,      // EBCDIC-ES
    0,      // csEBCDICES
    0,      // EBCDIC-ES-A
    0,      // csEBCDICESA
    0,      // EBCDIC-ES-S
    0,      // csEBCDICESS
    0,      // EBCDIC-UK
    0,      // csEBCDICUK
    0,      // EBCDIC-US
    0,      // csEBCDICUS
    0,      // UNKNOWN-8BIT
    0,      // csUnknown8BiT
    0,      // MNEMONIC
    0,      // csMnemonic
    0,      // MNEM
    0,      // csMnem
    0,      // VISCII
    0,      // csVISCII
    0,      // VIQR
    0,      // csVIQR
    20866,  // csKOI8R
    20866,  // KOI8-R
    0,      // KOI8-U
    858,    // IBM00858
    0,      // CCSID00858
    0,      // CP00858
    0,      // PC-Multilingual-850+euro
    0,      // IBM00924
    0,      // CCSID00924
    0,      // CP00924
    0,      // ebcdic-Latin9--euro
    1140,   // IBM01140
    0,      // CCSID01140
    0,      // CP01140
    0,      // ebcdic-us-37+euro
    1141,   // IBM01141
    0,      // CCSID01141
    0,      // CP01141
    0,      // ebcdic-de-273+euro
    1142,   // IBM01142
    0,      // CCSID01142
    0,      // CP01142
    0,      // ebcdic-dk-277+euro
    0,      // ebcdic-no-277+euro
    1143,   // IBM01143
    0,      // CCSID01143
    0,      // CP01143
    0,      // ebcdic-fi-278+euro
    0,      // ebcdic-se-278+euro
    1144,   // IBM01144
    0,      // CCSID01144
    0,      // CP01144
    0,      // ebcdic-it-280+euro
    1145,   // IBM01145
    0,      // CCSID01145
    0,      // CP01145
    0,      // ebcdic-es-284+euro
    1146,   // IBM01146
    0,      // CCSID01146
    0,      // CP01146
    0,      // ebcdic-gb-285+euro
    1147,   // IBM01147
    0,      // CCSID01147
    0,      // CP01147
    0,      // ebcdic-fr-297+euro
    1148,   // IBM01148
    0,      // CCSID01148
    0,      // CP01148
    0,      // ebcdic-international-500+euro
    1149,   // IBM01149
    0,      // CCSID01149
    0,      // CP01149
    0,      // ebcdic-is-871+euro
    0,      // Big5-HKSCS
    0,      // UNICODE-1-1
    0,      // csUnicode11
    0,      // SCSU
    65000,  // UTF-7
    0,      // UTF-16BE
    1200,   // UTF-16LE
    1200,   // UTF-16
    0,      // CESU-8
    0,      // csCESU-8
    12000,  // UTF-32
    12001,  // UTF-32BE
    12000,  // UTF-32LE
    65000,  // UNICODE-1-1-UTF-7
    65000,  // csUnicode11UTF7
    65001,  // UTF-8
    0,      // ISO-8859-13
    0,      // ISO-8859-14
    0,      // iso-ir-199
    0,      // ISO_8859-14:1998
    0,      // ISO_8859-14
    0,      // latin8
    0,      // iso-celtic
    0,      // l8
    0,      // ISO-8859-15
    0,      // ISO_8859-15
    0,      // ISO-8859-16
    936,    // GBK
    0,      // CP936
    0,      // MS936
    0,      // windows-936
    0,      // GB18030
    0,      // JIS_Encoding
    0,      // csJISEncoding
    932,    // csShiftJIS
    932,    // Shift_JIS
    932,    // MS_Kanji
    51932,  // EUC-JP
    51932,  // Extended_UNIX_Code_Packed_Format_for_Japanese
    51932,  // csEUCPkdFmtJapanese
    0,      // Extended_UNIX_Code_Fixed_Width_for_Japanese
    0,      // csEUCFixWidJapanese
    0,      // ISO-10646-UCS-Basic
    0,      // csUnicodeASCII
    0,      // ISO-10646-Unicode-Latin1
    0,      // csUnicodeLatin1
    0,      // ISO-10646
    0,      // ISO-10646-J-1
    0,      // ISO-Unicode-IBM-1261
    0,      // csUnicodeIBM1261
    0,      // ISO-Unicode-IBM-1268
    0,      // csUnicodeIBM1268
    0,      // ISO-Unicode-IBM-1276
    0,      // csUnicodeIBM1276
    0,      // ISO-Unicode-IBM-1264
    0,      // csUnicodeIBM1264
    0,      // ISO-Unicode-IBM-1265
    0,      // csUnicodeIBM1265
    0,      // ISO-8859-1-Windows-3.0-Latin-1
    0,      // csWindows30Latin1
    0,      // ISO-8859-1-Windows-3.1-Latin-1
    0,      // csWindows31Latin1
    0,      // ISO-8859-2-Windows-Latin-2
    0,      // csWindows31Latin2
    0,      // ISO-8859-9-Windows-Latin-5
    0,      // csWindows31Latin5
    0,      // Adobe-Standard-Encoding
    0,      // csAdobeStandardEncoding
    0,      // Ventura-US
    0,      // csVenturaUS
    0,      // Ventura-International
    0,      // csVenturaInternational
    0,      // PC8-Danish-Norwegian
    0,      // csPC8DanishNorwegian
    0,      // PC8-Turkish
    0,      // csPC8Turkish
    0,      // IBM-Symbols
    0,      // csIBMSymbols
    0,      // IBM-Thai
    0,      // csIBMThai
    0,      // HP-Legal
    0,      // csHPLegal
    0,      // HP-Pi-font
    0,      // csHPPiFont
    0,      // HP-Math8
    0,      // csHPMath8
    0,      // Adobe-Symbol-Encoding
    0,      // csHPPSMath
    0,      // HP-DeskTop
    0,      // csHPDesktop
    0,      // Ventura-Math
    0,      // csVenturaMath
    0,      // Microsoft-Publishing
    0,      // csMicrosoftPublishing
    0,      // Windows-31J
    932,    // csWindows31J
    936,    // csGB2312
    936,    // GB2312
    950,    // csBig5
    950,    // Big5
    1250,   // windows-1250
    1251,   // windows-1251
    1252,   // windows-1252
    1253,   // windows-1253
    1254,   // windows-1254
    1255,   // windows-1255
    1256,   // windows-1256
    1257,   // windows-1257
    1258,   // windows-1258
    0,      // TIS-620
    52936   // HZ-GB-2312
  );

function CharsetToCodePage(const ACharSet: TIdCharSet): Word;
begin
  Result := IdCharsetCodePages[ACharSet];
end;

function CharsetToCodePage(const ACharSet: String): Word; overload;
begin
  Result := IdCharsetCodePages[FindCharset(ACharSet)];
end;

end.
