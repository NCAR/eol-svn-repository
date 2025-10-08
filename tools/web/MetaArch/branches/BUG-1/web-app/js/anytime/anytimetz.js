/*****************************************************************************
 *  FILE:  anytimetz.js - The Any+Time(TM) JavaScript Library
 *                        Basic Time Zone Support (source)
 *  VERSION: 5.x
 *
 *  Copyright 2008-2010 Andrew M. Andrews III (www.AMA3.com). Some Rights
 *  Reserved. This work is licensed under the Any+Time Software License
 *  Agreement Version 5.0 (below).
 *
 *  This file adds basic labels for major time zones to the Any+Time(TM)
 *  JavaScript Library.  Time zone support is extremely complicated, and
 *  ECMA-262 (JavaScript) provides little support.  Developers are expected
 *  to tailor this file to meet their needs, mostly by removing lines that
 *  are not required by their users, and/or by removing either abbreviated
 *  (before double-dash) or long (after double-dash) names from the strings.
 *  
 *  Note that there is no automatic detection of daylight savings time
 *  (AKA summer time), due to lack of support in JavaScript and the 
 *  time-prohibitive complexity of attempting such support in code.
 *  If you want to take a stab at it, let me know; if you want to pay me
 *  large sums of money to add it, again, let me know. :-p
 *  
 *  This file should be included AFTER anytime.js (or anytimec.js) in any
 *  HTML page that requires it.
 *
 *  Any+Time is a trademark of Andrew M. Andrews III.
 *
 *****************************************************************************
 *  Any+Time Software License Agreement
 *  Version 5.0
 *
 *  THIS DOCUMENT IS A LEGAL AGREEMENT (the "License Agreement") BETWEEN
 *  ANDREW M. ANDREWS III ("We," "Us") AND YOU OR THE ORGANIZATION ON WHOSE
 *  BEHALF YOU ARE UNDERTAKING THE LICENSE DESCRIBED BELOW ("You") IN RELATION
    TO THE ANY+TIME JAVASCRIPT LIBRARY SOFTWARE (THE "Software"), IN BOTH
 *  SOURCE AND OBJECT CODE FORM, AND/OR ALL RELATED MATERIALS. BY
 *  DOWNLOADING,INSTALLING, COPYING OR OTHERWISE USING THE SOFTWARE, YOU
 *  ACCEPT THE FOLLOWING TERMS AND CONDITIONS. IF YOU DO NOT AGREE WITH ANY
 *  OF THE TERMS OR CONDITIONS OF THIS LICENSE AGREEMENT, DO NOT PROCEED
 *  WITH THE DOWNLOADING, COPYING, INSTALLATION OR ANY OTHER USE OF THE
 *  SOFTWARE OR ANY PORTION THEREOF. THE SOFTWARE IS PROTECTED BY UNITED
 *  STATES COPYRIGHT LAWS AND INTERNATIONAL COPYRIGHT LAWS, AS WELL AS
 *  OTHER INTELLECTUAL PROPERTY LAWS AND TREATIES. THE SOFTWARE IS
 *  LICENSED, NOT SOLD.
 *
 *  THIS LICENSE AGREEMENT DESCRIBES YOUR RIGHTS WITH RESPECT TO THE
 *  SOFTWARE AND ITS COMPONENTS.
 *
 *  We grant You a non-exclusive, non-transferable license to the Software
 *  solely as set forth in section 1, and subject to the terms and
 *  conditions of this License Agreement.
 *
 *  1. OWNERSHIP, LICENSE GRANT
 *
 *  This is a license agreement and not an agreement for sale. We reserve
 *  ownership of all intellectual property rights inherent in or relating
 *  to the Software, which include, but are not limited to, all copyright,
 *  patent rights, all rights in relation to registered and unregistered
 *  trademarks (including service marks), confidential information
 *  (including trade secrets and know-how) and all rights other than those
 *  expressly granted by this Agreement. Subject to the payment of the fee
 *  and/or royalties (if any) required for a License and subject to the
 *  terms and conditions of this License Agreement, We grant to You a
 *  revocable, non-transferable and non-exclusive license (i) for You to
 *  install and use the Software on any workstations used exclusively by
 *  You and Your employees, and (ii) for You to install and use the
 *  Software in connection with unlimited domains and sub-domains on
 *  unlimited servers, solely in connection with distribution of the
 *  Software in accordance with sections 3 and 4 below. You may not
 *  sublicense this license except as explicitly set forth herein.
 *
 *  2. PERMITTED USES, SOURCE CODE, MODIFICATIONS
 *
 *  We provide You with source code so that You can create Modifications of
 *  the original Software, where Modification means: a) any addition to or
 *  deletion from the contents of a file included in the original Software
 *  or previous Modifications created by You, or b) any new file that
 *  contains any part of the original Software or previous Modifications.
 *  While You retain all rights to any original work authored by You as
 *  part of the Modifications, We continue to own all copyright and other
 *  intellectual property rights in the Software.
 *
 *  3. DISTRIBUTION
 *
 *  You may distribute the Software in any applications (including
 *  Software-as-a-Service applications), frameworks, or elements that you
 *  develop using the Software in accordance with this License Agreement,
 *  provided that such distribution does not violate the restrictions set
 *  forth in section 4 of this agreement. You must not remove, obscure or
 *  interfere with any copyright, acknowledgment, attribution, trademark,
 *  warning or disclaimer statement affixed to, incorporated in or
 *  otherwise applied in connection with the Software.
 *
 *  You are required to ensure that the Software is not reused by or with
 *  any applications other than those with which You distribute it as
 *  permitted herein. For example, if You install the Software on a
 *  customer's server, that customer is not permitted to use the Software
 *  independently of Your application, and must be informed as such. Your
 *  customer is required to pay any fees and/or royalties for their License
 *  to use this Software, and you are required to inform them of this
 *  obligation.
 *
 *  4. PROHIBITED USES
 *
 *  You may not, without Our prior written consent, redistribute the
 *  Software or Modifications other than by including the Software or a
 *  portion thereof within Your own product, which must have substantially
 *  different functionality than the Software or Modifications and must not
 *  allow any third party to use the Software or Modifications, or any
 *  portions thereof, for software development purposes. You are explicitly
 *  not allowed to redistribute the Software or Modifications as part of
 *  any product that can be described as a development toolkit or library
 *  or is intended for use by software developers and not end-users. You
 *  are not allowed to redistribute any part of the Software documentation.
 *
 *  You may not: a) use any part of the Software or Modifications or Your
 *  knowledge of the Software (or any information that You learn as a
 *  result of Your use of the Software) to create a product with the same
 *  or substantially the same functionality as the Software; b) transfer,
 *  rent, lease, or sub-license the Software or Modifications, or any
 *  portions thereof; c) change or remove the copyright notice from any of
 *  the files included in the Software or Modifications.
 *
 *  UNDER NO CIRCUMSTANCES MAY YOU USE THE SOFTWARE (INCLUDING WITHOUT
 *  LIMITATION THE SOURCE CODE THEREOF) AS THE BASIS FOR OR IN CONNECTION
 *  WITH A PRODUCT THAT CONTAINS THE SAME, OR SUBSTANTIALLY THE SAME,
 *  FUNCTIONALITY AS THE SOFTWARE.
 *
 *  5. TERMINATION
 *
 *  This License Agreement and Your right to use the Software and
 *  Modifications will terminate immediately without notice if You fail to
 *  comply with the terms and conditions of this License Agreement. Upon
 *  termination, You agree to immediately cease using and destroy the
 *  Software or Modifications, including all accompanying documents. The
 *  provisions of sections 4, 5, 6, 7, and 8 will survive any termination
 *  of this License Agreement.
 *
 *  6. DISCLAIMER OF WARRANTIES
 *
 *  TO THE MAXIMUM EXTENT PERMITTED BY APPLICABLE LAW, WE AND OUR SUPPLIERS
 *  DISCLAIM ALL WARRANTIES AND CONDITIONS, EITHER EXPRESS OR IMPLIED,
 *  INCLUDING, BUT NOT LIMITED TO, IMPLIED WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE, TITLE AND NON-INFRINGEMENT, WITH
 *  REGARD TO THE SOFTWARE. WE DO NOT GUARANTEE THAT THE OPERATION OF THE
 *  SOFTWARE WILL BE UNINTERRUPTED OR ERROR-FREE, AND YOU ACKNOWLEDGE THAT
 *  IT IS NOT TECHNICALLY PRACTICABLE FOR US TO DO SO.
 *
 *  7. LIMITATION OF LIABILITIES
 *
 *  TO THE MAXIMUM EXTENT PERMITTED BY APPLICABLE LAW, IN NO EVENT SHALL WE
 *  OR OUR SUPPLIERS BE LIABLE FOR ANY SPECIAL, INCIDENTAL, INDIRECT OR
 *  CONSEQUENTIAL DAMAGES WHATSOEVER (INCLUDING, WITHOUT LIMITATION,
 *  DAMAGES FOR LOSS OF BUSINESS PROFITS, BUSINESS INTERRUPTION, LOSS OF
 *  BUSINESS INFORMATION OR ANY OTHER PECUNIARY LAW) ARISING OUT OF THE USE
 *  OF OR INABILITY TO USE THE SOFTWARE, EVEN IF WE HAVE BEEN ADVISED OF
 *  THE POSSIBILITY OF SUCH DAMAGES. IN ANY CASE, OUR ENTIRE LIABILITY
 *  UNDER ANY PROVISION OF THIS LICENSE AGREEMENT SHALL BE LIMITED TO THE
 *  GREATER OF THE AMOUNT ACTUALLY PAID BY YOU FOR THE SOFTWARE OR FIVE
 *  ($5.00) DOLLARS (USD).
 *
 *  8. MISCELLANEOUS
 *
 *  The license granted herein applies only to the version of the Software
 *  downloaded or installed in connection with the terms of this Agreement.
 *  Any previous or subsequent license granted to You for use of the
 *  Software shall be governed by the terms and conditions of the agreement
 *  entered in connection with downloading or installation of that version
 *  of the Software.
 *
 *  You agree that you will comply with all applicable laws and regulations
 *  with respect to the Software, including without limitation all export
 *  and re-export control laws and regulations.
 *
 *  While redistributing the Software or Modifications thereof, You may
 *  choose to offer acceptance of support, warranty, indemnity, or other
 *  liability obligations and/or rights consistent with this Agreement.
 *  However, in accepting such obligations, You may act only on Your own
 *  behalf and on Your sole responsibility, not on our behalf. You agree to
 *  indemnify, defend, and hold Us harmless from and against any liability
 *  incurred by, or claims asserted against, Us by reason of Your accepting
 *  any such support, warranty, indemnity or additional liability.
 *
 *  You may not assign this License Agreement without Our prior written
 *  consent. This License Agreement will inure to the benefit of Our
 *  successors and assigns.
 *
 *  You acknowledge that this License Agreement is complete and is the
 *  exclusive representation of our agreement. No oral or written
 *  information given by Us or on our behalf shall create a warranty or
 *  collateral contract, or in any way increase the scope of this License
 *  Agreement in any way, and You may not rely on any such oral or written
 *  information.
 *
 *  If any provision in this License Agreement shall be determined to be
 *  invalid, such provision shall be deemed omitted; the remainder of this
 *  License Agreement shall continue in full force and effect.
 *
 *  This License Agreement may be modified only by a written instrument
 *  signed by an authorized representative of each party.
 *
 *  This Agreement is governed by the law of the Commonwealth of
 *  Pennsylvania, United States (notwithstanding conflicts of laws
 *  provisions), and all parties irrevocably submit to the jurisdiction of
 *  the courts of the Commonwealth of Pennsylvania and further agree to
 *  commence any litigation which may arise hereunder in the state or
 *  federal courts located in the judicial district of Washington County,
 *  Pennsylvania, US.
 *
 *  If the Software or any related documentation is licensed to the U.S.
 *  government or any agency thereof, it will be deemed to be "commercial
 *  computer software" or "commercial computer software documentation,"
 *  pursuant to SFAR Section 227.7202 and FAR Section 12.212. Any use of
 *  the Software or related documentation by the U.S. government will be
 *  governed solely by the terms of this License Agreement.
 ****************************************************************************/

//=============================================================================
//  AnyTime.utcLabel is an array of arrays, indexed by UTC offset IN MINUTES
//  (not hours-and-minutes).  This is used by AnyTime.Converter to display
//  time zone labels when the "%@" format specifier is used.  It is also used
//  by AnyTime.widget() to determine which time zone labels to offer as valid
//  choices when a user attempts to change the time zone.  NOTE: Positive
//  indices are NOT signed.
//
//  Each sub-array contains a series of strings, each of which is a label
//  for a time-zone having the corresponding UTC offset.  The first string in
//  each sub-array is the default label for that UTC offset (the one used by
//  AnyTime.Converter.format() if utcFormatOffsetSubIndex is not specified and
//  setUtcFormatOffsetSubIndex() is not called.
//=============================================================================

AnyTime.utcLabelMinutes = [];
AnyTime.utcLabelMinutes[-720] = ["-1200"];
AnyTime.utcLabelMinutes[-690] = ["-1130"];
AnyTime.utcLabelMinutes[-660] = ["-1100"];
AnyTime.utcLabelMinutes[-600] = ["-1000"];
AnyTime.utcLabelMinutes[-570] = ["-0930"];
AnyTime.utcLabelMinutes[-540] = ["-0900"];
AnyTime.utcLabelMinutes[-480] = ["-0800"];
AnyTime.utcLabelMinutes[-420] = ["-0700"];
AnyTime.utcLabelMinutes[-360] = ["-0600"];
AnyTime.utcLabelMinutes[-300] = ["-0500"];
AnyTime.utcLabelMinutes[-270] = ["-0430"];
AnyTime.utcLabelMinutes[-240] = ["-0400"];
AnyTime.utcLabelMinutes[-210] = ["-0330"];
AnyTime.utcLabelMinutes[-180] = ["-0300"];
AnyTime.utcLabelMinutes[-150] = ["-0230"];
AnyTime.utcLabelMinutes[-120] = ["-0200"];
AnyTime.utcLabelMinutes[-60] = ["-0100"];
AnyTime.utcLabelMinutes[0] = ["+0000"];
AnyTime.utcLabelMinutes[60] = ["+0100"];
AnyTime.utcLabelMinutes[120] = ["+0200"];
AnyTime.utcLabelMinutes[180] = ["+0300"];
AnyTime.utcLabelMinutes[210] = ["+0330"];
AnyTime.utcLabelMinutes[240] = ["+0400"];
AnyTime.utcLabelMinutes[270] = ["+0430"];
AnyTime.utcLabelMinutes[300] = ["+0500"];
AnyTime.utcLabelMinutes[330] = ["+0530"];
AnyTime.utcLabelMinutes[345] = ["+0545"];
AnyTime.utcLabelMinutes[360] = ["+0600"];
AnyTime.utcLabelMinutes[390] = ["+0630"];
AnyTime.utcLabelMinutes[420] = ["+0700"];
AnyTime.utcLabelMinutes[480] = ["+0800"];
AnyTime.utcLabelMinutes[525] = ["+0845"];
AnyTime.utcLabelMinutes[540] = ["+0900"];
AnyTime.utcLabelMinutes[570] = ["+0930"];
AnyTime.utcLabelMinutes[600] = ["+1000"];
AnyTime.utcLabelMinutes[630] = ["+1030"];
AnyTime.utcLabelMinutes[660] = ["+1100"];
AnyTime.utcLabelMinutes[690] = ["+1130"];
AnyTime.utcLabelMinutes[720] = ["+1200"];
AnyTime.utcLabelMinutes[765] = ["+1245"];
AnyTime.utcLabelMinutes[780] = ["+1300"];
AnyTime.utcLabelMinutes[825] = ["+1345"];
AnyTime.utcLabelMinutes[840] = ["+1400"];


AnyTime.utcLabel = [];
AnyTime.utcLabel["-1200"]=["Baker Island Time  -- BIT"];
AnyTime.utcLabel["-1130"]=["Niue Time  -- NUT"];
AnyTime.utcLabel["-1100"]=["Samoa Standard Time  -- SST"];
AnyTime.utcLabel["-1000"]=[
	"Cook Island Time  -- CKT",
	"Hawaii-Aleutian Standard Time  -- HAST",
	"Hawaii Standard Time  -- HST",
	"Tahiti Time  -- TAHT"
];
AnyTime.utcLabel["-0930"]=[
	"Marquesas Islands Time  -- MART",
	"Marquesas Islands Time  -- MIT"
];
AnyTime.utcLabel["-0900"]=[
	"Alaska Standard Time  -- AKST",
	"Gambier Islands  -- GAMT",
	"Gambier Island Time  -- GIT",
	"Hawaii-Aleutian Daylight Time  -- HADT"
];
AnyTime.utcLabel["-0800"]=[
	"Alaska Daylight Time  -- AKDT",
	"Choibalsan  -- CHOT",
	"Clipperton Island Standard Time  -- CIST",
	"Pacific Standard Time (North America)  -- PST"
];
AnyTime.utcLabel["-0700"]=[
	"Mountain Standard Time (North America)  -- MST",
	"Pacific Daylight Time (North America)  -- PDT"
];
AnyTime.utcLabel["-0600"]=[
	"Central Standard Time (North America)  -- CST",
	"Easter Island Standard Time  -- EAST",
	"Galapagos Time  -- GALT",
	"Mountain Daylight Time (North America)  -- MDT"
];
AnyTime.utcLabel["-0500"]=[
	"Central Daylight Time (North America)  -- CDT",
	"Colombia Time  -- COT",
	"Cuba Standard Time  -- CST",,
	"Easter Island Standard Summer Time  -- EASST",
	"Ecuador Time  -- ECT",
	"Eastern Standard Time (North America)  -- EST",
	"Peru Time  -- PET"
];
AnyTime.utcLabel["-0430"]=["Venezuelan Standard Time  -- VET"];
AnyTime.utcLabel["-0400"]=[
	"Atlantic Standard Time  -- AST",
	"Bolivia Time  -- BOT",
	"Cuba Daylight Time  -- CDT",
	"Chile Standard Time  -- CLT",
	"Colombia Summer Time  -- COST",
	"Eastern Caribbean Time  -- ECT",
	"Eastern Daylight Time (North America)  -- EDT",
	"Falkland Islands Time  -- FKT",
	"Guyana Time  -- GYT"
];
AnyTime.utcLabel["-0330"]=[
	"Newfoundland Standard Time  -- NST",
	"Newfoundland Time  -- NT"
];
AnyTime.utcLabel["-0300"]=[
	"Atlantic Daylight Time  -- ADT",
	"Argentina Time  -- ART",
	"Brasilia Time  -- BRT",
	"Chile Summer Time  -- CLST",
	"Falkland Islands Summer Time  -- FKST",
	"French Guiana Time  -- GFT",
	"Saint Pierre and Miquelon Standard Time  -- PMST",
	"Rothera Research Station Time  -- ROTT",
	"Suriname Time  -- SRT",
	"Uruguay Standard Time  -- UYT"
];
AnyTime.utcLabel["-0230"]=["Newfoundland Daylight Time  -- NDT"];
AnyTime.utcLabel["-0200"]=[
	"Fernando de Noronha Time  -- FNT",
	"South Georgia and the South Sandwich Islands  -- GST",
	"Saint Pierre and Miquelon Daylight time  -- PMDT",
	"Uruguay Summer Time  -- UYST"
];
AnyTime.utcLabel["-0100"]=[
	"Azores Standard Time  -- AZOST",
	"Cape Verde Time  -- CVT",
	"Eastern Greenland Time  -- EGT"
];
AnyTime.utcLabel["+0000"]=[
	"Greenwich Mean Time  -- GMT",
	"Coordinated Universal Time  -- UTC",
	"Western European Time  -- WET",
	"Eastern Greenland Summer Time  -- EGST"
];
AnyTime.utcLabel["+0100"]=[
	"British Summer Time  -- BST",
	"Central European Time  -- CET",
	"AIX specific equivalent of Central European Time  -- DFT",
	"Irish Standard Time  -- IST",
	"Middle European Time  -- MET",
	"West Africa Time  -- WAT",
	"Western European Daylight Time  -- WEDT",
	"Western European Summer Time  -- WEST"
];
AnyTime.utcLabel["+0200"]=[
	"Central Africa Time  -- CAT",
	"Central European Daylight Time  -- CEDT",
	"Central European Summer Time  -- CEST",
	"Eastern European Time  -- EET",
	"Heure Avanc&eacute;e d'Europe Centrale (CEST)  -- HAEC",
	"Israel Standard Time  -- IST",
	"Middle European Saving Time  -- MEST",
	"South African Standard Time  -- SAST",
	"West Africa Summer Time  -- WAST"
];
AnyTime.utcLabel["+0300"]=[
	"Arabia Standard Time  -- AST",
	"East Africa Time  -- EAT",
	"Eastern European Daylight Time  -- EEDT",
	"Eastern European Summer Time  -- EEST",
	"Further-eastern European Time  -- FET",
	"Israel Daylight Time  -- IDT",
	"Indian Ocean Time  -- IOT",
	"Showa Station Time  -- SYOT"
];
AnyTime.utcLabel["+0330"]=["Iran Standard Time  -- IRST"];
AnyTime.utcLabel["+0400"]=[
	"Armenia Time  -- AMT",
	"Azerbaijan Time  -- AZT",
	"Georgia Standard Time  -- GET",
	"Gulf Standard Time  -- GST",
	"Moscow Time  -- MSK",
	"Mauritius Time  -- MUT",
	"R&eacute;union Time  -- RET",
	"Samara Time  -- SAMT",
	"Seychelles Time  -- SCT",
	"Volgograd Time  -- VOLT"
];
AnyTime.utcLabel["+0430"]=["Afghanistan Time  -- AFT"];
AnyTime.utcLabel["+0500"]=[
	"Armenia Summer Time  -- AMST",
	"Heard and McDonald Islands Time  -- HMT",
	"Mawson Station Time  -- MAWT",
	"Maldives Time  -- MVT",
	"Oral Time  -- ORAT",
	"Pakistan Standard Time  -- PKT",
	"Indian/Kerguelen  -- TFT",
	"Tajikistan Time  -- TJT",
	"Turkmenistan Time  -- TMT",
	"Uzbekistan Time  -- UZT",
	"Yekaterinburg Time  -- YEKT"
];
AnyTime.utcLabel["+0530"]=[
	"Indian Standard Time  -- IST",
	"Sri Lanka Time  -- SLT"
];
AnyTime.utcLabel["+0545"]=["Nepal Time  -- NPT"];
AnyTime.utcLabel["+0600"]=[
	"British Indian Ocean Time  -- BIOT",
	"Bangladesh Standard Time  -- BST",
	"Bhutan Time  -- BTT",
	"Kyrgyzstan time  -- KGT",
	"Omsk Time  -- OMST",
	"Vostok Station Time  -- VOST"
];
AnyTime.utcLabel["+0630"]=[
	"Cocos Islands Time  -- CCT",
	"Myanmar Time  -- MMT",
	"Myanmar Standard Time  -- MST"
];
AnyTime.utcLabel["+0700"]=[
	"Christmas Island Time  -- CXT",
	"Davis Time  -- DAVT",
	"Khovd Time  -- HOVT",
	"Indochina Time  -- ICT",
	"Krasnoyarsk Time  -- KRAT",
	"Thailand Standard Time  -- THA"
];
AnyTime.utcLabel["+0800"]=[
	"ASEAN Common Time  -- ACT",
	"Australian Western Standard Time  -- AWST",
	"Brunei Time  -- BDT",
	"Central Indonesia Time  -- CIT",
	"China Standard Time  -- CST",
	"China time  -- CT",
	"Hong Kong Time  -- HKT",
	"Iran Daylight Time  -- IRDT",
	"Malaysia Standard Time  -- MST",
	"Malaysia Time  -- MYT",
	"Philippine Time  -- PHT",
	"Singapore Time  -- SGT",
	"Singapore Standard Time  -- SST",
	"Ulaanbaatar Time  -- ULAT",
	"Western Standard Time  -- WST"
];
AnyTime.utcLabel["+0845"]=["Central Western Standard Time (Australia)  -- CWST"];
AnyTime.utcLabel["+0900"]=[
	"Australian Western Daylight Time  -- AWDT",
	"Eastern Indonesian Time  -- EIT",
	"Irkutsk Time  -- IRKT",
	"Japan Standard Time  -- JST",
	"Korea Standard Time  -- KST",
	"Timor Leste Time  -- TLT",
	"Yakutsk Time  -- YAKT"
];
AnyTime.utcLabel["+0930"]=[
	"Australian Central Standard Time  -- ACST",
	"Central Standard Time (Australia)  -- CST"
];
AnyTime.utcLabel["+1000"]=[
	"Australian Eastern Standard Time  -- AEST",
	"Chamorro Standard Time  -- ChST",
	"Chuuk Time  -- CHUT",
	"Dumont d'Urville Time  -- DDUT",
	"Eastern Standard Time (Australia)  -- EST",
	"Papua New Guinea Time  -- PGT",
	"Vladivostok Time  -- VLAT"
];
AnyTime.utcLabel["+1030"]=[
	"Australian Central Daylight Time  -- ACDT",
	"Central Summer Time (Australia)  -- CST",
	"Lord Howe Standard Time  -- LHST"
];
AnyTime.utcLabel["+1100"]=[
	"Australian Eastern Daylight Time  -- AEDT",
	"Kosrae Time  -- KOST",
	"Lord Howe Summer Time  -- LHST",
	"Macquarie Island Station Time  -- MIST",
	"New Caledonia Time  -- NCT",
	"Pohnpei Standard Time  -- PONT",
	"Sakhalin Island time  -- SAKT",
	"Solomon Islands Time  -- SBT",
	"Vanuatu Time  -- VUT"
];
AnyTime.utcLabel["+1130"]=["Norfolk Time  -- NFT"];
AnyTime.utcLabel["+1200"]=[
	"Fiji Time  -- FJT",
	"Gilbert Island Time  -- GILT",
	"Magadan Time  -- MAGT",
	"Marshall Islands  -- MHT",
	"New Zealand Standard Time  -- NZST",
	"Kamchatka Time  -- PETT",
	"Tuvalu Time  -- TVT",
	"Wake Island Time  -- WAKT"
];
AnyTime.utcLabel["+1245"]=["Chatham Standard Time  -- CHAST"];
AnyTime.utcLabel["+1300"]=[
	"New Zealand Daylight Time  -- NZDT",
	"Phoenix Island Time  -- PHOT",
	"Tonga Time  -- TOT"
];
AnyTime.utcLabel["+1345"]=["Chatham Daylight Time  -- CHADT"];
AnyTime.utcLabel["+1400"]=[
	"Line Islands Time  -- LINT",
	"Tokelau Time  -- TKT"
];

//
//END OF FILE
//