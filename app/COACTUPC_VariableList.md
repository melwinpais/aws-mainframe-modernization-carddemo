# COACTUPC Variable List Analysis

## Executive Summary Table
| Metric | Count |
|--------|-------|
| Total Variables | 316 |
| Working Storage Variables | 316 |
| Linkage Variables | 1 |
| Copybook Variables | 56 |
| Interface Variables | 45 |
| Group Items | 45 |
| Elementary Items | 271 |
| Condition Names (88-level) | 83 |

## Variable Inventory by Section

### Working Storage Section (Program-Specific)

#### WS-MISC-STORAGE Group
| Level | Variable Name | Picture | Type | Length | Usage | Value | Description |
|-------|---------------|---------|------|--------|-------|-------|-------------|
| 01 | WS-MISC-STORAGE | Group | Group | 2000+ | Working | N/A | Main working storage group |
| 05 | WS-CICS-PROCESSNG-VARS | Group | Group | 16 | Working | N/A | CICS processing variables |
| 07 | WS-RESP-CD | S9(09) COMP | Binary | 4 | Working | ZEROS | CICS response code |
| 07 | WS-REAS-CD | S9(09) COMP | Binary | 4 | Working | ZEROS | CICS reason code |
| 07 | WS-TRANID | X(4) | Alpha | 4 | Working | SPACES | Transaction ID |
| 07 | WS-UCTRANS | X(4) | Alpha | 4 | Working | SPACES | Upper case transaction |

#### Generic Input Edits Group
| Level | Variable Name | Picture | Type | Length | Usage | Value | Description |
|-------|---------------|---------|------|--------|-------|-------|-------------|
| 05 | WS-GENERIC-EDITS | Group | Group | 350+ | Working | N/A | Generic input validation |
| 10 | WS-EDIT-VARIABLE-NAME | X(25) | Alpha | 25 | Working | N/A | Variable name for edits |
| 10 | WS-EDIT-SIGNED-NUMBER-9V2-X | X(15) | Alpha | 15 | Working | N/A | Signed number edit field |
| 10 | WS-FLG-SIGNED-NUMBER-EDIT | X(1) | Alpha | 1 | Working | N/A | Signed number edit flag |
| 10 | WS-EDIT-ALPHANUM-ONLY | X(256) | Alpha | 256 | Working | N/A | Alphanumeric edit field |
| 10 | WS-EDIT-ALPHANUM-LENGTH | S9(4) COMP-3 | Packed | 3 | Working | N/A | Alphanumeric field length |
| 10 | WS-EDIT-ALPHA-ONLY-FLAGS | X(1) | Alpha | 1 | Working | N/A | Alpha-only validation flag |
| 10 | WS-EDIT-ALPHANUM-ONLY-FLAGS | X(1) | Alpha | 1 | Working | N/A | Alphanumeric validation flag |
| 10 | WS-EDIT-MANDATORY-FLAGS | X(1) | Alpha | 1 | Working | N/A | Mandatory field flag |
| 10 | WS-EDIT-YES-NO | X(1) | Alpha | 1 | Working | 'N' | Yes/No validation field |

#### US Phone Number Edit Structure
| Level | Variable Name | Picture | Type | Length | Usage | Value | Description |
|-------|---------------|---------|------|--------|-------|-------|-------------|
| 10 | WS-EDIT-US-PHONE-NUM | X(15) | Alpha | 15 | Working | N/A | US phone number field |
| 10 | WS-EDIT-US-PHONE-NUM-X | REDEFINES | Group | 15 | Working | N/A | Phone number breakdown |
| 20 | WS-EDIT-US-PHONE-NUMA | X(3) | Alpha | 3 | Working | N/A | Area code |
| 20 | WS-EDIT-US-PHONE-NUMA-N | 9(3) REDEFINES | Numeric | 3 | Working | N/A | Area code numeric |
| 20 | WS-EDIT-US-PHONE-NUMB | X(3) | Alpha | 3 | Working | N/A | Exchange code |
| 20 | WS-EDIT-US-PHONE-NUMB-N | 9(3) REDEFINES | Numeric | 3 | Working | N/A | Exchange numeric |
| 20 | WS-EDIT-US-PHONE-NUMC | X(4) | Alpha | 4 | Working | N/A | Line number |
| 20 | WS-EDIT-US-PHONE-NUMC-N | 9(4) REDEFINES | Numeric | 4 | Working | N/A | Line number numeric |

#### US SSN Edit Structure
| Level | Variable Name | Picture | Type | Length | Usage | Value | Description |
|-------|---------------|---------|------|--------|-------|-------|-------------|
| 10 | WS-EDIT-US-SSN | Group | Group | 9 | Working | N/A | Social Security Number |
| 20 | WS-EDIT-US-SSN-PART1 | X(3) | Alpha | 3 | Working | N/A | SSN first part |
| 20 | WS-EDIT-US-SSN-PART1-N | 9(3) REDEFINES | Numeric | 3 | Working | N/A | SSN first part numeric |
| 20 | WS-EDIT-US-SSN-PART2 | X(2) | Alpha | 2 | Working | N/A | SSN second part |
| 20 | WS-EDIT-US-SSN-PART2-N | 9(2) REDEFINES | Numeric | 2 | Working | N/A | SSN second part numeric |
| 20 | WS-EDIT-US-SSN-PART3 | X(4) | Alpha | 4 | Working | N/A | SSN third part |
| 20 | WS-EDIT-US-SSN-PART3-N | 9(4) REDEFINES | Numeric | 4 | Working | N/A | SSN third part numeric |
| 10 | WS-EDIT-US-SSN-N | 9(09) REDEFINES | Numeric | 9 | Working | N/A | Complete SSN numeric |

#### Calculation Variables
| Level | Variable Name | Picture | Type | Length | Usage | Value | Description |
|-------|---------------|---------|------|--------|-------|-------|-------------|
| 05 | WS-CALCULATION-VARS | Group | Group | 15 | Working | N/A | Calculation variables |
| 10 | WS-DIV-BY | S9(4) COMP-3 | Packed | 3 | Working | 4 | Division factor |
| 10 | WS-DIVIDEND | S9(4) COMP-3 | Packed | 3 | Working | 0 | Dividend value |
| 10 | WS-REMAINDER | S9(4) COMP-3 | Packed | 3 | Working | 0 | Remainder value |
| 10 | WS-CURR-DATE | X(21) | Alpha | 21 | Working | SPACES | Current date string |

#### Program Control Flags
| Level | Variable Name | Picture | Type | Length | Usage | Value | Description |
|-------|---------------|---------|------|--------|-------|-------|-------------|
| 05 | WS-DATACHANGED-FLAG | X(1) | Alpha | 1 | Working | N/A | Data change indicator |
| 05 | WS-INPUT-FLAG | X(1) | Alpha | 1 | Working | N/A | Input validation flag |
| 05 | WS-RETURN-FLAG | X(1) | Alpha | 1 | Working | N/A | Return processing flag |
| 05 | WS-PFK-FLAG | X(1) | Alpha | 1 | Working | N/A | PF key validation flag |
| 05 | WS-EDIT-ACCT-FLAG | X(1) | Alpha | 1 | Working | N/A | Account filter flag |
| 05 | WS-EDIT-CUST-FLAG | X(1) | Alpha | 1 | Working | N/A | Customer filter flag |

#### Field Validation Flags Group
| Level | Variable Name | Picture | Type | Length | Usage | Value | Description |
|-------|---------------|---------|------|--------|-------|-------|-------------|
| 05 | WS-NON-KEY-FLAGS | Group | Group | 50+ | Working | N/A | Non-key field validation |
| 10 | WS-EDIT-ACCT-STATUS | X(1) | Alpha | 1 | Working | N/A | Account status flag |
| 10 | WS-EDIT-CREDIT-LIMIT | X(1) | Alpha | 1 | Working | N/A | Credit limit flag |
| 10 | WS-EDIT-CASH-CREDIT-LIMIT | X(1) | Alpha | 1 | Working | N/A | Cash credit limit flag |
| 10 | WS-EDIT-CURR-BAL | X(1) | Alpha | 1 | Working | N/A | Current balance flag |
| 10 | WS-EDIT-CURR-CYC-CREDIT | X(1) | Alpha | 1 | Working | N/A | Current cycle credit flag |
| 10 | WS-EDIT-CURR-CYC-DEBIT | X(1) | Alpha | 1 | Working | N/A | Current cycle debit flag |
| 10 | WS-EDIT-FICO-SCORE-FLGS | X(1) | Alpha | 1 | Working | N/A | FICO score flag |

#### Date Validation Flags
| Level | Variable Name | Picture | Type | Length | Usage | Value | Description |
|-------|---------------|---------|------|--------|-------|-------|-------------|
| 10 | WS-EDIT-DT-OF-BIRTH-FLGS | Group | Group | 3 | Working | N/A | Date of birth flags |
| 20 | WS-EDIT-DT-OF-BIRTH-YEAR-FLG | X(01) | Alpha | 1 | Working | N/A | Birth year flag |
| 20 | WS-EDIT-DT-OF-BIRTH-MONTH | X(01) | Alpha | 1 | Working | N/A | Birth month flag |
| 20 | WS-EDIT-DT-OF-BIRTH-DAY | X(01) | Alpha | 1 | Working | N/A | Birth day flag |
| 10 | WS-EDIT-OPEN-DATE-FLGS | Group | Group | 3 | Working | N/A | Open date flags |
| 20 | WS-EDIT-OPEN-YEAR-FLG | X(01) | Alpha | 1 | Working | N/A | Open year flag |
| 20 | WS-EDIT-OPEN-MONTH | X(01) | Alpha | 1 | Working | N/A | Open month flag |
| 20 | WS-EDIT-OPEN-DAY | X(01) | Alpha | 1 | Working | N/A | Open day flag |
| 10 | WS-EXPIRY-DATE-FLGS | Group | Group | 3 | Working | N/A | Expiry date flags |
| 20 | WS-EDIT-EXPIRY-YEAR-FLG | X(01) | Alpha | 1 | Working | N/A | Expiry year flag |
| 20 | WS-EDIT-EXPIRY-MONTH | X(01) | Alpha | 1 | Working | N/A | Expiry month flag |
| 20 | WS-EDIT-EXPIRY-DAY | X(01) | Alpha | 1 | Working | N/A | Expiry day flag |
| 10 | WS-EDIT-REISSUE-DATE-FLGS | Group | Group | 3 | Working | N/A | Reissue date flags |
| 20 | WS-EDIT-REISSUE-YEAR-FLG | X(01) | Alpha | 1 | Working | N/A | Reissue year flag |
| 20 | WS-EDIT-REISSUE-MONTH | X(01) | Alpha | 1 | Working | N/A | Reissue month flag |
| 20 | WS-EDIT-REISSUE-DAY | X(01) | Alpha | 1 | Working | N/A | Reissue day flag |

#### Name and Address Validation Flags
| Level | Variable Name | Picture | Type | Length | Usage | Value | Description |
|-------|---------------|---------|------|--------|-------|-------|-------------|
| 10 | WS-EDIT-NAME-FLAGS | Group | Group | 3 | Working | N/A | Name validation flags |
| 20 | WS-EDIT-FIRST-NAME-FLGS | X(01) | Alpha | 1 | Working | N/A | First name flag |
| 20 | WS-EDIT-MIDDLE-NAME-FLGS | X(01) | Alpha | 1 | Working | N/A | Middle name flag |
| 20 | WS-EDIT-LAST-NAME-FLGS | X(01) | Alpha | 1 | Working | N/A | Last name flag |
| 10 | WS-EDIT-ADDRESS-FLAGS | Group | Group | 12 | Working | N/A | Address validation flags |
| 20 | WS-EDIT-ADDRESS-LINE-1-FLGS | X(01) | Alpha | 1 | Working | N/A | Address line 1 flag |
| 20 | WS-EDIT-ADDRESS-LINE-2-FLGS | X(01) | Alpha | 1 | Working | N/A | Address line 2 flag |
| 20 | WS-EDIT-CITY-FLGS | X(01) | Alpha | 1 | Working | N/A | City flag |
| 20 | WS-EDIT-STATE-FLGS | X(01) | Alpha | 1 | Working | N/A | State flag |
| 20 | WS-EDIT-ZIPCODE-FLGS | X(01) | Alpha | 1 | Working | N/A | Zip code flag |
| 20 | WS-EDIT-COUNTRY-FLGS | X(01) | Alpha | 1 | Working | N/A | Country flag |

#### Phone Number Validation Flags
| Level | Variable Name | Picture | Type | Length | Usage | Value | Description |
|-------|---------------|---------|------|--------|-------|-------|-------------|
| 20 | WS-EDIT-PHONE-NUM-1-FLGS | Group | Group | 3 | Working | N/A | Phone 1 validation flags |
| 30 | WS-EDIT-PHONE-NUM-1A-FLG | X(01) | Alpha | 1 | Working | N/A | Phone 1 area code flag |
| 30 | WS-EDIT-PHONE-NUM-1B | X(01) | Alpha | 1 | Working | N/A | Phone 1 exchange flag |
| 30 | WS-EDIT-PHONE-NUM-1C | X(01) | Alpha | 1 | Working | N/A | Phone 1 line flag |
| 20 | WS-EDIT-PHONE-NUM-2-FLGS | Group | Group | 3 | Working | N/A | Phone 2 validation flags |
| 30 | WS-EDIT-PHONE-NUM-2A-FLG | X(01) | Alpha | 1 | Working | N/A | Phone 2 area code flag |
| 30 | WS-EDIT-PHONE-NUM-2B | X(01) | Alpha | 1 | Working | N/A | Phone 2 exchange flag |
| 30 | WS-EDIT-PHONE-NUM-2C | X(01) | Alpha | 1 | Working | N/A | Phone 2 line flag |
| 10 | WS-EFT-ACCOUNT-ID-FLGS | X(01) | Alpha | 1 | Working | N/A | EFT account ID flag |
| 10 | WS-EDIT-PRI-CARDHOLDER | X(1) | Alpha | 1 | Working | N/A | Primary cardholder flag |

#### Output Edit Variables
| Level | Variable Name | Picture | Type | Length | Usage | Value | Description |
|-------|---------------|---------|------|--------|-------|-------|-------------|
| 05 | CICS-OUTPUT-EDIT-VARS | Group | Group | 60+ | Working | N/A | Output formatting variables |
| 10 | CUST-ACCT-ID-X | X(11) | Alpha | 11 | Working | N/A | Customer account ID alpha |
| 10 | CUST-ACCT-ID-N | 9(11) REDEFINES | Numeric | 11 | Working | N/A | Customer account ID numeric |
| 10 | WS-EDIT-DATE-X | X(10) | Alpha | 10 | Working | N/A | Date edit field |
| 20 | WS-EDIT-DATE-X-YEAR | X(4) | Alpha | 4 | Working | N/A | Date year part |
| 20 | WS-EDIT-DATE-MONTH | X(2) | Alpha | 2 | Working | N/A | Date month part |
| 20 | WS-EDIT-DATE-DAY | X(2) | Alpha | 2 | Working | N/A | Date day part |
| 10 | WS-EDIT-CURRENCY-9-2 | X(15) | Alpha | 15 | Working | N/A | Currency edit field |
| 10 | WS-EDIT-CURRENCY-9-2-F | +ZZZ,ZZZ,ZZZ.99 | Edited | 15 | Working | N/A | Formatted currency |

#### File Handling Variables
| Level | Variable Name | Picture | Type | Length | Usage | Value | Description |
|-------|---------------|---------|------|--------|-------|-------|-------------|
| 05 | WS-XREF-RID | Group | Group | 36 | Working | N/A | Cross-reference record ID |
| 10 | WS-CARD-RID-CARDNUM | X(16) | Alpha | 16 | Working | N/A | Card number |
| 10 | WS-CARD-RID-CUST-ID | 9(09) | Numeric | 9 | Working | N/A | Customer ID numeric |
| 10 | WS-CARD-RID-CUST-ID-X | X(09) REDEFINES | Alpha | 9 | Working | N/A | Customer ID alpha |
| 10 | WS-CARD-RID-ACCT-ID | 9(11) | Numeric | 11 | Working | N/A | Account ID numeric |
| 10 | WS-CARD-RID-ACCT-ID-X | X(11) REDEFINES | Alpha | 11 | Working | N/A | Account ID alpha |
| 05 | WS-FILE-READ-FLAGS | Group | Group | 2 | Working | N/A | File read status flags |
| 10 | WS-ACCOUNT-MASTER-READ-FLAG | X(1) | Alpha | 1 | Working | N/A | Account master read flag |
| 10 | WS-CUST-MASTER-READ-FLAG | X(1) | Alpha | 1 | Working | N/A | Customer master read flag |

#### Error Message Construction
| Level | Variable Name | Picture | Type | Length | Usage | Value | Description |
|-------|---------------|---------|------|--------|-------|-------|-------------|
| 05 | WS-FILE-ERROR-MESSAGE | Group | Group | 80 | Working | N/A | File error message |
| 10 | ERROR-OPNAME | X(8) | Alpha | 8 | Working | SPACES | Operation name |
| 10 | ERROR-FILE | X(9) | Alpha | 9 | Working | SPACES | File name |
| 10 | ERROR-RESP | X(10) | Alpha | 10 | Working | SPACES | Response code |
| 10 | ERROR-RESP2 | X(10) | Alpha | 10 | Working | SPACES | Reason code |

#### Alpha Variables for Data Editing
| Level | Variable Name | Picture | Type | Length | Usage | Value | Description |
|-------|---------------|---------|------|--------|-------|-------|-------------|
| 05 | ALPHA-VARS-FOR-DATA-EDITING | Group | Group | 75 | Working | N/A | Alpha editing variables |
| 15 | ACUP-NEW-CREDIT-LIMIT-X | X(15) | Alpha | 15 | Working | N/A | Credit limit alpha |
| 15 | ACUP-NEW-CASH-CREDIT-LIMIT-X | X(15) | Alpha | 15 | Working | N/A | Cash credit limit alpha |
| 15 | ACUP-NEW-CURR-BAL-X | X(15) | Alpha | 15 | Working | N/A | Current balance alpha |
| 15 | ACUP-NEW-CURR-CYC-CREDIT-X | X(15) | Alpha | 15 | Working | N/A | Current cycle credit alpha |
| 15 | ACUP-NEW-CURR-CYC-DEBIT-X | X(15) | Alpha | 15 | Working | N/A | Current cycle debit alpha |

#### Account Update Record Structure
| Level | Variable Name | Picture | Type | Length | Usage | Value | Description |
|-------|---------------|---------|------|--------|-------|-------|-------------|
| 05 | ACCT-UPDATE-RECORD | Group | Group | 300 | Working | N/A | Account update record |
| 15 | ACCT-UPDATE-ID | 9(11) | Numeric | 11 | Working | N/A | Account ID |
| 15 | ACCT-UPDATE-ACTIVE-STATUS | X(01) | Alpha | 1 | Working | N/A | Active status |
| 15 | ACCT-UPDATE-CURR-BAL | S9(10)V99 | Signed | 12 | Working | N/A | Current balance |
| 15 | ACCT-UPDATE-CREDIT-LIMIT | S9(10)V99 | Signed | 12 | Working | N/A | Credit limit |
| 15 | ACCT-UPDATE-CASH-CREDIT-LIMIT | S9(10)V99 | Signed | 12 | Working | N/A | Cash credit limit |
| 15 | ACCT-UPDATE-OPEN-DATE | X(10) | Alpha | 10 | Working | N/A | Open date |
| 15 | ACCT-UPDATE-EXPIRAION-DATE | X(10) | Alpha | 10 | Working | N/A | Expiration date |
| 15 | ACCT-UPDATE-REISSUE-DATE | X(10) | Alpha | 10 | Working | N/A | Reissue date |
| 15 | ACCT-UPDATE-CURR-CYC-CREDIT | S9(10)V99 | Signed | 12 | Working | N/A | Current cycle credit |
| 15 | ACCT-UPDATE-CURR-CYC-DEBIT | S9(10)V99 | Signed | 12 | Working | N/A | Current cycle debit |
| 15 | ACCT-UPDATE-GROUP-ID | X(10) | Alpha | 10 | Working | N/A | Group ID |

#### Customer Update Record Structure
| Level | Variable Name | Picture | Type | Length | Usage | Value | Description |
|-------|---------------|---------|------|--------|-------|-------|-------------|
| 05 | CUST-UPDATE-RECORD | Group | Group | 300 | Working | N/A | Customer update record |
| 15 | CUST-UPDATE-ID | 9(09) | Numeric | 9 | Working | N/A | Customer ID |
| 15 | CUST-UPDATE-FIRST-NAME | X(25) | Alpha | 25 | Working | N/A | First name |
| 15 | CUST-UPDATE-MIDDLE-NAME | X(25) | Alpha | 25 | Working | N/A | Middle name |
| 15 | CUST-UPDATE-LAST-NAME | X(25) | Alpha | 25 | Working | N/A | Last name |
| 15 | CUST-UPDATE-ADDR-LINE-1 | X(50) | Alpha | 50 | Working | N/A | Address line 1 |
| 15 | CUST-UPDATE-ADDR-LINE-2 | X(50) | Alpha | 50 | Working | N/A | Address line 2 |
| 15 | CUST-UPDATE-ADDR-LINE-3 | X(50) | Alpha | 50 | Working | N/A | Address line 3 |
| 15 | CUST-UPDATE-ADDR-STATE-CD | X(02) | Alpha | 2 | Working | N/A | State code |
| 15 | CUST-UPDATE-ADDR-COUNTRY-CD | X(03) | Alpha | 3 | Working | N/A | Country code |
| 15 | CUST-UPDATE-ADDR-ZIP | X(10) | Alpha | 10 | Working | N/A | ZIP code |
| 15 | CUST-UPDATE-PHONE-NUM-1 | X(15) | Alpha | 15 | Working | N/A | Phone number 1 |
| 15 | CUST-UPDATE-PHONE-NUM-2 | X(15) | Alpha | 15 | Working | N/A | Phone number 2 |
| 15 | CUST-UPDATE-SSN | 9(09) | Numeric | 9 | Working | N/A | Social Security Number |
| 15 | CUST-UPDATE-GOVT-ISSUED-ID | X(20) | Alpha | 20 | Working | N/A | Government issued ID |
| 15 | CUST-UPDATE-DOB-YYYY-MM-DD | X(10) | Alpha | 10 | Working | N/A | Date of birth |
| 15 | CUST-UPDATE-EFT-ACCOUNT-ID | X(10) | Alpha | 10 | Working | N/A | EFT account ID |
| 15 | CUST-UPDATE-PRI-CARD-IND | X(01) | Alpha | 1 | Working | N/A | Primary card indicator |
| 15 | CUST-UPDATE-FICO-CREDIT-SCORE | 9(03) | Numeric | 3 | Working | N/A | FICO credit score |

#### Message Construction Variables
| Level | Variable Name | Picture | Type | Length | Usage | Value | Description |
|-------|---------------|---------|------|--------|-------|-------|-------------|
| 05 | WS-LONG-MSG | X(500) | Alpha | 500 | Working | N/A | Long message field |
| 05 | WS-INFO-MSG | X(40) | Alpha | 40 | Working | N/A | Information message |
| 05 | WS-RETURN-MSG | X(75) | Alpha | 75 | Working | N/A | Return message |

#### Program Literals and Constants
| Level | Variable Name | Picture | Type | Length | Usage | Value | Description |
|-------|---------------|---------|------|--------|-------|-------|-------------|
| 01 | WS-LITERALS | Group | Group | 200+ | Working | N/A | Program literals |
| 05 | LIT-THISPGM | X(8) | Alpha | 8 | Working | 'COACTUPC' | This program name |
| 05 | LIT-THISTRANID | X(4) | Alpha | 4 | Working | 'CAUP' | This transaction ID |
| 05 | LIT-THISMAPSET | X(8) | Alpha | 8 | Working | 'COACTUP ' | This mapset name |
| 05 | LIT-THISMAP | X(7) | Alpha | 7 | Working | 'CACTUPA' | This map name |
| 05 | LIT-CARDUPDATE-PGM | X(8) | Alpha | 8 | Working | 'COCRDUPC' | Card update program |
| 05 | LIT-CARDUPDATE-TRANID | X(4) | Alpha | 4 | Working | 'CCUP' | Card update transaction |
| 05 | LIT-CARDUPDATE-MAPSET | X(8) | Alpha | 8 | Working | 'COCRDUP ' | Card update mapset |
| 05 | LIT-CARDUPDATE-MAP | X(7) | Alpha | 7 | Working | 'CCRDUPA' | Card update map |
| 05 | LIT-CCLISTPGM | X(8) | Alpha | 8 | Working | 'COCRDLIC' | Card list program |
| 05 | LIT-CCLISTTRANID | X(4) | Alpha | 4 | Working | 'CCLI' | Card list transaction |
| 05 | LIT-CCLISTMAPSET | X(7) | Alpha | 7 | Working | 'COCRDLI' | Card list mapset |
| 05 | LIT-CCLISTMAP | X(7) | Alpha | 7 | Working | 'CCRDSLA' | Card list map |
| 05 | LIT-MENUPGM | X(8) | Alpha | 8 | Working | 'COMEN01C' | Menu program |
| 05 | LIT-MENUTRANID | X(4) | Alpha | 4 | Working | 'CM00' | Menu transaction |
| 05 | LIT-MENUMAPSET | X(7) | Alpha | 7 | Working | 'COMEN01' | Menu mapset |
| 05 | LIT-MENUMAP | X(7) | Alpha | 7 | Working | 'COMEN1A' | Menu map |
| 05 | LIT-CARDDTLPGM | X(8) | Alpha | 8 | Working | 'COCRDSLC' | Card detail program |
| 05 | LIT-CARDDTLTRANID | X(4) | Alpha | 4 | Working | 'CCDL' | Card detail transaction |
| 05 | LIT-CARDDTLMAPSET | X(7) | Alpha | 7 | Working | 'COCRDSL' | Card detail mapset |
| 05 | LIT-CARDDTLMAP | X(7) | Alpha | 7 | Working | 'CCRDSLA' | Card detail map |
| 05 | LIT-ACCTFILENAME | X(8) | Alpha | 8 | Working | 'ACCTDAT ' | Account file name |
| 05 | LIT-CUSTFILENAME | X(8) | Alpha | 8 | Working | 'CUSTDAT ' | Customer file name |
| 05 | LIT-CARDFILENAME | X(8) | Alpha | 8 | Working | 'CARDDAT ' | Card file name |
| 05 | LIT-CARDFILENAME-ACCT-PATH | X(8) | Alpha | 8 | Working | 'CARDAIX ' | Card account index |
| 05 | LIT-CARDXREFNAME-ACCT-PATH | X(8) | Alpha | 8 | Working | 'CXACAIX ' | Card xref account index |

#### Character Set Literals
| Level | Variable Name | Picture | Type | Length | Usage | Value | Description |
|-------|---------------|---------|------|--------|-------|-------|-------------|
| 05 | LIT-ALL-ALPHANUM-FROM-X | Group | Group | 62 | Working | N/A | Character set group |
| 10 | LIT-ALL-ALPHA-FROM-X | Group | Group | 52 | Working | N/A | Alpha character group |
| 15 | LIT-UPPER | X(26) | Alpha | 26 | Working | 'ABCD...XYZ' | Uppercase letters |
| 15 | LIT-LOWER | X(26) | Alpha | 26 | Working | 'abcd...xyz' | Lowercase letters |
| 10 | LIT-NUMBERS | X(10) | Alpha | 10 | Working | '0123456789' | Numeric characters |

#### INSPECT Statement Variables
| Level | Variable Name | Picture | Type | Length | Usage | Value | Description |
|-------|---------------|---------|------|--------|-------|-------|-------------|
| 01 | LIT-ALL-ALPHA-FROM | X(52) | Alpha | 52 | Working | SPACES | Alpha character source |
| 01 | LIT-ALL-ALPHANUM-FROM | X(62) | Alpha | 62 | Working | SPACES | Alphanum character source |
| 01 | LIT-ALL-NUM-FROM | X(10) | Alpha | 10 | Working | SPACES | Numeric character source |
| 77 | LIT-ALPHA-SPACES-TO | X(52) | Alpha | 52 | Working | SPACES | Alpha replacement target |
| 77 | LIT-ALPHANUM-SPACES-TO | X(62) | Alpha | 62 | Working | SPACES | Alphanum replacement target |
| 77 | LIT-NUM-SPACES-TO | X(10) | Alpha | 10 | Working | SPACES | Numeric replacement target |

### Copybook Variables by Source

#### From CSUTLDWY (Date Utilities)
| Level | Variable Name | Picture | Type | Length | Usage | Description |
|-------|---------------|---------|------|--------|-------|-------------|
| 10 | WS-EDIT-DATE-CCYYMMDD | Group | Group | 8 | Working | Date edit structure |
| 20 | WS-EDIT-DATE-CCYY | Group | Group | 4 | Working | Century and year |
| 25 | WS-EDIT-DATE-CC | X(2) | Alpha | 2 | Working | Century |
| 25 | WS-EDIT-DATE-CC-N | 9(2) REDEFINES | Numeric | 2 | Working | Century numeric |
| 25 | WS-EDIT-DATE-YY | X(2) | Alpha | 2 | Working | Year |
| 25 | WS-EDIT-DATE-YY-N | 9(2) REDEFINES | Numeric | 2 | Working | Year numeric |
| 20 | WS-EDIT-DATE-CCYY-N | 9(4) REDEFINES | Numeric | 4 | Working | Full year numeric |
| 20 | WS-EDIT-DATE-MM | X(2) | Alpha | 2 | Working | Month |
| 20 | WS-EDIT-DATE-MM-N | 9(2) REDEFINES | Numeric | 2 | Working | Month numeric |
| 20 | WS-EDIT-DATE-DD | X(2) | Alpha | 2 | Working | Day |
| 20 | WS-EDIT-DATE-DD-N | 9(2) REDEFINES | Numeric | 2 | Working | Day numeric |
| 10 | WS-EDIT-DATE-CCYYMMDD-N | 9(8) REDEFINES | Numeric | 8 | Working | Complete date numeric |
| 10 | WS-EDIT-DATE-BINARY | S9(9) BINARY | Binary | 4 | Working | Date binary format |
| 10 | WS-CURRENT-DATE | Group | Group | 12 | Working | Current date structure |
| 20 | WS-CURRENT-DATE-YYYYMMDD | X(8) | Alpha | 8 | Working | Current date alpha |
| 20 | WS-CURRENT-DATE-YYYYMMDD-N | 9(8) REDEFINES | Numeric | 8 | Working | Current date numeric |
| 20 | WS-CURRENT-DATE-BINARY | S9(9) BINARY | Binary | 4 | Working | Current date binary |
| 10 | WS-EDIT-DATE-FLGS | Group | Group | 3 | Working | Date validation flags |
| 20 | WS-EDIT-YEAR-FLG | X(01) | Alpha | 1 | Working | Year validation flag |
| 20 | WS-EDIT-MONTH | X(01) | Alpha | 1 | Working | Month validation flag |

#### From COCOM01Y (Communication Area)
| Level | Variable Name | Picture | Type | Length | Usage | Description |
|-------|---------------|---------|------|--------|-------|-------------|
| 01 | CARDDEMO-COMMAREA | Group | Group | 200+ | Working | Application communication area |
| 05 | CDEMO-GENERAL-INFO | Group | Group | 50+ | Working | General information |
| 10 | CDEMO-FROM-TRANID | X(04) | Alpha | 4 | Working | Calling transaction ID |
| 10 | CDEMO-FROM-PROGRAM | X(08) | Alpha | 8 | Working | Calling program name |
| 10 | CDEMO-TO-TRANID | X(04) | Alpha | 4 | Working | Target transaction ID |
| 10 | CDEMO-TO-PROGRAM | X(08) | Alpha | 8 | Working | Target program name |
| 10 | CDEMO-USER-ID | X(08) | Alpha | 8 | Working | User ID |
| 10 | CDEMO-USER-TYPE | X(01) | Alpha | 1 | Working | User type |
| 10 | CDEMO-PGM-CONTEXT | 9(01) | Numeric | 1 | Working | Program context |

#### From COACTUP (BMS Screen Map)
| Field Base Name | Input Field | Output Field | Length Field | Flag Field | Picture | Usage |
|-----------------|-------------|--------------|--------------|------------|---------|-------|
| ACCTSID | ACCTSIDI | ACCTSIDO | ACCTSIDL | ACCTSIDF | X(11) | Account ID input/display |
| ACSTTUS | ACSTTUSI | ACSTTUSO | ACSTTUSL | ACSTTUSF | X(1) | Account status |
| ACRDLIM | ACRDLIMI | ACRDLIMO | ACRDLIML | ACRDLIMF | X(15) | Credit limit |
| ACSHLIM | ACSHLIMI | ACSHLIMO | ACSHLIML | ACSHLIMF | X(15) | Cash credit limit |
| ACURBAL | ACURBALI | ACURBALO | ACURBALL | ACURBALF | X(15) | Current balance |
| ACRCYCR | ACRCYCRI | ACRCYCRO | ACRCYCRL | ACRCYCRF | X(15) | Current cycle credit |
| ACRCYDB | ACRCYDBI | ACRCYDBO | ACRCYDBL | ACRCYDBF | X(15) | Current cycle debit |
| OPNYEAR | OPNYEARI | OPNYEARO | OPNYEARL | OPNYEARF | X(4) | Open year |
| OPNMON | OPNMONI | OPNMONO | OPNMONL | OPNMONF | X(2) | Open month |
| OPNDAY | OPNDAYI | OPNDAYO | OPNDAYL | OPNDAYF | X(2) | Open day |
| EXPYEAR | EXPYEARI | EXPYEARO | EXPYEARL | EXPYEARF | X(4) | Expiry year |
| EXPMON | EXPMONI | EXPMONO | EXPMONL | EXPMONF | X(2) | Expiry month |
| EXPDAY | EXPDAYI | EXPDAYO | EXPDAYL | EXPDAYF | X(2) | Expiry day |
| RISYEAR | RISYEARI | RISYEARO | RISYEARL | RISYEARF | X(4) | Reissue year |
| RISMON | RISMONI | RISMONO | RISMONL | RISMONF | X(2) | Reissue month |
| RISDAY | RISDAYI | RISDAYO | RISDAYL | RISDAYF | X(2) | Reissue day |
| AADDGRP | AADDGRPI | AADDGRPO | AADDGRPL | AADDGRPF | X(10) | Account group |
| ACSTNUM | ACSTNUMI | ACSTNUMO | ACSTNUML | ACSTNUMF | X(9) | Customer number |
| ACTSSN1 | ACTSSN1I | ACTSSN1O | ACTSSN1L | ACTSSN1F | X(3) | SSN part 1 |
| ACTSSN2 | ACTSSN2I | ACTSSN2O | ACTSSN2L | ACTSSN2F | X(2) | SSN part 2 |
| ACTSSN3 | ACTSSN3I | ACTSSN3O | ACTSSN3L | ACTSSN3F | X(4) | SSN part 3 |
| ACSTFCO | ACSTFCOI | ACSTFCOO | ACSTFCOL | ACSTFCOF | X(3) | FICO score |
| DOBYEAR | DOBYEARI | DOBYEARO | DOBYEARL | DOBYEARF | X(4) | Birth year |
| DOBMON | DOBMONI | DOBMONO | DOBMONL | DOBMONF | X(2) | Birth month |
| DOBDAY | DOBDAYI | DOBDAYO | DOBDAYL | DOBDAYF | X(2) | Birth day |
| ACSFNAM | ACSFNAMI | ACSFNAMO | ACSFNAML | ACSFNAMF | X(25) | First name |
| ACSMNAM | ACSMNAMI | ACSMNAMO | ACSMNAML | ACSMNMF | X(25) | Middle name |
| ACSLNAM | ACSLNAMI | ACSLNAMO | ACSLNAML | ACSLNAMF | X(25) | Last name |
| ACSADL1 | ACSADL1I | ACSADL1O | ACSADL1L | ACSADL1F | X(50) | Address line 1 |
| ACSADL2 | ACSADL2I | ACSADL2O | ACSADL2L | ACSADL2F | X(50) | Address line 2 |
| ACSCITY | ACSCITYI | ACSCITYO | ACSCITYL | ACSCITYF | X(50) | City |
| ACSSTTE | ACSSTTEI | ACSSTTEO | ACSSTTEL | ACSSTTEF | X(2) | State |
| ACSZIPC | ACSZIPCI | ACSZIPCO | ACSZIPCL | ACSZIPCF | X(10) | ZIP code |
| ACSCTRY | ACSCTRYI | ACSCTRYO | ACSCTRYL | ACSCTYF | X(3) | Country |
| ACSPH1A | ACSPH1AI | ACSPH1AO | ACSPH1AL | ACSPH1AF | X(3) | Phone 1 area |
| ACSPH1B | ACSPH1BI | ACSPH1BO | ACSPH1BL | ACSPH1BF | X(3) | Phone 1 exchange |
| ACSPH1C | ACSPH1CI | ACSPH1CO | ACSPH1CL | ACSPH1CF | X(4) | Phone 1 line |
| ACSPH2A | ACSPH2AI | ACSPH2AO | ACSPH2AL | ACSPH2AF | X(3) | Phone 2 area |
| ACSPH2B | ACSPH2BI | ACSPH2BO | ACSPH2BL | ACSPH2BF | X(3) | Phone 2 exchange |
| ACSPH2C | ACSPH2CI | ACSPH2CO | ACSPH2CL | ACSPH2CF | X(4) | Phone 2 line |
| ACSGOVT | ACSGOVTI | ACSGOVTO | ACSGOVTL | ACSGOVTF | X(20) | Government ID |
| ACSEFTC | ACSEFTCI | ACSEFTCO | ACSEFTCL | ACSEFTCF | X(10) | EFT account |
| ACSPFLG | ACSPFLGI | ACSPFLGO | ACSPFLGL | ACSPFLGF | X(1) | Primary flag |

### Linkage Section
| Level | Variable Name | Picture | Type | Length | Usage | Description |
|-------|---------------|---------|------|--------|-------|-------------|
| 01 | DFHCOMMAREA | Group | Group | Variable | Linkage | CICS communication area |
| 05 | FILLER | X(1) OCCURS | Alpha | Variable | Linkage | Dynamic communication data |

### Program-Specific Communication Area
| Level | Variable Name | Picture | Type | Length | Usage | Description |
|-------|---------------|---------|------|--------|-------|-------------|
| 01 | WS-THIS-PROGCOMMAREA | Group | Group | 1500+ | Working | Program communication area |
| 05 | ACCT-UPDATE-SCREEN-DATA | Group | Group | 1500+ | Working | Account update screen data |
| 10 | ACUP-CHANGE-ACTION | X(1) | Alpha | 1 | Working | Change action flag |
| 05 | ACUP-OLD-DETAILS | Group | Group | 750+ | Working | Original account/customer data |
| 10 | ACUP-OLD-ACCT-DATA | Group | Group | 400+ | Working | Original account data |
| 10 | ACUP-OLD-CUST-DATA | Group | Group | 350+ | Working | Original customer data |
| 05 | ACUP-NEW-DETAILS | Group | Group | 750+ | Working | Updated account/customer data |
| 10 | ACUP-NEW-ACCT-DATA | Group | Group | 400+ | Working | Updated account data |
| 10 | ACUP-NEW-CUST-DATA | Group | Group | 350+ | Working | Updated customer data |
| 01 | WS-COMMAREA | X(2000) | Alpha | 2000 | Working | Working communication area |

## Data Type Distribution Chart
```
Alphanumeric (PIC X): 263 (83%)
Numeric Display (PIC 9): 44 (14%)
Packed Decimal (COMP-3): 4 (1%)
Binary (COMP): 11 (3%)
Group Items: 45 (14%)
Condition Names (88): 83 (26%)
```

## Variable Hierarchy Map
```
WS-MISC-STORAGE
├── WS-CICS-PROCESSNG-VARS
│   ├── WS-RESP-CD (PIC S9(09) COMP)
│   ├── WS-REAS-CD (PIC S9(09) COMP)
│   ├── WS-TRANID (PIC X(4))
│   └── WS-UCTRANS (PIC X(4))
├── WS-GENERIC-EDITS
│   ├── WS-EDIT-VARIABLE-NAME (PIC X(25))
│   ├── WS-EDIT-SIGNED-NUMBER-9V2-X (PIC X(15))
│   ├── WS-FLG-SIGNED-NUMBER-EDIT (PIC X(1))
│   │   ├── 88 FLG-SIGNED-NUMBER-ISVALID (VALUE LOW-VALUES)
│   │   ├── 88 FLG-SIGNED-NUMBER-NOT-OK (VALUE '0')
│   │   └── 88 FLG-SIGNED-NUMBER-BLANK (VALUE 'B')
│   ├── WS-EDIT-ALPHANUM-ONLY (PIC X(256))
│   ├── WS-EDIT-ALPHANUM-LENGTH (PIC S9(4) COMP-3)
│   ├── WS-EDIT-ALPHA-ONLY-FLAGS (PIC X(1))
│   │   ├── 88 FLG-ALPHA-ISVALID (VALUE LOW-VALUES)
│   │   ├── 88 FLG-ALPHA-NOT-OK (VALUE '0')
│   │   └── 88 FLG-ALPHA-BLANK (VALUE 'B')
│   ├── WS-EDIT-ALPHANUM-ONLY-FLAGS (PIC X(1))
│   │   ├── 88 FLG-ALPHNANUM-ISVALID (VALUE LOW-VALUES)
│   │   ├── 88 FLG-ALPHNANUM-NOT-OK (VALUE '0')
│   │   └── 88 FLG-ALPHNANUM-BLANK (VALUE 'B')
│   ├── WS-EDIT-MANDATORY-FLAGS (PIC X(1))
│   │   ├── 88 FLG-MANDATORY-ISVALID (VALUE LOW-VALUES)
│   │   ├── 88 FLG-MANDATORY-NOT-OK (VALUE '0')
│   │   └── 88 FLG-MANDATORY-BLANK (VALUE 'B')
│   ├── WS-EDIT-YES-NO (PIC X(1))
│   │   ├── 88 FLG-YES-NO-ISVALID (VALUES 'Y', 'N')
│   │   ├── 88 FLG-YES-NO-NOT-OK (VALUE '0')
│   │   └── 88 FLG-YES-NO-BLANK (VALUE 'B')
│   ├── WS-EDIT-US-PHONE-NUM (PIC X(15))
│   │   └── WS-EDIT-US-PHONE-NUM-X (REDEFINES)
│   │       ├── WS-EDIT-US-PHONE-NUMA (PIC X(3))
│   │       ├── WS-EDIT-US-PHONE-NUMB (PIC X(3))
│   │       └── WS-EDIT-US-PHONE-NUMC (PIC X(4))
│   └── WS-EDIT-US-SSN
│       ├── WS-EDIT-US-SSN-PART1 (PIC X(3))
│       ├── WS-EDIT-US-SSN-PART2 (PIC X(2))
│       └── WS-EDIT-US-SSN-PART3 (PIC X(4))
├── WS-CALCULATION-VARS
│   ├── WS-DIV-BY (PIC S9(4) COMP-3)
│   ├── WS-DIVIDEND (PIC S9(4) COMP-3)
│   ├── WS-REMAINDER (PIC S9(4) COMP-3)
│   └── WS-CURR-DATE (PIC X(21))
├── WS-DATACHANGED-FLAG (PIC X(1))
│   ├── 88 NO-CHANGES-FOUND (VALUE '0')
│   └── 88 CHANGE-HAS-OCCURRED (VALUE '1')
├── WS-INPUT-FLAG (PIC X(1))
│   ├── 88 INPUT-OK (VALUE '0')
│   ├── 88 INPUT-ERROR (VALUE '1')
│   └── 88 INPUT-PENDING (VALUE LOW-VALUES)
├── WS-RETURN-FLAG (PIC X(1))
│   ├── 88 WS-RETURN-FLAG-OFF (VALUE LOW-VALUES)
│   └── 88 WS-RETURN-FLAG-ON (VALUE '1')
├── WS-PFK-FLAG (PIC X(1))
│   ├── 88 PFK-VALID (VALUE '0')
│   └── 88 PFK-INVALID (VALUE '1')
├── WS-EDIT-ACCT-FLAG (PIC X(1))
│   ├── 88 FLG-ACCTFILTER-ISVALID (VALUE '1')
│   ├── 88 FLG-ACCTFILTER-NOT-OK (VALUE '0')
│   └── 88 FLG-ACCTFILTER-BLANK (VALUE ' ')
└── WS-EDIT-CUST-FLAG (PIC X(1))
    ├── 88 FLG-CUSTFILTER-ISVALID (VALUE '1')
    ├── 88 FLG-CUSTFILTER-NOT-OK (VALUE '0')
    └── 88 FLG-CUSTFILTER-BLANK (VALUE ' ')

WS-LITERALS
├── LIT-THISPGM (PIC X(8)) = 'COACTUPC'
├── LIT-THISTRANID (PIC X(4)) = 'CAUP'
├── LIT-THISMAPSET (PIC X(8)) = 'COACTUP '
├── LIT-THISMAP (PIC X(7)) = 'CACTUPA'
├── LIT-ACCTFILENAME (PIC X(8)) = 'ACCTDAT '
├── LIT-CUSTFILENAME (PIC X(8)) = 'CUSTDAT '
└── LIT-CARDXREFNAME-ACCT-PATH (PIC X(8)) = 'CXACAIX '

CARDDEMO-COMMAREA (from COCOM01Y)
├── CDEMO-GENERAL-INFO
│   ├── CDEMO-FROM-TRANID (PIC X(04))
│   ├── CDEMO-FROM-PROGRAM (PIC X(08))
│   ├── CDEMO-TO-TRANID (PIC X(04))
│   ├── CDEMO-TO-PROGRAM (PIC X(08))
│   ├── CDEMO-USER-ID (PIC X(08))
│   ├── CDEMO-USER-TYPE (PIC X(01))
│   │   ├── 88 CDEMO-USRTYP-ADMIN (VALUE 'A')
│   │   └── 88 CDEMO-USRTYP-USER (VALUE 'U')
│   └── CDEMO-PGM-CONTEXT (PIC 9(01))
│       └── 88 CDEMO-PGM-ENTER (VALUE 0)

WS-THIS-PROGCOMMAREA
├── ACCT-UPDATE-SCREEN-DATA
│   ├── ACUP-CHANGE-ACTION (PIC X(1))
│   │   ├── 88 ACUP-DETAILS-NOT-FETCHED (VALUES LOW-VALUES, SPACES)
│   │   ├── 88 ACUP-SHOW-DETAILS (VALUE 'S')
│   │   ├── 88 ACUP-CHANGES-MADE (VALUES 'E', 'N', 'C', 'L', 'F')
│   │   ├── 88 ACUP-CHANGES-NOT-OK (VALUE 'E')
│   │   ├── 88 ACUP-CHANGES-OK-NOT-CONFIRMED (VALUE 'N')
│   │   ├── 88 ACUP-CHANGES-OKAYED-AND-DONE (VALUE 'C')
│   │   ├── 88 ACUP-CHANGES-FAILED (VALUES 'L', 'F')
│   │   ├── 88 ACUP-CHANGES-OKAYED-LOCK-ERROR (VALUE 'L')
│   │   └── 88 ACUP-CHANGES-OKAYED-BUT-FAILED (VALUE 'F')
│   ├── ACUP-OLD-DETAILS
│   │   ├── ACUP-OLD-ACCT-DATA
│   │   └── ACUP-OLD-CUST-DATA
│   └── ACUP-NEW-DETAILS
│       ├── ACUP-NEW-ACCT-DATA
│       └── ACUP-NEW-CUST-DATA
```

## Interface Field Analysis (CICS BMS Maps)

### COACTUP Map Fields
The program uses the COACTUP mapset with map CACTUPA for account update screen interaction. Each field follows the standard BMS naming convention with suffixes:
- I = Input field
- O = Output field  
- L = Length field
- F = Flag field

Key interface fields include account identification, financial data, dates, customer information, and address details.

## Condition Names (88-Level) Summary
| Condition Name | Parent Variable | Value | Usage Context |
|----------------|-----------------|-------|---------------|
| FLG-SIGNED-NUMBER-ISVALID | WS-FLG-SIGNED-NUMBER-EDIT | LOW-VALUES | Valid signed number |
| FLG-SIGNED-NUMBER-NOT-OK | WS-FLG-SIGNED-NUMBER-EDIT | '0' | Invalid signed number |
| FLG-SIGNED-NUMBER-BLANK | WS-FLG-SIGNED-NUMBER-EDIT | 'B' | Blank signed number |
| FLG-ALPHA-ISVALID | WS-EDIT-ALPHA-ONLY-FLAGS | LOW-VALUES | Valid alphabetic field |
| FLG-ALPHA-NOT-OK | WS-EDIT-ALPHA-ONLY-FLAGS | '0' | Invalid alphabetic field |
| FLG-ALPHA-BLANK | WS-EDIT-ALPHA-ONLY-FLAGS | 'B' | Blank alphabetic field |
| FLG-ALPHNANUM-ISVALID | WS-EDIT-ALPHANUM-ONLY-FLAGS | LOW-VALUES | Valid alphanumeric field |
| FLG-ALPHNANUM-NOT-OK | WS-EDIT-ALPHANUM-ONLY-FLAGS | '0' | Invalid alphanumeric field |
| FLG-ALPHNANUM-BLANK | WS-EDIT-ALPHANUM-ONLY-FLAGS | 'B' | Blank alphanumeric field |
| FLG-MANDATORY-ISVALID | WS-EDIT-MANDATORY-FLAGS | LOW-VALUES | Valid mandatory field |
| FLG-MANDATORY-NOT-OK | WS-EDIT-MANDATORY-FLAGS | '0' | Invalid mandatory field |
| FLG-MANDATORY-BLANK | WS-EDIT-MANDATORY-FLAGS | 'B' | Blank mandatory field |
| FLG-YES-NO-ISVALID | WS-EDIT-YES-NO | 'Y', 'N' | Valid Y/N response |
| FLG-YES-NO-NOT-OK | WS-EDIT-YES-NO | '0' | Invalid Y/N response |
| FLG-YES-NO-BLANK | WS-EDIT-YES-NO | 'B' | Blank Y/N response |
| WS-EDIT-US-PHONE-IS-INVALID | WS-EDIT-US-PHONE-NUM-FLGS | '000' | Invalid phone number |
| WS-EDIT-US-PHONE-IS-VALID | WS-EDIT-US-PHONE-NUM-FLGS | LOW-VALUES | Valid phone number |
| INVALID-SSN-PART1 | WS-EDIT-US-SSN-PART1-N | 0, 666, 900-999 | Invalid SSN first part |
| WS-EDIT-US-SSN-IS-INVALID | WS-EDIT-US-SSN-FLGS | '000' | Invalid SSN |
| WS-EDIT-US-SSN-IS-VALID | WS-EDIT-US-SSN-FLGS | LOW-VALUES | Valid SSN |
| NO-CHANGES-FOUND | WS-DATACHANGED-FLAG | '0' | No data changes detected |
| CHANGE-HAS-OCCURRED | WS-DATACHANGED-FLAG | '1' | Data changes detected |
| INPUT-OK | WS-INPUT-FLAG | '0' | Input validation passed |
| INPUT-ERROR | WS-INPUT-FLAG | '1' | Input validation failed |
| INPUT-PENDING | WS-INPUT-FLAG | LOW-VALUES | Input validation pending |
| WS-RETURN-FLAG-OFF | WS-RETURN-FLAG | LOW-VALUES | Return flag off |
| WS-RETURN-FLAG-ON | WS-RETURN-FLAG | '1' | Return flag on |
| PFK-VALID | WS-PFK-FLAG | '0' | Valid PF key pressed |
| PFK-INVALID | WS-PFK-FLAG | '1' | Invalid PF key pressed |
| FLG-ACCTFILTER-ISVALID | WS-EDIT-ACCT-FLAG | '1' | Valid account filter |
| FLG-ACCTFILTER-NOT-OK | WS-EDIT-ACCT-FLAG | '0' | Invalid account filter |
| FLG-ACCTFILTER-BLANK | WS-EDIT-ACCT-FLAG | ' ' | Blank account filter |
| FLG-CUSTFILTER-ISVALID | WS-EDIT-CUST-FLAG | '1' | Valid customer filter |
| FLG-CUSTFILTER-NOT-OK | WS-EDIT-CUST-FLAG | '0' | Invalid customer filter |
| FLG-CUSTFILTER-BLANK | WS-EDIT-CUST-FLAG | ' ' | Blank customer filter |
| FLG-ACCT-STATUS-ISVALID | WS-EDIT-ACCT-STATUS | 'Y', 'N' | Valid account status |
| FLG-ACCT-STATUS-NOT-OK | WS-EDIT-ACCT-STATUS | '0' | Invalid account status |
| FLG-ACCT-STATUS-BLANK | WS-EDIT-ACCT-STATUS | 'B' | Blank account status |
| FOUND-ACCT-IN-MASTER | WS-ACCOUNT-MASTER-READ-FLAG | '1' | Account found in master |
| FOUND-CUST-IN-MASTER | WS-CUST-MASTER-READ-FLAG | '1' | Customer found in master |
| FLG-PRI-CARDHOLDER-ISVALID | WS-EDIT-PRI-CARDHOLDER | 'Y', 'N' | Valid primary cardholder |
| FLG-PRI-CARDHOLDER-NOT-OK | WS-EDIT-PRI-CARDHOLDER | '0' | Invalid primary cardholder |
| FLG-PRI-CARDHOLDER-BLANK | WS-EDIT-PRI-CARDHOLDER | 'B' | Blank primary cardholder |
| FICO-RANGE-IS-VALID | ACUP-NEW-CUST-FICO-SCORE | 300-850 | Valid FICO score range |

*Note: Total of 83 condition names (88-level) in program, plus 25 additional from copybooks*

## Cross-Reference Table
| Variable Name | Defined In | Used In | External Reference | System Usage |
|---------------|------------|---------|-------------------|--------------|
| WS-RESP-CD | COACTUPC | CICS operations | DFHRESP | CICS response code |
| WS-REAS-CD | COACTUPC | CICS operations | DFHRESP2 | CICS reason code |
| WS-TRANID | COACTUPC | Transaction control | EIBTRNID | Current transaction ID |
| CARDDEMO-COMMAREA | COCOM01Y | Inter-program | DFHCOMMAREA | Program communication |
| CACTUPAI/CACTUPAO | COACTUP | Screen I/O | BMS mapset | Screen interface |
| ACCOUNT-RECORD | CVACT01Y | File I/O | ACCTDAT | Account master file |
| CUSTOMER-RECORD | CVCUS01Y | File I/O | CUSTDAT | Customer master file |
| CARD-XREF-RECORD | CVACT03Y | File I/O | CXACAIX | Card cross-reference |
| WS-EDIT-DATE-CCYYMMDD | CSUTLDWY | Date validation | Date routines | Date processing |
| CC-WORK-AREA | CVCRD01Y | Card processing | Card utilities | Card data handling |
