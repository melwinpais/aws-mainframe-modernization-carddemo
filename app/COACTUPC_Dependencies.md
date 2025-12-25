# COACTUPC Dependency Analysis

## Program Overview
**Program Name**: COACTUPC  
**Function**: Accept and process ACCOUNT UPDATE  
**Layer**: Business logic  
**Transaction ID**: CAUP  

## Dependency Summary Table
| Dependency Type | Count | Critical Level | Examples |
|----------------|-------|----------------|----------|
| Copybooks | 18 | High | CSUTLDWY, CVCRD01Y, COACTUP |
| Programs Called | 0 | N/A | None (uses XCTL only) |
| VSAM Files | 3 | High | ACCTDAT, CUSTDAT, CARDAIX |
| DB2 Tables | 0 | N/A | None |
| BMS Maps | 1 | High | COACTUP/CACTUPA |
| CICS Resources | 4 | Medium | CAUP, ACCTDAT, CUSTDAT, CXACAIX |
| External Systems | 0 | Low | None |

## Detailed Dependency Matrix
| Source Program | Depends On | Dependency Type | File Location | Usage Context |
|---------------|------------|-----------------|---------------|---------------|
| COACTUPC | CSUTLDWY | Copybook | cpy/CSUTLDWY.cpy | Date editing utilities |
| COACTUPC | CVCRD01Y | Copybook | cpy/CVCRD01Y.cpy | Card record layout |
| COACTUPC | CSLKPCDY | Copybook | cpy/CSLKPCDY.cpy | Phone area code lookups |
| COACTUPC | DFHBMSCA | IBM Copybook | System | BMS attribute constants |
| COACTUPC | DFHAID | IBM Copybook | System | AID key definitions |
| COACTUPC | COTTL01Y | Copybook | cpy/COTTL01Y.cpy | Screen titles |
| COACTUPC | COACTUP | BMS Copybook | cpy-bms/COACTUP.CPY | Account update screen layout |
| COACTUPC | CSDAT01Y | Copybook | cpy/CSDAT01Y.cpy | Current date utilities |
| COACTUPC | CSMSG01Y | Copybook | cpy/CSMSG01Y.cpy | Common messages |
| COACTUPC | CSMSG02Y | Copybook | cpy/CSMSG02Y.cpy | Abend variables |
| COACTUPC | CSUSR01Y | Copybook | cpy/CSUSR01Y.cpy | Signed on user data |
| COACTUPC | CVACT01Y | Copybook | cpy/CVACT01Y.cpy | Account record layout |
| COACTUPC | CVACT03Y | Copybook | cpy/CVACT03Y.cpy | Card cross-reference layout |
| COACTUPC | CVCUS01Y | Copybook | cpy/CVCUS01Y.cpy | Customer record layout |
| COACTUPC | COCOM01Y | Copybook | cpy/COCOM01Y.cpy | Application commarea |
| COACTUPC | CSSETATY | Copybook | cpy/CSSETATY.cpy | Screen attribute setting |
| COACTUPC | CSSTRPFY | Copybook | cpy/CSSTRPFY.cpy | PF key handling |
| COACTUPC | CSUTLDPY | Copybook | cpy/CSUTLDPY.cpy | Date validation routines |
| COACTUPC | ACCTDAT | VSAM File | data/ | Account master file |
| COACTUPC | CUSTDAT | VSAM File | data/ | Customer master file |
| COACTUPC | CXACAIX | VSAM Index | data/ | Card cross-reference by account |

## Dependency Flow Diagram
```
COACTUPC (Account Update Processing)
├── Input Dependencies
│   ├── CSUTLDWY.cpy (Date utilities)
│   ├── COACTUP.CPY (Screen definition)
│   ├── COCOM01Y.cpy (Communication area)
│   └── DFHBMSCA/DFHAID (IBM BMS/AID constants)
├── Data Access
│   ├── ACCTDAT (Account master VSAM file)
│   ├── CUSTDAT (Customer master VSAM file)
│   └── CXACAIX (Card cross-reference index)
├── Data Structures
│   ├── CVACT01Y.cpy (Account record layout)
│   ├── CVCUS01Y.cpy (Customer record layout)
│   ├── CVACT03Y.cpy (Card xref layout)
│   └── CVCRD01Y.cpy (Card record layout)
├── Validation & Utilities
│   ├── CSLKPCDY.cpy (Phone area code validation)
│   ├── CSUTLDPY.cpy (Date validation routines)
│   └── CSSETATY.cpy (Screen attribute utilities)
└── Program Flow
    ├── COMEN01C (Main menu - XCTL target)
    ├── COCRDUPC (Card update program - potential XCTL)
    └── COCRDLIC (Card list program - potential XCTL)
```

## Runtime Dependencies
- **Transaction**: CAUP (defined in CICS resource definitions)
- **CICS Program**: COACTUPC program definition
- **Screen Handling**: CACTUPA map from COACTUP mapset
- **File Access**: READ/UPDATE access to ACCTDAT, CUSTDAT, CXACAIX
- **Database**: No DB2 dependencies

## CICS Commands Used
- **EXEC CICS HANDLE ABEND** - Abend handling
- **EXEC CICS RECEIVE MAP** - Screen input processing
- **EXEC CICS SEND MAP** - Screen output
- **EXEC CICS READ** - File record retrieval
- **EXEC CICS READ UPDATE** - Record locking for update
- **EXEC CICS REWRITE** - Record update
- **EXEC CICS XCTL** - Program transfer
- **EXEC CICS RETURN** - Program termination
- **EXEC CICS SYNCPOINT** - Transaction commit
- **EXEC CICS SYNCPOINT ROLLBACK** - Transaction rollback
- **EXEC CICS SEND** - Error message display
- **EXEC CICS ABEND** - Abnormal termination

## File Operations
### ACCTDAT (Account Master)
- **Access Type**: READ, READ UPDATE, REWRITE
- **Key**: Account ID (11 digits)
- **Usage**: Account data retrieval and update

### CUSTDAT (Customer Master)  
- **Access Type**: READ, READ UPDATE, REWRITE
- **Key**: Customer ID (9 digits)
- **Usage**: Customer data retrieval and update

### CXACAIX (Card Cross-Reference Index)
- **Access Type**: READ
- **Key**: Account ID
- **Usage**: Lookup customer ID and card number by account

## Program Transfer Targets
- **COMEN01C** (CM00) - Main menu program
- **COCRDUPC** (CCUP) - Card update program  
- **COCRDLIC** (CCLI) - Card list program
- **COCRDSLC** (CCDL) - Card detail program

## Critical Dependencies
1. **COACTUP.CPY** - Screen layout (compilation failure without)
2. **CVACT01Y.cpy** - Account record structure (runtime failure)
3. **CVCUS01Y.cpy** - Customer record structure (runtime failure)
4. **ACCTDAT** - Account master file (runtime failure)
5. **CUSTDAT** - Customer master file (runtime failure)
6. **CXACAIX** - Card cross-reference index (runtime failure)

## Validation Dependencies
- **CSLKPCDY.cpy** - North American phone area code validation
- **CSUTLDPY.cpy** - Date validation and formatting
- **CSUTLDWY.cpy** - Date editing utilities

## Screen Dependencies
- **COACTUP** mapset containing **CACTUPA** map
- **DFHBMSCA** - BMS attribute constants
- **DFHAID** - Attention identifier constants

## Error Handling Dependencies
- **CSMSG01Y.cpy** - Common error messages
- **CSMSG02Y.cpy** - Abend handling variables
- **CSSTRPFY.cpy** - PF key processing

## Notes
- Program performs account and customer data updates in a single transaction
- Uses optimistic locking by comparing original values before update
- Implements comprehensive field validation for US-specific data formats
- No SQL or DB2 dependencies - pure VSAM file processing
- Supports rollback on update failures
