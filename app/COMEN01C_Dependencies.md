# COMEN01C Dependency Analysis

**Analysis Date**: 2025-12-24  
**Program Name**: COMEN01C  
**Program Type**: CICS COBOL Program  
**Function**: Main Menu for Regular Users  

## Dependency Summary Table

| Dependency Type | Count | Critical Level | Examples |
|----------------|-------|----------------|----------|
| Copybooks | 8 | High | COCOM01Y, COMEN02Y, COMEN01 |
| Programs Called | 12 | High | COSGN00C, COACTVWC, COACTUPC, COCRDLIC |
| VSAM Files | 0 | N/A | None (USRSEC defined but not used) |
| DB2 Tables | 0 | N/A | None |
| BMS Maps | 1 | High | COMEN1A from COMEN01 mapset |
| CICS Resources | 2 | Medium | Transaction CM00, Program COMEN01C |
| External Systems | 0 | N/A | None |

## Detailed Dependency Matrix

| Source Program | Depends On | Dependency Type | File Location | Usage Context |
|---------------|------------|-----------------|---------------|---------------|
| COMEN01C | COCOM01Y | Copybook | cpy/COCOM01Y.cpy | Communication area structure |
| COMEN01C | COMEN02Y | Copybook | cpy/COMEN02Y.cpy | Menu options data structure |
| COMEN01C | COMEN01 | BMS Copybook | cpy-bms/COMEN01.CPY | Screen map definitions |
| COMEN01C | COTTL01Y | Copybook | cpy/COTTL01Y.cpy | Screen title constants |
| COMEN01C | CSDAT01Y | Copybook | cpy/CSDAT01Y.cpy | Date/time formatting structures |
| COMEN01C | CSMSG01Y | Copybook | cpy/CSMSG01Y.cpy | Common message constants |
| COMEN01C | CSUSR01Y | Copybook | cpy/CSUSR01Y.cpy | User security data structure |
| COMEN01C | DFHAID | System Copybook | System | CICS attention identifier constants |
| COMEN01C | DFHBMSCA | System Copybook | System | BMS attribute constants |
| COMEN01C | COSGN00C | Program | cbl/COSGN00C.cbl | Sign-on screen program |
| COMEN01C | COACTVWC | Program | cbl/COACTVWC.cbl | Account view program |
| COMEN01C | COACTUPC | Program | cbl/COACTUPC.cbl | Account update program |
| COMEN01C | COCRDLIC | Program | cbl/COCRDLIC.cbl | Credit card list program |
| COMEN01C | COCRDSLC | Program | cbl/COCRDSLC.cbl | Credit card view program |
| COMEN01C | COCRDUPC | Program | cbl/COCRDUPC.cbl | Credit card update program |
| COMEN01C | COTRN00C | Program | cbl/COTRN00C.cbl | Transaction list program |
| COMEN01C | COTRN01C | Program | cbl/COTRN01C.cbl | Transaction view program |
| COMEN01C | COTRN02C | Program | cbl/COTRN02C.cbl | Transaction add program |
| COMEN01C | CORPT00C | Program | cbl/CORPT00C.cbl | Transaction reports program |
| COMEN01C | COBIL00C | Program | cbl/COBIL00C.cbl | Bill payment program |
| COMEN01C | COPAUS0C | Program | app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | Pending authorization view program |

## Dependency Flow Diagram

```
COMEN01C (Main Menu for Regular Users)
├── Input Dependencies
│   ├── COCOM01Y.cpy (Communication area structure)
│   ├── COMEN02Y.cpy (Menu options configuration)
│   ├── COMEN01.CPY (BMS screen map)
│   ├── COTTL01Y.cpy (Screen titles)
│   ├── CSDAT01Y.cpy (Date/time formatting)
│   ├── CSMSG01Y.cpy (Common messages)
│   ├── CSUSR01Y.cpy (User security data)
│   ├── DFHAID (CICS attention identifiers)
│   └── DFHBMSCA (BMS attribute constants)
├── Data Access
│   └── None (USRSEC file defined but not accessed)
└── Program Flow
    ├── COSGN00C (Sign-on screen - PF3/Exit)
    ├── COACTVWC (Account View - Option 1)
    ├── COACTUPC (Account Update - Option 2)
    ├── COCRDLIC (Credit Card List - Option 3)
    ├── COCRDSLC (Credit Card View - Option 4)
    ├── COCRDUPC (Credit Card Update - Option 5)
    ├── COTRN00C (Transaction List - Option 6)
    ├── COTRN01C (Transaction View - Option 7)
    ├── COTRN02C (Transaction Add - Option 8)
    ├── CORPT00C (Transaction Reports - Option 9)
    ├── COBIL00C (Bill Payment - Option 10)
    └── COPAUS0C (Pending Authorization View - Option 11)
```

## Runtime Dependencies

- **Transaction**: CM00 (defined in CARDDEMO.CSD)
- **CICS Program**: COMEN01C program definition
- **Screen Handling**: COMEN1A map from COMEN01 mapset
- **File Access**: None (no file I/O operations)
- **Database**: None (no DB2 access)

## Critical Dependencies Analysis

### High Priority (Critical for Execution)
1. **COCOM01Y** - Communication area structure used throughout program
2. **COMEN02Y** - Menu options data that drives program selection logic
3. **COMEN01.CPY** - BMS map required for screen I/O operations
4. **All called programs** - Menu functionality depends on these programs being available

### Medium Priority (Affects Functionality)
1. **CICS Transaction CM00** - Transaction definition for program execution
2. **Screen formatting copybooks** - COTTL01Y, CSDAT01Y for display formatting

### Low Priority (Utility/Enhancement)
1. **CSMSG01Y** - Common messages (fallback messages available)
2. **CSUSR01Y** - User data structure (not actively used in main logic)

## Program Control Flow

1. **Entry Point**: Transaction CM00 invokes COMEN01C
2. **Initial Display**: Shows main menu with 11 options (numbered 1-11)
3. **User Selection**: Processes menu option selection (1-11)
4. **Special Handling**: COPAUS0C program checked for availability before XCTL
5. **Program Transfer**: Uses XCTL to transfer control to selected program
6. **Return Path**: PF3 returns to COSGN00C (sign-on screen)
7. **Error Handling**: Invalid selections display error messages

## Security Dependencies

- **User Type Validation**: Checks CDEMO-USRTYP-USER vs admin-only options
- **Access Control**: Restricts admin-only options for regular users (though all current options are 'U' type)
- **File Security**: None (no file access in this program)

## CICS Commands Used

The program uses the following CICS commands:
1. **EXEC CICS RETURN** - Returns control with transaction ID and communication area
2. **EXEC CICS INQUIRE PROGRAM** - Checks if COPAUS0C program is available
3. **EXEC CICS XCTL** - Transfers control to selected programs
4. **EXEC CICS SEND MAP** - Sends screen to terminal
5. **EXEC CICS RECEIVE MAP** - Receives input from terminal

## External Interface Points

- **Entry**: Called from COSGN00C after successful sign-on
- **Exit**: Returns to COSGN00C on PF3 or error conditions
- **Menu Options**: Transfers control to 11 different functional programs
- **Communication**: Uses CARDDEMO-COMMAREA for inter-program data sharing

## Compilation Dependencies

All copybooks must be available in their respective directories:
- `cpy/` for COBOL copybooks
- `cpy-bms/` for BMS map copybooks
- System copybooks (DFHAID, DFHBMSCA) from CICS libraries

## Installation Dependencies

1. CICS resource definitions in CARDDEMO.CSD
2. All called programs must be installed and enabled
3. Special handling for COPAUS0C - program availability checked at runtime
4. BMS mapset COMEN01 must be installed

*Analysis completed using actual code inspection and cross-reference verification*

## Verification Summary

**Code-Verified Facts:**
- 8 copybooks confirmed via COPY statements in source code
- 11 callable programs confirmed via COMEN02Y menu options data
- USRSEC file defined in working storage but NO CICS file commands found
- All menu options are 'U' (User) type - no admin-only restrictions in current configuration
- COPAUS0C has special runtime availability check via CICS INQUIRE
- 5 distinct CICS commands used: RETURN, INQUIRE, XCTL, SEND MAP, RECEIVE MAP
- Transaction CM00 confirmed in CARDDEMO.CSD
- Program count corrected from 11 to 12 (including COSGN00C)
