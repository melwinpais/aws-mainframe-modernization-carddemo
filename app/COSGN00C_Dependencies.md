# COSGN00C Dependency Analysis

## Program Overview
**Program Name**: COSGN00C  
**Function**: Signon Screen for the CardDemo Application  
**Type**: CICS COBOL Program  
**Transaction ID**: CC00  

## Dependency Summary Table
| Dependency Type | Count | Critical Level | Examples |
|----------------|-------|----------------|----------|
| Copybooks | 8 | High | COCOM01Y, COSGN00, CSUSR01Y |
| Programs Called | 2 | High | COADM01C, COMEN01C |
| VSAM Files | 1 | High | USRSEC |
| DB2 Tables | 0 | N/A | None |
| BMS Maps | 1 | High | COSGN0A from COSGN00 mapset |
| CICS Resources | 2 | Medium | Transaction CC00, Program COSGN00C |
| External Systems | 0 | N/A | None |

## Detailed Dependency Matrix
| Source Program | Depends On | Dependency Type | File Location | Usage Context |
|---------------|------------|-----------------|---------------|---------------|
| COSGN00C | COCOM01Y | Copybook | cpy/COCOM01Y.cpy | Communication area structure |
| COSGN00C | COSGN00 | BMS Copybook | cpy-bms/COSGN00.CPY | Screen map definitions |
| COSGN00C | COTTL01Y | Copybook | cpy/COTTL01Y.cpy | Screen title constants |
| COSGN00C | CSDAT01Y | Copybook | cpy/CSDAT01Y.cpy | Date/time formatting |
| COSGN00C | CSMSG01Y | Copybook | cpy/CSMSG01Y.cpy | Common message constants |
| COSGN00C | CSUSR01Y | Copybook | cpy/CSUSR01Y.cpy | User security data structure |
| COSGN00C | DFHAID | System Copybook | System | CICS attention identifier constants |
| COSGN00C | DFHBMSCA | System Copybook | System | BMS attribute constants |
| COSGN00C | COADM01C | Program Transfer | cbl/COADM01C.cbl | Admin menu for admin users |
| COSGN00C | COMEN01C | Program Transfer | cbl/COMEN01C.cbl | Main menu for regular users |
| COSGN00C | USRSEC | VSAM File | VSAM Dataset | User security validation |
| COSGN00C | COSGN00 | BMS Mapset | bms/COSGN00.bms | Signon screen definition |

## Dependency Flow Diagram
```
COSGN00C (Signon Screen Program)
├── Input Dependencies
│   ├── COCOM01Y.cpy (Communication area structure)
│   ├── COSGN00.CPY (BMS screen map definitions)
│   ├── COTTL01Y.cpy (Screen title constants)
│   ├── CSDAT01Y.cpy (Date/time formatting structures)
│   ├── CSMSG01Y.cpy (Common message constants)
│   ├── CSUSR01Y.cpy (User security data structure)
│   ├── DFHAID (CICS attention identifier constants)
│   └── DFHBMSCA (BMS attribute constants)
├── Data Access
│   └── USRSEC (VSAM KSDS - User security file)
├── Screen Handling
│   └── COSGN0A (Map from COSGN00 mapset)
└── Program Flow
    ├── COADM01C (Admin menu - for admin users)
    └── COMEN01C (Main menu - for regular users)
```

## Runtime Dependencies

### Transaction Definition
- **Transaction**: CC00 (defined in CARDDEMO.CSD)
- **CICS Program**: COSGN00C program definition
- **Screen Handling**: COSGN0A map from COSGN00 mapset
- **File Access**: READ access to USRSEC VSAM file

### CICS Resources
- **Program Definition**: COSGN00C in CARDDEMO group
- **Transaction Definition**: CC00 in CARDDEMO group  
- **File Definition**: USRSEC file pointing to AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS
- **Mapset**: COSGN00 containing COSGN0A map

## Critical Dependencies Analysis

### High Priority (Critical for Execution)
1. **COCOM01Y.cpy** - Communication area structure used for inter-program data sharing
2. **COSGN00.CPY** - BMS map copybook defining screen layout and field structures
3. **CSUSR01Y.cpy** - User security data structure for authentication
4. **USRSEC VSAM file** - User security file for login validation
5. **COADM01C/COMEN01C** - Target programs for successful authentication

### Medium Priority (Functional Features)
1. **COTTL01Y.cpy** - Screen titles and application branding
2. **CSMSG01Y.cpy** - Standard error and information messages
3. **CSDAT01Y.cpy** - Date/time display formatting

### System Dependencies
1. **DFHAID** - CICS system copybook for attention identifiers (ENTER, PF3, etc.)
2. **DFHBMSCA** - CICS BMS attribute constants for screen formatting

## Data Flow Analysis

### Input Processing
- Receives user ID and password from COSGN0A screen map
- Validates input fields for presence and format
- Converts input to uppercase for processing

### Authentication Flow
1. Read USRSEC VSAM file using user ID as key
2. Compare entered password with stored password
3. Retrieve user type (Admin/User) for menu routing
4. Transfer control to appropriate menu program

### Program Transfer Logic
- **Admin Users (Type 'A')**: XCTL to COADM01C (Admin Menu)
- **Regular Users (Type 'U')**: XCTL to COMEN01C (Main Menu)
- **Authentication Failure**: Redisplay signon screen with error message

## File Access Patterns

### VSAM File Operations
- **File**: USRSEC (AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS)
- **Operation**: EXEC CICS READ
- **Key**: User ID (8 characters)
- **Record Structure**: SEC-USER-DATA from CSUSR01Y.cpy

## Error Handling Dependencies

### CICS Response Codes
- **Normal (0)**: Successful file read, proceed with authentication
- **Not Found (13)**: User ID not found in security file
- **Other**: General file access errors

### Error Messages
- Sourced from CSMSG01Y.cpy and inline literals
- Displayed on signon screen for user feedback

## Security Considerations

### Authentication Dependencies
- User ID and password validation against USRSEC file
- User type determination for menu access control
- Password comparison using exact match logic

### Data Protection
- Password fields handled in working storage
- No password logging or display in clear text
- Secure transfer of user context via communication area

## Compilation Dependencies

### Required Copybooks (in order)
1. COCOM01Y - Must be available for communication area
2. COSGN00 - Must be generated from BMS mapset
3. COTTL01Y - Screen title constants
4. CSDAT01Y - Date/time structures  
5. CSMSG01Y - Message constants
6. CSUSR01Y - User data structure
7. DFHAID - CICS system copybook
8. DFHBMSCA - CICS BMS system copybook

### BMS Preprocessing
- COSGN00.bms must be assembled to generate COSGN00.CPY
- Map and mapset definitions must be installed in CICS

## Installation Dependencies

### CICS Resource Definitions
- Program COSGN00C must be defined and installed
- Transaction CC00 must be defined and installed  
- File USRSEC must be defined and opened
- Mapset COSGN00 must be installed

### VSAM Dataset Requirements
- AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS must exist and be accessible
- Proper CICS file definition pointing to the dataset
- Read access permissions for CICS region

## Anti-Hallucination Verification

### Code Analysis Completeness
- [x] All COPY statements documented with verified file paths
- [x] All EXEC CICS READ commands mapped to USRSEC dataset
- [x] No EXEC SQL statements found (program uses VSAM only)
- [x] All XCTL program transfers identified (COADM01C, COMEN01C)
- [x] BMS mapset COSGN00 and map COSGN0A cross-referenced
- [x] Working storage file names captured (USRSEC)
- [x] No external system calls found

### System Resource Verification  
- [x] Transaction ID CC00 confirmed in CSD definitions
- [x] Program name COSGN00C matches CICS resource definition
- [x] VSAM file name USRSEC matches dataset definition in CSD
- [x] No DB2 table references found
- [x] Screen map names consistent between BMS and copybooks
- [x] No external system endpoints found

### Dependency Classification Verified
- [x] **Critical**: COCOM01Y, COSGN00, CSUSR01Y, USRSEC file, target programs
- [x] **High**: BMS maps, system copybooks for CICS operations  
- [x] **Medium**: Title and message copybooks for display
- [x] **Low**: Date/time formatting copybooks

### Output Accuracy Verified
- [x] Dependency counts match actual findings (8 copybooks, 2 programs, 1 file)
- [x] File locations use correct relative paths from app/
- [x] Usage context accurately describes code function
- [x] Flow diagram reflects actual program logic
- [x] Critical level assignments based on code analysis
- [x] All placeholders replaced with actual values from source code
