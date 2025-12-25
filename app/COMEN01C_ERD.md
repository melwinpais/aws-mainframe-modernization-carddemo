# COMEN01C Entity Relationship Diagram

**Program**: COMEN01C  
**Analysis Date**: 2025-12-24  
**Program Type**: CICS COBOL Program  
**Function**: Main Menu for Regular Users  

## Executive Summary

- **Program Purpose**: Main menu interface for regular users to access CardDemo application functions
- **Key Entities Identified**: 5
- **Main Relationships**: 6
- **Data Complexity Level**: MEDIUM

## Entity Relationship Diagram

```
┌─────────────────┐    manages    ┌─────────────────┐    displays    ┌─────────────────┐
│   MenuSession   │ ────────────→ │   MenuOptions   │ ────────────→ │   ScreenDisplay │
│                 │               │                 │               │                 │
│ - FROM-PROGRAM  │               │ - OPT-COUNT     │               │ - TITLE01       │
│ - TO-PROGRAM    │               │ - OPT-NUM       │               │ - TITLE02       │
│ - PGM-CONTEXT   │               │ - OPT-NAME      │               │ - CURDATE       │
│ - FROM-TRANID   │               │ - OPT-PGMNAME   │               │ - CURTIME       │
│                 │               │ - OPT-USRTYPE   │               │ - OPTN001-012   │
└─────────────────┘               └─────────────────┘               │ - OPTION        │
         │                                 │                        │ - ERRMSG        │
         │                                 │                        └─────────────────┘
         │ controls                        │ routes_to                       │
         ↓                                 ↓                                 │
┌─────────────────┐               ┌─────────────────┐                       │
│ WorkingStorage  │               │  TargetPrograms │                       │
│                 │               │                 │                       │
│ - WS-PGMNAME    │               │ - COACTVWC      │                       │
│ - WS-TRANID     │               │ - COACTUPC      │                       │
│ - WS-MESSAGE    │               │ - COCRDLIC      │                       │
│ - WS-ERR-FLG    │               │ - COCRDSLC      │                       │
│ - WS-OPTION     │               │ - COCRDUPC      │                       │
│ - WS-IDX        │               │ - COTRN00C      │                       │
│ - WS-USRSEC-FILE│               │ - COTRN01C      │                       │
└─────────────────┘               │ - COTRN02C      │                       │
         │                        │ - CORPT00C      │                       │
         │                        │ - COBIL00C      │                       │
         │ processes               │ - COPAUS0C      │                       │
         └────────────────────────│ - COSGN00C      │                       │
                                  └─────────────────┘                       │
                                           │                                │
                                           │ transfers_to                   │
                                           └────────────────────────────────┘
```

## Entity Summary Table

| Entity Name | Source | Primary Key | Key Attributes | Related Entities | Business Purpose |
|-------------|--------|-------------|----------------|------------------|------------------|
| MenuSession | COCOM01Y | N/A | CDEMO-FROM-PROGRAM, CDEMO-TO-PROGRAM, CDEMO-PGM-CONTEXT, CDEMO-FROM-TRANID | MenuOptions, TargetPrograms | Inter-program communication and session management |
| MenuOptions | COMEN02Y | CDEMO-MENU-OPT-NUM | CDEMO-MENU-OPT-NAME, CDEMO-MENU-OPT-PGMNAME, CDEMO-MENU-OPT-USRTYPE, CDEMO-MENU-OPT-COUNT | MenuSession, TargetPrograms | Menu configuration and program routing |
| ScreenDisplay | COMEN01 BMS | Map Name | TITLE01O, TITLE02O, OPTN001O-OPTN012O, OPTIONI, ERRMSGO | MenuSession, WorkingStorage | User interface presentation |
| TargetPrograms | Program List | Program Name | COACTVWC, COACTUPC, COCRDLIC, etc. | MenuOptions, MenuSession | Application function modules |
| WorkingStorage | WS-VARIABLES | WS-PGMNAME | WS-TRANID, WS-MESSAGE, WS-ERR-FLG, WS-OPTION, WS-IDX, WS-USRSEC-FILE | ScreenDisplay, MenuSession | Program control and processing |

## Data Flow Matrix

| Source Entity | Data Flow | Target Entity | Flow Type | Business Rule | Implementation |
|---------------|-----------|---------------|-----------|---------------|----------------|
| ScreenDisplay | User Option Selection | WorkingStorage | Input Validation | Option must be 1-11 and numeric | BMS RECEIVE → WS-OPTION |
| WorkingStorage | Menu Option | MenuOptions | Option Lookup | Option must exist in menu array | Array access via WS-OPTION |
| MenuOptions | Program Name | TargetPrograms | Program Transfer | User type validation for access | CICS XCTL to selected program |
| MenuSession | Program Context | TargetPrograms | Context Passing | Communication area preserved | COMMAREA in XCTL |
| WorkingStorage | Error Messages | ScreenDisplay | Error Display | Invalid selections show messages | WS-MESSAGE → ERRMSGO |
| MenuSession | Program Flow | WorkingStorage | Control Logic | Entry vs re-entry handling | CDEMO-PGM-CONTEXT → logic flow |
| WorkingStorage | Screen Data | ScreenDisplay | Display Update | Current date/time and titles | WS fields → BMS output fields |

## Variable Mapping by Category

| Variable Name | Entity | Source | Data Type | Usage Pattern | Validation Rules |
|---------------|--------|--------|-----------|---------------|------------------|
| OPTIONI | ScreenDisplay | BMS Map | X(2) | Input | Required, numeric 1-11 |
| WS-OPTION | WorkingStorage | Working Storage | 9(02) | Processing | Converted from OPTIONI |
| CDEMO-MENU-OPT-NUM | MenuOptions | COMEN02Y | 9(02) | Reference | Menu option numbers 1-11 |
| CDEMO-MENU-OPT-PGMNAME | MenuOptions | COMEN02Y | X(08) | Target | Program names for XCTL |
| CDEMO-USRTYP-USER | MenuSession | COCOM01Y | Condition | Authorization | Condition name for user type |
| WS-ERR-FLG | WorkingStorage | Working Storage | X(01) | Control | 'Y' = Error, 'N' = No Error |
| ERRMSGO | ScreenDisplay | BMS Map | X(78) | Output | Error message display |
| WS-MESSAGE | WorkingStorage | Working Storage | X(80) | Processing | Temporary message storage |
| CDEMO-FROM-PROGRAM | MenuSession | COCOM01Y | X(08) | Context | Calling program name |
| CDEMO-TO-PROGRAM | MenuSession | COCOM01Y | X(08) | Context | Target program name |

## Detailed Entity Relationships

### 1. MenuSession ↔ MenuOptions (1:M)
- **Relationship**: One session manages multiple menu options
- **Cardinality**: 1 session : 11 menu options
- **Implementation**: CDEMO-MENU-OPT array with OCCURS 12
- **Business Rule**: User type determines accessible options (though all current options are 'U')

### 2. MenuOptions ↔ TargetPrograms (1:1)
- **Relationship**: Each menu option maps to one target program
- **Cardinality**: 1 option : 1 program
- **Implementation**: CDEMO-MENU-OPT-PGMNAME field
- **Business Rule**: Program must exist and be enabled (COPAUS0C checked via INQUIRE)

### 3. WorkingStorage ↔ ScreenDisplay (1:1)
- **Relationship**: Working storage controls screen presentation
- **Cardinality**: 1 program instance : 1 screen instance
- **Implementation**: WS fields → BMS output fields
- **Business Rule**: Screen reflects current program state

### 4. MenuSession ↔ WorkingStorage (1:1)
- **Relationship**: Session context drives working storage logic
- **Cardinality**: 1 session : 1 working storage instance
- **Implementation**: CDEMO-PGM-CONTEXT → program flow
- **Business Rule**: Entry vs re-entry determines processing

## Copybook Structure Relationships

```
COCOM01Y (Communication Area) → MenuSession Entity
├── CDEMO-GENERAL-INFO → Session Management
│   ├── CDEMO-FROM-TRANID → Transaction routing
│   ├── CDEMO-FROM-PROGRAM → Program flow control
│   ├── CDEMO-TO-PROGRAM → Target program routing
│   ├── CDEMO-USER-ID → User identification
│   ├── CDEMO-USER-TYPE → Authorization level
│   └── CDEMO-PGM-CONTEXT → Entry/re-entry control
├── CDEMO-CUSTOMER-INFO → Business context (not used in COMEN01C)
├── CDEMO-ACCOUNT-INFO → Account context (not used in COMEN01C)
├── CDEMO-CARD-INFO → Card context (not used in COMEN01C)
└── CDEMO-MORE-INFO → Additional context (not used in COMEN01C)

COMEN02Y (Menu Options) → MenuOptions Entity
├── CDEMO-MENU-OPT-COUNT → Option count control
└── CDEMO-MENU-OPTIONS (OCCURS 12) → Menu option array
    ├── CDEMO-MENU-OPT-NUM → Option number (1-11)
    ├── CDEMO-MENU-OPT-NAME → Option description
    ├── CDEMO-MENU-OPT-PGMNAME → Target program name
    └── CDEMO-MENU-OPT-USRTYPE → Required user type

CSUSR01Y (User Security) → Not Used in COMEN01C
├── SEC-USR-ID → User identification key (copybook included but not accessed)
├── SEC-USR-FNAME → User first name (not used)
├── SEC-USR-LNAME → User last name (not used)
├── SEC-USR-PWD → User password (not used)
├── SEC-USR-TYPE → User authorization type (commented out in code)
└── SEC-USR-FILLER → Reserved space (not used)

COMEN01 BMS Map → ScreenDisplay Entity
├── Input Area (COMEN1AI) → User input fields
│   ├── OPTIONI → Menu option selection
│   └── Field attributes (L/F/A suffixes)
└── Output Area (COMEN1AO) → Display fields
    ├── TITLE01O/TITLE02O → Screen titles
    ├── OPTN001O-OPTN012O → Menu option displays
    ├── OPTIONO → Selected option echo
    └── ERRMSGO → Error message display
```

## Technical Notes

### Assumptions Made During Analysis
- Communication area structure is consistent across all called programs
- BMS map field naming follows standard CICS conventions
- USRSEC file structure follows CSUSR01Y copybook layout (though not accessed in this program)
- All menu options are currently configured as 'U' (User) type

### Areas Requiring Clarification
- Actual user authentication is handled by calling programs (not COMEN01C)
- Admin-only menu options (none currently configured)
- Error handling for unavailable programs (special case for COPAUS0C only)
- Customer/Account/Card context usage in called programs

### Potential Data Quality Issues
- Menu option validation relies on numeric conversion of character input
- No explicit validation of program availability (except COPAUS0C)
- Error messages stored in working storage may be truncated
- Communication area size limits for inter-program data passing
- User type validation code exists but user fields are not populated in this program

### Performance Considerations
- Menu options loaded as static data (no file I/O)
- Single screen interaction per program execution
- Minimal working storage usage
- Direct program transfer via XCTL (no return to menu)
- No VSAM file access in this program (USRSEC defined but not used)

## CICS Command Patterns

### Screen Management Flow
```
CICS RECEIVE MAP → Input Validation → Menu Processing → CICS SEND MAP
     ↓                    ↓                  ↓              ↓
  COMEN1AI          WS-OPTION         Menu Array      COMEN1AO
```

### Program Transfer Flow
```
Menu Selection → Option Validation → Program Lookup → CICS XCTL
      ↓               ↓                    ↓             ↓
   WS-OPTION    Range Check 1-11    CDEMO-MENU-OPT   Target Program
```

### Error Handling Flow
```
Validation Error → Set Error Flag → Build Message → Display Screen
       ↓              ↓               ↓              ↓
   Invalid Input   WS-ERR-FLG      WS-MESSAGE     ERRMSGO
```

*Analysis completed using actual code inspection - all entities and relationships verified against source code, copybooks, and program logic*

## Verification Summary

**Code-Verified Facts:**
- **UserSecurity Entity**: Removed from ERD - CSUSR01Y copybook included but not accessed in COMEN01C
- **User Authentication**: Not performed in this program - user fields commented out in code
- **VSAM File Access**: None - USRSEC defined in working storage but no CICS READ commands
- **Communication Area Usage**: Only CDEMO-FROM-PROGRAM, CDEMO-TO-PROGRAM, CDEMO-PGM-CONTEXT, and CDEMO-FROM-TRANID actually used
- **Menu Options**: All 11 options configured as 'U' type, no admin restrictions in current data
- **Program Validation**: Only COPAUS0C checked via CICS INQUIRE command
- **Entity Count**: Reduced from 6 to 5 entities (removed UserSecurity)

**Corrections Made:**
- Removed UserSecurity entity and all related relationships
- Updated MenuSession entity to show only actually used fields
- Corrected data flow matrix to remove authentication flows
- Updated variable mapping to reflect actual code usage
- Added WS-USRSEC-FILE to WorkingStorage entity (defined but not used)
- Clarified that user type validation code exists but user fields are not populated
- Updated technical notes to reflect actual program behavior
