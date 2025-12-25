# COSGN00C Program Flow Analysis

## Program Overview
- **Program**: COSGN00C.cbl
- **Function**: Signon Screen for the CardDemo Application
- **Transaction ID**: CC00
- **Type**: CICS COBOL Program
- **Entry Point**: MAIN-PARA (Line 73)
- **Author**: AWS
- **Purpose**: User authentication and session initialization for CardDemo application

## High-Level Program Flow Diagram

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           COSGN00C PROGRAM FLOW                            │
└─────────────────────────────────────────────────────────────────────────────┘

                                    START
                                    │
                                    ▼
                            ┌─────────────┐
                            │ MAIN-PARA   │◄─── Entry Point (Line 73)
                            └─────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ SET ERR-FLG-OFF         │
                        │ MOVE SPACES TO MESSAGES │
                        └─────────────────────────┘
                                    │
                                    ▼
                            ┌─────────────┐
                            │EIBCALEN = 0?│
                            └─────────────┘
                                │     │
                            YES │     │ NO
                                ▼     ▼
                    ┌─────────────────┐   ┌─────────────────┐
                    │ Initialize      │   │ EVALUATE EIBAID │
                    │ Screen Display  │   │ (User Input)    │
                    └─────────────────┘   └─────────────────┘
                            │                     │
                            ▼                     ▼
                    ┌─────────────────┐   ┌─────────────────┐
                    │ PERFORM         │   │ DFHENTER?       │
                    │ SEND-SIGNON-    │   │ DFHPF3?         │
                    │ SCREEN          │   │ OTHER?          │
                    └─────────────────┘   └─────────────────┘
                            │                     │
                            │         ┌───────────┼───────────┐
                            │         ▼           ▼           ▼
                            │ ┌─────────────┐ ┌─────────┐ ┌─────────┐
                            │ │PROCESS-ENTER│ │SEND-    │ │Invalid  │
                            │ │KEY          │ │PLAIN-   │ │Key      │
                            │ └─────────────┘ │TEXT     │ │Error    │
                            │         │       └─────────┘ └─────────┘
                            │         │           │           │
                            │         │           ▼           │
                            │         │       ┌─────────┐     │
                            │         │       │TERMINATE│     │
                            │         │       │PROGRAM  │     │
                            │         │       └─────────┘     │
                            │         │                       │
                            └─────────┼───────────────────────┘
                                      │
                                      ▼
                            ┌─────────────────┐
                            │ EXEC CICS       │
                            │ RETURN          │
                            │ TRANSID(CC00)   │
                            │ COMMAREA        │
                            └─────────────────┘
                                      │
                                      ▼
                                    END
```

## Detailed Function Call Flow

### 1. MAIN-PARA Function Call Relationships

```
MAIN-PARA
├── Conditional Calls:
│   ├── SEND-SIGNON-SCREEN (when EIBCALEN = 0)
│   ├── PROCESS-ENTER-KEY (when EIBAID = DFHENTER)
│   ├── SEND-PLAIN-TEXT (when EIBAID = DFHPF3)
│   └── SEND-SIGNON-SCREEN (when EIBAID = OTHER)
└── Always Calls:
    └── EXEC CICS RETURN (program termination)
```

**Call Conditions:**
- `MAIN-PARA` calls `SEND-SIGNON-SCREEN` when `EIBCALEN = 0` (first time entry)
- `MAIN-PARA` calls `PROCESS-ENTER-KEY` when `EIBAID = DFHENTER` (user pressed ENTER)
- `MAIN-PARA` calls `SEND-PLAIN-TEXT` when `EIBAID = DFHPF3` (user pressed PF3)
- `MAIN-PARA` calls `SEND-SIGNON-SCREEN` when `EIBAID = OTHER` (invalid key pressed)

### 2. PROCESS-ENTER-KEY Function Flow

```
                            PROCESS-ENTER-KEY (Line 108)
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ EXEC CICS RECEIVE       │
                        │ MAP('COSGN0A')          │
                        │ MAPSET('COSGN00')       │
                        └─────────────────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ EVALUATE TRUE:          │
                        │ - USERIDI = SPACES?     │
                        │ - PASSWDI = SPACES?     │
                        │ - OTHER                 │
                        └─────────────────────────┘
                                    │
                    ┌─────────────────┼─────────────────┐
                    ▼                 ▼                 ▼
        ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
        │ User ID Error   │ │ Password Error  │ │ Continue        │
        │ Set ERR-FLG     │ │ Set ERR-FLG     │ │ Processing      │
        │ PERFORM SEND-   │ │ PERFORM SEND-   │ │                 │
        │ SIGNON-SCREEN   │ │ SIGNON-SCREEN   │ │                 │
        └─────────────────┘ └─────────────────┘ └─────────────────┘
                    │                 │                 │
                    └─────────────────┼─────────────────┘
                                      │
                                      ▼
                        ┌─────────────────────────┐
                        │ MOVE UPPER-CASE         │
                        │ USERIDI → WS-USER-ID    │
                        │ USERIDI → CDEMO-USER-ID │
                        │ PASSWDI → WS-USER-PWD   │
                        └─────────────────────────┘
                                      │
                                      ▼
                            ┌─────────────────┐
                            │ ERR-FLG-ON?     │
                            └─────────────────┘
                                  │     │
                              NO  │     │ YES
                                  ▼     ▼
                        ┌─────────────────┐   RETURN
                        │ PERFORM         │
                        │ READ-USER-SEC-  │
                        │ FILE            │
                        └─────────────────┘
                                  │
                                  ▼
                                RETURN
```

### 3. READ-USER-SEC-FILE Authentication Flow

```
                            READ-USER-SEC-FILE (Line 209)
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ EXEC CICS READ          │
                        │ DATASET(USRSEC)         │
                        │ RIDFLD(WS-USER-ID)      │
                        │ INTO(SEC-USER-DATA)     │
                        └─────────────────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ EVALUATE WS-RESP-CD:    │
                        │ - WHEN 0 (Success)      │
                        │ - WHEN 13 (Not Found)   │
                        │ - WHEN OTHER (Error)    │
                        └─────────────────────────┘
                                    │
                    ┌─────────────────┼─────────────────┐
                    ▼                 ▼                 ▼
        ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
        │ SUCCESS (0)     │ │ NOT FOUND (13)  │ │ OTHER ERROR     │
        │ Password Check  │ │ Set Error Flag  │ │ Set Error Flag  │
        └─────────────────┘ │ "User not found"│ │ "Unable to      │
                │           │ PERFORM SEND-   │ │ verify User"    │
                ▼           │ SIGNON-SCREEN   │ │ PERFORM SEND-   │
        ┌─────────────────┐ └─────────────────┘ │ SIGNON-SCREEN   │
        │SEC-USR-PWD =    │                     └─────────────────┘
        │WS-USER-PWD?     │                             │
        └─────────────────┘                             │
                │     │                                 │
            YES │     │ NO                              │
                ▼     ▼                                 │
    ┌─────────────┐ ┌─────────────────┐                │
    │ SUCCESS     │ │ "Wrong Password"│                │
    │ POPULATE    │ │ PERFORM SEND-   │                │
    │ COMMAREA    │ │ SIGNON-SCREEN   │                │
    └─────────────┘ └─────────────────┘                │
            │               │                          │
            ▼               │                          │
    ┌─────────────┐         │                          │
    │CDEMO-USRTYP-│         │                          │
    │ADMIN?       │         │                          │
    └─────────────┘         │                          │
        │     │             │                          │
    YES │     │ NO          │                          │
        ▼     ▼             │                          │
┌─────────┐ ┌─────────┐     │                          │
│XCTL     │ │XCTL     │     │                          │
│COADM01C │ │COMEN01C │     │                          │
│(Admin)  │ │(User)   │     │                          │
└─────────┘ └─────────┘     │                          │
    │           │           │                          │
    └───────────┼───────────┼──────────────────────────┘
                │           │
                ▼           ▼
            PROGRAM     PROGRAM
            TRANSFER    TRANSFER
```

### 4. SEND-SIGNON-SCREEN Display Flow

```
                            SEND-SIGNON-SCREEN (Line 145)
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ PERFORM                 │
                        │ POPULATE-HEADER-INFO    │
                        └─────────────────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ MOVE WS-MESSAGE TO      │
                        │ ERRMSGO OF COSGN0AO     │
                        └─────────────────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ EXEC CICS SEND          │
                        │ MAP('COSGN0A')          │
                        │ MAPSET('COSGN00')       │
                        │ FROM(COSGN0AO)          │
                        │ ERASE CURSOR            │
                        └─────────────────────────┘
                                    │
                                    ▼
                                  RETURN
```

### 5. POPULATE-HEADER-INFO System Information Flow

```
                            POPULATE-HEADER-INFO (Line 177)
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ MOVE CURRENT-DATE TO    │
                        │ WS-CURDATE-DATA         │
                        └─────────────────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ MOVE Title Constants:   │
                        │ CCDA-TITLE01 → TITLE01O │
                        │ CCDA-TITLE02 → TITLE02O │
                        │ WS-TRANID → TRNNAMEO    │
                        │ WS-PGMNAME → PGMNAMEO   │
                        └─────────────────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ Format Date/Time:       │
                        │ MM/DD/YY → CURDATEO     │
                        │ HH:MM:SS → CURTIMEO     │
                        └─────────────────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ EXEC CICS ASSIGN        │
                        │ APPLID(APPLIDO)         │
                        │ SYSID(SYSIDO)           │
                        └─────────────────────────┘
                                    │
                                    ▼
                                  RETURN
```

### 6. SEND-PLAIN-TEXT Termination Flow

```
                            SEND-PLAIN-TEXT (Line 162)
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ EXEC CICS SEND TEXT     │
                        │ FROM(WS-MESSAGE)        │
                        │ LENGTH(80)              │
                        │ ERASE FREEKB            │
                        └─────────────────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ EXEC CICS RETURN        │
                        │ (No TRANSID)            │
                        └─────────────────────────┘
                                    │
                                    ▼
                            PROGRAM TERMINATION
```

## Function Interaction Matrix

| Function | Calls | Called By | Conditions | Data Flow |
|----------|-------|-----------|------------|-----------|
| MAIN-PARA | SEND-SIGNON-SCREEN, PROCESS-ENTER-KEY, SEND-PLAIN-TEXT | CICS (CC00) | EIBCALEN=0, EIBAID values | EIBCALEN, EIBAID → Function selection |
| PROCESS-ENTER-KEY | SEND-SIGNON-SCREEN, READ-USER-SEC-FILE | MAIN-PARA | DFHENTER pressed | Screen input → WS variables |
| SEND-SIGNON-SCREEN | POPULATE-HEADER-INFO | MAIN-PARA, PROCESS-ENTER-KEY, READ-USER-SEC-FILE | Display required | WS-MESSAGE → Screen display |
| SEND-PLAIN-TEXT | None (terminates) | MAIN-PARA | DFHPF3 pressed | WS-MESSAGE → Terminal |
| POPULATE-HEADER-INFO | None | SEND-SIGNON-SCREEN | Screen display | System info → Screen fields |
| READ-USER-SEC-FILE | SEND-SIGNON-SCREEN | PROCESS-ENTER-KEY | Validation passed | User data → Authentication |

## Error Handling Flow

```
Error Conditions → Error Messages → Screen Actions
    │                   │              │
    ▼                   ▼              ▼
┌─────────────┐  ┌─────────────────┐  ┌─────────────────┐
│ Missing     │  │ "Please enter   │  │ Cursor to       │
│ User ID     │  │ User ID ..."    │  │ USERIDL         │
└─────────────┘  └─────────────────┘  └─────────────────┘

┌─────────────┐  ┌─────────────────┐  ┌─────────────────┐
│ Missing     │  │ "Please enter   │  │ Cursor to       │
│ Password    │  │ Password ..."   │  │ PASSWDL         │
└─────────────┘  └─────────────────┘  └─────────────────┘

┌─────────────┐  ┌─────────────────┐  ┌─────────────────┐
│ User Not    │  │ "User not found.│  │ Cursor to       │
│ Found (13)  │  │ Try again ..."  │  │ USERIDL         │
└─────────────┘  └─────────────────┘  └─────────────────┘

┌─────────────┐  ┌─────────────────┐  ┌─────────────────┐
│ Wrong       │  │ "Wrong Password.│  │ Cursor to       │
│ Password    │  │ Try again ..."  │  │ PASSWDL         │
└─────────────┘  └─────────────────┘  └─────────────────┘

┌─────────────┐  ┌─────────────────┐  ┌─────────────────┐
│ File Access │  │ "Unable to      │  │ Cursor to       │
│ Error       │  │ verify User..." │  │ USERIDL         │
└─────────────┘  └─────────────────┘  └─────────────────┘

┌─────────────┐  ┌─────────────────┐  ┌─────────────────┐
│ Invalid Key │  │ CCDA-MSG-       │  │ Redisplay       │
│ Pressed     │  │ INVALID-KEY     │  │ Screen          │
└─────────────┘  └─────────────────┘  └─────────────────┘
```

## Success Path Flow

```
Valid Input → Authentication → User Type Check → Program Transfer
    │              │              │                │
    ▼              ▼              ▼                ▼
┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐
│ USERIDI &   │  │ USRSEC File │  │ SEC-USR-TYPE│  │ XCTL to     │
│ PASSWDI     │  │ Read Success│  │ = 'A' or 'U'│  │ COADM01C or │
│ Not Spaces  │  │ Password    │  │             │  │ COMEN01C    │
│             │  │ Match       │  │             │  │             │
└─────────────┘  └─────────────┘  └─────────────┘  └─────────────┘
```

## Data Flow Through Functions

### Input Data Flow
```
Terminal Input → BMS Map → Working Storage → VSAM File
    │              │           │              │
    ▼              ▼           ▼              ▼
┌─────────────┐  ┌─────────┐  ┌─────────────┐  ┌─────────────┐
│ User Types  │  │COSGN0AI │  │ WS-USER-ID  │  │ RIDFLD for  │
│ Credentials │  │USERIDI  │  │ WS-USER-PWD │  │ CICS READ   │
│             │  │PASSWDI  │  │             │  │             │
└─────────────┘  └─────────┘  └─────────────┘  └─────────────┘
```

### Output Data Flow
```
System Info → Working Storage → BMS Map → Terminal Display
    │              │             │           │
    ▼              ▼             ▼           ▼
┌─────────────┐  ┌─────────────┐  ┌─────────┐  ┌─────────────┐
│ CURRENT-DATE│  │ Formatted   │  │COSGN0AO │  │ Screen      │
│ APPLID      │  │ Date/Time   │  │CURDATEO │  │ Display     │
│ SYSID       │  │ Titles      │  │CURTIMEO │  │             │
└─────────────┘  └─────────────┘  └─────────┘  └─────────────┘
```

### Communication Area Flow
```
Authentication Success → COMMAREA Population → Program Transfer
    │                        │                    │
    ▼                        ▼                    ▼
┌─────────────┐  ┌─────────────────────────┐  ┌─────────────┐
│ SEC-USR-TYPE│  │ CDEMO-FROM-TRANID       │  │ XCTL with   │
│ WS-USER-ID  │  │ CDEMO-FROM-PROGRAM      │  │ CARDDEMO-   │
│ WS-TRANID   │  │ CDEMO-USER-ID           │  │ COMMAREA    │
│ WS-PGMNAME  │  │ CDEMO-USER-TYPE         │  │             │
│             │  │ CDEMO-PGM-CONTEXT       │  │             │
└─────────────┘  └─────────────────────────┘  └─────────────┘
```

## Program Termination Points

1. **Normal Transaction Continuation (MAIN-PARA)**:
   - EXEC CICS RETURN TRANSID(WS-TRANID) COMMAREA(CARDDEMO-COMMAREA)
   - **CRITICAL**: Maintains pseudo-conversational state with transaction CC00

2. **Program Transfer (READ-USER-SEC-FILE)**:
   - EXEC CICS XCTL PROGRAM('COADM01C') for admin users
   - EXEC CICS XCTL PROGRAM('COMEN01C') for regular users
   - **CRITICAL**: Transfers control with populated communication area

3. **Session Termination (SEND-PLAIN-TEXT)**:
   - EXEC CICS RETURN (no TRANSID)
   - **CRITICAL**: Terminates pseudo-conversational session completely

## Integration with External Programs

### Upstream Integration
- **CICS Transaction Manager**: Invokes COSGN00C via transaction CC00
- **Previous Programs**: May pass communication area with context

### Downstream Integration
- **COADM01C**: Admin menu program for administrative users
- **COMEN01C**: Main menu program for regular users

### External Resources
- **USRSEC VSAM File**: User security authentication database
- **COSGN00 BMS Mapset**: Signon screen interface definition
- **System Copybooks**: DFHAID (attention identifiers), DFHBMSCA (BMS attributes)

## Critical Execution Paths

### Path 1: First Time User (EIBCALEN = 0)
```
START → MAIN-PARA → SEND-SIGNON-SCREEN → POPULATE-HEADER-INFO → RETURN
```

### Path 2: Successful Authentication
```
START → MAIN-PARA → PROCESS-ENTER-KEY → READ-USER-SEC-FILE → XCTL (COADM01C/COMEN01C)
```

### Path 3: Authentication Failure
```
START → MAIN-PARA → PROCESS-ENTER-KEY → READ-USER-SEC-FILE → SEND-SIGNON-SCREEN → RETURN
```

### Path 4: User Exit (PF3)
```
START → MAIN-PARA → SEND-PLAIN-TEXT → TERMINATE
```

## Quality Checklist Verification

### Completeness Verification
- [x] All paragraphs from source code are documented in flow charts
- [x] All PERFORM statements are traced with proper conditions
- [x] All decision structures (EVALUATE/IF) are visualized with branches
- [x] All program termination points are clearly marked
- [x] All external program calls (XCTL) are documented
- [x] All file/database operations (CICS READ) are included in flow

### Flow Chart Accuracy
- [x] ASCII flow chart symbols are consistent (┌─┐│▼►)
- [x] Decision points use proper condition labels (EIBCALEN=0, EIBAID values)
- [x] All execution paths lead to proper termination
- [x] Function call hierarchy matches source code exactly
- [x] Error handling paths are complete and accurate
- [x] Data flow between functions is correctly mapped

### Source Code Validation
- [x] Every function name in charts exists in source code (MAIN-PARA, PROCESS-ENTER-KEY, etc.)
- [x] Every condition matches actual program logic (EIBAID evaluation, WS-RESP-CD values)
- [x] Every variable reference exists in data definitions
- [x] Every system command is accurately represented (CICS READ, SEND, XCTL)
- [x] No assumptions made about missing code logic
- [x] All flow elements traceable to specific source lines

### Documentation Standards
- [x] Program overview section is complete and accurate
- [x] Function interaction matrix includes all relationships
- [x] Error handling section covers all error conditions with exact messages
- [x] Success path documentation is comprehensive
- [x] Integration points with external programs are documented
- [x] Critical execution notes are highlighted
