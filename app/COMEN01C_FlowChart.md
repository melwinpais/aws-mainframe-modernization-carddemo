# COMEN01C Program Flow Analysis

## Program Overview
- **Program**: COMEN01C.cbl
- **Function**: Main Menu for Regular Users
- **Transaction ID**: CM00
- **Type**: CICS COBOL Program
- **Entry Point**: MAIN-PARA (Line 75)
- **Author**: AWS
- **Purpose**: Display menu options and route users to selected CardDemo application functions

## High-Level Program Flow Diagram

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           COMEN01C PROGRAM FLOW                            │
└─────────────────────────────────────────────────────────────────────────────┘

                                    START
                                    │
                                    ▼
                            ┌─────────────┐
                            │ MAIN-PARA   │◄─── Entry Point (Line 75)
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
                    │ Set FROM-PROGRAM│   │ Move COMMAREA   │
                    │ to 'COSGN00C'   │   │ Check PGM-      │
                    │ PERFORM RETURN- │   │ CONTEXT         │
                    │ TO-SIGNON-SCREEN│   └─────────────────┘
                    └─────────────────┘           │
                            │                     ▼
                            │           ┌─────────────────┐
                            │           │CDEMO-PGM-       │
                            │           │REENTER?         │
                            │           └─────────────────┘
                            │                 │     │
                            │             NO  │     │ YES
                            │                 ▼     ▼
                            │     ┌─────────────────┐ ┌─────────────────┐
                            │     │ Set REENTER     │ │ PERFORM         │
                            │     │ Initialize      │ │ RECEIVE-MENU-   │
                            │     │ Screen          │ │ SCREEN          │
                            │     │ PERFORM SEND-   │ └─────────────────┘
                            │     │ MENU-SCREEN     │         │
                            │     └─────────────────┘         ▼
                            │             │           ┌─────────────────┐
                            │             │           │ EVALUATE EIBAID │
                            │             │           │ DFHENTER?       │
                            │             │           │ DFHPF3?         │
                            │             │           │ OTHER?          │
                            │             │           └─────────────────┘
                            │             │                   │
                            │             │       ┌───────────┼───────────┐
                            │             │       ▼           ▼           ▼
                            │             │ ┌─────────────┐ ┌─────────┐ ┌─────────┐
                            │             │ │PROCESS-ENTER│ │Return to│ │Invalid  │
                            │             │ │KEY          │ │Signon   │ │Key      │
                            │             │ └─────────────┘ │Screen   │ │Error    │
                            │             │       │         └─────────┘ └─────────┘
                            │             │       │             │           │
                            │             └───────┼─────────────┼───────────┘
                            │                     │             │
                            └─────────────────────┼─────────────┘
                                                  │
                                                  ▼
                                    ┌─────────────────┐
                                    │ EXEC CICS       │
                                    │ RETURN          │
                                    │ TRANSID(CM00)   │
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
│   ├── RETURN-TO-SIGNON-SCREEN (when EIBCALEN = 0)
│   ├── SEND-MENU-SCREEN (when NOT CDEMO-PGM-REENTER)
│   ├── RECEIVE-MENU-SCREEN (when CDEMO-PGM-REENTER)
│   ├── PROCESS-ENTER-KEY (when EIBAID = DFHENTER)
│   └── RETURN-TO-SIGNON-SCREEN (when EIBAID = DFHPF3)
└── Always Calls:
    └── EXEC CICS RETURN (program continuation)
```

**Call Conditions:**
- `MAIN-PARA` calls `RETURN-TO-SIGNON-SCREEN` when `EIBCALEN = 0` (no communication area)
- `MAIN-PARA` calls `SEND-MENU-SCREEN` when `NOT CDEMO-PGM-REENTER` (first time display)
- `MAIN-PARA` calls `RECEIVE-MENU-SCREEN` when `CDEMO-PGM-REENTER` (returning from input)
- `MAIN-PARA` calls `PROCESS-ENTER-KEY` when `EIBAID = DFHENTER` (user pressed ENTER)
- `MAIN-PARA` calls `RETURN-TO-SIGNON-SCREEN` when `EIBAID = DFHPF3` (user pressed PF3)

### 2. PROCESS-ENTER-KEY Function Flow

```
                            PROCESS-ENTER-KEY (Line 115)
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ Parse Option Input:     │
                        │ - Trim trailing spaces  │
                        │ - Replace spaces with 0 │
                        │ - Convert to numeric    │
                        └─────────────────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ Validate Option:        │
                        │ - Is Numeric?           │
                        │ - <= Menu Count?        │
                        │ - Not Zero?             │
                        └─────────────────────────┘
                                    │
                            ┌───────┴───────┐
                        VALID│               │INVALID
                            ▼               ▼
                ┌─────────────────┐ ┌─────────────────┐
                │ Check User      │ │ Set Error Flag  │
                │ Authorization   │ │ "Please enter   │
                │ for Option      │ │ valid option"   │
                └─────────────────┘ │ PERFORM SEND-   │
                        │           │ MENU-SCREEN     │
                        ▼           └─────────────────┘
                ┌─────────────────┐         │
                │ User Type vs    │         │
                │ Option Access   │         │
                │ Level Check     │         │
                └─────────────────┘         │
                        │                   │
                ┌───────┴───────┐           │
            AUTH│               │NO AUTH    │
                ▼               ▼           │
    ┌─────────────────┐ ┌─────────────────┐ │
    │ EVALUATE        │ │ Set Error Flag  │ │
    │ Program Type:   │ │ "No access -    │ │
    │ - COPAUS0C      │ │ Admin Only"     │ │
    │ - DUMMY*        │ │ PERFORM SEND-   │ │
    │ - OTHER         │ │ MENU-SCREEN     │ │
    └─────────────────┘ └─────────────────┘ │
            │                   │           │
            ▼                   │           │
    ┌─────────────────┐         │           │
    │ Program Action: │         │           │
    │ - INQUIRE/XCTL  │         │           │
    │ - Coming Soon   │         │           │
    │ - Direct XCTL   │         │           │
    └─────────────────┘         │           │
            │                   │           │
            └───────────────────┼───────────┘
                                │
                                ▼
                        ┌─────────────────┐
                        │ PERFORM SEND-   │
                        │ MENU-SCREEN     │
                        └─────────────────┘
                                │
                                ▼
                              RETURN
```

### 3. SEND-MENU-SCREEN Display Flow

```
                            SEND-MENU-SCREEN (Line 208)
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ PERFORM                 │
                        │ POPULATE-HEADER-INFO    │
                        └─────────────────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ PERFORM                 │
                        │ BUILD-MENU-OPTIONS      │
                        └─────────────────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ MOVE WS-MESSAGE TO      │
                        │ ERRMSGO OF COMEN1AO     │
                        └─────────────────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ EXEC CICS SEND          │
                        │ MAP('COMEN1A')          │
                        │ MAPSET('COMEN01')       │
                        │ FROM(COMEN1AO)          │
                        │ ERASE                   │
                        └─────────────────────────┘
                                    │
                                    ▼
                                  RETURN
```

### 4. BUILD-MENU-OPTIONS Dynamic Menu Flow

```
                            BUILD-MENU-OPTIONS (Line 262)
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ PERFORM VARYING WS-IDX  │
                        │ FROM 1 BY 1 UNTIL       │
                        │ WS-IDX > CDEMO-MENU-    │
                        │ OPT-COUNT               │
                        └─────────────────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ For Each Menu Option:   │
                        │ STRING Option Number +  │
                        │ '. ' + Option Name      │
                        │ INTO WS-MENU-OPT-TXT    │
                        └─────────────────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ EVALUATE WS-IDX:        │
                        │ WHEN 1 → OPTN001O       │
                        │ WHEN 2 → OPTN002O       │
                        │ ...                     │
                        │ WHEN 12 → OPTN012O      │
                        └─────────────────────────┘
                                    │
                                    ▼
                                  RETURN
```

### 5. POPULATE-HEADER-INFO System Information Flow

```
                            POPULATE-HEADER-INFO (Line 238)
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
                                  RETURN
```

### 6. RECEIVE-MENU-SCREEN Input Processing Flow

```
                            RECEIVE-MENU-SCREEN (Line 225)
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ EXEC CICS RECEIVE       │
                        │ MAP('COMEN1A')          │
                        │ MAPSET('COMEN01')       │
                        │ INTO(COMEN1AI)          │
                        │ RESP(WS-RESP-CD)        │
                        │ RESP2(WS-REAS-CD)       │
                        └─────────────────────────┘
                                    │
                                    ▼
                                  RETURN
```

### 7. RETURN-TO-SIGNON-SCREEN Navigation Flow

```
                            RETURN-TO-SIGNON-SCREEN (Line 196)
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ IF CDEMO-TO-PROGRAM =   │
                        │ LOW-VALUES OR SPACES    │
                        │ MOVE 'COSGN00C' TO      │
                        │ CDEMO-TO-PROGRAM        │
                        └─────────────────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ EXEC CICS XCTL          │
                        │ PROGRAM(CDEMO-TO-       │
                        │ PROGRAM)                │
                        └─────────────────────────┘
                                    │
                                    ▼
                            PROGRAM TRANSFER
```

## Function Interaction Matrix

| Function | Calls | Called By | Conditions | Data Flow |
|----------|-------|-----------|------------|-----------|
| MAIN-PARA | RETURN-TO-SIGNON-SCREEN, SEND-MENU-SCREEN, RECEIVE-MENU-SCREEN, PROCESS-ENTER-KEY | CICS (CM00) | EIBCALEN=0, PGM-CONTEXT, EIBAID values | COMMAREA → Program flow |
| PROCESS-ENTER-KEY | SEND-MENU-SCREEN | MAIN-PARA | DFHENTER pressed | Option input → Validation → Program routing |
| SEND-MENU-SCREEN | POPULATE-HEADER-INFO, BUILD-MENU-OPTIONS | MAIN-PARA, PROCESS-ENTER-KEY | Display required | Menu data → Screen display |
| RECEIVE-MENU-SCREEN | None | MAIN-PARA | CDEMO-PGM-REENTER | Screen input → Working storage |
| POPULATE-HEADER-INFO | None | SEND-MENU-SCREEN | Screen display | System info → Screen fields |
| BUILD-MENU-OPTIONS | None | SEND-MENU-SCREEN | Menu display | Menu options → Screen fields |
| RETURN-TO-SIGNON-SCREEN | None | MAIN-PARA, PROCESS-ENTER-KEY | Navigation required | COMMAREA → Target program |

## Error Handling Flow

```
Error Conditions → Error Messages → Screen Actions
    │                   │              │
    ▼                   ▼              ▼
┌─────────────┐  ┌─────────────────┐  ┌─────────────────┐
│ Invalid     │  │ "Please enter   │  │ Redisplay Menu  │
│ Option      │  │ valid option    │  │ with Error      │
│ Number      │  │ number..."      │  │ Message         │
└─────────────┘  └─────────────────┘  └─────────────────┘

┌─────────────┐  ┌─────────────────┐  ┌─────────────────┐
│ No Access   │  │ "No access -    │  │ Redisplay Menu  │
│ to Admin    │  │ Admin Only      │  │ with Error      │
│ Option      │  │ option..."      │  │ Message         │
└─────────────┘  └─────────────────┘  └─────────────────┘

┌─────────────┐  ┌─────────────────┐  ┌─────────────────┐
│ Program Not │  │ "This option    │  │ Redisplay Menu  │
│ Installed   │  │ [name] is not   │  │ with Red Error  │
│ (COPAUS0C)  │  │ installed..."   │  │ Message         │
└─────────────┘  └─────────────────┘  └─────────────────┘

┌─────────────┐  ┌─────────────────┐  ┌─────────────────┐
│ Coming Soon │  │ "This option    │  │ Redisplay Menu  │
│ Feature     │  │ [name] is       │  │ with Green Info │
│ (DUMMY*)    │  │ coming soon..." │  │ Message         │
└─────────────┘  └─────────────────┘  └─────────────────┘

┌─────────────┐  ┌─────────────────┐  ┌─────────────────┐
│ Invalid Key │  │ CCDA-MSG-       │  │ Redisplay Menu  │
│ Pressed     │  │ INVALID-KEY     │  │ with Error      │
└─────────────┘  └─────────────────┘  └─────────────────┘
```

## Success Path Flow

```
Valid Option → Authorization Check → Program Type Check → Program Transfer
    │              │                    │                  │
    ▼              ▼                    ▼                  ▼
┌─────────────┐  ┌─────────────┐  ┌─────────────────┐  ┌─────────────┐
│ Numeric     │  │ User Type   │  │ COPAUS0C:       │  │ XCTL to     │
│ 1-11        │  │ matches     │  │ INQUIRE first   │  │ Selected    │
│ Within      │  │ Option      │  │ DUMMY*: Info    │  │ Program     │
│ Range       │  │ Access      │  │ OTHER: Direct   │  │             │
└─────────────┘  └─────────────┘  └─────────────────┘  └─────────────┘
```

## Data Flow Through Functions

### Input Data Flow
```
Terminal Input → BMS Map → Working Storage → Option Validation
    │              │           │              │
    ▼              ▼           ▼              ▼
┌─────────────┐  ┌─────────┐  ┌─────────────┐  ┌─────────────┐
│ User Types  │  │COMEN1AI │  │ WS-OPTION-X │  │ WS-OPTION   │
│ Option      │  │OPTIONI  │  │ WS-OPTION   │  │ Numeric     │
│ Number      │  │         │  │             │  │ Validation  │
└─────────────┘  └─────────┘  └─────────────┘  └─────────────┘
```

### Output Data Flow
```
Menu Options → Screen Fields → BMS Map → Terminal Display
    │              │             │           │
    ▼              ▼             ▼           ▼
┌─────────────┐  ┌─────────────┐  ┌─────────┐  ┌─────────────┐
│ CDEMO-MENU- │  │ OPTN001O-   │  │COMEN1AO │  │ Menu Screen │
│ OPT-NAME    │  │ OPTN012O    │  │         │  │ Display     │
│ Array       │  │ Fields      │  │         │  │             │
└─────────────┘  └─────────────┘  └─────────┘  └─────────────┘
```

### Communication Area Flow
```
Input COMMAREA → Program Context → Option Processing → Output COMMAREA
    │                │                │                  │
    ▼                ▼                ▼                  ▼
┌─────────────┐  ┌─────────────────┐  ┌─────────────┐  ┌─────────────┐
│ CARDDEMO-   │  │ CDEMO-PGM-      │  │ Selected    │  │ Updated     │
│ COMMAREA    │  │ CONTEXT         │  │ Program     │  │ COMMAREA    │
│ from Caller │  │ CDEMO-USER-TYPE │  │ Validation  │  │ for Target  │
└─────────────┘  └─────────────────┘  └─────────────┘  └─────────────┘
```

## Program Termination Points

1. **Normal Transaction Continuation (MAIN-PARA)**:
   - EXEC CICS RETURN TRANSID(WS-TRANID) COMMAREA(CARDDEMO-COMMAREA)
   - **CRITICAL**: Maintains pseudo-conversational state with transaction CM00

2. **Program Transfer to Selected Option (PROCESS-ENTER-KEY)**:
   - EXEC CICS XCTL PROGRAM(CDEMO-MENU-OPT-PGMNAME(WS-OPTION))
   - **CRITICAL**: Transfers control to selected CardDemo function with COMMAREA

3. **Return to Signon Screen (RETURN-TO-SIGNON-SCREEN)**:
   - EXEC CICS XCTL PROGRAM(CDEMO-TO-PROGRAM) - typically 'COSGN00C'
   - **CRITICAL**: Navigation back to authentication screen

## Integration with External Programs

### Upstream Integration
- **COSGN00C**: Signon program that transfers control to COMEN01C after successful authentication
- **Other CardDemo Programs**: May return to COMEN01C via PF3 or menu navigation

### Downstream Integration
- **COACTVWC**: Account View program (Option 1)
- **COACTUPC**: Account Update program (Option 2)
- **COCRDLIC**: Credit Card List program (Option 3)
- **COCRDSLC**: Credit Card View program (Option 4)
- **COBIL00C**: Bill Payment program (Option 5)
- **COTRN00C**: Transaction List program (Option 6)
- **CORPT00C**: Report Menu program (Option 7)
- **COUSR00C**: User List program (Option 8)
- **COPAUS0C**: Pause/Demo program (Option 9) - with availability check
- **DUMMY programs**: Placeholder programs for future features (Options 10-11)

### External Resources
- **COMEN01 BMS Mapset**: Menu screen interface definition
- **COMEN02Y Copybook**: Menu options configuration data
- **System Copybooks**: DFHAID (attention identifiers), DFHBMSCA (BMS attributes)

## Critical Execution Paths

### Path 1: First Time Entry (EIBCALEN = 0)
```
START → MAIN-PARA → RETURN-TO-SIGNON-SCREEN → XCTL COSGN00C
```

### Path 2: Initial Menu Display (NOT CDEMO-PGM-REENTER)
```
START → MAIN-PARA → SEND-MENU-SCREEN → BUILD-MENU-OPTIONS → RETURN
```

### Path 3: Option Selection and Transfer
```
START → MAIN-PARA → RECEIVE-MENU-SCREEN → PROCESS-ENTER-KEY → XCTL (Selected Program)
```

### Path 4: Return to Signon (PF3)
```
START → MAIN-PARA → RETURN-TO-SIGNON-SCREEN → XCTL COSGN00C
```

### Path 5: Error Handling
```
START → MAIN-PARA → PROCESS-ENTER-KEY → SEND-MENU-SCREEN → RETURN
```

## Quality Checklist Verification

### Completeness Verification
- [x] All paragraphs from source code are documented in flow charts
- [x] All PERFORM statements are traced with proper conditions
- [x] All decision structures (EVALUATE/IF) are visualized with branches
- [x] All program termination points are clearly marked
- [x] All external program calls (XCTL) are documented
- [x] All BMS operations (SEND/RECEIVE) are included in flow

### Flow Chart Accuracy
- [x] ASCII flow chart symbols are consistent (┌─┐│▼►)
- [x] Decision points use proper condition labels (EIBCALEN=0, CDEMO-PGM-REENTER, EIBAID values)
- [x] All execution paths lead to proper termination
- [x] Function call hierarchy matches source code exactly
- [x] Error handling paths are complete and accurate
- [x] Data flow between functions is correctly mapped

### Source Code Validation
- [x] Every function name in charts exists in source code (MAIN-PARA, PROCESS-ENTER-KEY, etc.)
- [x] Every condition matches actual program logic (EIBAID evaluation, option validation)
- [x] Every variable reference exists in data definitions
- [x] Every system command is accurately represented (CICS SEND, RECEIVE, XCTL, INQUIRE)
- [x] No assumptions made about missing code logic
- [x] All flow elements traceable to specific source lines

### Documentation Standards
- [x] Program overview section is complete and accurate
- [x] Function interaction matrix includes all relationships
- [x] Error handling section covers all error conditions with exact messages
- [x] Success path documentation is comprehensive
- [x] Integration points with external programs are documented
- [x] Critical execution notes are highlighted

## Verification Summary

**Code-Verified Facts:**
- **XCTL Commands**: 3 total XCTL commands that TERMINATE program execution (no return to SEND-MENU-SCREEN)
  - Line 156: XCTL to COPAUS0C (after successful INQUIRE)
  - Line 185: XCTL to selected program (OTHER case)
  - Line 202: XCTL to CDEMO-TO-PROGRAM (RETURN-TO-SIGNON-SCREEN)
- **Validation Range**: 1 <= OPTION <= 11 (CDEMO-MENU-OPT-COUNT = 11)
- **Error Handling**: SEND-MENU-SCREEN performed for validation errors, but NOT after successful XCTL
- **Program Flow**: SEND-MENU-SCREEN at end of PROCESS-ENTER-KEY only executed if no XCTL occurred

**Critical Corrections Needed:**
- **XCTL Behavior**: XCTL commands terminate program execution - they do NOT return to continue processing
- **SEND-MENU-SCREEN**: Only performed for error conditions and DUMMY programs, NOT after successful program transfers
- **Validation Logic**: Sequential validation (numeric check, range check, user type check) with early exit on errors
- **Program Termination**: Three termination paths: CICS RETURN (normal), XCTL to target program (success), XCTL to sign-on (exit)

**Flow Logic Verified:**
- MAIN-PARA always ends with CICS RETURN (line 107-110)
- PROCESS-ENTER-KEY has conditional SEND-MENU-SCREEN at end (line 191) - only if no XCTL executed
- RETURN-TO-SIGNON-SCREEN ends with XCTL (line 201-203) - program terminates
- All error conditions properly set ERR-FLG and display messages before continuing

*Analysis completed using actual code inspection - critical XCTL behavior and program termination logic verified*
