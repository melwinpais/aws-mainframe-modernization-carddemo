# COACTUPC Program Flow Analysis

## Program Overview
- **Program**: COACTUPC.cbl
- **Function**: Accept and process ACCOUNT UPDATE
- **Transaction ID**: CAUP
- **Type**: CICS Online Transaction Processing

## High-Level Program Flow Diagram

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           COACTUPC PROGRAM FLOW                            │
└─────────────────────────────────────────────────────────────────────────────┘

                                    START
                                    │
                                    ▼
                            ┌─────────────┐
                            │ 0000-MAIN   │◄─── Entry Point (CAUP Transaction)
                            └─────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ Initialize Program      │
                        │ - HANDLE ABEND          │
                        │ - Process COMMAREA      │
                        │ - Store PF Keys         │
                        └─────────────────────────┘
                                    │
                                    ▼
                            ┌─────────────┐
                            │ AID Key?    │
                            └─────────────┘
                                │     │
                            PF03│     │ENTER/PF05/PF12
                                ▼     ▼
                    ┌─────────────────┐   ┌─────────────────┐
                    │ SYNCPOINT       │   │ Program State   │
                    │ XCTL to Menu    │   │ Evaluation      │
                    └─────────────────┘   └─────────────────┘
                            │                     │
                            ▼                     ▼
                        TERMINATE           ┌─────────────┐
                                           │ EVALUATE    │
                                           │ TRUE        │
                                           └─────────────┘
                                                   │
                    ┌──────────────────────────────┼──────────────────────────────┐
                    │                              │                              │
                    ▼                              ▼                              ▼
        ┌─────────────────────┐        ┌─────────────────────┐        ┌─────────────────────┐
        │ Fresh Entry         │        │ Input Processing    │        │ Update Complete     │
        │ (ACUP-DETAILS-NOT-  │        │ (OTHER condition)   │        │ (ACUP-CHANGES-      │
        │ FETCHED AND         │        │                     │        │ OKAYED-AND-DONE     │
        │ CDEMO-PGM-ENTER)    │        │                     │        │ OR ACUP-CHANGES-    │
        │                     │        │                     │        │ FAILED)             │
        └─────────────────────┘        └─────────────────────┘        └─────────────────────┘
                    │                              │                              │
                    ▼                              ▼                              ▼
        ┌─────────────────────┐        ┌─────────────────────┐        ┌─────────────────────┐
        │ 3000-SEND-MAP       │        │ 1000-PROCESS-INPUTS │        │ INITIALIZE          │
        │ GO TO COMMON-RETURN │        │ 2000-DECIDE-ACTION  │        │ 3000-SEND-MAP       │
        └─────────────────────┘        │ 3000-SEND-MAP       │        │ GO TO COMMON-RETURN │
                    │                  │ GO TO COMMON-RETURN │        └─────────────────────┘
                    ▼                  └─────────────────────┘                    │
                RETURN                             │                              ▼
                                                   ▼                          RETURN
                                                RETURN
```

## Detailed Function Call Flow

### 1. 0000-MAIN Function Call Relationships

```
0000-MAIN
├── Always Calls:
│   └── YYYY-STORE-PFKEY (PF key processing)
├── Conditional Calls:
│   ├── 3000-SEND-MAP (when fresh entry or update complete)
│   ├── 1000-PROCESS-INPUTS (when input processing required)
│   └── 2000-DECIDE-ACTION (when input validation successful)
└── Termination:
    ├── XCTL to CDEMO-TO-PROGRAM (PF03 exit)
    └── RETURN with TRANSID (continue transaction)
```

**Call Conditions:**
- `0000-MAIN` calls `3000-SEND-MAP` when `ACUP-DETAILS-NOT-FETCHED AND CDEMO-PGM-ENTER` OR `CDEMO-FROM-PROGRAM EQUAL LIT-MENUPGM AND NOT CDEMO-PGM-REENTER` OR `ACUP-CHANGES-OKAYED-AND-DONE` OR `ACUP-CHANGES-FAILED`
- `0000-MAIN` calls `1000-PROCESS-INPUTS` when `OTHER` condition (after PF03 and fresh entry conditions)
- `0000-MAIN` calls `2000-DECIDE-ACTION` when `OTHER` condition (after input processing)

### 2. 1000-PROCESS-INPUTS Function Flow

```
                            1000-PROCESS-INPUTS
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ 1100-RECEIVE-MAP        │
                        └─────────────────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ 1200-EDIT-MAP-INPUTS    │
                        │ - Account validation    │
                        │ - Field validations     │
                        │ - Data format checks    │
                        └─────────────────────────┘
                                    │
                    ┌─────────────────┼─────────────────┐
                    ▼                 ▼                 ▼
        ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
        │ Validation      │ │ Validation      │ │ All Validations │
        │ Errors Found    │ │ Warnings        │ │ Passed          │
        └─────────────────┘ └─────────────────┘ └─────────────────┘
                    │                 │                 │
                    └─────────────────┼─────────────────┘
                                    │
                                    ▼
                                    RETURN
```

### 3. 2000-DECIDE-ACTION Function Flow

```
                            2000-DECIDE-ACTION
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ EVALUATE TRUE:          │
                        │ - ACUP-DETAILS-NOT-     │
                        │   FETCHED               │
                        │ - CCARD-AID-PFK12       │
                        │ - ACUP-SHOW-DETAILS     │
                        │ - ACUP-CHANGES-NOT-OK   │
                        │ - ACUP-CHANGES-OK-NOT-  │
                        │   CONFIRMED             │
                        │ - OTHER                 │
                        └─────────────────────────┘
                                    │
                    ┌─────────────────┼─────────────────┐
                    ▼                 ▼                 ▼
        ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
        │ 9000-READ-ACCT  │ │ 9600-WRITE-     │ │ CONTINUE or     │
        │ (when ACUP-     │ │ PROCESSING      │ │ ABEND-ROUTINE   │
        │ DETAILS-NOT-    │ │ (when ACUP-     │ │ (when OTHER)    │
        │ FETCHED or      │ │ CHANGES-OK-NOT- │ │                 │
        │ CCARD-AID-PFK12)│ │ CONFIRMED AND   │ │                 │
        │                 │ │ CCARD-AID-PFK05)│ │                 │
        └─────────────────┘ └─────────────────┘ └─────────────────┘
                    │                 │                 │
                    └─────────────────┼─────────────────┘
                                    │
                                    ▼
                                    RETURN
```

### 4. 9000-READ-ACCT Data Retrieval Flow

```
                            9000-READ-ACCT
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ 9200-GETCARDXREF-BYACCT │
                        │ READ CXACAIX            │
                        └─────────────────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ 9300-GETACCTDATA-BYACCT │
                        │ READ ACCTDAT            │
                        └─────────────────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ 9400-GETCUSTDATA-BYCUST │
                        │ READ CUSTDAT            │
                        └─────────────────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ 9500-STORE-FETCHED-DATA │
                        │ Store for comparison    │
                        └─────────────────────────┘
                                    │
                                    ▼
                                    RETURN
```

### 5. 9600-WRITE-PROCESSING Update Flow

```
                            9600-WRITE-PROCESSING
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ READ UPDATE ACCTDAT     │
                        │ (with account ID)       │
                        └─────────────────────────┘
                                    │
                            ┌─────────────┐
                            │ Success?    │
                            └─────────────┘
                                │     │
                            YES │     │ NO
                                ▼     ▼
                    ┌─────────────────┐   ┌─────────────────┐
                    │ READ UPDATE     │   │ Set COULD-NOT-  │
                    │ CUSTDAT         │   │ LOCK-ACCT-FOR-  │
                    │ (with cust ID)  │   │ UPDATE flag     │
                    └─────────────────┘   └─────────────────┘
                            │                     │
                            ▼                     │
                    ┌─────────────────┐           │
                    │ 9700-CHECK-     │           │
                    │ CHANGE-IN-REC   │           │
                    └─────────────────┘           │
                            │                     │
                    ┌─────────────┐               │
                    │ Changed?    │               │
                    └─────────────┘               │
                        │     │                   │
                    NO  │     │ YES               │
                        ▼     ▼                   │
            ┌─────────────────┐   ┌─────────────┐ │
            │ Update Records  │   │ Set DATA-   │ │
            │ REWRITE ACCTDAT │   │ WAS-CHANGED │ │
            │ REWRITE CUSTDAT │   │ flag        │ │
            │ SYNCPOINT       │   └─────────────┘ │
            └─────────────────┘           │       │
                    │                     │       │
                    └─────────────────────┼───────┘
                                        │
                                        ▼
                                        RETURN
```

## Function Interaction Matrix

| Function | Calls | Called By | Conditions |
|----------|-------|-----------|------------|
| 0000-MAIN | YYYY-STORE-PFKEY, 3000-SEND-MAP, 1000-PROCESS-INPUTS, 2000-DECIDE-ACTION | CICS Transaction | Entry point, EVALUATE TRUE conditions |
| 1000-PROCESS-INPUTS | 1100-RECEIVE-MAP, 1200-EDIT-MAP-INPUTS | 0000-MAIN | When OTHER condition in main EVALUATE |
| 1200-EDIT-MAP-INPUTS | 1210-EDIT-ACCOUNT, 1205-COMPARE-OLD-NEW, validation functions | 1000-PROCESS-INPUTS | Input validation required |
| 2000-DECIDE-ACTION | 9000-READ-ACCT, 9600-WRITE-PROCESSING | 0000-MAIN | When OTHER condition in main EVALUATE |
| 3000-SEND-MAP | 3100-SCREEN-INIT, 3200-SETUP-SCREEN-VARS, 3300-SETUP-SCREEN-ATTRS, 3400-SEND-SCREEN | 0000-MAIN | Multiple conditions: fresh entry, update complete, changes failed |
| 9000-READ-ACCT | 9200-GETCARDXREF-BYACCT, 9300-GETACCTDATA-BYACCT, 9400-GETCUSTDATA-BYCUST, 9500-STORE-FETCHED-DATA | 2000-DECIDE-ACTION | When ACUP-DETAILS-NOT-FETCHED or CCARD-AID-PFK12 |
| 9600-WRITE-PROCESSING | 9700-CHECK-CHANGE-IN-REC | 2000-DECIDE-ACTION | When ACUP-CHANGES-OK-NOT-CONFIRMED AND CCARD-AID-PFK05 |

## Error Handling Flow

```
Error Conditions → Error Messages → Screen Actions
    │                   │              │
    ▼                   ▼              ▼
┌─────────────┐  ┌─────────────────┐  ┌─────────────────┐
│ File Not    │  │ "ACCOUNT NOT    │  │ Redisplay Screen│
│ Found       │  │ FOUND"          │  │ with Error      │
└─────────────┘  └─────────────────┘  └─────────────────┘
┌─────────────┐  ┌─────────────────┐  ┌─────────────────┐
│ Validation  │  │ Field-specific  │  │ Highlight Field │
│ Error       │  │ Error Messages  │  │ Set Cursor      │
└─────────────┘  └─────────────────┘  └─────────────────┘
┌─────────────┐  ┌─────────────────┐  ┌─────────────────┐
│ Record      │  │ "RECORD CHANGED │  │ SYNCPOINT       │
│ Changed     │  │ BY ANOTHER USER"│  │ ROLLBACK        │
└─────────────┘  └─────────────────┘  └─────────────────┘
┌─────────────┐  ┌─────────────────┐  ┌─────────────────┐
│ ABEND       │  │ "TRANSACTION    │  │ ABEND with      │
│ Condition   │  │ ABENDED"        │  │ ABCODE          │
└─────────────┘  └─────────────────┘  └─────────────────┘
```

## Success Path Flow

```
Valid Input → Data Retrieval → Change Detection → Update Processing → Success Display
     │             │              │                 │                    │
     ▼             ▼              ▼                 ▼                    ▼
┌─────────────┐ ┌─────────────┐ ┌─────────────┐ ┌─────────────┐ ┌─────────────┐
│ All Fields  │ │ CXACAIX →   │ │ Compare     │ │ READ UPDATE │ │ Success     │
│ Validated   │ │ ACCTDAT →   │ │ Original    │ │ REWRITE     │ │ Message     │
│             │ │ CUSTDAT     │ │ vs Current  │ │ SYNCPOINT   │ │ Display     │
└─────────────┘ └─────────────┘ └─────────────┘ └─────────────┘ └─────────────┘
```

## Data Flow Through Functions

### Input Data Flow
```
BMS Screen Input → 1100-RECEIVE-MAP → 1200-EDIT-MAP-INPUTS → Working Storage Variables
```

### Output Data Flow
```
Working Storage → 3200-SETUP-SCREEN-VARS → BMS Map Fields → 3400-SEND-SCREEN
```

### Communication Area Flow
```
CARDDEMO-COMMAREA ← Data Population ← 0000-MAIN Initialization
```

## Program Termination Points

1. **PF03 Exit**: 
   - SYNCPOINT to commit any pending work
   - XCTL to CDEMO-TO-PROGRAM (calling program or main menu)
   - **CRITICAL**: Preserves transaction integrity before transfer

2. **Normal Return**:
   - RETURN with TRANSID(WS-TRANID) and COMMAREA
   - Continues pseudo-conversational transaction

3. **ABEND Termination**:
   - ABEND-ROUTINE handles abnormal termination
   - Sends error message and ABENDs with specific code

## Integration with External Programs

### Upstream Integration
- **Calling Programs**: Any program can XCTL to COACTUPC with COMMAREA
- **Main Menu (COMEN01C)**: Primary entry point via transaction CAUP

### Downstream Integration
- **CDEMO-TO-PROGRAM**: Dynamic XCTL target based on COMMAREA
- **No Direct Program Calls**: Uses XCTL for program-to-program communication

### External Resources
- **VSAM Files**: ACCTDAT (Account Master), CUSTDAT (Customer Master), CXACAIX (Card Cross-Reference)
- **BMS Map**: CACTUPA for screen I/O operations
- **CICS Resources**: Transaction CAUP, file definitions, and system services
