# Function Analysis for COMEN01C

**Program**: COMEN01C  
**Type**: CICS COBOL Program  
**Function**: Main Menu for Regular Users  
**Analysis Date**: 2025-12-24  

## Function Summary

| Function Name | Type | CICS Operations | Purpose | Complexity |
|---------------|------|-----------------|---------|------------|
| MAIN-PARA | PARAGRAPH | RETURN | Main program entry and control logic | HIGH |
| PROCESS-ENTER-KEY | PARAGRAPH | INQUIRE, XCTL | Menu option validation and program transfer | HIGH |
| RETURN-TO-SIGNON-SCREEN | PARAGRAPH | XCTL | Return to sign-on screen | LOW |
| SEND-MENU-SCREEN | PARAGRAPH | SEND MAP | Display menu screen to user | MEDIUM |
| RECEIVE-MENU-SCREEN | PARAGRAPH | RECEIVE MAP | Receive user input from screen | LOW |
| POPULATE-HEADER-INFO | PARAGRAPH | None | Populate screen header with date/time | MEDIUM |
| BUILD-MENU-OPTIONS | PARAGRAPH | None | Build menu option display text | MEDIUM |

## Program Flow Diagram

```
MAIN-PARA (Entry Point)
├── RETURN-TO-SIGNON-SCREEN (Program Transfer)
├── SEND-MENU-SCREEN (Screen Display)
│   ├── POPULATE-HEADER-INFO (Header Setup)
│   └── BUILD-MENU-OPTIONS (Menu Setup)
├── RECEIVE-MENU-SCREEN (User Input)
└── PROCESS-ENTER-KEY (Input Processing)
    ├── SEND-MENU-SCREEN (Error Display)
    └── RETURN-TO-SIGNON-SCREEN (Exit)
```

## Detailed Function Documentation

### Function: MAIN-PARA
- **Location**: Line 75 - 113
- **Type**: PARAGRAPH
- **CICS Operations**: 
  - EXEC CICS RETURN: Return control with transaction ID and communication area
- **Description**: Main program entry point that controls the overall program flow based on communication area length and program context
- **Function Flow**: 
  1. Initialize error flag to OFF
  2. Clear message fields
  3. Check if first entry (EIBCALEN = 0) and return to sign-on
  4. Move communication area and check program context
  5. Handle first entry vs re-entry logic
  6. Process user attention identifiers (ENTER, PF3, OTHER)
  7. Return to CICS with transaction ID
- **Input Variables**: 
  - EIBCALEN: Communication area length from CICS
  - DFHCOMMAREA: CICS communication area
  - EIBAID: Attention identifier from user input
- **Output Variables**: 
  - CARDDEMO-COMMAREA: Application communication area
  - CDEMO-PGM-REENTER: Program re-entry flag
  - WS-ERR-FLG: Error flag indicator
- **BMS Maps Used**: 
  - COMEN1AO: Output map for error message clearing
- **Files Accessed**: None
- **Programs Called**: 
  - COSGN00C: XCTL - Return to sign-on screen
- **Called By**: CICS Transaction Manager (Entry Point)
- **Error Handling**: Sets error flag and displays invalid key message

### Function: PROCESS-ENTER-KEY
- **Location**: Line 115 - 194
- **Type**: PARAGRAPH
- **CICS Operations**: 
  - EXEC CICS INQUIRE: Check program availability (COPAUS0C only)
  - EXEC CICS XCTL: Transfer control to selected program
- **Description**: Processes user menu selection, validates input, and transfers control to appropriate target program
- **Function Flow**: 
  1. Trim trailing spaces from user input
  2. Convert input to numeric option
  3. Validate option range (1-11) and numeric format
  4. Check user type authorization for admin options
  5. Handle special cases (COPAUS0C availability, DUMMY programs)
  6. Set up communication area for target program
  7. Transfer control to selected program
- **Input Variables**: 
  - OPTIONI: User menu selection from BMS input
  - CDEMO-MENU-OPT-COUNT: Number of available menu options
  - CDEMO-USRTYP-USER: User type condition name
- **Output Variables**: 
  - WS-OPTION: Numeric menu option
  - WS-OPTION-X: Character menu option
  - OPTIONO: Menu option echo to screen
  - CDEMO-FROM-TRANID: Source transaction ID
  - CDEMO-FROM-PROGRAM: Source program name
- **BMS Maps Used**: 
  - COMEN1AI: Input map for option selection
  - COMEN1AO: Output map for option echo and error messages
- **Files Accessed**: None
- **Programs Called**: 
  - Dynamic program call via CDEMO-MENU-OPT-PGMNAME: XCTL - Transfer to selected application function
- **Called By**: 
  - MAIN-PARA: PERFORM when DFHENTER pressed
- **Error Handling**: Validates input range, checks program availability, displays error messages

### Function: RETURN-TO-SIGNON-SCREEN
- **Location**: Line 196 - 206
- **Type**: PARAGRAPH
- **CICS Operations**: 
  - EXEC CICS XCTL: Transfer control to sign-on program
- **Description**: Transfers control back to the sign-on screen program
- **Function Flow**: 
  1. Check if target program is specified
  2. Default to COSGN00C if not specified
  3. Transfer control to target program
- **Input Variables**: 
  - CDEMO-TO-PROGRAM: Target program name from communication area
- **Output Variables**: 
  - CDEMO-TO-PROGRAM: Updated with default program if needed
- **BMS Maps Used**: None
- **Files Accessed**: None
- **Programs Called**: 
  - CDEMO-TO-PROGRAM (typically COSGN00C): XCTL - Return to sign-on screen
- **Called By**: 
  - MAIN-PARA: PERFORM when EIBCALEN = 0 or DFHPF3 pressed
  - PROCESS-ENTER-KEY: PERFORM for program exit
- **Error Handling**: Provides default program if none specified

### Function: SEND-MENU-SCREEN
- **Location**: Line 208 - 223
- **Type**: PARAGRAPH
- **CICS Operations**: 
  - EXEC CICS SEND MAP: Send formatted screen to terminal
- **Description**: Sends the menu screen to the user terminal with populated header and menu options
- **Function Flow**: 
  1. Populate header information (date, time, titles)
  2. Build menu option display text
  3. Move error message to output area
  4. Send formatted map to terminal
- **Input Variables**: 
  - WS-MESSAGE: Error or status message
- **Output Variables**: 
  - ERRMSGO: Error message output field
- **BMS Maps Used**: 
  - COMEN1A: Map name for screen display
  - COMEN01: Mapset name
  - COMEN1AO: Output map structure
- **Files Accessed**: None
- **Programs Called**: None
- **Called By**: 
  - MAIN-PARA: PERFORM for initial display and error conditions
  - PROCESS-ENTER-KEY: PERFORM for validation errors and program completion
- **Error Handling**: Displays error messages in ERRMSGO field

### Function: RECEIVE-MENU-SCREEN
- **Location**: Line 225 - 234
- **Type**: PARAGRAPH
- **CICS Operations**: 
  - EXEC CICS RECEIVE MAP: Receive user input from terminal
- **Description**: Receives user input from the menu screen
- **Function Flow**: 
  1. Receive map data from terminal
  2. Store response codes for error checking
- **Input Variables**: None (receives from terminal)
- **Output Variables**: 
  - COMEN1AI: Input map structure with user data
  - WS-RESP-CD: CICS response code
  - WS-REAS-CD: CICS reason code
- **BMS Maps Used**: 
  - COMEN1A: Map name for screen input
  - COMEN01: Mapset name
  - COMEN1AI: Input map structure
- **Files Accessed**: None
- **Programs Called**: None
- **Called By**: 
  - MAIN-PARA: PERFORM for user input processing
- **Error Handling**: Captures RESP and RESP2 codes for error analysis

### Function: POPULATE-HEADER-INFO
- **Location**: Line 238 - 258
- **Type**: PARAGRAPH
- **CICS Operations**: None
- **Description**: Populates screen header fields with current date, time, program name, and titles
- **Function Flow**: 
  1. Get current date and time using FUNCTION CURRENT-DATE
  2. Move title constants to output fields
  3. Move program and transaction identifiers
  4. Format date as MM/DD/YY
  5. Format time as HH:MM:SS
  6. Move formatted values to screen output fields
- **Input Variables**: 
  - CCDA-TITLE01: Primary screen title
  - CCDA-TITLE02: Secondary screen title
  - WS-TRANID: Transaction identifier
  - WS-PGMNAME: Program identifier
- **Output Variables**: 
  - TITLE01O: Primary title output
  - TITLE02O: Secondary title output
  - TRNNAMEO: Transaction name output
  - PGMNAMEO: Program name output
  - CURDATEO: Current date output
  - CURTIMEO: Current time output
- **BMS Maps Used**: 
  - COMEN1AO: Output map for header fields
- **Files Accessed**: None
- **Programs Called**: None
- **Called By**: 
  - SEND-MENU-SCREEN: PERFORM for screen header setup
- **Error Handling**: None (straightforward data movement)

### Function: BUILD-MENU-OPTIONS
- **Location**: Line 262 - 303
- **Type**: PARAGRAPH
- **CICS Operations**: None
- **Description**: Builds formatted menu option text for display on screen
- **Function Flow**: 
  1. Loop through all menu options (1 to CDEMO-MENU-OPT-COUNT)
  2. For each option, build formatted text with number and description
  3. Use EVALUATE to move text to appropriate screen field (OPTN001O-OPTN012O)
  4. Continue until all options processed
- **Input Variables**: 
  - CDEMO-MENU-OPT-COUNT: Number of menu options
  - CDEMO-MENU-OPT-NUM: Option numbers from menu data
  - CDEMO-MENU-OPT-NAME: Option descriptions from menu data
- **Output Variables**: 
  - WS-MENU-OPT-TXT: Temporary formatted option text
  - OPTN001O through OPTN012O: Screen output fields for menu options
- **BMS Maps Used**: 
  - COMEN1AO: Output map for menu option fields
- **Files Accessed**: None
- **Programs Called**: None
- **Called By**: 
  - SEND-MENU-SCREEN: PERFORM for menu option display setup
- **Error Handling**: None (straightforward data formatting)

## CICS Command Summary

| Command | Usage Count | Purpose |
|---------|-------------|---------|
| EXEC CICS RETURN | 1 | Return control to CICS with transaction ID |
| EXEC CICS XCTL | 3 | Transfer control to other programs |
| EXEC CICS SEND MAP | 1 | Send formatted screen to terminal |
| EXEC CICS RECEIVE MAP | 1 | Receive user input from terminal |
| EXEC CICS INQUIRE | 1 | Check program availability |

## Program Integration Points

### Entry Points
- **CICS Transaction CM00**: Invokes MAIN-PARA as program entry point

### Exit Points
- **COSGN00C**: Sign-on screen (via RETURN-TO-SIGNON-SCREEN)
- **COACTVWC**: Account View (via PROCESS-ENTER-KEY)
- **COACTUPC**: Account Update (via PROCESS-ENTER-KEY)
- **COCRDLIC**: Credit Card List (via PROCESS-ENTER-KEY)
- **COCRDSLC**: Credit Card View (via PROCESS-ENTER-KEY)
- **COCRDUPC**: Credit Card Update (via PROCESS-ENTER-KEY)
- **COTRN00C**: Transaction List (via PROCESS-ENTER-KEY)
- **COTRN01C**: Transaction View (via PROCESS-ENTER-KEY)
- **COTRN02C**: Transaction Add (via PROCESS-ENTER-KEY)
- **CORPT00C**: Transaction Reports (via PROCESS-ENTER-KEY)
- **COBIL00C**: Bill Payment (via PROCESS-ENTER-KEY)
- **COPAUS0C**: Pending Authorization View (via PROCESS-ENTER-KEY)

### Communication Areas
- **CARDDEMO-COMMAREA**: Inter-program data sharing structure
- **DFHCOMMAREA**: CICS standard communication area

## Error Handling Strategy

- **Input Validation**: Range checking for menu options (1-11)
- **Program Availability**: INQUIRE command for COPAUS0C program
- **User Authorization**: Check for admin-only options (though none currently configured)
- **Default Handling**: Graceful defaults for missing program names
- **Error Display**: User-friendly error messages via ERRMSGO field

*Analysis completed using actual code inspection - all functions verified against source code with exact line numbers and CICS command usage*

## Verification Summary

**Code-Verified Facts:**
- **7 Paragraphs Total**: All paragraph names and line numbers verified against source code
- **POPULATE-HEADER-INFO**: Lines 238-258 (corrected)
- **BUILD-MENU-OPTIONS**: Lines 262-303 (corrected from 295)
- **CICS Commands**: 7 total commands verified (1 RETURN, 3 XCTL, 1 SEND, 1 RECEIVE, 1 INQUIRE)
- **Program Flow**: All PERFORM relationships verified in source code
- **Variable Usage**: All input/output variables confirmed in actual code
- **BMS Map Usage**: COMEN1A/COMEN01 mapset usage verified
- **Error Handling**: All error conditions and messages verified

**Corrections Made:**
- Fixed BUILD-MENU-OPTIONS end line from 295 to 303
- Verified all 7 CICS commands and their exact usage
- Confirmed all paragraph line number ranges
- Validated all variable names and BMS map references
- Verified program transfer targets and communication area usage
