# Function Analysis for COSGN00C

## Program Overview
**Program**: COSGN00C  
**Type**: CICS COBOL Program  
**Function**: Signon Screen for CardDemo Application  
**Transaction**: CC00  
**Analysis Date**: 2025-12-24  

## Function Summary
| Function Name | Type | CICS Operations | Purpose | Complexity |
|---------------|------|-----------------|---------|------------|
| MAIN-PARA | PARAGRAPH | RETURN | Main program entry point and flow control | MEDIUM |
| PROCESS-ENTER-KEY | PARAGRAPH | RECEIVE | Process user input and validate credentials | HIGH |
| SEND-SIGNON-SCREEN | PARAGRAPH | SEND | Display signon screen with messages | MEDIUM |
| SEND-PLAIN-TEXT | PARAGRAPH | SEND TEXT, RETURN | Display exit message and terminate | LOW |
| POPULATE-HEADER-INFO | PARAGRAPH | ASSIGN | Populate screen header with system information | MEDIUM |
| READ-USER-SEC-FILE | PARAGRAPH | READ, XCTL | Authenticate user and transfer to next program | HIGH |

## Program Flow Diagram
```
MAIN-PARA (Entry Point)
├── SEND-SIGNON-SCREEN (Initial Screen Display)
│   └── POPULATE-HEADER-INFO (Header Information)
├── PROCESS-ENTER-KEY (User Input Processing)
│   ├── SEND-SIGNON-SCREEN (Error Display)
│   └── READ-USER-SEC-FILE (Authentication)
│       ├── SEND-SIGNON-SCREEN (Auth Failure)
│       └── XCTL to COADM01C/COMEN01C (Success)
└── SEND-PLAIN-TEXT (PF3 Exit)
```

## Detailed Function Documentation

### Function: MAIN-PARA
- **Location**: Line 73 - 102
- **Type**: PARAGRAPH
- **CICS Operations**: 
  - EXEC CICS RETURN: Return to CICS with transaction ID and communication area
- **Description**: Main program entry point that controls the overall flow based on communication area length and user input (EIBAID). Handles initial screen display, user interaction processing, and program termination.
- **Function Flow**: 
  1. Initialize error flag to OFF
  2. Clear message fields
  3. Check if first time entry (EIBCALEN = 0)
  4. If first time, display initial signon screen
  5. If returning, evaluate attention identifier (EIBAID)
  6. Process ENTER key, PF3 key, or invalid key
  7. Return to CICS with transaction continuation
- **Input Variables**: 
  - EIBCALEN: Communication area length from CICS
  - EIBAID: Attention identifier from CICS
  - CCDA-MSG-THANK-YOU: Thank you message from COTTL01Y
  - CCDA-MSG-INVALID-KEY: Invalid key message from CSMSG01Y
- **Output Variables**: 
  - WS-ERR-FLG: Error flag set to 'Y' for invalid keys
  - WS-MESSAGE: Message text for display
  - ERRMSGO: Error message output field
  - USERIDL: User ID field length for cursor positioning
- **BMS Maps Used**: 
  - COSGN0A: Initial screen setup (LOW-VALUES)
- **Files Accessed**: None
- **Programs Called**: None (returns to CICS)
- **Called By**: CICS Transaction Manager (CC00)
- **Error Handling**: Sets error flag and displays appropriate message for invalid attention identifiers

### Function: PROCESS-ENTER-KEY
- **Location**: Line 108 - 140
- **Type**: PARAGRAPH
- **CICS Operations**: 
  - EXEC CICS RECEIVE: Receive data from BMS map
- **Description**: Processes ENTER key input by receiving screen data, validating required fields (User ID and Password), converting to uppercase, and initiating authentication if validation passes.
- **Function Flow**: 
  1. Receive map data from COSGN0A
  2. Validate User ID field (not spaces or low-values)
  3. Validate Password field (not spaces or low-values)
  4. Convert both fields to uppercase
  5. Populate communication area with user ID
  6. If no validation errors, proceed to authentication
- **Input Variables**: 
  - COSGN0AI: Input map structure
  - USERIDI: User ID input field
  - PASSWDI: Password input field
- **Output Variables**: 
  - WS-RESP-CD: CICS response code from RECEIVE
  - WS-REAS-CD: CICS reason code from RECEIVE
  - WS-ERR-FLG: Error flag for validation failures
  - WS-MESSAGE: Validation error messages
  - WS-USER-ID: Uppercase user ID
  - WS-USER-PWD: Uppercase password
  - CDEMO-USER-ID: User ID in communication area
  - USERIDL/PASSWDL: Field lengths for cursor positioning
- **BMS Maps Used**: 
  - COSGN0A: RECEIVE operation for input data
- **Files Accessed**: None
- **Programs Called**: 
  - READ-USER-SEC-FILE: PERFORM for authentication
  - SEND-SIGNON-SCREEN: PERFORM for validation errors
- **Called By**: 
  - MAIN-PARA: PERFORM when EIBAID = DFHENTER
- **Error Handling**: Validates required fields and displays specific error messages with cursor positioning

### Function: SEND-SIGNON-SCREEN
- **Location**: Line 145 - 157
- **Type**: PARAGRAPH
- **CICS Operations**: 
  - EXEC CICS SEND: Send BMS map to terminal
- **Description**: Displays the signon screen with populated header information and any error messages. Erases the screen and positions cursor appropriately.
- **Function Flow**: 
  1. Populate header information (date, time, system info)
  2. Move message text to error message output field
  3. Send map to terminal with erase and cursor options
- **Input Variables**: 
  - WS-MESSAGE: Message text to display
  - COSGN0AO: Output map structure
- **Output Variables**: 
  - ERRMSGO: Error message output field
- **BMS Maps Used**: 
  - COSGN0A: SEND operation with FROM(COSGN0AO)
- **Files Accessed**: None
- **Programs Called**: 
  - POPULATE-HEADER-INFO: PERFORM to set header fields
- **Called By**: 
  - MAIN-PARA: PERFORM for initial display and invalid key
  - PROCESS-ENTER-KEY: PERFORM for validation errors
  - READ-USER-SEC-FILE: PERFORM for authentication errors
- **Error Handling**: Displays error messages and positions cursor on appropriate field

### Function: SEND-PLAIN-TEXT
- **Location**: Line 162 - 172
- **Type**: PARAGRAPH
- **CICS Operations**: 
  - EXEC CICS SEND TEXT: Send text message to terminal
  - EXEC CICS RETURN: Return to CICS without transaction continuation
- **Description**: Displays a plain text message (typically thank you message) and terminates the program without continuing the transaction. Used for PF3 exit processing.
- **Function Flow**: 
  1. Send text message to terminal with erase and free keyboard
  2. Return to CICS without TRANSID (terminates conversation)
- **Input Variables**: 
  - WS-MESSAGE: Text message to display (CCDA-MSG-THANK-YOU)
- **Output Variables**: None
- **BMS Maps Used**: None (uses SEND TEXT)
- **Files Accessed**: None
- **Programs Called**: None (terminates program)
- **Called By**: 
  - MAIN-PARA: PERFORM when EIBAID = DFHPF3
- **Error Handling**: None (simple termination function)

### Function: POPULATE-HEADER-INFO
- **Location**: Line 177 - 204
- **Type**: PARAGRAPH
- **CICS Operations**: 
  - EXEC CICS ASSIGN: Get application ID and system ID
- **Description**: Populates the screen header with current date, time, program information, and system identifiers. Formats date and time for display and retrieves CICS system information.
- **Function Flow**: 
  1. Get current date and time using CURRENT-DATE function
  2. Move title constants to output fields
  3. Move program and transaction identifiers
  4. Format date as MM/DD/YY
  5. Format time as HH:MM:SS
  6. Get application ID and system ID from CICS
- **Input Variables**: 
  - CCDA-TITLE01: Main title from COTTL01Y
  - CCDA-TITLE02: Application title from COTTL01Y
  - WS-TRANID: Transaction ID ('CC00')
  - WS-PGMNAME: Program name ('COSGN00C')
  - WS-CURDATE-DATA: Current date/time structure
- **Output Variables**: 
  - TITLE01O: Title 1 output field
  - TITLE02O: Title 2 output field
  - TRNNAMEO: Transaction name output
  - PGMNAMEO: Program name output
  - CURDATEO: Formatted current date
  - CURTIMEO: Formatted current time
  - APPLIDO: Application ID from CICS
  - SYSIDO: System ID from CICS
  - WS-CURDATE-MM/DD/YY: Formatted date components
  - WS-CURTIME-HH/MM/SS: Formatted time components
- **BMS Maps Used**: 
  - COSGN0A: Output fields populated
- **Files Accessed**: None
- **Programs Called**: None
- **Called By**: 
  - SEND-SIGNON-SCREEN: PERFORM to populate header before display
- **Error Handling**: None (utility function with no error conditions)

### Function: READ-USER-SEC-FILE
- **Location**: Line 209 - 257
- **Type**: PARAGRAPH
- **CICS Operations**: 
  - EXEC CICS READ: Read user security record from VSAM file
  - EXEC CICS XCTL: Transfer control to target program
- **Description**: Authenticates user by reading security file, validating password, and transferring control to appropriate menu program based on user type. Handles authentication errors with appropriate messages.
- **Function Flow**: 
  1. Read USRSEC file using user ID as key
  2. Evaluate CICS response code
  3. If successful read, compare passwords
  4. If password matches, populate communication area
  5. Transfer to admin menu (COADM01C) or user menu (COMEN01C)
  6. Handle authentication failures with error messages
- **Input Variables**: 
  - WS-USRSEC-FILE: VSAM file name ('USRSEC  ')
  - WS-USER-ID: User ID for file key
  - WS-USER-PWD: Password for validation
  - SEC-USER-DATA: User security record structure
  - WS-TRANID: Source transaction ID
  - WS-PGMNAME: Source program name
- **Output Variables**: 
  - WS-RESP-CD: CICS response code from READ
  - WS-REAS-CD: CICS reason code from READ
  - SEC-USR-PWD: Password from file record
  - SEC-USR-TYPE: User type from file record
  - CDEMO-FROM-TRANID: Source transaction in communication area
  - CDEMO-FROM-PROGRAM: Source program in communication area
  - CDEMO-USER-ID: User ID in communication area
  - CDEMO-USER-TYPE: User type in communication area
  - CDEMO-PGM-CONTEXT: Program context (set to ZEROS)
  - WS-ERR-FLG: Error flag for failures
  - WS-MESSAGE: Error messages
  - USERIDL/PASSWDL: Field lengths for cursor positioning
- **BMS Maps Used**: None directly (calls SEND-SIGNON-SCREEN for errors)
- **Files Accessed**: 
  - USRSEC: READ operation with user ID key
- **Programs Called**: 
  - COADM01C: XCTL for admin users (CDEMO-USRTYP-ADMIN)
  - COMEN01C: XCTL for regular users
  - SEND-SIGNON-SCREEN: PERFORM for authentication errors
- **Called By**: 
  - PROCESS-ENTER-KEY: PERFORM after successful input validation
- **Error Handling**: Comprehensive error handling for file not found (RESP=13), wrong password, and general file access errors with specific user messages

## Function Relationships Matrix
| Function | Calls | Called By | CICS Operations | Complexity Factors |
|----------|-------|-----------|-----------------|-------------------|
| MAIN-PARA | SEND-SIGNON-SCREEN, PROCESS-ENTER-KEY, SEND-PLAIN-TEXT | CICS (CC00) | RETURN | EIBAID evaluation, flow control |
| PROCESS-ENTER-KEY | SEND-SIGNON-SCREEN, READ-USER-SEC-FILE | MAIN-PARA | RECEIVE | Input validation, data conversion |
| SEND-SIGNON-SCREEN | POPULATE-HEADER-INFO | MAIN-PARA, PROCESS-ENTER-KEY, READ-USER-SEC-FILE | SEND | Screen display coordination |
| SEND-PLAIN-TEXT | None | MAIN-PARA | SEND TEXT, RETURN | Simple termination |
| POPULATE-HEADER-INFO | None | SEND-SIGNON-SCREEN | ASSIGN | Date/time formatting, system info |
| READ-USER-SEC-FILE | SEND-SIGNON-SCREEN | PROCESS-ENTER-KEY | READ, XCTL | Authentication logic, program routing |

## CICS Resource Usage Summary
| Resource Type | Resource Name | Operations | Purpose |
|---------------|---------------|------------|---------|
| BMS Map | COSGN0A | SEND, RECEIVE | User interface for signon |
| BMS Mapset | COSGN00 | SEND, RECEIVE | Signon screen mapset |
| VSAM File | USRSEC | READ | User authentication data |
| Program | COADM01C | XCTL | Admin menu program |
| Program | COMEN01C | XCTL | User menu program |
| Transaction | CC00 | RETURN | Signon transaction |

## Error Handling Summary
| Error Condition | Detection Method | Response | User Message |
|-----------------|------------------|----------|--------------|
| Missing User ID | Field validation | Redisplay screen | "Please enter User ID ..." |
| Missing Password | Field validation | Redisplay screen | "Please enter Password ..." |
| Invalid Key | EIBAID evaluation | Redisplay screen | CCDA-MSG-INVALID-KEY |
| User Not Found | CICS RESP = 13 | Redisplay screen | "User not found. Try again ..." |
| Wrong Password | Password comparison | Redisplay screen | "Wrong Password. Try again ..." |
| File Access Error | CICS RESP other | Redisplay screen | "Unable to verify the User ..." |

## Self-Check Validation

### Completeness Check
- [x] MAIN-PARA entry point identified and documented
- [x] All 6 PARAGRAPH definitions identified (no SECTION usage)
- [x] All PERFORM statements traced to target paragraphs
- [x] All EXEC CICS XCTL statements documented (COADM01C, COMEN01C)
- [x] All EXEC CICS commands categorized (SEND, RECEIVE, READ, RETURN, ASSIGN, XCTL)
- [x] Function hierarchy and PERFORM relationships mapped completely

### CICS-Specific Validation
- [x] BMS SEND/RECEIVE operations documented with map names (COSGN0A/COSGN00)
- [x] File I/O operations documented with dataset name (USRSEC)
- [x] EIBAID processing logic identified (DFHENTER, DFHPF3, OTHER)
- [x] COMMAREA usage and structure documented (CARDDEMO-COMMAREA)
- [x] Transaction control documented (RETURN TRANSID vs plain RETURN)
- [x] Error handling via RESP/RESP2 codes documented

### Accuracy Check
- [x] Paragraph names match exactly with source code (case-sensitive)
- [x] Line numbers are accurate and verified (73, 108, 145, 162, 177, 209)
- [x] CICS command syntax correctly documented
- [x] Variable names from BMS maps correctly identified (I/O suffixes)
- [x] PERFORM relationships verified in both directions
- [x] EVALUATE/IF logic flow accurately represented

### Quality Check
- [x] Function descriptions reflect CICS business context (authentication flow)
- [x] No duplicate paragraph entries
- [x] Screen flow logic clearly documented (signon → validation → authentication)
- [x] User interaction patterns identified (ENTER, PF3, invalid keys)
- [x] Data validation functions properly categorized
- [x] Program transfer logic (XCTL) clearly explained with user type routing

### Anti-Hallucination Measures
- [x] Every paragraph name verified against source code
- [x] All line references double-checked (73, 108, 145, 162, 177, 209)
- [x] CICS command parameters confirmed in actual code
- [x] BMS map names verified (COSGN0A, COSGN00)
- [x] File names confirmed (USRSEC from WS-USRSEC-FILE)
- [x] Variable names confirmed from actual source
- [x] No assumptions made about CICS behavior beyond code evidence
- [x] Function flow steps match actual COBOL paragraph sequence
- [x] EIBAID values match DFHAID copybook constants (DFHENTER, DFHPF3)

### COBOL/CICS Integration Check
- [x] Copybook inclusions properly documented (COCOM01Y, COSGN00, etc.)
- [x] System copybooks usage identified (DFHAID, DFHBMSCA)
- [x] Working storage variables linked to function usage
- [x] Condition names usage documented (ERR-FLG-ON/OFF, CDEMO-USRTYP-ADMIN)
- [x] MOVE statements and data flow documented
- [x] EVALUATE statement logic properly mapped

### Final Validation
- [x] Output format follows template structure exactly
- [x] All placeholders replaced with actual values
- [x] Cross-references between paragraphs validated
- [x] CICS program flow documentation complete
- [x] Ready for technical review and validation
- [x] File name corrected: Document is COSGN00C_FunctionList.md (not COSGN00C_Functions.md)
