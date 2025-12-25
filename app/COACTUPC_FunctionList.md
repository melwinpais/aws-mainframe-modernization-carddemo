# Function Analysis for COACTUPC

## Function Summary
| Function Name | Type | CICS Operations | Purpose | Complexity |
|---------------|------|-----------------|---------|------------|
| 0000-MAIN | PARAGRAPH | HANDLE ABEND, SYNCPOINT, XCTL, RETURN | Main program entry and control flow | HIGH |
| COMMON-RETURN | PARAGRAPH | RETURN | Program termination with commarea | MEDIUM |
| 1000-PROCESS-INPUTS | PARAGRAPH | None | Input processing coordinator | MEDIUM |
| 1100-RECEIVE-MAP | PARAGRAPH | RECEIVE MAP | Screen input processing | MEDIUM |
| 1200-EDIT-MAP-INPUTS | PARAGRAPH | None | Input validation coordinator | HIGH |
| 1205-COMPARE-OLD-NEW | PARAGRAPH | None | Data change detection | MEDIUM |
| 1210-EDIT-ACCOUNT | PARAGRAPH | None | Account ID validation | MEDIUM |
| 1215-EDIT-MANDATORY | PARAGRAPH | None | Mandatory field validation | LOW |
| 1220-EDIT-YESNO | PARAGRAPH | None | Yes/No field validation | LOW |
| 1225-EDIT-ALPHA-REQD | PARAGRAPH | None | Required alphabetic validation | MEDIUM |
| 1230-EDIT-ALPHANUM-REQD | PARAGRAPH | None | Required alphanumeric validation | MEDIUM |
| 1235-EDIT-ALPHA-OPT | PARAGRAPH | None | Optional alphabetic validation | MEDIUM |
| 1240-EDIT-ALPHANUM-OPT | PARAGRAPH | None | Optional alphanumeric validation | MEDIUM |
| 1245-EDIT-NUM-REQD | PARAGRAPH | None | Required numeric validation | MEDIUM |
| 1250-EDIT-SIGNED-9V2 | PARAGRAPH | None | Signed decimal validation | MEDIUM |
| 1260-EDIT-US-PHONE-NUM | PARAGRAPH | None | US phone number validation coordinator | HIGH |
| EDIT-AREA-CODE | PARAGRAPH | None | Phone area code validation | MEDIUM |
| EDIT-US-PHONE-PREFIX | PARAGRAPH | None | Phone prefix validation | MEDIUM |
| EDIT-US-PHONE-LINENUM | PARAGRAPH | None | Phone line number validation | MEDIUM |
| EDIT-US-PHONE-EXIT | PARAGRAPH | None | Phone validation exit point | LOW |
| 1265-EDIT-US-SSN | PARAGRAPH | None | US Social Security Number validation | HIGH |
| 1270-EDIT-US-STATE-CD | PARAGRAPH | None | US state code validation | MEDIUM |
| 1275-EDIT-FICO-SCORE | PARAGRAPH | None | FICO score range validation | LOW |
| 1280-EDIT-US-STATE-ZIP-CD | PARAGRAPH | None | State/ZIP code cross-validation | MEDIUM |
| 2000-DECIDE-ACTION | PARAGRAPH | None | Business logic decision controller | HIGH |
| 3000-SEND-MAP | PARAGRAPH | None | Screen display coordinator | MEDIUM |
| 3100-SCREEN-INIT | PARAGRAPH | None | Screen initialization | LOW |
| 3200-SETUP-SCREEN-VARS | PARAGRAPH | None | Screen variable setup coordinator | MEDIUM |
| 3201-SHOW-INITIAL-VALUES | PARAGRAPH | None | Display initial screen values | LOW |
| 3202-SHOW-ORIGINAL-VALUES | PARAGRAPH | None | Display original data values | MEDIUM |
| 3203-SHOW-UPDATED-VALUES | PARAGRAPH | None | Display updated data values | MEDIUM |
| 3250-SETUP-INFOMSG | PARAGRAPH | None | Information message setup | LOW |
| 3300-SETUP-SCREEN-ATTRS | PARAGRAPH | None | Screen attribute setup coordinator | HIGH |
| 3310-PROTECT-ALL-ATTRS | PARAGRAPH | None | Protect all screen fields | MEDIUM |
| 3320-UNPROTECT-FEW-ATTRS | PARAGRAPH | None | Unprotect editable fields | MEDIUM |
| 3390-SETUP-INFOMSG-ATTRS | PARAGRAPH | None | Information message attributes | LOW |
| 3400-SEND-SCREEN | PARAGRAPH | SEND MAP | Screen display execution | MEDIUM |
| 9000-READ-ACCT | PARAGRAPH | None | Account data retrieval coordinator | MEDIUM |
| 9200-GETCARDXREF-BYACCT | PARAGRAPH | READ | Card cross-reference file access | MEDIUM |
| 9300-GETACCTDATA-BYACCT | PARAGRAPH | READ | Account master file access | MEDIUM |
| 9400-GETCUSTDATA-BYCUST | PARAGRAPH | READ | Customer master file access | MEDIUM |
| 9500-STORE-FETCHED-DATA | PARAGRAPH | None | Store retrieved data for comparison | MEDIUM |
| 9600-WRITE-PROCESSING | PARAGRAPH | READ UPDATE, REWRITE, SYNCPOINT ROLLBACK | Database update processing | HIGH |
| 9700-CHECK-CHANGE-IN-REC | PARAGRAPH | None | Optimistic locking validation | HIGH |
| ABEND-ROUTINE | PARAGRAPH | SEND, HANDLE ABEND, ABEND | Error handling and program termination | MEDIUM |

*Note: EXIT paragraphs and copybook functions (YYYY-STORE-PFKEY, EDIT-DATE-CCYYMMDD) are performed but not defined in this program*

## Program Flow Diagram
```
0000-MAIN (Entry Point)
├── YYYY-STORE-PFKEY (PF Key Processing)
├── 3000-SEND-MAP (Initial Screen Display)
│   ├── 3100-SCREEN-INIT
│   ├── 3200-SETUP-SCREEN-VARS
│   │   ├── 3201-SHOW-INITIAL-VALUES
│   │   ├── 3202-SHOW-ORIGINAL-VALUES
│   │   └── 3203-SHOW-UPDATED-VALUES
│   ├── 3250-SETUP-INFOMSG
│   ├── 3300-SETUP-SCREEN-ATTRS
│   │   ├── 3310-PROTECT-ALL-ATTRS
│   │   └── 3320-UNPROTECT-FEW-ATTRS
│   ├── 3390-SETUP-INFOMSG-ATTRS
│   └── 3400-SEND-SCREEN
├── 1000-PROCESS-INPUTS (Input Processing)
│   ├── 1100-RECEIVE-MAP
│   └── 1200-EDIT-MAP-INPUTS
│       ├── 1210-EDIT-ACCOUNT
│       ├── 1205-COMPARE-OLD-NEW
│       ├── 1215-EDIT-MANDATORY
│       ├── 1220-EDIT-YESNO
│       ├── 1225-EDIT-ALPHA-REQD
│       ├── 1230-EDIT-ALPHANUM-REQD
│       ├── 1235-EDIT-ALPHA-OPT
│       ├── 1240-EDIT-ALPHANUM-OPT
│       ├── 1245-EDIT-NUM-REQD
│       ├── 1250-EDIT-SIGNED-9V2
│       ├── 1260-EDIT-US-PHONE-NUM
│       │   ├── EDIT-AREA-CODE
│       │   ├── EDIT-US-PHONE-PREFIX
│       │   ├── EDIT-US-PHONE-LINENUM
│       │   └── EDIT-US-PHONE-EXIT
│       ├── 1265-EDIT-US-SSN
│       ├── 1270-EDIT-US-STATE-CD
│       ├── 1275-EDIT-FICO-SCORE
│       ├── 1280-EDIT-US-STATE-ZIP-CD
│       └── EDIT-DATE-CCYYMMDD (from copybook)
├── 2000-DECIDE-ACTION (Business Logic)
│   ├── 9000-READ-ACCT (Data Retrieval)
│   │   ├── 9200-GETCARDXREF-BYACCT
│   │   ├── 9300-GETACCTDATA-BYACCT
│   │   ├── 9400-GETCUSTDATA-BYCUST
│   │   └── 9500-STORE-FETCHED-DATA
│   └── 9600-WRITE-PROCESSING (Data Update)
│       └── 9700-CHECK-CHANGE-IN-REC
└── COMMON-RETURN (Program Termination)

*Note: YYYY-STORE-PFKEY and EDIT-DATE-CCYYMMDD are performed from copybooks but not defined in this program*
```

## Detailed Function Documentation

### Function: 0000-MAIN
- **Location**: Line 859 - 1020
- **Type**: PARAGRAPH
- **CICS Operations**: 
  - EXEC CICS HANDLE ABEND: Set up error handling with ABEND-ROUTINE label
  - EXEC CICS SYNCPOINT: Commit transaction before program transfer
  - EXEC CICS XCTL: Transfer to calling program or main menu
  - EXEC CICS RETURN: Return with transaction continuation
- **Description**: Main program entry point that handles initialization, program flow control based on AID keys and program state, and termination
- **Function Flow**: 
  1. Set up abend handling with ABEND-ROUTINE label
  2. Initialize CC-WORK-AREA, WS-MISC-STORAGE, WS-COMMAREA
  3. Store transaction ID (LIT-THISTRANID) to WS-TRANID
  4. Process DFHCOMMAREA if passed from calling program
  5. Perform YYYY-STORE-PFKEY for PF key processing
  6. Validate AID keys (ENTER, PF03, PF05, PF12)
  7. Execute EVALUATE TRUE logic based on program state
  8. Handle PF03 exit with SYNCPOINT and XCTL
  9. Process fresh entry, completed updates, or input processing
- **Input Variables**: 
  - DFHCOMMAREA: Communication area from calling program
  - EIBCALEN: Length of communication area
  - EIBAID: Attention identifier from user input
  - CCARD-AID-*: AID key flags
- **Output Variables**: 
  - CARDDEMO-COMMAREA: Updated communication area
  - WS-THIS-PROGCOMMAREA: Program-specific communication data
  - WS-TRANID: Current transaction ID
- **BMS Maps Used**: None directly
- **Files Accessed**: None directly
- **Programs Called**: 
  - CDEMO-TO-PROGRAM: XCTL - Dynamic transfer to calling program or main menu
- **Called By**: CICS transaction CAUP
- **Error Handling**: HANDLE ABEND mechanism with ABEND-ROUTINE label

### Function: 1100-RECEIVE-MAP
- **Location**: Line 1039 - 1425
- **Type**: PARAGRAPH
- **CICS Operations**: 
  - EXEC CICS RECEIVE MAP: Receive user input from CACTUPA screen
- **Description**: Processes screen input from BMS map and populates program variables with field-level processing
- **Function Flow**: 
  1. Receive map data from CACTUPA screen using LIT-THISMAP and LIT-THISMAPSET
  2. Initialize ACUP-NEW-DETAILS structure
  3. Extract and validate account ID from ACCTSIDI field
  4. Process account master data fields (status, limits, dates) if details already fetched
  5. Process customer master data fields (names, address, phone, SSN, etc.)
  6. Handle special characters ('*' and SPACES) as null values
- **Input Variables**: 
  - CACTUPAI: BMS input map structure from screen
  - LIT-THISMAP: Map name ('CACTUPA')
  - LIT-THISMAPSET: Mapset name ('COACTUP ')
- **Output Variables**: 
  - ACUP-NEW-DETAILS: Updated account and customer data structure
  - CC-ACCT-ID: Account ID for processing
  - WS-RESP-CD, WS-REAS-CD: CICS response codes
- **BMS Maps Used**: 
  - CACTUPA: RECEIVE - Account update screen input
- **Files Accessed**: None
- **Programs Called**: None
- **Called By**: 1000-PROCESS-INPUTS
- **Error Handling**: RESP/RESP2 codes captured for RECEIVE MAP operation

### Function: 1200-EDIT-MAP-INPUTS
- **Location**: Line 1429 - 1677
- **Type**: PARAGRAPH
- **CICS Operations**: None
- **Description**: Comprehensive input validation coordinator for all screen fields
- **Function Flow**: 
  1. Validate search keys if details not yet fetched
  2. Compare old vs new data for change detection
  3. Validate account-specific fields
  4. Validate customer personal information
  5. Validate address and contact information
  6. Perform cross-field validation
  7. Set overall input status flags
- **Input Variables**: 
  - ACUP-NEW-DETAILS: User input data
  - ACUP-OLD-DETAILS: Original data for comparison
- **Output Variables**: 
  - Various validation flags (WS-NON-KEY-FLAGS)
  - INPUT-OK/INPUT-ERROR flags
- **BMS Maps Used**: None directly
- **Files Accessed**: None
- **Programs Called**: None
- **Called By**: 1000-PROCESS-INPUTS
- **Error Handling**: Sets validation flags and error messages

### Function: 9200-GETCARDXREF-BYACCT
- **Location**: Line 3650 - 3697
- **Type**: PARAGRAPH
- **CICS Operations**: 
  - EXEC CICS READ: Read card cross-reference file
- **Description**: Retrieves card cross-reference data using account ID as alternate index key
- **Function Flow**: 
  1. Read CXACAIX file using account ID
  2. Extract customer ID and card number from cross-reference
  3. Handle file not found conditions
  4. Set error flags and messages for failures
- **Input Variables**: 
  - WS-CARD-RID-ACCT-ID-X: Account ID for file access
- **Output Variables**: 
  - CARD-XREF-RECORD: Cross-reference record data
  - CDEMO-CUST-ID: Customer ID from cross-reference
  - CDEMO-CARD-NUM: Card number from cross-reference
- **BMS Maps Used**: None
- **Files Accessed**: 
  - CXACAIX: READ - Card cross-reference by account index
- **Programs Called**: None
- **Called By**: 9000-READ-ACCT
- **Error Handling**: RESP/RESP2 evaluation with error message construction

### Function: 9300-GETACCTDATA-BYACCT
- **Location**: Line 3701 - 3747
- **Type**: PARAGRAPH
- **CICS Operations**: 
  - EXEC CICS READ: Read account master file
- **Description**: Retrieves account master data using account ID as primary key
- **Function Flow**: 
  1. Read ACCTDAT file using account ID
  2. Set found flag for successful read
  3. Handle file not found conditions
  4. Set error flags and messages for failures
- **Input Variables**: 
  - WS-CARD-RID-ACCT-ID-X: Account ID for file access
- **Output Variables**: 
  - ACCOUNT-RECORD: Account master record data
  - FOUND-ACCT-IN-MASTER: Success flag
- **BMS Maps Used**: None
- **Files Accessed**: 
  - ACCTDAT: READ - Account master file
- **Programs Called**: None
- **Called By**: 9000-READ-ACCT
- **Error Handling**: RESP/RESP2 evaluation with error message construction

### Function: 9400-GETCUSTDATA-BYCUST
- **Location**: Line 3752 - 3796
- **Type**: PARAGRAPH
- **CICS Operations**: 
  - EXEC CICS READ: Read customer master file
- **Description**: Retrieves customer master data using customer ID as primary key
- **Function Flow**: 
  1. Read CUSTDAT file using customer ID
  2. Set found flag for successful read
  3. Handle file not found conditions
  4. Set error flags and messages for failures
- **Input Variables**: 
  - WS-CARD-RID-CUST-ID-X: Customer ID for file access
- **Output Variables**: 
  - CUSTOMER-RECORD: Customer master record data
  - FOUND-CUST-IN-MASTER: Success flag
- **BMS Maps Used**: None
- **Files Accessed**: 
  - CUSTDAT: READ - Customer master file
- **Programs Called**: None
- **Called By**: 9000-READ-ACCT
- **Error Handling**: RESP/RESP2 evaluation with error message construction

### Function: 9600-WRITE-PROCESSING
- **Location**: Line 3888 - 4104
- **Type**: PARAGRAPH
- **CICS Operations**: 
  - EXEC CICS READ UPDATE: Lock records for update
  - EXEC CICS REWRITE: Update account and customer records
  - EXEC CICS SYNCPOINT ROLLBACK: Rollback on failure
- **Description**: Performs database updates with optimistic locking and transaction control
- **Function Flow**: 
  1. Lock account record for update
  2. Lock customer record for update
  3. Check for concurrent changes using optimistic locking
  4. Prepare update records from new data
  5. Rewrite account record
  6. Rewrite customer record
  7. Handle update failures with rollback
- **Input Variables**: 
  - ACUP-NEW-DETAILS: Updated data to write
  - CC-ACCT-ID: Account ID for locking
  - CDEMO-CUST-ID: Customer ID for locking
- **Output Variables**: 
  - ACCT-UPDATE-RECORD: Formatted account update data
  - CUST-UPDATE-RECORD: Formatted customer update data
- **BMS Maps Used**: None
- **Files Accessed**: 
  - ACCTDAT: READ UPDATE, REWRITE - Account master file
  - CUSTDAT: READ UPDATE, REWRITE - Customer master file
- **Programs Called**: None
- **Called By**: 2000-DECIDE-ACTION
- **Error Handling**: Comprehensive error handling with rollback and status flags

### Function: 9700-CHECK-CHANGE-IN-REC
- **Location**: Line 4109 - 4192
- **Type**: PARAGRAPH
- **CICS Operations**: None
- **Description**: Implements optimistic locking by comparing current file data with stored original values
- **Function Flow**: 
  1. Compare current account data with ACUP-OLD-ACCT-DATA
  2. Compare current customer data with ACUP-OLD-CUST-DATA
  3. Check all significant fields for changes
  4. Set DATA-WAS-CHANGED-BEFORE-UPDATE flag if differences found
- **Input Variables**: 
  - ACCOUNT-RECORD: Current account data from file
  - CUSTOMER-RECORD: Current customer data from file
  - ACUP-OLD-DETAILS: Original data stored at read time
- **Output Variables**: 
  - DATA-WAS-CHANGED-BEFORE-UPDATE: Concurrent change flag
- **BMS Maps Used**: None
- **Files Accessed**: None
- **Programs Called**: None
- **Called By**: 9600-WRITE-PROCESSING
- **Error Handling**: Sets flag for concurrent update detection

### Function: 3400-SEND-SCREEN
- **Location**: Line 3589 - 3602
- **Type**: PARAGRAPH
- **CICS Operations**: 
  - EXEC CICS SEND MAP: Display screen to user
- **Description**: Sends formatted screen data to user terminal
- **Function Flow**: 
  1. Set map and mapset names
  2. Send map with cursor positioning
  3. Clear screen and free keyboard
- **Input Variables**: 
  - CACTUPAO: BMS output map structure
  - CCARD-NEXT-MAP: Map name to send
  - CCARD-NEXT-MAPSET: Mapset name to send
- **Output Variables**: None
- **BMS Maps Used**: 
  - CACTUPA: SEND - Account update screen output
- **Files Accessed**: None
- **Programs Called**: None
- **Called By**: 3000-SEND-MAP
- **Error Handling**: RESP code handling for send operation

### Function: ABEND-ROUTINE
- **Location**: Line 4203 - 4225
- **Type**: PARAGRAPH
- **CICS Operations**: 
  - EXEC CICS SEND: Send error message
  - EXEC CICS HANDLE ABEND: Cancel abend handling
  - EXEC CICS ABEND: Force program termination
- **Description**: Handles program abends and provides error information to user
- **Function Flow**: 
  1. Set default abend message if none provided
  2. Set program name as abend culprit
  3. Send abend data to terminal
  4. Cancel abend handling
  5. Force abend with code 9999
- **Input Variables**: 
  - ABEND-MSG: Error message text
  - ABEND-DATA: Complete abend information structure
- **Output Variables**: None
- **BMS Maps Used**: None
- **Files Accessed**: None
- **Programs Called**: None
- **Called By**: CICS HANDLE ABEND mechanism
- **Error Handling**: Terminal error processing with forced abend

## Function Categories

### Screen Management Functions
- 3000-SEND-MAP, 3100-SCREEN-INIT, 3200-SETUP-SCREEN-VARS, 3201-SHOW-INITIAL-VALUES
- 3202-SHOW-ORIGINAL-VALUES, 3203-SHOW-UPDATED-VALUES, 3250-SETUP-INFOMSG
- 3300-SETUP-SCREEN-ATTRS, 3310-PROTECT-ALL-ATTRS, 3320-UNPROTECT-FEW-ATTRS
- 3390-SETUP-INFOMSG-ATTRS, 3400-SEND-SCREEN, 1100-RECEIVE-MAP

### Data Validation Functions
- 1200-EDIT-MAP-INPUTS, 1205-COMPARE-OLD-NEW, 1210-EDIT-ACCOUNT, 1215-EDIT-MANDATORY
- 1220-EDIT-YESNO, 1225-EDIT-ALPHA-REQD, 1230-EDIT-ALPHANUM-REQD, 1235-EDIT-ALPHA-OPT
- 1240-EDIT-ALPHANUM-OPT, 1245-EDIT-NUM-REQD, 1250-EDIT-SIGNED-9V2, 1260-EDIT-US-PHONE-NUM
- 1265-EDIT-US-SSN, 1270-EDIT-US-STATE-CD, 1275-EDIT-FICO-SCORE, 1280-EDIT-US-STATE-ZIP-CD

### File I/O Functions
- 9000-READ-ACCT, 9200-GETCARDXREF-BYACCT, 9300-GETACCTDATA-BYACCT, 9400-GETCUSTDATA-BYCUST
- 9500-STORE-FETCHED-DATA, 9600-WRITE-PROCESSING, 9700-CHECK-CHANGE-IN-REC

### Program Control Functions
- 0000-MAIN, COMMON-RETURN, 1000-PROCESS-INPUTS, 2000-DECIDE-ACTION, ABEND-ROUTINE

### Utility Functions
- YYYY-STORE-PFKEY, EDIT-DATE-CCYYMMDD

## Technical Notes

### Program Architecture
- Pseudo-conversational CICS design with RETURN TRANSID
- Optimistic locking implementation for concurrent update control
- Comprehensive field validation with US-specific formats
- Multi-file update with transaction rollback capability

### Error Handling Strategy
- HANDLE ABEND for unexpected errors
- RESP/RESP2 evaluation for CICS command failures
- Field-level validation with user-friendly error messages
- Transaction rollback for data integrity

### Performance Considerations
- Sequential file access pattern (CXACAIX → ACCTDAT → CUSTDAT)
- Optimistic locking reduces lock contention
- Screen attribute management for user experience
- Efficient data comparison for change detection
