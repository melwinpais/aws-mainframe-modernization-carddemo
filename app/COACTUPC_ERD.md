# COACTUPC Entity Relationship Diagram

## Program: COACTUPC
## Analysis Date: 2025-12-24
## Function: Account Update Processing

### Executive Summary
- Brief description: COACTUPC processes account and customer data updates through a CICS screen interface with comprehensive validation and optimistic locking
- Key entities identified: 7
- Main relationships: 12
- Data complexity level: HIGH

## Entity Relationship Diagram

```
┌─────────────────┐    manages     ┌─────────────────┐    contains    ┌─────────────────┐
│   User Session  │ ──────────────→│ Account Update  │ ──────────────→│   Update Data   │
│   (COCOM01Y)    │                │   Transaction   │                │ (WS-THIS-PROG)  │
│                 │                │                 │                │                 │
│ - USER-ID       │                │ - TRAN-ID       │                │ - OLD-DETAILS   │
│ - USER-TYPE     │                │ - CHANGE-ACTION │                │ - NEW-DETAILS   │
│ - FROM-PROGRAM  │                │ - INPUT-FLAG    │                │ - CHANGE-FLAGS  │
│ - TO-PROGRAM    │                └─────────────────┘                └─────────────────┘
└─────────────────┘                         │                                   │
         │                                  │ validates                         │
         │                                  ↓                                   │
         │                        ┌─────────────────┐                          │
         │                        │ Screen Interface│                          │
         │                        │   (COACTUP)     │                          │
         │                        │                 │                          │
         │                        │ - Input Fields  │                          │
         │                        │ - Output Fields │                          │
         │                        │ - Length Fields │                          │
         │                        │ - Flag Fields   │                          │
         │                        └─────────────────┘                          │
         │                                  │                                   │
         │                                  │ displays                          │
         │                                  ↓                                   │
         │                        ┌─────────────────┐                          │
         │                        │   Account Data  │←─────────────────────────┘
         │                        │   (CVACT01Y)    │
         │                        │                 │
         │                        │ - ACCT-ID       │
         │                        │ - ACTIVE-STATUS │
         │                        │ - CURR-BAL      │
         │                        │ - CREDIT-LIMIT  │
         │                        │ - OPEN-DATE     │
         │                        │ - EXPIRY-DATE   │
         │                        └─────────────────┘
         │                                  │
         │                                  │ linked_to
         │                                  ↓
         │                        ┌─────────────────┐    references    ┌─────────────────┐
         │                        │ Card Cross-Ref  │ ─────────────────→│  Customer Data  │
         │                        │   (CVACT03Y)    │                  │   (CVCUS01Y)    │
         │                        │                 │                  │                 │
         │                        │ - CARD-NUM      │                  │ - CUST-ID       │
         │                        │ - CUST-ID       │                  │ - FIRST-NAME    │
         │                        │ - ACCT-ID       │                  │ - LAST-NAME     │
         │                        └─────────────────┘                  │ - ADDRESS       │
         │                                  │                          │ - PHONE-NUM     │
         │                                  │                          │ - SSN           │
         │                                  │                          │ - DOB           │
         │                                  │                          │ - FICO-SCORE    │
         │                                  │                          └─────────────────┘
         │                                  │
         │                                  │ stored_in
         │                                  ↓
         └─────────────────────────→ ┌─────────────────┐
                                    │ VSAM File Store │
                                    │                 │
                                    │ - ACCTDAT       │
                                    │ - CUSTDAT       │
                                    │ - CXACAIX       │
                                    └─────────────────┘
```

## Entity Summary Table

| Entity Name | Source | Primary Key | Key Attributes | Related Entities | Business Purpose |
|-------------|--------|-------------|----------------|------------------|------------------|
| User Session | COCOM01Y | CDEMO-USER-ID | CDEMO-USER-TYPE, CDEMO-FROM-PROGRAM, CDEMO-TO-PROGRAM | Account Update Transaction | Inter-program communication and user context |
| Account Update Transaction | WS-THIS-PROGCOMMAREA | ACUP-CHANGE-ACTION | ACUP-OLD-DETAILS, ACUP-NEW-DETAILS | User Session, Account Data, Customer Data | Transaction state management and data comparison |
| Screen Interface | COACTUP BMS | Map Name | Input/Output field pairs (I/O/L/F) | Account Update Transaction | User interface for account/customer data entry |
| Account Data | CVACT01Y | ACCT-ID | ACCT-ACTIVE-STATUS, ACCT-CURR-BAL, ACCT-CREDIT-LIMIT, ACCT-OPEN-DATE | Card Cross-Ref, Update Data | Core account information and financial data |
| Customer Data | CVCUS01Y | CUST-ID | CUST-FIRST-NAME, CUST-LAST-NAME, CUST-SSN, CUST-DOB, CUST-FICO-SCORE | Card Cross-Ref, Update Data | Customer personal and contact information |
| Card Cross-Ref | CVACT03Y | XREF-CARD-NUM | XREF-CUST-ID, XREF-ACCT-ID | Account Data, Customer Data | Links cards to accounts and customers |
| VSAM File Store | Physical Files | File Name | Dataset characteristics | Account Data, Customer Data, Card Cross-Ref | Persistent data storage |

## Data Flow Matrix

| Source Entity | Data Flow | Target Entity | Flow Type | Business Rule | Implementation |
|---------------|-----------|---------------|-----------|---------------|----------------|
| Screen Interface | Account ID | Account Update Transaction | Input Validation | Required 11-digit non-zero number | BMS RECEIVE → CC-ACCT-ID |
| Account Update Transaction | Account ID | Card Cross-Ref | Key Lookup | Account must exist in cross-reference | CICS READ CXACAIX via LIT-CARDXREFNAME-ACCT-PATH |
| Card Cross-Ref | Customer ID | Account Update Transaction | Data Retrieval | Extract customer ID from xref | MOVE XREF-CUST-ID TO CDEMO-CUST-ID |
| Card Cross-Ref | Card Number | Account Update Transaction | Data Retrieval | Extract card number from xref | MOVE XREF-CARD-NUM TO CDEMO-CARD-NUM |
| Account Update Transaction | Account ID | Account Data | Key Lookup | Account must exist in master | CICS READ ACCTDAT via LIT-ACCTFILENAME |
| Account Update Transaction | Customer ID | Customer Data | Key Lookup | Customer must exist in master | CICS READ CUSTDAT via LIT-CUSTFILENAME |
| Account Data | Original Values | Account Update Transaction | Data Comparison | Store for optimistic locking | MOVE to ACUP-OLD-ACCT-DATA |
| Customer Data | Original Values | Account Update Transaction | Data Comparison | Store for optimistic locking | MOVE to ACUP-OLD-CUST-DATA |
| Screen Interface | Updated Values | Account Update Transaction | Input Processing | Validate all field edits | BMS fields → ACUP-NEW-DETAILS |
| Account Update Transaction | Change Detection | Account Update Transaction | Business Logic | Compare old vs new values | 9700-CHECK-CHANGE-IN-REC |
| Account Update Transaction | Updated Account | Account Data | Data Update | Optimistic locking with READ UPDATE | CICS READ UPDATE then REWRITE ACCTDAT |
| Account Update Transaction | Updated Customer | Customer Data | Data Update | Optimistic locking with READ UPDATE | CICS READ UPDATE then REWRITE CUSTDAT |
| Account Update Transaction | Success/Failure | Screen Interface | Status Display | Show update result | Message → BMS SEND MAP |
| Account Update Transaction | Program Context | User Session | Session Management | Update communication area | MOVE to CARDDEMO-COMMAREA |

## Variable Mapping by Category

| Variable Name | Entity | Source | Data Type | Usage Pattern | Validation Rules |
|---------------|--------|--------|-----------|---------------|------------------|
| ACCTSIDI/ACCTSIDO | Screen Interface | COACTUP BMS | X(11) | Input/Output | Required, numeric, non-zero |
| CC-ACCT-ID | Account Update Transaction | Working Storage | X(11) | Processing Key | Receives from screen input |
| WS-CARD-RID-ACCT-ID | Account Update Transaction | Working Storage | 9(11) | VSAM Key | Used for file operations |
| WS-CARD-RID-ACCT-ID-X | Account Update Transaction | Working Storage | X(11) REDEFINES | VSAM Key Alpha | Alpha version for RIDFLD |
| ACCT-ID | Account Data | CVACT01Y | 9(11) | Primary Key | VSAM key for account file |
| XREF-ACCT-ID | Card Cross-Ref | CVACT03Y | 9(11) | Foreign Key | Links to account data |
| XREF-CUST-ID | Card Cross-Ref | CVACT03Y | 9(09) | Foreign Key | Links to customer data |
| XREF-CARD-NUM | Card Cross-Ref | CVACT03Y | X(16) | Card Identifier | Card number reference |
| CUST-ID | Customer Data | CVCUS01Y | 9(09) | Primary Key | VSAM key for customer file |
| WS-CARD-RID-CUST-ID | Account Update Transaction | Working Storage | 9(09) | VSAM Key | Used for customer file operations |
| WS-CARD-RID-CUST-ID-X | Account Update Transaction | Working Storage | X(09) REDEFINES | VSAM Key Alpha | Alpha version for RIDFLD |
| ACUP-OLD-ACCT-ID | Account Update Transaction | WS-THIS-PROGCOMMAREA | 9(11) | Comparison | Original account ID |
| ACUP-NEW-ACCT-ID | Account Update Transaction | WS-THIS-PROGCOMMAREA | 9(11) | Comparison | Updated account ID |
| ACUP-CHANGE-ACTION | Account Update Transaction | WS-THIS-PROGCOMMAREA | X(1) | State Control | Transaction state flag |
| CDEMO-USER-ID | User Session | COCOM01Y | X(8) | Session Context | Current user identifier |
| CDEMO-ACCT-ID | User Session | COCOM01Y | 9(11) | Business Context | Account being processed |
| CDEMO-CUST-ID | User Session | COCOM01Y | 9(09) | Business Context | Customer being processed |
| CDEMO-CARD-NUM | User Session | COCOM01Y | 9(16) | Business Context | Card number being processed |

## Detailed Entity Relationships

### 1. User Session ↔ Account Update Transaction
- **Relationship Type**: One-to-One (per transaction)
- **Cardinality**: 1:1
- **Business Rule**: Each user session manages one account update transaction at a time
- **Implementation**: CARDDEMO-COMMAREA passed between programs
- **Key Fields**: CDEMO-USER-ID, CDEMO-ACCT-ID, CDEMO-CUST-ID

### 2. Account Update Transaction ↔ Screen Interface
- **Relationship Type**: Manages
- **Cardinality**: 1:1
- **Business Rule**: Transaction controls screen display and input processing
- **Implementation**: BMS SEND MAP/RECEIVE MAP operations
- **Key Fields**: All BMS field pairs (I/O/L/F suffixes)

### 3. Account Update Transaction ↔ Update Data
- **Relationship Type**: Contains
- **Cardinality**: 1:1
- **Business Rule**: Transaction maintains old and new data versions
- **Implementation**: WS-THIS-PROGCOMMAREA structure
- **Key Fields**: ACUP-OLD-DETAILS, ACUP-NEW-DETAILS

### 4. Card Cross-Ref ↔ Account Data
- **Relationship Type**: References
- **Cardinality**: 1:1
- **Business Rule**: Each account has one cross-reference entry
- **Implementation**: XREF-ACCT-ID used as key to read ACCTDAT
- **Key Fields**: XREF-ACCT-ID, ACCT-ID

### 5. Card Cross-Ref ↔ Customer Data
- **Relationship Type**: References
- **Cardinality**: 1:1
- **Business Rule**: Cross-reference provides customer ID for account
- **Implementation**: XREF-CUST-ID used as key to read CUSTDAT
- **Key Fields**: XREF-CUST-ID, CUST-ID

### 6. Account Data ↔ VSAM File Store
- **Relationship Type**: Stored In
- **Cardinality**: 1:1
- **Business Rule**: Each account record stored in ACCTDAT file
- **Implementation**: CICS READ/READ UPDATE/REWRITE operations
- **Key Fields**: ACCT-ID

### 7. Customer Data ↔ VSAM File Store
- **Relationship Type**: Stored In
- **Cardinality**: 1:1
- **Business Rule**: Each customer record stored in CUSTDAT file
- **Implementation**: CICS READ/READ UPDATE/REWRITE operations
- **Key Fields**: CUST-ID

### 8. Card Cross-Ref ↔ VSAM File Store
- **Relationship Type**: Stored In
- **Cardinality**: 1:1
- **Business Rule**: Cross-reference records stored in CXACAIX file
- **Implementation**: CICS READ operations via alternate index
- **Key Fields**: XREF-ACCT-ID (alternate index key)

## Copybook Structure Relationships

```
COCOM01Y (Communication Area)
├── CDEMO-GENERAL-INFO → Session Management
│   ├── CDEMO-USER-ID → User Context
│   ├── CDEMO-USER-TYPE → Authorization Level
│   ├── CDEMO-FROM-PROGRAM → Program Flow
│   └── CDEMO-TO-PROGRAM → Program Flow
├── CDEMO-CUSTOMER-INFO → Business Context
│   ├── CDEMO-CUST-ID → Customer Reference
│   └── CDEMO-CUST-* → Customer Names
├── CDEMO-ACCOUNT-INFO → Account Context
│   ├── CDEMO-ACCT-ID → Account Reference
│   └── CDEMO-ACCT-STATUS → Account State
└── CDEMO-CARD-INFO → Card Context
    └── CDEMO-CARD-NUM → Card Reference

CVACT01Y (Account Record)
├── ACCT-ID → Primary Key
├── ACCT-ACTIVE-STATUS → Business State
├── ACCT-CURR-BAL → Financial Data
├── ACCT-CREDIT-LIMIT → Financial Data
├── ACCT-CASH-CREDIT-LIMIT → Financial Data
├── ACCT-OPEN-DATE → Temporal Data
├── ACCT-EXPIRAION-DATE → Temporal Data
├── ACCT-REISSUE-DATE → Temporal Data
├── ACCT-CURR-CYC-CREDIT → Financial Data
├── ACCT-CURR-CYC-DEBIT → Financial Data
└── ACCT-GROUP-ID → Classification

CVCUS01Y (Customer Record)
├── CUST-ID → Primary Key
├── CUST-FIRST-NAME → Personal Data
├── CUST-MIDDLE-NAME → Personal Data
├── CUST-LAST-NAME → Personal Data
├── CUST-ADDR-* → Address Data
├── CUST-PHONE-NUM-* → Contact Data
├── CUST-SSN → Identity Data
├── CUST-GOVT-ISSUED-ID → Identity Data
├── CUST-DOB-YYYY-MM-DD → Personal Data
├── CUST-EFT-ACCOUNT-ID → Financial Data
├── CUST-PRI-CARD-HOLDER-IND → Business Data
└── CUST-FICO-CREDIT-SCORE → Credit Data

CVACT03Y (Card Cross-Reference)
├── XREF-CARD-NUM → Card Identifier
├── XREF-CUST-ID → Customer Foreign Key
└── XREF-ACCT-ID → Account Foreign Key

WS-THIS-PROGCOMMAREA (Transaction State)
├── ACUP-CHANGE-ACTION → State Control
├── ACUP-OLD-DETAILS → Original Data
│   ├── ACUP-OLD-ACCT-DATA → Account Snapshot
│   └── ACUP-OLD-CUST-DATA → Customer Snapshot
└── ACUP-NEW-DETAILS → Updated Data
    ├── ACUP-NEW-ACCT-DATA → Account Changes
    └── ACUP-NEW-CUST-DATA → Customer Changes
```

## Technical Notes

### Assumptions Made During Analysis
- Account and Customer updates are processed as a single transaction
- Optimistic locking is implemented through data comparison
- Card cross-reference provides the link between accounts and customers
- All validation occurs before database updates

### Areas Requiring Clarification
- Business rules for account/customer relationship cardinality
- Specific validation rules for financial data fields
- Error recovery procedures for failed updates
- Audit trail requirements for data changes

### Potential Data Quality Issues
- Date format consistency across different date fields
- Phone number format validation complexity
- SSN validation and privacy requirements
- FICO score range validation (300-850)

### Performance Considerations
- Sequential file reads (CXACAIX → ACCTDAT → CUSTDAT) create three I/O operations per account lookup
- Optimistic locking with READ UPDATE may cause contention in high-concurrency scenarios
- Screen field validation complexity (83 condition names) may affect response time
- Large communication area size (2000 bytes) for program transfers
- Field-by-field comparison in 9700-CHECK-CHANGE-IN-REC for optimistic locking verification

## Data Access Patterns

### Read Operations (Sequence verified from code)
1. **Account Lookup**: CC-ACCT-ID → WS-CARD-RID-ACCT-ID → CICS READ CXACAIX (LIT-CARDXREFNAME-ACCT-PATH)
2. **Account Master**: WS-CARD-RID-ACCT-ID-X → CICS READ ACCTDAT (LIT-ACCTFILENAME)
3. **Customer Lookup**: CDEMO-CUST-ID → WS-CARD-RID-CUST-ID → CICS READ CUSTDAT (LIT-CUSTFILENAME)

### Update Operations (Verified from code)
1. **Account Update**: CICS READ UPDATE ACCTDAT → 9700-CHECK-CHANGE-IN-REC → CICS REWRITE ACCTDAT
2. **Customer Update**: CICS READ UPDATE CUSTDAT → 9700-CHECK-CHANGE-IN-REC → CICS REWRITE CUSTDAT
3. **Transaction Rollback**: CICS SYNCPOINT ROLLBACK on any update failure

### Validation Patterns (Verified from code)
1. **Field-Level**: Individual field format and range validation
2. **Cross-Field**: Date consistency, phone number format validation
3. **Business-Level**: Account status, credit limits validation
4. **Data-Level**: Optimistic locking comparison in 9700-CHECK-CHANGE-IN-REC

### File Access Sequence (Verified from 9000-READ-ACCT)
1. **9200-GETCARDXREF-BYACCT**: Read CXACAIX using account ID
2. **9300-GETACCTDATA-BYACCT**: Read ACCTDAT using account ID
3. **9400-GETCUSTDATA-BYCUST**: Read CUSTDAT using customer ID from xref
4. **9500-STORE-FETCHED-DATA**: Move data to ACUP-OLD-DETAILS for comparison
