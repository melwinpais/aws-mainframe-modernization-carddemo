# COACTVWC Entity Relationship Diagram

## Program: COACTVWC
## Analysis Date: 2025-12-24
## Function: Account View Processing

### Executive Summary
- **Program Purpose**: Display comprehensive account information including customer details
- **Key Entities Identified**: 6 primary entities
- **Main Relationships**: 8 data flow relationships
- **Data Complexity Level**: MEDIUM (3 VSAM files, cross-reference lookups, screen interface)

### Entity Identification

#### Primary Business Entities
1. **Account** (CVACT01Y) - Core financial account data
2. **Customer** (CVCUS01Y) - Customer demographic and contact information  
3. **CardXref** (CVACT03Y) - Cross-reference linking cards to accounts and customers
4. **Session** (COCOM01Y) - Inter-program communication and context
5. **Screen** (COACTVW) - User interface for account viewing
6. **ProcessingContext** (Working Storage) - Program control and validation

### Entity Relationship Diagram

```
┌─────────────────┐    1:1 lookup    ┌─────────────────┐
│     Screen      │ ─────────────────→│ ProcessingContext│
│                 │                   │                 │
│ - ACCTSIDI      │                   │ - CC-ACCT-ID    │
│ - ACCTSIDO      │                   │ - WS-INPUT-FLAG │
│ - ACSTTUSO      │                   │ - WS-RESP-CD    │
│ - ACURBALO      │                   │ - WS-REAS-CD    │
│ - Customer Info │                   │ - Validation    │
└─────────────────┘                   └─────────────────┘
         │                                     │
         │ displays                            │ validates/reads
         ↓                                     ↓
┌─────────────────┐                   ┌─────────────────┐
│    Session      │                   │    CardXref     │
│   (COCOM01Y)    │                   │   (CVACT03Y)    │
│                 │                   │                 │
│ - CDEMO-ACCT-ID │←──────────────────│ - XREF-ACCT-ID  │
│ - CDEMO-CUST-ID │                   │ - XREF-CUST-ID  │
│ - CDEMO-CARD-NUM│                   │ - XREF-CARD-NUM │
│ - CDEMO-USER-ID │                   │                 │
└─────────────────┘                   └─────────────────┘
         │                                     │
         │ contains context                    │ references
         ↓                                     ↓
┌─────────────────┐    1:1 lookup    ┌─────────────────┐
│    Account      │ ←─────────────────│    Customer     │
│   (CVACT01Y)    │                   │   (CVCUS01Y)    │
│                 │                   │                 │
│ - ACCT-ID       │                   │ - CUST-ID       │
│ - ACCT-STATUS   │                   │ - CUST-FNAME    │
│ - ACCT-CURR-BAL │                   │ - CUST-LNAME    │
│ - ACCT-CREDIT-  │                   │ - CUST-SSN      │
│   LIMIT         │                   │ - CUST-ADDR-*   │
│ - ACCT-OPEN-DATE│                   │ - CUST-PHONE-*  │
│ - ACCT-GROUP-ID │                   │ - CUST-FICO-*   │
└─────────────────┘                   └─────────────────┘
```

### Entity Summary Table

| Entity Name | Source | Primary Key | Key Attributes | Related Entities | Business Purpose |
|-------------|--------|-------------|----------------|------------------|------------------|
| Account | CVACT01Y | ACCT-ID | ACCT-ACTIVE-STATUS, ACCT-CURR-BAL, ACCT-CREDIT-LIMIT | Customer, CardXref | Financial account management |
| Customer | CVCUS01Y | CUST-ID | CUST-FIRST-NAME, CUST-LAST-NAME, CUST-SSN, CUST-ADDR-* | Account, CardXref | Customer demographic data |
| CardXref | CVACT03Y | XREF-ACCT-ID | XREF-CUST-ID, XREF-CARD-NUM | Account, Customer | Account-Customer-Card linking |
| Session | COCOM01Y | CDEMO-USER-ID | CDEMO-ACCT-ID, CDEMO-CUST-ID, CDEMO-FROM-PROGRAM | All entities | Inter-program communication |
| Screen | COACTVW | Map Name | ACCTSIDI/O, Customer display fields | Session, ProcessingContext | User interface |
| ProcessingContext | Working Storage | Program Instance | WS-INPUT-FLAG, WS-RESP-CD, CC-ACCT-ID | Screen, All file entities | Program control and validation |

### Data Flow Matrix

| Source Entity | Data Flow | Target Entity | Flow Type | Business Rule | Implementation |
|---------------|-----------|---------------|-----------|---------------|----------------|
| Screen Input | Account ID | ProcessingContext | Input Validation | Required 11-digit number | BMS RECEIVE → CC-ACCT-ID |
| ProcessingContext | Account ID | Session | Context Update | Valid account stored | MOVE CC-ACCT-ID → CDEMO-ACCT-ID |
| Session | Account ID | ProcessingContext | Key Setup | Account ID for file access | MOVE CDEMO-ACCT-ID → WS-CARD-RID-ACCT-ID |
| ProcessingContext | Account ID | CardXref | Key Lookup | Account must exist in xref | CICS READ CXACAIX |
| CardXref | Customer ID | Session | Context Setup | Link account to customer | MOVE XREF-CUST-ID → CDEMO-CUST-ID |
| CardXref | Card Number | Session | Context Setup | Link account to card | MOVE XREF-CARD-NUM → CDEMO-CARD-NUM |
| ProcessingContext | Account ID | Account | Master Lookup | Account must exist | CICS READ ACCTDAT |
| Session | Customer ID | Customer | Master Lookup | Customer must exist | CICS READ CUSTDAT (via WS-CARD-RID-CUST-ID) |
| Account | Account Data | Screen | Display Output | Show account details | MOVE fields → BMS output |
| Customer | Customer Data | Screen | Display Output | Show customer details | MOVE fields → BMS output |

### Variable Mapping by Category

| Variable Name | Entity | Source | Data Type | Usage Pattern | Validation Rules |
|---------------|--------|--------|-----------|---------------|------------------|
| ACCTSIDI | Screen | BMS Map | 99999999999 | Input | Required, non-zero, 11 digits |
| CC-ACCT-ID | ProcessingContext | Working Storage | X(11) | Temporary | Receives from screen input |
| CDEMO-ACCT-ID | Session | COCOM01Y | 9(11) | Context | Receives from CC-ACCT-ID after validation |
| WS-CARD-RID-ACCT-ID | ProcessingContext | Working Storage | 9(11) | Key Field | Receives from CDEMO-ACCT-ID for VSAM access |
| WS-CARD-RID-ACCT-ID-X | ProcessingContext | Working Storage | X(11) | Key Field | REDEFINES for VSAM key (alphanumeric) |
| XREF-ACCT-ID | CardXref | CVACT03Y | 9(11) | Key Field | Cross-reference key |
| XREF-CUST-ID | CardXref | CVACT03Y | 9(09) | Foreign Key | Links to customer |
| ACCT-ID | Account | CVACT01Y | 9(11) | Primary Key | Account master key |
| CUST-ID | Customer | CVCUS01Y | 9(09) | Primary Key | Customer master key |
| CDEMO-CUST-ID | Session | COCOM01Y | 9(09) | Context | Receives from XREF-CUST-ID |
| WS-CARD-RID-CUST-ID | ProcessingContext | Working Storage | 9(09) | Key Field | Receives from CDEMO-CUST-ID for customer lookup |
| WS-CARD-RID-CUST-ID-X | ProcessingContext | Working Storage | X(09) | Key Field | REDEFINES for VSAM key (alphanumeric) |

### Detailed Entity Relationships

#### 1. Account ↔ CardXref Relationship
- **Type**: One-to-Many (1:M)
- **Key**: ACCT-ID = XREF-ACCT-ID
- **Business Rule**: Each account can have multiple card cross-references
- **Implementation**: CICS READ CXACAIX using account ID as key
- **Access Pattern**: Account ID → CardXref lookup via alternate index

#### 2. Customer ↔ CardXref Relationship  
- **Type**: One-to-Many (1:M)
- **Key**: CUST-ID = XREF-CUST-ID
- **Business Rule**: Each customer can have multiple accounts/cards
- **Implementation**: Retrieved from CardXref, used to read Customer master
- **Access Pattern**: CardXref provides Customer ID → Customer master lookup

#### 3. Account ↔ Customer Relationship
- **Type**: Many-to-One (M:1) 
- **Key**: Via CardXref (XREF-CUST-ID)
- **Business Rule**: Multiple accounts can belong to one customer
- **Implementation**: Indirect relationship through CardXref entity
- **Access Pattern**: Account → CardXref → Customer (sequential reads)

#### 4. Screen ↔ ProcessingContext Relationship
- **Type**: One-to-One (1:1)
- **Key**: Program instance
- **Business Rule**: Each screen interaction has one processing context
- **Implementation**: BMS RECEIVE/SEND operations with working storage
- **Access Pattern**: Bidirectional data movement via MOVE statements

#### 5. Session ↔ All Entities Relationship
- **Type**: One-to-Many (1:M)
- **Key**: Program context
- **Business Rule**: Session maintains context for all entity interactions
- **Implementation**: Communication area passed between programs
- **Access Pattern**: Central hub for inter-program data sharing

### Copybook Structure Relationships

```
COCOM01Y (Communication Area)
├── CDEMO-GENERAL-INFO → Session Management
│   ├── CDEMO-FROM-PROGRAM → Program Flow Control
│   ├── CDEMO-TO-PROGRAM → Program Routing
│   └── CDEMO-USER-ID → User Context
├── CDEMO-CUSTOMER-INFO → Customer Context
│   └── CDEMO-CUST-ID → Customer Identification
├── CDEMO-ACCOUNT-INFO → Account Context
│   └── CDEMO-ACCT-ID → Account Identification
└── CDEMO-CARD-INFO → Card Context
    └── CDEMO-CARD-NUM → Card Identification

CVACT01Y (Account Master)
├── ACCT-ID → Primary Key
├── ACCT-ACTIVE-STATUS → Business Status
├── ACCT-CURR-BAL → Financial Data
├── ACCT-CREDIT-LIMIT → Credit Management
└── ACCT-OPEN-DATE → Temporal Data

CVCUS01Y (Customer Master)  
├── CUST-ID → Primary Key
├── CUST-FIRST-NAME → Personal Data
├── CUST-LAST-NAME → Personal Data
├── CUST-SSN → Identification
├── CUST-ADDR-* → Address Information
└── CUST-FICO-CREDIT-SCORE → Credit Information

CVACT03Y (Card Cross-Reference)
├── XREF-ACCT-ID → Account Link
├── XREF-CUST-ID → Customer Link
└── XREF-CARD-NUM → Card Identification
```

### Data Access Patterns

#### Sequential Read Pattern (Primary Flow)
1. **Input**: User enters Account ID via screen (ACCTSIDI)
2. **Validation**: Account ID moved to CC-ACCT-ID and validated in ProcessingContext
3. **Context Update**: Valid CC-ACCT-ID moved to CDEMO-ACCT-ID in Session
4. **Key Setup**: CDEMO-ACCT-ID moved to WS-CARD-RID-ACCT-ID for file access
5. **CardXref Lookup**: CICS READ CXACAIX using WS-CARD-RID-ACCT-ID-X
6. **Context Setup**: Move XREF-CUST-ID → CDEMO-CUST-ID, XREF-CARD-NUM → CDEMO-CARD-NUM
7. **Account Lookup**: CICS READ ACCTDAT using WS-CARD-RID-ACCT-ID-X  
8. **Customer Setup**: Move CDEMO-CUST-ID → WS-CARD-RID-CUST-ID for customer lookup
9. **Customer Lookup**: CICS READ CUSTDAT using WS-CARD-RID-CUST-ID-X
10. **Display**: Move all retrieved data to screen output fields

#### Error Handling Pattern
- **Not Found Conditions**: Each VSAM read has NOTFND handling
- **Validation Errors**: Input validation sets error flags and messages
- **Response Codes**: CICS RESP/RESP2 captured in WS-RESP-CD/WS-REAS-CD
- **Error Messages**: Constructed in WS-RETURN-MSG for screen display

### VSAM File Access Summary

| File | Access Type | Key Field | Record Layout | Business Purpose |
|------|-------------|-----------|---------------|------------------|
| CXACAIX | READ (Alternate Index) | WS-CARD-RID-ACCT-ID-X | CARD-XREF-RECORD | Account-Customer-Card linking |
| ACCTDAT | READ (Primary) | WS-CARD-RID-ACCT-ID-X | ACCOUNT-RECORD | Account master data retrieval |
| CUSTDAT | READ (Primary) | WS-CARD-RID-CUST-ID-X | CUSTOMER-RECORD | Customer master data retrieval |

### Technical Notes

#### Assumptions Made During Analysis
- CardXref serves as the primary linking mechanism between accounts and customers
- All file reads are for display purposes only (no updates performed)
- Session context is maintained through COCOM01Y communication area
- Screen interaction follows standard BMS input/output pattern

#### Areas Requiring Clarification
- Business rules for account-customer relationships (1:1 vs 1:M vs M:M)
- Card number usage and validation requirements
- Customer address formatting and display rules
- Error recovery procedures for file access failures

#### Potential Data Quality Issues
- Account-Customer relationship integrity depends on CardXref accuracy
- Customer ID consistency between CardXref and Customer master
- Account ID validation limited to format checking (11 digits)
- No referential integrity enforcement at application level

#### Performance Considerations
- Sequential file reads (CardXref → Account → Customer) create latency
- Alternate index access on CXACAIX may impact response time
- Large customer records (500 bytes) affect memory usage
- Screen field formatting (SSN with dashes) adds processing overhead

### Quality Assurance Verification

✅ **Completeness Verification**
- All VSAM files (ACCTDAT, CUSTDAT, CXACAIX) represented as entities
- All major copybooks (COCOM01Y, CVACT01Y, CVCUS01Y, CVACT03Y) analyzed
- BMS map input/output relationships documented
- Working storage validation logic captured

✅ **COBOL/CICS Accuracy Validation**  
- Entity keys match RIDFLD specifications in CICS READ commands
- Data relationships traced through actual MOVE statements
- VSAM access patterns match program logic (9200/9300/9400 paragraphs)
- Communication area usage verified against XCTL operations

✅ **Data Flow Consistency**
- Screen input validation matches program EVALUATE logic
- File access sequence follows program execution flow
- Error handling patterns consistent with RESP code checking
- Display field population matches MOVE statements to BMS output

✅ **Business Logic Alignment**
- Account viewing process reflects read-only access pattern
- Customer-account relationship via CardXref matches program logic
- Session management through communication area supports program transfers
- Input validation rules match field editing in program

✅ **Anti-Hallucination Measures**
- Every entity traced to specific copybook definition
- All relationships based on actual field usage in PROCEDURE DIVISION
- VSAM access patterns match exact CICS READ operations
- No assumed relationships beyond coded program logic

**Analysis Complete**: ER diagram accurately represents COACTVWC program data relationships and access patterns.

## Post-Verification Corrections Made:

### **Critical Data Flow Corrections:**
1. **Account ID Flow**: Corrected to show proper flow: Screen → CC-ACCT-ID → CDEMO-ACCT-ID → WS-CARD-RID-ACCT-ID
2. **VSAM Key Fields**: Updated to show actual RIDFLD usage (WS-CARD-RID-ACCT-ID-X, WS-CARD-RID-CUST-ID-X)
3. **Customer ID Setup**: Added missing step where CDEMO-CUST-ID is moved to WS-CARD-RID-CUST-ID before customer lookup
4. **Data Flow Sequence**: Corrected to show Session entity as intermediate storage between validation and file access

### **Technical Accuracy Improvements:**
1. **REDEFINES Usage**: Documented that alphanumeric REDEFINES fields are used as VSAM keys
2. **File Access Pattern**: Clarified that all VSAM reads use working storage key fields, not direct copybook fields
3. **Context Management**: Showed proper role of COCOM01Y as intermediate storage for validated data
4. **Sequential Processing**: Updated to reflect actual 10-step process including all data movements

### **Verification Against Source Code:**
✅ **Line 691**: `MOVE CDEMO-ACCT-ID TO WS-CARD-RID-ACCT-ID` - Confirmed Session → ProcessingContext flow  
✅ **Line 678**: `MOVE CC-ACCT-ID TO CDEMO-ACCT-ID` - Confirmed ProcessingContext → Session flow  
✅ **Line 632**: `MOVE ACCTSIDI OF CACTVWAI TO CC-ACCT-ID` - Confirmed Screen → ProcessingContext flow  
✅ **Line 708**: `MOVE CDEMO-CUST-ID TO WS-CARD-RID-CUST-ID` - Confirmed customer key setup  
✅ **Line 727-731**: CICS READ using `WS-CARD-RID-ACCT-ID-X` - Confirmed actual RIDFLD usage  
✅ **Line 827-831**: CICS read using `WS-CARD-RID-CUST-ID-X` - Confirmed customer lookup key  

All data flows and relationships now accurately reflect the actual program implementation.
