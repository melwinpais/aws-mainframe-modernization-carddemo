Please use the prompt template to draw the ER diagram for program COSGN00C, 
and provide me the result (COSGN00C_ERD.md).

# ER Diagram Generation Template

## Input Requirements

### Primary Sources
- **Main Program File**: `app/cbl/{PROGRAM_NAME}.cbl` - COBOL source code
- **Dependencies File**: `{PROGRAM_NAME}_Dependencies.md` - defines program scope and external connections
- **Variable List**: `{PROGRAM_NAME}_VariableList.md` - comprehensive list of all program variables

### Supporting Files (from Dependencies analysis)
- **Copybooks**: `app/cpy/*.cpy` - Data structure definitions (COCOM01Y, COTTL01Y, CSDAT01Y, CSMSG01Y, CSUSR01Y)
- **BMS Maps**: `app/cpy-bms/*.CPY` and `app/bms/*.bms` - Screen interface definitions
- **VSAM Files**: Referenced datasets (e.g., USRSEC for user security)
- **Called Programs**: Target programs for XCTL operations
- **System Copybooks**: DFHAID, DFHBMSCA for CICS constants

## Analysis Logic

### Step 1: Variable Classification by Source
Categorize variables from the Variable List by their origin:
- **Working Storage Variables**: Program-specific variables (WS-* fields)
- **Copybook Structures**: Shared data structures from COPY statements
- Communication areas (COCOM01Y - inter-program data flow)
- User/Security records (CSUSR01Y - authentication entities)
- Screen constants (COTTL01Y, CSMSG01Y - display data)
- **BMS Map Fields**: Screen interface variables (input/output pairs)
- **VSAM File Records**: Persistent data structures
- **System Variables**: CICS/COBOL built-in fields

### Step 2: Entity Identification
- **Primary Business Entities**: Core objects from copybook structures
- User entities (from CSUSR01Y structure)
- Communication/Session entities (from COCOM01Y)
- Screen/Interface entities (from BMS maps)
- **Supporting Entities**: Program control and system entities
- Transaction context (program flow control)
- Error handling (message and status entities)
- **Data Storage Entities**: VSAM files and their record structures

### Step 3: Relationship Mapping
- **Structural Relationships**: How copybook structures relate
- Group item hierarchies within copybooks
- Cross-references between different copybooks
- **Functional Relationships**: Business logic connections
- User authentication (User ID → Security Record)
- Session management (Communication Area → Program Context)

### Step 4: Attribute Assignment
- **Input Attributes**: BMS map input fields and their validation rules
- **Processing Attributes**: Working storage fields used in business logic
- **Output Attributes**: BMS map output fields and communication area fields
- **Persistent Attributes**: VSAM file record fields
- **Control Attributes**: Program flow and error handling fields

## Output Requirements

### 1. Entity Relationship Diagram
```
[Visual ER Diagram showing:]
- Business entities as rectangles (User, Session, Screen, etc.)
- Data flow relationships as diamonds with cardinality labels
- Copybook structures as grouped entities
- BMS interface connections as specialized relationships
- VSAM file access patterns
- Program transfer relationships (XCTL flows)
```

### 2. Entity Summary Table
| Entity Name | Source | Primary Key | Key Attributes | Related Entities | Business Purpose |
|-------------|--------|-------------|----------------|------------------|------------------|
| User | CSUSR01Y | SEC-USR-ID | SEC-USR-FNAME, SEC-USR-PWD, SEC-USR-TYPE | Session, Security | Authentication & Authorization |
| Session | COCOM01Y | CDEMO-USER-ID | CDEMO-FROM-PROGRAM, CDEMO-TO-PROGRAM | User, Screen | Inter-program communication |
| Screen | BMS Maps | Map Name | Input/Output field pairs | Session, User | User interface |

### 3. Data Flow Matrix
| Source Entity | Data Flow | Target Entity | Flow Type | Business Rule | Implementation |
|---------------|-----------|---------------|-----------|---------------|----------------|
| Screen Input | User Credentials | Working Storage | Input Validation | Required fields check | BMS RECEIVE → WS fields |
| Working Storage | User ID | VSAM File | Authentication | User must exist | CICS READ USRSEC |
| VSAM Record | User Data | Communication Area | Session Setup | Valid user → session | MOVE to COCOM01Y |
| Communication Area | Program Context | Target Program | Program Transfer | Role-based routing | CICS XCTL |

### 4. Variable Mapping by Category
| Variable Name | Entity | Source | Data Type | Usage Pattern | Validation Rules |
|---------------|--------|--------|-----------|---------------|------------------|
| USERIDI/USERIDO | Screen | BMS Map | X(8) | Input/Output | Required, non-spaces |
| WS-USER-ID | Processing | Working Storage | X(8) | Temporary | Receives from screen |
| SEC-USR-ID | User | CSUSR01Y | X(8) | Key Field | VSAM key for authentication |
| CDEMO-USER-ID | Session | COCOM01Y | X(8) | Context | Passed between programs |

### 5. Entity Relationship Diagram
```
[Show entity relationships only:]

┌─────────────┐    authenticates    ┌─────────────┐
│    User     │ ──────────────────→ │   Session   │
│             │                     │             │
│ - USER-ID   │                     │ - USER-ID   │
│ - PASSWORD  │                     │ - USER-TYPE │
│ - USER-TYPE │                     │ - TRAN-ID   │
└─────────────┘                     └─────────────┘
    │                                   │
    │ stored_in                         │ contains
    ↓                                   ↓
┌─────────────┐                     ┌─────────────┐
│ UserSecurity│                     │ CommArea    │
│   (VSAM)    │                     │ (COCOM01Y)  │
│             │                     │             │
│ - USRSEC-ID │                     │ - CA-*      │
│ - SEC-USR-* │                     │   fields    │
└─────────────┘                     └─────────────┘
```

### 6. Copybook Structure Relationships
```
COCOM01Y (Communication Area)
├── CDEMO-GENERAL-INFO → Session Management
├── CDEMO-CUSTOMER-INFO → Business Context  
├── CDEMO-ACCOUNT-INFO → Account Context
└── CDEMO-CARD-INFO → Card Context

CSUSR01Y (User Security)
├── SEC-USR-ID → Authentication Key
├── SEC-USR-PWD → Authentication Credential
└── SEC-USR-TYPE → Authorization Level
```

## Template Format

### Program: {PROGRAM_NAME}
### Analysis Date: {DATE}
### Analyst: {ANALYST_NAME}

#### Executive Summary
- Brief description of program purpose
- Key entities identified: {COUNT}
- Main relationships: {COUNT}
- Data complexity level: {LOW/MEDIUM/HIGH}

#### Detailed Analysis
[Include all tables and diagrams from Output Requirements section]

#### Technical Notes
- Assumptions made during analysis
- Areas requiring clarification
- Potential data quality issues
- Performance considerations

## Self-Check Checklist

### Completeness Verification
- [ ] All variables from Variable List are categorized by source (Working Storage, Copybooks, BMS, System)
- [ ] All copybook structures (COCOM01Y, CSUSR01Y, etc.) are represented as entities or entity groups
- [ ] BMS map input/output field pairs are properly linked
- [ ] VSAM file access patterns are documented with their record structures
- [ ] Program transfer flows (XCTL operations) are mapped with communication areas
- [ ] All CICS commands (READ, SEND, RECEIVE, RETURN) are reflected in data flow

### COBOL/CICS Accuracy Validation
- [ ] Group items and elementary items hierarchy is preserved
- [ ] PICTURE clauses and data types are accurately represented
- [ ] 88-level condition names are linked to their parent fields
- [ ] REDEFINES relationships are documented
- [ ] OCCURS clauses and array structures are noted
- [ ] CICS RESP/RESP2 error handling patterns are included

### Data Flow Consistency
- [ ] Screen input validation flow matches EVALUATE/WHEN logic in program
- [ ] VSAM file access keys match the RIDFLD specifications
- [ ] Communication area field usage aligns with program transfer logic
- [ ] Working storage field movements follow program MOVE statements
- [ ] BMS map field attributes (length, flag fields) are consistent

### Business Logic Alignment
- [ ] Authentication flow reflects actual USRSEC file read operations
- [ ] User type validation (Admin vs User) matches SEC-USR-TYPE usage
- [ ] Error message handling corresponds to WS-ERR-FLG logic
- [ ] Program routing logic matches CDEMO-USER-TYPE conditions
- [ ] Session management reflects COCOM01Y structure usage

### Anti-Hallucination Measures (COBOL-Specific)
- [ ] Every entity is traceable to specific copybook or working storage definition
- [ ] Data relationships are based on actual MOVE statements and field usage
- [ ] No assumed relationships beyond what's coded in PROCEDURE DIVISION
- [ ] VSAM file structure matches actual record layout in copybooks
- [ ] BMS map relationships reflect actual SEND MAP/RECEIVE MAP operations

### Technical Validation
- [ ] CICS resource definitions (files, programs, transactions) are documented
- [ ] Copybook dependencies match actual COPY statements in source
- [ ] System copybook usage (DFHAID, DFHBMSCA) is properly categorized
- [ ] LINKAGE SECTION variables are distinguished from WORKING-STORAGE
- [ ] COMP fields and binary data types are correctly identified

### Documentation Quality for COBOL Programs
- [ ] Copybook purposes are clearly explained (communication, security, constants, etc.)
- [ ] BMS map field naming conventions are documented (I/O/L/F suffixes)
- [ ] CICS command patterns are explained in business context
- [ ] Program transfer mechanisms (XCTL vs LINK) are clarified
- [ ] VSAM file access patterns are documented with business purpose

### Final Validation
- [ ] ER diagram supports understanding of program's authentication flow
- [ ] Data model enables optimization of VSAM file access patterns
- [ ] Documentation helps with BMS map maintenance and enhancement
- [ ] Model supports future program modifications and debugging
- [ ] Relationships enable understanding of inter-program communication patterns

## Usage Instructions

1. Replace `{PROGRAM_NAME}` with actual program identifier (e.g., COSGN00C)
2. Gather all required input files:
- Main COBOL source: `app/cbl/{PROGRAM_NAME}.cbl`
- Dependencies analysis: `{PROGRAM_NAME}_Dependencies.md`
- Variable inventory: `{PROGRAM_NAME}_VariableList.md`
3. Follow analysis steps systematically, focusing on COBOL/CICS patterns
4. Complete all output sections with specific examples from the program
5. Verify against checklist, paying special attention to COBOL-specific items
6. Review with stakeholders for business accuracy and technical correctness

## Notes for Analysts

### COBOL Program Analysis Focus
- **Copybook Structures**: Treat each major copybook as a potential entity group
- **Data Movement Patterns**: Trace MOVE statements to understand data relationships
- **CICS Command Analysis**: Map CICS operations to data access patterns
- **BMS Map Relationships**: Link input/output field pairs as interface entities

### Common COBOL/CICS Patterns to Look For
- **Authentication Flows**: User input → validation → VSAM read → session setup
- **Communication Areas**: Inter-program data passing via COCOM01Y-style structures
- **Error Handling**: Flag variables (WS-ERR-FLG) and message display patterns
- **Screen Management**: BMS map send/receive cycles with field validation
- **File Access**: VSAM READ operations with key field relationships

### Validation Guidelines
- When in doubt, trace variable usage through PROCEDURE DIVISION code
- Verify copybook relationships by examining actual COPY statements
- Validate VSAM access patterns against file definitions in Dependencies
- Check BMS map field usage in both SEND MAP and RECEIVE MAP operations
- Confirm program transfer logic by examining XCTL/LINK target programs

### Documentation Best Practices
- Focus on data relationships that support business processes
- Prioritize entities that represent persistent or shared data
- Document CICS resource dependencies clearly
- Explain copybook purposes in business terms
- Consider both current functionality and potential system evolution