Please use the prompt template to find the dependencies for program COSGN00C, 
and provide me the result (COSGN00C_Dependencies.md).

# Dependency Analysis Prompt Template

## Input Requirements
Analyze all files in the `app/` folder to identify dependencies for program: **[PROGRAM_NAME]**

### Key Directories to Examine:
- `cbl/` - COBOL source programs
- `cpy/` - COBOL copybooks  
- `cpy-bms/` - BMS map copybooks
- `jcl/` - Job Control Language files
- `bms/` - BMS map definitions
- `csd/` - CICS resource definitions
- `data/` - Data files and datasets
- `sql/` - SQL scripts and DDL
- `proc/` - Stored procedures

## Analysis Logic

### 1. Direct Code Dependencies
- **COPY statements**: Include copybooks and shared code modules
- **BMS maps**: Screen definitions and mapsets
- **CICS commands**: File access, screen handling, program control
- **CALL/XCTL statements**: Program transfers and subroutine calls
- **SQL statements**: Database table and view references
- **INCLUDE statements**: External code inclusions

### 2. Data Dependencies  
- **VSAM files**: Dataset access and file operations
- **DB2 tables**: Database table access (SELECT, INSERT, UPDATE, DELETE)
- **Communication areas**: Inter-program data sharing
- **Working storage**: Shared data structures and constants
- **Screen maps**: Input/output field definitions
- **Message queues**: MQ series or other messaging

### 3. System Dependencies
- **Transaction IDs**: CICS transaction definitions
- **Program definitions**: CICS program resource definitions
- **File definitions**: VSAM file access permissions and FCT entries
- **Security**: User authentication and authorization
- **JCL jobs**: Batch job dependencies and scheduling
- **External systems**: Web services, APIs, external interfaces

## Output Format

### Dependency Summary Table
| Dependency Type | Count | Critical Level | Examples |
|----------------|-------|----------------|----------|
| Copybooks | [X] | High/Medium/Low | [List key copybooks] |
| Programs Called | [X] | High/Medium/Low | [List called programs] |
| VSAM Files | [X] | High/Medium/Low | [List VSAM files] |
| DB2 Tables | [X] | High/Medium/Low | [List database tables] |
| BMS Maps | [X] | High/Medium/Low | [List screen maps] |
| CICS Resources | [X] | Medium/Low | [List transactions, programs] |
| External Systems | [X] | Medium/Low | [List external interfaces] |

### Detailed Dependency Matrix
| Source Program | Depends On | Dependency Type | File Location | Usage Context |
|---------------|------------|-----------------|---------------|---------------|
| [PROGRAM_NAME] | [DEPENDENCY] | [TYPE] | [PATH] | [CONTEXT] |

### Dependency Flow Diagram
```
[PROGRAM_NAME] (Program Description)
├── Input Dependencies
│   ├── [COPYBOOK].cpy (Purpose)
│   ├── [BMS_MAP].CPY (Screen definition)
│   └── [SHARED_MODULE] (Utility functions)
├── Data Access
│   ├── [VSAM_FILE] (File operations)
│   ├── [DB2_TABLE] (Database access)
│   └── [MESSAGE_QUEUE] (Messaging)
└── Program Flow
    ├── [CALLED_PROGRAM1] (Function)
    └── [CALLED_PROGRAM2] (Function)
```

### Runtime Dependencies
- **Transaction**: [TRANID] (defined in [JCL_FILE])
- **CICS Program**: [PROGRAM_NAME] program definition
- **Screen Handling**: [MAP_NAME] from [MAPSET] mapset
- **File Access**: [ACCESS_TYPE] access to [DATASET_NAME]
- **Database**: [DB2_PLAN] plan for database access

## Template Usage Instructions

1. Replace `[PROGRAM_NAME]` with actual program identifier
2. Search source code for COPY statements to identify copybooks
3. Look for EXEC CICS commands to find system dependencies
4. Identify CALL/XCTL statements for program transfers
5. Scan for EXEC SQL statements to find database dependencies
6. Check JCL files for resource definitions and job dependencies
7. Map BMS screen definitions to their copybook equivalents
8. Document external system interfaces and API calls

## Self-Check List

### Code Analysis Completeness
- [ ] All COPY statements documented with file paths verified
- [ ] All EXEC CICS READ/WRITE commands mapped to datasets
- [ ] All EXEC SQL statements mapped to database tables
- [ ] All CALL/XCTL program transfers identified
- [ ] BMS mapset and map names cross-referenced
- [ ] Working storage file names captured
- [ ] External system calls and interfaces documented

### System Resource Verification  
- [ ] Transaction ID confirmed in JCL definitions
- [ ] Program name matches CICS resource definition
- [ ] VSAM file names match dataset definitions
- [ ] DB2 table names verified in database catalog
- [ ] Screen map names consistent between BMS and copybooks
- [ ] External system endpoints and protocols confirmed

### Dependency Classification
- [ ] **Critical**: Dependencies that prevent compilation/execution
- [ ] **High**: Dependencies affecting core functionality  
- [ ] **Medium**: Dependencies for enhanced features
- [ ] **Low**: Optional or utility dependencies

### Anti-Hallucination Measures
- [ ] Every dependency traced to actual code reference
- [ ] File paths verified to exist in app/ structure
- [ ] Program names match actual .cbl file names
- [ ] Copybook names match actual .cpy/.CPY file names
- [ ] Database table names verified in SQL statements
- [ ] No assumptions made about "standard" dependencies
- [ ] VSAM file names extracted from actual DATASET parameters
- [ ] Transaction IDs confirmed in JCL or resource definitions
- [ ] External system references verified in code comments or documentation

### Output Accuracy Check
- [ ] Dependency counts match actual findings
- [ ] File locations use correct relative paths from app/
- [ ] Usage context accurately describes code function
- [ ] Flow diagram reflects actual program logic
- [ ] Critical level assignments based on code analysis
- [ ] All placeholders replaced with actual values

## Common Patterns to Look For

### CICS Program Patterns
- XCTL vs CALL for program transfer
- VSAM file authentication and validation
- User type-based branching logic
- BMS screen interaction models
- Communication area data sharing

### Database Access Patterns
- EXEC SQL INCLUDE for SQLCA and host variables
- Cursor processing for multi-row operations
- Transaction management (COMMIT/ROLLBACK)
- Error handling for SQL operations

### Batch Processing Patterns
- JCL job step dependencies
- Dataset allocation and disposition
- Sort/merge utility usage
- Report generation and output routing

### Integration Patterns
- MQ message processing
- Web service calls (SOAP/REST)
- File transfer protocols (FTP/SFTP)
- External system authentication