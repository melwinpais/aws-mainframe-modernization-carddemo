# CardDemo Technology Stack

## Core Technologies
- **COBOL**: Primary programming language for business logic
- **CICS**: Transaction processing system for online components
- **VSAM (KSDS with AIX)**: Primary data storage with key-sequenced datasets and alternate indexes
- **JCL**: Job Control Language for batch processing
- **RACF**: Security and access control
- **BMS**: Basic Mapping Support for screen definitions
- **ASSEMBLER**: System-level programming utilities

## Optional Technologies
- **DB2**: Relational database for transaction type management and fraud detection
- **IMS DB**: Hierarchical database for authorization data
- **MQ**: Message queuing for real-time processing and system integration
- **JCL Utilities**: FTP, TXT2PDF, DB2 LOAD/UNLOAD, IMS DB LOAD/UNLOAD, Internal Reader

## Data Formats and Structures
- **Advanced Data Formats**: COMP, COMP-3, Zoned Decimal, Signed, Unsigned
- **Dataset Types**: VSAM (ESDS/RRDS), GDG (Generation Data Groups), PDS
- **Record Formats**: VB (Variable Blocked), FBA (Fixed Blocked ASCII)
- **Complex Copybook Structures**: REDEFINES, OCCURS, OCCURS DEPENDING ON

## Build and Compilation

### Standard Dataset Structure
All source code follows the AWS.M2.CARDDEMO.* naming convention:
- `AWS.M2.CARDDEMO.JCL` - Job Control Language (FB, LRECL=80)
- `AWS.M2.CARDDEMO.CBL` - COBOL source programs (FB, LRECL=80)
- `AWS.M2.CARDDEMO.CPY` - COBOL copybooks (FB, LRECL=80)
- `AWS.M2.CARDDEMO.BMS` - BMS map definitions (FB, LRECL=80)
- `AWS.M2.CARDDEMO.ASM` - Assembler programs (FB, LRECL=80)
- `AWS.M2.CARDDEMO.MACLIB` - Macro library (FB, LRECL=80)

### Compilation Process
1. **BMS Maps**: Compile first to generate copybooks for screen layouts
2. **COBOL Programs**: 
   - Batch programs: Use BATCMP.jcl template
   - CICS programs: Use CICCMP.jcl template
   - DB2 programs: Use CICDBCMP.jcl template
3. **Assembler**: Use standard assembler compilation procedures

### Common JCL Patterns
- Replace `BATCHPGM`/`CICSPGMN`/`CICSMAP` with actual program names
- Set HLQ (High Level Qualifier) to your dataset prefix
- Use `C programname newname all` for global replacements in templates

### Environment Setup Commands
```
// Initialize environment
CLOSEFIL -> ACCTFILE -> CARDFILE -> CUSTFILE -> XREFFILE -> OPENFIL

// CICS Resource Management
CEDA INSTALL TRANS(CC00) GROUP(CARDDEMO)
CEMT SET PROG(programname) NEWCOPY
```

### File Transfer Requirements
- Use binary mode for EBCDIC data files
- Use text mode for source code (JCL, COBOL, BMS)
- Preserve record formats and lengths during transfer