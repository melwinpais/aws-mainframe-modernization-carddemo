# CardDemo Project Structure

## Root Level Organization

### Core Application (`/app`)
Main application source code organized by component type:

- **`/app/bms`** - BMS map definitions for CICS screens
- **`/app/cbl`** - COBOL source programs (both CICS and batch)
- **`/app/cpy`** - COBOL copybooks for data structures
- **`/app/cpy-bms`** - Generated copybooks from BMS compilation
- **`/app/jcl`** - Job Control Language for batch processing
- **`/app/asm`** - Assembler programs and utilities
- **`/app/maclib`** - Macro definitions
- **`/app/proc`** - JCL procedures
- **`/app/csd`** - CICS System Definition files
- **`/app/data`** - Sample data files (ASCII and EBCDIC formats)
- **`/app/ctl`** - Control files for utilities
- **`/app/catlg`** - Catalog listings and definitions
- **`/app/scheduler`** - Job scheduling definitions (CA-7, Control-M)

### Optional Extensions (`/app/app-*`)
Modular extensions that enhance base functionality:

- **`/app/app-authorization-ims-db2-mq`** - Credit card authorization processing
- **`/app/app-transaction-type-db2`** - Transaction type management with DB2
- **`/app/app-vsam-mq`** - VSAM and MQ integration components

Each extension follows the same internal structure as the main app (bms, cbl, cpy, jcl, etc.).

### Supporting Directories

- **`/samples`** - Template JCL and procedures for compilation and deployment
- **`/scripts`** - Build automation and utility scripts
- **`/diagrams`** - Application flow diagrams and data models
- **`/samples/m2`** - Runtime packages for different platforms

## Naming Conventions

### Program Naming
- **CICS Programs**: `CO[function][screen]C.cbl` (e.g., COSGN00C.cbl for signon)
- **Batch Programs**: `CB[function][sequence]C.cbl` (e.g., CBTRN02C.cbl for transaction processing)
- **BMS Maps**: `CO[function][screen].bms` (e.g., COSGN00.bms)
- **Copybooks**: `C[type][function][sequence]Y.cpy` (e.g., COCOM01Y.cpy for communication area)

### Transaction IDs
- **CC00** - Signon
- **CM00** - Main Menu  
- **CA[nn]** - Account functions
- **CC[nn]** - Card functions
- **CT[nn]** - Transaction functions
- **CU[nn]** - User management (Admin)
- **CP[nn]** - Pending authorizations (optional)

### File Organization Patterns
- **Working Storage**: `WS-` prefix for working storage variables
- **Communication Areas**: `CDEMO-` prefix for COMMAREA fields
- **Copy Members**: Consistent `COPY` statement placement in COBOL programs
- **Error Handling**: Standardized `WS-ERR-FLG` and response code patterns

## Data Flow Architecture

### Online Processing
1. **BMS Maps** → **CICS Programs** → **VSAM Files**
2. **Communication Areas** pass data between programs
3. **Copybooks** ensure consistent data structures

### Batch Processing  
1. **JCL Jobs** → **COBOL Batch Programs** → **VSAM/Sequential Files**
2. **Control Files** manage processing parameters
3. **GDG** (Generation Data Groups) for historical data

### Optional Extensions Integration
- **IMS DB**: Hierarchical data access through PSBs and DBDs
- **DB2**: SQL embedded in COBOL programs with DCLGEN copybooks  
- **MQ**: Message-driven processing with request/response patterns

## Development Workflow

1. **Source Management**: All source in appropriate `/app` subdirectories
2. **Compilation**: Use templates from `/samples/jcl` 
3. **Testing**: Sample data in `/app/data` for unit testing
4. **Deployment**: JCL sequences defined in main README
5. **Documentation**: Diagrams and specifications in `/diagrams`

## Key Integration Points

- **COMMAREA**: `COCOM01Y.cpy` - Central communication structure
- **Error Handling**: Standardized across all programs
- **Security**: `USRSEC` file integration in all online components
- **Logging**: Consistent message and error logging patterns