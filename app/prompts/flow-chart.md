Please use the prompt template to draw all the flow chart for program COSGN00C, 
and provide me the result (COSGN00C_FlowChart.md).

# Flow Chart Generation Template

## Input Requirements

### Program Files Analysis
- **Target Program**: `{PROGRAM_NAME}` (e.g., COSGN00C)
- **Source Location**: `app/` folder
- **Reference Files**:
- Dependencies file: `{PROGRAM_NAME}_Dependencies`
- Variable list: `{PROGRAM_NAME}_VariableList` 
- Function list: `{PROGRAM_NAME}_FunctionList`

### Analysis Scope
Analyze the following components from the input files:
1. **Program Structure**: Main program flow and entry points
2. **Dependencies**: External calls, copybooks, and includes
3. **Variables**: Data structures, working storage, and linkage sections
4. **Functions**: Subroutines, paragraphs, and perform statements
5. **Control Flow**: Decision points, loops, and conditional logic

## Analysis Logic Instructions

### Step 1: Program Overview Analysis
- Identify program type (CICS, batch, subroutine) and main purpose
- Extract transaction ID, program ID, and functional description
- Map entry points and program initialization logic
- Document external dependencies and file access patterns

### Step 2: Main Flow Structure
- Trace execution from entry point through main logic branches
- Map all EVALUATE/IF decision structures with conditions
- Identify loop constructs (PERFORM UNTIL, PERFORM VARYING)
- Document program termination points (RETURN, XCTL, GOBACK)

### Step 3: Function Call Hierarchy
- Map all PERFORM statements to target paragraphs
- Trace function call chains and dependencies
- Identify conditional vs unconditional function calls
- Document function parameters and return patterns

### Step 4: Data and Error Flow
- Track data movement between functions and external programs
- Map error handling patterns and message flows
- Document screen/file I/O operations and response handling
- Trace communication area and parameter passing

## Output Requirements

### Document Structure
Generate a comprehensive flow analysis document that includes:

#### 1. Program Overview Section
- Program identification and purpose
- Transaction ID and program type
- Key dependencies and external resources

#### 2. High-Level Program Flow Diagram
- Visual ASCII flow chart showing complete program execution
- Decision points with clear condition labels
- All termination paths and program transfers
- Integration points with external programs

#### 3. Detailed Function Call Flow
- Function call relationships with conditions
- Call hierarchy and dependency mapping
- Critical execution paths and program transfers

#### 4. Function Interaction Matrix
- Tabular view of function relationships
- Call conditions and parameters
- Return patterns and data flow

#### 5. Error Handling and Success Paths
- Error condition flows with specific messages
- Success path routing and program transfers
- Data validation and user interaction patterns

## Template Format

```markdown
# {PROGRAM_NAME} Program Flow Analysis

## Program Overview
- **Program**: {PROGRAM_NAME}.cbl
- **Function**: {PROGRAM_PURPOSE}
- **Transaction ID**: {TRANSACTION_ID}
- **Type**: {PROGRAM_TYPE}

## High-Level Program Flow Diagram

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           {PROGRAM_NAME} PROGRAM FLOW                      │
└─────────────────────────────────────────────────────────────────────────────┘

                                    START
                                    │
                                    ▼
                            ┌─────────────┐
                            │ {ENTRY_PARA}│◄─── Entry Point
                            └─────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ {INITIALIZATION_LOGIC}  │
                        └─────────────────────────┘
                                    │
                                    ▼
                            ┌─────────────┐
                            │{MAIN_COND}? │
                            └─────────────┘
                                │     │
                            YES  │     │ NO
                                ▼     ▼
                    ┌─────────────────┐   ┌─────────────────┐
                    │ {TRUE_ACTION}   │   │ {FALSE_ACTION}  │
                    └─────────────────┘   └─────────────────┘
                            │                     │
                            ▼                     ▼
                        {CONTINUE_FLOW}       {ALTERNATE_FLOW}
```

## Detailed Function Call Flow

### 1. {MAIN_FUNCTION} Function Call Relationships

```
{MAIN_FUNCTION}
├── Conditional Calls:
│   ├── {FUNCTION_1} (when {CONDITION_1})
│   ├── {FUNCTION_2} (when {CONDITION_2})
│   └── {FUNCTION_N} (when {CONDITION_N})
└── Always Calls:
    └── {ALWAYS_CALLED_FUNCTION}
```

**Call Conditions:**
- `{MAIN_FUNCTION}` calls `{FUNCTION_1}` when `{DETAILED_CONDITION_1}`
- `{MAIN_FUNCTION}` calls `{FUNCTION_2}` when `{DETAILED_CONDITION_2}`

### 2. {SECONDARY_FUNCTION} Function Flow

```
                            {SECONDARY_FUNCTION}
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ {OPERATION_1}           │
                        └─────────────────────────┘
                                    │
                                    ▼
                        ┌─────────────────────────┐
                        │ EVALUATE {CONDITION}:   │
                        │ - {VALUE_1}             │
                        │ - {VALUE_2}             │
                        │ - OTHER                 │
                        └─────────────────────────┘
                                    │
                    ┌─────────────────┼─────────────────┐
                    ▼                 ▼                 ▼
        ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
        │ {ACTION_1}      │ │ {ACTION_2}      │ │ {ACTION_3}      │
        └─────────────────┘ └─────────────────┘ └─────────────────┘
                    │                 │                 │
                    └─────────────────┼─────────────────┘
                                    │
                                    ▼
                                    RETURN
```

## Function Interaction Matrix

| Function | Calls | Called By | Conditions |
|----------|-------|-----------|------------|
| {FUNCTION_1} | {CALLS_LIST} | {CALLED_BY_LIST} | {CONDITIONS} |
| {FUNCTION_2} | {CALLS_LIST} | {CALLED_BY_LIST} | {CONDITIONS} |

## Error Handling Flow

```
Error Conditions → Error Messages → Screen Actions
    │                   │              │
    ▼                   ▼              ▼
┌─────────────┐  ┌─────────────────┐  ┌─────────────────┐
│ {ERROR_1}   │  │ "{ERROR_MSG_1}" │  │ {ACTION_1}      │
└─────────────┘  └─────────────────┘  └─────────────────┘
```

## Success Path Flow

```
{SUCCESS_CONDITION} → {VALIDATION} → {USER_TYPE_CHECK} → {PROGRAM_TRANSFER}
        │                 │              │                │
        ▼                 ▼              ▼                ▼
┌─────────────┐  ┌─────────────────┐  ┌─────────────┐  ┌─────────────┐
│ {INPUT}     │  │ {VALIDATION}    │  │ {CHECK}     │  │ {TRANSFER}  │
└─────────────┘  └─────────────────┘  └─────────────┘  └─────────────┘
```

## Data Flow Through Functions

### Input Data Flow
```
{INPUT_SOURCE} → {PROCESSING_STEP} → {VALIDATION} → {STORAGE}
```

### Output Data Flow
```
{DATA_SOURCE} → {PROCESSING} → {OUTPUT_FORMAT} → {DESTINATION}
```

### Communication Area Flow
```
{COMMAREA_STRUCTURE} ← {DATA_POPULATION} ← {SOURCE_FUNCTION}
```

## Program Termination Points

1. **{TERMINATION_TYPE_1}**: 
- {TERMINATION_DESCRIPTION}
- **CRITICAL**: {IMPORTANT_NOTE}

2. **{TERMINATION_TYPE_2}**:
- {TERMINATION_DESCRIPTION}

## Integration with External Programs

### Upstream Integration
- **{CALLING_PROGRAM}**: {DESCRIPTION}

### Downstream Integration
- **{CALLED_PROGRAM_1}**: {DESCRIPTION}
- **{CALLED_PROGRAM_2}**: {DESCRIPTION}

### External Resources
- **{RESOURCE_1}**: {DESCRIPTION}
- **{RESOURCE_2}**: {DESCRIPTION}
```

## Quality Checklist

### Completeness Verification
- [ ] All paragraphs from source code are documented in flow charts
- [ ] All PERFORM statements are traced with proper conditions
- [ ] All decision structures (EVALUATE/IF) are visualized with branches
- [ ] All program termination points are clearly marked
- [ ] All external program calls (XCTL, CALL) are documented
- [ ] All file/database operations are included in flow

### Flow Chart Accuracy
- [ ] ASCII flow chart symbols are consistent (┌─┐│▼►)
- [ ] Decision points use proper condition labels
- [ ] All execution paths lead to proper termination
- [ ] Function call hierarchy matches source code
- [ ] Error handling paths are complete and accurate
- [ ] Data flow between functions is correctly mapped

### Source Code Validation
- [ ] Every function name in charts exists in source code
- [ ] Every condition matches actual program logic
- [ ] Every variable reference exists in data definitions
- [ ] Every system command is accurately represented
- [ ] No assumptions made about missing code logic
- [ ] All flow elements traceable to specific source lines

### Documentation Standards
- [ ] Program overview section is complete and accurate
- [ ] Function interaction matrix includes all relationships
- [ ] Error handling section covers all error conditions
- [ ] Success path documentation is comprehensive
- [ ] Integration points with external programs are documented
- [ ] Critical execution notes are highlighted

## Usage Instructions

1. **Preparation**: Replace `{PROGRAM_NAME}` with actual program identifier
2. **Analysis**: Load and analyze the three reference files (Dependencies, VariableList, FunctionList)
3. **Generation**: Follow the analysis steps and generate flow charts using the template format
4. **Validation**: Complete the quality checklist to ensure accuracy
5. **Verification**: Cross-reference all elements against source files to prevent hallucination

**Critical Notes:**
- Every element in the flow chart must exist in the actual source code
- Use exact paragraph names, variable names, and condition logic from source
- Document all program termination points and transfer operations
- Include comprehensive error handling and success path documentation