Please use the prompt template to find all the function list for program COSGN00C, 
and provide me the result (COSGN00C_FunctionList.md).

# Function List Analysis Template

## Input Requirements

### Primary Source Files
- **Target Program**: `{PROGRAM_NAME}` (e.g., COSGN00C)
- **Program Location**: `app/cbl/{PROGRAM_NAME}.cbl`
- **Dependencies Reference**: `app/{PROGRAM_NAME}_Dependencies.md`
- **Variable Reference**: `app/{PROGRAM_NAME}_VariableList.md`

### Supporting Materials
- Copybook files from `app/cpy/` and `app/cpy-bms/`
- BMS mapset definitions from `app/bms/`
- Related programs identified in dependencies
- CICS system copybooks (DFHAID, DFHBMSCA, etc.)

## Analysis Logic

### Step 1: Program Structure Analysis
1. Identify MAIN-PARA as primary entry point
2. Extract all paragraph names in PROCEDURE DIVISION (format: paragraph-name.)
3. Map PERFORM statements to target paragraphs
4. Trace EXEC CICS XCTL/LINK to external programs
5. Identify CICS commands and their context

### Step 2: Function Identification
1. Extract paragraphs as functional units (not sections in CICS programs)
2. Identify screen handling functions (SEND/RECEIVE operations)
3. Map data validation and business logic functions
4. Categorize by CICS operation type (BMS, file I/O, program control)
5. Identify error handling patterns

### Step 3: Function Documentation
For each identified paragraph, document:
- Paragraph name and line location
- CICS operations performed
- BMS maps or files accessed
- Variables modified or validated
- PERFORM relationships (caller/called)
- Error handling approach

## Output Format

### Function List Structure

```markdown
## Function Analysis for {PROGRAM_NAME}

### Function Summary
| Function Name | Type | CICS Operations | Purpose | Complexity |
|---------------|------|-----------------|---------|------------|
| {paragraph_name} | PARAGRAPH | {SEND/RECEIVE/READ/XCTL} | {brief_description} | {HIGH/MEDIUM/LOW} |

### Program Flow Diagram
```
MAIN-PARA (Entry Point)
├── {function_1} (Screen Display)
├── {function_2} (User Input Processing)
├── {function_3} (Data Validation)
└── {function_4} (Program Transfer)
```

### Detailed Function Documentation

#### Function: {PARAGRAPH_NAME}
- **Location**: Line {line_number} - {line_number}
- **Type**: PARAGRAPH
- **CICS Operations**: 
- {EXEC CICS command}: {purpose}
- **Description**: {detailed_description}
- **Function Flow**: 
1. {step_1}
2. {step_2}
3. {step_n}
- **Input Variables**: 
- {variable_name}: {description_and_source}
- **Output Variables**: 
- {variable_name}: {description_and_target}
- **BMS Maps Used**: 
- {map_name}: {operation_type}
- **Files Accessed**: 
- {file_name}: {read/WRITE/UPDATE}
- **Programs Called**: 
- {program_name}: {XCTL/LINK} - {purpose}
- **Called By**: 
- {calling_paragraph}: {PERFORM/direct_call}
- **Error Handling**: {error_processing_approach}
```

## Template Usage Instructions

### For CICS COBOL Program Analysis
1. Replace `{PROGRAM_NAME}` with actual program name (e.g., COSGN00C)
2. Ensure all input files are available: program source, dependencies, variable list
3. Focus on PARAGRAPH-level analysis (CICS programs typically don't use SECTIONs)
4. Document CICS operations as primary function characteristics
5. Map screen flow for BMS-based programs
6. Trace EIBAID processing for user interaction logic

### CICS-Specific Considerations
- **Entry Point**: Always starts with MAIN-PARA or similar main paragraph
- **Screen Handling**: Document SEND/RECEIVE MAP operations with map names
- **Program Control**: Track XCTL/LINK operations for program flow
- **Error Handling**: Document RESP/RESP2 code processing
- **Transaction Flow**: Document RETURN TRANSID for conversation management
- **Data Areas**: Include COMMAREA and DFHCOMMAREA usage

### Customization Guidelines
- Adjust complexity criteria: LOW (simple MOVE/IF), MEDIUM (CICS I/O), HIGH (complex business logic)
- Add CICS-specific categories: Screen Control, File I/O, Program Control, Validation
- Include user interaction patterns for online programs
- Reference CICS manuals for command documentation
- Document pseudo-conversational design patterns

## Self-Check Validation List

### Completeness Check
- [ ] MAIN-PARA entry point identified and documented
- [ ] All PARAGRAPH definitions identified (no SECTION usage in CICS programs)
- [ ] All PERFORM statements traced to target paragraphs
- [ ] All EXEC CICS XCTL/LINK statements documented with target programs
- [ ] All EXEC CICS commands categorized (BMS, file I/O, program control)
- [ ] Function hierarchy and PERFORM relationships mapped completely

### CICS-Specific Validation
- [ ] BMS SEND/RECEIVE operations documented with map names
- [ ] File I/O operations documented with dataset names
- [ ] EIBAID processing logic identified and documented
- [ ] COMMAREA usage and structure documented
- [ ] Transaction control (RETURN TRANSID) documented
- [ ] Error handling via RESP/RESP2 codes documented

### Accuracy Check
- [ ] Paragraph names match exactly with source code (case-sensitive)
- [ ] Line numbers are accurate and verified
- [ ] CICS command syntax correctly documented
- [ ] Variable names from BMS maps correctly identified (input/output suffixes)
- [ ] PERFORM relationships verified in both directions
- [ ] EVALUATE/IF logic flow accurately represented

### Quality Check
- [ ] Function descriptions reflect CICS business context
- [ ] No duplicate paragraph entries
- [ ] Screen flow logic clearly documented
- [ ] User interaction patterns identified
- [ ] Data validation functions properly categorized
- [ ] Program transfer logic (XCTL) clearly explained

### Anti-Hallucination Measures
- [ ] Every paragraph name verified against source code
- [ ] All line references double-checked
- [ ] CICS command parameters confirmed in actual code
- [ ] BMS map names verified in dependencies file
- [ ] File names confirmed in WORKING-STORAGE definitions
- [ ] Variable names confirmed in variable list
- [ ] No assumptions made about CICS behavior beyond code evidence
- [ ] Function flow steps match actual COBOL paragraph sequence
- [ ] EIBAID values match DFHAID copybook constants

### COBOL/CICS Integration Check
- [ ] Copybook inclusions properly documented
- [ ] System copybooks (DFHAID, DFHBMSCA) usage identified
- [ ] Working storage variables linked to function usage
- [ ] Condition names (88-level) usage documented
- [ ] MOVE statements and data flow documented
- [ ] EVALUATE statement logic properly mapped

### Final Validation
- [ ] Output format follows template structure exactly
- [ ] All placeholders replaced with actual values
- [ ] Cross-references between paragraphs validated
- [ ] CICS program flow documentation complete
- [ ] Ready for technical review and validation

## Notes for General Usage
- This template works for any COBOL program by replacing the program name
- Adjust the complexity criteria based on your organization's standards
- Add program-specific sections if the standard template doesn't cover unique aspects
- Use this template consistently across all program analyses for standardization