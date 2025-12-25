Please use the prompt template to generate a comprehensive System Analysis/System Design for the program, 
and provide me the result (Carddemo_SA_SD.md).

# System Analysis/System Design (SA/SD) Document Generation Prompt Template

## Input Requirements
Generate a comprehensive System Analysis/System Design document for CardDemo mainframe application programs based on the following reference files:

### Required Reference Files (if they exist in app/ folder):
- **Dependencies**: `[PROGRAM_NAME]_Dependencies.md`
- **Flow Charts**: `[PROGRAM_NAME]_Flows.md` or `[PROGRAM_NAME]_FlowChart.md`
- **Function Lists**: `[PROGRAM_NAME]_Functions.md` or `[PROGRAM_NAME]_FunctionList.md`

### Additional Supporting Files (if available):
- **Business Rules**: `[PROGRAM_NAME]_BusinessRules.md`
- **Variable Lists**: `[PROGRAM_NAME]_VariableList.md`
- **ERD**: `[PROGRAM_NAME]_ERD.md`

## Analysis Logic and Constraints

### Critical Requirements:
1. **FACTUAL ONLY**: Use only information explicitly documented in the provided reference files
2. **NO ASSUMPTIONS**: Do not infer, assume, or hallucinate any system behaviors or relationships
3. **NO EXTERNAL KNOWLEDGE**: Do not use general mainframe or COBOL knowledge beyond what's documented
4. **VISUAL PRESENTATION**: Present information using charts, tables, diagrams, and structured formats
5. **CROSS-REFERENCE**: Always reference the source file for each piece of information

### Analysis Approach:
- Extract and synthesize information from reference files only
- Present actual code behaviors and documented system interactions
- Use visual elements to enhance understanding
- Maintain traceability to source documentation

## Output Document Structure

### Document Header
```
# System Analysis/System Design Document
## Program: [PROGRAM_NAME]
## Application: CardDemo
## Generated Date: [CURRENT_DATE]
## Reference Files Used: [LIST_OF_FILES]
```

### 1. System Overview

#### 1.1 Program Summary
- **Program Name**: [From reference files]
- **Program Type**: [From Dependencies file]
- **Transaction ID**: [From Dependencies file]
- **Primary Function**: [From Dependencies/Functions files]
- **Application Context**: [From reference files]

#### 1.2 System Architecture and Component Relationships
Create a visual diagram showing program relationships and data flow:
```
┌─────────────────────────────────────────────────────────────────────────────┐
│                          [PROGRAM_NAME] SYSTEM ARCHITECTURE                 │
└─────────────────────────────────────────────────────────────────────────────┘

                            ┌─────────────┐
                            │ Entry Point │
                            │[PROGRAM_NAME]│
                            │ ([TRAN_ID]) │
                            └──────┬──────┘
                                    │
                    ┌────────────────┼────────────────┐
                    │                │                │
                    ▼                ▼                ▼
            ┌─────────────┐  ┌─────────────┐  ┌─────────────┐
            │Input Layer  │  │Process Layer│  │Output Layer │
            │[BMS Maps]   │  │[Functions]  │  │[Targets]    │
            │[Copybooks]  │  │[Paragraphs] │  │[Files]      │
            └─────────────┘  └─────────────┘  └─────────────┘
                    │                │                │
                    └────────────────┼────────────────┘
                                    │
                            ┌─────────────┐
                            │ Data Layer  │
                            │[VSAM Files] │
                            │[DB2 Tables] │
                            └─────────────┘
```

#### 1.3 Technology Stack and Dependencies
Create a dependency matrix table:
| Dependency Type | Component Name | Source File | Usage Context | Critical Level |
|----------------|----------------|-------------|---------------|----------------|
| [Extract from Dependencies file] | | | | |

### 2. Business Flow Analysis

#### 2.1 End-to-End Process Flows
For each program flow documented in Flow Charts file:

**Flow Name**: [From Flow Charts file]
```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           [FLOW_NAME] PROCESS                               │
└─────────────────────────────────────────────────────────────────────────────┘

[Entry Point] → [Validation] → [Processing] → [Output/Transfer]
    │              │             │              │
    ▼              ▼             ▼              ▼
[Initial Setup] [Input Check] [Business Logic] [Result Display]
    │              │             │              │
    └──────────────┼─────────────┼──────────────┘
                    │             │
                    ▼             ▼
            [Error Handling] [Success Path]
```

**Flow Description**: [From Flow Charts file]
- **Trigger**: [From Flow Charts file]
- **Input**: [From Flow Charts file]  
- **Processing Steps**: [From Flow Charts file]
- **Output**: [From Flow Charts file]
- **Error Handling**: [From Flow Charts file]

#### 2.2 User Interaction Patterns and Navigation Paths

**Navigation Patterns Table:**
| From Program | To Program | Trigger | Method | Data Passed | Source Reference |
|--------------|------------|---------|---------|-------------|------------------|
| [Extract from Flow Charts file] | | | | | |

**Key Mappings Table:**
| Key | Function | Available In | Action | Source Reference |
|-----|----------|--------------|--------|------------------|
| [Extract from Flow Charts file] | | | | |

**Screen Flow Patterns:**
```
Pattern 1: [Pattern Name from Flow Charts]
[Entry] → [Validation] → [Processing] → [Display/Transfer]

Pattern 2: [Pattern Name from Flow Charts]  
[Search] → [Fetch] → [Display] → [Edit] → [Validate] → [Update]
```

#### 2.3 Program Integration Points
```
Integration Flow Diagram:
┌─────────────────────────────────────────────────────────────────────────────┐
│                        [PROGRAM_NAME] INTEGRATION                           │
└─────────────────────────────────────────────────────────────────────────────┘

[PROGRAM_NAME]
├── Called By: [From Dependencies file]
├── Calls: [From Dependencies file]
├── XCTL To: [From Flow Charts file]
├── Returns To: [From Flow Charts file]
└── Data Access: [From Dependencies file]
    ├── VSAM Files: [List files]
    ├── Copybooks: [List copybooks]
    └── BMS Maps: [List maps]
```

### 3. Functional Analysis

#### 3.1 Function Breakdown by Program
Create detailed function table from Functions file:
| Function Name | Type | Line Numbers | CICS Operations | Purpose | Complexity | Source |
|---------------|------|--------------|-----------------|---------|------------|--------|
| [Extract from Functions file] | | | | | | |

#### 3.2 Input/Output Specifications

**Input Specifications**:
| Input Type | Name | Source | Format | Validation Rules | Reference |
|------------|------|--------|--------|------------------|-----------|
| [Extract from Functions/Flow files] | | | | | |

**Output Specifications**:
| Output Type | Name | Destination | Format | Business Rules | Reference |
|-------------|------|-------------|--------|----------------|-----------|
| [Extract from Functions/Flow files] | | | | | |

#### 3.3 State Management and Control Flow
| State/Mode | Purpose | Entry Condition | Exit Condition | Source Reference |
|------------|---------|-----------------|----------------|------------------|
| [Extract from Functions file] | | | | |

#### 3.4 Business Rules and Validation Logic
Create business rules matrix:
| Rule Category | Rule Description | Implementation Location | Error Handling | Source Reference |
|---------------|------------------|------------------------|----------------|------------------|
| [Extract from Business Rules/Functions files] | | | | |

### 4. Data Flow Analysis

#### 4.1 Data Access Patterns
| Data Store | Access Type | Function | Purpose | Source Reference |
|------------|-------------|----------|---------|------------------|
| [Extract from Dependencies file] | | | | |

#### 4.2 Communication Areas and Data Structures
```
Data Structure Diagram:
[Extract from Dependencies/Variable files if available]
```

### 5. Error Handling and Exception Management

#### 5.1 Error Scenarios
| Error Type | Trigger Condition | Error Message | Recovery Action | Source Reference |
|------------|-------------------|---------------|-----------------|------------------|
| [Extract from Flow Charts/Functions files] | | | | |

#### 5.2 Exception Flow Paths
```
Exception Handling Flow:
[Extract from Flow Charts file]
```

### 6. Performance and Resource Considerations

#### 6.1 Resource Usage Analysis
| Resource Type | Usage Pattern | Impact Level | Optimization Notes | Source Reference |
|---------------|---------------|--------------|-------------------|------------------|
| [Extract from Dependencies file] | | | | |

#### 6.2 Transaction Volume and Throughput
| Transaction Type | Expected Volume | Response Time | Resource Impact | Source Reference |
|------------------|-----------------|---------------|-----------------|------------------|
| [Extract from Functions file] | | | | |

### 7. Security and Access Control

#### 7.1 Security Mechanisms
| Security Feature | Implementation | Access Level | Source Reference |
|------------------|----------------|--------------|------------------|
| [Extract from Functions/Dependencies files] | | | |

#### 7.2 Data Protection and Privacy
| Data Type | Protection Method | Access Control | Compliance Notes | Source Reference |
|-----------|-------------------|----------------|------------------|------------------|
| [Extract from Functions file] | | | | |

### 8. Code Quality and Technical Debt Analysis

#### 8.1 Code Issues Identified
| Issue Type | Location | Description | Impact | Recommendation | Source Reference |
|------------|----------|-------------|--------|----------------|------------------|
| [Extract from Functions file] | | | | | |

#### 8.2 Technical Debt Assessment
| Debt Category | Severity | Effort to Fix | Business Impact | Priority | Source Reference |
|---------------|----------|---------------|-----------------|----------|------------------|
| [Extract from Functions file] | | | | | |

### 9. Summary and Verification

#### 9.1 System Summary
[Provide concise summary of the system based on documented facts]

#### 9.2 Key Findings
- **Strengths**: [List documented positive aspects]
- **Areas for Improvement**: [List documented issues or gaps]
- **Critical Dependencies**: [List key dependencies from analysis]

#### 9.3 Document Verification Statement
**Key Verification Points:**
- Dependency counts verified against actual file lists
- Program flow diagrams match actual code execution paths
- Function descriptions reflect actual implementation
- Business rules derived from actual validation logic
- Integration patterns confirmed through code analysis
- Code issues documented where identified in source analysis

## Quality Assurance Checklist

### Pre-Generation Verification:
- [ ] All required reference files are available and readable
- [ ] Reference files contain the expected sections and data
- [ ] No placeholder or template content remains in reference files

### Content Accuracy Checklist:
- [ ] All information is directly extracted from reference files
- [ ] No assumptions or inferences made beyond documented facts
- [ ] All tables and diagrams reference source files
- [ ] Cross-references are accurate and complete
- [ ] No external knowledge or general practices included

### Completeness Verification:
- [ ] System Overview section completed with factual data
- [ ] Business Flow section includes all documented flows
- [ ] Functional Analysis covers all documented functions
- [ ] All dependency relationships documented
- [ ] Error handling scenarios captured
- [ ] Visual elements (tables, diagrams) properly formatted

### Anti-Hallucination Controls:
- [ ] Every statement can be traced to a reference file
- [ ] No "typical" or "standard" practices mentioned
- [ ] No assumptions about undocumented features
- [ ] All technical details verified against source files
- [ ] No interpolation between documented facts

### Final Review:
- [ ] Document structure follows template exactly
- [ ] All sections contain factual content only
- [ ] Visual presentations enhance understanding
- [ ] Source references are complete and accurate
- [ ] No speculative or assumed content present

## Template Usage Instructions

1. **Replace Placeholders**: Replace all `[PROGRAM_NAME]`, `[TRAN_ID]`, `[FLOW_NAME]` and other bracketed placeholders with actual values from reference files
2. **Extract Information**: Copy information directly from reference files - do not paraphrase unless necessary for clarity
3. **Create Visual Diagrams**: Use the provided ASCII art templates and populate with actual program flow information
4. **Maintain Traceability**: Always include source file references for each piece of information
5. **Use Visual Elements**: Convert text information into tables, diagrams, and structured formats where possible
6. **Document State Patterns**: For complex programs, identify and document state management patterns
7. **Capture Integration Points**: Document all program-to-program communication and data sharing
8. **Verify Completeness**: Ensure all available information from reference files is included
9. **Quality Check**: Use the checklist to verify no hallucinated content is included
10. **Document Code Issues**: Include any code quality issues or technical debt identified during analysis

## Output File Naming Convention
Save the generated document as: `[PROGRAM_NAME]_SA_SD.md`

## Notes for General Usage
- This template is designed for any CardDemo mainframe program with enhanced visual presentation
- Sections will be populated based on available reference files with improved ASCII art diagrams
- If a reference file doesn't exist, mark that section as "Not Available - Reference file not found"
- The template scales from single program analysis to multi-program system analysis
- Visual elements include flow diagrams, integration charts, and structured tables
- State management patterns are captured for complex CICS programs
- Code quality issues and technical debt are documented for improvement planning
- All content must be traceable to source documentation with no assumptions or hallucinations