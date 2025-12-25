Please use the prompt template to extract all business rules for program {PROGRAM_NAME}, 
and provide me the result ({PROGRAM_NAME}_BusinessRules.md).

# Business Rules Analysis Template

## Input Requirements

### Primary Source Files
- **Target Program**: `{PROGRAM_NAME}` (e.g., COSGN00C, COACTUPC, CBTRN02C)
- **Program Location**: `app/cbl/{PROGRAM_NAME}.cbl`
- **Dependencies Reference**: `app/{PROGRAM_NAME}_Dependencies.md`
- **Function Reference**: `app/{PROGRAM_NAME}_FunctionList.md`

### Supporting Materials
- Copybook files from `app/cpy/` (data structure definitions)
- BMS mapset definitions from `app/bms/` (screen validation rules)
- Related programs identified in dependencies
- VSAM file structures and validation logic
- Error message definitions (CSMSG01Y, CSMSG02Y)

## Analysis Logic

### Step 1: Business Rule Categories
Identify and categorize business rules by type:

1. **Authentication & Security Rules**
   - User ID validation patterns
   - Password requirements and verification
   - User type restrictions (Admin vs Regular User)
   - Session management rules

2. **Data Validation Rules**
   - Field format requirements (numeric, alphanumeric, dates)
   - Field length constraints
   - Required field validations
   - Cross-field validation dependencies

3. **Business Process Rules**
   - Transaction processing logic
   - Account balance calculations
   - Credit limit enforcement
   - Interest calculation rules
   - Bill payment processing

4. **System Integration Rules**
   - File access permissions
   - Program transfer conditions
   - CICS transaction flow rules
   - Error handling and recovery procedures

5. **Data Integrity Rules**
   - Record key uniqueness requirements
   - Referential integrity between files
   - Data consistency checks
   - Audit trail requirements

### Step 2: Rule Extraction Methods
Extract business rules from:
- **EVALUATE/IF statements**: Decision logic and conditions
- **88-level condition names**: Business value definitions
- **PERFORM statements**: Conditional processing rules
- **File I/O operations**: Data access and update rules
- **Error handling**: Exception conditions and responses
- **Screen validation**: Input field requirements
- **Calculation logic**: Mathematical business rules

### Step 3: Rule Documentation Standards
For each identified rule, document:
- Rule category and unique identifier
- Business description in plain language
- Technical implementation details
- Conditions that trigger the rule
- Actions taken when rule is violated
- Source location in code (line numbers)
- Related copybooks or data structures

## Output Format

### Business Rules Structure

```markdown
# Business Rules Analysis for {PROGRAM_NAME}

## Program Overview
- **Program**: {PROGRAM_NAME}.cbl
- **Function**: {PROGRAM_PURPOSE}
- **Transaction ID**: {TRANSACTION_ID}
- **Business Domain**: {DOMAIN_AREA}

## Business Rules Summary

### Rule Categories Overview
| Category | Rule Count | Complexity | Critical Level |
|----------|------------|------------|----------------|
| Authentication & Security | {count} | {HIGH/MEDIUM/LOW} | {CRITICAL/HIGH/MEDIUM} |
| Data Validation | {count} | {HIGH/MEDIUM/LOW} | {CRITICAL/HIGH/MEDIUM} |
| Business Process | {count} | {HIGH/MEDIUM/LOW} | {CRITICAL/HIGH/MEDIUM} |
| System Integration | {count} | {HIGH/MEDIUM/LOW} | {CRITICAL/HIGH/MEDIUM} |
| Data Integrity | {count} | {HIGH/MEDIUM/LOW} | {CRITICAL/HIGH/MEDIUM} |

## Detailed Business Rules

### 1. Authentication & Security Rules

#### Rule AS-001: User Authentication
- **Description**: {business_rule_description}
- **Implementation**: {technical_implementation}
- **Trigger Condition**: {when_rule_applies}
- **Validation Logic**: {validation_steps}
- **Success Action**: {what_happens_on_success}
- **Failure Action**: {what_happens_on_failure}
- **Error Messages**: {specific_error_messages}
- **Source Location**: Lines {line_start}-{line_end}
- **Related Copybooks**: {copybook_references}

#### Rule AS-002: User Type Authorization
- **Description**: {business_rule_description}
- **Implementation**: {technical_implementation}
- **Conditions**: 
  - Admin User: {admin_conditions}
  - Regular User: {regular_user_conditions}
- **Access Controls**: {what_each_type_can_access}
- **Source Location**: Lines {line_start}-{line_end}

### 2. Data Validation Rules

#### Rule DV-001: Required Field Validation
- **Description**: {business_rule_description}
- **Required Fields**: 
  - {field_name}: {validation_criteria}
  - {field_name}: {validation_criteria}
- **Validation Logic**: {how_validation_is_performed}
- **Error Handling**: {error_response_for_missing_fields}
- **Source Location**: Lines {line_start}-{line_end}

#### Rule DV-002: Field Format Validation
- **Description**: {business_rule_description}
- **Format Requirements**:
  - Numeric Fields: {numeric_validation_rules}
  - Date Fields: {date_format_requirements}
  - Alphanumeric Fields: {text_validation_rules}
- **Implementation**: {technical_validation_method}
- **Source Location**: Lines {line_start}-{line_end}

#### Rule DV-003: Field Length Constraints
- **Description**: {business_rule_description}
- **Length Requirements**:
  - {field_name}: {min_length} to {max_length} characters
  - {field_name}: {exact_length} characters required
- **Validation Method**: {how_length_is_checked}
- **Source Location**: Lines {line_start}-{line_end}

### 3. Business Process Rules

#### Rule BP-001: Transaction Processing Logic
- **Description**: {business_rule_description}
- **Processing Steps**:
  1. {step_1_description}
  2. {step_2_description}
  3. {step_n_description}
- **Business Conditions**: {when_processing_occurs}
- **Calculation Rules**: {mathematical_formulas_or_logic}
- **Source Location**: Lines {line_start}-{line_end}

#### Rule BP-002: Account Balance Management
- **Description**: {business_rule_description}
- **Balance Calculation**: {how_balance_is_calculated}
- **Credit Limit Rules**: {credit_limit_enforcement}
- **Overdraft Handling**: {what_happens_on_insufficient_funds}
- **Source Location**: Lines {line_start}-{line_end}

#### Rule BP-003: Interest Calculation
- **Description**: {business_rule_description}
- **Interest Rate Logic**: {how_rates_are_determined}
- **Calculation Frequency**: {when_interest_is_calculated}
- **Compounding Rules**: {simple_or_compound_interest}
- **Source Location**: Lines {line_start}-{line_end}

### 4. System Integration Rules

#### Rule SI-001: Program Transfer Conditions
- **Description**: {business_rule_description}
- **Transfer Triggers**:
  - To {target_program}: {condition_for_transfer}
  - To {target_program}: {condition_for_transfer}
- **Data Passing Rules**: {what_data_is_passed}
- **Source Location**: Lines {line_start}-{line_end}

#### Rule SI-002: File Access Permissions
- **Description**: {business_rule_description}
- **File Access Rules**:
  - {file_name}: {read/write/update_permissions}
  - {file_name}: {access_conditions}
- **Concurrent Access**: {how_concurrent_access_is_handled}
- **Source Location**: Lines {line_start}-{line_end}

#### Rule SI-003: Error Recovery Procedures
- **Description**: {business_rule_description}
- **Error Categories**:
  - System Errors: {how_system_errors_are_handled}
  - Business Errors: {how_business_errors_are_handled}
  - User Errors: {how_user_errors_are_handled}
- **Recovery Actions**: {what_recovery_steps_are_taken}
- **Source Location**: Lines {line_start}-{line_end}

### 5. Data Integrity Rules

#### Rule DI-001: Record Key Uniqueness
- **Description**: {business_rule_description}
- **Key Fields**: {which_fields_form_unique_keys}
- **Uniqueness Enforcement**: {how_uniqueness_is_enforced}
- **Duplicate Handling**: {what_happens_with_duplicates}
- **Source Location**: Lines {line_start}-{line_end}

#### Rule DI-002: Referential Integrity
- **Description**: {business_rule_description}
- **Relationships**:
  - {parent_entity} → {child_entity}: {relationship_rule}
  - {entity_1} ↔ {entity_2}: {cross_reference_rule}
- **Integrity Checks**: {how_integrity_is_verified}
- **Source Location**: Lines {line_start}-{line_end}

#### Rule DI-003: Data Consistency Requirements
- **Description**: {business_rule_description}
- **Consistency Rules**:
  - {field_1} must match {field_2} when {condition}
  - {calculated_field} = {formula_or_source}
- **Validation Timing**: {when_consistency_is_checked}
- **Source Location**: Lines {line_start}-{line_end}

## Business Rule Dependencies

### Cross-Program Rule Dependencies
| Rule ID | Depends On Program | Dependency Type | Description |
|---------|-------------------|-----------------|-------------|
| {rule_id} | {program_name} | {CALLS/XCTL/DATA} | {dependency_description} |

### Data Structure Dependencies
| Rule ID | Copybook | Field Dependencies | Description |
|---------|----------|-------------------|-------------|
| {rule_id} | {copybook_name} | {field_list} | {how_rule_uses_structure} |

## Business Rule Violations and Responses

### Error Handling Matrix
| Rule Category | Violation Type | Error Code | Error Message | Recovery Action |
|---------------|----------------|------------|---------------|-----------------|
| {category} | {violation_type} | {error_code} | "{error_message}" | {recovery_steps} |

### User Experience Impact
| Rule ID | User Impact | Screen Response | Navigation Effect |
|---------|-------------|-----------------|-------------------|
| {rule_id} | {impact_description} | {screen_changes} | {where_user_goes} |

## Business Value and Compliance

### Regulatory Compliance
- **Financial Regulations**: {which_rules_support_compliance}
- **Data Protection**: {privacy_and_security_rules}
- **Audit Requirements**: {rules_that_support_auditing}

### Business Value Protection
- **Revenue Protection**: {rules_that_protect_revenue}
- **Risk Mitigation**: {rules_that_reduce_business_risk}
- **Customer Experience**: {rules_that_improve_user_experience}

## Implementation Quality Assessment

### Rule Completeness Analysis
- **Well-Implemented Rules**: {count_and_examples}
- **Incomplete Rules**: {gaps_or_missing_validations}
- **Inconsistent Rules**: {conflicting_or_unclear_rules}

### Technical Debt in Business Rules
- **Hard-coded Values**: {business_rules_with_hard_coded_values}
- **Complex Logic**: {overly_complex_rule_implementations}
- **Maintenance Issues**: {rules_that_are_difficult_to_maintain}

## Recommendations for Rule Enhancement

### Priority 1 - Critical Issues
1. **{issue_description}**: {recommendation_and_impact}
2. **{issue_description}**: {recommendation_and_impact}

### Priority 2 - Improvements
1. **{improvement_description}**: {benefit_and_effort}
2. **{improvement_description}**: {benefit_and_effort}

### Priority 3 - Optimizations
1. **{optimization_description}**: {efficiency_gain}
2. **{optimization_description}**: {efficiency_gain}
```

## Quality Assurance Checklist

### Rule Identification Completeness
- [ ] All EVALUATE/IF statements analyzed for business logic
- [ ] All 88-level condition names documented as business rules
- [ ] All validation logic extracted from screen handling
- [ ] All file I/O operations checked for business constraints
- [ ] All error handling reviewed for business rule violations
- [ ] All calculation logic documented as business rules

### Rule Documentation Accuracy
- [ ] Each rule has clear business description
- [ ] Technical implementation matches actual code
- [ ] Source line references are accurate
- [ ] Error messages match actual program messages
- [ ] Copybook references are correct and complete
- [ ] Rule categories are appropriate and consistent

### Business Logic Validation
- [ ] Rules reflect actual business requirements
- [ ] No assumptions made about undocumented behavior
- [ ] All conditional logic properly captured
- [ ] Cross-program dependencies identified
- [ ] Data integrity rules are comprehensive
- [ ] Security rules cover all access controls

### Anti-Hallucination Controls
- [ ] Every rule can be traced to specific code lines
- [ ] No "typical" or "standard" business practices assumed
- [ ] All error conditions verified in actual code
- [ ] Business descriptions match technical implementation
- [ ] No interpolation between documented facts
- [ ] All field names and values verified in copybooks

### Completeness Verification
- [ ] All major business functions covered by rules
- [ ] Authentication and authorization rules complete
- [ ] Data validation rules comprehensive
- [ ] Process flow rules documented
- [ ] Integration rules identified
- [ ] Error handling rules complete

## Template Usage Instructions

1. **Program Analysis**: Replace `{PROGRAM_NAME}` with actual program identifier
2. **Rule Extraction**: Systematically analyze code for each rule category
3. **Documentation**: Use exact field names, error messages, and conditions from source
4. **Validation**: Cross-reference all rules against actual program behavior
5. **Quality Check**: Complete the checklist to ensure no hallucinated content

**Critical Notes:**
- Every business rule must exist in the actual source code
- Use exact error messages and field names from the program
- Document line numbers for traceability
- Include comprehensive cross-references to copybooks and related programs
- Focus on business impact and value of each rule

## Notes for CardDemo Application

### Common Business Rule Patterns
- **User Authentication**: USRSEC file validation with user ID and password
- **User Type Authorization**: Admin vs Regular user access controls
- **Account Management**: Balance calculations, credit limits, transaction processing
- **Transaction Validation**: Amount limits, account status checks, referential integrity
- **Screen Validation**: Required fields, format checks, length constraints
- **File Integrity**: VSAM key uniqueness, cross-reference validation
- **Error Handling**: Standardized error messages and recovery procedures

### CardDemo-Specific Considerations
- **CICS Transaction Flow**: Rules governing program transfers (XCTL)
- **Communication Areas**: Data passing rules between programs
- **BMS Screen Handling**: Input validation and error display rules
- **VSAM File Access**: Record locking and concurrent access rules
- **Batch Processing**: Transaction posting and balance update rules
- **Security Integration**: RACF and user security file coordination

This template is designed to extract comprehensive business rules from any CardDemo program while maintaining strict adherence to actual code implementation and avoiding any assumptions or hallucinations.
