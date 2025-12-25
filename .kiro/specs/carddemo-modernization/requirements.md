# Requirements Document

## Introduction

This document specifies the requirements for modernizing the CardDemo mainframe application from COBOL/CICS/VSAM to a modern technology stack consisting of Java Spring Boot 3 (JDK 17) backend, Vue.js frontend, and PostgreSQL 16.9 database. The modernized application must replicate all behaviors of the original COBOL system while providing a modern, maintainable architecture suitable for cloud deployment.

## Glossary

- **CardDemo_System**: The complete credit card management application being modernized
- **Backend_Service**: Java Spring Boot 3 REST API services with JDK 17
- **Frontend_Application**: Vue.js single-page application for user interface
- **Database**: PostgreSQL 16.9 relational database management system
- **VSAM_File**: Virtual Storage Access Method file from mainframe (source system)
- **BMS_Map**: Basic Mapping Support screen definition from mainframe (source system)
- **CICS_Transaction**: Customer Information Control System transaction from mainframe (source system)
- **Communication_Area**: Data structure passed between programs (COMMAREA in COBOL)
- **User_Session**: Authenticated user context maintained across requests
- **Account_Record**: Customer account information entity
- **Customer_Record**: Customer personal information entity
- **Card_Record**: Credit card information entity
- **Transaction_Record**: Financial transaction information entity
- **Security_Record**: User authentication and authorization information entity

## Requirements

### Requirement 1: User Authentication and Authorization

**User Story:** As a system user, I want to securely authenticate with my credentials and access features appropriate to my role, so that I can perform authorized operations on the credit card management system.

#### Acceptance Criteria

1. WHEN a user submits a user ID and password, THE Backend_Service SHALL validate the credentials against the Security_Record in the Database
2. WHEN authentication succeeds, THE Backend_Service SHALL create a User_Session with user ID and user type (A for Admin or U for User)
3. WHEN authentication fails due to invalid user ID, THE Backend_Service SHALL return an error message "User not found. Try again ..."
4. WHEN authentication fails due to incorrect password, THE Backend_Service SHALL return an error message "Wrong Password. Try again ..."
5. WHEN a user ID field is empty or contains only spaces, THE Backend_Service SHALL return an error message "Please enter User ID ..."
6. WHEN a password field is empty or contains only spaces, THE Backend_Service SHALL return an error message "Please enter Password ..."
7. WHEN an authenticated admin user logs in, THE Backend_Service SHALL transfer control to the admin menu program (COADM01C equivalent)
8. WHEN an authenticated regular user logs in, THE Backend_Service SHALL transfer control to the main menu program (COMEN01C equivalent)

### Requirement 2: Main Menu Navigation

**User Story:** As an authenticated user, I want to navigate to different application features through a menu system, so that I can access the functionality I need.

#### Acceptance Criteria

1. WHEN a regular user successfully authenticates, THE Frontend_Application SHALL display the main menu with available options
2. WHEN an admin user successfully authenticates, THE Frontend_Application SHALL display the admin menu with administrative options
3. WHEN a user selects a menu option, THE Backend_Service SHALL validate the option is numeric and within the valid range (1-11)
4. WHEN a user selects an invalid menu option, THE Backend_Service SHALL return an error message "Please enter a valid option number..."
5. WHEN a user selects a valid menu option, THE Backend_Service SHALL navigate to the corresponding feature program
6. WHEN a user presses PF3 key, THE Backend_Service SHALL return to the sign-on screen
7. WHEN a user selects an admin-only option and the user is not an admin, THE Backend_Service SHALL return an error message "No access - Admin Only option... "

### Requirement 3: Account Viewing

**User Story:** As an authenticated user, I want to view account details including customer information and associated cards, so that I can review account status and information.

#### Acceptance Criteria

1. WHEN a user enters an account ID, THE Backend_Service SHALL validate the account ID is numeric and 11 digits
2. WHEN an account ID is empty or contains only spaces, THE Backend_Service SHALL return an error message "Account number not provided"
3. WHEN an account ID is all zeroes, THE Backend_Service SHALL return an error message "Account number must be a non zero 11 digit number"
4. WHEN an account ID is not numeric, THE Backend_Service SHALL return an error message "Account number must be a non zero 11 digit number"
5. WHEN a user searches for an account, THE Backend_Service SHALL retrieve the Account_Record from the ACCTDAT file
6. WHEN a user searches for an account, THE Backend_Service SHALL retrieve the associated Customer_Record from the CUSTDAT file
7. WHEN a user searches for an account, THE Backend_Service SHALL retrieve all associated Card_Record entries from the CARDDAT file using the account index
8. WHEN an account is not found in ACCTDAT, THE Backend_Service SHALL return an error message "Did not find this account in account master file"
9. WHEN a customer is not found in CUSTDAT, THE Backend_Service SHALL return an error message "Did not find associated customer in master file"
10. WHEN account data is retrieved successfully, THE Frontend_Application SHALL display account ID, active status, current balance, credit limit, cash credit limit, open date, expiration date, reissue date, current cycle credit, current cycle debit, customer name, and card information
11. WHEN a user presses PF3 key, THE Frontend_Application SHALL navigate back to the main menu

### Requirement 4: Account Updating

**User Story:** As an authenticated user, I want to update account and customer information, so that I can maintain accurate records in the system.

#### Acceptance Criteria

1. WHEN a user enters an account ID for update, THE Backend_Service SHALL validate the account ID is numeric and 11 digits
2. WHEN a user submits account updates, THE Backend_Service SHALL validate all modified fields according to their data type and business rules
3. WHEN account updates are valid, THE Backend_Service SHALL update the Account_Record in the Database
4. WHEN customer information is modified, THE Backend_Service SHALL update the Customer_Record in the Database
5. WHEN an update operation succeeds, THE Backend_Service SHALL return a success confirmation
6. WHEN an update operation fails due to validation errors, THE Backend_Service SHALL return field-specific error messages
7. WHEN a user presses the cancel function, THE Frontend_Application SHALL discard changes and return to the previous screen
8. WHEN a user presses the return function after successful update, THE Frontend_Application SHALL navigate to the account view screen

### Requirement 5: Card Management

**User Story:** As an authenticated user, I want to view, add, and update credit card information, so that I can manage cards associated with customer accounts.

#### Acceptance Criteria

1. WHEN a user views card list, THE Backend_Service SHALL retrieve all Card_Record entries for the specified account
2. WHEN a user selects a card, THE Frontend_Application SHALL display detailed card information including card number, status, and expiration date
3. WHEN a user adds a new card, THE Backend_Service SHALL validate card number format and uniqueness
4. WHEN a user updates card information, THE Backend_Service SHALL validate all modified fields
5. WHEN card operations succeed, THE Backend_Service SHALL persist changes to the Database
6. WHEN card operations fail, THE Backend_Service SHALL return appropriate error messages

### Requirement 6: Transaction Processing and Viewing

**User Story:** As an authenticated user, I want to view transaction history and process new transactions, so that I can track and manage account activity.

#### Acceptance Criteria

1. WHEN a user requests transaction history, THE Backend_Service SHALL retrieve Transaction_Record entries for the specified account or card
2. WHEN displaying transactions, THE Frontend_Application SHALL show transaction ID, date, amount, type, and description
3. WHEN a user filters transactions by date range, THE Backend_Service SHALL return only transactions within the specified range
4. WHEN a user processes a new transaction, THE Backend_Service SHALL validate transaction amount, type, and associated account
5. WHEN a transaction is processed, THE Backend_Service SHALL create a new Transaction_Record in the Database
6. WHEN a transaction is processed, THE Backend_Service SHALL update the account balance in the Account_Record

### Requirement 7: Bill Payment Processing

**User Story:** As an authenticated user, I want to process bill payments against customer accounts, so that I can record payment transactions.

#### Acceptance Criteria

1. WHEN a user initiates a bill payment, THE Backend_Service SHALL validate the account ID and payment amount
2. WHEN a payment amount is invalid or negative, THE Backend_Service SHALL return an error message
3. WHEN a payment is processed, THE Backend_Service SHALL create a payment Transaction_Record in the Database
4. WHEN a payment is processed, THE Backend_Service SHALL update the account balance in the Account_Record
5. WHEN a payment operation succeeds, THE Backend_Service SHALL return a confirmation with transaction details

### Requirement 8: User Management (Admin)

**User Story:** As an admin user, I want to manage system users including creating, updating, and viewing user accounts, so that I can control system access.

#### Acceptance Criteria

1. WHEN an admin user accesses user management, THE Backend_Service SHALL verify the user has admin privileges
2. WHEN a non-admin user attempts to access user management, THE Backend_Service SHALL return an authorization error
3. WHEN an admin creates a new user, THE Backend_Service SHALL validate user ID uniqueness and password requirements
4. WHEN an admin updates a user, THE Backend_Service SHALL validate all modified fields
5. WHEN user operations succeed, THE Backend_Service SHALL persist changes to the Security_Record in the Database
6. WHEN an admin views user list, THE Backend_Service SHALL return all Security_Record entries with sensitive data masked

### Requirement 9: Report Generation

**User Story:** As an authenticated user, I want to generate reports on accounts, transactions, and cards, so that I can analyze system data.

#### Acceptance Criteria

1. WHEN a user requests a report, THE Backend_Service SHALL validate report parameters including date ranges and filters
2. WHEN generating account reports, THE Backend_Service SHALL retrieve and aggregate Account_Record data
3. WHEN generating transaction reports, THE Backend_Service SHALL retrieve and aggregate Transaction_Record data
4. WHEN report generation succeeds, THE Backend_Service SHALL return formatted report data
5. WHEN report data exceeds size limits, THE Backend_Service SHALL implement pagination

### Requirement 10: Data Migration from VSAM to PostgreSQL

**User Story:** As a system administrator, I want to migrate all existing data from VSAM files to PostgreSQL database, so that the modernized system has complete historical data.

#### Acceptance Criteria

1. THE CardDemo_System SHALL provide migration utilities to extract data from VSAM_File sources
2. WHEN extracting USRSEC data, THE migration utility SHALL convert Security_Record entries to PostgreSQL format
3. WHEN extracting ACCTDAT data, THE migration utility SHALL convert Account_Record entries to PostgreSQL format
4. WHEN extracting CUSTDAT data, THE migration utility SHALL convert Customer_Record entries to PostgreSQL format
5. WHEN extracting CARDDAT data, THE migration utility SHALL convert Card_Record entries to PostgreSQL format
6. WHEN extracting TRANSACT data, THE migration utility SHALL convert Transaction_Record entries to PostgreSQL format
7. WHEN data conversion encounters errors, THE migration utility SHALL log errors and continue processing
8. WHEN migration completes, THE migration utility SHALL provide a summary report of records migrated and errors encountered

### Requirement 11: Database Schema Design

**User Story:** As a system architect, I want a normalized relational database schema, so that data integrity is maintained and queries are efficient.

#### Acceptance Criteria

1. THE Database SHALL implement a users table for Security_Record with columns: user_id (PK, VARCHAR(8)), first_name (VARCHAR(20)), last_name (VARCHAR(20)), password (VARCHAR(8)), user_type (CHAR(1))
2. THE Database SHALL implement an accounts table for Account_Record with columns: account_id (PK, NUMERIC(11)), active_status (CHAR(1)), current_balance (NUMERIC(10,2)), credit_limit (NUMERIC(10,2)), cash_credit_limit (NUMERIC(10,2)), open_date (VARCHAR(10)), expiration_date (VARCHAR(10)), reissue_date (VARCHAR(10)), current_cycle_credit (NUMERIC(10,2)), current_cycle_debit (NUMERIC(10,2)), address_zip (VARCHAR(10)), group_id (VARCHAR(10))
3. THE Database SHALL implement a customers table for Customer_Record with columns: customer_id (PK, NUMERIC(9)), first_name (VARCHAR(25)), middle_name (VARCHAR(25)), last_name (VARCHAR(25)), address_line_1 (VARCHAR(50)), address_line_2 (VARCHAR(50)), address_line_3 (VARCHAR(50)), state_code (CHAR(2)), country_code (CHAR(3)), zip_code (VARCHAR(10)), phone_number_1 (VARCHAR(15)), phone_number_2 (VARCHAR(15)), ssn (NUMERIC(9)), government_issued_id (VARCHAR(20)), date_of_birth (VARCHAR(10)), eft_account_id (VARCHAR(10)), primary_cardholder_indicator (CHAR(1)), fico_credit_score (NUMERIC(3))
4. THE Database SHALL implement a cards table for Card_Record with primary key on card_number (NUMERIC(16))
5. THE Database SHALL implement a transactions table for Transaction_Record with primary key on transaction_id
6. THE Database SHALL implement foreign key constraints between accounts and customers
7. THE Database SHALL implement foreign key constraints between cards and accounts
8. THE Database SHALL implement foreign key constraints between transactions and accounts
9. THE Database SHALL implement indexes on frequently queried fields including account_id, customer_id, and card_number

### Requirement 12: REST API Design

**User Story:** As a frontend developer, I want well-defined REST APIs, so that I can integrate the Vue.js application with backend services.

#### Acceptance Criteria

1. THE Backend_Service SHALL implement RESTful endpoints following standard HTTP methods (GET, POST, PUT, DELETE)
2. WHEN handling requests, THE Backend_Service SHALL accept and return JSON formatted data
3. WHEN operations succeed, THE Backend_Service SHALL return appropriate HTTP status codes (200, 201, 204)
4. WHEN operations fail, THE Backend_Service SHALL return appropriate HTTP error codes (400, 401, 404, 500)
5. WHEN returning errors, THE Backend_Service SHALL include descriptive error messages in the response body
6. THE Backend_Service SHALL implement CORS configuration to allow Frontend_Application requests
7. THE Backend_Service SHALL implement request validation using Spring Boot validation annotations

### Requirement 13: Session Management

**User Story:** As a system architect, I want secure session management, so that user authentication state is maintained across requests.

#### Acceptance Criteria

1. WHEN a user authenticates successfully, THE Backend_Service SHALL generate a session token
2. WHEN returning authentication response, THE Backend_Service SHALL include the session token
3. WHEN processing subsequent requests, THE Backend_Service SHALL validate the session token
4. WHEN a session token is invalid or expired, THE Backend_Service SHALL return HTTP 401 Unauthorized
5. WHEN a user logs out, THE Backend_Service SHALL invalidate the session token
6. THE Backend_Service SHALL implement session timeout after a configured period of inactivity

### Requirement 14: Error Handling and Logging

**User Story:** As a system administrator, I want comprehensive error handling and logging, so that I can troubleshoot issues and monitor system health.

#### Acceptance Criteria

1. WHEN exceptions occur, THE Backend_Service SHALL log error details including stack traces
2. WHEN business validation fails, THE Backend_Service SHALL log validation errors at INFO level
3. WHEN database operations fail, THE Backend_Service SHALL log errors at ERROR level
4. WHEN authentication fails, THE Backend_Service SHALL log security events at WARN level
5. THE Backend_Service SHALL implement structured logging with timestamps, log levels, and context information
6. THE Backend_Service SHALL not log sensitive information including passwords and full card numbers

### Requirement 15: Frontend User Interface

**User Story:** As an end user, I want an intuitive web interface, so that I can easily navigate and use the application.

#### Acceptance Criteria

1. THE Frontend_Application SHALL implement responsive design supporting desktop and tablet screen sizes
2. WHEN displaying forms, THE Frontend_Application SHALL provide clear field labels and validation feedback
3. WHEN validation errors occur, THE Frontend_Application SHALL display error messages near the relevant fields
4. WHEN operations are in progress, THE Frontend_Application SHALL display loading indicators
5. WHEN operations complete, THE Frontend_Application SHALL display success or error notifications
6. THE Frontend_Application SHALL implement navigation breadcrumbs showing the current location
7. THE Frontend_Application SHALL implement keyboard navigation support for accessibility

### Requirement 16: Batch Processing Migration

**User Story:** As a system administrator, I want batch processing capabilities equivalent to the original COBOL batch jobs, so that scheduled operations continue to function.

#### Acceptance Criteria

1. THE CardDemo_System SHALL implement scheduled jobs for account processing equivalent to CBACT01C, CBACT02C, CBACT03C, CBACT04C
2. THE CardDemo_System SHALL implement scheduled jobs for customer processing equivalent to CBCUS01C
3. THE CardDemo_System SHALL implement scheduled jobs for transaction processing equivalent to CBTRN01C, CBTRN02C, CBTRN03C
4. WHEN batch jobs execute, THE Backend_Service SHALL log start time, end time, and records processed
5. WHEN batch jobs encounter errors, THE Backend_Service SHALL log errors and continue processing remaining records
6. THE CardDemo_System SHALL provide configuration for batch job scheduling using Spring Batch or equivalent

### Requirement 17: Data Validation and Business Rules

**User Story:** As a business analyst, I want all original business rules enforced, so that data integrity is maintained in the modernized system.

#### Acceptance Criteria

1. WHEN validating account IDs, THE Backend_Service SHALL enforce 11-digit numeric format and reject all zeroes
2. WHEN validating card numbers, THE Backend_Service SHALL enforce 16-digit numeric format
3. WHEN validating dates, THE Backend_Service SHALL enforce YYYY-MM-DD format with valid year, month (1-12), and day ranges
4. WHEN validating currency amounts, THE Backend_Service SHALL enforce signed numeric format with two decimal places (S9(10)V99)
5. WHEN validating user IDs, THE Backend_Service SHALL enforce 8-character format and convert to uppercase
6. WHEN validating passwords, THE Backend_Service SHALL enforce 8-character format
7. WHEN validating account active status, THE Backend_Service SHALL enforce values 'Y' or 'N'
8. WHEN validating user type, THE Backend_Service SHALL enforce values 'A' (Admin) or 'U' (User)
9. WHEN validating customer names, THE Backend_Service SHALL enforce alphabetic characters and spaces only
10. WHEN validating US phone numbers, THE Backend_Service SHALL enforce format (XXX)XXX-XXXX with numeric digits
11. WHEN validating US SSN, THE Backend_Service SHALL enforce 9-digit numeric format and reject invalid SSN patterns (000, 666, 900-999 in first part)
12. WHEN validating FICO credit scores, THE Backend_Service SHALL enforce 3-digit numeric format

### Requirement 18: Performance Requirements

**User Story:** As a system administrator, I want the modernized system to meet performance requirements, so that user experience is acceptable.

#### Acceptance Criteria

1. WHEN processing authentication requests, THE Backend_Service SHALL respond within 1 second under normal load
2. WHEN processing account view requests, THE Backend_Service SHALL respond within 2 seconds under normal load
3. WHEN processing account update requests, THE Backend_Service SHALL respond within 3 seconds under normal load
4. WHEN processing transaction queries, THE Backend_Service SHALL respond within 2 seconds for up to 1000 transactions
5. THE Database SHALL support concurrent access by at least 100 simultaneous users
6. THE Backend_Service SHALL implement connection pooling for database connections

### Requirement 19: Security Requirements

**User Story:** As a security officer, I want the modernized system to implement security best practices, so that sensitive data is protected.

#### Acceptance Criteria

1. WHEN storing passwords, THE Backend_Service SHALL hash passwords using bcrypt or equivalent strong hashing algorithm
2. WHEN transmitting data, THE Frontend_Application SHALL use HTTPS for all communications
3. WHEN logging data, THE Backend_Service SHALL mask sensitive fields including passwords and full card numbers
4. WHEN displaying card numbers, THE Frontend_Application SHALL mask all but the last 4 digits
5. THE Backend_Service SHALL implement SQL injection prevention through parameterized queries
6. THE Backend_Service SHALL implement CSRF protection for state-changing operations
7. THE Backend_Service SHALL implement rate limiting to prevent brute force attacks on authentication

### Requirement 20: Deployment and Configuration

**User Story:** As a DevOps engineer, I want the modernized system to support modern deployment practices, so that it can be deployed to cloud environments.

#### Acceptance Criteria

1. THE Backend_Service SHALL externalize configuration using Spring Boot application properties
2. THE Backend_Service SHALL support environment-specific configuration for development, testing, and production
3. THE CardDemo_System SHALL provide Docker containers for Backend_Service and Frontend_Application
4. THE CardDemo_System SHALL provide database initialization scripts for PostgreSQL schema creation
5. THE CardDemo_System SHALL provide database migration scripts using Flyway or Liquibase
6. THE CardDemo_System SHALL document deployment procedures in README files
