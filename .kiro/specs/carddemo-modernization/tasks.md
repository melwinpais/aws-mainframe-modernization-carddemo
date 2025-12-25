# Implementation Plan: CardDemo Modernization

## Overview

This implementation plan transforms the CardDemo mainframe application from COBOL/CICS/VSAM to Java Spring Boot 3 (JDK 17) backend, Vue.js frontend, and PostgreSQL 16.9 database. The plan follows an incremental approach, building core infrastructure first, then implementing features module by module, with comprehensive testing at each step.

**CRITICAL: All modernized code must be placed in the `app/transform/` directory.**

## Tasks

- [x] 1. Project Setup and Infrastructure
- [x] 1.1 Initialize Spring Boot 3 project with JDK 17 in app/transform/backend
  - Create Maven/Gradle project structure in app/transform/backend
  - Configure Spring Boot 3.x dependencies
  - Set up application.yml for development and production profiles
  - _Requirements: 20.1, 20.2_

- [x] 1.2 Initialize Vue.js 3 project in app/transform/frontend
  - Create Vue.js project with Vite in app/transform/frontend
  - Configure Vue Router and Vuex/Pinia
  - Set up Axios for API calls
  - Configure environment variables
  - _Requirements: 15.1_

- [x] 1.3 Set up PostgreSQL 16.9 database
  - Create database and user
  - Configure connection pooling
  - Set up Flyway for database migrations in app/transform/backend/src/main/resources/db/migration
  - _Requirements: 11.1, 20.5_

- [x] 1.4 Configure Docker environment in app/transform
  - Create Dockerfile for backend in app/transform/backend
  - Create Dockerfile for frontend in app/transform/frontend
  - Create docker-compose.yml in app/transform for all services
  - _Requirements: 20.3_

- [x] 2. Database Schema Implementation
- [x] 2.1 Create initial database migration script in app/transform/backend/src/main/resources/db/migration/V1__initial_schema.sql
  - Create users table with constraints and indexes
  - Create accounts table with constraints and indexes
  - Create customers table with constraints and indexes
  - Create cards table with foreign keys and indexes
  - Create transactions table with foreign keys and indexes
  - Create card_xref table with composite key
  - _Requirements: 11.1, 11.2, 11.3, 11.4, 11.5, 11.6, 11.7, 11.8, 11.9_

- [x] 2.2 Write property test for database schema
  - **Property 14: VSAM to PostgreSQL Conversion Round Trip**
  - **Validates: Requirements 10.2, 10.3, 10.4, 10.5, 10.6**

- [x] 3. Core Backend Infrastructure
- [x] 3.1 Implement JPA entities in app/transform/backend/src/main/java/com/carddemo/entity
  - Create User entity with validation annotations
  - Create Account entity with validation annotations
  - Create Customer entity with validation annotations
  - Create Card entity with validation annotations
  - Create Transaction entity with validation annotations
  - _Requirements: 11.1, 11.2, 11.3, 11.4, 11.5_

- [x] 3.2 Implement JPA repositories in app/transform/backend/src/main/java/com/carddemo/repository
  - Create UserRepository interface
  - Create AccountRepository interface
  - Create CustomerRepository interface
  - Create CardRepository interface
  - Create TransactionRepository interface
  - _Requirements: 11.1, 11.2, 11.3, 11.4, 11.5_

- [x] 3.3 Implement validation service in app/transform/backend/src/main/java/com/carddemo/service
  - Create ValidationService with all validation methods
  - Implement account ID validation
  - Implement card number validation
  - Implement date validation
  - Implement currency amount validation
  - Implement user ID validation
  - Implement password validation
  - Implement enumerated value validation
  - Implement name validation
  - Implement phone number validation
  - Implement SSN validation
  - Implement FICO score validation
  - _Requirements: 17.1, 17.2, 17.3, 17.4, 17.5, 17.6, 17.7, 17.8, 17.9, 17.10, 17.11, 17.12_


- [x] 3.4 Write property tests for validation service in app/transform/backend/src/test/java/com/carddemo/service
  - **Property 16: Numeric ID Validation**
  - **Property 17: Date Format Validation**
  - **Property 18: Currency Amount Validation**
  - **Property 19: Enumerated Value Validation**
  - **Property 20: Name Validation**
  - **Property 21: US Phone Number Validation**
  - **Property 22: US SSN Validation**
  - **Property 23: FICO Score Validation**
  - **Property 24: User ID Case Normalization**
  - **Validates: Requirements 17.1-17.12**

- [x] 3.5 Implement global exception handler in app/transform/backend/src/main/java/com/carddemo/exception
  - Create GlobalExceptionHandler with @ControllerAdvice
  - Handle ValidationException (400)
  - Handle AuthenticationException (401)
  - Handle AuthorizationException (403)
  - Handle ResourceNotFoundException (404)
  - Handle generic Exception (500)
  - Create ErrorResponse DTO
  - _Requirements: 14.1, 14.2, 14.3, 14.4, 14.5_

- [x] 3.6 Configure logging in app/transform/backend/src/main/resources
  - Set up SLF4J with Logback
  - Configure log levels for different environments
  - Implement sensitive data masking for passwords, card numbers, SSN
  - _Requirements: 14.5, 14.6_

- [x] 4. Authentication Module
- [x] 4.1 Implement JWT token provider in app/transform/backend/src/main/java/com/carddemo/security
  - Create JwtTokenProvider class
  - Implement token generation
  - Implement token validation
  - Implement token parsing
  - Configure JWT secret and expiration
  - _Requirements: 13.1, 13.2, 13.3, 13.4_

- [x] 4.2 Implement Spring Security configuration in app/transform/backend/src/main/java/com/carddemo/config
  - Create SecurityConfig class
  - Configure authentication manager
  - Configure password encoder (BCrypt)
  - Configure HTTP security (CORS, CSRF)
  - Configure JWT filter
  - _Requirements: 19.1, 19.2, 19.6_

- [x] 4.3 Implement authentication service in app/transform/backend/src/main/java/com/carddemo/service
  - Create AuthService interface and implementation
  - Implement authenticate method with user lookup and password verification
  - Implement logout method
  - Implement validateSession method
  - Implement normalizeUserId method (uppercase conversion)
  - _Requirements: 1.1, 1.2, 1.5, 1.6, 17.5_

- [x] 4.4 Implement authentication controller in app/transform/backend/src/main/java/com/carddemo/controller
  - Create AuthController with REST endpoints
  - POST /api/auth/login endpoint
  - POST /api/auth/logout endpoint
  - GET /api/auth/validate endpoint
  - Create LoginRequestDto and LoginResponseDto in app/transform/backend/src/main/java/com/carddemo/dto
  - _Requirements: 1.1, 1.2, 12.1, 12.2, 12.3, 12.4, 12.5_

- [x] 4.5 Write property tests for authentication in app/transform/backend/src/test/java/com/carddemo/service
  - **Property 1: Credential Validation**
  - **Property 2: User Type Routing**
  - **Validates: Requirements 1.1, 1.2, 1.3, 1.4, 1.7, 1.8**

- [x] 4.6 Write unit tests for authentication in app/transform/backend/src/test/java/com/carddemo/service
  - Test successful authentication with valid credentials
  - Test authentication failure with invalid user ID
  - Test authentication failure with wrong password
  - Test empty user ID validation
  - Test empty password validation
  - _Requirements: 1.3, 1.4, 1.5, 1.6_

- [x] 5. Menu Navigation Module
- [x] 5.1 Implement menu configuration
  - Create MenuOption class
  - Create MenuConfig with regular user menu options (11 options from COMEN02Y)
  - Create AdminMenuConfig with admin menu options (6 options from COADM02Y)
  - _Requirements: 2.1, 2.2_

- [x] 5.2 Implement menu service
  - Create MenuService interface and implementation
  - Implement getMenuOptions method (filter by user type)
  - Implement validateAndRoute method
  - _Requirements: 2.3, 2.4, 2.5, 2.7_

- [x] 5.3 Implement menu controller
  - Create MenuController with REST endpoints
  - GET /api/menu/options endpoint
  - POST /api/menu/select endpoint
  - Create MenuOptionDto and MenuSelectionDto
  - _Requirements: 2.1, 2.2, 2.3, 2.4, 2.5_

- [x] 5.4 Write property tests for menu navigation
  - **Property 3: Menu Option Validation**
  - **Property 4: Menu Option Routing**
  - **Property 5: Admin Option Authorization**
  - **Validates: Requirements 2.3, 2.4, 2.5, 2.7**

- [x] 6. Account Management Module
- [x] 6.1 Implement account service
  - Create AccountService interface and implementation
  - Implement getAccount method with account, customer, and cards retrieval
  - Implement updateAccount method with validation
  - Implement getCustomerForAccount method
  - Implement getCardsForAccount method
  - Implement searchAccounts method
  - Implement validateAccountId method
  - _Requirements: 3.1, 3.5, 3.6, 3.7, 4.1, 4.2, 4.3, 4.4_

- [x] 6.2 Implement account controller
  - Create AccountController with REST endpoints
  - GET /api/accounts/{accountId} endpoint
  - PUT /api/accounts/{accountId} endpoint
  - GET /api/accounts/{accountId}/customer endpoint
  - GET /api/accounts/{accountId}/cards endpoint
  - GET /api/accounts/search endpoint
  - Create AccountDto, AccountUpdateDto, CustomerDto, CardDto
  - _Requirements: 3.1, 3.5, 3.10, 4.1, 4.2_

- [x] 6.3 Write property tests for account management
  - **Property 6: Account ID Validation**
  - **Property 7: Account Data Retrieval**
  - **Property 8: Account Not Found Handling**
  - **Property 9: Customer Referential Integrity**
  - **Property 10: Account Response Completeness**
  - **Property 11: Account Update Validation**
  - **Property 12: Account Update Persistence**
  - **Property 13: Customer Update Persistence**
  - **Validates: Requirements 3.1-3.11, 4.1-4.6**

- [x] 6.4 Write unit tests for account management
  - Test account retrieval with valid ID
  - Test account not found error
  - Test customer not found error
  - Test account update with valid data
  - Test account update with invalid data
  - _Requirements: 3.8, 3.9, 4.6_

- [x] 7. Card Management Module
- [x] 7.1 Implement card service
  - Create CardService interface and implementation
  - Implement getCard method
  - Implement getCardsByAccount method
  - Implement createCard method with validation
  - Implement updateCard method with validation
  - Implement deactivateCard method
  - _Requirements: 5.1, 5.2, 5.3, 5.4, 5.5_

- [x] 7.2 Implement card controller
  - Create CardController with REST endpoints
  - GET /api/cards/{cardNumber} endpoint
  - GET /api/cards/account/{accountId} endpoint
  - POST /api/cards endpoint
  - PUT /api/cards/{cardNumber} endpoint
  - DELETE /api/cards/{cardNumber} endpoint
  - Create CardDto, CardCreateDto, CardUpdateDto
  - _Requirements: 5.1, 5.2, 5.3, 5.4, 5.5_

- [x] 7.3 Write property tests for card management
  - Test card number validation (16 digits)
  - Test card retrieval by card number
  - Test card retrieval by account ID
  - Test card creation with valid data
  - Test card update with valid data
  - _Requirements: 5.1, 5.2, 5.3, 5.4, 17.2_

- [x] 8. Transaction Management Module
- [x] 8.1 Implement transaction service
  - Create TransactionService interface and implementation
  - Implement getTransaction method
  - Implement getTransactionsByAccount method with pagination
  - Implement getTransactionsByCard method with pagination
  - Implement createTransaction method with validation and balance update
  - _Requirements: 6.1, 6.2, 6.3, 6.4, 6.5, 6.6_

- [x] 8.2 Implement transaction controller
  - Create TransactionController with REST endpoints
  - GET /api/transactions/{transactionId} endpoint
  - GET /api/transactions/account/{accountId} endpoint with pagination
  - GET /api/transactions/card/{cardNumber} endpoint with pagination
  - POST /api/transactions endpoint
  - Create TransactionDto, TransactionCreateDto
  - _Requirements: 6.1, 6.2, 6.3, 6.4_

- [x] 8.3 Write property tests for transaction management
  - Test transaction retrieval by ID
  - Test transaction retrieval by account with date filters
  - Test transaction creation with balance update
  - Test pagination for transaction lists
  - _Requirements: 6.1, 6.2, 6.3, 6.4, 6.5, 6.6_

- [x] 9. Bill Payment Module
- [x] 9.1 Implement bill payment service
  - Create BillPaymentService interface and implementation
  - Implement processBillPayment method (creates payment transaction)
  - Implement getBillInfo method (returns balance and due date)
  - _Requirements: 7.1, 7.2, 7.3, 7.4, 7.5_

- [x] 9.2 Implement bill payment controller
  - Create BillPaymentController with REST endpoints
  - POST /api/bills/payment endpoint
  - GET /api/bills/account/{accountId} endpoint
  - Create BillPaymentDto, BillInfoDto
  - _Requirements: 7.1, 7.2, 7.3, 7.4, 7.5_

- [x] 9.3 Write unit tests for bill payment
  - Test bill payment processing
  - Test bill payment validation
  - Test balance update after payment
  - _Requirements: 7.1, 7.2, 7.3, 7.4, 7.5_

- [x] 10. Report Generation Module
- [x] 10.1 Implement report service
  - Create ReportService interface and implementation
  - Implement generateAccountReport method
  - Implement generateTransactionReport method
  - Implement generateCardReport method
  - _Requirements: 9.1, 9.2, 9.3, 9.4, 9.5_

- [x] 10.2 Implement report controller
  - Create ReportController with REST endpoints
  - GET /api/reports/accounts endpoint
  - GET /api/reports/transactions endpoint
  - GET /api/reports/cards endpoint
  - Create AccountReportDto, TransactionReportDto, CardReportDto
  - _Requirements: 9.1, 9.2, 9.3, 9.4_

- [x] 10.3 Write unit tests for report generation
  - Test account report generation with filters
  - Test transaction report generation with date range
  - Test card report generation with status filter
  - _Requirements: 9.1, 9.2, 9.3, 9.4_

- [x] 11. User Management Module (Admin Only)
- [x] 11.1 Implement user service
  - Create UserService interface and implementation
  - Implement listUsers method with pagination
  - Implement getUser method
  - Implement createUser method with password hashing
  - Implement updateUser method
  - Implement deactivateUser method
  - _Requirements: 8.1, 8.2, 8.3, 8.4, 8.5, 8.6_

- [x] 11.2 Implement user controller
  - Create UserController with REST endpoints and @PreAuthorize("ADMIN")
  - GET /api/users endpoint with pagination
  - GET /api/users/{userId} endpoint
  - POST /api/users endpoint
  - PUT /api/users/{userId} endpoint
  - DELETE /api/users/{userId} endpoint
  - Create UserDto, UserCreateDto, UserUpdateDto
  - _Requirements: 8.1, 8.2, 8.3, 8.4, 8.5, 8.6_

- [x] 11.3 Write unit tests for user management
  - Test user list retrieval (admin only)
  - Test user creation with password hashing
  - Test user update
  - Test authorization check for non-admin users
  - _Requirements: 8.1, 8.2, 8.3, 8.4, 8.5, 8.6_

- [x] 12. Data Migration Utilities
- [x] 12.1 Implement VSAM to PostgreSQL converter
  - Create VsamConverter class
  - Implement convertUserRecord method
  - Implement convertAccountRecord method
  - Implement convertCustomerRecord method
  - Implement convertCardRecord method
  - Implement convertTransactionRecord method
  - _Requirements: 10.1, 10.2, 10.3, 10.4, 10.5, 10.6_

- [x] 12.2 Implement migration utility
  - Create MigrationUtility class
  - Implement extractFromVsam method
  - Implement loadToPostgres method
  - Implement error logging and continue processing
  - Implement summary report generation
  - _Requirements: 10.1, 10.7, 10.8_

- [x] 12.3 Write property tests for data migration
  - **Property 14: VSAM to PostgreSQL Conversion Round Trip**
  - **Property 15: Migration Error Handling**
  - **Validates: Requirements 10.2-10.7**

- [x] 13. Checkpoint - Backend Core Complete
- Ensure all backend services are implemented
- Ensure all REST APIs are functional
- Ensure all property tests pass
- Ensure all unit tests pass
- Ask the user if questions arise

- [x] 14. Frontend - Core Infrastructure
- [x] 14.1 Implement API client service
  - Create api.js with Axios configuration
  - Configure base URL and interceptors
  - Implement request interceptor for JWT token
  - Implement response interceptor for error handling
  - _Requirements: 12.6, 13.2, 13.3_

- [x] 14.2 Implement Vuex/Pinia store
  - Create auth module for authentication state
  - Create account module for account state
  - Create card module for card state
  - Create transaction module for transaction state
  - _Requirements: 15.1_

- [x] 14.3 Implement common components
  - Create Header.vue component
  - Create ErrorMessage.vue component
  - Create LoadingSpinner.vue component
  - Create FormInput.vue component
  - _Requirements: 15.2, 15.3, 15.4_

- [x] 14.4 Configure Vue Router
  - Create router/index.js with all routes
  - Implement navigation guards for authentication
  - Implement navigation guards for admin authorization
  - _Requirements: 15.6_

- [x] 15. Frontend - Authentication Views
- [x] 15.1 Implement Login.vue component
  - Create login form with user ID and password fields
  - Implement form validation
  - Implement login method calling authService
  - Implement error message display
  - Implement navigation to menu on success
  - _Requirements: 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 15.2, 15.3_

- [x] 15.2 Implement authService.js
  - Implement login method (POST /api/auth/login)
  - Implement logout method (POST /api/auth/logout)
  - Implement validateSession method (GET /api/auth/validate)
  - Implement token storage in localStorage
  - Implement user info storage in localStorage
  - _Requirements: 1.1, 1.2, 13.1, 13.2, 13.3, 13.4_

- [x] 15.3 Write unit tests for Login component
  - Test successful login flow
  - Test login with invalid credentials
  - Test empty field validation
  - _Requirements: 1.1, 1.3, 1.4, 1.5, 1.6_

- [x] 16. Frontend - Menu Navigation Views
- [x] 16.1 Implement MainMenu.vue component
  - Create menu display with dynamic options
  - Implement option selection and validation
  - Implement navigation to selected feature
  - Implement logout functionality
  - _Requirements: 2.1, 2.3, 2.4, 2.5, 15.2, 15.3_

- [x] 16.2 Implement AdminMenu.vue component
  - Create admin menu display with admin options
  - Implement option selection and validation
  - Implement navigation to selected feature
  - _Requirements: 2.2, 2.3, 2.4, 2.5_

- [x] 16.3 Implement menuService.js
  - Implement getOptions method (GET /api/menu/options)
  - Implement selectOption method (POST /api/menu/select)
  - _Requirements: 2.1, 2.2, 2.3, 2.4, 2.5_

- [x] 17. Frontend - Account Management Views
- [x] 17.1 Implement AccountView.vue component
  - Create account search form
  - Implement account ID validation
  - Implement account data display (account, customer, cards)
  - Implement navigation to account update
  - Implement return to menu
  - _Requirements: 3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.10, 3.11, 15.2, 15.3, 15.4_

- [x] 17.2 Implement AccountUpdate.vue component
  - Create account update form with all fields
  - Implement field validation
  - Implement change detection
  - Implement save changes method
  - Implement cancel changes method
  - Implement field-specific error display
  - _Requirements: 4.1, 4.2, 4.3, 4.4, 4.5, 4.6, 15.2, 15.3_

- [x] 17.3 Implement accountService.js
  - Implement getAccount method (GET /api/accounts/{accountId})
  - Implement updateAccount method (PUT /api/accounts/{accountId})
  - Implement getCustomer method (GET /api/accounts/{accountId}/customer)
  - Implement getCards method (GET /api/accounts/{accountId}/cards)
  - _Requirements: 3.5, 3.6, 3.7, 4.3_

- [x] 18. Frontend - Card Management Views
- [x] 18.1 Implement CardList.vue component
  - Create card list display
  - Implement card search by account
  - Implement navigation to card detail
  - Implement navigation to card update
  - _Requirements: 5.1, 5.2, 15.2, 15.4_

- [x] 18.2 Implement CardDetail.vue component
  - Create card detail display
  - Implement navigation to card update
  - Implement return to card list
  - _Requirements: 5.1, 15.2_

- [x] 18.3 Implement CardUpdate.vue component
  - Create card update form
  - Implement field validation
  - Implement save changes method
  - Implement cancel changes method
  - _Requirements: 5.4, 15.2, 15.3_

- [x] 18.4 Implement cardService.js
  - Implement getCard method (GET /api/cards/{cardNumber})
  - Implement getCardsByAccount method (GET /api/cards/account/{accountId})
  - Implement createCard method (POST /api/cards)
  - Implement updateCard method (PUT /api/cards/{cardNumber})
  - _Requirements: 5.1, 5.2, 5.3, 5.4_

- [x] 19. Frontend - Transaction Management Views
- [x] 19.1 Implement TransactionList.vue component
  - Create transaction list display with pagination
  - Implement transaction search by account/card
  - Implement date range filtering
  - Implement navigation to transaction detail
  - _Requirements: 6.1, 6.2, 6.3, 15.2, 15.4_

- [x] 19.2 Implement TransactionView.vue component
  - Create transaction detail display
  - Implement return to transaction list
  - _Requirements: 6.1, 15.2_

- [x] 19.3 Implement transactionService.js
  - Implement getTransaction method (GET /api/transactions/{transactionId})
  - Implement getTransactionsByAccount method (GET /api/transactions/account/{accountId})
  - Implement getTransactionsByCard method (GET /api/transactions/card/{cardNumber})
  - Implement createTransaction method (POST /api/transactions)
  - _Requirements: 6.1, 6.2, 6.3, 6.4_

- [x] 20. Frontend - Bill Payment and Reports Views
- [x] 20.1 Implement BillPayment.vue component
  - Create bill payment form
  - Implement payment amount validation
  - Implement payment processing
  - Implement success/error message display
  - _Requirements: 7.1, 7.2, 7.3, 7.4, 15.2, 15.3, 15.5_

- [x] 20.2 Implement Reports.vue component
  - Create report selection interface
  - Implement report parameter inputs (date ranges, filters)
  - Implement report generation and display
  - _Requirements: 9.1, 9.2, 9.3, 9.4, 15.2_

- [x] 21. Frontend - User Management Views (Admin Only)
- [x] 21.1 Implement UserManagement.vue component
  - Create user list display with pagination
  - Implement user creation dialog
  - Implement user update dialog
  - Implement user deactivation with confirmation
  - Implement admin authorization check
  - _Requirements: 8.1, 8.2, 8.3, 8.4, 8.5, 8.6, 15.2, 15.3, 15.5_

- [x] 21.2 Implement userService.js
  - Implement listUsers method (GET /api/users)
  - Implement getUser method (GET /api/users/{userId})
  - Implement createUser method (POST /api/users)
  - Implement updateUser method (PUT /api/users/{userId})
  - Implement deactivateUser method (DELETE /api/users/{userId})
  - _Requirements: 8.1, 8.2, 8.3, 8.4, 8.5_

- [x] 22. Checkpoint - Frontend Complete
- Ensure all Vue components are implemented
- Ensure all frontend services are functional
- Ensure navigation flows work correctly
- Ensure error handling displays properly
- Ask the user if questions arise

- [x] 23. Integration and End-to-End Testing
- [x] 23.1 Write integration tests for authentication flow
  - Test login → menu navigation → logout
  - Test session validation across requests
  - _Requirements: 1.1, 1.2, 2.1, 13.3_

- [x] 23.2 Write integration tests for account management flow
  - Test account view → account update → verification
  - Test account search with various inputs
  - _Requirements: 3.1-3.11, 4.1-4.6_

- [x] 23.3 Write integration tests for card management flow
  - Test card list → card detail → card update
  - Test card creation and validation
  - _Requirements: 5.1-5.5_

- [x] 23.4 Write integration tests for transaction flow
  - Test transaction list with pagination
  - Test transaction creation with balance update
  - _Requirements: 6.1-6.6_

- [x] 24. Deployment Preparation
- [x] 24.1 Create deployment documentation
  - Document environment variables
  - Document deployment steps
  - Document database initialization
  - _Requirements: 20.6_

- [x] 24.2 Test Docker deployment
  - Build all Docker images
  - Start services with docker-compose
  - Run database migrations
  - Verify all services are healthy
  - _Requirements: 20.3, 20.4, 20.5_

- [x] 24.3 Load sample data
  - Create data loading script
  - Load sample users, accounts, customers, cards, transactions
  - Verify data integrity
  - _Requirements: 10.1_

- [x] 25. Final Checkpoint - System Complete
- Ensure all backend APIs are functional
- Ensure all frontend views are functional
- Ensure all tests pass (unit, property, integration)
- Ensure Docker deployment works
- Ensure sample data loads correctly
- Ask the user if questions arise

## Notes

- **CRITICAL: All modernized code must be placed in the `app/transform/` directory**
  - Backend: `app/transform/backend/` (Spring Boot 3 with JDK 17)
  - Frontend: `app/transform/frontend/` (Vue.js 3)
  - Docker: `app/transform/docker-compose.yml`
- Tasks marked with `*` are optional test tasks and can be skipped for faster MVP
- Each task references specific requirements for traceability
- Checkpoints ensure incremental validation at major milestones
- Property tests validate universal correctness properties with 100+ iterations
- Unit tests validate specific examples and edge cases
- Integration tests validate end-to-end workflows
- The implementation follows a bottom-up approach: infrastructure → backend → frontend → integration
