# Design Document

## Introduction

This document provides the technical design for modernizing the CardDemo mainframe application from COBOL/CICS/VSAM to a modern three-tier architecture using Java Spring Boot 3 (JDK 17) for the backend, Vue.js for the frontend, and PostgreSQL 16.9 for the database. The design preserves all functional behaviors of the original system while adopting modern architectural patterns and best practices.

## Overview

The modernized CardDemo system will transform a mainframe CICS transaction processing application into a cloud-native web application. The system maintains the same business logic, data structures, and user workflows while replacing:

- CICS transaction processing → RESTful HTTP APIs
- BMS screen maps → Vue.js single-page application
- VSAM files → PostgreSQL relational database
- COBOL programs → Java Spring Boot services
- COMMAREA data passing → JWT-based session management

The system will support all original features including user authentication, account management, card management, transaction processing, bill payments, reporting, and user administration.

## Architecture

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         Client Layer                            │
│                                                                 │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │              Vue.js Single Page Application              │  │
│  │  - Authentication UI    - Account Management UI          │  │
│  │  - Menu Navigation      - Card Management UI             │  │
│  │  - Transaction UI       - Reporting UI                   │  │
│  └──────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ HTTPS / REST API
                              │
┌─────────────────────────────────────────────────────────────────┐
│                      Application Layer                          │
│                                                                 │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │         Spring Boot 3 Application (JDK 17)               │  │
│  │                                                          │  │
│  │  ┌────────────────────────────────────────────────────┐ │  │
│  │  │           REST Controllers                         │ │  │
│  │  │  - AuthController    - AccountController           │ │  │
│  │  │  - MenuController    - CardController              │ │  │
│  │  │  - TransactionController                           │ │  │
│  │  └────────────────────────────────────────────────────┘ │  │
│  │                                                          │  │
│  │  ┌────────────────────────────────────────────────────┐ │  │
│  │  │           Service Layer                            │ │  │
│  │  │  - AuthService       - AccountService              │ │  │
│  │  │  - MenuService       - CardService                 │ │  │
│  │  │  - TransactionService                              │ │  │
│  │  └────────────────────────────────────────────────────┘ │  │
│  │                                                          │  │
│  │  ┌────────────────────────────────────────────────────┐ │  │
│  │  │           Repository Layer (JPA)                   │ │  │
│  │  │  - UserRepository    - AccountRepository           │ │  │
│  │  │  - CustomerRepository - CardRepository             │ │  │
│  │  │  - TransactionRepository                           │ │  │
│  │  └────────────────────────────────────────────────────┘ │  │
│  └──────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ JDBC
                              │
┌─────────────────────────────────────────────────────────────────┐
│                         Data Layer                              │
│                                                                 │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │              PostgreSQL 16.9 Database                    │  │
│  │  - users table          - accounts table                 │  │
│  │  - customers table      - cards table                    │  │
│  │  - transactions table   - card_xref table                │  │
│  └──────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

### Technology Stack Mapping

| Original Technology | Modern Equivalent | Purpose |
|---------------------|-------------------|---------|
| CICS Transaction Manager | Spring Boot Web MVC | HTTP request handling |
| COBOL Programs | Java Services | Business logic |
| BMS Maps | Vue.js Components | User interface |
| VSAM Files | PostgreSQL Tables | Data persistence |
| COMMAREA | JWT + Session State | Inter-component communication |
| CICS XCTL | REST API calls | Program-to-program transfer |
| CICS RETURN | HTTP Response | Return to caller |
| CICS READ/WRITE | JPA Repository methods | Data access |

## REST API Specification

### Authentication APIs

| Method | Endpoint | Request Body | Response | Description |
|--------|----------|--------------|----------|-------------|
| POST | /api/auth/login | `{userId, password}` | `{token, userId, userType, firstName, lastName}` | Authenticate user and create session |
| POST | /api/auth/logout | - | `{message}` | Invalidate current session |
| GET | /api/auth/validate | - | `{valid, userId, userType}` | Validate current session token |

### Menu APIs

| Method | Endpoint | Request Body | Response | Description |
|--------|----------|--------------|----------|-------------|
| GET | /api/menu/options | - | `[{optionNumber, optionName, programName, userType}]` | Get menu options for current user |
| POST | /api/menu/select | `{optionNumber}` | `{programName, route}` | Validate and route menu selection |

### Account APIs

| Method | Endpoint | Request Body | Response | Description |
|--------|----------|--------------|----------|-------------|
| GET | /api/accounts/{accountId} | - | `AccountDto` | Get account details |
| PUT | /api/accounts/{accountId} | `AccountUpdateDto` | `AccountDto` | Update account information |
| GET | /api/accounts/{accountId}/customer | - | `CustomerDto` | Get customer for account |
| GET | /api/accounts/{accountId}/cards | - | `[CardDto]` | Get cards for account |
| GET | /api/accounts/search | Query params: `accountId`, `customerId` | `[AccountDto]` | Search accounts |

### Customer APIs

| Method | Endpoint | Request Body | Response | Description |
|--------|----------|--------------|----------|-------------|
| GET | /api/customers/{customerId} | - | `CustomerDto` | Get customer details |
| PUT | /api/customers/{customerId} | `CustomerUpdateDto` | `CustomerDto` | Update customer information |
| GET | /api/customers/{customerId}/accounts | - | `[AccountDto]` | Get accounts for customer |

### Card APIs

| Method | Endpoint | Request Body | Response | Description |
|--------|----------|--------------|----------|-------------|
| GET | /api/cards/{cardNumber} | - | `CardDto` | Get card details |
| GET | /api/cards/account/{accountId} | - | `[CardDto]` | Get cards by account |
| POST | /api/cards | `CardCreateDto` | `CardDto` | Create new card |
| PUT | /api/cards/{cardNumber} | `CardUpdateDto` | `CardDto` | Update card information |
| DELETE | /api/cards/{cardNumber} | - | `{message}` | Deactivate card |

### Transaction APIs

| Method | Endpoint | Request Body | Response | Description |
|--------|----------|--------------|----------|-------------|
| GET | /api/transactions/{transactionId} | - | `TransactionDto` | Get transaction details |
| GET | /api/transactions/account/{accountId} | Query params: `startDate`, `endDate`, `page`, `size` | `Page<TransactionDto>` | Get transactions by account |
| GET | /api/transactions/card/{cardNumber} | Query params: `startDate`, `endDate`, `page`, `size` | `Page<TransactionDto>` | Get transactions by card |
| POST | /api/transactions | `TransactionCreateDto` | `TransactionDto` | Create new transaction |

### Bill Payment APIs

| Method | Endpoint | Request Body | Response | Description |
|--------|----------|--------------|----------|-------------|
| POST | /api/bills/payment | `{accountId, amount, paymentDate}` | `TransactionDto` | Process bill payment |
| GET | /api/bills/account/{accountId} | - | `{currentBalance, minimumPayment, dueDate}` | Get bill information |

### Report APIs

| Method | Endpoint | Request Body | Response | Description |
|--------|----------|--------------|----------|-------------|
| GET | /api/reports/accounts | Query params: `startDate`, `endDate`, `status` | `AccountReportDto` | Generate account report |
| GET | /api/reports/transactions | Query params: `startDate`, `endDate`, `type` | `TransactionReportDto` | Generate transaction report |
| GET | /api/reports/cards | Query params: `status`, `expiringBefore` | `CardReportDto` | Generate card report |

### User Management APIs (Admin Only)

| Method | Endpoint | Request Body | Response | Description |
|--------|----------|--------------|----------|-------------|
| GET | /api/users | Query params: `userType`, `page`, `size` | `Page<UserDto>` | List all users |
| GET | /api/users/{userId} | - | `UserDto` | Get user details |
| POST | /api/users | `UserCreateDto` | `UserDto` | Create new user |
| PUT | /api/users/{userId} | `UserUpdateDto` | `UserDto` | Update user information |
| DELETE | /api/users/{userId} | - | `{message}` | Deactivate user |

## Components and Interfaces

### Backend Interface Design

#### Controller Layer Interfaces

```java
@RestController
@RequestMapping("/api/auth")
public interface AuthController {
    @PostMapping("/login")
    ResponseEntity<LoginResponseDto> login(@RequestBody LoginRequestDto request);
    
    @PostMapping("/logout")
    ResponseEntity<MessageDto> logout(@RequestHeader("Authorization") String token);
    
    @GetMapping("/validate")
    ResponseEntity<SessionValidationDto> validateSession(
        @RequestHeader("Authorization") String token);
}

@RestController
@RequestMapping("/api/menu")
public interface MenuController {
    @GetMapping("/options")
    ResponseEntity<List<MenuOptionDto>> getMenuOptions(
        @RequestHeader("Authorization") String token);
    
    @PostMapping("/select")
    ResponseEntity<MenuSelectionDto> selectOption(
        @RequestBody MenuSelectionRequestDto request,
        @RequestHeader("Authorization") String token);
}

@RestController
@RequestMapping("/api/accounts")
public interface AccountController {
    @GetMapping("/{accountId}")
    ResponseEntity<AccountDto> getAccount(
        @PathVariable Long accountId,
        @RequestHeader("Authorization") String token);
    
    @PutMapping("/{accountId}")
    ResponseEntity<AccountDto> updateAccount(
        @PathVariable Long accountId,
        @RequestBody AccountUpdateDto updates,
        @RequestHeader("Authorization") String token);
    
    @GetMapping("/{accountId}/customer")
    ResponseEntity<CustomerDto> getCustomerForAccount(
        @PathVariable Long accountId,
        @RequestHeader("Authorization") String token);
    
    @GetMapping("/{accountId}/cards")
    ResponseEntity<List<CardDto>> getCardsForAccount(
        @PathVariable Long accountId,
        @RequestHeader("Authorization") String token);
    
    @GetMapping("/search")
    ResponseEntity<List<AccountDto>> searchAccounts(
        @RequestParam(required = false) Long accountId,
        @RequestParam(required = false) Long customerId,
        @RequestHeader("Authorization") String token);
}

@RestController
@RequestMapping("/api/cards")
public interface CardController {
    @GetMapping("/{cardNumber}")
    ResponseEntity<CardDto> getCard(
        @PathVariable Long cardNumber,
        @RequestHeader("Authorization") String token);
    
    @GetMapping("/account/{accountId}")
    ResponseEntity<List<CardDto>> getCardsByAccount(
        @PathVariable Long accountId,
        @RequestHeader("Authorization") String token);
    
    @PostMapping
    ResponseEntity<CardDto> createCard(
        @RequestBody CardCreateDto cardData,
        @RequestHeader("Authorization") String token);
    
    @PutMapping("/{cardNumber}")
    ResponseEntity<CardDto> updateCard(
        @PathVariable Long cardNumber,
        @RequestBody CardUpdateDto updates,
        @RequestHeader("Authorization") String token);
    
    @DeleteMapping("/{cardNumber}")
    ResponseEntity<MessageDto> deactivateCard(
        @PathVariable Long cardNumber,
        @RequestHeader("Authorization") String token);
}

@RestController
@RequestMapping("/api/transactions")
public interface TransactionController {
    @GetMapping("/{transactionId}")
    ResponseEntity<TransactionDto> getTransaction(
        @PathVariable Long transactionId,
        @RequestHeader("Authorization") String token);
    
    @GetMapping("/account/{accountId}")
    ResponseEntity<Page<TransactionDto>> getTransactionsByAccount(
        @PathVariable Long accountId,
        @RequestParam(required = false) String startDate,
        @RequestParam(required = false) String endDate,
        @RequestParam(defaultValue = "0") int page,
        @RequestParam(defaultValue = "20") int size,
        @RequestHeader("Authorization") String token);
    
    @GetMapping("/card/{cardNumber}")
    ResponseEntity<Page<TransactionDto>> getTransactionsByCard(
        @PathVariable Long cardNumber,
        @RequestParam(required = false) String startDate,
        @RequestParam(required = false) String endDate,
        @RequestParam(defaultValue = "0") int page,
        @RequestParam(defaultValue = "20") int size,
        @RequestHeader("Authorization") String token);
    
    @PostMapping
    ResponseEntity<TransactionDto> createTransaction(
        @RequestBody TransactionCreateDto txnData,
        @RequestHeader("Authorization") String token);
}

@RestController
@RequestMapping("/api/users")
@PreAuthorize("hasRole('ADMIN')")
public interface UserController {
    @GetMapping
    ResponseEntity<Page<UserDto>> listUsers(
        @RequestParam(required = false) String userType,
        @RequestParam(defaultValue = "0") int page,
        @RequestParam(defaultValue = "20") int size,
        @RequestHeader("Authorization") String token);
    
    @GetMapping("/{userId}")
    ResponseEntity<UserDto> getUser(
        @PathVariable String userId,
        @RequestHeader("Authorization") String token);
    
    @PostMapping
    ResponseEntity<UserDto> createUser(
        @RequestBody UserCreateDto userData,
        @RequestHeader("Authorization") String token);
    
    @PutMapping("/{userId}")
    ResponseEntity<UserDto> updateUser(
        @PathVariable String userId,
        @RequestBody UserUpdateDto updates,
        @RequestHeader("Authorization") String token);
    
    @DeleteMapping("/{userId}")
    ResponseEntity<MessageDto> deactivateUser(
        @PathVariable String userId,
        @RequestHeader("Authorization") String token);
}
```

#### Service Layer Interfaces

```java
public interface AuthService {
    LoginResponseDto authenticate(String userId, String password);
    void logout(String token);
    SessionDto validateSession(String token);
    String normalizeUserId(String userId);
}

public interface AccountService {
    AccountDto getAccount(Long accountId);
    AccountDto updateAccount(Long accountId, AccountUpdateDto updates);
    CustomerDto getCustomerForAccount(Long accountId);
    List<CardDto> getCardsForAccount(Long accountId);
    List<AccountDto> searchAccounts(Long accountId, Long customerId);
    ValidationResult validateAccountId(String accountId);
}

public interface CardService {
    CardDto getCard(Long cardNumber);
    List<CardDto> getCardsByAccount(Long accountId);
    CardDto createCard(CardCreateDto cardData);
    CardDto updateCard(Long cardNumber, CardUpdateDto updates);
    void deactivateCard(Long cardNumber);
}

public interface TransactionService {
    TransactionDto getTransaction(Long transactionId);
    Page<TransactionDto> getTransactionsByAccount(
        Long accountId, String startDate, String endDate, Pageable pageable);
    Page<TransactionDto> getTransactionsByCard(
        Long cardNumber, String startDate, String endDate, Pageable pageable);
    TransactionDto createTransaction(TransactionCreateDto txnData);
}

public interface ValidationService {
    ValidationResult validateAccountId(String accountId);
    ValidationResult validateCardNumber(String cardNumber);
    ValidationResult validateDate(String date);
    ValidationResult validateCurrencyAmount(String amount);
    ValidationResult validateUserId(String userId);
    ValidationResult validatePassword(String password);
    ValidationResult validateAccountStatus(String status);
    ValidationResult validateUserType(String userType);
    ValidationResult validateName(String name);
    ValidationResult validatePhoneNumber(String phoneNumber);
    ValidationResult validateSSN(String ssn);
    ValidationResult validateFicoScore(String score);
}
```

### Frontend Interface Design

#### Vue.js Component Structure

```javascript
// src/views/Login.vue
export default {
  name: 'Login',
  data() {
    return {
      userId: '',
      password: '',
      errorMessage: '',
      loading: false
    }
  },
  methods: {
    async login() {
      // Call authService.login()
      // On success: store token, navigate to menu
      // On error: display error message
    }
  }
}

// src/views/MainMenu.vue
export default {
  name: 'MainMenu',
  data() {
    return {
      menuOptions: [],
      selectedOption: '',
      errorMessage: '',
      userInfo: {}
    }
  },
  computed: {
    filteredOptions() {
      // Filter options based on user type
    }
  },
  methods: {
    async loadMenuOptions() {
      // Call menuService.getOptions()
    },
    async selectOption() {
      // Call menuService.selectOption()
      // Navigate to selected feature
    },
    logout() {
      // Call authService.logout()
      // Navigate to login
    }
  }
}

// src/views/AccountView.vue
export default {
  name: 'AccountView',
  data() {
    return {
      accountId: '',
      account: null,
      customer: null,
      cards: [],
      errorMessage: '',
      loading: false
    }
  },
  methods: {
    async searchAccount() {
      // Validate accountId
      // Call accountService.getAccount()
      // Call accountService.getCustomer()
      // Call accountService.getCards()
    },
    navigateToUpdate() {
      // Navigate to AccountUpdate with accountId
    },
    returnToMenu() {
      // Navigate back to MainMenu
    }
  }
}

// src/views/AccountUpdate.vue
export default {
  name: 'AccountUpdate',
  data() {
    return {
      accountId: '',
      account: null,
      customer: null,
      originalAccount: null,
      originalCustomer: null,
      errorMessages: {},
      successMessage: '',
      loading: false,
      hasChanges: false
    }
  },
  watch: {
    account: {
      deep: true,
      handler() {
        this.checkForChanges()
      }
    }
  },
  methods: {
    async loadAccount() {
      // Call accountService.getAccount()
      // Store original values
    },
    checkForChanges() {
      // Compare current values with original
      // Set hasChanges flag
    },
    validateFields() {
      // Validate all modified fields
      // Return true if all valid
    },
    async saveChanges() {
      // Validate fields
      // Call accountService.updateAccount()
      // On success: show confirmation, navigate to view
      // On error: display field-specific errors
    },
    cancelChanges() {
      // Discard changes, navigate back
    }
  }
}

// src/views/CardList.vue
export default {
  name: 'CardList',
  data() {
    return {
      accountId: '',
      cards: [],
      errorMessage: '',
      loading: false
    }
  },
  methods: {
    async loadCards() {
      // Call cardService.getCardsByAccount()
    },
    viewCardDetails(cardNumber) {
      // Navigate to CardDetail
    },
    updateCard(cardNumber) {
      // Navigate to CardUpdate
    }
  }
}

// src/views/TransactionList.vue
export default {
  name: 'TransactionList',
  data() {
    return {
      accountId: '',
      cardNumber: '',
      startDate: '',
      endDate: '',
      transactions: [],
      currentPage: 0,
      totalPages: 0,
      errorMessage: '',
      loading: false
    }
  },
  methods: {
    async loadTransactions() {
      // Call transactionService.getTransactions()
      // Support pagination
    },
    filterByDateRange() {
      // Apply date filters
      // Reload transactions
    },
    viewTransactionDetails(transactionId) {
      // Navigate to TransactionView
    },
    nextPage() {
      // Load next page of transactions
    },
    previousPage() {
      // Load previous page of transactions
    }
  }
}

// src/views/UserManagement.vue (Admin Only)
export default {
  name: 'UserManagement',
  data() {
    return {
      users: [],
      selectedUser: null,
      showCreateDialog: false,
      showUpdateDialog: false,
      errorMessage: '',
      loading: false
    }
  },
  methods: {
    async loadUsers() {
      // Call userService.listUsers()
    },
    async createUser(userData) {
      // Call userService.createUser()
      // Reload user list
    },
    async updateUser(userId, updates) {
      // Call userService.updateUser()
      // Reload user list
    },
    async deactivateUser(userId) {
      // Confirm action
      // Call userService.deactivateUser()
      // Reload user list
    }
  }
}
```

#### Frontend Service Interfaces

```javascript
// src/services/authService.js
export default {
  async login(userId, password) {
    // POST /api/auth/login
    // Return: {token, userId, userType, firstName, lastName}
  },
  
  async logout() {
    // POST /api/auth/logout
    // Clear local storage
  },
  
  async validateSession() {
    // GET /api/auth/validate
    // Return: {valid, userId, userType}
  },
  
  getToken() {
    // Get token from local storage
  },
  
  setToken(token) {
    // Store token in local storage
  },
  
  clearToken() {
    // Remove token from local storage
  },
  
  getUserInfo() {
    // Get user info from local storage
  }
}

// src/services/accountService.js
export default {
  async getAccount(accountId) {
    // GET /api/accounts/{accountId}
    // Return: AccountDto
  },
  
  async updateAccount(accountId, updates) {
    // PUT /api/accounts/{accountId}
    // Return: AccountDto
  },
  
  async getCustomer(accountId) {
    // GET /api/accounts/{accountId}/customer
    // Return: CustomerDto
  },
  
  async getCards(accountId) {
    // GET /api/accounts/{accountId}/cards
    // Return: [CardDto]
  },
  
  async searchAccounts(accountId, customerId) {
    // GET /api/accounts/search
    // Return: [AccountDto]
  }
}

// src/services/cardService.js
export default {
  async getCard(cardNumber) {
    // GET /api/cards/{cardNumber}
    // Return: CardDto
  },
  
  async getCardsByAccount(accountId) {
    // GET /api/cards/account/{accountId}
    // Return: [CardDto]
  },
  
  async createCard(cardData) {
    // POST /api/cards
    // Return: CardDto
  },
  
  async updateCard(cardNumber, updates) {
    // PUT /api/cards/{cardNumber}
    // Return: CardDto
  },
  
  async deactivateCard(cardNumber) {
    // DELETE /api/cards/{cardNumber}
    // Return: {message}
  }
}

// src/services/transactionService.js
export default {
  async getTransaction(transactionId) {
    // GET /api/transactions/{transactionId}
    // Return: TransactionDto
  },
  
  async getTransactionsByAccount(accountId, startDate, endDate, page, size) {
    // GET /api/transactions/account/{accountId}
    // Return: Page<TransactionDto>
  },
  
  async getTransactionsByCard(cardNumber, startDate, endDate, page, size) {
    // GET /api/transactions/card/{cardNumber}
    // Return: Page<TransactionDto>
  },
  
  async createTransaction(txnData) {
    // POST /api/transactions
    // Return: TransactionDto
  }
}

// src/services/userService.js (Admin Only)
export default {
  async listUsers(userType, page, size) {
    // GET /api/users
    // Return: Page<UserDto>
  },
  
  async getUser(userId) {
    // GET /api/users/{userId}
    // Return: UserDto
  },
  
  async createUser(userData) {
    // POST /api/users
    // Return: UserDto
  },
  
  async updateUser(userId, updates) {
    // PUT /api/users/{userId}
    // Return: UserDto
  },
  
  async deactivateUser(userId) {
    // DELETE /api/users/{userId}
    // Return: {message}
  }
}
```

#### Frontend Routing

```javascript
// src/router/index.js
const routes = [
  {
    path: '/',
    redirect: '/login'
  },
  {
    path: '/login',
    name: 'Login',
    component: () => import('@/views/Login.vue')
  },
  {
    path: '/menu',
    name: 'MainMenu',
    component: () => import('@/views/MainMenu.vue'),
    meta: { requiresAuth: true }
  },
  {
    path: '/admin-menu',
    name: 'AdminMenu',
    component: () => import('@/views/AdminMenu.vue'),
    meta: { requiresAuth: true, requiresAdmin: true }
  },
  {
    path: '/accounts/view',
    name: 'AccountView',
    component: () => import('@/views/AccountView.vue'),
    meta: { requiresAuth: true }
  },
  {
    path: '/accounts/update',
    name: 'AccountUpdate',
    component: () => import('@/views/AccountUpdate.vue'),
    meta: { requiresAuth: true }
  },
  {
    path: '/cards/list',
    name: 'CardList',
    component: () => import('@/views/CardList.vue'),
    meta: { requiresAuth: true }
  },
  {
    path: '/cards/detail/:cardNumber',
    name: 'CardDetail',
    component: () => import('@/views/CardDetail.vue'),
    meta: { requiresAuth: true }
  },
  {
    path: '/cards/update/:cardNumber',
    name: 'CardUpdate',
    component: () => import('@/views/CardUpdate.vue'),
    meta: { requiresAuth: true }
  },
  {
    path: '/transactions/list',
    name: 'TransactionList',
    component: () => import('@/views/TransactionList.vue'),
    meta: { requiresAuth: true }
  },
  {
    path: '/transactions/view/:transactionId',
    name: 'TransactionView',
    component: () => import('@/views/TransactionView.vue'),
    meta: { requiresAuth: true }
  },
  {
    path: '/bills/payment',
    name: 'BillPayment',
    component: () => import('@/views/BillPayment.vue'),
    meta: { requiresAuth: true }
  },
  {
    path: '/reports',
    name: 'Reports',
    component: () => import('@/views/Reports.vue'),
    meta: { requiresAuth: true }
  },
  {
    path: '/users',
    name: 'UserManagement',
    component: () => import('@/views/UserManagement.vue'),
    meta: { requiresAuth: true, requiresAdmin: true }
  }
]

// Navigation guard for authentication
router.beforeEach((to, from, next) => {
  const requiresAuth = to.matched.some(record => record.meta.requiresAuth)
  const requiresAdmin = to.matched.some(record => record.meta.requiresAdmin)
  const token = authService.getToken()
  const userInfo = authService.getUserInfo()
  
  if (requiresAuth && !token) {
    next('/login')
  } else if (requiresAdmin && userInfo.userType !== 'A') {
    next('/menu')
  } else {
    next()
  }
})
```

### Backend Components

#### 1. Authentication Module

**Purpose**: Handle user authentication and session management

**Components**:
- `AuthController`: REST endpoints for login/logout
- `AuthService`: Business logic for credential validation
- `UserRepository`: Data access for user records
- `JwtTokenProvider`: JWT token generation and validation
- `SecurityConfig`: Spring Security configuration

**Key Classes**:
```java
@RestController
@RequestMapping("/api/auth")
public class AuthController {
    POST /login
    POST /logout
    GET /validate
}

@Service
public class AuthService {
    UserDto authenticate(String userId, String password)
    void logout(String token)
    boolean validateSession(String token)
}

@Entity
@Table(name = "users")
public class User {
    String userId;        // PK, 8 chars
    String firstName;     // 20 chars
    String lastName;      // 20 chars
    String password;      // 8 chars, hashed
    String userType;      // 'A' or 'U'
}
```

#### 2. Menu Navigation Module

**Purpose**: Provide menu options and route to appropriate features

**Components**:
- `MenuController`: REST endpoints for menu operations
- `MenuService`: Business logic for menu generation
- `MenuConfig`: Configuration for menu options

**Key Classes**:
```java
@RestController
@RequestMapping("/api/menu")
public class MenuController {
    GET /options
    POST /select
}

@Service
public class MenuService {
    List<MenuOption> getMenuOptions(String userType)
    String validateAndRoute(int optionNumber, String userType)
}

public class MenuOption {
    int optionNumber;
    String optionName;
    String programName;
    String userType;      // 'A', 'U', or 'B' (both)
}
```

#### 3. Account Management Module

**Purpose**: Handle account viewing and updating operations

**Components**:
- `AccountController`: REST endpoints for account operations
- `AccountService`: Business logic for account management
- `AccountRepository`: Data access for account records
- `CustomerRepository`: Data access for customer records

**Key Classes**:
```java
@RestController
@RequestMapping("/api/accounts")
public class AccountController {
    GET /{accountId}
    PUT /{accountId}
    GET /{accountId}/customer
    GET /{accountId}/cards
}

@Service
public class AccountService {
    AccountDto getAccount(Long accountId)
    AccountDto updateAccount(Long accountId, AccountUpdateDto updates)
    CustomerDto getCustomerForAccount(Long accountId)
    List<CardDto> getCardsForAccount(Long accountId)
}

@Entity
@Table(name = "accounts")
public class Account {
    Long accountId;                  // PK, 11 digits
    String activeStatus;             // 'Y' or 'N'
    BigDecimal currentBalance;       // S9(10)V99
    BigDecimal creditLimit;          // S9(10)V99
    BigDecimal cashCreditLimit;      // S9(10)V99
    String openDate;                 // YYYY-MM-DD
    String expirationDate;           // YYYY-MM-DD
    String reissueDate;              // YYYY-MM-DD
    BigDecimal currentCycleCredit;   // S9(10)V99
    BigDecimal currentCycleDebit;    // S9(10)V99
    String addressZip;               // 10 chars
    String groupId;                  // 10 chars
}

@Entity
@Table(name = "customers")
public class Customer {
    Long customerId;                 // PK, 9 digits
    String firstName;                // 25 chars
    String middleName;               // 25 chars
    String lastName;                 // 25 chars
    String addressLine1;             // 50 chars
    String addressLine2;             // 50 chars
    String addressLine3;             // 50 chars
    String stateCode;                // 2 chars
    String countryCode;              // 3 chars
    String zipCode;                  // 10 chars
    String phoneNumber1;             // 15 chars
    String phoneNumber2;             // 15 chars
    Long ssn;                        // 9 digits
    String governmentIssuedId;       // 20 chars
    String dateOfBirth;              // YYYY-MM-DD
    String eftAccountId;             // 10 chars
    String primaryCardholderInd;     // 'Y' or 'N'
    Integer ficoCreditScore;         // 3 digits
}
```

#### 4. Card Management Module

**Purpose**: Handle card viewing, creation, and updates

**Components**:
- `CardController`: REST endpoints for card operations
- `CardService`: Business logic for card management
- `CardRepository`: Data access for card records

**Key Classes**:
```java
@RestController
@RequestMapping("/api/cards")
public class CardController {
    GET /{cardNumber}
    GET /account/{accountId}
    POST /
    PUT /{cardNumber}
}

@Service
public class CardService {
    CardDto getCard(Long cardNumber)
    List<CardDto> getCardsByAccount(Long accountId)
    CardDto createCard(CardCreateDto cardData)
    CardDto updateCard(Long cardNumber, CardUpdateDto updates)
}

@Entity
@Table(name = "cards")
public class Card {
    Long cardNumber;                 // PK, 16 digits
    Long accountId;                  // FK to accounts
    Long customerId;                 // FK to customers
    String cardStatus;               // Status code
    String expirationDate;           // YYYY-MM-DD
    String issueDate;                // YYYY-MM-DD
}
```

#### 5. Transaction Management Module

**Purpose**: Handle transaction viewing and processing

**Components**:
- `TransactionController`: REST endpoints for transaction operations
- `TransactionService`: Business logic for transaction processing
- `TransactionRepository`: Data access for transaction records

**Key Classes**:
```java
@RestController
@RequestMapping("/api/transactions")
public class TransactionController {
    GET /account/{accountId}
    GET /card/{cardNumber}
    GET /{transactionId}
    POST /
}

@Service
public class TransactionService {
    List<TransactionDto> getTransactionsByAccount(Long accountId)
    List<TransactionDto> getTransactionsByCard(Long cardNumber)
    TransactionDto getTransaction(Long transactionId)
    TransactionDto createTransaction(TransactionCreateDto txnData)
}

@Entity
@Table(name = "transactions")
public class Transaction {
    Long transactionId;              // PK, auto-generated
    Long accountId;                  // FK to accounts
    Long cardNumber;                 // FK to cards
    String transactionType;          // Type code
    BigDecimal amount;               // S9(10)V99
    String transactionDate;          // YYYY-MM-DD
    String description;              // Description text
}
```

### Frontend Components

#### Vue.js Application Structure

```
src/
├── main.js                 # Application entry point
├── App.vue                 # Root component
├── router/
│   └── index.js            # Route definitions
├── store/
│   ├── index.js            # Vuex store
│   └── modules/
│       ├── auth.js         # Authentication state
│       ├── account.js      # Account state
│       └── card.js         # Card state
├── views/
│   ├── Login.vue           # Sign-on screen (COSGN00)
│   ├── MainMenu.vue        # Main menu (COMEN01)
│   ├── AdminMenu.vue       # Admin menu (COADM01)
│   ├── AccountView.vue     # Account view (COACTVW)
│   ├── AccountUpdate.vue   # Account update (COACTUP)
│   ├── CardList.vue        # Card list (COCRDLI)
│   ├── CardDetail.vue      # Card detail (COCRDSL)
│   ├── CardUpdate.vue      # Card update (COCRDUP)
│   ├── TransactionList.vue # Transaction list (COTRN00)
│   ├── TransactionView.vue # Transaction view (COTRN01)
│   ├── BillPayment.vue     # Bill payment (COBIL00)
│   ├── Reports.vue         # Reports (CORPT00)
│   └── UserManagement.vue  # User management (COUSR00)
├── components/
│   ├── Header.vue          # Common header
│   ├── ErrorMessage.vue    # Error display
│   └── LoadingSpinner.vue  # Loading indicator
└── services/
    ├── api.js              # API client
    ├── authService.js      # Authentication API calls
    ├── accountService.js   # Account API calls
    ├── cardService.js      # Card API calls
    └── transactionService.js # Transaction API calls
```

## Data Models

### Database Schema

#### users Table
```sql
CREATE TABLE users (
    user_id VARCHAR(8) PRIMARY KEY,
    first_name VARCHAR(20) NOT NULL,
    last_name VARCHAR(20) NOT NULL,
    password VARCHAR(255) NOT NULL,  -- Hashed with bcrypt
    user_type CHAR(1) NOT NULL CHECK (user_type IN ('A', 'U')),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_users_type ON users(user_type);
```

#### accounts Table
```sql
CREATE TABLE accounts (
    account_id NUMERIC(11) PRIMARY KEY,
    active_status CHAR(1) NOT NULL CHECK (active_status IN ('Y', 'N')),
    current_balance NUMERIC(12, 2) NOT NULL DEFAULT 0,
    credit_limit NUMERIC(12, 2) NOT NULL,
    cash_credit_limit NUMERIC(12, 2) NOT NULL,
    open_date VARCHAR(10) NOT NULL,
    expiration_date VARCHAR(10),
    reissue_date VARCHAR(10),
    current_cycle_credit NUMERIC(12, 2) DEFAULT 0,
    current_cycle_debit NUMERIC(12, 2) DEFAULT 0,
    address_zip VARCHAR(10),
    group_id VARCHAR(10),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_accounts_status ON accounts(active_status);
CREATE INDEX idx_accounts_zip ON accounts(address_zip);
```

#### customers Table
```sql
CREATE TABLE customers (
    customer_id NUMERIC(9) PRIMARY KEY,
    first_name VARCHAR(25) NOT NULL,
    middle_name VARCHAR(25),
    last_name VARCHAR(25) NOT NULL,
    address_line_1 VARCHAR(50),
    address_line_2 VARCHAR(50),
    address_line_3 VARCHAR(50),
    state_code CHAR(2),
    country_code CHAR(3),
    zip_code VARCHAR(10),
    phone_number_1 VARCHAR(15),
    phone_number_2 VARCHAR(15),
    ssn NUMERIC(9),
    government_issued_id VARCHAR(20),
    date_of_birth VARCHAR(10),
    eft_account_id VARCHAR(10),
    primary_cardholder_ind CHAR(1) CHECK (primary_cardholder_ind IN ('Y', 'N')),
    fico_credit_score NUMERIC(3),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_customers_name ON customers(last_name, first_name);
CREATE INDEX idx_customers_ssn ON customers(ssn);
```

#### cards Table
```sql
CREATE TABLE cards (
    card_number NUMERIC(16) PRIMARY KEY,
    account_id NUMERIC(11) NOT NULL,
    customer_id NUMERIC(9) NOT NULL,
    card_status VARCHAR(10) NOT NULL,
    expiration_date VARCHAR(10),
    issue_date VARCHAR(10),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (account_id) REFERENCES accounts(account_id),
    FOREIGN KEY (customer_id) REFERENCES customers(customer_id)
);

CREATE INDEX idx_cards_account ON cards(account_id);
CREATE INDEX idx_cards_customer ON cards(customer_id);
CREATE INDEX idx_cards_status ON cards(card_status);
```

#### transactions Table
```sql
CREATE TABLE transactions (
    transaction_id BIGSERIAL PRIMARY KEY,
    account_id NUMERIC(11) NOT NULL,
    card_number NUMERIC(16),
    transaction_type VARCHAR(10) NOT NULL,
    transaction_category VARCHAR(20),
    amount NUMERIC(12, 2) NOT NULL,
    transaction_date VARCHAR(10) NOT NULL,
    transaction_time VARCHAR(8),
    description VARCHAR(100),
    merchant_name VARCHAR(50),
    merchant_city VARCHAR(30),
    merchant_zip VARCHAR(10),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (account_id) REFERENCES accounts(account_id),
    FOREIGN KEY (card_number) REFERENCES cards(card_number)
);

CREATE INDEX idx_transactions_account ON transactions(account_id);
CREATE INDEX idx_transactions_card ON transactions(card_number);
CREATE INDEX idx_transactions_date ON transactions(transaction_date);
CREATE INDEX idx_transactions_type ON transactions(transaction_type);
```

#### card_xref Table
```sql
CREATE TABLE card_xref (
    card_number NUMERIC(16) NOT NULL,
    customer_id NUMERIC(9) NOT NULL,
    account_id NUMERIC(11) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (card_number, customer_id, account_id),
    FOREIGN KEY (card_number) REFERENCES cards(card_number),
    FOREIGN KEY (customer_id) REFERENCES customers(customer_id),
    FOREIGN KEY (account_id) REFERENCES accounts(account_id)
);

CREATE INDEX idx_card_xref_account ON card_xref(account_id);
CREATE INDEX idx_card_xref_customer ON card_xref(customer_id);
```

### Data Transfer Objects (DTOs)

#### Authentication DTOs
```java
public class LoginRequestDto {
    String userId;
    String password;
}

public class LoginResponseDto {
    String token;
    String userId;
    String userType;
    String firstName;
    String lastName;
}

public class SessionDto {
    String userId;
    String userType;
    String fromTransactionId;
    String fromProgram;
}
```

#### Account DTOs
```java
public class AccountDto {
    Long accountId;
    String activeStatus;
    BigDecimal currentBalance;
    BigDecimal creditLimit;
    BigDecimal cashCreditLimit;
    String openDate;
    String expirationDate;
    String reissueDate;
    BigDecimal currentCycleCredit;
    BigDecimal currentCycleDebit;
    String addressZip;
    String groupId;
    CustomerDto customer;
    List<CardDto> cards;
}

public class AccountUpdateDto {
    String activeStatus;
    BigDecimal creditLimit;
    BigDecimal cashCreditLimit;
    BigDecimal currentBalance;
    BigDecimal currentCycleCredit;
    BigDecimal currentCycleDebit;
}
```

## Correctness Properties

A property is a characteristic or behavior that should hold true across all valid executions of a system—essentially, a formal statement about what the system should do. Properties serve as the bridge between human-readable specifications and machine-verifiable correctness guarantees.


### Authentication and Authorization Properties

**Property 1: Credential Validation**
*For any* user ID and password combination, when submitted for authentication, the system should validate against the database and return either a valid session (if credentials match) or an appropriate error message (if credentials don't match or are invalid)
**Validates: Requirements 1.1, 1.2, 1.3, 1.4**

**Property 2: User Type Routing**
*For any* authenticated user, the system should route admin users (type 'A') to the admin menu and regular users (type 'U') to the main menu
**Validates: Requirements 1.7, 1.8**

### Menu Navigation Properties

**Property 3: Menu Option Validation**
*For any* menu option input, the system should validate that it is numeric and within the valid range (1-11), returning an error message for invalid inputs
**Validates: Requirements 2.3, 2.4**

**Property 4: Menu Option Routing**
*For any* valid menu option number, the system should return the correct program name for that option
**Validates: Requirements 2.5**

**Property 5: Admin Option Authorization**
*For any* admin-only menu option, when selected by a non-admin user, the system should return an authorization error message
**Validates: Requirements 2.7**

### Account Management Properties

**Property 6: Account ID Validation**
*For any* account ID input, the system should validate that it is numeric, exactly 11 digits, and not all zeroes, returning appropriate error messages for invalid inputs
**Validates: Requirements 3.1, 3.3, 3.4, 4.1**

**Property 7: Account Data Retrieval**
*For any* valid account ID, when retrieving account data, the system should return the account record, associated customer record, and all associated card records
**Validates: Requirements 3.5, 3.6, 3.7**

**Property 8: Account Not Found Handling**
*For any* non-existent account ID, the system should return an appropriate error message indicating the account was not found
**Validates: Requirements 3.8**

**Property 9: Customer Referential Integrity**
*For any* account record, if the associated customer does not exist, the system should return an error message indicating the customer was not found
**Validates: Requirements 3.9**

**Property 10: Account Response Completeness**
*For any* successfully retrieved account, the API response should include all required fields: account ID, active status, current balance, credit limit, cash credit limit, open date, expiration date, reissue date, current cycle credit, current cycle debit, and customer information
**Validates: Requirements 3.10**

**Property 11: Account Update Validation**
*For any* account update request, the system should validate all modified fields according to their data types and business rules, returning field-specific error messages for validation failures
**Validates: Requirements 4.2, 4.6**

**Property 12: Account Update Persistence**
*For any* valid account update, the system should persist the changes to the database and return a success confirmation
**Validates: Requirements 4.3, 4.5**

**Property 13: Customer Update Persistence**
*For any* valid customer information update, the system should persist the changes to the customer record in the database
**Validates: Requirements 4.4**

### Data Migration Properties

**Property 14: VSAM to PostgreSQL Conversion Round Trip**
*For any* valid VSAM record (USRSEC, ACCTDAT, CUSTDAT, CARDDAT, or TRANSACT), converting to PostgreSQL format and then reading back should produce an equivalent record with all field values preserved
**Validates: Requirements 10.2, 10.3, 10.4, 10.5, 10.6**

**Property 15: Migration Error Handling**
*For any* data conversion error during migration, the utility should log the error and continue processing remaining records
**Validates: Requirements 10.7**

### Data Validation Properties

**Property 16: Numeric ID Validation**
*For any* numeric identifier input (account ID, card number, customer ID, user ID), the system should validate the format matches the expected length and pattern, rejecting invalid inputs with appropriate error messages
**Validates: Requirements 17.1, 17.2, 17.5**

**Property 17: Date Format Validation**
*For any* date input, the system should validate YYYY-MM-DD format with valid year, month (1-12), and day ranges, rejecting invalid dates with error messages
**Validates: Requirements 17.3**

**Property 18: Currency Amount Validation**
*For any* currency amount input, the system should validate signed numeric format with exactly two decimal places, rejecting invalid amounts with error messages
**Validates: Requirements 17.4**

**Property 19: Enumerated Value Validation**
*For any* enumerated field input (account status, user type, primary cardholder indicator), the system should validate the value is one of the allowed values, rejecting invalid values with error messages
**Validates: Requirements 17.7, 17.8**

**Property 20: Name Validation**
*For any* customer name input (first, middle, last), the system should validate that it contains only alphabetic characters and spaces, rejecting invalid names with error messages
**Validates: Requirements 17.9**

**Property 21: US Phone Number Validation**
*For any* US phone number input, the system should validate the format (XXX)XXX-XXXX with numeric digits, rejecting invalid phone numbers with error messages
**Validates: Requirements 17.10**

**Property 22: US SSN Validation**
*For any* US SSN input, the system should validate 9-digit numeric format and reject invalid SSN patterns (000, 666, 900-999 in first part) with error messages
**Validates: Requirements 17.11**

**Property 23: FICO Score Validation**
*For any* FICO credit score input, the system should validate 3-digit numeric format, rejecting invalid scores with error messages
**Validates: Requirements 17.12**

**Property 24: User ID Case Normalization**
*For any* user ID input, the system should convert it to uppercase before validation and storage
**Validates: Requirements 17.5**

## Error Handling

### Error Response Structure

All API errors will follow a consistent structure:

```json
{
  "timestamp": "2024-12-24T10:30:00Z",
  "status": 400,
  "error": "Bad Request",
  "message": "Account number must be a non zero 11 digit number",
  "path": "/api/accounts/00000000000",
  "fieldErrors": [
    {
      "field": "accountId",
      "message": "Account number must be a non zero 11 digit number",
      "rejectedValue": "00000000000"
    }
  ]
}
```

### Error Categories

#### 1. Validation Errors (HTTP 400)
- Invalid input format
- Missing required fields
- Business rule violations
- Field-level validation failures

**Examples**:
- "Please enter User ID ..."
- "Account number must be a non zero 11 digit number"
- "Account Active Status must be Y or N"
- "Credit Limit must be supplied"

#### 2. Authentication Errors (HTTP 401)
- Invalid credentials
- Expired session
- Missing authentication token

**Examples**:
- "User not found. Try again ..."
- "Wrong Password. Try again ..."
- "Session expired. Please login again"

#### 3. Authorization Errors (HTTP 403)
- Insufficient permissions
- Admin-only feature access by regular user

**Examples**:
- "No access - Admin Only option... "

#### 4. Not Found Errors (HTTP 404)
- Resource does not exist
- Invalid ID references

**Examples**:
- "Did not find this account in account master file"
- "Did not find associated customer in master file"

#### 5. Server Errors (HTTP 500)
- Database connection failures
- Unexpected exceptions
- System errors

**Examples**:
- "Unable to verify the User ..."
- "Error reading account card xref File"

### Exception Handling Strategy

```java
@ControllerAdvice
public class GlobalExceptionHandler {
    
    @ExceptionHandler(ValidationException.class)
    public ResponseEntity<ErrorResponse> handleValidationException(
        ValidationException ex) {
        // Return 400 with field-specific errors
    }
    
    @ExceptionHandler(AuthenticationException.class)
    public ResponseEntity<ErrorResponse> handleAuthenticationException(
        AuthenticationException ex) {
        // Return 401 with authentication error
    }
    
    @ExceptionHandler(AuthorizationException.class)
    public ResponseEntity<ErrorResponse> handleAuthorizationException(
        AuthorizationException ex) {
        // Return 403 with authorization error
    }
    
    @ExceptionHandler(ResourceNotFoundException.class)
    public ResponseEntity<ErrorResponse> handleNotFoundException(
        ResourceNotFoundException ex) {
        // Return 404 with not found error
    }
    
    @ExceptionHandler(Exception.class)
    public ResponseEntity<ErrorResponse> handleGenericException(
        Exception ex) {
        // Log error, return 500 with generic message
    }
}
```

### Logging Strategy

#### Log Levels

- **ERROR**: System errors, database failures, unexpected exceptions
- **WARN**: Authentication failures, authorization denials, business rule violations
- **INFO**: Successful operations, business events (login, logout, updates)
- **DEBUG**: Detailed execution flow, method entry/exit
- **TRACE**: Very detailed debugging information

#### Sensitive Data Handling

Never log:
- Passwords (plain or hashed)
- Full credit card numbers (mask all but last 4 digits)
- Full SSN (mask all but last 4 digits)
- Session tokens

Always log:
- User ID (for audit trail)
- Timestamp
- Operation type
- Success/failure status
- Error codes and messages

## Testing Strategy

### Dual Testing Approach

The modernized CardDemo system will employ both unit testing and property-based testing to ensure comprehensive coverage and correctness.

#### Unit Tests

Unit tests verify specific examples, edge cases, and error conditions. They are valuable for:
- Testing specific business scenarios
- Verifying integration points between components
- Testing edge cases like empty inputs, boundary values
- Ensuring error messages match expected text

**Example Unit Tests**:
```java
@Test
public void testAuthenticateWithValidCredentials() {
    // Given a valid user
    User user = createTestUser("TESTUSER", "password", "U");
    userRepository.save(user);
    
    // When authenticating
    LoginResponseDto response = authService.authenticate("TESTUSER", "password");
    
    // Then session is created
    assertNotNull(response.getToken());
    assertEquals("TESTUSER", response.getUserId());
    assertEquals("U", response.getUserType());
}

@Test
public void testAuthenticateWithInvalidUserId() {
    // When authenticating with non-existent user
    Exception exception = assertThrows(AuthenticationException.class, () -> {
        authService.authenticate("INVALID", "password");
    });
    
    // Then error message matches
    assertEquals("User not found. Try again ...", exception.getMessage());
}

@Test
public void testValidateAccountIdWithZeroes() {
    // When validating all-zero account ID
    ValidationResult result = accountService.validateAccountId("00000000000");
    
    // Then validation fails with specific message
    assertFalse(result.isValid());
    assertEquals("Account number must be a non zero 11 digit number", 
                 result.getMessage());
}
```

#### Property-Based Tests

Property-based tests verify universal properties across all inputs. They are valuable for:
- Testing that properties hold for all valid inputs
- Discovering edge cases through randomization
- Verifying data transformations and round-trips
- Ensuring validation rules work consistently

**Property Test Configuration**:
- Minimum 100 iterations per property test
- Each property test references its design document property
- Tag format: **Feature: carddemo-modernization, Property {number}: {property_text}**

**Example Property Tests**:
```java
@Property
@Tag("Feature: carddemo-modernization, Property 1: Credential Validation")
public void testCredentialValidationProperty(
    @ForAll("validUsers") User user,
    @ForAll("passwords") String password) {
    
    // For any user and password combination
    // Authentication should either succeed (if password matches) or fail with appropriate error
    
    if (password.equals(user.getPassword())) {
        LoginResponseDto response = authService.authenticate(
            user.getUserId(), password);
        assertNotNull(response.getToken());
        assertEquals(user.getUserId(), response.getUserId());
    } else {
        Exception exception = assertThrows(AuthenticationException.class, () -> {
            authService.authenticate(user.getUserId(), password);
        });
        assertTrue(exception.getMessage().contains("Wrong Password") ||
                   exception.getMessage().contains("User not found"));
    }
}

@Property
@Tag("Feature: carddemo-modernization, Property 6: Account ID Validation")
public void testAccountIdValidationProperty(
    @ForAll("accountIdInputs") String accountId) {
    
    // For any account ID input
    // Validation should correctly identify valid vs invalid IDs
    
    ValidationResult result = accountService.validateAccountId(accountId);
    
    boolean isValid = accountId.matches("\\d{11}") && 
                      !accountId.equals("00000000000");
    
    assertEquals(isValid, result.isValid());
    
    if (!result.isValid()) {
        assertNotNull(result.getMessage());
        assertTrue(result.getMessage().length() > 0);
    }
}

@Property
@Tag("Feature: carddemo-modernization, Property 14: VSAM to PostgreSQL Conversion Round Trip")
public void testVsamToPostgresRoundTripProperty(
    @ForAll("vsamAccountRecords") VsamAccountRecord vsamRecord) {
    
    // For any VSAM account record
    // Converting to PostgreSQL and back should preserve all values
    
    Account pgAccount = migrationService.convertVsamToPostgres(vsamRecord);
    VsamAccountRecord roundTripped = migrationService.convertPostgresToVsam(pgAccount);
    
    assertEquals(vsamRecord.getAccountId(), roundTripped.getAccountId());
    assertEquals(vsamRecord.getActiveStatus(), roundTripped.getActiveStatus());
    assertEquals(vsamRecord.getCurrentBalance(), roundTripped.getCurrentBalance());
    assertEquals(vsamRecord.getCreditLimit(), roundTripped.getCreditLimit());
    // ... assert all fields match
}

@Property
@Tag("Feature: carddemo-modernization, Property 24: User ID Case Normalization")
public void testUserIdCaseNormalizationProperty(
    @ForAll("userIds") String userId) {
    
    // For any user ID input
    // The system should convert it to uppercase
    
    String normalized = authService.normalizeUserId(userId);
    assertEquals(userId.toUpperCase(), normalized);
}
```

**Property Test Generators**:
```java
@Provide
Arbitrary<User> validUsers() {
    return Combinators.combine(
        Arbitraries.strings().alpha().ofLength(8),
        Arbitraries.strings().alpha().ofLength(20),
        Arbitraries.strings().alpha().ofLength(20),
        Arbitraries.strings().ofLength(8),
        Arbitraries.of('A', 'U')
    ).as((id, fname, lname, pwd, type) -> 
        new User(id, fname, lname, pwd, String.valueOf(type)));
}

@Provide
Arbitrary<String> accountIdInputs() {
    return Arbitraries.oneOf(
        Arbitraries.strings().numeric().ofLength(11),  // Valid format
        Arbitraries.strings().ofLength(11),            // Invalid characters
        Arbitraries.strings().numeric().ofMinLength(1).ofMaxLength(10), // Too short
        Arbitraries.strings().numeric().ofMinLength(12).ofMaxLength(20), // Too long
        Arbitraries.just("00000000000")                // All zeroes
    );
}

@Provide
Arbitrary<VsamAccountRecord> vsamAccountRecords() {
    return Combinators.combine(
        Arbitraries.longs().between(10000000000L, 99999999999L),
        Arbitraries.of('Y', 'N'),
        Arbitraries.bigDecimals().between(
            BigDecimal.valueOf(-9999999999.99), 
            BigDecimal.valueOf(9999999999.99)),
        // ... other fields
    ).as(VsamAccountRecord::new);
}
```

### Integration Testing

Integration tests verify that components work together correctly:
- REST API endpoints with database
- Service layer with repository layer
- Authentication with authorization
- Data migration utilities with database

### End-to-End Testing

E2E tests verify complete user workflows:
- User login → menu navigation → account view
- User login → account update → verification
- Admin login → user management → user creation

## Deployment and Configuration

### Application Configuration

#### application.yml (Development)
```yaml
spring:
  application:
    name: carddemo-api
  datasource:
    url: jdbc:postgresql://localhost:5432/carddemo
    username: carddemo_user
    password: ${DB_PASSWORD}
    driver-class-name: org.postgresql.Driver
  jpa:
    hibernate:
      ddl-auto: validate
    show-sql: true
    properties:
      hibernate:
        dialect: org.hibernate.dialect.PostgreSQLDialect
        format_sql: true
  flyway:
    enabled: true
    locations: classpath:db/migration
    baseline-on-migrate: true

jwt:
  secret: ${JWT_SECRET}
  expiration: 3600000  # 1 hour in milliseconds

logging:
  level:
    com.carddemo: DEBUG
    org.springframework.web: INFO
    org.hibernate.SQL: DEBUG
```

#### application-prod.yml (Production)
```yaml
spring:
  datasource:
    url: ${DATABASE_URL}
    username: ${DB_USERNAME}
    password: ${DB_PASSWORD}
    hikari:
      maximum-pool-size: 20
      minimum-idle: 5
      connection-timeout: 30000
  jpa:
    show-sql: false
    properties:
      hibernate:
        format_sql: false

logging:
  level:
    com.carddemo: INFO
    org.springframework.web: WARN
```

### Docker Configuration

#### Dockerfile (Backend)
```dockerfile
FROM eclipse-temurin:17-jdk-alpine AS build
WORKDIR /app
COPY mvnw .
COPY .mvn .mvn
COPY pom.xml .
COPY src src
RUN ./mvnw clean package -DskipTests

FROM eclipse-temurin:17-jre-alpine
WORKDIR /app
COPY --from=build /app/target/carddemo-api-*.jar app.jar
EXPOSE 8080
ENTRYPOINT ["java", "-jar", "app.jar"]
```

#### Dockerfile (Frontend)
```dockerfile
FROM node:18-alpine AS build
WORKDIR /app
COPY package*.json ./
RUN npm ci
COPY . .
RUN npm run build

FROM nginx:alpine
COPY --from=build /app/dist /usr/share/nginx/html
COPY nginx.conf /etc/nginx/nginx.conf
EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
```

#### docker-compose.yml
```yaml
version: '3.8'

services:
  postgres:
    image: postgres:16.9-alpine
    environment:
      POSTGRES_DB: carddemo
      POSTGRES_USER: carddemo_user
      POSTGRES_PASSWORD: ${DB_PASSWORD}
    ports:
      - "5432:5432"
    volumes:
      - postgres_data:/var/lib/postgresql/data

  backend:
    build: ./backend
    ports:
      - "8080:8080"
    environment:
      DATABASE_URL: jdbc:postgresql://postgres:5432/carddemo
      DB_USERNAME: carddemo_user
      DB_PASSWORD: ${DB_PASSWORD}
      JWT_SECRET: ${JWT_SECRET}
    depends_on:
      - postgres

  frontend:
    build: ./frontend
    ports:
      - "80:80"
    depends_on:
      - backend

volumes:
  postgres_data:
```

### Database Migration Scripts

Database schema changes will be managed using Flyway migrations:

#### V1__initial_schema.sql
```sql
-- Create users table
CREATE TABLE users (
    user_id VARCHAR(8) PRIMARY KEY,
    first_name VARCHAR(20) NOT NULL,
    last_name VARCHAR(20) NOT NULL,
    password VARCHAR(255) NOT NULL,
    user_type CHAR(1) NOT NULL CHECK (user_type IN ('A', 'U')),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create accounts table
CREATE TABLE accounts (
    account_id NUMERIC(11) PRIMARY KEY,
    active_status CHAR(1) NOT NULL CHECK (active_status IN ('Y', 'N')),
    current_balance NUMERIC(12, 2) NOT NULL DEFAULT 0,
    credit_limit NUMERIC(12, 2) NOT NULL,
    cash_credit_limit NUMERIC(12, 2) NOT NULL,
    open_date VARCHAR(10) NOT NULL,
    expiration_date VARCHAR(10),
    reissue_date VARCHAR(10),
    current_cycle_credit NUMERIC(12, 2) DEFAULT 0,
    current_cycle_debit NUMERIC(12, 2) DEFAULT 0,
    address_zip VARCHAR(10),
    group_id VARCHAR(10),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create customers table
CREATE TABLE customers (
    customer_id NUMERIC(9) PRIMARY KEY,
    first_name VARCHAR(25) NOT NULL,
    middle_name VARCHAR(25),
    last_name VARCHAR(25) NOT NULL,
    address_line_1 VARCHAR(50),
    address_line_2 VARCHAR(50),
    address_line_3 VARCHAR(50),
    state_code CHAR(2),
    country_code CHAR(3),
    zip_code VARCHAR(10),
    phone_number_1 VARCHAR(15),
    phone_number_2 VARCHAR(15),
    ssn NUMERIC(9),
    government_issued_id VARCHAR(20),
    date_of_birth VARCHAR(10),
    eft_account_id VARCHAR(10),
    primary_cardholder_ind CHAR(1) CHECK (primary_cardholder_ind IN ('Y', 'N')),
    fico_credit_score NUMERIC(3),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create cards table
CREATE TABLE cards (
    card_number NUMERIC(16) PRIMARY KEY,
    account_id NUMERIC(11) NOT NULL,
    customer_id NUMERIC(9) NOT NULL,
    card_status VARCHAR(10) NOT NULL,
    expiration_date VARCHAR(10),
    issue_date VARCHAR(10),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (account_id) REFERENCES accounts(account_id),
    FOREIGN KEY (customer_id) REFERENCES customers(customer_id)
);

-- Create transactions table
CREATE TABLE transactions (
    transaction_id BIGSERIAL PRIMARY KEY,
    account_id NUMERIC(11) NOT NULL,
    card_number NUMERIC(16),
    transaction_type VARCHAR(10) NOT NULL,
    transaction_category VARCHAR(20),
    amount NUMERIC(12, 2) NOT NULL,
    transaction_date VARCHAR(10) NOT NULL,
    transaction_time VARCHAR(8),
    description VARCHAR(100),
    merchant_name VARCHAR(50),
    merchant_city VARCHAR(30),
    merchant_zip VARCHAR(10),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (account_id) REFERENCES accounts(account_id),
    FOREIGN KEY (card_number) REFERENCES cards(card_number)
);

-- Create card_xref table
CREATE TABLE card_xref (
    card_number NUMERIC(16) NOT NULL,
    customer_id NUMERIC(9) NOT NULL,
    account_id NUMERIC(11) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (card_number, customer_id, account_id),
    FOREIGN KEY (card_number) REFERENCES cards(card_number),
    FOREIGN KEY (customer_id) REFERENCES customers(customer_id),
    FOREIGN KEY (account_id) REFERENCES accounts(account_id)
);

-- Create indexes
CREATE INDEX idx_users_type ON users(user_type);
CREATE INDEX idx_accounts_status ON accounts(active_status);
CREATE INDEX idx_accounts_zip ON accounts(address_zip);
CREATE INDEX idx_customers_name ON customers(last_name, first_name);
CREATE INDEX idx_customers_ssn ON customers(ssn);
CREATE INDEX idx_cards_account ON cards(account_id);
CREATE INDEX idx_cards_customer ON cards(customer_id);
CREATE INDEX idx_cards_status ON cards(card_status);
CREATE INDEX idx_transactions_account ON transactions(account_id);
CREATE INDEX idx_transactions_card ON transactions(card_number);
CREATE INDEX idx_transactions_date ON transactions(transaction_date);
CREATE INDEX idx_transactions_type ON transactions(transaction_type);
CREATE INDEX idx_card_xref_account ON card_xref(account_id);
CREATE INDEX idx_card_xref_customer ON card_xref(customer_id);
```

### Environment Variables

Required environment variables for deployment:

| Variable | Description | Example |
|----------|-------------|---------|
| DATABASE_URL | PostgreSQL connection URL | jdbc:postgresql://localhost:5432/carddemo |
| DB_USERNAME | Database username | carddemo_user |
| DB_PASSWORD | Database password | (secret) |
| JWT_SECRET | Secret key for JWT signing | (secret, min 256 bits) |
| SPRING_PROFILES_ACTIVE | Active Spring profile | prod |
| LOG_LEVEL | Logging level | INFO |

### Deployment Steps

1. **Build Docker images**:
   ```bash
   docker-compose build
   ```

2. **Start services**:
   ```bash
   docker-compose up -d
   ```

3. **Run database migrations**:
   ```bash
   docker-compose exec backend java -jar app.jar --spring.flyway.migrate
   ```

4. **Verify deployment**:
   ```bash
   curl http://localhost:8080/actuator/health
   curl http://localhost/
   ```

5. **Load initial data** (if needed):
   ```bash
   docker-compose exec backend java -jar app.jar --spring.batch.job.names=dataLoadJob
   ```

## Summary

This design document provides a comprehensive blueprint for modernizing the CardDemo mainframe application to a modern three-tier architecture. The design:

- Preserves all functional behaviors from the original COBOL system
- Adopts modern architectural patterns (REST APIs, SPA, relational database)
- Implements comprehensive validation and error handling
- Provides property-based testing for correctness verification
- Supports cloud-native deployment with Docker and environment-based configuration
- Maintains data integrity through proper schema design and constraints
- Ensures security through JWT authentication, password hashing, and sensitive data masking

The modernized system will provide the same business capabilities as the original while offering improved maintainability, scalability, and developer experience.
