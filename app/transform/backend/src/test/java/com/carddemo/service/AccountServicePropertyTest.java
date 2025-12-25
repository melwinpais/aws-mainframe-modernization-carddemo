package com.carddemo.service;

import com.carddemo.dto.*;
import com.carddemo.entity.Account;
import com.carddemo.entity.Card;
import com.carddemo.entity.Customer;
import com.carddemo.exception.ResourceNotFoundException;
import com.carddemo.exception.ValidationException;
import com.carddemo.repository.AccountRepository;
import com.carddemo.repository.CardRepository;
import com.carddemo.repository.CustomerRepository;
import net.jqwik.api.*;
import org.mockito.ArgumentMatchers;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Property-based tests for AccountService
 * Tests universal properties across all inputs
 * Validates Requirements 3.1-3.11, 4.1-4.6
 */
class AccountServicePropertyTest {

    private AccountRepository accountRepository;
    private CustomerRepository customerRepository;
    private CardRepository cardRepository;
    private ValidationService validationService;
    private AccountServiceImpl accountService;

    /**
     * Property 6: Account ID Validation
     * For any account ID input, the system should validate that it is numeric,
     * exactly 11 digits, and not all zeroes, returning appropriate error messages for invalid inputs
     * Validates: Requirements 3.1, 3.3, 3.4, 4.1
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 6: Account ID Validation")
    void testAccountIdValidation(
            @ForAll("accountIdInputs") String accountId) {
        
        // Setup mocks for each test iteration
        setupMocks();
        
        // Test validation
        ValidationResult result = accountService.validateAccountId(accountId);
        
        // Determine expected validity
        boolean shouldBeValid = accountId != null && 
                                !accountId.trim().isEmpty() &&
                                accountId.matches("\\d{11}") && 
                                !accountId.equals("00000000000");
        
        // Verify validation result
        assertThat(result.isValid()).isEqualTo(shouldBeValid);
        
        if (!result.isValid()) {
            // Verify error message is present
            assertThat(result.getMessage()).isNotNull();
            assertThat(result.getMessage()).isNotEmpty();
            
            // Verify specific error messages
            if (accountId == null || accountId.trim().isEmpty()) {
                assertThat(result.getMessage()).contains("Account number not provided");
            } else if (accountId.equals("00000000000")) {
                assertThat(result.getMessage()).contains("Account number must be a non zero 11 digit number");
            } else if (!accountId.matches("\\d+") || accountId.length() != 11) {
                assertThat(result.getMessage()).contains("Account number must be a non zero 11 digit number");
            }
        }
    }

    /**
     * Property 7: Account Data Retrieval
     * For any valid account ID, when retrieving account data, the system should return
     * the account record, associated customer record, and all associated card records
     * Validates: Requirements 3.5, 3.6, 3.7
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 7: Account Data Retrieval")
    void testAccountDataRetrieval(
            @ForAll("validAccountIds") Long accountId,
            @ForAll("validCustomerIds") Long customerId) {
        
        // Setup mocks for each test iteration
        setupMocks();
        
        // Create test account
        Account testAccount = createTestAccount(accountId, customerId);
        
        // Create test customer
        Customer testCustomer = createTestCustomer(customerId);
        
        // Create test cards
        List<Card> testCards = createTestCards(accountId, customerId, 2);
        
        // Mock repository responses
        when(accountRepository.findByAccountId(accountId)).thenReturn(Optional.of(testAccount));
        when(customerRepository.findByCustomerId(customerId)).thenReturn(Optional.of(testCustomer));
        when(accountRepository.existsByAccountId(accountId)).thenReturn(true);
        when(cardRepository.findByAccountId(accountId)).thenReturn(testCards);
        
        // Mock validation service
        when(validationService.validateAccountStatus(anyString())).thenReturn(ValidationResult.success());
        when(validationService.validateCurrencyAmount(anyString())).thenReturn(ValidationResult.success());
        
        // Retrieve account
        AccountDto result = accountService.getAccount(accountId);
        
        // Verify account data is returned
        assertThat(result).isNotNull();
        assertThat(result.getAccountId()).isEqualTo(accountId);
        assertThat(result.getActiveStatus()).isEqualTo(testAccount.getActiveStatus());
        assertThat(result.getCurrentBalance()).isEqualTo(testAccount.getCurrentBalance());
        assertThat(result.getCreditLimit()).isEqualTo(testAccount.getCreditLimit());
        
        // Verify customer data is included
        assertThat(result.getCustomer()).isNotNull();
        assertThat(result.getCustomer().getCustomerId()).isEqualTo(customerId);
        assertThat(result.getCustomer().getFirstName()).isEqualTo(testCustomer.getFirstName());
        assertThat(result.getCustomer().getLastName()).isEqualTo(testCustomer.getLastName());
        
        // Verify cards are included
        assertThat(result.getCards()).isNotNull();
        assertThat(result.getCards()).hasSize(testCards.size());
        
        // Verify repository methods were called
        verify(accountRepository, atLeastOnce()).findByAccountId(accountId);
        verify(customerRepository, atLeastOnce()).findByCustomerId(customerId);
        verify(cardRepository, atLeastOnce()).findByAccountId(accountId);
    }

    /**
     * Property 8: Account Not Found Handling
     * For any non-existent account ID, the system should return an appropriate error message
     * indicating the account was not found
     * Validates: Requirements 3.8
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 8: Account Not Found Handling")
    void testAccountNotFoundHandling(
            @ForAll("validAccountIds") Long accountId) {
        
        // Setup mocks for each test iteration
        setupMocks();
        
        // Mock account not found
        when(accountRepository.findByAccountId(accountId)).thenReturn(Optional.empty());
        
        // Attempt to retrieve non-existent account
        assertThatThrownBy(() -> accountService.getAccount(accountId))
            .isInstanceOf(ResourceNotFoundException.class)
            .hasMessageContaining("Did not find this account in account master file");
        
        // Verify repository was called
        verify(accountRepository).findByAccountId(accountId);
    }

    /**
     * Property 9: Customer Referential Integrity
     * For any account record, if the associated customer does not exist,
     * the system should return an error message indicating the customer was not found
     * Validates: Requirements 3.9
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 9: Customer Referential Integrity")
    void testCustomerReferentialIntegrity(
            @ForAll("validAccountIds") Long accountId,
            @ForAll("validCustomerIds") Long customerId) {
        
        // Setup mocks for each test iteration
        setupMocks();
        
        // Create test account
        Account testAccount = createTestAccount(accountId, customerId);
        
        // Mock account found but customer not found
        when(accountRepository.findByAccountId(accountId)).thenReturn(Optional.of(testAccount));
        when(customerRepository.findByCustomerId(customerId)).thenReturn(Optional.empty());
        
        // Attempt to retrieve account with missing customer
        assertThatThrownBy(() -> accountService.getAccount(accountId))
            .isInstanceOf(ResourceNotFoundException.class)
            .hasMessageContaining("Did not find associated customer in master file");
        
        // Verify repositories were called
        verify(accountRepository, atLeastOnce()).findByAccountId(accountId);
        verify(customerRepository, atLeastOnce()).findByCustomerId(customerId);
    }

    /**
     * Property 10: Account Response Completeness
     * For any successfully retrieved account, the API response should include all required fields:
     * account ID, active status, current balance, credit limit, cash credit limit, open date,
     * expiration date, reissue date, current cycle credit, current cycle debit, and customer information
     * Validates: Requirements 3.10
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 10: Account Response Completeness")
    void testAccountResponseCompleteness(
            @ForAll("validAccountIds") Long accountId,
            @ForAll("validCustomerIds") Long customerId) {
        
        // Setup mocks for each test iteration
        setupMocks();
        
        // Create test account with all fields
        Account testAccount = createTestAccount(accountId, customerId);
        testAccount.setExpirationDate("2025-12-31");
        testAccount.setReissueDate("2024-01-01");
        testAccount.setAddressZip("12345");
        testAccount.setGroupId("GROUP001");
        
        // Create test customer
        Customer testCustomer = createTestCustomer(customerId);
        
        // Create test cards
        List<Card> testCards = createTestCards(accountId, customerId, 1);
        
        // Mock repository responses
        when(accountRepository.findByAccountId(accountId)).thenReturn(Optional.of(testAccount));
        when(customerRepository.findByCustomerId(customerId)).thenReturn(Optional.of(testCustomer));
        when(accountRepository.existsByAccountId(accountId)).thenReturn(true);
        when(cardRepository.findByAccountId(accountId)).thenReturn(testCards);
        
        // Retrieve account
        AccountDto result = accountService.getAccount(accountId);
        
        // Verify all required fields are present
        assertThat(result.getAccountId()).isNotNull();
        assertThat(result.getActiveStatus()).isNotNull();
        assertThat(result.getCurrentBalance()).isNotNull();
        assertThat(result.getCreditLimit()).isNotNull();
        assertThat(result.getCashCreditLimit()).isNotNull();
        assertThat(result.getOpenDate()).isNotNull();
        assertThat(result.getExpirationDate()).isNotNull();
        assertThat(result.getReissueDate()).isNotNull();
        assertThat(result.getCurrentCycleCredit()).isNotNull();
        assertThat(result.getCurrentCycleDebit()).isNotNull();
        
        // Verify customer information is present
        assertThat(result.getCustomer()).isNotNull();
        assertThat(result.getCustomer().getCustomerId()).isNotNull();
        assertThat(result.getCustomer().getFirstName()).isNotNull();
        assertThat(result.getCustomer().getLastName()).isNotNull();
    }

    /**
     * Property 11: Account Update Validation
     * For any account update request, the system should validate all modified fields
     * according to their data types and business rules, returning field-specific error messages
     * for validation failures
     * Validates: Requirements 4.2, 4.6
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 11: Account Update Validation")
    void testAccountUpdateValidation(
            @ForAll("validAccountIds") Long accountId,
            @ForAll("validCustomerIds") Long customerId,
            @ForAll("accountStatuses") String activeStatus) {
        
        // Setup mocks for each test iteration
        setupMocks();
        
        // Create test account
        Account testAccount = createTestAccount(accountId, customerId);
        
        // Mock account found
        when(accountRepository.findByAccountId(accountId)).thenReturn(Optional.of(testAccount));
        
        // Create test customer (needed for both valid and invalid cases)
        Customer testCustomer = createTestCustomer(customerId);
        when(customerRepository.findByCustomerId(customerId)).thenReturn(Optional.of(testCustomer));
        when(accountRepository.existsByAccountId(accountId)).thenReturn(true);
        when(cardRepository.findByAccountId(accountId)).thenReturn(new ArrayList<>());
        
        // Create update DTO with status change
        AccountUpdateDto updates = new AccountUpdateDto();
        updates.setActiveStatus(activeStatus);
        
        // Determine if status is valid
        // Null is valid (means no update), Y and N are valid
        boolean isValidStatus = activeStatus == null || 
                               (activeStatus.equals("Y") || activeStatus.equals("N"));
        
        // Mock validation service based on actual validation logic
        // Only mock if activeStatus is not null (since null skips validation)
        if (activeStatus != null) {
            ValidationResult statusValidation = (activeStatus.equals("Y") || activeStatus.equals("N")) ? 
                ValidationResult.success() : 
                ValidationResult.failure("Account Active Status must be Y or N");
            when(validationService.validateAccountStatus(activeStatus)).thenReturn(statusValidation);
        }
        
        if (isValidStatus) {
            // Mock save operation
            when(accountRepository.save(any(Account.class))).thenReturn(testAccount);
            
            // Update should succeed
            AccountDto result = accountService.updateAccount(accountId, updates);
            assertThat(result).isNotNull();
            
            // Verify validation was called only if activeStatus was not null
            if (activeStatus != null) {
                verify(validationService).validateAccountStatus(activeStatus);
            }
        } else {
            // Update should fail with validation error
            assertThatThrownBy(() -> accountService.updateAccount(accountId, updates))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Active Status");
            
            // Verify validation was called
            verify(validationService).validateAccountStatus(activeStatus);
        }
    }

    /**
     * Property 12: Account Update Persistence
     * For any valid account update, the system should persist the changes to the database
     * and return a success confirmation
     * Validates: Requirements 4.3, 4.5
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 12: Account Update Persistence")
    void testAccountUpdatePersistence(
            @ForAll("validAccountIds") Long accountId,
            @ForAll("validCustomerIds") Long customerId,
            @ForAll("validCreditLimits") BigDecimal newCreditLimit) {
        
        // Setup mocks for each test iteration
        setupMocks();
        
        // Create test account
        Account testAccount = createTestAccount(accountId, customerId);
        BigDecimal originalCreditLimit = testAccount.getCreditLimit();
        
        // Mock account found
        when(accountRepository.findByAccountId(accountId)).thenReturn(Optional.of(testAccount));
        
        // Create update DTO
        AccountUpdateDto updates = new AccountUpdateDto();
        updates.setCreditLimit(newCreditLimit);
        
        // Mock validation service
        when(validationService.validateCurrencyAmount(anyString())).thenReturn(ValidationResult.success());
        
        // Mock save operation
        Account updatedAccount = createTestAccount(accountId, customerId);
        updatedAccount.setCreditLimit(newCreditLimit);
        when(accountRepository.save(any(Account.class))).thenReturn(updatedAccount);
        
        // Mock customer and cards for getAccount call
        Customer testCustomer = createTestCustomer(customerId);
        when(customerRepository.findByCustomerId(customerId)).thenReturn(Optional.of(testCustomer));
        when(accountRepository.existsByAccountId(accountId)).thenReturn(true);
        when(cardRepository.findByAccountId(accountId)).thenReturn(new ArrayList<>());
        
        // Perform update
        AccountDto result = accountService.updateAccount(accountId, updates);
        
        // Verify update was persisted
        assertThat(result).isNotNull();
        assertThat(result.getCreditLimit()).isEqualTo(newCreditLimit);
        
        // Verify save was called
        verify(accountRepository, atLeastOnce()).save(any(Account.class));
    }

    /**
     * Property 13: Customer Update Persistence
     * For any valid customer information update, the system should persist the changes
     * to the customer record in the database
     * Validates: Requirements 4.4
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 13: Customer Update Persistence")
    void testCustomerUpdatePersistence(
            @ForAll("validAccountIds") Long accountId,
            @ForAll("validCustomerIds") Long customerId,
            @ForAll("validNames") String newFirstName) {
        
        // Setup mocks for each test iteration
        setupMocks();
        
        // Create test account
        Account testAccount = createTestAccount(accountId, customerId);
        
        // Create test customer
        Customer testCustomer = createTestCustomer(customerId);
        String originalFirstName = testCustomer.getFirstName();
        
        // Mock account found
        when(accountRepository.findByAccountId(accountId)).thenReturn(Optional.of(testAccount));
        
        // Mock customer found
        when(customerRepository.findByCustomerId(customerId)).thenReturn(Optional.of(testCustomer));
        
        // Create update DTO with customer changes
        AccountUpdateDto updates = new AccountUpdateDto();
        CustomerUpdateDto customerUpdates = new CustomerUpdateDto();
        customerUpdates.setFirstName(newFirstName);
        updates.setCustomer(customerUpdates);
        
        // Mock validation service
        when(validationService.validateName(anyString())).thenReturn(ValidationResult.success());
        
        // Mock save operations
        when(accountRepository.save(any(Account.class))).thenReturn(testAccount);
        Customer updatedCustomer = createTestCustomer(customerId);
        updatedCustomer.setFirstName(newFirstName);
        when(customerRepository.save(any(Customer.class))).thenReturn(updatedCustomer);
        
        // Mock for getAccount call
        when(accountRepository.existsByAccountId(accountId)).thenReturn(true);
        when(cardRepository.findByAccountId(accountId)).thenReturn(new ArrayList<>());
        
        // Perform update
        AccountDto result = accountService.updateAccount(accountId, updates);
        
        // Verify customer update was persisted
        assertThat(result).isNotNull();
        assertThat(result.getCustomer()).isNotNull();
        assertThat(result.getCustomer().getFirstName()).isEqualTo(newFirstName);
        
        // Verify customer save was called
        verify(customerRepository, atLeastOnce()).save(any(Customer.class));
    }

    // Helper methods

    private void setupMocks() {
        accountRepository = mock(AccountRepository.class);
        customerRepository = mock(CustomerRepository.class);
        cardRepository = mock(CardRepository.class);
        validationService = mock(ValidationService.class);
        
        accountService = new AccountServiceImpl();
        
        // Use reflection to inject mocks
        try {
            java.lang.reflect.Field accountRepoField = AccountServiceImpl.class.getDeclaredField("accountRepository");
            accountRepoField.setAccessible(true);
            accountRepoField.set(accountService, accountRepository);
            
            java.lang.reflect.Field customerRepoField = AccountServiceImpl.class.getDeclaredField("customerRepository");
            customerRepoField.setAccessible(true);
            customerRepoField.set(accountService, customerRepository);
            
            java.lang.reflect.Field cardRepoField = AccountServiceImpl.class.getDeclaredField("cardRepository");
            cardRepoField.setAccessible(true);
            cardRepoField.set(accountService, cardRepository);
            
            java.lang.reflect.Field validationField = AccountServiceImpl.class.getDeclaredField("validationService");
            validationField.setAccessible(true);
            validationField.set(accountService, validationService);
        } catch (Exception e) {
            throw new RuntimeException("Failed to inject mocks", e);
        }
    }

    private Account createTestAccount(Long accountId, Long customerId) {
        Account account = new Account();
        account.setAccountId(accountId);
        account.setCustomerId(customerId);
        account.setActiveStatus("Y");
        account.setCurrentBalance(new BigDecimal("1000.00"));
        account.setCreditLimit(new BigDecimal("5000.00"));
        account.setCashCreditLimit(new BigDecimal("1000.00"));
        account.setOpenDate("2024-01-01");
        account.setCurrentCycleCredit(new BigDecimal("0.00"));
        account.setCurrentCycleDebit(new BigDecimal("0.00"));
        return account;
    }

    private Customer createTestCustomer(Long customerId) {
        Customer customer = new Customer();
        customer.setCustomerId(customerId);
        customer.setFirstName("John");
        customer.setMiddleName("A");
        customer.setLastName("Doe");
        customer.setAddressLine1("123 Main St");
        customer.setStateCode("CA");
        customer.setCountryCode("USA");
        customer.setZipCode("12345");
        customer.setPhoneNumber1("(555)123-4567");
        customer.setSsn(123456789L);
        customer.setDateOfBirth("1980-01-01");
        customer.setPrimaryCardholderInd("Y");
        customer.setFicoCreditScore(750);
        return customer;
    }

    private List<Card> createTestCards(Long accountId, Long customerId, int count) {
        List<Card> cards = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            Card card = new Card();
            card.setCardNumber(4000000000000000L + i);
            card.setAccountId(accountId);
            card.setCustomerId(customerId);
            card.setCardStatus("ACTIVE");
            card.setExpirationDate("2025-12-31");
            card.setIssueDate("2024-01-01");
            cards.add(card);
        }
        return cards;
    }

    // Providers for generating test data

    @Provide
    Arbitrary<String> accountIdInputs() {
        return Arbitraries.oneOf(
            // Valid account IDs
            Arbitraries.longs().between(10000000000L, 99999999999L).map(String::valueOf),
            // Invalid: too short
            Arbitraries.longs().between(1L, 9999999999L).map(String::valueOf),
            // Invalid: too long
            Arbitraries.longs().between(100000000000L, 999999999999L).map(String::valueOf),
            // Invalid: all zeroes
            Arbitraries.just("00000000000"),
            // Invalid: non-numeric
            Arbitraries.strings().alpha().ofLength(11),
            // Invalid: empty
            Arbitraries.just(""),
            // Invalid: whitespace
            Arbitraries.just("   "),
            // Invalid: null
            Arbitraries.just(null)
        );
    }

    @Provide
    Arbitrary<Long> validAccountIds() {
        return Arbitraries.longs().between(10000000000L, 99999999999L);
    }

    @Provide
    Arbitrary<Long> validCustomerIds() {
        return Arbitraries.longs().between(100000000L, 999999999L);
    }

    @Provide
    Arbitrary<String> accountStatuses() {
        return Arbitraries.oneOf(
            Arbitraries.of("Y", "N"),  // Valid statuses
            Arbitraries.of("X", "A", "1", ""),  // Invalid statuses
            Arbitraries.just(null)  // Null status
        );
    }

    @Provide
    Arbitrary<BigDecimal> validCreditLimits() {
        return Arbitraries.bigDecimals()
            .between(BigDecimal.valueOf(100.00), BigDecimal.valueOf(50000.00))
            .ofScale(2);
    }

    @Provide
    Arbitrary<String> validNames() {
        return Arbitraries.strings()
            .alpha()
            .ofMinLength(1)
            .ofMaxLength(25);
    }
}
