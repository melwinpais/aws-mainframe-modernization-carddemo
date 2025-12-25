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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Unit tests for AccountService
 * Tests specific examples and edge cases for account management
 * Validates Requirements 3.8, 3.9, 4.6
 */
@ExtendWith(MockitoExtension.class)
class AccountServiceTest {

    @Mock
    private AccountRepository accountRepository;

    @Mock
    private CustomerRepository customerRepository;

    @Mock
    private CardRepository cardRepository;

    @Mock
    private ValidationService validationService;

    @InjectMocks
    private AccountServiceImpl accountService;

    private Account testAccount;
    private Customer testCustomer;
    private Card testCard1;
    private Card testCard2;

    private static final Long TEST_ACCOUNT_ID = 12345678901L;
    private static final Long TEST_CUSTOMER_ID = 123456789L;
    private static final Long TEST_CARD_NUMBER_1 = 1234567890123456L;
    private static final Long TEST_CARD_NUMBER_2 = 9876543210987654L;

    @BeforeEach
    void setUp() {
        // Set up test account
        testAccount = new Account();
        testAccount.setAccountId(TEST_ACCOUNT_ID);
        testAccount.setCustomerId(TEST_CUSTOMER_ID);
        testAccount.setActiveStatus("Y");
        testAccount.setCurrentBalance(new BigDecimal("1500.00"));
        testAccount.setCreditLimit(new BigDecimal("5000.00"));
        testAccount.setCashCreditLimit(new BigDecimal("1000.00"));
        testAccount.setOpenDate("2020-01-15");
        testAccount.setExpirationDate("2025-01-15");
        testAccount.setReissueDate("2024-01-15");
        testAccount.setCurrentCycleCredit(new BigDecimal("500.00"));
        testAccount.setCurrentCycleDebit(new BigDecimal("200.00"));
        testAccount.setAddressZip("12345");
        testAccount.setGroupId("GROUP001");

        // Set up test customer
        testCustomer = new Customer();
        testCustomer.setCustomerId(TEST_CUSTOMER_ID);
        testCustomer.setFirstName("John");
        testCustomer.setMiddleName("A");
        testCustomer.setLastName("Doe");
        testCustomer.setAddressLine1("123 Main St");
        testCustomer.setAddressLine2("Apt 4B");
        testCustomer.setAddressLine3("");
        testCustomer.setStateCode("NY");
        testCustomer.setCountryCode("USA");
        testCustomer.setZipCode("12345");
        testCustomer.setPhoneNumber1("(555)123-4567");
        testCustomer.setPhoneNumber2("(555)987-6543");
        testCustomer.setSsn(123456789L);
        testCustomer.setGovernmentIssuedId("DL123456");
        testCustomer.setDateOfBirth("1980-05-15");
        testCustomer.setEftAccountId("EFT001");
        testCustomer.setPrimaryCardholderInd("Y");
        testCustomer.setFicoCreditScore(750);

        // Set up test cards
        testCard1 = new Card();
        testCard1.setCardNumber(TEST_CARD_NUMBER_1);
        testCard1.setAccountId(TEST_ACCOUNT_ID);
        testCard1.setCustomerId(TEST_CUSTOMER_ID);
        testCard1.setCardStatus("A");
        testCard1.setExpirationDate("2025-12-31");
        testCard1.setIssueDate("2020-01-15");

        testCard2 = new Card();
        testCard2.setCardNumber(TEST_CARD_NUMBER_2);
        testCard2.setAccountId(TEST_ACCOUNT_ID);
        testCard2.setCustomerId(TEST_CUSTOMER_ID);
        testCard2.setCardStatus("A");
        testCard2.setExpirationDate("2026-06-30");
        testCard2.setIssueDate("2021-06-15");
    }

    /**
     * Test account retrieval with valid ID
     * Requirements: 3.8
     */
    @Test
    void testAccountRetrievalWithValidId() {
        // Arrange
        when(accountRepository.findByAccountId(TEST_ACCOUNT_ID))
                .thenReturn(Optional.of(testAccount));
        when(customerRepository.findByCustomerId(TEST_CUSTOMER_ID))
                .thenReturn(Optional.of(testCustomer));
        when(accountRepository.existsByAccountId(TEST_ACCOUNT_ID))
                .thenReturn(true);
        when(cardRepository.findByAccountId(TEST_ACCOUNT_ID))
                .thenReturn(Arrays.asList(testCard1, testCard2));

        // Act
        AccountDto result = accountService.getAccount(TEST_ACCOUNT_ID);

        // Assert
        assertThat(result).isNotNull();
        assertThat(result.getAccountId()).isEqualTo(TEST_ACCOUNT_ID);
        assertThat(result.getActiveStatus()).isEqualTo("Y");
        assertThat(result.getCurrentBalance()).isEqualByComparingTo(new BigDecimal("1500.00"));
        assertThat(result.getCreditLimit()).isEqualByComparingTo(new BigDecimal("5000.00"));
        assertThat(result.getCashCreditLimit()).isEqualByComparingTo(new BigDecimal("1000.00"));

        // Verify customer is included
        assertThat(result.getCustomer()).isNotNull();
        assertThat(result.getCustomer().getCustomerId()).isEqualTo(TEST_CUSTOMER_ID);
        assertThat(result.getCustomer().getFirstName()).isEqualTo("John");
        assertThat(result.getCustomer().getLastName()).isEqualTo("Doe");

        // Verify cards are included
        assertThat(result.getCards()).isNotNull();
        assertThat(result.getCards()).hasSize(2);
        assertThat(result.getCards().get(0).getCardNumber()).isEqualTo(TEST_CARD_NUMBER_1);
        assertThat(result.getCards().get(1).getCardNumber()).isEqualTo(TEST_CARD_NUMBER_2);

        // Verify interactions
        verify(accountRepository, times(2)).findByAccountId(TEST_ACCOUNT_ID); // Called in getAccount and getCustomerForAccount
        verify(customerRepository).findByCustomerId(TEST_CUSTOMER_ID);
        verify(accountRepository).existsByAccountId(TEST_ACCOUNT_ID);
        verify(cardRepository).findByAccountId(TEST_ACCOUNT_ID);
    }

    /**
     * Test account not found error
     * Requirements: 3.8
     */
    @Test
    void testAccountNotFoundError() {
        // Arrange
        Long nonExistentAccountId = 99999999999L;
        when(accountRepository.findByAccountId(nonExistentAccountId))
                .thenReturn(Optional.empty());

        // Act & Assert
        assertThatThrownBy(() -> accountService.getAccount(nonExistentAccountId))
                .isInstanceOf(ResourceNotFoundException.class)
                .hasMessageContaining("Did not find this account in account master file");

        // Verify interactions
        verify(accountRepository).findByAccountId(nonExistentAccountId);
        verify(customerRepository, never()).findByCustomerId(anyLong());
        verify(cardRepository, never()).findByAccountId(anyLong());
    }

    /**
     * Test customer not found error
     * Requirements: 3.9
     */
    @Test
    void testCustomerNotFoundError() {
        // Arrange
        when(accountRepository.findByAccountId(TEST_ACCOUNT_ID))
                .thenReturn(Optional.of(testAccount));
        when(customerRepository.findByCustomerId(TEST_CUSTOMER_ID))
                .thenReturn(Optional.empty());

        // Act & Assert
        assertThatThrownBy(() -> accountService.getAccount(TEST_ACCOUNT_ID))
                .isInstanceOf(ResourceNotFoundException.class)
                .hasMessageContaining("Did not find associated customer in master file");

        // Verify interactions
        verify(accountRepository, times(2)).findByAccountId(TEST_ACCOUNT_ID); // Called in getAccount and getCustomerForAccount
        verify(customerRepository).findByCustomerId(TEST_CUSTOMER_ID);
        verify(cardRepository, never()).findByAccountId(anyLong());
    }

    /**
     * Test account update with valid data
     * Requirements: 4.6
     */
    @Test
    void testAccountUpdateWithValidData() {
        // Arrange
        AccountUpdateDto updates = new AccountUpdateDto();
        updates.setActiveStatus("N");
        updates.setCreditLimit(new BigDecimal("7500.00"));
        updates.setCashCreditLimit(new BigDecimal("1500.00"));
        updates.setCurrentBalance(new BigDecimal("2000.00"));

        when(accountRepository.findByAccountId(TEST_ACCOUNT_ID))
                .thenReturn(Optional.of(testAccount));
        when(validationService.validateAccountStatus("N"))
                .thenReturn(ValidationResult.success());
        when(validationService.validateCurrencyAmount(anyString()))
                .thenReturn(ValidationResult.success());
        when(accountRepository.save(any(Account.class)))
                .thenReturn(testAccount);
        when(customerRepository.findByCustomerId(TEST_CUSTOMER_ID))
                .thenReturn(Optional.of(testCustomer));
        when(accountRepository.existsByAccountId(TEST_ACCOUNT_ID))
                .thenReturn(true);
        when(cardRepository.findByAccountId(TEST_ACCOUNT_ID))
                .thenReturn(Arrays.asList(testCard1, testCard2));

        // Act
        AccountDto result = accountService.updateAccount(TEST_ACCOUNT_ID, updates);

        // Assert
        assertThat(result).isNotNull();
        assertThat(result.getAccountId()).isEqualTo(TEST_ACCOUNT_ID);

        // Verify the account was saved with updated values
        verify(accountRepository).save(argThat(account ->
                account.getAccountId().equals(TEST_ACCOUNT_ID) &&
                account.getActiveStatus().equals("N") &&
                account.getCreditLimit().compareTo(new BigDecimal("7500.00")) == 0 &&
                account.getCashCreditLimit().compareTo(new BigDecimal("1500.00")) == 0 &&
                account.getCurrentBalance().compareTo(new BigDecimal("2000.00")) == 0
        ));

        // Verify validations were called
        verify(validationService).validateAccountStatus("N");
        verify(validationService, times(3)).validateCurrencyAmount(anyString());
    }

    /**
     * Test account update with invalid data
     * Requirements: 4.6
     */
    @Test
    void testAccountUpdateWithInvalidData() {
        // Arrange
        AccountUpdateDto updates = new AccountUpdateDto();
        updates.setActiveStatus("X"); // Invalid status

        when(accountRepository.findByAccountId(TEST_ACCOUNT_ID))
                .thenReturn(Optional.of(testAccount));
        when(validationService.validateAccountStatus("X"))
                .thenReturn(ValidationResult.failure("Invalid account status"));

        // Act & Assert
        assertThatThrownBy(() -> accountService.updateAccount(TEST_ACCOUNT_ID, updates))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Invalid account status");

        // Verify the account was not saved
        verify(accountRepository, never()).save(any(Account.class));
    }

    /**
     * Test account update with invalid credit limit
     * Requirements: 4.6
     */
    @Test
    void testAccountUpdateWithInvalidCreditLimit() {
        // Arrange
        AccountUpdateDto updates = new AccountUpdateDto();
        updates.setCreditLimit(new BigDecimal("-1000.00")); // Negative credit limit

        when(accountRepository.findByAccountId(TEST_ACCOUNT_ID))
                .thenReturn(Optional.of(testAccount));
        when(validationService.validateCurrencyAmount(anyString()))
                .thenReturn(ValidationResult.failure("Amount cannot be negative"));

        // Act & Assert
        assertThatThrownBy(() -> accountService.updateAccount(TEST_ACCOUNT_ID, updates))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Credit Limit")
                .hasMessageContaining("Amount cannot be negative");

        // Verify the account was not saved
        verify(accountRepository, never()).save(any(Account.class));
    }

    /**
     * Test account update with customer information
     * Requirements: 4.6
     */
    @Test
    void testAccountUpdateWithCustomerInformation() {
        // Arrange
        CustomerUpdateDto customerUpdates = new CustomerUpdateDto();
        customerUpdates.setFirstName("Jane");
        customerUpdates.setLastName("Smith");
        customerUpdates.setPhoneNumber1("(555)999-8888");

        AccountUpdateDto updates = new AccountUpdateDto();
        updates.setCustomer(customerUpdates);

        when(accountRepository.findByAccountId(TEST_ACCOUNT_ID))
                .thenReturn(Optional.of(testAccount));
        when(accountRepository.save(any(Account.class)))
                .thenReturn(testAccount);
        when(customerRepository.findByCustomerId(TEST_CUSTOMER_ID))
                .thenReturn(Optional.of(testCustomer));
        when(validationService.validateName(anyString()))
                .thenReturn(ValidationResult.success());
        when(validationService.validatePhoneNumber(anyString()))
                .thenReturn(ValidationResult.success());
        when(customerRepository.save(any(Customer.class)))
                .thenReturn(testCustomer);
        when(accountRepository.existsByAccountId(TEST_ACCOUNT_ID))
                .thenReturn(true);
        when(cardRepository.findByAccountId(TEST_ACCOUNT_ID))
                .thenReturn(Arrays.asList(testCard1, testCard2));

        // Act
        AccountDto result = accountService.updateAccount(TEST_ACCOUNT_ID, updates);

        // Assert
        assertThat(result).isNotNull();

        // Verify customer was updated
        verify(customerRepository).save(argThat(customer ->
                customer.getCustomerId().equals(TEST_CUSTOMER_ID) &&
                customer.getFirstName().equals("Jane") &&
                customer.getLastName().equals("Smith") &&
                customer.getPhoneNumber1().equals("(555)999-8888")
        ));

        // Verify validations were called
        verify(validationService, times(2)).validateName(anyString());
        verify(validationService).validatePhoneNumber("(555)999-8888");
    }

    /**
     * Test account update with invalid customer name
     * Requirements: 4.6
     */
    @Test
    void testAccountUpdateWithInvalidCustomerName() {
        // Arrange
        CustomerUpdateDto customerUpdates = new CustomerUpdateDto();
        customerUpdates.setFirstName("John123"); // Invalid name with numbers

        AccountUpdateDto updates = new AccountUpdateDto();
        updates.setCustomer(customerUpdates);

        when(accountRepository.findByAccountId(TEST_ACCOUNT_ID))
                .thenReturn(Optional.of(testAccount));
        when(accountRepository.save(any(Account.class)))
                .thenReturn(testAccount);
        when(customerRepository.findByCustomerId(TEST_CUSTOMER_ID))
                .thenReturn(Optional.of(testCustomer));
        when(validationService.validateName("John123"))
                .thenReturn(ValidationResult.failure("Name must contain only alphabetic characters and spaces"));

        // Act & Assert
        assertThatThrownBy(() -> accountService.updateAccount(TEST_ACCOUNT_ID, updates))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("First Name")
                .hasMessageContaining("Name must contain only alphabetic characters and spaces");

        // Verify customer was not saved
        verify(customerRepository, never()).save(any(Customer.class));
    }

    /**
     * Test getCustomerForAccount with valid account
     * Requirements: 3.8
     */
    @Test
    void testGetCustomerForAccountWithValidAccount() {
        // Arrange
        when(accountRepository.findByAccountId(TEST_ACCOUNT_ID))
                .thenReturn(Optional.of(testAccount));
        when(customerRepository.findByCustomerId(TEST_CUSTOMER_ID))
                .thenReturn(Optional.of(testCustomer));

        // Act
        CustomerDto result = accountService.getCustomerForAccount(TEST_ACCOUNT_ID);

        // Assert
        assertThat(result).isNotNull();
        assertThat(result.getCustomerId()).isEqualTo(TEST_CUSTOMER_ID);
        assertThat(result.getFirstName()).isEqualTo("John");
        assertThat(result.getLastName()).isEqualTo("Doe");

        // Verify interactions
        verify(accountRepository).findByAccountId(TEST_ACCOUNT_ID);
        verify(customerRepository).findByCustomerId(TEST_CUSTOMER_ID);
    }

    /**
     * Test getCardsForAccount with valid account
     * Requirements: 3.8
     */
    @Test
    void testGetCardsForAccountWithValidAccount() {
        // Arrange
        when(accountRepository.existsByAccountId(TEST_ACCOUNT_ID))
                .thenReturn(true);
        when(cardRepository.findByAccountId(TEST_ACCOUNT_ID))
                .thenReturn(Arrays.asList(testCard1, testCard2));

        // Act
        List<CardDto> result = accountService.getCardsForAccount(TEST_ACCOUNT_ID);

        // Assert
        assertThat(result).isNotNull();
        assertThat(result).hasSize(2);
        assertThat(result.get(0).getCardNumber()).isEqualTo(TEST_CARD_NUMBER_1);
        assertThat(result.get(1).getCardNumber()).isEqualTo(TEST_CARD_NUMBER_2);

        // Verify interactions
        verify(accountRepository).existsByAccountId(TEST_ACCOUNT_ID);
        verify(cardRepository).findByAccountId(TEST_ACCOUNT_ID);
    }
}
