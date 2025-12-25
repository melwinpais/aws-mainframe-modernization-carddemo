package com.carddemo.service;

import com.carddemo.dto.TransactionCreateDto;
import com.carddemo.dto.TransactionDto;
import com.carddemo.entity.Account;
import com.carddemo.entity.Card;
import com.carddemo.entity.Transaction;
import com.carddemo.exception.ResourceNotFoundException;
import com.carddemo.exception.ValidationException;
import com.carddemo.repository.AccountRepository;
import com.carddemo.repository.CardRepository;
import com.carddemo.repository.TransactionRepository;
import net.jqwik.api.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Property-based tests for TransactionService
 * Tests universal properties across all inputs
 * Validates Requirements 6.1, 6.2, 6.3, 6.4, 6.5, 6.6
 */
class TransactionServicePropertyTest {

    private TransactionRepository transactionRepository;
    private AccountRepository accountRepository;
    private CardRepository cardRepository;
    private ValidationService validationService;
    private TransactionServiceImpl transactionService;

    /**
     * Test transaction retrieval by ID
     * For any valid transaction ID, the system should retrieve the transaction
     * or return a not found error if the transaction doesn't exist
     * Validates: Requirements 6.1
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Transaction Retrieval by ID")
    void testTransactionRetrievalById(
            @ForAll("validTransactionIds") Long transactionId,
            @ForAll("validAccountIds") Long accountId) {
        
        // Setup mocks for each test iteration
        setupMocks();
        
        // Create test transaction
        Transaction testTransaction = createTestTransaction(transactionId, accountId);
        
        // Mock repository response
        when(transactionRepository.findById(transactionId)).thenReturn(Optional.of(testTransaction));
        
        // Retrieve transaction
        TransactionDto result = transactionService.getTransaction(transactionId);
        
        // Verify transaction data is returned
        assertThat(result).isNotNull();
        assertThat(result.getTransactionId()).isEqualTo(transactionId);
        assertThat(result.getAccountId()).isEqualTo(accountId);
        assertThat(result.getTransactionType()).isEqualTo(testTransaction.getTransactionType());
        assertThat(result.getAmount()).isEqualTo(testTransaction.getAmount());
        assertThat(result.getTransactionDate()).isEqualTo(testTransaction.getTransactionDate());
        
        // Verify repository was called
        verify(transactionRepository).findById(transactionId);
    }

    /**
     * Test transaction not found handling
     * For any non-existent transaction ID, the system should return a not found error
     * Validates: Requirements 6.1
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Transaction Not Found Handling")
    void testTransactionNotFoundHandling(
            @ForAll("validTransactionIds") Long transactionId) {
        
        // Setup mocks for each test iteration
        setupMocks();
        
        // Mock transaction not found
        when(transactionRepository.findById(transactionId)).thenReturn(Optional.empty());
        
        // Attempt to retrieve non-existent transaction
        assertThatThrownBy(() -> transactionService.getTransaction(transactionId))
            .isInstanceOf(ResourceNotFoundException.class)
            .hasMessageContaining("Transaction not found");
        
        // Verify repository was called
        verify(transactionRepository).findById(transactionId);
    }

    /**
     * Test transaction retrieval by account with date filters
     * For any valid account ID and date range, the system should retrieve
     * transactions within that date range with proper pagination
     * Validates: Requirements 6.2, 6.3
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Transaction Retrieval by Account with Date Filters")
    void testTransactionRetrievalByAccountWithDateFilters(
            @ForAll("validAccountIds") Long accountId,
            @ForAll("validDates") String startDate,
            @ForAll("validDates") String endDate) {
        
        // Setup mocks for each test iteration
        setupMocks();
        
        // Ensure startDate <= endDate
        if (startDate.compareTo(endDate) > 0) {
            String temp = startDate;
            startDate = endDate;
            endDate = temp;
        }
        
        // Mock account exists
        when(accountRepository.existsById(accountId)).thenReturn(true);
        
        // Mock date validation
        when(validationService.validateDate(anyString())).thenReturn(ValidationResult.success());
        
        // Create test transactions within date range
        List<Transaction> testTransactions = createTestTransactionsInDateRange(
            accountId, startDate, endDate, 5);
        
        Pageable pageable = PageRequest.of(0, 20);
        Page<Transaction> transactionPage = new PageImpl<>(testTransactions, pageable, testTransactions.size());
        
        // Mock repository response
        when(transactionRepository.findByAccountIdAndDateRange(
            eq(accountId), eq(startDate), eq(endDate), any(Pageable.class)))
            .thenReturn(transactionPage);
        
        // Retrieve transactions
        Page<TransactionDto> result = transactionService.getTransactionsByAccount(
            accountId, startDate, endDate, pageable);
        
        // Verify transactions are returned
        assertThat(result).isNotNull();
        assertThat(result.getContent()).hasSize(testTransactions.size());
        
        // Verify all transactions are within date range
        for (TransactionDto txn : result.getContent()) {
            assertThat(txn.getAccountId()).isEqualTo(accountId);
            assertThat(txn.getTransactionDate()).isGreaterThanOrEqualTo(startDate);
            assertThat(txn.getTransactionDate()).isLessThanOrEqualTo(endDate);
        }
        
        // Verify repository methods were called
        verify(accountRepository).existsById(accountId);
        verify(validationService, times(2)).validateDate(anyString());
        verify(transactionRepository).findByAccountIdAndDateRange(
            eq(accountId), eq(startDate), eq(endDate), any(Pageable.class));
    }

    /**
     * Test transaction retrieval by account without date filters
     * For any valid account ID without date filters, the system should retrieve
     * all transactions for that account with proper pagination
     * Validates: Requirements 6.2
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Transaction Retrieval by Account without Filters")
    void testTransactionRetrievalByAccountWithoutFilters(
            @ForAll("validAccountIds") Long accountId) {
        
        // Setup mocks for each test iteration
        setupMocks();
        
        // Mock account exists
        when(accountRepository.existsById(accountId)).thenReturn(true);
        
        // Create test transactions
        List<Transaction> testTransactions = createTestTransactions(accountId, 10);
        
        Pageable pageable = PageRequest.of(0, 20);
        Page<Transaction> transactionPage = new PageImpl<>(testTransactions, pageable, testTransactions.size());
        
        // Mock repository response
        when(transactionRepository.findByAccountId(eq(accountId), any(Pageable.class)))
            .thenReturn(transactionPage);
        
        // Retrieve transactions without date filters
        Page<TransactionDto> result = transactionService.getTransactionsByAccount(
            accountId, null, null, pageable);
        
        // Verify transactions are returned
        assertThat(result).isNotNull();
        assertThat(result.getContent()).hasSize(testTransactions.size());
        
        // Verify all transactions belong to the account
        for (TransactionDto txn : result.getContent()) {
            assertThat(txn.getAccountId()).isEqualTo(accountId);
        }
        
        // Verify repository methods were called
        verify(accountRepository).existsById(accountId);
        verify(transactionRepository).findByAccountId(eq(accountId), any(Pageable.class));
    }

    /**
     * Test transaction creation with balance update
     * For any valid transaction creation request, the system should create the transaction
     * and update the account balance accordingly
     * Validates: Requirements 6.4, 6.5, 6.6
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Transaction Creation with Balance Update")
    void testTransactionCreationWithBalanceUpdate(
            @ForAll("validAccountIds") Long accountId,
            @ForAll("validTransactionAmounts") BigDecimal amount,
            @ForAll("validDates") String transactionDate) {
        
        // Setup mocks for each test iteration
        setupMocks();
        
        // Create test account with initial balance
        Account testAccount = createTestAccount(accountId);
        BigDecimal initialBalance = testAccount.getCurrentBalance();
        
        // Mock account exists
        when(accountRepository.findById(accountId)).thenReturn(Optional.of(testAccount));
        
        // Mock validation service
        when(validationService.validateCurrencyAmount(anyString())).thenReturn(ValidationResult.success());
        when(validationService.validateDate(anyString())).thenReturn(ValidationResult.success());
        
        // Create transaction DTO
        TransactionCreateDto txnData = new TransactionCreateDto();
        txnData.setAccountId(accountId);
        txnData.setTransactionType("PURCHASE");
        txnData.setAmount(amount);
        txnData.setTransactionDate(transactionDate);
        txnData.setDescription("Test transaction");
        
        // Mock transaction save
        Transaction savedTransaction = new Transaction();
        savedTransaction.setTransactionId(1L);
        savedTransaction.setAccountId(accountId);
        savedTransaction.setTransactionType("PURCHASE");
        savedTransaction.setAmount(amount);
        savedTransaction.setTransactionDate(transactionDate);
        savedTransaction.setDescription("Test transaction");
        
        when(transactionRepository.save(any(Transaction.class))).thenReturn(savedTransaction);
        when(accountRepository.save(any(Account.class))).thenReturn(testAccount);
        
        // Create transaction
        TransactionDto result = transactionService.createTransaction(txnData);
        
        // Verify transaction was created
        assertThat(result).isNotNull();
        assertThat(result.getAccountId()).isEqualTo(accountId);
        assertThat(result.getAmount()).isEqualTo(amount);
        assertThat(result.getTransactionDate()).isEqualTo(transactionDate);
        
        // Verify account balance was updated
        BigDecimal expectedBalance = initialBalance.add(amount);
        verify(accountRepository).save(argThat(account -> 
            account.getAccountId().equals(accountId) &&
            account.getCurrentBalance().compareTo(expectedBalance) == 0
        ));
        
        // Verify repository methods were called
        verify(accountRepository).findById(accountId);
        verify(transactionRepository).save(any(Transaction.class));
        verify(validationService).validateCurrencyAmount(anyString());
        verify(validationService).validateDate(anyString());
    }

    /**
     * Test pagination for transaction lists
     * For any account with multiple transactions, the system should properly
     * paginate the results according to the requested page size
     * Validates: Requirements 6.2
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Transaction Pagination")
    void testTransactionPagination(
            @ForAll("validAccountIds") Long accountId,
            @ForAll("pageNumbers") int pageNumber,
            @ForAll("pageSizes") int pageSize) {
        
        // Setup mocks for each test iteration
        setupMocks();
        
        // Mock account exists
        when(accountRepository.existsById(accountId)).thenReturn(true);
        
        // Create a larger set of test transactions
        int totalTransactions = 50;
        List<Transaction> allTransactions = createTestTransactions(accountId, totalTransactions);
        
        // Calculate expected page content
        int startIndex = pageNumber * pageSize;
        int endIndex = Math.min(startIndex + pageSize, totalTransactions);
        
        List<Transaction> pageContent = startIndex < totalTransactions ?
            allTransactions.subList(startIndex, endIndex) : new ArrayList<>();
        
        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Page<Transaction> transactionPage = new PageImpl<>(pageContent, pageable, totalTransactions);
        
        // Mock repository response
        when(transactionRepository.findByAccountId(eq(accountId), any(Pageable.class)))
            .thenReturn(transactionPage);
        
        // Retrieve transactions with pagination
        Page<TransactionDto> result = transactionService.getTransactionsByAccount(
            accountId, null, null, pageable);
        
        // Verify pagination properties
        assertThat(result).isNotNull();
        assertThat(result.getNumber()).isEqualTo(pageNumber);
        assertThat(result.getSize()).isEqualTo(pageSize);
        assertThat(result.getTotalElements()).isEqualTo(totalTransactions);
        assertThat(result.getContent()).hasSize(pageContent.size());
        
        // Verify all transactions in page belong to the account
        for (TransactionDto txn : result.getContent()) {
            assertThat(txn.getAccountId()).isEqualTo(accountId);
        }
        
        // Verify repository was called
        verify(transactionRepository).findByAccountId(eq(accountId), any(Pageable.class));
    }

    /**
     * Test transaction creation validation
     * For any invalid transaction creation request, the system should
     * return appropriate validation errors
     * Validates: Requirements 6.4
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Transaction Creation Validation")
    void testTransactionCreationValidation(
            @ForAll("validAccountIds") Long accountId,
            @ForAll("transactionTypes") String transactionType,
            @ForAll("transactionAmounts") BigDecimal amount) {
        
        // Setup mocks for each test iteration
        setupMocks();
        
        // Create transaction DTO
        TransactionCreateDto txnData = new TransactionCreateDto();
        txnData.setAccountId(accountId);
        txnData.setTransactionType(transactionType);
        txnData.setAmount(amount);
        txnData.setTransactionDate("2024-12-24");
        
        // Determine if inputs are valid
        boolean isValidType = transactionType != null && !transactionType.trim().isEmpty();
        boolean isValidAmount = amount != null;
        
        if (!isValidType || !isValidAmount) {
            // Should fail validation
            assertThatThrownBy(() -> transactionService.createTransaction(txnData))
                .isInstanceOf(ValidationException.class);
        } else {
            // Mock account exists
            Account testAccount = createTestAccount(accountId);
            when(accountRepository.findById(accountId)).thenReturn(Optional.of(testAccount));
            
            // Mock validation service
            when(validationService.validateCurrencyAmount(anyString())).thenReturn(ValidationResult.success());
            when(validationService.validateDate(anyString())).thenReturn(ValidationResult.success());
            
            // Mock transaction save
            Transaction savedTransaction = new Transaction();
            savedTransaction.setTransactionId(1L);
            savedTransaction.setAccountId(accountId);
            savedTransaction.setTransactionType(transactionType);
            savedTransaction.setAmount(amount);
            savedTransaction.setTransactionDate("2024-12-24");
            
            when(transactionRepository.save(any(Transaction.class))).thenReturn(savedTransaction);
            when(accountRepository.save(any(Account.class))).thenReturn(testAccount);
            
            // Should succeed
            TransactionDto result = transactionService.createTransaction(txnData);
            assertThat(result).isNotNull();
        }
    }

    /**
     * Test transaction retrieval by card
     * For any valid card number, the system should retrieve all transactions
     * for that card with proper pagination
     * Validates: Requirements 6.3
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Transaction Retrieval by Card")
    void testTransactionRetrievalByCard(
            @ForAll("validCardNumbers") Long cardNumber,
            @ForAll("validAccountIds") Long accountId) {
        
        // Setup mocks for each test iteration
        setupMocks();
        
        // Mock card exists
        when(cardRepository.existsById(cardNumber)).thenReturn(true);
        
        // Create test transactions for the card
        List<Transaction> testTransactions = createTestTransactionsForCard(cardNumber, accountId, 5);
        
        Pageable pageable = PageRequest.of(0, 20);
        Page<Transaction> transactionPage = new PageImpl<>(testTransactions, pageable, testTransactions.size());
        
        // Mock repository response
        when(transactionRepository.findByCardNumber(eq(cardNumber), any(Pageable.class)))
            .thenReturn(transactionPage);
        
        // Retrieve transactions
        Page<TransactionDto> result = transactionService.getTransactionsByCard(
            cardNumber, null, null, pageable);
        
        // Verify transactions are returned
        assertThat(result).isNotNull();
        assertThat(result.getContent()).hasSize(testTransactions.size());
        
        // Verify all transactions belong to the card
        for (TransactionDto txn : result.getContent()) {
            assertThat(txn.getCardNumber()).isEqualTo(cardNumber);
        }
        
        // Verify repository methods were called
        verify(cardRepository).existsById(cardNumber);
        verify(transactionRepository).findByCardNumber(eq(cardNumber), any(Pageable.class));
    }

    // Helper methods

    private void setupMocks() {
        transactionRepository = mock(TransactionRepository.class);
        accountRepository = mock(AccountRepository.class);
        cardRepository = mock(CardRepository.class);
        validationService = mock(ValidationService.class);
        
        transactionService = new TransactionServiceImpl(
            transactionRepository,
            accountRepository,
            cardRepository,
            validationService
        );
    }

    private Transaction createTestTransaction(Long transactionId, Long accountId) {
        Transaction transaction = new Transaction();
        transaction.setTransactionId(transactionId);
        transaction.setAccountId(accountId);
        transaction.setTransactionType("PURCHASE");
        transaction.setAmount(new BigDecimal("100.00"));
        transaction.setTransactionDate("2024-12-24");
        transaction.setDescription("Test transaction");
        return transaction;
    }

    private List<Transaction> createTestTransactions(Long accountId, int count) {
        List<Transaction> transactions = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            Transaction transaction = new Transaction();
            transaction.setTransactionId((long) (i + 1));
            transaction.setAccountId(accountId);
            transaction.setTransactionType("PURCHASE");
            transaction.setAmount(new BigDecimal("100.00").add(new BigDecimal(i)));
            transaction.setTransactionDate("2024-12-" + String.format("%02d", (i % 28) + 1));
            transaction.setDescription("Test transaction " + i);
            transactions.add(transaction);
        }
        return transactions;
    }

    private List<Transaction> createTestTransactionsInDateRange(
            Long accountId, String startDate, String endDate, int count) {
        List<Transaction> transactions = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            Transaction transaction = new Transaction();
            transaction.setTransactionId((long) (i + 1));
            transaction.setAccountId(accountId);
            transaction.setTransactionType("PURCHASE");
            transaction.setAmount(new BigDecimal("100.00").add(new BigDecimal(i)));
            // Use a date within the range
            transaction.setTransactionDate(startDate);
            transaction.setDescription("Test transaction " + i);
            transactions.add(transaction);
        }
        return transactions;
    }

    private List<Transaction> createTestTransactionsForCard(
            Long cardNumber, Long accountId, int count) {
        List<Transaction> transactions = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            Transaction transaction = new Transaction();
            transaction.setTransactionId((long) (i + 1));
            transaction.setAccountId(accountId);
            transaction.setCardNumber(cardNumber);
            transaction.setTransactionType("PURCHASE");
            transaction.setAmount(new BigDecimal("100.00").add(new BigDecimal(i)));
            transaction.setTransactionDate("2024-12-24");
            transaction.setDescription("Test transaction " + i);
            transactions.add(transaction);
        }
        return transactions;
    }

    private Account createTestAccount(Long accountId) {
        Account account = new Account();
        account.setAccountId(accountId);
        account.setCustomerId(100000000L);
        account.setActiveStatus("Y");
        account.setCurrentBalance(new BigDecimal("1000.00"));
        account.setCreditLimit(new BigDecimal("5000.00"));
        account.setCashCreditLimit(new BigDecimal("1000.00"));
        account.setOpenDate("2024-01-01");
        account.setCurrentCycleCredit(new BigDecimal("0.00"));
        account.setCurrentCycleDebit(new BigDecimal("0.00"));
        return account;
    }

    // Providers for generating test data

    @Provide
    Arbitrary<Long> validTransactionIds() {
        return Arbitraries.longs().between(1L, 999999999L);
    }

    @Provide
    Arbitrary<Long> validAccountIds() {
        return Arbitraries.longs().between(10000000000L, 99999999999L);
    }

    @Provide
    Arbitrary<Long> validCardNumbers() {
        return Arbitraries.longs().between(4000000000000000L, 4999999999999999L);
    }

    @Provide
    Arbitrary<String> validDates() {
        return Arbitraries.integers().between(2020, 2025)
            .flatMap(year -> Arbitraries.integers().between(1, 12)
                .flatMap(month -> Arbitraries.integers().between(1, 28)
                    .map(day -> String.format("%04d-%02d-%02d", year, month, day))));
    }

    @Provide
    Arbitrary<BigDecimal> validTransactionAmounts() {
        return Arbitraries.bigDecimals()
            .between(BigDecimal.valueOf(-10000.00), BigDecimal.valueOf(10000.00))
            .ofScale(2);
    }

    @Provide
    Arbitrary<BigDecimal> transactionAmounts() {
        return Arbitraries.oneOf(
            // Valid amounts
            Arbitraries.bigDecimals()
                .between(BigDecimal.valueOf(-10000.00), BigDecimal.valueOf(10000.00))
                .ofScale(2),
            // Null amount
            Arbitraries.just(null)
        );
    }

    @Provide
    Arbitrary<String> transactionTypes() {
        return Arbitraries.oneOf(
            // Valid types
            Arbitraries.of("PURCHASE", "PAYMENT", "REFUND", "FEE", "INTEREST"),
            // Invalid: empty
            Arbitraries.just(""),
            // Invalid: whitespace
            Arbitraries.just("   "),
            // Invalid: null
            Arbitraries.just(null)
        );
    }

    @Provide
    Arbitrary<Integer> pageNumbers() {
        return Arbitraries.integers().between(0, 5);
    }

    @Provide
    Arbitrary<Integer> pageSizes() {
        return Arbitraries.of(10, 20, 50);
    }
}
