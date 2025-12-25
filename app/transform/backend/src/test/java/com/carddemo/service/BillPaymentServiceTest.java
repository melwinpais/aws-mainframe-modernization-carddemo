package com.carddemo.service;

import com.carddemo.dto.BillInfoDto;
import com.carddemo.dto.BillPaymentDto;
import com.carddemo.dto.TransactionCreateDto;
import com.carddemo.dto.TransactionDto;
import com.carddemo.entity.Account;
import com.carddemo.exception.ResourceNotFoundException;
import com.carddemo.exception.ValidationException;
import com.carddemo.repository.AccountRepository;
import com.carddemo.service.ValidationResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.lenient;

/**
 * Unit tests for BillPaymentService.
 * Tests bill payment processing, validation, and balance updates.
 * 
 * Requirements: 7.1, 7.2, 7.3, 7.4, 7.5
 */
@ExtendWith(MockitoExtension.class)
class BillPaymentServiceTest {

    @Mock
    private AccountRepository accountRepository;

    @Mock
    private TransactionService transactionService;

    @Mock
    private ValidationService validationService;

    @InjectMocks
    private BillPaymentServiceImpl billPaymentService;

    private Account testAccount;
    private BillPaymentDto validPayment;
    private TransactionDto mockTransaction;

    @BeforeEach
    void setUp() {
        // Set up test account
        testAccount = new Account();
        testAccount.setAccountId(12345678901L);
        testAccount.setCurrentBalance(new BigDecimal("1500.00"));
        testAccount.setActiveStatus("Y");

        // Set up valid payment
        validPayment = new BillPaymentDto();
        validPayment.setAccountId(12345678901L);
        validPayment.setAmount(new BigDecimal("100.00"));
        validPayment.setPaymentDate("2024-01-15");

        // Set up mock transaction response
        mockTransaction = new TransactionDto();
        mockTransaction.setTransactionId(999L);
        mockTransaction.setAccountId(12345678901L);
        mockTransaction.setAmount(new BigDecimal("-100.00"));
        mockTransaction.setTransactionType("PAYMENT");
        mockTransaction.setTransactionCategory("BILL_PAYMENT");
        mockTransaction.setDescription("Bill Payment");
        mockTransaction.setTransactionDate("2024-01-15");

        // Set up default validation responses (lenient to avoid unnecessary stubbing errors)
        ValidationResult validAmount = ValidationResult.success();
        ValidationResult validDate = ValidationResult.success();
        lenient().when(validationService.validateCurrencyAmount(anyString())).thenReturn(validAmount);
        lenient().when(validationService.validateDate(anyString())).thenReturn(validDate);
    }

    /**
     * Test successful bill payment processing.
     * Validates: Requirements 7.1, 7.3, 7.4
     */
    @Test
    void testProcessBillPayment_Success() {
        // Arrange
        when(accountRepository.findById(12345678901L)).thenReturn(Optional.of(testAccount));
        when(transactionService.createTransaction(any(TransactionCreateDto.class))).thenReturn(mockTransaction);

        // Act
        TransactionDto result = billPaymentService.processBillPayment(validPayment);

        // Assert
        assertNotNull(result);
        assertEquals(999L, result.getTransactionId());
        assertEquals("PAYMENT", result.getTransactionType());
        assertEquals("BILL_PAYMENT", result.getTransactionCategory());
        assertEquals(new BigDecimal("-100.00"), result.getAmount());

        // Verify account was looked up
        verify(accountRepository).findById(12345678901L);

        // Verify transaction was created with correct data
        ArgumentCaptor<TransactionCreateDto> captor = ArgumentCaptor.forClass(TransactionCreateDto.class);
        verify(transactionService).createTransaction(captor.capture());
        
        TransactionCreateDto capturedTransaction = captor.getValue();
        assertEquals(12345678901L, capturedTransaction.getAccountId());
        assertEquals("PAYMENT", capturedTransaction.getTransactionType());
        assertEquals("BILL_PAYMENT", capturedTransaction.getTransactionCategory());
        assertEquals(new BigDecimal("-100.00"), capturedTransaction.getAmount()); // Negative to reduce balance
        assertEquals("2024-01-15", capturedTransaction.getTransactionDate());
        assertEquals("Bill Payment", capturedTransaction.getDescription());
    }

    /**
     * Test bill payment validation - missing account ID.
     * Validates: Requirement 7.2
     */
    @Test
    void testProcessBillPayment_MissingAccountId() {
        // Arrange
        validPayment.setAccountId(null);

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class, () -> {
            billPaymentService.processBillPayment(validPayment);
        });

        assertEquals("Account ID is required", exception.getMessage());
        verify(accountRepository, never()).findById(any());
        verify(transactionService, never()).createTransaction(any());
    }

    /**
     * Test bill payment validation - missing amount.
     * Validates: Requirement 7.2
     */
    @Test
    void testProcessBillPayment_MissingAmount() {
        // Arrange
        validPayment.setAmount(null);

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class, () -> {
            billPaymentService.processBillPayment(validPayment);
        });

        assertEquals("Payment amount is required", exception.getMessage());
        verify(accountRepository, never()).findById(any());
        verify(transactionService, never()).createTransaction(any());
    }

    /**
     * Test bill payment validation - missing payment date.
     * Validates: Requirement 7.2
     */
    @Test
    void testProcessBillPayment_MissingPaymentDate() {
        // Arrange
        validPayment.setPaymentDate(null);

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class, () -> {
            billPaymentService.processBillPayment(validPayment);
        });

        assertEquals("Payment date is required", exception.getMessage());
        verify(accountRepository, never()).findById(any());
        verify(transactionService, never()).createTransaction(any());
    }

    /**
     * Test bill payment validation - empty payment date.
     * Validates: Requirement 7.2
     */
    @Test
    void testProcessBillPayment_EmptyPaymentDate() {
        // Arrange
        validPayment.setPaymentDate("   ");

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class, () -> {
            billPaymentService.processBillPayment(validPayment);
        });

        assertEquals("Payment date is required", exception.getMessage());
        verify(accountRepository, never()).findById(any());
        verify(transactionService, never()).createTransaction(any());
    }

    /**
     * Test bill payment validation - negative amount.
     * Validates: Requirement 7.2
     */
    @Test
    void testProcessBillPayment_NegativeAmount() {
        // Arrange
        validPayment.setAmount(new BigDecimal("-50.00"));

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class, () -> {
            billPaymentService.processBillPayment(validPayment);
        });

        assertEquals("Payment amount must be greater than zero", exception.getMessage());
        verify(accountRepository, never()).findById(any());
        verify(transactionService, never()).createTransaction(any());
    }

    /**
     * Test bill payment validation - zero amount.
     * Validates: Requirement 7.2
     */
    @Test
    void testProcessBillPayment_ZeroAmount() {
        // Arrange
        validPayment.setAmount(BigDecimal.ZERO);

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class, () -> {
            billPaymentService.processBillPayment(validPayment);
        });

        assertEquals("Payment amount must be greater than zero", exception.getMessage());
        verify(accountRepository, never()).findById(any());
        verify(transactionService, never()).createTransaction(any());
    }

    /**
     * Test bill payment validation - invalid amount format.
     * Validates: Requirement 7.2
     */
    @Test
    void testProcessBillPayment_InvalidAmountFormat() {
        // Arrange
        validPayment.setAmount(new BigDecimal("100.123")); // Too many decimal places
        ValidationResult invalidAmount = ValidationResult.failure("Invalid currency format");
        when(validationService.validateCurrencyAmount(anyString())).thenReturn(invalidAmount);

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class, () -> {
            billPaymentService.processBillPayment(validPayment);
        });

        assertEquals("Invalid currency format", exception.getMessage());
        verify(accountRepository, never()).findById(any());
        verify(transactionService, never()).createTransaction(any());
    }

    /**
     * Test bill payment validation - invalid date format.
     * Validates: Requirement 7.2
     */
    @Test
    void testProcessBillPayment_InvalidDateFormat() {
        // Arrange
        validPayment.setPaymentDate("01/15/2024"); // Wrong format
        ValidationResult invalidDate = ValidationResult.failure("Invalid date format");
        when(validationService.validateDate(anyString())).thenReturn(invalidDate);

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class, () -> {
            billPaymentService.processBillPayment(validPayment);
        });

        assertEquals("Invalid date format", exception.getMessage());
        verify(accountRepository, never()).findById(any());
        verify(transactionService, never()).createTransaction(any());
    }

    /**
     * Test bill payment with non-existent account.
     * Validates: Requirement 7.1
     */
    @Test
    void testProcessBillPayment_AccountNotFound() {
        // Arrange
        when(accountRepository.findById(12345678901L)).thenReturn(Optional.empty());

        // Act & Assert
        ResourceNotFoundException exception = assertThrows(ResourceNotFoundException.class, () -> {
            billPaymentService.processBillPayment(validPayment);
        });

        assertEquals("Account not found with ID: 12345678901", exception.getMessage());
        verify(transactionService, never()).createTransaction(any());
    }

    /**
     * Test balance update after payment.
     * Verifies that the transaction service is called with negative amount to reduce balance.
     * Validates: Requirement 7.4
     */
    @Test
    void testProcessBillPayment_BalanceUpdate() {
        // Arrange
        when(accountRepository.findById(12345678901L)).thenReturn(Optional.of(testAccount));
        when(transactionService.createTransaction(any(TransactionCreateDto.class))).thenReturn(mockTransaction);

        // Act
        billPaymentService.processBillPayment(validPayment);

        // Assert - verify transaction was created with negative amount
        ArgumentCaptor<TransactionCreateDto> captor = ArgumentCaptor.forClass(TransactionCreateDto.class);
        verify(transactionService).createTransaction(captor.capture());
        
        TransactionCreateDto capturedTransaction = captor.getValue();
        assertEquals(new BigDecimal("-100.00"), capturedTransaction.getAmount());
        assertTrue(capturedTransaction.getAmount().compareTo(BigDecimal.ZERO) < 0, 
                  "Payment amount should be negative to reduce balance");
    }

    /**
     * Test getBillInfo with valid account.
     * Validates: Requirement 7.5
     */
    @Test
    void testGetBillInfo_Success() {
        // Arrange
        when(accountRepository.findById(12345678901L)).thenReturn(Optional.of(testAccount));

        // Act
        BillInfoDto result = billPaymentService.getBillInfo(12345678901L);

        // Assert
        assertNotNull(result);
        assertEquals(12345678901L, result.getAccountId());
        assertEquals(new BigDecimal("1500.00"), result.getCurrentBalance());
        
        // Minimum payment should be 2% of balance or $25, whichever is greater
        // 2% of $1500 = $30, so minimum should be $30
        assertEquals(new BigDecimal("30.00"), result.getMinimumPayment());
        
        // Due date should be 30 days from today
        LocalDate expectedDueDate = LocalDate.now().plusDays(30);
        assertEquals(expectedDueDate.format(DateTimeFormatter.ISO_LOCAL_DATE), result.getDueDate());

        verify(accountRepository).findById(12345678901L);
    }

    /**
     * Test getBillInfo with low balance (minimum payment floor).
     * Validates: Requirement 7.5
     */
    @Test
    void testGetBillInfo_MinimumPaymentFloor() {
        // Arrange
        testAccount.setCurrentBalance(new BigDecimal("500.00")); // 2% = $10, should use $25 floor
        when(accountRepository.findById(12345678901L)).thenReturn(Optional.of(testAccount));

        // Act
        BillInfoDto result = billPaymentService.getBillInfo(12345678901L);

        // Assert
        assertNotNull(result);
        assertEquals(new BigDecimal("500.00"), result.getCurrentBalance());
        
        // 2% of $500 = $10, but minimum floor is $25
        assertEquals(new BigDecimal("25.00"), result.getMinimumPayment());
    }

    /**
     * Test getBillInfo with high balance.
     * Validates: Requirement 7.5
     */
    @Test
    void testGetBillInfo_HighBalance() {
        // Arrange
        testAccount.setCurrentBalance(new BigDecimal("5000.00")); // 2% = $100
        when(accountRepository.findById(12345678901L)).thenReturn(Optional.of(testAccount));

        // Act
        BillInfoDto result = billPaymentService.getBillInfo(12345678901L);

        // Assert
        assertNotNull(result);
        assertEquals(new BigDecimal("5000.00"), result.getCurrentBalance());
        
        // 2% of $5000 = $100
        assertEquals(new BigDecimal("100.00"), result.getMinimumPayment());
    }

    /**
     * Test getBillInfo with missing account ID.
     * Validates: Requirement 7.5
     */
    @Test
    void testGetBillInfo_MissingAccountId() {
        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class, () -> {
            billPaymentService.getBillInfo(null);
        });

        assertEquals("Account ID is required", exception.getMessage());
        verify(accountRepository, never()).findById(any());
    }

    /**
     * Test getBillInfo with non-existent account.
     * Validates: Requirement 7.5
     */
    @Test
    void testGetBillInfo_AccountNotFound() {
        // Arrange
        when(accountRepository.findById(12345678901L)).thenReturn(Optional.empty());

        // Act & Assert
        ResourceNotFoundException exception = assertThrows(ResourceNotFoundException.class, () -> {
            billPaymentService.getBillInfo(12345678901L);
        });

        assertEquals("Account not found with ID: 12345678901", exception.getMessage());
    }

    /**
     * Test bill payment with large amount.
     * Validates: Requirements 7.1, 7.3, 7.4
     */
    @Test
    void testProcessBillPayment_LargeAmount() {
        // Arrange
        validPayment.setAmount(new BigDecimal("10000.00"));
        mockTransaction.setAmount(new BigDecimal("-10000.00"));
        when(accountRepository.findById(12345678901L)).thenReturn(Optional.of(testAccount));
        when(transactionService.createTransaction(any(TransactionCreateDto.class))).thenReturn(mockTransaction);

        // Act
        TransactionDto result = billPaymentService.processBillPayment(validPayment);

        // Assert
        assertNotNull(result);
        assertEquals(new BigDecimal("-10000.00"), result.getAmount());

        // Verify transaction was created with correct negative amount
        ArgumentCaptor<TransactionCreateDto> captor = ArgumentCaptor.forClass(TransactionCreateDto.class);
        verify(transactionService).createTransaction(captor.capture());
        assertEquals(new BigDecimal("-10000.00"), captor.getValue().getAmount());
    }

    /**
     * Test bill payment with exact two decimal places.
     * Validates: Requirements 7.1, 7.2
     */
    @Test
    void testProcessBillPayment_ExactTwoDecimals() {
        // Arrange
        validPayment.setAmount(new BigDecimal("99.99"));
        mockTransaction.setAmount(new BigDecimal("-99.99"));
        when(accountRepository.findById(12345678901L)).thenReturn(Optional.of(testAccount));
        when(transactionService.createTransaction(any(TransactionCreateDto.class))).thenReturn(mockTransaction);

        // Act
        TransactionDto result = billPaymentService.processBillPayment(validPayment);

        // Assert
        assertNotNull(result);
        assertEquals(new BigDecimal("-99.99"), result.getAmount());
    }
}
