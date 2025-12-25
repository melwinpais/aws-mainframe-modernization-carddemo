package com.carddemo.service;

import com.carddemo.dto.BillInfoDto;
import com.carddemo.dto.BillPaymentDto;
import com.carddemo.dto.TransactionCreateDto;
import com.carddemo.dto.TransactionDto;
import com.carddemo.entity.Account;
import com.carddemo.exception.ResourceNotFoundException;
import com.carddemo.exception.ValidationException;
import com.carddemo.repository.AccountRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

/**
 * Implementation of BillPaymentService for bill payment operations.
 */
@Service
public class BillPaymentServiceImpl implements BillPaymentService {
    
    private static final Logger logger = LoggerFactory.getLogger(BillPaymentServiceImpl.class);
    private static final String PAYMENT_TRANSACTION_TYPE = "PAYMENT";
    private static final String PAYMENT_CATEGORY = "BILL_PAYMENT";
    private static final BigDecimal MINIMUM_PAYMENT_PERCENTAGE = new BigDecimal("0.02"); // 2% of balance
    
    private final AccountRepository accountRepository;
    private final TransactionService transactionService;
    private final ValidationService validationService;
    
    public BillPaymentServiceImpl(AccountRepository accountRepository,
                                 TransactionService transactionService,
                                 ValidationService validationService) {
        this.accountRepository = accountRepository;
        this.transactionService = transactionService;
        this.validationService = validationService;
    }
    
    @Override
    @Transactional
    public TransactionDto processBillPayment(BillPaymentDto billPayment) {
        logger.debug("Processing bill payment for account: {}, amount: {}", 
                    billPayment.getAccountId(), billPayment.getAmount());
        
        // Validate required fields
        if (billPayment.getAccountId() == null) {
            throw new ValidationException("Account ID is required");
        }
        
        if (billPayment.getAmount() == null) {
            throw new ValidationException("Payment amount is required");
        }
        
        if (billPayment.getPaymentDate() == null || billPayment.getPaymentDate().trim().isEmpty()) {
            throw new ValidationException("Payment date is required");
        }
        
        // Validate amount is positive
        if (billPayment.getAmount().compareTo(BigDecimal.ZERO) <= 0) {
            throw new ValidationException("Payment amount must be greater than zero");
        }
        
        // Validate amount format
        var amountValidation = validationService.validateCurrencyAmount(billPayment.getAmount().toString());
        if (!amountValidation.isValid()) {
            throw new ValidationException(amountValidation.getMessage());
        }
        
        // Validate date format
        var dateValidation = validationService.validateDate(billPayment.getPaymentDate());
        if (!dateValidation.isValid()) {
            throw new ValidationException(dateValidation.getMessage());
        }
        
        // Verify account exists
        Account account = accountRepository.findById(billPayment.getAccountId())
            .orElseThrow(() -> new ResourceNotFoundException("Account not found with ID: " + billPayment.getAccountId()));
        
        // Create payment transaction (negative amount to reduce balance)
        TransactionCreateDto transactionData = new TransactionCreateDto();
        transactionData.setAccountId(billPayment.getAccountId());
        transactionData.setTransactionType(PAYMENT_TRANSACTION_TYPE);
        transactionData.setTransactionCategory(PAYMENT_CATEGORY);
        transactionData.setAmount(billPayment.getAmount().negate()); // Negative to reduce balance
        transactionData.setTransactionDate(billPayment.getPaymentDate());
        transactionData.setDescription("Bill Payment");
        
        // Create transaction (this will also update the account balance)
        TransactionDto createdTransaction = transactionService.createTransaction(transactionData);
        
        logger.info("Successfully processed bill payment for account: {}, amount: {}, transaction ID: {}", 
                   billPayment.getAccountId(), billPayment.getAmount(), createdTransaction.getTransactionId());
        
        return createdTransaction;
    }
    
    @Override
    public BillInfoDto getBillInfo(Long accountId) {
        logger.debug("Retrieving bill info for account: {}", accountId);
        
        if (accountId == null) {
            throw new ValidationException("Account ID is required");
        }
        
        // Verify account exists
        Account account = accountRepository.findById(accountId)
            .orElseThrow(() -> new ResourceNotFoundException("Account not found with ID: " + accountId));
        
        // Calculate minimum payment (2% of current balance, minimum $25)
        BigDecimal minimumPayment = calculateMinimumPayment(account.getCurrentBalance());
        
        // Calculate due date (30 days from today)
        String dueDate = calculateDueDate();
        
        BillInfoDto billInfo = new BillInfoDto(
            accountId,
            account.getCurrentBalance(),
            minimumPayment,
            dueDate
        );
        
        logger.info("Retrieved bill info for account: {}, balance: {}, minimum payment: {}", 
                   accountId, account.getCurrentBalance(), minimumPayment);
        
        return billInfo;
    }
    
    /**
     * Calculate minimum payment based on current balance.
     * Minimum payment is 2% of balance or $25, whichever is greater.
     */
    private BigDecimal calculateMinimumPayment(BigDecimal currentBalance) {
        BigDecimal minimumPayment = currentBalance.multiply(MINIMUM_PAYMENT_PERCENTAGE)
            .setScale(2, RoundingMode.HALF_UP);
        
        BigDecimal minimumFloor = new BigDecimal("25.00");
        
        // Return the greater of 2% or $25
        return minimumPayment.compareTo(minimumFloor) > 0 ? minimumPayment : minimumFloor;
    }
    
    /**
     * Calculate due date (30 days from today).
     */
    private String calculateDueDate() {
        LocalDate today = LocalDate.now();
        LocalDate dueDate = today.plusDays(30);
        return dueDate.format(DateTimeFormatter.ISO_LOCAL_DATE);
    }
}
