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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;

/**
 * Implementation of TransactionService for transaction management operations.
 */
@Service
public class TransactionServiceImpl implements TransactionService {
    
    private static final Logger logger = LoggerFactory.getLogger(TransactionServiceImpl.class);
    
    private final TransactionRepository transactionRepository;
    private final AccountRepository accountRepository;
    private final CardRepository cardRepository;
    private final ValidationService validationService;
    
    public TransactionServiceImpl(TransactionRepository transactionRepository,
                                 AccountRepository accountRepository,
                                 CardRepository cardRepository,
                                 ValidationService validationService) {
        this.transactionRepository = transactionRepository;
        this.accountRepository = accountRepository;
        this.cardRepository = cardRepository;
        this.validationService = validationService;
    }
    
    @Override
    public TransactionDto getTransaction(Long transactionId) {
        logger.debug("Retrieving transaction with ID: {}", transactionId);
        
        Transaction transaction = transactionRepository.findById(transactionId)
            .orElseThrow(() -> new ResourceNotFoundException("Transaction not found with ID: " + transactionId));
        
        logger.info("Successfully retrieved transaction ID: {}", transactionId);
        return convertToDto(transaction);
    }
    
    @Override
    public Page<TransactionDto> getTransactionsByAccount(Long accountId, String startDate, String endDate, Pageable pageable) {
        logger.debug("Retrieving transactions for account: {}, startDate: {}, endDate: {}", accountId, startDate, endDate);
        
        // Verify account exists
        if (!accountRepository.existsById(accountId)) {
            throw new ResourceNotFoundException("Account not found with ID: " + accountId);
        }
        
        // Validate dates if provided
        if (startDate != null && !startDate.isEmpty()) {
            var dateValidation = validationService.validateDate(startDate);
            if (!dateValidation.isValid()) {
                throw new ValidationException(dateValidation.getMessage());
            }
        }
        
        if (endDate != null && !endDate.isEmpty()) {
            var dateValidation = validationService.validateDate(endDate);
            if (!dateValidation.isValid()) {
                throw new ValidationException(dateValidation.getMessage());
            }
        }
        
        Page<Transaction> transactions;
        
        // Use appropriate repository method based on whether date filters are provided
        if ((startDate != null && !startDate.isEmpty()) && (endDate != null && !endDate.isEmpty())) {
            transactions = transactionRepository.findByAccountIdAndDateRange(accountId, startDate, endDate, pageable);
        } else {
            transactions = transactionRepository.findByAccountId(accountId, pageable);
        }
        
        logger.info("Retrieved {} transactions for account: {}", transactions.getTotalElements(), accountId);
        return transactions.map(this::convertToDto);
    }
    
    @Override
    public Page<TransactionDto> getTransactionsByCard(Long cardNumber, String startDate, String endDate, Pageable pageable) {
        logger.debug("Retrieving transactions for card: {}, startDate: {}, endDate: {}", cardNumber, startDate, endDate);
        
        // Verify card exists
        if (!cardRepository.existsById(cardNumber)) {
            throw new ResourceNotFoundException("Card not found with number: " + cardNumber);
        }
        
        // Validate dates if provided
        if (startDate != null && !startDate.isEmpty()) {
            var dateValidation = validationService.validateDate(startDate);
            if (!dateValidation.isValid()) {
                throw new ValidationException(dateValidation.getMessage());
            }
        }
        
        if (endDate != null && !endDate.isEmpty()) {
            var dateValidation = validationService.validateDate(endDate);
            if (!dateValidation.isValid()) {
                throw new ValidationException(dateValidation.getMessage());
            }
        }
        
        Page<Transaction> transactions;
        
        // Use appropriate repository method based on whether date filters are provided
        if ((startDate != null && !startDate.isEmpty()) && (endDate != null && !endDate.isEmpty())) {
            transactions = transactionRepository.findByCardNumberAndDateRange(cardNumber, startDate, endDate, pageable);
        } else {
            transactions = transactionRepository.findByCardNumber(cardNumber, pageable);
        }
        
        logger.info("Retrieved {} transactions for card: {}", transactions.getTotalElements(), cardNumber);
        return transactions.map(this::convertToDto);
    }
    
    @Override
    @Transactional
    public TransactionDto createTransaction(TransactionCreateDto txnData) {
        logger.debug("Creating transaction for account: {}", txnData.getAccountId());
        
        // Validate required fields
        if (txnData.getAccountId() == null) {
            throw new ValidationException("Account ID is required");
        }
        
        if (txnData.getTransactionType() == null || txnData.getTransactionType().trim().isEmpty()) {
            throw new ValidationException("Transaction type is required");
        }
        
        if (txnData.getAmount() == null) {
            throw new ValidationException("Transaction amount is required");
        }
        
        if (txnData.getTransactionDate() == null || txnData.getTransactionDate().trim().isEmpty()) {
            throw new ValidationException("Transaction date is required");
        }
        
        // Validate amount format
        var amountValidation = validationService.validateCurrencyAmount(txnData.getAmount().toString());
        if (!amountValidation.isValid()) {
            throw new ValidationException(amountValidation.getMessage());
        }
        
        // Validate date format
        var dateValidation = validationService.validateDate(txnData.getTransactionDate());
        if (!dateValidation.isValid()) {
            throw new ValidationException(dateValidation.getMessage());
        }
        
        // Verify account exists
        Account account = accountRepository.findById(txnData.getAccountId())
            .orElseThrow(() -> new ResourceNotFoundException("Account not found with ID: " + txnData.getAccountId()));
        
        // Verify card exists if provided
        if (txnData.getCardNumber() != null) {
            Card card = cardRepository.findById(txnData.getCardNumber())
                .orElseThrow(() -> new ResourceNotFoundException("Card not found with number: " + txnData.getCardNumber()));
            
            // Verify card belongs to account
            if (!card.getAccountId().equals(txnData.getAccountId())) {
                throw new ValidationException("Card does not belong to the specified account");
            }
        }
        
        // Create transaction entity
        Transaction transaction = new Transaction();
        transaction.setAccountId(txnData.getAccountId());
        transaction.setCardNumber(txnData.getCardNumber());
        transaction.setTransactionType(txnData.getTransactionType());
        transaction.setTransactionCategory(txnData.getTransactionCategory());
        transaction.setAmount(txnData.getAmount());
        transaction.setTransactionDate(txnData.getTransactionDate());
        transaction.setTransactionTime(txnData.getTransactionTime());
        transaction.setDescription(txnData.getDescription());
        transaction.setMerchantName(txnData.getMerchantName());
        transaction.setMerchantCity(txnData.getMerchantCity());
        transaction.setMerchantZip(txnData.getMerchantZip());
        
        // Save transaction
        Transaction savedTransaction = transactionRepository.save(transaction);
        
        // Update account balance
        BigDecimal newBalance = account.getCurrentBalance().add(txnData.getAmount());
        account.setCurrentBalance(newBalance);
        accountRepository.save(account);
        
        logger.info("Successfully created transaction ID: {} for account: {}, new balance: {}", 
                   savedTransaction.getTransactionId(), txnData.getAccountId(), newBalance);
        
        return convertToDto(savedTransaction);
    }
    
    /**
     * Convert Transaction entity to DTO.
     */
    private TransactionDto convertToDto(Transaction transaction) {
        return new TransactionDto(
            transaction.getTransactionId(),
            transaction.getAccountId(),
            transaction.getCardNumber(),
            transaction.getTransactionType(),
            transaction.getTransactionCategory(),
            transaction.getAmount(),
            transaction.getTransactionDate(),
            transaction.getTransactionTime(),
            transaction.getDescription(),
            transaction.getMerchantName(),
            transaction.getMerchantCity(),
            transaction.getMerchantZip()
        );
    }
}
