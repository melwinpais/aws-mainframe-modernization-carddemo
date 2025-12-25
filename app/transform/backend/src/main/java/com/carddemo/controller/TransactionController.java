package com.carddemo.controller;

import com.carddemo.dto.TransactionCreateDto;
import com.carddemo.dto.TransactionDto;
import com.carddemo.service.TransactionService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * REST controller for transaction management operations.
 * Provides endpoints for retrieving and creating transactions.
 */
@RestController
@RequestMapping("/api/transactions")
public class TransactionController {
    
    private static final Logger logger = LoggerFactory.getLogger(TransactionController.class);
    
    private final TransactionService transactionService;
    
    public TransactionController(TransactionService transactionService) {
        this.transactionService = transactionService;
    }
    
    /**
     * Get a transaction by ID.
     * 
     * GET /api/transactions/{transactionId}
     * 
     * @param transactionId the transaction ID
     * @return the transaction DTO
     */
    @GetMapping("/{transactionId}")
    public ResponseEntity<TransactionDto> getTransaction(@PathVariable Long transactionId) {
        logger.info("GET /api/transactions/{} - Retrieving transaction", transactionId);
        
        TransactionDto transaction = transactionService.getTransaction(transactionId);
        
        logger.info("Successfully retrieved transaction ID: {}", transactionId);
        return ResponseEntity.ok(transaction);
    }
    
    /**
     * Get transactions by account with optional date filtering and pagination.
     * 
     * GET /api/transactions/account/{accountId}?startDate=YYYY-MM-DD&endDate=YYYY-MM-DD&page=0&size=20
     * 
     * @param accountId the account ID
     * @param startDate optional start date filter (YYYY-MM-DD)
     * @param endDate optional end date filter (YYYY-MM-DD)
     * @param page page number (default 0)
     * @param size page size (default 20)
     * @return page of transaction DTOs
     */
    @GetMapping("/account/{accountId}")
    public ResponseEntity<Page<TransactionDto>> getTransactionsByAccount(
            @PathVariable Long accountId,
            @RequestParam(required = false) String startDate,
            @RequestParam(required = false) String endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {
        
        logger.info("GET /api/transactions/account/{} - Retrieving transactions (page: {}, size: {}, startDate: {}, endDate: {})", 
                   accountId, page, size, startDate, endDate);
        
        // Create pageable with sorting by transaction date descending
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "transactionDate", "transactionId"));
        
        Page<TransactionDto> transactions = transactionService.getTransactionsByAccount(
            accountId, startDate, endDate, pageable);
        
        logger.info("Successfully retrieved {} transactions for account: {}", 
                   transactions.getTotalElements(), accountId);
        return ResponseEntity.ok(transactions);
    }
    
    /**
     * Get transactions by card with optional date filtering and pagination.
     * 
     * GET /api/transactions/card/{cardNumber}?startDate=YYYY-MM-DD&endDate=YYYY-MM-DD&page=0&size=20
     * 
     * @param cardNumber the card number
     * @param startDate optional start date filter (YYYY-MM-DD)
     * @param endDate optional end date filter (YYYY-MM-DD)
     * @param page page number (default 0)
     * @param size page size (default 20)
     * @return page of transaction DTOs
     */
    @GetMapping("/card/{cardNumber}")
    public ResponseEntity<Page<TransactionDto>> getTransactionsByCard(
            @PathVariable Long cardNumber,
            @RequestParam(required = false) String startDate,
            @RequestParam(required = false) String endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {
        
        logger.info("GET /api/transactions/card/{} - Retrieving transactions (page: {}, size: {}, startDate: {}, endDate: {})", 
                   cardNumber, page, size, startDate, endDate);
        
        // Create pageable with sorting by transaction date descending
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "transactionDate", "transactionId"));
        
        Page<TransactionDto> transactions = transactionService.getTransactionsByCard(
            cardNumber, startDate, endDate, pageable);
        
        logger.info("Successfully retrieved {} transactions for card: {}", 
                   transactions.getTotalElements(), cardNumber);
        return ResponseEntity.ok(transactions);
    }
    
    /**
     * Create a new transaction.
     * 
     * POST /api/transactions
     * 
     * @param txnData the transaction creation data
     * @return the created transaction DTO
     */
    @PostMapping
    public ResponseEntity<TransactionDto> createTransaction(@RequestBody TransactionCreateDto txnData) {
        logger.info("POST /api/transactions - Creating transaction for account: {}", txnData.getAccountId());
        
        TransactionDto createdTransaction = transactionService.createTransaction(txnData);
        
        logger.info("Successfully created transaction ID: {} for account: {}", 
                   createdTransaction.getTransactionId(), createdTransaction.getAccountId());
        return ResponseEntity.status(HttpStatus.CREATED).body(createdTransaction);
    }
}
