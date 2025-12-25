package com.carddemo.service;

import com.carddemo.dto.TransactionCreateDto;
import com.carddemo.dto.TransactionDto;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

/**
 * Service interface for transaction management operations.
 * Handles transaction retrieval, creation, and processing.
 */
public interface TransactionService {
    
    /**
     * Retrieve a transaction by its ID.
     *
     * @param transactionId the transaction ID
     * @return the transaction DTO
     * @throws com.carddemo.exception.ResourceNotFoundException if transaction not found
     */
    TransactionDto getTransaction(Long transactionId);
    
    /**
     * Retrieve transactions for an account with pagination and optional date filtering.
     *
     * @param accountId the account ID
     * @param startDate optional start date filter (YYYY-MM-DD format)
     * @param endDate optional end date filter (YYYY-MM-DD format)
     * @param pageable pagination parameters
     * @return page of transaction DTOs
     * @throws com.carddemo.exception.ResourceNotFoundException if account not found
     */
    Page<TransactionDto> getTransactionsByAccount(Long accountId, String startDate, String endDate, Pageable pageable);
    
    /**
     * Retrieve transactions for a card with pagination and optional date filtering.
     *
     * @param cardNumber the card number
     * @param startDate optional start date filter (YYYY-MM-DD format)
     * @param endDate optional end date filter (YYYY-MM-DD format)
     * @param pageable pagination parameters
     * @return page of transaction DTOs
     * @throws com.carddemo.exception.ResourceNotFoundException if card not found
     */
    Page<TransactionDto> getTransactionsByCard(Long cardNumber, String startDate, String endDate, Pageable pageable);
    
    /**
     * Create a new transaction with validation and balance update.
     *
     * @param txnData the transaction creation data
     * @return the created transaction DTO
     * @throws com.carddemo.exception.ValidationException if validation fails
     * @throws com.carddemo.exception.ResourceNotFoundException if account or card not found
     */
    TransactionDto createTransaction(TransactionCreateDto txnData);
}
