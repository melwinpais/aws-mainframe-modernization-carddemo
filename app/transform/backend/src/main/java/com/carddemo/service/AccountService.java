package com.carddemo.service;

import com.carddemo.dto.AccountDto;
import com.carddemo.dto.AccountUpdateDto;
import com.carddemo.dto.CardDto;
import com.carddemo.dto.CustomerDto;

import java.util.List;

/**
 * Service interface for account management operations
 * Handles account viewing, updating, and related data retrieval
 */
public interface AccountService {

    /**
     * Get account details by account ID
     * Retrieves account information along with associated customer and cards
     * 
     * @param accountId the account ID to retrieve
     * @return AccountDto containing account details
     * @throws com.carddemo.exception.ResourceNotFoundException if account not found
     * @throws com.carddemo.exception.ValidationException if account ID is invalid
     */
    AccountDto getAccount(Long accountId);

    /**
     * Update account information
     * Validates and persists account updates
     * 
     * @param accountId the account ID to update
     * @param updates the account update data
     * @return AccountDto containing updated account details
     * @throws com.carddemo.exception.ResourceNotFoundException if account not found
     * @throws com.carddemo.exception.ValidationException if update data is invalid
     */
    AccountDto updateAccount(Long accountId, AccountUpdateDto updates);

    /**
     * Get customer information for an account
     * 
     * @param accountId the account ID
     * @return CustomerDto containing customer details
     * @throws com.carddemo.exception.ResourceNotFoundException if account or customer not found
     */
    CustomerDto getCustomerForAccount(Long accountId);

    /**
     * Get all cards associated with an account
     * 
     * @param accountId the account ID
     * @return List of CardDto containing card details
     * @throws com.carddemo.exception.ResourceNotFoundException if account not found
     */
    List<CardDto> getCardsForAccount(Long accountId);

    /**
     * Search accounts by account ID or customer ID
     * 
     * @param accountId optional account ID to search for
     * @param customerId optional customer ID to search for
     * @return List of AccountDto matching search criteria
     */
    List<AccountDto> searchAccounts(Long accountId, Long customerId);

    /**
     * Validate account ID format and value
     * 
     * @param accountId the account ID string to validate
     * @return ValidationResult indicating if valid and any error message
     */
    com.carddemo.service.ValidationResult validateAccountId(String accountId);
}
