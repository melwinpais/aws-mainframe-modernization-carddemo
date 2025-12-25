package com.carddemo.controller;

import com.carddemo.dto.AccountDto;
import com.carddemo.dto.AccountUpdateDto;
import com.carddemo.dto.CardDto;
import com.carddemo.dto.CustomerDto;
import com.carddemo.service.AccountService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * REST Controller for Account Management
 * Provides endpoints for account viewing, updating, and related data retrieval
 * 
 * Endpoints:
 * - GET /api/accounts/{accountId} - Get account details
 * - PUT /api/accounts/{accountId} - Update account information
 * - GET /api/accounts/{accountId}/customer - Get customer for account
 * - GET /api/accounts/{accountId}/cards - Get cards for account
 * - GET /api/accounts/search - Search accounts by criteria
 */
@RestController
@RequestMapping("/api/accounts")
@CrossOrigin(origins = "*", maxAge = 3600)
public class AccountController {

    private static final Logger logger = LoggerFactory.getLogger(AccountController.class);

    @Autowired
    private AccountService accountService;

    /**
     * Get account details by account ID
     * Retrieves account information along with associated customer and cards
     * 
     * @param accountId the account ID to retrieve
     * @return ResponseEntity containing AccountDto with account, customer, and cards
     */
    @GetMapping("/{accountId}")
    public ResponseEntity<AccountDto> getAccount(@PathVariable Long accountId) {
        logger.info("GET /api/accounts/{} - Retrieving account", accountId);
        
        AccountDto account = accountService.getAccount(accountId);
        
        logger.info("Successfully retrieved account: {}", accountId);
        return ResponseEntity.ok(account);
    }

    /**
     * Update account information
     * Validates and persists account updates
     * 
     * @param accountId the account ID to update
     * @param updates the account update data
     * @return ResponseEntity containing updated AccountDto
     */
    @PutMapping("/{accountId}")
    public ResponseEntity<AccountDto> updateAccount(
            @PathVariable Long accountId,
            @RequestBody AccountUpdateDto updates) {
        logger.info("PUT /api/accounts/{} - Updating account", accountId);
        
        AccountDto updatedAccount = accountService.updateAccount(accountId, updates);
        
        logger.info("Successfully updated account: {}", accountId);
        return ResponseEntity.ok(updatedAccount);
    }

    /**
     * Get customer information for an account
     * 
     * @param accountId the account ID
     * @return ResponseEntity containing CustomerDto
     */
    @GetMapping("/{accountId}/customer")
    public ResponseEntity<CustomerDto> getCustomerForAccount(@PathVariable Long accountId) {
        logger.info("GET /api/accounts/{}/customer - Retrieving customer", accountId);
        
        CustomerDto customer = accountService.getCustomerForAccount(accountId);
        
        logger.info("Successfully retrieved customer for account: {}", accountId);
        return ResponseEntity.ok(customer);
    }

    /**
     * Get all cards associated with an account
     * 
     * @param accountId the account ID
     * @return ResponseEntity containing List of CardDto
     */
    @GetMapping("/{accountId}/cards")
    public ResponseEntity<List<CardDto>> getCardsForAccount(@PathVariable Long accountId) {
        logger.info("GET /api/accounts/{}/cards - Retrieving cards", accountId);
        
        List<CardDto> cards = accountService.getCardsForAccount(accountId);
        
        logger.info("Successfully retrieved {} cards for account: {}", cards.size(), accountId);
        return ResponseEntity.ok(cards);
    }

    /**
     * Search accounts by account ID or customer ID
     * 
     * @param accountId optional account ID to search for
     * @param customerId optional customer ID to search for
     * @return ResponseEntity containing List of AccountDto matching search criteria
     */
    @GetMapping("/search")
    public ResponseEntity<List<AccountDto>> searchAccounts(
            @RequestParam(required = false) Long accountId,
            @RequestParam(required = false) Long customerId) {
        logger.info("GET /api/accounts/search - accountId: {}, customerId: {}", accountId, customerId);
        
        List<AccountDto> accounts = accountService.searchAccounts(accountId, customerId);
        
        logger.info("Found {} accounts matching search criteria", accounts.size());
        return ResponseEntity.ok(accounts);
    }
}
