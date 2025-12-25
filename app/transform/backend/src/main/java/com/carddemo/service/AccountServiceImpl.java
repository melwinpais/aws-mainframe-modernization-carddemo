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
import com.carddemo.service.ValidationResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Implementation of AccountService
 * Handles account management operations including viewing, updating, and related data retrieval
 */
@Service
@Transactional
public class AccountServiceImpl implements AccountService {

    private static final Logger logger = LoggerFactory.getLogger(AccountServiceImpl.class);

    @Autowired
    private AccountRepository accountRepository;

    @Autowired
    private CustomerRepository customerRepository;

    @Autowired
    private CardRepository cardRepository;

    @Autowired
    private ValidationService validationService;

    @Override
    @Transactional(readOnly = true)
    public AccountDto getAccount(Long accountId) {
        logger.info("Retrieving account with ID: {}", accountId);

        // Validate account ID
        ValidationResult validation = validateAccountId(String.valueOf(accountId));
        if (!validation.isValid()) {
            logger.warn("Invalid account ID: {}", accountId);
            throw new ValidationException(validation.getMessage());
        }

        // Retrieve account
        Account account = accountRepository.findByAccountId(accountId)
                .orElseThrow(() -> {
                    logger.error("Account not found: {}", accountId);
                    return new ResourceNotFoundException("Did not find this account in account master file");
                });

        // Convert to DTO
        AccountDto accountDto = convertToDto(account);

        // Retrieve and set customer
        try {
            CustomerDto customerDto = getCustomerForAccount(accountId);
            accountDto.setCustomer(customerDto);
        } catch (ResourceNotFoundException e) {
            logger.error("Customer not found for account: {}", accountId);
            throw new ResourceNotFoundException("Did not find associated customer in master file");
        }

        // Retrieve and set cards
        List<CardDto> cards = getCardsForAccount(accountId);
        accountDto.setCards(cards);

        logger.info("Successfully retrieved account: {}", accountId);
        return accountDto;
    }

    @Override
    public AccountDto updateAccount(Long accountId, AccountUpdateDto updates) {
        logger.info("Updating account with ID: {}", accountId);

        // Validate account ID
        ValidationResult validation = validateAccountId(String.valueOf(accountId));
        if (!validation.isValid()) {
            logger.warn("Invalid account ID for update: {}", accountId);
            throw new ValidationException(validation.getMessage());
        }

        // Retrieve existing account
        Account account = accountRepository.findByAccountId(accountId)
                .orElseThrow(() -> {
                    logger.error("Account not found for update: {}", accountId);
                    return new ResourceNotFoundException("Did not find this account in account master file");
                });

        // Validate and apply updates
        if (updates.getActiveStatus() != null) {
            ValidationResult statusValidation = validationService.validateAccountStatus(updates.getActiveStatus());
            if (!statusValidation.isValid()) {
                throw new ValidationException(statusValidation.getMessage());
            }
            account.setActiveStatus(updates.getActiveStatus());
        }

        if (updates.getCreditLimit() != null) {
            ValidationResult creditLimitValidation = validationService.validateCurrencyAmount(
                    updates.getCreditLimit().toString());
            if (!creditLimitValidation.isValid()) {
                throw new ValidationException("Credit Limit: " + creditLimitValidation.getMessage());
            }
            account.setCreditLimit(updates.getCreditLimit());
        }

        if (updates.getCashCreditLimit() != null) {
            ValidationResult cashLimitValidation = validationService.validateCurrencyAmount(
                    updates.getCashCreditLimit().toString());
            if (!cashLimitValidation.isValid()) {
                throw new ValidationException("Cash Credit Limit: " + cashLimitValidation.getMessage());
            }
            account.setCashCreditLimit(updates.getCashCreditLimit());
        }

        if (updates.getCurrentBalance() != null) {
            ValidationResult balanceValidation = validationService.validateCurrencyAmount(
                    updates.getCurrentBalance().toString());
            if (!balanceValidation.isValid()) {
                throw new ValidationException("Current Balance: " + balanceValidation.getMessage());
            }
            account.setCurrentBalance(updates.getCurrentBalance());
        }

        if (updates.getCurrentCycleCredit() != null) {
            account.setCurrentCycleCredit(updates.getCurrentCycleCredit());
        }

        if (updates.getCurrentCycleDebit() != null) {
            account.setCurrentCycleDebit(updates.getCurrentCycleDebit());
        }

        // Save account
        Account savedAccount = accountRepository.save(account);
        logger.info("Successfully updated account: {}", accountId);

        // Update customer if provided
        if (updates.getCustomer() != null) {
            updateCustomer(account.getCustomerId(), updates.getCustomer());
        }

        // Return updated account with customer and cards
        return getAccount(accountId);
    }

    @Override
    @Transactional(readOnly = true)
    public CustomerDto getCustomerForAccount(Long accountId) {
        logger.info("Retrieving customer for account: {}", accountId);

        // Retrieve account to get customer ID
        Account account = accountRepository.findByAccountId(accountId)
                .orElseThrow(() -> {
                    logger.error("Account not found: {}", accountId);
                    return new ResourceNotFoundException("Did not find this account in account master file");
                });

        // Retrieve customer
        Customer customer = customerRepository.findByCustomerId(account.getCustomerId())
                .orElseThrow(() -> {
                    logger.error("Customer not found for account: {}", accountId);
                    return new ResourceNotFoundException("Did not find associated customer in master file");
                });

        logger.info("Successfully retrieved customer for account: {}", accountId);
        return convertToCustomerDto(customer);
    }

    @Override
    @Transactional(readOnly = true)
    public List<CardDto> getCardsForAccount(Long accountId) {
        logger.info("Retrieving cards for account: {}", accountId);

        // Verify account exists
        if (!accountRepository.existsByAccountId(accountId)) {
            logger.error("Account not found: {}", accountId);
            throw new ResourceNotFoundException("Did not find this account in account master file");
        }

        // Retrieve cards
        List<Card> cards = cardRepository.findByAccountId(accountId);
        logger.info("Found {} cards for account: {}", cards.size(), accountId);

        return cards.stream()
                .map(this::convertToCardDto)
                .collect(Collectors.toList());
    }

    @Override
    @Transactional(readOnly = true)
    public List<AccountDto> searchAccounts(Long accountId, Long customerId) {
        logger.info("Searching accounts - accountId: {}, customerId: {}", accountId, customerId);

        List<Account> accounts = new ArrayList<>();

        if (accountId != null) {
            // Search by account ID
            accountRepository.findByAccountId(accountId).ifPresent(accounts::add);
        } else if (customerId != null) {
            // Search by customer ID - find all accounts for this customer
            // Note: This requires a query method in AccountRepository
            accounts = accountRepository.findAll().stream()
                    .filter(account -> account.getCustomerId().equals(customerId))
                    .collect(Collectors.toList());
        } else {
            // No search criteria provided, return empty list
            logger.warn("No search criteria provided");
        }

        logger.info("Found {} accounts matching search criteria", accounts.size());

        return accounts.stream()
                .map(this::convertToDto)
                .collect(Collectors.toList());
    }

    @Override
    public com.carddemo.service.ValidationResult validateAccountId(String accountId) {
        // Check if empty or null
        if (accountId == null || accountId.trim().isEmpty()) {
            return com.carddemo.service.ValidationResult.failure("Account number not provided");
        }

        // Check if numeric
        if (!accountId.matches("\\d+")) {
            return com.carddemo.service.ValidationResult.failure("Account number must be a non zero 11 digit number");
        }

        // Check if 11 digits
        if (accountId.length() != 11) {
            return com.carddemo.service.ValidationResult.failure("Account number must be a non zero 11 digit number");
        }

        // Check if all zeroes
        if (accountId.equals("00000000000")) {
            return com.carddemo.service.ValidationResult.failure("Account number must be a non zero 11 digit number");
        }

        return com.carddemo.service.ValidationResult.success();
    }

    /**
     * Update customer information
     */
    private void updateCustomer(Long customerId, CustomerUpdateDto updates) {
        logger.info("Updating customer: {}", customerId);

        Customer customer = customerRepository.findByCustomerId(customerId)
                .orElseThrow(() -> {
                    logger.error("Customer not found for update: {}", customerId);
                    return new ResourceNotFoundException("Did not find associated customer in master file");
                });

        // Apply updates
        if (updates.getFirstName() != null) {
            ValidationResult nameValidation = validationService.validateName(updates.getFirstName());
            if (!nameValidation.isValid()) {
                throw new ValidationException("First Name: " + nameValidation.getMessage());
            }
            customer.setFirstName(updates.getFirstName());
        }

        if (updates.getMiddleName() != null) {
            ValidationResult nameValidation = validationService.validateName(updates.getMiddleName());
            if (!nameValidation.isValid()) {
                throw new ValidationException("Middle Name: " + nameValidation.getMessage());
            }
            customer.setMiddleName(updates.getMiddleName());
        }

        if (updates.getLastName() != null) {
            ValidationResult nameValidation = validationService.validateName(updates.getLastName());
            if (!nameValidation.isValid()) {
                throw new ValidationException("Last Name: " + nameValidation.getMessage());
            }
            customer.setLastName(updates.getLastName());
        }

        if (updates.getAddressLine1() != null) {
            customer.setAddressLine1(updates.getAddressLine1());
        }

        if (updates.getAddressLine2() != null) {
            customer.setAddressLine2(updates.getAddressLine2());
        }

        if (updates.getAddressLine3() != null) {
            customer.setAddressLine3(updates.getAddressLine3());
        }

        if (updates.getStateCode() != null) {
            customer.setStateCode(updates.getStateCode());
        }

        if (updates.getCountryCode() != null) {
            customer.setCountryCode(updates.getCountryCode());
        }

        if (updates.getZipCode() != null) {
            customer.setZipCode(updates.getZipCode());
        }

        if (updates.getPhoneNumber1() != null) {
            ValidationResult phoneValidation = validationService.validatePhoneNumber(updates.getPhoneNumber1());
            if (!phoneValidation.isValid()) {
                throw new ValidationException("Phone Number 1: " + phoneValidation.getMessage());
            }
            customer.setPhoneNumber1(updates.getPhoneNumber1());
        }

        if (updates.getPhoneNumber2() != null) {
            ValidationResult phoneValidation = validationService.validatePhoneNumber(updates.getPhoneNumber2());
            if (!phoneValidation.isValid()) {
                throw new ValidationException("Phone Number 2: " + phoneValidation.getMessage());
            }
            customer.setPhoneNumber2(updates.getPhoneNumber2());
        }

        if (updates.getGovernmentIssuedId() != null) {
            customer.setGovernmentIssuedId(updates.getGovernmentIssuedId());
        }

        if (updates.getDateOfBirth() != null) {
            ValidationResult dateValidation = validationService.validateDate(updates.getDateOfBirth());
            if (!dateValidation.isValid()) {
                throw new ValidationException("Date of Birth: " + dateValidation.getMessage());
            }
            customer.setDateOfBirth(updates.getDateOfBirth());
        }

        if (updates.getEftAccountId() != null) {
            customer.setEftAccountId(updates.getEftAccountId());
        }

        if (updates.getPrimaryCardholderInd() != null) {
            customer.setPrimaryCardholderInd(updates.getPrimaryCardholderInd());
        }

        if (updates.getFicoCreditScore() != null) {
            ValidationResult ficoValidation = validationService.validateFicoScore(
                    updates.getFicoCreditScore().toString());
            if (!ficoValidation.isValid()) {
                throw new ValidationException("FICO Score: " + ficoValidation.getMessage());
            }
            customer.setFicoCreditScore(updates.getFicoCreditScore());
        }

        customerRepository.save(customer);
        logger.info("Successfully updated customer: {}", customerId);
    }

    /**
     * Convert Account entity to AccountDto
     */
    private AccountDto convertToDto(Account account) {
        AccountDto dto = new AccountDto();
        dto.setAccountId(account.getAccountId());
        dto.setActiveStatus(account.getActiveStatus());
        dto.setCurrentBalance(account.getCurrentBalance());
        dto.setCreditLimit(account.getCreditLimit());
        dto.setCashCreditLimit(account.getCashCreditLimit());
        dto.setOpenDate(account.getOpenDate());
        dto.setExpirationDate(account.getExpirationDate());
        dto.setReissueDate(account.getReissueDate());
        dto.setCurrentCycleCredit(account.getCurrentCycleCredit());
        dto.setCurrentCycleDebit(account.getCurrentCycleDebit());
        dto.setAddressZip(account.getAddressZip());
        dto.setGroupId(account.getGroupId());
        return dto;
    }

    /**
     * Convert Customer entity to CustomerDto
     */
    private CustomerDto convertToCustomerDto(Customer customer) {
        CustomerDto dto = new CustomerDto();
        dto.setCustomerId(customer.getCustomerId());
        dto.setFirstName(customer.getFirstName());
        dto.setMiddleName(customer.getMiddleName());
        dto.setLastName(customer.getLastName());
        dto.setAddressLine1(customer.getAddressLine1());
        dto.setAddressLine2(customer.getAddressLine2());
        dto.setAddressLine3(customer.getAddressLine3());
        dto.setStateCode(customer.getStateCode());
        dto.setCountryCode(customer.getCountryCode());
        dto.setZipCode(customer.getZipCode());
        dto.setPhoneNumber1(customer.getPhoneNumber1());
        dto.setPhoneNumber2(customer.getPhoneNumber2());
        dto.setSsn(customer.getSsn());
        dto.setGovernmentIssuedId(customer.getGovernmentIssuedId());
        dto.setDateOfBirth(customer.getDateOfBirth());
        dto.setEftAccountId(customer.getEftAccountId());
        dto.setPrimaryCardholderInd(customer.getPrimaryCardholderInd());
        dto.setFicoCreditScore(customer.getFicoCreditScore());
        return dto;
    }

    /**
     * Convert Card entity to CardDto
     */
    private CardDto convertToCardDto(Card card) {
        CardDto dto = new CardDto();
        dto.setCardNumber(card.getCardNumber());
        dto.setAccountId(card.getAccountId());
        dto.setCustomerId(card.getCustomerId());
        dto.setCardStatus(card.getCardStatus());
        dto.setExpirationDate(card.getExpirationDate());
        dto.setIssueDate(card.getIssueDate());
        return dto;
    }
}
