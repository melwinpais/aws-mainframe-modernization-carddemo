package com.carddemo.service;

import com.carddemo.dto.CardCreateDto;
import com.carddemo.dto.CardDto;
import com.carddemo.dto.CardUpdateDto;
import com.carddemo.entity.Account;
import com.carddemo.entity.Card;
import com.carddemo.entity.Customer;
import com.carddemo.exception.ResourceNotFoundException;
import com.carddemo.exception.ValidationException;
import com.carddemo.repository.AccountRepository;
import com.carddemo.repository.CardRepository;
import com.carddemo.repository.CustomerRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Implementation of CardService interface
 * Handles business logic for card management operations
 */
@Service
@Transactional
public class CardServiceImpl implements CardService {
    
    private static final Logger logger = LoggerFactory.getLogger(CardServiceImpl.class);
    
    @Autowired
    private CardRepository cardRepository;
    
    @Autowired
    private AccountRepository accountRepository;
    
    @Autowired
    private CustomerRepository customerRepository;
    
    @Autowired
    private ValidationService validationService;
    
    @Override
    @Transactional(readOnly = true)
    public CardDto getCard(Long cardNumber) {
        logger.info("Retrieving card with card number: {}", maskCardNumber(cardNumber));
        
        Card card = cardRepository.findByCardNumber(cardNumber)
                .orElseThrow(() -> {
                    logger.warn("Card not found: {}", maskCardNumber(cardNumber));
                    return new ResourceNotFoundException("Card not found with card number: " + maskCardNumber(cardNumber));
                });
        
        logger.info("Successfully retrieved card: {}", maskCardNumber(cardNumber));
        return convertToDto(card);
    }
    
    @Override
    @Transactional(readOnly = true)
    public List<CardDto> getCardsByAccount(Long accountId) {
        logger.info("Retrieving cards for account: {}", accountId);
        
        // Validate account exists
        if (!accountRepository.existsById(accountId)) {
            logger.warn("Account not found: {}", accountId);
            throw new ResourceNotFoundException("Account not found with ID: " + accountId);
        }
        
        List<Card> cards = cardRepository.findByAccountId(accountId);
        logger.info("Found {} cards for account: {}", cards.size(), accountId);
        
        return cards.stream()
                .map(this::convertToDto)
                .collect(Collectors.toList());
    }
    
    @Override
    public CardDto createCard(CardCreateDto cardData) {
        logger.info("Creating new card for account: {}", cardData.getAccountId());
        
        // Validate card number format (16 digits)
        validateCardNumber(cardData.getCardNumber());
        
        // Check if card already exists
        if (cardRepository.existsByCardNumber(cardData.getCardNumber())) {
            logger.warn("Card already exists: {}", maskCardNumber(cardData.getCardNumber()));
            throw new ValidationException("Card already exists with card number: " + maskCardNumber(cardData.getCardNumber()));
        }
        
        // Validate account exists
        Account account = accountRepository.findById(cardData.getAccountId())
                .orElseThrow(() -> {
                    logger.warn("Account not found: {}", cardData.getAccountId());
                    return new ResourceNotFoundException("Account not found with ID: " + cardData.getAccountId());
                });
        
        // Validate customer exists
        Customer customer = customerRepository.findById(cardData.getCustomerId())
                .orElseThrow(() -> {
                    logger.warn("Customer not found: {}", cardData.getCustomerId());
                    return new ResourceNotFoundException("Customer not found with ID: " + cardData.getCustomerId());
                });
        
        // Validate dates if provided
        if (cardData.getExpirationDate() != null && !cardData.getExpirationDate().isEmpty()) {
            var dateValidation = validationService.validateDate(cardData.getExpirationDate());
            if (!dateValidation.isValid()) {
                throw new ValidationException("Invalid expiration date: " + dateValidation.getMessage());
            }
        }
        
        if (cardData.getIssueDate() != null && !cardData.getIssueDate().isEmpty()) {
            var dateValidation = validationService.validateDate(cardData.getIssueDate());
            if (!dateValidation.isValid()) {
                throw new ValidationException("Invalid issue date: " + dateValidation.getMessage());
            }
        }
        
        // Create card entity
        Card card = new Card();
        card.setCardNumber(cardData.getCardNumber());
        card.setAccountId(cardData.getAccountId());
        card.setCustomerId(cardData.getCustomerId());
        card.setCardStatus(cardData.getCardStatus());
        card.setExpirationDate(cardData.getExpirationDate());
        card.setIssueDate(cardData.getIssueDate());
        
        Card savedCard = cardRepository.save(card);
        logger.info("Successfully created card: {}", maskCardNumber(savedCard.getCardNumber()));
        
        return convertToDto(savedCard);
    }
    
    @Override
    public CardDto updateCard(Long cardNumber, CardUpdateDto updates) {
        logger.info("Updating card: {}", maskCardNumber(cardNumber));
        
        Card card = cardRepository.findByCardNumber(cardNumber)
                .orElseThrow(() -> {
                    logger.warn("Card not found: {}", maskCardNumber(cardNumber));
                    return new ResourceNotFoundException("Card not found with card number: " + maskCardNumber(cardNumber));
                });
        
        // Update fields if provided
        if (updates.getCardStatus() != null && !updates.getCardStatus().isEmpty()) {
            card.setCardStatus(updates.getCardStatus());
        }
        
        if (updates.getExpirationDate() != null && !updates.getExpirationDate().isEmpty()) {
            var dateValidation = validationService.validateDate(updates.getExpirationDate());
            if (!dateValidation.isValid()) {
                throw new ValidationException("Invalid expiration date: " + dateValidation.getMessage());
            }
            card.setExpirationDate(updates.getExpirationDate());
        }
        
        if (updates.getIssueDate() != null && !updates.getIssueDate().isEmpty()) {
            var dateValidation = validationService.validateDate(updates.getIssueDate());
            if (!dateValidation.isValid()) {
                throw new ValidationException("Invalid issue date: " + dateValidation.getMessage());
            }
            card.setIssueDate(updates.getIssueDate());
        }
        
        Card updatedCard = cardRepository.save(card);
        logger.info("Successfully updated card: {}", maskCardNumber(updatedCard.getCardNumber()));
        
        return convertToDto(updatedCard);
    }
    
    @Override
    public void deactivateCard(Long cardNumber) {
        logger.info("Deactivating card: {}", maskCardNumber(cardNumber));
        
        Card card = cardRepository.findByCardNumber(cardNumber)
                .orElseThrow(() -> {
                    logger.warn("Card not found: {}", maskCardNumber(cardNumber));
                    return new ResourceNotFoundException("Card not found with card number: " + maskCardNumber(cardNumber));
                });
        
        // Set card status to inactive/deactivated
        card.setCardStatus("INACTIVE");
        cardRepository.save(card);
        
        logger.info("Successfully deactivated card: {}", maskCardNumber(cardNumber));
    }
    
    /**
     * Validate card number format (16 digits)
     */
    private void validateCardNumber(Long cardNumber) {
        if (cardNumber == null) {
            throw new ValidationException("Card number is required");
        }
        
        String cardNumberStr = String.valueOf(cardNumber);
        if (cardNumberStr.length() != 16) {
            throw new ValidationException("Card number must be exactly 16 digits");
        }
        
        if (!cardNumberStr.matches("\\d{16}")) {
            throw new ValidationException("Card number must contain only digits");
        }
    }
    
    /**
     * Convert Card entity to CardDto
     */
    private CardDto convertToDto(Card card) {
        return new CardDto(
                card.getCardNumber(),
                card.getAccountId(),
                card.getCustomerId(),
                card.getCardStatus(),
                card.getExpirationDate(),
                card.getIssueDate()
        );
    }
    
    /**
     * Mask card number for logging (show only last 4 digits)
     */
    private String maskCardNumber(Long cardNumber) {
        if (cardNumber == null) {
            return "null";
        }
        String cardNumberStr = String.valueOf(cardNumber);
        if (cardNumberStr.length() <= 4) {
            return "****";
        }
        return "************" + cardNumberStr.substring(cardNumberStr.length() - 4);
    }
}
