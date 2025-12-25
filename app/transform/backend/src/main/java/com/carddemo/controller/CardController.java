package com.carddemo.controller;

import com.carddemo.dto.CardCreateDto;
import com.carddemo.dto.CardDto;
import com.carddemo.dto.CardUpdateDto;
import com.carddemo.dto.MessageDto;
import com.carddemo.service.CardService;
import jakarta.validation.Valid;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * REST Controller for Card management operations
 * Handles HTTP requests for card viewing, creation, and updates
 * 
 * Endpoints:
 * - GET /api/cards/{cardNumber} - Get card by card number
 * - GET /api/cards/account/{accountId} - Get cards by account
 * - POST /api/cards - Create new card
 * - PUT /api/cards/{cardNumber} - Update card
 * - DELETE /api/cards/{cardNumber} - Deactivate card
 */
@RestController
@RequestMapping("/api/cards")
@CrossOrigin(origins = "*")
public class CardController {
    
    private static final Logger logger = LoggerFactory.getLogger(CardController.class);
    
    @Autowired
    private CardService cardService;
    
    /**
     * Get card by card number
     * GET /api/cards/{cardNumber}
     * 
     * @param cardNumber the card number to retrieve
     * @return ResponseEntity containing CardDto
     */
    @GetMapping("/{cardNumber}")
    public ResponseEntity<CardDto> getCard(@PathVariable Long cardNumber) {
        logger.info("GET /api/cards/{} - Retrieving card", maskCardNumber(cardNumber));
        
        CardDto card = cardService.getCard(cardNumber);
        
        logger.info("GET /api/cards/{} - Successfully retrieved card", maskCardNumber(cardNumber));
        return ResponseEntity.ok(card);
    }
    
    /**
     * Get all cards for a specific account
     * GET /api/cards/account/{accountId}
     * 
     * @param accountId the account ID to retrieve cards for
     * @return ResponseEntity containing list of CardDto
     */
    @GetMapping("/account/{accountId}")
    public ResponseEntity<List<CardDto>> getCardsByAccount(@PathVariable Long accountId) {
        logger.info("GET /api/cards/account/{} - Retrieving cards for account", accountId);
        
        List<CardDto> cards = cardService.getCardsByAccount(accountId);
        
        logger.info("GET /api/cards/account/{} - Successfully retrieved {} cards", accountId, cards.size());
        return ResponseEntity.ok(cards);
    }
    
    /**
     * Create a new card
     * POST /api/cards
     * 
     * @param cardData the card creation data
     * @return ResponseEntity containing created CardDto
     */
    @PostMapping
    public ResponseEntity<CardDto> createCard(@Valid @RequestBody CardCreateDto cardData) {
        logger.info("POST /api/cards - Creating new card for account: {}", cardData.getAccountId());
        
        CardDto createdCard = cardService.createCard(cardData);
        
        logger.info("POST /api/cards - Successfully created card: {}", maskCardNumber(createdCard.getCardNumber()));
        return ResponseEntity.status(HttpStatus.CREATED).body(createdCard);
    }
    
    /**
     * Update an existing card
     * PUT /api/cards/{cardNumber}
     * 
     * @param cardNumber the card number to update
     * @param updates the card update data
     * @return ResponseEntity containing updated CardDto
     */
    @PutMapping("/{cardNumber}")
    public ResponseEntity<CardDto> updateCard(
            @PathVariable Long cardNumber,
            @Valid @RequestBody CardUpdateDto updates) {
        logger.info("PUT /api/cards/{} - Updating card", maskCardNumber(cardNumber));
        
        CardDto updatedCard = cardService.updateCard(cardNumber, updates);
        
        logger.info("PUT /api/cards/{} - Successfully updated card", maskCardNumber(cardNumber));
        return ResponseEntity.ok(updatedCard);
    }
    
    /**
     * Deactivate a card (soft delete)
     * DELETE /api/cards/{cardNumber}
     * 
     * @param cardNumber the card number to deactivate
     * @return ResponseEntity containing success message
     */
    @DeleteMapping("/{cardNumber}")
    public ResponseEntity<MessageDto> deactivateCard(@PathVariable Long cardNumber) {
        logger.info("DELETE /api/cards/{} - Deactivating card", maskCardNumber(cardNumber));
        
        cardService.deactivateCard(cardNumber);
        
        logger.info("DELETE /api/cards/{} - Successfully deactivated card", maskCardNumber(cardNumber));
        return ResponseEntity.ok(new MessageDto("Card successfully deactivated"));
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
