package com.carddemo.service;

import com.carddemo.dto.CardCreateDto;
import com.carddemo.dto.CardDto;
import com.carddemo.dto.CardUpdateDto;

import java.util.List;

/**
 * Service interface for Card management operations
 * Handles business logic for card viewing, creation, and updates
 */
public interface CardService {
    
    /**
     * Get card by card number
     * @param cardNumber the card number to retrieve
     * @return CardDto containing card details
     * @throws com.carddemo.exception.ResourceNotFoundException if card not found
     */
    CardDto getCard(Long cardNumber);
    
    /**
     * Get all cards for a specific account
     * @param accountId the account ID to retrieve cards for
     * @return List of CardDto objects
     */
    List<CardDto> getCardsByAccount(Long accountId);
    
    /**
     * Create a new card
     * @param cardData the card creation data
     * @return CardDto containing the created card details
     * @throws com.carddemo.exception.ValidationException if validation fails
     * @throws com.carddemo.exception.ResourceNotFoundException if account or customer not found
     */
    CardDto createCard(CardCreateDto cardData);
    
    /**
     * Update an existing card
     * @param cardNumber the card number to update
     * @param updates the card update data
     * @return CardDto containing the updated card details
     * @throws com.carddemo.exception.ResourceNotFoundException if card not found
     * @throws com.carddemo.exception.ValidationException if validation fails
     */
    CardDto updateCard(Long cardNumber, CardUpdateDto updates);
    
    /**
     * Deactivate a card (soft delete)
     * @param cardNumber the card number to deactivate
     * @throws com.carddemo.exception.ResourceNotFoundException if card not found
     */
    void deactivateCard(Long cardNumber);
}
