package com.carddemo.dto;

import jakarta.validation.constraints.*;

/**
 * Data Transfer Object for updating an existing card
 * Used for PUT /api/cards/{cardNumber} requests
 */
public class CardUpdateDto {
    
    @Size(max = 10, message = "Card status must not exceed 10 characters")
    private String cardStatus;
    
    @Pattern(regexp = "^\\d{4}-\\d{2}-\\d{2}$", message = "Expiration date must be in YYYY-MM-DD format")
    private String expirationDate;
    
    @Pattern(regexp = "^\\d{4}-\\d{2}-\\d{2}$", message = "Issue date must be in YYYY-MM-DD format")
    private String issueDate;

    // Constructors
    public CardUpdateDto() {
    }

    public CardUpdateDto(String cardStatus, String expirationDate, String issueDate) {
        this.cardStatus = cardStatus;
        this.expirationDate = expirationDate;
        this.issueDate = issueDate;
    }

    // Getters and Setters
    public String getCardStatus() {
        return cardStatus;
    }

    public void setCardStatus(String cardStatus) {
        this.cardStatus = cardStatus;
    }

    public String getExpirationDate() {
        return expirationDate;
    }

    public void setExpirationDate(String expirationDate) {
        this.expirationDate = expirationDate;
    }

    public String getIssueDate() {
        return issueDate;
    }

    public void setIssueDate(String issueDate) {
        this.issueDate = issueDate;
    }
}
