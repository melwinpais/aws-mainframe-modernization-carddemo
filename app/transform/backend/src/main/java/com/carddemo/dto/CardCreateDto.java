package com.carddemo.dto;

import jakarta.validation.constraints.*;

/**
 * Data Transfer Object for creating a new card
 * Used for POST /api/cards requests
 */
public class CardCreateDto {
    
    @NotNull(message = "Card number is required")
    @Digits(integer = 16, fraction = 0, message = "Card number must be 16 digits")
    private Long cardNumber;
    
    @NotNull(message = "Account ID is required")
    @Digits(integer = 11, fraction = 0, message = "Account ID must be 11 digits")
    private Long accountId;
    
    @NotNull(message = "Customer ID is required")
    @Digits(integer = 9, fraction = 0, message = "Customer ID must be 9 digits")
    private Long customerId;
    
    @NotBlank(message = "Card status is required")
    @Size(max = 10, message = "Card status must not exceed 10 characters")
    private String cardStatus;
    
    @Pattern(regexp = "^\\d{4}-\\d{2}-\\d{2}$", message = "Expiration date must be in YYYY-MM-DD format")
    private String expirationDate;
    
    @Pattern(regexp = "^\\d{4}-\\d{2}-\\d{2}$", message = "Issue date must be in YYYY-MM-DD format")
    private String issueDate;

    // Constructors
    public CardCreateDto() {
    }

    public CardCreateDto(Long cardNumber, Long accountId, Long customerId, String cardStatus,
                         String expirationDate, String issueDate) {
        this.cardNumber = cardNumber;
        this.accountId = accountId;
        this.customerId = customerId;
        this.cardStatus = cardStatus;
        this.expirationDate = expirationDate;
        this.issueDate = issueDate;
    }

    // Getters and Setters
    public Long getCardNumber() {
        return cardNumber;
    }

    public void setCardNumber(Long cardNumber) {
        this.cardNumber = cardNumber;
    }

    public Long getAccountId() {
        return accountId;
    }

    public void setAccountId(Long accountId) {
        this.accountId = accountId;
    }

    public Long getCustomerId() {
        return customerId;
    }

    public void setCustomerId(Long customerId) {
        this.customerId = customerId;
    }

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
