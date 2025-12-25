package com.carddemo.dto;

/**
 * Data Transfer Object for Card information
 * Used for API responses containing card details
 */
public class CardDto {
    private Long cardNumber;
    private Long accountId;
    private Long customerId;
    private String cardStatus;
    private String expirationDate;
    private String issueDate;

    // Constructors
    public CardDto() {
    }

    public CardDto(Long cardNumber, Long accountId, Long customerId, String cardStatus,
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
