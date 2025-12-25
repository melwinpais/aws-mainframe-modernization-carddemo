package com.carddemo.entity;

import jakarta.persistence.*;
import jakarta.validation.constraints.*;
import java.time.LocalDateTime;

/**
 * JPA Entity for Card (Card_Record)
 * Maps to the cards table in PostgreSQL
 * Corresponds to CARDDAT VSAM file in the original COBOL system
 */
@Entity
@Table(name = "cards")
public class Card {

    @Id
    @Column(name = "card_number", precision = 16, scale = 0, nullable = false)
    @NotNull(message = "Card number is required")
    @Digits(integer = 16, fraction = 0, message = "Card number must be 16 digits")
    private Long cardNumber;

    @Column(name = "account_id", precision = 11, scale = 0, nullable = false)
    @NotNull(message = "Account ID is required")
    @Digits(integer = 11, fraction = 0, message = "Account ID must be 11 digits")
    private Long accountId;

    @Column(name = "customer_id", precision = 9, scale = 0, nullable = false)
    @NotNull(message = "Customer ID is required")
    @Digits(integer = 9, fraction = 0, message = "Customer ID must be 9 digits")
    private Long customerId;

    @Column(name = "card_status", length = 10, nullable = false)
    @NotBlank(message = "Card status is required")
    @Size(max = 10, message = "Card status must not exceed 10 characters")
    private String cardStatus;

    @Column(name = "expiration_date", length = 10)
    @Pattern(regexp = "^(\\d{4}-\\d{2}-\\d{2})?$", message = "Expiration date must be in YYYY-MM-DD format")
    private String expirationDate;

    @Column(name = "issue_date", length = 10)
    @Pattern(regexp = "^(\\d{4}-\\d{2}-\\d{2})?$", message = "Issue date must be in YYYY-MM-DD format")
    private String issueDate;

    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
        updatedAt = LocalDateTime.now();
    }

    @PreUpdate
    protected void onUpdate() {
        updatedAt = LocalDateTime.now();
    }

    // Constructors
    public Card() {
    }

    public Card(Long cardNumber, Long accountId, Long customerId, String cardStatus) {
        this.cardNumber = cardNumber;
        this.accountId = accountId;
        this.customerId = customerId;
        this.cardStatus = cardStatus;
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

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    public LocalDateTime getUpdatedAt() {
        return updatedAt;
    }

    public void setUpdatedAt(LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }

    @Override
    public String toString() {
        // Mask card number for security - show only last 4 digits
        String maskedCardNumber = cardNumber != null ? 
            "************" + String.valueOf(cardNumber).substring(12) : null;
        
        return "Card{" +
                "cardNumber='" + maskedCardNumber + '\'' +
                ", accountId=" + accountId +
                ", customerId=" + customerId +
                ", cardStatus='" + cardStatus + '\'' +
                ", expirationDate='" + expirationDate + '\'' +
                ", issueDate='" + issueDate + '\'' +
                ", createdAt=" + createdAt +
                ", updatedAt=" + updatedAt +
                '}';
    }
}
