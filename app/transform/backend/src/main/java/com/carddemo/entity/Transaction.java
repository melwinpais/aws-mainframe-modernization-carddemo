package com.carddemo.entity;

import jakarta.persistence.*;
import jakarta.validation.constraints.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * JPA Entity for Transaction (Transaction_Record)
 * Maps to the transactions table in PostgreSQL
 * Corresponds to TRANSACT VSAM file in the original COBOL system
 */
@Entity
@Table(name = "transactions")
public class Transaction {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "transaction_id", nullable = false)
    private Long transactionId;

    @Column(name = "account_id", precision = 11, scale = 0, nullable = false)
    @NotNull(message = "Account ID is required")
    @Digits(integer = 11, fraction = 0, message = "Account ID must be 11 digits")
    private Long accountId;

    @Column(name = "card_number", precision = 16, scale = 0)
    @Digits(integer = 16, fraction = 0, message = "Card number must be 16 digits")
    private Long cardNumber;

    @Column(name = "transaction_type", length = 10, nullable = false)
    @NotBlank(message = "Transaction type is required")
    @Size(max = 10, message = "Transaction type must not exceed 10 characters")
    private String transactionType;

    @Column(name = "transaction_category", length = 20)
    @Size(max = 20, message = "Transaction category must not exceed 20 characters")
    private String transactionCategory;

    @Column(name = "amount", precision = 12, scale = 2, nullable = false)
    @NotNull(message = "Amount is required")
    @Digits(integer = 10, fraction = 2, message = "Amount must be in format S9(10)V99")
    private BigDecimal amount;

    @Column(name = "transaction_date", length = 10, nullable = false)
    @NotBlank(message = "Transaction date is required")
    @Pattern(regexp = "\\d{4}-\\d{2}-\\d{2}", message = "Transaction date must be in YYYY-MM-DD format")
    private String transactionDate;

    @Column(name = "transaction_time", length = 8)
    @Pattern(regexp = "^(\\d{2}:\\d{2}:\\d{2})?$", message = "Transaction time must be in HH:MM:SS format")
    private String transactionTime;

    @Column(name = "description", length = 100)
    @Size(max = 100, message = "Description must not exceed 100 characters")
    private String description;

    @Column(name = "merchant_name", length = 50)
    @Size(max = 50, message = "Merchant name must not exceed 50 characters")
    private String merchantName;

    @Column(name = "merchant_city", length = 30)
    @Size(max = 30, message = "Merchant city must not exceed 30 characters")
    private String merchantCity;

    @Column(name = "merchant_zip", length = 10)
    @Size(max = 10, message = "Merchant ZIP must not exceed 10 characters")
    private String merchantZip;

    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
    }

    // Constructors
    public Transaction() {
    }

    public Transaction(Long accountId, String transactionType, BigDecimal amount, String transactionDate) {
        this.accountId = accountId;
        this.transactionType = transactionType;
        this.amount = amount;
        this.transactionDate = transactionDate;
    }

    // Getters and Setters
    public Long getTransactionId() {
        return transactionId;
    }

    public void setTransactionId(Long transactionId) {
        this.transactionId = transactionId;
    }

    public Long getAccountId() {
        return accountId;
    }

    public void setAccountId(Long accountId) {
        this.accountId = accountId;
    }

    public Long getCardNumber() {
        return cardNumber;
    }

    public void setCardNumber(Long cardNumber) {
        this.cardNumber = cardNumber;
    }

    public String getTransactionType() {
        return transactionType;
    }

    public void setTransactionType(String transactionType) {
        this.transactionType = transactionType;
    }

    public String getTransactionCategory() {
        return transactionCategory;
    }

    public void setTransactionCategory(String transactionCategory) {
        this.transactionCategory = transactionCategory;
    }

    public BigDecimal getAmount() {
        return amount;
    }

    public void setAmount(BigDecimal amount) {
        this.amount = amount;
    }

    public String getTransactionDate() {
        return transactionDate;
    }

    public void setTransactionDate(String transactionDate) {
        this.transactionDate = transactionDate;
    }

    public String getTransactionTime() {
        return transactionTime;
    }

    public void setTransactionTime(String transactionTime) {
        this.transactionTime = transactionTime;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getMerchantName() {
        return merchantName;
    }

    public void setMerchantName(String merchantName) {
        this.merchantName = merchantName;
    }

    public String getMerchantCity() {
        return merchantCity;
    }

    public void setMerchantCity(String merchantCity) {
        this.merchantCity = merchantCity;
    }

    public String getMerchantZip() {
        return merchantZip;
    }

    public void setMerchantZip(String merchantZip) {
        this.merchantZip = merchantZip;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    @Override
    public String toString() {
        // Mask card number for security if present
        String maskedCardNumber = cardNumber != null ? 
            "************" + String.valueOf(cardNumber).substring(12) : null;
        
        return "Transaction{" +
                "transactionId=" + transactionId +
                ", accountId=" + accountId +
                ", cardNumber='" + maskedCardNumber + '\'' +
                ", transactionType='" + transactionType + '\'' +
                ", transactionCategory='" + transactionCategory + '\'' +
                ", amount=" + amount +
                ", transactionDate='" + transactionDate + '\'' +
                ", transactionTime='" + transactionTime + '\'' +
                ", description='" + description + '\'' +
                ", merchantName='" + merchantName + '\'' +
                ", merchantCity='" + merchantCity + '\'' +
                ", merchantZip='" + merchantZip + '\'' +
                ", createdAt=" + createdAt +
                '}';
    }
}
