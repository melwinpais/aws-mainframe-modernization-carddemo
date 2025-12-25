package com.carddemo.migration.model;

import java.math.BigDecimal;

/**
 * Represents a VSAM Transaction record from the TRANSACT file.
 * Maps to the COBOL copybook structure for transaction records.
 */
public class VsamTransactionRecord {
    private Long accountId;                  // 11 digits
    private Long cardNumber;                 // 16 digits
    private String transactionType;          // Type code
    private String transactionCategory;      // Category code
    private BigDecimal amount;               // S9(10)V99
    private String transactionDate;          // 10 characters (YYYY-MM-DD)
    private String transactionTime;          // 8 characters (HH:MM:SS)
    private String description;              // Description text
    private String merchantName;             // Merchant name
    private String merchantCity;             // Merchant city
    private String merchantZip;              // Merchant zip code

    public VsamTransactionRecord() {
    }

    public VsamTransactionRecord(Long accountId, Long cardNumber, String transactionType,
                                 String transactionCategory, BigDecimal amount, String transactionDate,
                                 String transactionTime, String description, String merchantName,
                                 String merchantCity, String merchantZip) {
        this.accountId = accountId;
        this.cardNumber = cardNumber;
        this.transactionType = transactionType;
        this.transactionCategory = transactionCategory;
        this.amount = amount;
        this.transactionDate = transactionDate;
        this.transactionTime = transactionTime;
        this.description = description;
        this.merchantName = merchantName;
        this.merchantCity = merchantCity;
        this.merchantZip = merchantZip;
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
}
