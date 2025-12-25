package com.carddemo.dto;

import java.math.BigDecimal;

/**
 * Data Transfer Object for creating a new transaction.
 */
public class TransactionCreateDto {
    
    private Long accountId;
    private Long cardNumber;
    private String transactionType;
    private String transactionCategory;
    private BigDecimal amount;
    private String transactionDate;
    private String transactionTime;
    private String description;
    private String merchantName;
    private String merchantCity;
    private String merchantZip;
    
    // Constructors
    public TransactionCreateDto() {
    }
    
    public TransactionCreateDto(Long accountId, Long cardNumber, String transactionType, 
                               String transactionCategory, BigDecimal amount, 
                               String transactionDate, String transactionTime,
                               String description, String merchantName, 
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
    
    // Getters and Setters
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
