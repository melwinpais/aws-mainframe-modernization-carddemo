package com.carddemo.model;

import java.math.BigDecimal;
import java.util.Objects;

/**
 * VSAM Transaction Record structure matching CVTRA05Y.cpy
 * RECLN 350
 */
public class VsamTransactionRecord {
    private String transactionId;        // PIC X(16)
    private String transactionTypeCode;  // PIC X(02)
    private Integer transactionCatCode;  // PIC 9(04)
    private String transactionSource;    // PIC X(10)
    private String description;          // PIC X(100)
    private BigDecimal amount;           // PIC S9(09)V99
    private Long merchantId;             // PIC 9(09)
    private String merchantName;         // PIC X(50)
    private String merchantCity;         // PIC X(50)
    private String merchantZip;          // PIC X(10)
    private String cardNumber;           // PIC X(16)
    private String originalTimestamp;    // PIC X(26)
    private String processedTimestamp;   // PIC X(26)

    public VsamTransactionRecord() {}

    public VsamTransactionRecord(String transactionId, String transactionTypeCode,
                                Integer transactionCatCode, String transactionSource,
                                String description, BigDecimal amount, Long merchantId,
                                String merchantName, String merchantCity, String merchantZip,
                                String cardNumber, String originalTimestamp, String processedTimestamp) {
        this.transactionId = transactionId;
        this.transactionTypeCode = transactionTypeCode;
        this.transactionCatCode = transactionCatCode;
        this.transactionSource = transactionSource;
        this.description = description;
        this.amount = amount;
        this.merchantId = merchantId;
        this.merchantName = merchantName;
        this.merchantCity = merchantCity;
        this.merchantZip = merchantZip;
        this.cardNumber = cardNumber;
        this.originalTimestamp = originalTimestamp;
        this.processedTimestamp = processedTimestamp;
    }

    // Getters and Setters
    public String getTransactionId() { return transactionId; }
    public void setTransactionId(String transactionId) { this.transactionId = transactionId; }

    public String getTransactionTypeCode() { return transactionTypeCode; }
    public void setTransactionTypeCode(String transactionTypeCode) { this.transactionTypeCode = transactionTypeCode; }

    public Integer getTransactionCatCode() { return transactionCatCode; }
    public void setTransactionCatCode(Integer transactionCatCode) { this.transactionCatCode = transactionCatCode; }

    public String getTransactionSource() { return transactionSource; }
    public void setTransactionSource(String transactionSource) { this.transactionSource = transactionSource; }

    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }

    public BigDecimal getAmount() { return amount; }
    public void setAmount(BigDecimal amount) { this.amount = amount; }

    public Long getMerchantId() { return merchantId; }
    public void setMerchantId(Long merchantId) { this.merchantId = merchantId; }

    public String getMerchantName() { return merchantName; }
    public void setMerchantName(String merchantName) { this.merchantName = merchantName; }

    public String getMerchantCity() { return merchantCity; }
    public void setMerchantCity(String merchantCity) { this.merchantCity = merchantCity; }

    public String getMerchantZip() { return merchantZip; }
    public void setMerchantZip(String merchantZip) { this.merchantZip = merchantZip; }

    public String getCardNumber() { return cardNumber; }
    public void setCardNumber(String cardNumber) { this.cardNumber = cardNumber; }

    public String getOriginalTimestamp() { return originalTimestamp; }
    public void setOriginalTimestamp(String originalTimestamp) { this.originalTimestamp = originalTimestamp; }

    public String getProcessedTimestamp() { return processedTimestamp; }
    public void setProcessedTimestamp(String processedTimestamp) { this.processedTimestamp = processedTimestamp; }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        VsamTransactionRecord that = (VsamTransactionRecord) o;
        return Objects.equals(transactionId, that.transactionId) &&
               Objects.equals(transactionTypeCode, that.transactionTypeCode) &&
               Objects.equals(transactionCatCode, that.transactionCatCode) &&
               Objects.equals(transactionSource, that.transactionSource) &&
               Objects.equals(description, that.description) &&
               Objects.equals(amount, that.amount) &&
               Objects.equals(merchantId, that.merchantId) &&
               Objects.equals(merchantName, that.merchantName) &&
               Objects.equals(merchantCity, that.merchantCity) &&
               Objects.equals(merchantZip, that.merchantZip) &&
               Objects.equals(cardNumber, that.cardNumber) &&
               Objects.equals(originalTimestamp, that.originalTimestamp) &&
               Objects.equals(processedTimestamp, that.processedTimestamp);
    }

    @Override
    public int hashCode() {
        return Objects.hash(transactionId, transactionTypeCode, transactionCatCode, transactionSource,
                          description, amount, merchantId, merchantName, merchantCity, merchantZip,
                          cardNumber, originalTimestamp, processedTimestamp);
    }
}
