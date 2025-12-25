package com.carddemo.migration.model;

/**
 * Represents a VSAM Card record from the CARDDAT file.
 * Maps to the COBOL copybook structure for card records.
 */
public class VsamCardRecord {
    private Long cardNumber;                 // 16 digits
    private Long accountId;                  // 11 digits
    private Long customerId;                 // 9 digits
    private String cardStatus;               // Status code
    private String expirationDate;           // 10 characters (YYYY-MM-DD)
    private String issueDate;                // 10 characters (YYYY-MM-DD)

    public VsamCardRecord() {
    }

    public VsamCardRecord(Long cardNumber, Long accountId, Long customerId,
                          String cardStatus, String expirationDate, String issueDate) {
        this.cardNumber = cardNumber;
        this.accountId = accountId;
        this.customerId = customerId;
        this.cardStatus = cardStatus;
        this.expirationDate = expirationDate;
        this.issueDate = issueDate;
    }

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
