package com.carddemo.model;

import java.util.Objects;

/**
 * VSAM Card Record structure matching CVACT02Y.cpy
 * RECLN 150
 */
public class VsamCardRecord {
    private String cardNumber;           // PIC X(16)
    private Long accountId;              // PIC 9(11)
    private Integer cvvCode;             // PIC 9(03)
    private String embossedName;         // PIC X(50)
    private String expirationDate;       // PIC X(10)
    private String activeStatus;         // PIC X(01)

    public VsamCardRecord() {}

    public VsamCardRecord(String cardNumber, Long accountId, Integer cvvCode,
                         String embossedName, String expirationDate, String activeStatus) {
        this.cardNumber = cardNumber;
        this.accountId = accountId;
        this.cvvCode = cvvCode;
        this.embossedName = embossedName;
        this.expirationDate = expirationDate;
        this.activeStatus = activeStatus;
    }

    // Getters and Setters
    public String getCardNumber() { return cardNumber; }
    public void setCardNumber(String cardNumber) { this.cardNumber = cardNumber; }

    public Long getAccountId() { return accountId; }
    public void setAccountId(Long accountId) { this.accountId = accountId; }

    public Integer getCvvCode() { return cvvCode; }
    public void setCvvCode(Integer cvvCode) { this.cvvCode = cvvCode; }

    public String getEmbossedName() { return embossedName; }
    public void setEmbossedName(String embossedName) { this.embossedName = embossedName; }

    public String getExpirationDate() { return expirationDate; }
    public void setExpirationDate(String expirationDate) { this.expirationDate = expirationDate; }

    public String getActiveStatus() { return activeStatus; }
    public void setActiveStatus(String activeStatus) { this.activeStatus = activeStatus; }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        VsamCardRecord that = (VsamCardRecord) o;
        return Objects.equals(cardNumber, that.cardNumber) &&
               Objects.equals(accountId, that.accountId) &&
               Objects.equals(cvvCode, that.cvvCode) &&
               Objects.equals(embossedName, that.embossedName) &&
               Objects.equals(expirationDate, that.expirationDate) &&
               Objects.equals(activeStatus, that.activeStatus);
    }

    @Override
    public int hashCode() {
        return Objects.hash(cardNumber, accountId, cvvCode, embossedName, expirationDate, activeStatus);
    }
}
