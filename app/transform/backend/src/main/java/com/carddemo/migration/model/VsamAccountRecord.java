package com.carddemo.migration.model;

import java.math.BigDecimal;

/**
 * Represents a VSAM Account record from the ACCTDAT file.
 * Maps to the COBOL copybook structure for account records.
 */
public class VsamAccountRecord {
    private Long accountId;                  // 11 digits
    private String activeStatus;             // 1 character ('Y' or 'N')
    private BigDecimal currentBalance;       // S9(10)V99
    private BigDecimal creditLimit;          // S9(10)V99
    private BigDecimal cashCreditLimit;      // S9(10)V99
    private String openDate;                 // 10 characters (YYYY-MM-DD)
    private String expirationDate;           // 10 characters (YYYY-MM-DD)
    private String reissueDate;              // 10 characters (YYYY-MM-DD)
    private BigDecimal currentCycleCredit;   // S9(10)V99
    private BigDecimal currentCycleDebit;    // S9(10)V99
    private String addressZip;               // 10 characters
    private String groupId;                  // 10 characters

    public VsamAccountRecord() {
    }

    public VsamAccountRecord(Long accountId, String activeStatus, BigDecimal currentBalance,
                             BigDecimal creditLimit, BigDecimal cashCreditLimit, String openDate,
                             String expirationDate, String reissueDate, BigDecimal currentCycleCredit,
                             BigDecimal currentCycleDebit, String addressZip, String groupId) {
        this.accountId = accountId;
        this.activeStatus = activeStatus;
        this.currentBalance = currentBalance;
        this.creditLimit = creditLimit;
        this.cashCreditLimit = cashCreditLimit;
        this.openDate = openDate;
        this.expirationDate = expirationDate;
        this.reissueDate = reissueDate;
        this.currentCycleCredit = currentCycleCredit;
        this.currentCycleDebit = currentCycleDebit;
        this.addressZip = addressZip;
        this.groupId = groupId;
    }

    public Long getAccountId() {
        return accountId;
    }

    public void setAccountId(Long accountId) {
        this.accountId = accountId;
    }

    public String getActiveStatus() {
        return activeStatus;
    }

    public void setActiveStatus(String activeStatus) {
        this.activeStatus = activeStatus;
    }

    public BigDecimal getCurrentBalance() {
        return currentBalance;
    }

    public void setCurrentBalance(BigDecimal currentBalance) {
        this.currentBalance = currentBalance;
    }

    public BigDecimal getCreditLimit() {
        return creditLimit;
    }

    public void setCreditLimit(BigDecimal creditLimit) {
        this.creditLimit = creditLimit;
    }

    public BigDecimal getCashCreditLimit() {
        return cashCreditLimit;
    }

    public void setCashCreditLimit(BigDecimal cashCreditLimit) {
        this.cashCreditLimit = cashCreditLimit;
    }

    public String getOpenDate() {
        return openDate;
    }

    public void setOpenDate(String openDate) {
        this.openDate = openDate;
    }

    public String getExpirationDate() {
        return expirationDate;
    }

    public void setExpirationDate(String expirationDate) {
        this.expirationDate = expirationDate;
    }

    public String getReissueDate() {
        return reissueDate;
    }

    public void setReissueDate(String reissueDate) {
        this.reissueDate = reissueDate;
    }

    public BigDecimal getCurrentCycleCredit() {
        return currentCycleCredit;
    }

    public void setCurrentCycleCredit(BigDecimal currentCycleCredit) {
        this.currentCycleCredit = currentCycleCredit;
    }

    public BigDecimal getCurrentCycleDebit() {
        return currentCycleDebit;
    }

    public void setCurrentCycleDebit(BigDecimal currentCycleDebit) {
        this.currentCycleDebit = currentCycleDebit;
    }

    public String getAddressZip() {
        return addressZip;
    }

    public void setAddressZip(String addressZip) {
        this.addressZip = addressZip;
    }

    public String getGroupId() {
        return groupId;
    }

    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }
}
