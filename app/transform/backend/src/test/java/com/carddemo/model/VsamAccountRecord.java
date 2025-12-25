package com.carddemo.model;

import java.math.BigDecimal;
import java.util.Objects;

/**
 * VSAM Account Record structure matching CVACT01Y.cpy
 * RECLN 300
 */
public class VsamAccountRecord {
    private Long accountId;              // PIC 9(11)
    private String activeStatus;         // PIC X(01)
    private BigDecimal currentBalance;   // PIC S9(10)V99
    private BigDecimal creditLimit;      // PIC S9(10)V99
    private BigDecimal cashCreditLimit;  // PIC S9(10)V99
    private String openDate;             // PIC X(10)
    private String expirationDate;       // PIC X(10)
    private String reissueDate;          // PIC X(10)
    private BigDecimal currentCycleCredit; // PIC S9(10)V99
    private BigDecimal currentCycleDebit;  // PIC S9(10)V99
    private String addressZip;           // PIC X(10)
    private String groupId;              // PIC X(10)

    public VsamAccountRecord() {}

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

    // Getters and Setters
    public Long getAccountId() { return accountId; }
    public void setAccountId(Long accountId) { this.accountId = accountId; }

    public String getActiveStatus() { return activeStatus; }
    public void setActiveStatus(String activeStatus) { this.activeStatus = activeStatus; }

    public BigDecimal getCurrentBalance() { return currentBalance; }
    public void setCurrentBalance(BigDecimal currentBalance) { this.currentBalance = currentBalance; }

    public BigDecimal getCreditLimit() { return creditLimit; }
    public void setCreditLimit(BigDecimal creditLimit) { this.creditLimit = creditLimit; }

    public BigDecimal getCashCreditLimit() { return cashCreditLimit; }
    public void setCashCreditLimit(BigDecimal cashCreditLimit) { this.cashCreditLimit = cashCreditLimit; }

    public String getOpenDate() { return openDate; }
    public void setOpenDate(String openDate) { this.openDate = openDate; }

    public String getExpirationDate() { return expirationDate; }
    public void setExpirationDate(String expirationDate) { this.expirationDate = expirationDate; }

    public String getReissueDate() { return reissueDate; }
    public void setReissueDate(String reissueDate) { this.reissueDate = reissueDate; }

    public BigDecimal getCurrentCycleCredit() { return currentCycleCredit; }
    public void setCurrentCycleCredit(BigDecimal currentCycleCredit) { this.currentCycleCredit = currentCycleCredit; }

    public BigDecimal getCurrentCycleDebit() { return currentCycleDebit; }
    public void setCurrentCycleDebit(BigDecimal currentCycleDebit) { this.currentCycleDebit = currentCycleDebit; }

    public String getAddressZip() { return addressZip; }
    public void setAddressZip(String addressZip) { this.addressZip = addressZip; }

    public String getGroupId() { return groupId; }
    public void setGroupId(String groupId) { this.groupId = groupId; }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        VsamAccountRecord that = (VsamAccountRecord) o;
        return Objects.equals(accountId, that.accountId) &&
               Objects.equals(activeStatus, that.activeStatus) &&
               Objects.equals(currentBalance, that.currentBalance) &&
               Objects.equals(creditLimit, that.creditLimit) &&
               Objects.equals(cashCreditLimit, that.cashCreditLimit) &&
               Objects.equals(openDate, that.openDate) &&
               Objects.equals(expirationDate, that.expirationDate) &&
               Objects.equals(reissueDate, that.reissueDate) &&
               Objects.equals(currentCycleCredit, that.currentCycleCredit) &&
               Objects.equals(currentCycleDebit, that.currentCycleDebit) &&
               Objects.equals(addressZip, that.addressZip) &&
               Objects.equals(groupId, that.groupId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(accountId, activeStatus, currentBalance, creditLimit, cashCreditLimit,
                          openDate, expirationDate, reissueDate, currentCycleCredit, currentCycleDebit,
                          addressZip, groupId);
    }
}
