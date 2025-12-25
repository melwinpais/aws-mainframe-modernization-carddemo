package com.carddemo.dto;

import java.math.BigDecimal;

/**
 * Data Transfer Object for Account update requests
 * Contains fields that can be updated for an account
 */
public class AccountUpdateDto {
    private String activeStatus;
    private BigDecimal creditLimit;
    private BigDecimal cashCreditLimit;
    private BigDecimal currentBalance;
    private BigDecimal currentCycleCredit;
    private BigDecimal currentCycleDebit;
    private CustomerUpdateDto customer;

    // Constructors
    public AccountUpdateDto() {
    }

    public AccountUpdateDto(String activeStatus, BigDecimal creditLimit, BigDecimal cashCreditLimit,
                            BigDecimal currentBalance, BigDecimal currentCycleCredit, BigDecimal currentCycleDebit) {
        this.activeStatus = activeStatus;
        this.creditLimit = creditLimit;
        this.cashCreditLimit = cashCreditLimit;
        this.currentBalance = currentBalance;
        this.currentCycleCredit = currentCycleCredit;
        this.currentCycleDebit = currentCycleDebit;
    }

    // Getters and Setters
    public String getActiveStatus() {
        return activeStatus;
    }

    public void setActiveStatus(String activeStatus) {
        this.activeStatus = activeStatus;
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

    public BigDecimal getCurrentBalance() {
        return currentBalance;
    }

    public void setCurrentBalance(BigDecimal currentBalance) {
        this.currentBalance = currentBalance;
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

    public CustomerUpdateDto getCustomer() {
        return customer;
    }

    public void setCustomer(CustomerUpdateDto customer) {
        this.customer = customer;
    }
}
