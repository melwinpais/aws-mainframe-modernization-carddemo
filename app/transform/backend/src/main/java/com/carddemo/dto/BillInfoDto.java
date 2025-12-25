package com.carddemo.dto;

import java.math.BigDecimal;

public class BillInfoDto {
    private Long accountId;
    private BigDecimal currentBalance;
    private BigDecimal minimumPayment;
    private String dueDate;

    public BillInfoDto() {
    }

    public BillInfoDto(Long accountId, BigDecimal currentBalance, BigDecimal minimumPayment, String dueDate) {
        this.accountId = accountId;
        this.currentBalance = currentBalance;
        this.minimumPayment = minimumPayment;
        this.dueDate = dueDate;
    }

    public Long getAccountId() {
        return accountId;
    }

    public void setAccountId(Long accountId) {
        this.accountId = accountId;
    }

    public BigDecimal getCurrentBalance() {
        return currentBalance;
    }

    public void setCurrentBalance(BigDecimal currentBalance) {
        this.currentBalance = currentBalance;
    }

    public BigDecimal getMinimumPayment() {
        return minimumPayment;
    }

    public void setMinimumPayment(BigDecimal minimumPayment) {
        this.minimumPayment = minimumPayment;
    }

    public String getDueDate() {
        return dueDate;
    }

    public void setDueDate(String dueDate) {
        this.dueDate = dueDate;
    }
}
