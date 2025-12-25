package com.carddemo.dto;

import java.math.BigDecimal;

public class BillPaymentDto {
    private Long accountId;
    private BigDecimal amount;
    private String paymentDate;

    public BillPaymentDto() {
    }

    public BillPaymentDto(Long accountId, BigDecimal amount, String paymentDate) {
        this.accountId = accountId;
        this.amount = amount;
        this.paymentDate = paymentDate;
    }

    public Long getAccountId() {
        return accountId;
    }

    public void setAccountId(Long accountId) {
        this.accountId = accountId;
    }

    public BigDecimal getAmount() {
        return amount;
    }

    public void setAmount(BigDecimal amount) {
        this.amount = amount;
    }

    public String getPaymentDate() {
        return paymentDate;
    }

    public void setPaymentDate(String paymentDate) {
        this.paymentDate = paymentDate;
    }
}
