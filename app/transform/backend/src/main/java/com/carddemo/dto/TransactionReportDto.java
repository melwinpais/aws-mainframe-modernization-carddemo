package com.carddemo.dto;

import java.math.BigDecimal;
import java.util.List;

/**
 * DTO for Transaction Report
 * Contains aggregated transaction information for reporting purposes
 */
public class TransactionReportDto {
    private Integer totalTransactions;
    private BigDecimal totalAmount;
    private BigDecimal averageAmount;
    private BigDecimal maxAmount;
    private BigDecimal minAmount;
    private String startDate;
    private String endDate;
    private List<TransactionSummaryDto> transactions;

    public TransactionReportDto() {
    }

    public TransactionReportDto(Integer totalTransactions, BigDecimal totalAmount,
                               BigDecimal averageAmount, BigDecimal maxAmount, BigDecimal minAmount,
                               String startDate, String endDate, List<TransactionSummaryDto> transactions) {
        this.totalTransactions = totalTransactions;
        this.totalAmount = totalAmount;
        this.averageAmount = averageAmount;
        this.maxAmount = maxAmount;
        this.minAmount = minAmount;
        this.startDate = startDate;
        this.endDate = endDate;
        this.transactions = transactions;
    }

    // Getters and Setters
    public Integer getTotalTransactions() {
        return totalTransactions;
    }

    public void setTotalTransactions(Integer totalTransactions) {
        this.totalTransactions = totalTransactions;
    }

    public BigDecimal getTotalAmount() {
        return totalAmount;
    }

    public void setTotalAmount(BigDecimal totalAmount) {
        this.totalAmount = totalAmount;
    }

    public BigDecimal getAverageAmount() {
        return averageAmount;
    }

    public void setAverageAmount(BigDecimal averageAmount) {
        this.averageAmount = averageAmount;
    }

    public BigDecimal getMaxAmount() {
        return maxAmount;
    }

    public void setMaxAmount(BigDecimal maxAmount) {
        this.maxAmount = maxAmount;
    }

    public BigDecimal getMinAmount() {
        return minAmount;
    }

    public void setMinAmount(BigDecimal minAmount) {
        this.minAmount = minAmount;
    }

    public String getStartDate() {
        return startDate;
    }

    public void setStartDate(String startDate) {
        this.startDate = startDate;
    }

    public String getEndDate() {
        return endDate;
    }

    public void setEndDate(String endDate) {
        this.endDate = endDate;
    }

    public List<TransactionSummaryDto> getTransactions() {
        return transactions;
    }

    public void setTransactions(List<TransactionSummaryDto> transactions) {
        this.transactions = transactions;
    }

    /**
     * Inner DTO for individual transaction summary in report
     */
    public static class TransactionSummaryDto {
        private Long transactionId;
        private Long accountId;
        private Long cardNumber;
        private String transactionType;
        private BigDecimal amount;
        private String transactionDate;
        private String description;

        public TransactionSummaryDto() {
        }

        public TransactionSummaryDto(Long transactionId, Long accountId, Long cardNumber,
                                    String transactionType, BigDecimal amount,
                                    String transactionDate, String description) {
            this.transactionId = transactionId;
            this.accountId = accountId;
            this.cardNumber = cardNumber;
            this.transactionType = transactionType;
            this.amount = amount;
            this.transactionDate = transactionDate;
            this.description = description;
        }

        // Getters and Setters
        public Long getTransactionId() {
            return transactionId;
        }

        public void setTransactionId(Long transactionId) {
            this.transactionId = transactionId;
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

        public String getDescription() {
            return description;
        }

        public void setDescription(String description) {
            this.description = description;
        }
    }
}
