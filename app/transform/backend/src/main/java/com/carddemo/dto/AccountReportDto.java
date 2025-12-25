package com.carddemo.dto;

import java.math.BigDecimal;
import java.util.List;

/**
 * DTO for Account Report
 * Contains aggregated account information for reporting purposes
 */
public class AccountReportDto {
    private Integer totalAccounts;
    private Integer activeAccounts;
    private Integer inactiveAccounts;
    private BigDecimal totalBalance;
    private BigDecimal totalCreditLimit;
    private BigDecimal averageBalance;
    private BigDecimal averageCreditLimit;
    private List<AccountSummaryDto> accounts;

    public AccountReportDto() {
    }

    public AccountReportDto(Integer totalAccounts, Integer activeAccounts, Integer inactiveAccounts,
                           BigDecimal totalBalance, BigDecimal totalCreditLimit,
                           BigDecimal averageBalance, BigDecimal averageCreditLimit,
                           List<AccountSummaryDto> accounts) {
        this.totalAccounts = totalAccounts;
        this.activeAccounts = activeAccounts;
        this.inactiveAccounts = inactiveAccounts;
        this.totalBalance = totalBalance;
        this.totalCreditLimit = totalCreditLimit;
        this.averageBalance = averageBalance;
        this.averageCreditLimit = averageCreditLimit;
        this.accounts = accounts;
    }

    // Getters and Setters
    public Integer getTotalAccounts() {
        return totalAccounts;
    }

    public void setTotalAccounts(Integer totalAccounts) {
        this.totalAccounts = totalAccounts;
    }

    public Integer getActiveAccounts() {
        return activeAccounts;
    }

    public void setActiveAccounts(Integer activeAccounts) {
        this.activeAccounts = activeAccounts;
    }

    public Integer getInactiveAccounts() {
        return inactiveAccounts;
    }

    public void setInactiveAccounts(Integer inactiveAccounts) {
        this.inactiveAccounts = inactiveAccounts;
    }

    public BigDecimal getTotalBalance() {
        return totalBalance;
    }

    public void setTotalBalance(BigDecimal totalBalance) {
        this.totalBalance = totalBalance;
    }

    public BigDecimal getTotalCreditLimit() {
        return totalCreditLimit;
    }

    public void setTotalCreditLimit(BigDecimal totalCreditLimit) {
        this.totalCreditLimit = totalCreditLimit;
    }

    public BigDecimal getAverageBalance() {
        return averageBalance;
    }

    public void setAverageBalance(BigDecimal averageBalance) {
        this.averageBalance = averageBalance;
    }

    public BigDecimal getAverageCreditLimit() {
        return averageCreditLimit;
    }

    public void setAverageCreditLimit(BigDecimal averageCreditLimit) {
        this.averageCreditLimit = averageCreditLimit;
    }

    public List<AccountSummaryDto> getAccounts() {
        return accounts;
    }

    public void setAccounts(List<AccountSummaryDto> accounts) {
        this.accounts = accounts;
    }

    /**
     * Inner DTO for individual account summary in report
     */
    public static class AccountSummaryDto {
        private Long accountId;
        private String activeStatus;
        private BigDecimal currentBalance;
        private BigDecimal creditLimit;
        private String openDate;
        private String expirationDate;

        public AccountSummaryDto() {
        }

        public AccountSummaryDto(Long accountId, String activeStatus, BigDecimal currentBalance,
                                BigDecimal creditLimit, String openDate, String expirationDate) {
            this.accountId = accountId;
            this.activeStatus = activeStatus;
            this.currentBalance = currentBalance;
            this.creditLimit = creditLimit;
            this.openDate = openDate;
            this.expirationDate = expirationDate;
        }

        // Getters and Setters
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
    }
}
