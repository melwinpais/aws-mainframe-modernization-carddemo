package com.carddemo.entity;

import jakarta.persistence.*;
import jakarta.validation.constraints.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * JPA Entity for Account (Account_Record)
 * Maps to the accounts table in PostgreSQL
 * Corresponds to ACCTDAT VSAM file in the original COBOL system
 */
@Entity
@Table(name = "accounts")
public class Account {

    @Id
    @Column(name = "account_id", precision = 11, scale = 0, nullable = false)
    @NotNull(message = "Account ID is required")
    @Digits(integer = 11, fraction = 0, message = "Account ID must be 11 digits")
    private Long accountId;

    @Column(name = "customer_id", precision = 9, scale = 0, nullable = false)
    @NotNull(message = "Customer ID is required")
    @Digits(integer = 9, fraction = 0, message = "Customer ID must be 9 digits")
    private Long customerId;

    @Column(name = "active_status", length = 1, nullable = false)
    @NotBlank(message = "Active status is required")
    @Pattern(regexp = "[YN]", message = "Active status must be 'Y' or 'N'")
    private String activeStatus;

    @Column(name = "current_balance", precision = 12, scale = 2, nullable = false)
    @NotNull(message = "Current balance is required")
    @Digits(integer = 10, fraction = 2, message = "Current balance must be in format S9(10)V99")
    private BigDecimal currentBalance;

    @Column(name = "credit_limit", precision = 12, scale = 2, nullable = false)
    @NotNull(message = "Credit limit is required")
    @Digits(integer = 10, fraction = 2, message = "Credit limit must be in format S9(10)V99")
    private BigDecimal creditLimit;

    @Column(name = "cash_credit_limit", precision = 12, scale = 2, nullable = false)
    @NotNull(message = "Cash credit limit is required")
    @Digits(integer = 10, fraction = 2, message = "Cash credit limit must be in format S9(10)V99")
    private BigDecimal cashCreditLimit;

    @Column(name = "open_date", length = 10, nullable = false)
    @NotBlank(message = "Open date is required")
    @Pattern(regexp = "\\d{4}-\\d{2}-\\d{2}", message = "Open date must be in YYYY-MM-DD format")
    private String openDate;

    @Column(name = "expiration_date", length = 10)
    @Pattern(regexp = "\\d{4}-\\d{2}-\\d{2}", message = "Expiration date must be in YYYY-MM-DD format")
    private String expirationDate;

    @Column(name = "reissue_date", length = 10)
    @Pattern(regexp = "\\d{4}-\\d{2}-\\d{2}", message = "Reissue date must be in YYYY-MM-DD format")
    private String reissueDate;

    @Column(name = "current_cycle_credit", precision = 12, scale = 2)
    @Digits(integer = 10, fraction = 2, message = "Current cycle credit must be in format S9(10)V99")
    private BigDecimal currentCycleCredit;

    @Column(name = "current_cycle_debit", precision = 12, scale = 2)
    @Digits(integer = 10, fraction = 2, message = "Current cycle debit must be in format S9(10)V99")
    private BigDecimal currentCycleDebit;

    @Column(name = "address_zip", length = 10)
    @Size(max = 10, message = "Address ZIP must not exceed 10 characters")
    private String addressZip;

    @Column(name = "group_id", length = 10)
    @Size(max = 10, message = "Group ID must not exceed 10 characters")
    private String groupId;

    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
        updatedAt = LocalDateTime.now();
        if (currentBalance == null) {
            currentBalance = BigDecimal.ZERO;
        }
        if (currentCycleCredit == null) {
            currentCycleCredit = BigDecimal.ZERO;
        }
        if (currentCycleDebit == null) {
            currentCycleDebit = BigDecimal.ZERO;
        }
    }

    @PreUpdate
    protected void onUpdate() {
        updatedAt = LocalDateTime.now();
    }

    // Constructors
    public Account() {
    }

    public Account(Long accountId, String activeStatus, BigDecimal currentBalance, 
                   BigDecimal creditLimit, BigDecimal cashCreditLimit, String openDate) {
        this.accountId = accountId;
        this.activeStatus = activeStatus;
        this.currentBalance = currentBalance;
        this.creditLimit = creditLimit;
        this.cashCreditLimit = cashCreditLimit;
        this.openDate = openDate;
    }

    // Getters and Setters
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

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    public LocalDateTime getUpdatedAt() {
        return updatedAt;
    }

    public void setUpdatedAt(LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }

    @Override
    public String toString() {
        return "Account{" +
                "accountId=" + accountId +
                ", activeStatus='" + activeStatus + '\'' +
                ", currentBalance=" + currentBalance +
                ", creditLimit=" + creditLimit +
                ", cashCreditLimit=" + cashCreditLimit +
                ", openDate='" + openDate + '\'' +
                ", expirationDate='" + expirationDate + '\'' +
                ", reissueDate='" + reissueDate + '\'' +
                ", currentCycleCredit=" + currentCycleCredit +
                ", currentCycleDebit=" + currentCycleDebit +
                ", addressZip='" + addressZip + '\'' +
                ", groupId='" + groupId + '\'' +
                ", createdAt=" + createdAt +
                ", updatedAt=" + updatedAt +
                '}';
    }
}
