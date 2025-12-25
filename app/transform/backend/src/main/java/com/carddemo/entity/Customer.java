package com.carddemo.entity;

import jakarta.persistence.*;
import jakarta.validation.constraints.*;
import java.time.LocalDateTime;

/**
 * JPA Entity for Customer (Customer_Record)
 * Maps to the customers table in PostgreSQL
 * Corresponds to CUSTDAT VSAM file in the original COBOL system
 */
@Entity
@Table(name = "customers")
public class Customer {

    @Id
    @Column(name = "customer_id", precision = 9, scale = 0, nullable = false)
    @NotNull(message = "Customer ID is required")
    @Digits(integer = 9, fraction = 0, message = "Customer ID must be 9 digits")
    private Long customerId;

    @Column(name = "first_name", length = 25, nullable = false)
    @NotBlank(message = "First name is required")
    @Size(max = 25, message = "First name must not exceed 25 characters")
    private String firstName;

    @Column(name = "middle_name", length = 25)
    @Size(max = 25, message = "Middle name must not exceed 25 characters")
    private String middleName;

    @Column(name = "last_name", length = 25, nullable = false)
    @NotBlank(message = "Last name is required")
    @Size(max = 25, message = "Last name must not exceed 25 characters")
    private String lastName;

    @Column(name = "address_line_1", length = 50)
    @Size(max = 50, message = "Address line 1 must not exceed 50 characters")
    private String addressLine1;

    @Column(name = "address_line_2", length = 50)
    @Size(max = 50, message = "Address line 2 must not exceed 50 characters")
    private String addressLine2;

    @Column(name = "address_line_3", length = 50)
    @Size(max = 50, message = "Address line 3 must not exceed 50 characters")
    private String addressLine3;

    @Column(name = "state_code", length = 2)
    @Size(max = 2, message = "State code must be 2 characters")
    private String stateCode;

    @Column(name = "country_code", length = 3)
    @Size(max = 3, message = "Country code must be 3 characters")
    private String countryCode;

    @Column(name = "zip_code", length = 10)
    @Size(max = 10, message = "ZIP code must not exceed 10 characters")
    private String zipCode;

    @Column(name = "phone_number_1", length = 15)
    @Size(max = 15, message = "Phone number 1 must not exceed 15 characters")
    @Pattern(regexp = "^(\\(\\d{3}\\)\\d{3}-\\d{4})?$", message = "Phone number must be in format (XXX)XXX-XXXX")
    private String phoneNumber1;

    @Column(name = "phone_number_2", length = 15)
    @Size(max = 15, message = "Phone number 2 must not exceed 15 characters")
    @Pattern(regexp = "^(\\(\\d{3}\\)\\d{3}-\\d{4})?$", message = "Phone number must be in format (XXX)XXX-XXXX")
    private String phoneNumber2;

    @Column(name = "ssn", precision = 9, scale = 0)
    @Digits(integer = 9, fraction = 0, message = "SSN must be 9 digits")
    private Long ssn;

    @Column(name = "government_issued_id", length = 20)
    @Size(max = 20, message = "Government issued ID must not exceed 20 characters")
    private String governmentIssuedId;

    @Column(name = "date_of_birth", length = 10)
    @Pattern(regexp = "^(\\d{4}-\\d{2}-\\d{2})?$", message = "Date of birth must be in YYYY-MM-DD format")
    private String dateOfBirth;

    @Column(name = "eft_account_id", length = 10)
    @Size(max = 10, message = "EFT account ID must not exceed 10 characters")
    private String eftAccountId;

    @Column(name = "primary_cardholder_ind", length = 1)
    @Pattern(regexp = "^[YN]?$", message = "Primary cardholder indicator must be 'Y' or 'N'")
    private String primaryCardholderInd;

    @Column(name = "fico_credit_score", precision = 3, scale = 0)
    @Digits(integer = 3, fraction = 0, message = "FICO credit score must be 3 digits")
    @Min(value = 300, message = "FICO credit score must be at least 300")
    @Max(value = 850, message = "FICO credit score must not exceed 850")
    private Integer ficoCreditScore;

    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
        updatedAt = LocalDateTime.now();
    }

    @PreUpdate
    protected void onUpdate() {
        updatedAt = LocalDateTime.now();
    }

    // Constructors
    public Customer() {
    }

    public Customer(Long customerId, String firstName, String lastName) {
        this.customerId = customerId;
        this.firstName = firstName;
        this.lastName = lastName;
    }

    // Getters and Setters
    public Long getCustomerId() {
        return customerId;
    }

    public void setCustomerId(Long customerId) {
        this.customerId = customerId;
    }

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getMiddleName() {
        return middleName;
    }

    public void setMiddleName(String middleName) {
        this.middleName = middleName;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    public String getAddressLine1() {
        return addressLine1;
    }

    public void setAddressLine1(String addressLine1) {
        this.addressLine1 = addressLine1;
    }

    public String getAddressLine2() {
        return addressLine2;
    }

    public void setAddressLine2(String addressLine2) {
        this.addressLine2 = addressLine2;
    }

    public String getAddressLine3() {
        return addressLine3;
    }

    public void setAddressLine3(String addressLine3) {
        this.addressLine3 = addressLine3;
    }

    public String getStateCode() {
        return stateCode;
    }

    public void setStateCode(String stateCode) {
        this.stateCode = stateCode;
    }

    public String getCountryCode() {
        return countryCode;
    }

    public void setCountryCode(String countryCode) {
        this.countryCode = countryCode;
    }

    public String getZipCode() {
        return zipCode;
    }

    public void setZipCode(String zipCode) {
        this.zipCode = zipCode;
    }

    public String getPhoneNumber1() {
        return phoneNumber1;
    }

    public void setPhoneNumber1(String phoneNumber1) {
        this.phoneNumber1 = phoneNumber1;
    }

    public String getPhoneNumber2() {
        return phoneNumber2;
    }

    public void setPhoneNumber2(String phoneNumber2) {
        this.phoneNumber2 = phoneNumber2;
    }

    public Long getSsn() {
        return ssn;
    }

    public void setSsn(Long ssn) {
        this.ssn = ssn;
    }

    public String getGovernmentIssuedId() {
        return governmentIssuedId;
    }

    public void setGovernmentIssuedId(String governmentIssuedId) {
        this.governmentIssuedId = governmentIssuedId;
    }

    public String getDateOfBirth() {
        return dateOfBirth;
    }

    public void setDateOfBirth(String dateOfBirth) {
        this.dateOfBirth = dateOfBirth;
    }

    public String getEftAccountId() {
        return eftAccountId;
    }

    public void setEftAccountId(String eftAccountId) {
        this.eftAccountId = eftAccountId;
    }

    public String getPrimaryCardholderInd() {
        return primaryCardholderInd;
    }

    public void setPrimaryCardholderInd(String primaryCardholderInd) {
        this.primaryCardholderInd = primaryCardholderInd;
    }

    public Integer getFicoCreditScore() {
        return ficoCreditScore;
    }

    public void setFicoCreditScore(Integer ficoCreditScore) {
        this.ficoCreditScore = ficoCreditScore;
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
        return "Customer{" +
                "customerId=" + customerId +
                ", firstName='" + firstName + '\'' +
                ", middleName='" + middleName + '\'' +
                ", lastName='" + lastName + '\'' +
                ", addressLine1='" + addressLine1 + '\'' +
                ", addressLine2='" + addressLine2 + '\'' +
                ", addressLine3='" + addressLine3 + '\'' +
                ", stateCode='" + stateCode + '\'' +
                ", countryCode='" + countryCode + '\'' +
                ", zipCode='" + zipCode + '\'' +
                ", phoneNumber1='" + phoneNumber1 + '\'' +
                ", phoneNumber2='" + phoneNumber2 + '\'' +
                ", ssn=" + (ssn != null ? "***-**-" + String.valueOf(ssn).substring(5) : null) +
                ", governmentIssuedId='" + governmentIssuedId + '\'' +
                ", dateOfBirth='" + dateOfBirth + '\'' +
                ", eftAccountId='" + eftAccountId + '\'' +
                ", primaryCardholderInd='" + primaryCardholderInd + '\'' +
                ", ficoCreditScore=" + ficoCreditScore +
                ", createdAt=" + createdAt +
                ", updatedAt=" + updatedAt +
                '}';
    }
}
