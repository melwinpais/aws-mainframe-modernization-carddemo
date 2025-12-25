package com.carddemo.model;

import java.util.Objects;

/**
 * VSAM Customer Record structure matching CVCUS01Y.cpy
 * RECLN 500
 */
public class VsamCustomerRecord {
    private Long customerId;              // PIC 9(09)
    private String firstName;             // PIC X(25)
    private String middleName;            // PIC X(25)
    private String lastName;              // PIC X(25)
    private String addressLine1;          // PIC X(50)
    private String addressLine2;          // PIC X(50)
    private String addressLine3;          // PIC X(50)
    private String stateCode;             // PIC X(02)
    private String countryCode;           // PIC X(03)
    private String zipCode;               // PIC X(10)
    private String phoneNumber1;          // PIC X(15)
    private String phoneNumber2;          // PIC X(15)
    private Long ssn;                     // PIC 9(09)
    private String governmentIssuedId;    // PIC X(20)
    private String dateOfBirth;           // PIC X(10)
    private String eftAccountId;          // PIC X(10)
    private String primaryCardholderInd;  // PIC X(01)
    private Integer ficoCreditScore;      // PIC 9(03)

    public VsamCustomerRecord() {}

    public VsamCustomerRecord(Long customerId, String firstName, String middleName, String lastName,
                             String addressLine1, String addressLine2, String addressLine3,
                             String stateCode, String countryCode, String zipCode,
                             String phoneNumber1, String phoneNumber2, Long ssn,
                             String governmentIssuedId, String dateOfBirth, String eftAccountId,
                             String primaryCardholderInd, Integer ficoCreditScore) {
        this.customerId = customerId;
        this.firstName = firstName;
        this.middleName = middleName;
        this.lastName = lastName;
        this.addressLine1 = addressLine1;
        this.addressLine2 = addressLine2;
        this.addressLine3 = addressLine3;
        this.stateCode = stateCode;
        this.countryCode = countryCode;
        this.zipCode = zipCode;
        this.phoneNumber1 = phoneNumber1;
        this.phoneNumber2 = phoneNumber2;
        this.ssn = ssn;
        this.governmentIssuedId = governmentIssuedId;
        this.dateOfBirth = dateOfBirth;
        this.eftAccountId = eftAccountId;
        this.primaryCardholderInd = primaryCardholderInd;
        this.ficoCreditScore = ficoCreditScore;
    }

    // Getters and Setters
    public Long getCustomerId() { return customerId; }
    public void setCustomerId(Long customerId) { this.customerId = customerId; }

    public String getFirstName() { return firstName; }
    public void setFirstName(String firstName) { this.firstName = firstName; }

    public String getMiddleName() { return middleName; }
    public void setMiddleName(String middleName) { this.middleName = middleName; }

    public String getLastName() { return lastName; }
    public void setLastName(String lastName) { this.lastName = lastName; }

    public String getAddressLine1() { return addressLine1; }
    public void setAddressLine1(String addressLine1) { this.addressLine1 = addressLine1; }

    public String getAddressLine2() { return addressLine2; }
    public void setAddressLine2(String addressLine2) { this.addressLine2 = addressLine2; }

    public String getAddressLine3() { return addressLine3; }
    public void setAddressLine3(String addressLine3) { this.addressLine3 = addressLine3; }

    public String getStateCode() { return stateCode; }
    public void setStateCode(String stateCode) { this.stateCode = stateCode; }

    public String getCountryCode() { return countryCode; }
    public void setCountryCode(String countryCode) { this.countryCode = countryCode; }

    public String getZipCode() { return zipCode; }
    public void setZipCode(String zipCode) { this.zipCode = zipCode; }

    public String getPhoneNumber1() { return phoneNumber1; }
    public void setPhoneNumber1(String phoneNumber1) { this.phoneNumber1 = phoneNumber1; }

    public String getPhoneNumber2() { return phoneNumber2; }
    public void setPhoneNumber2(String phoneNumber2) { this.phoneNumber2 = phoneNumber2; }

    public Long getSsn() { return ssn; }
    public void setSsn(Long ssn) { this.ssn = ssn; }

    public String getGovernmentIssuedId() { return governmentIssuedId; }
    public void setGovernmentIssuedId(String governmentIssuedId) { this.governmentIssuedId = governmentIssuedId; }

    public String getDateOfBirth() { return dateOfBirth; }
    public void setDateOfBirth(String dateOfBirth) { this.dateOfBirth = dateOfBirth; }

    public String getEftAccountId() { return eftAccountId; }
    public void setEftAccountId(String eftAccountId) { this.eftAccountId = eftAccountId; }

    public String getPrimaryCardholderInd() { return primaryCardholderInd; }
    public void setPrimaryCardholderInd(String primaryCardholderInd) { this.primaryCardholderInd = primaryCardholderInd; }

    public Integer getFicoCreditScore() { return ficoCreditScore; }
    public void setFicoCreditScore(Integer ficoCreditScore) { this.ficoCreditScore = ficoCreditScore; }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        VsamCustomerRecord that = (VsamCustomerRecord) o;
        return Objects.equals(customerId, that.customerId) &&
               Objects.equals(firstName, that.firstName) &&
               Objects.equals(middleName, that.middleName) &&
               Objects.equals(lastName, that.lastName) &&
               Objects.equals(addressLine1, that.addressLine1) &&
               Objects.equals(addressLine2, that.addressLine2) &&
               Objects.equals(addressLine3, that.addressLine3) &&
               Objects.equals(stateCode, that.stateCode) &&
               Objects.equals(countryCode, that.countryCode) &&
               Objects.equals(zipCode, that.zipCode) &&
               Objects.equals(phoneNumber1, that.phoneNumber1) &&
               Objects.equals(phoneNumber2, that.phoneNumber2) &&
               Objects.equals(ssn, that.ssn) &&
               Objects.equals(governmentIssuedId, that.governmentIssuedId) &&
               Objects.equals(dateOfBirth, that.dateOfBirth) &&
               Objects.equals(eftAccountId, that.eftAccountId) &&
               Objects.equals(primaryCardholderInd, that.primaryCardholderInd) &&
               Objects.equals(ficoCreditScore, that.ficoCreditScore);
    }

    @Override
    public int hashCode() {
        return Objects.hash(customerId, firstName, middleName, lastName, addressLine1, addressLine2,
                          addressLine3, stateCode, countryCode, zipCode, phoneNumber1, phoneNumber2,
                          ssn, governmentIssuedId, dateOfBirth, eftAccountId, primaryCardholderInd,
                          ficoCreditScore);
    }
}
