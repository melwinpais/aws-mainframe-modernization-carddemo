package com.carddemo.migration.model;

/**
 * Represents a VSAM Customer record from the CUSTDAT file.
 * Maps to the COBOL copybook structure for customer records.
 */
public class VsamCustomerRecord {
    private Long customerId;                 // 9 digits
    private String firstName;                // 25 characters
    private String middleName;               // 25 characters
    private String lastName;                 // 25 characters
    private String addressLine1;             // 50 characters
    private String addressLine2;             // 50 characters
    private String addressLine3;             // 50 characters
    private String stateCode;                // 2 characters
    private String countryCode;              // 3 characters
    private String zipCode;                  // 10 characters
    private String phoneNumber1;             // 15 characters
    private String phoneNumber2;             // 15 characters
    private Long ssn;                        // 9 digits
    private String governmentIssuedId;       // 20 characters
    private String dateOfBirth;              // 10 characters (YYYY-MM-DD)
    private String eftAccountId;             // 10 characters
    private String primaryCardholderInd;     // 1 character ('Y' or 'N')
    private Integer ficoCreditScore;         // 3 digits

    public VsamCustomerRecord() {
    }

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
}
