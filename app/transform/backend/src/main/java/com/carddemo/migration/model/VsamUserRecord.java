package com.carddemo.migration.model;

/**
 * Represents a VSAM User record from the USRSEC file.
 * Maps to the COBOL copybook structure for user security records.
 */
public class VsamUserRecord {
    private String userId;        // 8 characters
    private String firstName;     // 20 characters
    private String lastName;      // 20 characters
    private String password;      // 8 characters
    private String userType;      // 1 character ('A' or 'U')

    public VsamUserRecord() {
    }

    public VsamUserRecord(String userId, String firstName, String lastName, String password, String userType) {
        this.userId = userId;
        this.firstName = firstName;
        this.lastName = lastName;
        this.password = password;
        this.userType = userType;
    }

    public String getUserId() {
        return userId;
    }

    public void setUserId(String userId) {
        this.userId = userId;
    }

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getUserType() {
        return userType;
    }

    public void setUserType(String userType) {
        this.userType = userType;
    }
}
