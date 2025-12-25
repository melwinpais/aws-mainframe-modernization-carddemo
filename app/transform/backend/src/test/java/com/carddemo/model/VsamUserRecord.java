package com.carddemo.model;

import java.util.Objects;

/**
 * VSAM User Record structure matching CSUSR01Y.cpy
 * SEC-USER-DATA
 */
public class VsamUserRecord {
    private String userId;               // PIC X(08)
    private String firstName;            // PIC X(20)
    private String lastName;             // PIC X(20)
    private String password;             // PIC X(08)
    private String userType;             // PIC X(01)

    public VsamUserRecord() {}

    public VsamUserRecord(String userId, String firstName, String lastName,
                         String password, String userType) {
        this.userId = userId;
        this.firstName = firstName;
        this.lastName = lastName;
        this.password = password;
        this.userType = userType;
    }

    // Getters and Setters
    public String getUserId() { return userId; }
    public void setUserId(String userId) { this.userId = userId; }

    public String getFirstName() { return firstName; }
    public void setFirstName(String firstName) { this.firstName = firstName; }

    public String getLastName() { return lastName; }
    public void setLastName(String lastName) { this.lastName = lastName; }

    public String getPassword() { return password; }
    public void setPassword(String password) { this.password = password; }

    public String getUserType() { return userType; }
    public void setUserType(String userType) { this.userType = userType; }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        VsamUserRecord that = (VsamUserRecord) o;
        return Objects.equals(userId, that.userId) &&
               Objects.equals(firstName, that.firstName) &&
               Objects.equals(lastName, that.lastName) &&
               Objects.equals(password, that.password) &&
               Objects.equals(userType, that.userType);
    }

    @Override
    public int hashCode() {
        return Objects.hash(userId, firstName, lastName, password, userType);
    }
}
