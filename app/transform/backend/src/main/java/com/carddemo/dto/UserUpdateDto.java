package com.carddemo.dto;

import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;

/**
 * DTO for updating an existing user
 * All fields are optional - only provided fields will be updated
 */
public class UserUpdateDto {
    
    @Size(max = 20, message = "First name must not exceed 20 characters")
    private String firstName;

    @Size(max = 20, message = "Last name must not exceed 20 characters")
    private String lastName;

    @Size(min = 8, max = 8, message = "Password must be exactly 8 characters")
    private String password;

    @Pattern(regexp = "[AU]", message = "User type must be 'A' (Admin) or 'U' (User)")
    private String userType;

    // Constructors
    public UserUpdateDto() {
    }

    public UserUpdateDto(String firstName, String lastName, String password, String userType) {
        this.firstName = firstName;
        this.lastName = lastName;
        this.password = password;
        this.userType = userType;
    }

    // Getters and Setters
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
