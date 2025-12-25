package com.carddemo.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;

/**
 * DTO for creating a new user
 * Includes password field for initial user creation
 */
public class UserCreateDto {
    
    @NotBlank(message = "User ID is required")
    @Size(min = 1, max = 8, message = "User ID must be 1-8 characters")
    private String userId;

    @NotBlank(message = "First name is required")
    @Size(max = 20, message = "First name must not exceed 20 characters")
    private String firstName;

    @NotBlank(message = "Last name is required")
    @Size(max = 20, message = "Last name must not exceed 20 characters")
    private String lastName;

    @NotBlank(message = "Password is required")
    @Size(min = 8, max = 8, message = "Password must be exactly 8 characters")
    private String password;

    @NotBlank(message = "User type is required")
    @Pattern(regexp = "[AU]", message = "User type must be 'A' (Admin) or 'U' (User)")
    private String userType;

    // Constructors
    public UserCreateDto() {
    }

    public UserCreateDto(String userId, String firstName, String lastName, String password, String userType) {
        this.userId = userId;
        this.firstName = firstName;
        this.lastName = lastName;
        this.password = password;
        this.userType = userType;
    }

    // Getters and Setters
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
