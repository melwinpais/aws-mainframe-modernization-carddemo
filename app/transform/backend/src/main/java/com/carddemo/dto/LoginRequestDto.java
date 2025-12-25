package com.carddemo.dto;

import jakarta.validation.constraints.NotBlank;

/**
 * Login Request DTO
 * Contains user credentials for authentication
 */
public class LoginRequestDto {

    @NotBlank(message = "User ID is required")
    private String userId;

    @NotBlank(message = "Password is required")
    private String password;

    public LoginRequestDto() {
    }

    public LoginRequestDto(String userId, String password) {
        this.userId = userId;
        this.password = password;
    }

    public String getUserId() {
        return userId;
    }

    public void setUserId(String userId) {
        this.userId = userId;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    @Override
    public String toString() {
        return "LoginRequestDto{" +
                "userId='" + userId + '\'' +
                ", password='[PROTECTED]'" +
                '}';
    }
}
