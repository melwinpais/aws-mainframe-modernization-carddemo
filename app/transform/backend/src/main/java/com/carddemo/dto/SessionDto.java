package com.carddemo.dto;

/**
 * Session DTO
 * Contains session validation information
 */
public class SessionDto {

    private boolean valid;
    private String userId;
    private String userType;

    public SessionDto() {
    }

    public SessionDto(boolean valid, String userId, String userType) {
        this.valid = valid;
        this.userId = userId;
        this.userType = userType;
    }

    public boolean isValid() {
        return valid;
    }

    public void setValid(boolean valid) {
        this.valid = valid;
    }

    public String getUserId() {
        return userId;
    }

    public void setUserId(String userId) {
        this.userId = userId;
    }

    public String getUserType() {
        return userType;
    }

    public void setUserType(String userType) {
        this.userType = userType;
    }

    @Override
    public String toString() {
        return "SessionDto{" +
                "valid=" + valid +
                ", userId='" + userId + '\'' +
                ", userType='" + userType + '\'' +
                '}';
    }
}
