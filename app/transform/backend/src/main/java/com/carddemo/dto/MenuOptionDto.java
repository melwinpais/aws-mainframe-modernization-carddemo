package com.carddemo.dto;

/**
 * Data Transfer Object for menu options.
 * Used in API responses to provide menu option information to the frontend.
 */
public class MenuOptionDto {
    private int optionNumber;
    private String optionName;
    private String programName;
    private String userType;

    public MenuOptionDto() {
    }

    public MenuOptionDto(int optionNumber, String optionName, String programName, String userType) {
        this.optionNumber = optionNumber;
        this.optionName = optionName;
        this.programName = programName;
        this.userType = userType;
    }

    public int getOptionNumber() {
        return optionNumber;
    }

    public void setOptionNumber(int optionNumber) {
        this.optionNumber = optionNumber;
    }

    public String getOptionName() {
        return optionName;
    }

    public void setOptionName(String optionName) {
        this.optionName = optionName;
    }

    public String getProgramName() {
        return programName;
    }

    public void setProgramName(String programName) {
        this.programName = programName;
    }

    public String getUserType() {
        return userType;
    }

    public void setUserType(String userType) {
        this.userType = userType;
    }
}
