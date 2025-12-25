package com.carddemo.model;

/**
 * Represents a menu option in the CardDemo application.
 * Maps to COBOL menu option structures from COMEN02Y and COADM02Y copybooks.
 */
public class MenuOption {
    private int optionNumber;
    private String optionName;
    private String programName;
    private String userType;  // 'A' for Admin, 'U' for User, 'B' for Both

    public MenuOption() {
    }

    public MenuOption(int optionNumber, String optionName, String programName, String userType) {
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

    @Override
    public String toString() {
        return "MenuOption{" +
                "optionNumber=" + optionNumber +
                ", optionName='" + optionName + '\'' +
                ", programName='" + programName + '\'' +
                ", userType='" + userType + '\'' +
                '}';
    }
}
