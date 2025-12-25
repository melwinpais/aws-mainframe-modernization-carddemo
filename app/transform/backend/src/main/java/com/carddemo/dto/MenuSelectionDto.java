package com.carddemo.dto;

/**
 * Data Transfer Object for menu selection requests and responses.
 */
public class MenuSelectionDto {
    private int optionNumber;
    private String programName;
    private String route;

    public MenuSelectionDto() {
    }

    public MenuSelectionDto(int optionNumber, String programName, String route) {
        this.optionNumber = optionNumber;
        this.programName = programName;
        this.route = route;
    }

    public int getOptionNumber() {
        return optionNumber;
    }

    public void setOptionNumber(int optionNumber) {
        this.optionNumber = optionNumber;
    }

    public String getProgramName() {
        return programName;
    }

    public void setProgramName(String programName) {
        this.programName = programName;
    }

    public String getRoute() {
        return route;
    }

    public void setRoute(String route) {
        this.route = route;
    }
}
