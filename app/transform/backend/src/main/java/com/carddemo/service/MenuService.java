package com.carddemo.service;

import com.carddemo.model.MenuOption;

import java.util.List;

/**
 * Service interface for menu navigation operations.
 * Handles menu option retrieval and validation based on user type.
 */
public interface MenuService {

    /**
     * Get menu options filtered by user type.
     * Admin users see admin menu options, regular users see standard menu options.
     * 
     * @param userType The user type ('A' for Admin, 'U' for User)
     * @return List of menu options available to the user type
     */
    List<MenuOption> getMenuOptions(String userType);

    /**
     * Validate a menu option selection and return the route information.
     * Validates that:
     * - The option number is numeric and within valid range
     * - The user has permission to access the option
     * 
     * @param optionNumber The selected option number
     * @param userType The user type ('A' for Admin, 'U' for User)
     * @return The program name/route for the selected option
     * @throws IllegalArgumentException if option is invalid or user lacks permission
     */
    String validateAndRoute(int optionNumber, String userType);
}
