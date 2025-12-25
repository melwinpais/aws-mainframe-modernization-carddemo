package com.carddemo.service;

import com.carddemo.config.AdminMenuConfig;
import com.carddemo.config.MenuConfig;
import com.carddemo.exception.AuthorizationException;
import com.carddemo.exception.ValidationException;
import com.carddemo.model.MenuOption;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * Implementation of MenuService for menu navigation operations.
 * Provides menu options based on user type and validates menu selections.
 */
@Service
public class MenuServiceImpl implements MenuService {

    private static final Logger logger = LoggerFactory.getLogger(MenuServiceImpl.class);

    private final MenuConfig menuConfig;
    private final AdminMenuConfig adminMenuConfig;

    public MenuServiceImpl(MenuConfig menuConfig, AdminMenuConfig adminMenuConfig) {
        this.menuConfig = menuConfig;
        this.adminMenuConfig = adminMenuConfig;
    }

    @Override
    public List<MenuOption> getMenuOptions(String userType) {
        logger.debug("Getting menu options for user type: {}", userType);
        
        if ("A".equals(userType)) {
            logger.debug("Returning admin menu options");
            return adminMenuConfig.getAdminMenuOptions();
        } else {
            logger.debug("Returning regular user menu options");
            return menuConfig.getMenuOptions();
        }
    }

    @Override
    public String validateAndRoute(int optionNumber, String userType) {
        logger.debug("Validating menu option {} for user type {}", optionNumber, userType);
        
        MenuOption selectedOption;
        int maxOptions;
        
        // Determine which menu to use based on user type
        if ("A".equals(userType)) {
            selectedOption = adminMenuConfig.getAdminMenuOption(optionNumber);
            maxOptions = adminMenuConfig.getAdminMenuOptionCount();
        } else {
            selectedOption = menuConfig.getMenuOption(optionNumber);
            maxOptions = menuConfig.getMenuOptionCount();
        }
        
        // Validate option number is in valid range
        if (optionNumber < 1 || optionNumber > maxOptions) {
            logger.warn("Invalid menu option number: {} (valid range: 1-{})", optionNumber, maxOptions);
            throw new ValidationException("Please enter a valid option number...");
        }
        
        // Check if option exists
        if (selectedOption == null) {
            logger.warn("Menu option {} not found", optionNumber);
            throw new ValidationException("Please enter a valid option number...");
        }
        
        // Check if user has permission for this option
        if ("A".equals(selectedOption.getUserType()) && !"A".equals(userType)) {
            logger.warn("User type {} attempted to access admin-only option {}", userType, optionNumber);
            throw new AuthorizationException("No access - Admin Only option... ");
        }
        
        logger.info("Menu option {} validated successfully, routing to program: {}", 
                    optionNumber, selectedOption.getProgramName());
        
        return selectedOption.getProgramName();
    }
}
