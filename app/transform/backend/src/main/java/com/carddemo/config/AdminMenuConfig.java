package com.carddemo.config;

import com.carddemo.model.MenuOption;
import org.springframework.context.annotation.Configuration;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Configuration for admin user menu options.
 * Maps to COADM02Y copybook from the original COBOL application.
 */
@Configuration
public class AdminMenuConfig {

    private static final List<MenuOption> ADMIN_MENU_OPTIONS;

    static {
        List<MenuOption> options = new ArrayList<>();
        
        // Option 1: User List (Security)
        options.add(new MenuOption(
            1,
            "User List (Security)",
            "COUSR00C",
            "A"
        ));
        
        // Option 2: User Add (Security)
        options.add(new MenuOption(
            2,
            "User Add (Security)",
            "COUSR01C",
            "A"
        ));
        
        // Option 3: User Update (Security)
        options.add(new MenuOption(
            3,
            "User Update (Security)",
            "COUSR02C",
            "A"
        ));
        
        // Option 4: User Delete (Security)
        options.add(new MenuOption(
            4,
            "User Delete (Security)",
            "COUSR03C",
            "A"
        ));
        
        // Option 5: Transaction Type List/Update (Db2)
        options.add(new MenuOption(
            5,
            "Transaction Type List/Update (Db2)",
            "COTRTLIC",
            "A"
        ));
        
        // Option 6: Transaction Type Maintenance (Db2)
        options.add(new MenuOption(
            6,
            "Transaction Type Maintenance (Db2)",
            "COTRTUPC",
            "A"
        ));
        
        ADMIN_MENU_OPTIONS = Collections.unmodifiableList(options);
    }

    /**
     * Get all menu options for admin users.
     * 
     * @return Unmodifiable list of admin menu options
     */
    public List<MenuOption> getAdminMenuOptions() {
        return ADMIN_MENU_OPTIONS;
    }

    /**
     * Get the total count of admin menu options.
     * 
     * @return Number of admin menu options
     */
    public int getAdminMenuOptionCount() {
        return ADMIN_MENU_OPTIONS.size();
    }

    /**
     * Get a specific admin menu option by number.
     * 
     * @param optionNumber The option number (1-6)
     * @return The menu option, or null if not found
     */
    public MenuOption getAdminMenuOption(int optionNumber) {
        return ADMIN_MENU_OPTIONS.stream()
            .filter(option -> option.getOptionNumber() == optionNumber)
            .findFirst()
            .orElse(null);
    }
}
