package com.carddemo.config;

import com.carddemo.model.MenuOption;
import org.springframework.context.annotation.Configuration;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Configuration for regular user menu options.
 * Maps to COMEN02Y copybook from the original COBOL application.
 */
@Configuration
public class MenuConfig {

    private static final List<MenuOption> MENU_OPTIONS;

    static {
        List<MenuOption> options = new ArrayList<>();
        
        // Option 1: Account View
        options.add(new MenuOption(
            1,
            "Account View",
            "COACTVWC",
            "U"
        ));
        
        // Option 2: Account Update
        options.add(new MenuOption(
            2,
            "Account Update",
            "COACTUPC",
            "U"
        ));
        
        // Option 3: Credit Card List
        options.add(new MenuOption(
            3,
            "Credit Card List",
            "COCRDLIC",
            "U"
        ));
        
        // Option 4: Credit Card View
        options.add(new MenuOption(
            4,
            "Credit Card View",
            "COCRDSLC",
            "U"
        ));
        
        // Option 5: Credit Card Update
        options.add(new MenuOption(
            5,
            "Credit Card Update",
            "COCRDUPC",
            "U"
        ));
        
        // Option 6: Transaction List
        options.add(new MenuOption(
            6,
            "Transaction List",
            "COTRN00C",
            "U"
        ));
        
        // Option 7: Transaction View
        options.add(new MenuOption(
            7,
            "Transaction View",
            "COTRN01C",
            "U"
        ));
        
        // Option 8: Transaction Add
        options.add(new MenuOption(
            8,
            "Transaction Add",
            "COTRN02C",
            "U"
        ));
        
        // Option 9: Transaction Reports
        options.add(new MenuOption(
            9,
            "Transaction Reports",
            "CORPT00C",
            "U"
        ));
        
        // Option 10: Bill Payment
        options.add(new MenuOption(
            10,
            "Bill Payment",
            "COBIL00C",
            "U"
        ));
        
        // Option 11: Pending Authorization View
        options.add(new MenuOption(
            11,
            "Pending Authorization View",
            "COPAUS0C",
            "U"
        ));
        
        MENU_OPTIONS = Collections.unmodifiableList(options);
    }

    /**
     * Get all menu options for regular users.
     * 
     * @return Unmodifiable list of menu options
     */
    public List<MenuOption> getMenuOptions() {
        return MENU_OPTIONS;
    }

    /**
     * Get the total count of menu options.
     * 
     * @return Number of menu options
     */
    public int getMenuOptionCount() {
        return MENU_OPTIONS.size();
    }

    /**
     * Get a specific menu option by number.
     * 
     * @param optionNumber The option number (1-11)
     * @return The menu option, or null if not found
     */
    public MenuOption getMenuOption(int optionNumber) {
        return MENU_OPTIONS.stream()
            .filter(option -> option.getOptionNumber() == optionNumber)
            .findFirst()
            .orElse(null);
    }
}
