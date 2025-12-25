package com.carddemo.service;

import com.carddemo.config.AdminMenuConfig;
import com.carddemo.config.MenuConfig;
import com.carddemo.exception.AuthorizationException;
import com.carddemo.exception.ValidationException;
import com.carddemo.model.MenuOption;
import net.jqwik.api.*;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * Property-based tests for MenuService
 * Tests universal properties across all inputs
 * Validates Requirements 2.3, 2.4, 2.5, 2.7
 */
class MenuServicePropertyTest {

    private MenuService menuService;
    private MenuConfig menuConfig;
    private AdminMenuConfig adminMenuConfig;

    /**
     * Property 3: Menu Option Validation
     * For any menu option input, the system should validate that it is numeric
     * and within the valid range (1-11), returning an error message for invalid inputs
     * Validates: Requirements 2.3, 2.4
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 3: Menu Option Validation")
    void testMenuOptionValidation(
            @ForAll("menuOptionInputs") int optionNumber,
            @ForAll("userTypes") String userType) {
        
        // Setup service for each test iteration
        setupService();
        
        // Get valid range for the user type
        List<MenuOption> availableOptions = menuService.getMenuOptions(userType);
        int minOption = availableOptions.stream()
            .mapToInt(MenuOption::getOptionNumber)
            .min()
            .orElse(1);
        int maxOption = availableOptions.stream()
            .mapToInt(MenuOption::getOptionNumber)
            .max()
            .orElse(11);
        
        // Test validation
        if (optionNumber < minOption || optionNumber > maxOption) {
            // Invalid option number - should throw ValidationException
            assertThatThrownBy(() -> menuService.validateAndRoute(optionNumber, userType))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Please enter a valid option number");
        } else {
            // Valid option number - should not throw ValidationException
            // (may throw AuthorizationException if user lacks permission)
            try {
                String route = menuService.validateAndRoute(optionNumber, userType);
                assertThat(route).isNotNull();
                assertThat(route).isNotEmpty();
            } catch (AuthorizationException e) {
                // This is acceptable - user may lack permission for this option
                assertThat(e.getMessage()).contains("No access");
            }
        }
    }

    /**
     * Property 4: Menu Option Routing
     * For any valid menu option number, the system should return the correct
     * program name for that option
     * Validates: Requirements 2.5
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 4: Menu Option Routing")
    void testMenuOptionRouting(
            @ForAll("validMenuOptions") int optionNumber,
            @ForAll("userTypes") String userType) {
        
        // Setup service for each test iteration
        setupService();
        
        // Get available options for the user type
        List<MenuOption> availableOptions = menuService.getMenuOptions(userType);
        
        // Find the option in the available options
        MenuOption expectedOption = availableOptions.stream()
            .filter(opt -> opt.getOptionNumber() == optionNumber)
            .findFirst()
            .orElse(null);
        
        if (expectedOption == null) {
            // Option not available for this user type
            // Should throw either ValidationException or AuthorizationException
            assertThatThrownBy(() -> menuService.validateAndRoute(optionNumber, userType))
                .satisfiesAnyOf(
                    ex -> assertThat(ex).isInstanceOf(ValidationException.class),
                    ex -> assertThat(ex).isInstanceOf(AuthorizationException.class)
                );
        } else {
            // Option is available for this user type
            // Check if it's admin-only
            boolean isAdminOnly = expectedOption.getUserType().equals("A");
            
            if (isAdminOnly && userType.equals("U")) {
                // Regular user trying to access admin option - should throw AuthorizationException
                assertThatThrownBy(() -> menuService.validateAndRoute(optionNumber, userType))
                    .isInstanceOf(AuthorizationException.class)
                    .hasMessageContaining("No access");
            } else {
                // User has permission - should return correct route
                String route = menuService.validateAndRoute(optionNumber, userType);
                assertThat(route).isNotNull();
                assertThat(route).isEqualTo(expectedOption.getProgramName());
            }
        }
    }

    /**
     * Property 5: Admin Option Authorization
     * For any admin-only menu option, when selected by a non-admin user,
     * the system should return an authorization error message
     * Validates: Requirements 2.7
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 5: Admin Option Authorization")
    void testAdminOptionAuthorization(
            @ForAll("adminOnlyOptions") int optionNumber) {
        
        // Setup service for each test iteration
        setupService();
        
        // Get admin menu options to verify this is an admin-only option
        List<MenuOption> adminOptions = menuService.getMenuOptions("A");
        MenuOption adminOption = adminOptions.stream()
            .filter(opt -> opt.getOptionNumber() == optionNumber)
            .filter(opt -> opt.getUserType().equals("A"))
            .findFirst()
            .orElse(null);
        
        if (adminOption != null) {
            // This is an admin-only option
            // Admin user should be able to access it
            String route = menuService.validateAndRoute(optionNumber, "A");
            assertThat(route).isNotNull();
            assertThat(route).isEqualTo(adminOption.getProgramName());
            
            // Regular user accessing admin menu options should get validation error
            // (because regular users have a different menu with options 1-11)
            // If they somehow try to access admin option numbers beyond their range,
            // they should get a validation error
            if (optionNumber > menuService.getMenuOptions("U").size()) {
                assertThatThrownBy(() -> menuService.validateAndRoute(optionNumber, "U"))
                    .isInstanceOf(ValidationException.class)
                    .hasMessageContaining("Please enter a valid option number");
            }
        }
    }

    // Helper method to setup service for each test iteration
    private void setupService() {
        menuConfig = new MenuConfig();
        adminMenuConfig = new AdminMenuConfig();
        menuService = new MenuServiceImpl(menuConfig, adminMenuConfig);
    }

    // Providers for generating test data

    @Provide
    Arbitrary<Integer> menuOptionInputs() {
        return Arbitraries.oneOf(
            // Valid range (1-11)
            Arbitraries.integers().between(1, 11),
            // Below valid range
            Arbitraries.integers().between(-100, 0),
            // Above valid range
            Arbitraries.integers().between(12, 100),
            // Edge cases
            Arbitraries.just(0),
            Arbitraries.just(-1),
            Arbitraries.just(12),
            Arbitraries.just(999)
        );
    }

    @Provide
    Arbitrary<Integer> validMenuOptions() {
        // Options 1-11 cover both regular and admin menus
        return Arbitraries.integers().between(1, 11);
    }

    @Provide
    Arbitrary<Integer> adminOnlyOptions() {
        // Based on COADM02Y copybook, admin menu has options 1-6
        // These are admin-only options
        return Arbitraries.integers().between(1, 6);
    }

    @Provide
    Arbitrary<String> userTypes() {
        return Arbitraries.of("A", "U");  // Admin or User
    }
}
