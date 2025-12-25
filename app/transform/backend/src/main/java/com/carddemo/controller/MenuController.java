package com.carddemo.controller;

import com.carddemo.dto.ErrorResponse;
import com.carddemo.dto.MenuOptionDto;
import com.carddemo.dto.MenuSelectionDto;
import com.carddemo.model.MenuOption;
import com.carddemo.security.JwtTokenProvider;
import com.carddemo.service.MenuService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.stream.Collectors;

/**
 * REST Controller for menu navigation operations.
 * Provides endpoints for retrieving menu options and validating menu selections.
 */
@RestController
@RequestMapping("/api/menu")
public class MenuController {

    private static final Logger logger = LoggerFactory.getLogger(MenuController.class);

    private final MenuService menuService;
    private final JwtTokenProvider jwtTokenProvider;

    public MenuController(MenuService menuService, JwtTokenProvider jwtTokenProvider) {
        this.menuService = menuService;
        this.jwtTokenProvider = jwtTokenProvider;
    }

    /**
     * Get menu options for the authenticated user.
     * Returns different menu options based on user type (Admin vs Regular User).
     * 
     * @param authHeader Authorization header containing JWT token
     * @return List of menu options available to the user
     */
    @GetMapping("/options")
    public ResponseEntity<?> getMenuOptions(@RequestHeader("Authorization") String authHeader) {
        try {
            logger.debug("Received request for menu options");
            
            // Extract token and get user type
            String token = authHeader.substring(7); // Remove "Bearer " prefix
            String userType = jwtTokenProvider.getUserTypeFromToken(token);
            
            logger.debug("User type from token: {}", userType);
            
            // Get menu options for user type
            List<MenuOption> menuOptions = menuService.getMenuOptions(userType);
            
            // Convert to DTOs
            List<MenuOptionDto> menuOptionDtos = menuOptions.stream()
                .map(option -> new MenuOptionDto(
                    option.getOptionNumber(),
                    option.getOptionName(),
                    option.getProgramName(),
                    option.getUserType()
                ))
                .collect(Collectors.toList());
            
            logger.info("Returning {} menu options for user type {}", menuOptionDtos.size(), userType);
            
            return ResponseEntity.ok(menuOptionDtos);
            
        } catch (Exception e) {
            logger.error("Error retrieving menu options", e);
            ErrorResponse errorResponse = new ErrorResponse(
                HttpStatus.INTERNAL_SERVER_ERROR.value(),
                "Internal Server Error",
                "Error retrieving menu options: " + e.getMessage(),
                "/api/menu/options"
            );
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(errorResponse);
        }
    }

    /**
     * Validate and process a menu selection.
     * Validates that the option is valid and the user has permission to access it.
     * 
     * @param selectionDto The menu selection containing option number
     * @param authHeader Authorization header containing JWT token
     * @return Menu selection response with program name and route
     */
    @PostMapping("/select")
    public ResponseEntity<?> selectOption(
            @RequestBody MenuSelectionDto selectionDto,
            @RequestHeader("Authorization") String authHeader) {
        try {
            logger.debug("Received menu selection request for option: {}", selectionDto.getOptionNumber());
            
            // Extract token and get user type
            String token = authHeader.substring(7); // Remove "Bearer " prefix
            String userType = jwtTokenProvider.getUserTypeFromToken(token);
            
            logger.debug("User type from token: {}", userType);
            
            // Validate and get program name
            String programName = menuService.validateAndRoute(selectionDto.getOptionNumber(), userType);
            
            // Map program name to route
            String route = mapProgramToRoute(programName);
            
            MenuSelectionDto response = new MenuSelectionDto(
                selectionDto.getOptionNumber(),
                programName,
                route
            );
            
            logger.info("Menu option {} validated successfully, routing to: {}", 
                       selectionDto.getOptionNumber(), route);
            
            return ResponseEntity.ok(response);
            
        } catch (IllegalArgumentException e) {
            logger.warn("Invalid menu selection: {}", e.getMessage());
            ErrorResponse errorResponse = new ErrorResponse(
                HttpStatus.BAD_REQUEST.value(),
                "Bad Request",
                e.getMessage(),
                "/api/menu/select"
            );
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(errorResponse);
            
        } catch (Exception e) {
            logger.error("Error processing menu selection", e);
            ErrorResponse errorResponse = new ErrorResponse(
                HttpStatus.INTERNAL_SERVER_ERROR.value(),
                "Internal Server Error",
                "Error processing menu selection: " + e.getMessage(),
                "/api/menu/select"
            );
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(errorResponse);
        }
    }

    /**
     * Map COBOL program names to frontend routes.
     * 
     * @param programName The COBOL program name
     * @return The corresponding frontend route
     */
    private String mapProgramToRoute(String programName) {
        switch (programName) {
            case "COACTVWC":
                return "/accounts/view";
            case "COACTUPC":
                return "/accounts/view";  // Route to account view first, then user can update
            case "COCRDLIC":
                return "/cards/list";
            case "COCRDSLC":
                return "/cards/list";     // Route to card list first, then user can view details
            case "COCRDUPC":
                return "/cards/list";     // Route to card list first, then user can update
            case "COTRN00C":
                return "/transactions/list";
            case "COTRN01C":
                return "/transactions/list";  // Map to transaction list since view needs ID
            case "COTRN02C":
                return "/transactions/list";  // Map to transaction list, no add page exists
            case "CORPT00C":
                return "/reports";
            case "COBIL00C":
                return "/bills/payment";
            case "COPAUS0C":
                return "/authorizations";  // Route to authorizations page
            case "COUSR00C":
                return "/users";
            case "COUSR01C":
                return "/users";  // Map to user management page
            case "COUSR02C":
                return "/users";  // Map to user management page
            case "COUSR03C":
                return "/users";  // Map to user management page
            case "COTRTLIC":
                return "/reports";   // Transaction Type List/Update - map to reports
            case "COTRTUPC":
                return "/reports";   // Transaction Type Maintenance - map to reports
            default:
                logger.warn("Unknown program name: {}", programName);
                return "/menu";
        }
    }
}
