package com.carddemo.controller;

import com.carddemo.dto.UserCreateDto;
import com.carddemo.dto.UserDto;
import com.carddemo.dto.UserUpdateDto;
import com.carddemo.service.UserService;
import jakarta.validation.Valid;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.Map;

/**
 * REST Controller for User Management (Admin Only)
 * Provides endpoints for CRUD operations on users
 * All endpoints require ADMIN role
 */
@RestController
@RequestMapping("/api/users")
@PreAuthorize("hasRole('ADMIN')")
public class UserController {

    private static final Logger logger = LoggerFactory.getLogger(UserController.class);

    private final UserService userService;

    public UserController(UserService userService) {
        this.userService = userService;
    }

    /**
     * List all users with optional filtering and pagination
     * GET /api/users?userType=A&page=0&size=20
     */
    @GetMapping
    public ResponseEntity<Page<UserDto>> listUsers(
            @RequestParam(required = false) String userType,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {
        
        logger.info("GET /api/users - userType: {}, page: {}, size: {}", userType, page, size);

        Pageable pageable = PageRequest.of(page, size);
        Page<UserDto> users = userService.listUsers(userType, pageable);

        logger.info("Returning {} users", users.getTotalElements());
        return ResponseEntity.ok(users);
    }

    /**
     * Get a specific user by user ID
     * GET /api/users/{userId}
     */
    @GetMapping("/{userId}")
    public ResponseEntity<UserDto> getUser(@PathVariable String userId) {
        logger.info("GET /api/users/{}", userId);

        UserDto user = userService.getUser(userId);

        logger.info("Returning user: {}", userId);
        return ResponseEntity.ok(user);
    }

    /**
     * Create a new user
     * POST /api/users
     */
    @PostMapping
    public ResponseEntity<UserDto> createUser(@Valid @RequestBody UserCreateDto userCreateDto) {
        logger.info("POST /api/users - Creating user: {}", userCreateDto.getUserId());

        UserDto createdUser = userService.createUser(userCreateDto);

        logger.info("User created successfully: {}", createdUser.getUserId());
        return ResponseEntity.status(HttpStatus.CREATED).body(createdUser);
    }

    /**
     * Update an existing user
     * PUT /api/users/{userId}
     */
    @PutMapping("/{userId}")
    public ResponseEntity<UserDto> updateUser(
            @PathVariable String userId,
            @Valid @RequestBody UserUpdateDto userUpdateDto) {
        
        logger.info("PUT /api/users/{} - Updating user", userId);

        UserDto updatedUser = userService.updateUser(userId, userUpdateDto);

        logger.info("User updated successfully: {}", userId);
        return ResponseEntity.ok(updatedUser);
    }

    /**
     * Deactivate a user
     * DELETE /api/users/{userId}
     */
    @DeleteMapping("/{userId}")
    public ResponseEntity<Map<String, String>> deactivateUser(@PathVariable String userId) {
        logger.info("DELETE /api/users/{} - Deactivating user", userId);

        userService.deactivateUser(userId);

        Map<String, String> response = new HashMap<>();
        response.put("message", "User deactivated successfully: " + userId);

        logger.info("User deactivated successfully: {}", userId);
        return ResponseEntity.ok(response);
    }
}
