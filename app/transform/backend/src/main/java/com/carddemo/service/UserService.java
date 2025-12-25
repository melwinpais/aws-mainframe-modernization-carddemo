package com.carddemo.service;

import com.carddemo.dto.UserCreateDto;
import com.carddemo.dto.UserDto;
import com.carddemo.dto.UserUpdateDto;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

/**
 * Service interface for User management operations
 * Handles user CRUD operations with admin authorization
 */
public interface UserService {

    /**
     * List all users with pagination
     * @param userType optional filter by user type ('A' or 'U')
     * @param pageable pagination parameters
     * @return Page of UserDto objects
     */
    Page<UserDto> listUsers(String userType, Pageable pageable);

    /**
     * Get a specific user by user ID
     * @param userId the user ID to retrieve
     * @return UserDto containing user information
     * @throws com.carddemo.exception.ResourceNotFoundException if user not found
     */
    UserDto getUser(String userId);

    /**
     * Create a new user with password hashing
     * @param userCreateDto the user data to create
     * @return UserDto containing the created user information
     * @throws com.carddemo.exception.ValidationException if user ID already exists
     */
    UserDto createUser(UserCreateDto userCreateDto);

    /**
     * Update an existing user
     * @param userId the user ID to update
     * @param userUpdateDto the updated user data
     * @return UserDto containing the updated user information
     * @throws com.carddemo.exception.ResourceNotFoundException if user not found
     */
    UserDto updateUser(String userId, UserUpdateDto userUpdateDto);

    /**
     * Deactivate a user (soft delete by marking as inactive)
     * @param userId the user ID to deactivate
     * @throws com.carddemo.exception.ResourceNotFoundException if user not found
     */
    void deactivateUser(String userId);
}
