package com.carddemo.service;

import com.carddemo.dto.LoginResponseDto;
import com.carddemo.dto.SessionDto;

/**
 * Authentication Service Interface
 * Handles user authentication, session management, and user ID normalization
 */
public interface AuthService {

    /**
     * Authenticate user with credentials
     *
     * @param userId User ID
     * @param password Password
     * @return LoginResponseDto containing token and user information
     * @throws com.carddemo.exception.AuthenticationException if authentication fails
     * @throws com.carddemo.exception.ValidationException if inputs are invalid
     */
    LoginResponseDto authenticate(String userId, String password);

    /**
     * Logout user and invalidate session
     *
     * @param token JWT token
     */
    void logout(String token);

    /**
     * Validate session token
     *
     * @param token JWT token
     * @return SessionDto containing session information
     * @throws com.carddemo.exception.AuthenticationException if token is invalid
     */
    SessionDto validateSession(String token);

    /**
     * Normalize user ID to uppercase
     *
     * @param userId User ID
     * @return Normalized user ID in uppercase
     */
    String normalizeUserId(String userId);
}
