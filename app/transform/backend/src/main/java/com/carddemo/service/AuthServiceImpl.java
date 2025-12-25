package com.carddemo.service;

import com.carddemo.dto.LoginResponseDto;
import com.carddemo.dto.SessionDto;
import com.carddemo.entity.User;
import com.carddemo.exception.AuthenticationException;
import com.carddemo.exception.ValidationException;
import com.carddemo.repository.UserRepository;
import com.carddemo.security.JwtTokenProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import java.util.Optional;

/**
 * Authentication Service Implementation
 * Handles user authentication, session management, and user ID normalization
 */
@Service
@Transactional
public class AuthServiceImpl implements AuthService {

    private static final Logger logger = LoggerFactory.getLogger(AuthServiceImpl.class);

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private PasswordEncoder passwordEncoder;

    @Autowired
    private JwtTokenProvider jwtTokenProvider;

    @Override
    public LoginResponseDto authenticate(String userId, String password) {
        logger.info("Authentication attempt for user: {}", userId);

        // Validate inputs
        validateAuthenticationInputs(userId, password);

        // Normalize user ID to uppercase
        String normalizedUserId = normalizeUserId(userId);

        // Look up user
        Optional<User> userOptional = userRepository.findById(normalizedUserId);
        if (userOptional.isEmpty()) {
            logger.warn("Authentication failed: User not found - {}", normalizedUserId);
            throw new AuthenticationException("User not found. Try again ...");
        }

        User user = userOptional.get();

        // Verify password
        if (!passwordEncoder.matches(password, user.getPassword())) {
            logger.warn("Authentication failed: Wrong password for user - {}", normalizedUserId);
            throw new AuthenticationException("Wrong Password. Try again ...");
        }

        // Generate JWT token
        String token = jwtTokenProvider.generateToken(
                user.getUserId(),
                user.getUserType(),
                user.getFirstName(),
                user.getLastName()
        );

        logger.info("Authentication successful for user: {} (type: {})", 
                    normalizedUserId, user.getUserType());

        // Build response
        LoginResponseDto response = new LoginResponseDto();
        response.setToken(token);
        response.setUserId(user.getUserId());
        response.setUserType(user.getUserType());
        response.setFirstName(user.getFirstName());
        response.setLastName(user.getLastName());

        return response;
    }

    @Override
    public void logout(String token) {
        // Extract user ID from token for logging
        if (StringUtils.hasText(token) && jwtTokenProvider.validateToken(token)) {
            String userId = jwtTokenProvider.getUserIdFromToken(token);
            logger.info("User logged out: {}", userId);
        }
        
        // Note: With stateless JWT, we don't need to invalidate tokens server-side
        // Token will expire naturally based on expiration time
        // For enhanced security, could implement token blacklist if needed
    }

    @Override
    public SessionDto validateSession(String token) {
        if (!StringUtils.hasText(token)) {
            throw new AuthenticationException("Session token is required");
        }

        if (!jwtTokenProvider.validateToken(token)) {
            throw new AuthenticationException("Invalid or expired session token");
        }

        String userId = jwtTokenProvider.getUserIdFromToken(token);
        String userType = jwtTokenProvider.getUserTypeFromToken(token);

        SessionDto session = new SessionDto();
        session.setUserId(userId);
        session.setUserType(userType);
        session.setValid(true);

        logger.debug("Session validated for user: {}", userId);

        return session;
    }

    @Override
    public String normalizeUserId(String userId) {
        if (userId == null) {
            return null;
        }
        return userId.toUpperCase().trim();
    }

    /**
     * Validate authentication inputs
     *
     * @param userId User ID
     * @param password Password
     * @throws ValidationException if inputs are invalid
     */
    private void validateAuthenticationInputs(String userId, String password) {
        if (!StringUtils.hasText(userId) || userId.trim().isEmpty()) {
            throw new ValidationException("Please enter User ID ...");
        }

        if (!StringUtils.hasText(password) || password.trim().isEmpty()) {
            throw new ValidationException("Please enter Password ...");
        }
    }
}
