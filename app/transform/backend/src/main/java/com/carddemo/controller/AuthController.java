package com.carddemo.controller;

import com.carddemo.dto.LoginRequestDto;
import com.carddemo.dto.LoginResponseDto;
import com.carddemo.dto.MessageDto;
import com.carddemo.dto.SessionDto;
import com.carddemo.service.AuthService;
import jakarta.validation.Valid;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * Authentication Controller
 * Handles user authentication, logout, and session validation
 */
@RestController
@RequestMapping("/api/auth")
public class AuthController {

    private static final Logger logger = LoggerFactory.getLogger(AuthController.class);

    @Autowired
    private AuthService authService;

    /**
     * Login endpoint
     * POST /api/auth/login
     *
     * @param loginRequest Login credentials
     * @return LoginResponseDto with token and user information
     */
    @PostMapping("/login")
    public ResponseEntity<LoginResponseDto> login(@Valid @RequestBody LoginRequestDto loginRequest) {
        logger.info("Login request received for user: {}", loginRequest.getUserId());

        LoginResponseDto response = authService.authenticate(
                loginRequest.getUserId(),
                loginRequest.getPassword()
        );

        logger.info("Login successful for user: {}", response.getUserId());
        return ResponseEntity.ok(response);
    }

    /**
     * Logout endpoint
     * POST /api/auth/logout
     *
     * @param authorizationHeader Authorization header containing JWT token
     * @return MessageDto with logout confirmation
     */
    @PostMapping("/logout")
    public ResponseEntity<MessageDto> logout(
            @RequestHeader(value = "Authorization", required = false) String authorizationHeader) {
        
        String token = extractTokenFromHeader(authorizationHeader);
        
        if (token != null) {
            authService.logout(token);
            logger.info("Logout successful");
        }

        MessageDto response = new MessageDto("Logout successful");
        return ResponseEntity.ok(response);
    }

    /**
     * Validate session endpoint
     * GET /api/auth/validate
     *
     * @param authorizationHeader Authorization header containing JWT token
     * @return SessionDto with session validation information
     */
    @GetMapping("/validate")
    public ResponseEntity<SessionDto> validateSession(
            @RequestHeader(value = "Authorization", required = false) String authorizationHeader) {
        
        logger.debug("Session validation request received");

        String token = extractTokenFromHeader(authorizationHeader);
        
        if (token == null) {
            SessionDto invalidSession = new SessionDto(false, null, null);
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(invalidSession);
        }

        SessionDto session = authService.validateSession(token);
        
        logger.debug("Session validation successful for user: {}", session.getUserId());
        return ResponseEntity.ok(session);
    }

    /**
     * Extract JWT token from Authorization header
     *
     * @param authorizationHeader Authorization header value
     * @return JWT token or null if not found
     */
    private String extractTokenFromHeader(String authorizationHeader) {
        if (authorizationHeader != null && authorizationHeader.startsWith("Bearer ")) {
            return authorizationHeader.substring(7);
        }
        return null;
    }
}
