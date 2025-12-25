package com.carddemo.service;

import com.carddemo.dto.LoginResponseDto;
import com.carddemo.entity.User;
import com.carddemo.exception.AuthenticationException;
import com.carddemo.exception.ValidationException;
import com.carddemo.repository.UserRepository;
import com.carddemo.security.JwtTokenProvider;
import net.jqwik.api.*;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

/**
 * Property-based tests for AuthService
 * Tests universal properties across all inputs
 * Validates Requirements 1.1, 1.2, 1.3, 1.4, 1.7, 1.8
 */
class AuthServicePropertyTest {

    private UserRepository userRepository;
    private PasswordEncoder passwordEncoder;
    private JwtTokenProvider jwtTokenProvider;
    private AuthServiceImpl authService;

    /**
     * Property 1: Credential Validation
     * For any user ID and password combination, when submitted for authentication,
     * the system should validate against the database and return either a valid session
     * (if credentials match) or an appropriate error message (if credentials don't match or are invalid)
     * Validates: Requirements 1.1, 1.2, 1.3, 1.4
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 1: Credential Validation")
    void testCredentialValidation(
            @ForAll("userIds") String userId,
            @ForAll("passwords") String password) {
        
        // Setup mocks for each test iteration
        setupMocks();
        
        // Test empty/whitespace user ID
        if (userId == null || userId.trim().isEmpty()) {
            assertThatThrownBy(() -> authService.authenticate(userId, password))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Please enter User ID");
            return;
        }
        
        // Test empty/whitespace password
        if (password == null || password.trim().isEmpty()) {
            assertThatThrownBy(() -> authService.authenticate(userId, password))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Please enter Password");
            return;
        }
        
        // Normalize user ID for lookup
        String normalizedUserId = userId.toUpperCase().trim();
        
        // Create a test user with known password
        String knownPassword = "testpass";
        String hashedPassword = passwordEncoder.encode(knownPassword);
        User testUser = new User(normalizedUserId, "Test", "User", hashedPassword, "U");
        
        // Mock user lookup
        when(userRepository.findById(normalizedUserId)).thenReturn(Optional.of(testUser));
        
        // Mock JWT token generation
        String mockToken = "mock.jwt.token";
        when(jwtTokenProvider.generateToken(anyString(), anyString(), anyString(), anyString()))
            .thenReturn(mockToken);
        
        // Test with correct password
        if (password.equals(knownPassword)) {
            LoginResponseDto response = authService.authenticate(userId, password);
            
            // Verify successful authentication
            assertThat(response).isNotNull();
            assertThat(response.getToken()).isEqualTo(mockToken);
            assertThat(response.getUserId()).isEqualTo(normalizedUserId);
            assertThat(response.getUserType()).isEqualTo("U");
            assertThat(response.getFirstName()).isEqualTo("Test");
            assertThat(response.getLastName()).isEqualTo("User");
        } else {
            // Test with incorrect password
            assertThatThrownBy(() -> authService.authenticate(userId, password))
                .isInstanceOf(AuthenticationException.class)
                .hasMessageContaining("Wrong Password");
        }
        
        // Test with non-existent user
        String nonExistentUserId = "NOUSER";
        when(userRepository.findById(nonExistentUserId)).thenReturn(Optional.empty());
        
        assertThatThrownBy(() -> authService.authenticate(nonExistentUserId, password))
            .isInstanceOf(AuthenticationException.class)
            .hasMessageContaining("User not found");
    }

    /**
     * Property 2: User Type Routing
     * For any authenticated user, the system should route admin users (type 'A')
     * to the admin menu and regular users (type 'U') to the main menu
     * Validates: Requirements 1.7, 1.8
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Property 2: User Type Routing")
    void testUserTypeRouting(
            @ForAll("validUserIds") String userId,
            @ForAll("validPasswords") String password,
            @ForAll("userTypes") String userType) {
        
        // Setup mocks for each test iteration
        setupMocks();
        
        // Skip if inputs are invalid (empty/whitespace)
        if (userId == null || userId.trim().isEmpty() || 
            password == null || password.trim().isEmpty()) {
            return;
        }
        
        // Normalize user ID
        String normalizedUserId = userId.toUpperCase().trim();
        
        // Create test user with specified type
        String hashedPassword = passwordEncoder.encode(password);
        User testUser = new User(normalizedUserId, "Test", "User", hashedPassword, userType);
        
        // Mock user lookup
        when(userRepository.findById(normalizedUserId)).thenReturn(Optional.of(testUser));
        
        // Mock JWT token generation
        String mockToken = "mock.jwt.token";
        when(jwtTokenProvider.generateToken(anyString(), anyString(), anyString(), anyString()))
            .thenReturn(mockToken);
        
        // Authenticate
        LoginResponseDto response = authService.authenticate(userId, password);
        
        // Verify user type is correctly returned
        assertThat(response).isNotNull();
        assertThat(response.getUserType()).isEqualTo(userType);
        
        // Verify routing based on user type
        if (userType.equals("A")) {
            // Admin users should be routed to admin menu (COADM01C equivalent)
            assertThat(response.getUserType()).isEqualTo("A");
        } else if (userType.equals("U")) {
            // Regular users should be routed to main menu (COMEN01C equivalent)
            assertThat(response.getUserType()).isEqualTo("U");
        }
        
        // The actual routing logic is handled by the frontend/menu service
        // This property verifies that the authentication response correctly
        // identifies the user type for routing decisions
    }

    // Helper method to setup mocks for each test iteration
    private void setupMocks() {
        userRepository = mock(UserRepository.class);
        passwordEncoder = new BCryptPasswordEncoder();
        jwtTokenProvider = mock(JwtTokenProvider.class);
        
        authService = new AuthServiceImpl();
        // Use reflection to inject mocks (in real tests, use @InjectMocks or constructor injection)
        try {
            java.lang.reflect.Field userRepoField = AuthServiceImpl.class.getDeclaredField("userRepository");
            userRepoField.setAccessible(true);
            userRepoField.set(authService, userRepository);
            
            java.lang.reflect.Field passwordEncoderField = AuthServiceImpl.class.getDeclaredField("passwordEncoder");
            passwordEncoderField.setAccessible(true);
            passwordEncoderField.set(authService, passwordEncoder);
            
            java.lang.reflect.Field jwtProviderField = AuthServiceImpl.class.getDeclaredField("jwtTokenProvider");
            jwtProviderField.setAccessible(true);
            jwtProviderField.set(authService, jwtTokenProvider);
        } catch (Exception e) {
            throw new RuntimeException("Failed to inject mocks", e);
        }
    }

    // Providers for generating test data

    @Provide
    Arbitrary<String> userIds() {
        return Arbitraries.oneOf(
            // Valid user IDs
            Arbitraries.strings().alpha().ofMinLength(1).ofMaxLength(8),
            Arbitraries.strings().alpha().ofMinLength(1).ofMaxLength(8)
                       .map(String::toLowerCase),  // Lowercase to test normalization
            Arbitraries.strings().alpha().ofMinLength(1).ofMaxLength(8)
                       .map(s -> s.substring(0, Math.min(s.length(), 4)).toUpperCase() + 
                                s.substring(Math.min(s.length(), 4)).toLowerCase()), // Mixed case
            // Invalid user IDs
            Arbitraries.just(""),           // Empty
            Arbitraries.just("   "),        // Whitespace only
            Arbitraries.just(null)          // Null
        );
    }

    @Provide
    Arbitrary<String> validUserIds() {
        return Arbitraries.strings().alpha().ofMinLength(1).ofMaxLength(8);
    }

    @Provide
    Arbitrary<String> passwords() {
        return Arbitraries.oneOf(
            // Valid passwords
            Arbitraries.strings().ofMinLength(1).ofMaxLength(20),
            Arbitraries.just("testpass"),   // Known password for testing
            Arbitraries.just("password123"),
            Arbitraries.just("admin"),
            // Invalid passwords
            Arbitraries.just(""),           // Empty
            Arbitraries.just("   "),        // Whitespace only
            Arbitraries.just(null)          // Null
        );
    }

    @Provide
    Arbitrary<String> validPasswords() {
        return Arbitraries.strings().ofMinLength(1).ofMaxLength(20);
    }

    @Provide
    Arbitrary<String> userTypes() {
        return Arbitraries.of("A", "U");  // Admin or User
    }
}
