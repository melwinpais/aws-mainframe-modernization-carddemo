package com.carddemo.service;

import com.carddemo.dto.LoginResponseDto;
import com.carddemo.entity.User;
import com.carddemo.exception.AuthenticationException;
import com.carddemo.exception.ValidationException;
import com.carddemo.repository.UserRepository;
import com.carddemo.security.JwtTokenProvider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

/**
 * Unit tests for AuthService
 * Tests specific examples and edge cases for authentication
 * Validates Requirements 1.3, 1.4, 1.5, 1.6
 */
@ExtendWith(MockitoExtension.class)
class AuthServiceTest {

    @Mock
    private UserRepository userRepository;

    @Mock
    private PasswordEncoder passwordEncoder;

    @Mock
    private JwtTokenProvider jwtTokenProvider;

    @InjectMocks
    private AuthServiceImpl authService;

    private User testUser;
    private static final String TEST_USER_ID = "TESTUSER";
    private static final String TEST_PASSWORD = "password123";
    private static final String TEST_HASHED_PASSWORD = "$2a$10$hashedpassword";
    private static final String TEST_TOKEN = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.test.token";

    @BeforeEach
    void setUp() {
        testUser = new User();
        testUser.setUserId(TEST_USER_ID);
        testUser.setFirstName("Test");
        testUser.setLastName("User");
        testUser.setPassword(TEST_HASHED_PASSWORD);
        testUser.setUserType("U");
    }

    /**
     * Test successful authentication with valid credentials
     * Requirements: 1.3
     */
    @Test
    void testSuccessfulAuthenticationWithValidCredentials() {
        // Arrange
        when(userRepository.findById(TEST_USER_ID)).thenReturn(Optional.of(testUser));
        when(passwordEncoder.matches(TEST_PASSWORD, TEST_HASHED_PASSWORD)).thenReturn(true);
        when(jwtTokenProvider.generateToken(
                TEST_USER_ID,
                "U",
                "Test",
                "User"
        )).thenReturn(TEST_TOKEN);

        // Act
        LoginResponseDto response = authService.authenticate(TEST_USER_ID, TEST_PASSWORD);

        // Assert
        assertThat(response).isNotNull();
        assertThat(response.getToken()).isEqualTo(TEST_TOKEN);
        assertThat(response.getUserId()).isEqualTo(TEST_USER_ID);
        assertThat(response.getUserType()).isEqualTo("U");
        assertThat(response.getFirstName()).isEqualTo("Test");
        assertThat(response.getLastName()).isEqualTo("User");

        // Verify interactions
        verify(userRepository).findById(TEST_USER_ID);
        verify(passwordEncoder).matches(TEST_PASSWORD, TEST_HASHED_PASSWORD);
        verify(jwtTokenProvider).generateToken(TEST_USER_ID, "U", "Test", "User");
    }

    /**
     * Test authentication failure with invalid user ID
     * Requirements: 1.3
     */
    @Test
    void testAuthenticationFailureWithInvalidUserId() {
        // Arrange
        String invalidUserId = "INVALIDUSER";
        when(userRepository.findById(invalidUserId)).thenReturn(Optional.empty());

        // Act & Assert
        assertThatThrownBy(() -> authService.authenticate(invalidUserId, TEST_PASSWORD))
                .isInstanceOf(AuthenticationException.class)
                .hasMessageContaining("User not found. Try again ...");

        // Verify interactions
        verify(userRepository).findById(invalidUserId);
        verify(passwordEncoder, never()).matches(anyString(), anyString());
        verify(jwtTokenProvider, never()).generateToken(anyString(), anyString(), anyString(), anyString());
    }

    /**
     * Test authentication failure with wrong password
     * Requirements: 1.4
     */
    @Test
    void testAuthenticationFailureWithWrongPassword() {
        // Arrange
        String wrongPassword = "wrongpassword";
        when(userRepository.findById(TEST_USER_ID)).thenReturn(Optional.of(testUser));
        when(passwordEncoder.matches(wrongPassword, TEST_HASHED_PASSWORD)).thenReturn(false);

        // Act & Assert
        assertThatThrownBy(() -> authService.authenticate(TEST_USER_ID, wrongPassword))
                .isInstanceOf(AuthenticationException.class)
                .hasMessageContaining("Wrong Password. Try again ...");

        // Verify interactions
        verify(userRepository).findById(TEST_USER_ID);
        verify(passwordEncoder).matches(wrongPassword, TEST_HASHED_PASSWORD);
        verify(jwtTokenProvider, never()).generateToken(anyString(), anyString(), anyString(), anyString());
    }

    /**
     * Test empty user ID validation
     * Requirements: 1.5
     */
    @Test
    void testEmptyUserIdValidation() {
        // Test with empty string
        assertThatThrownBy(() -> authService.authenticate("", TEST_PASSWORD))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Please enter User ID ...");

        // Test with whitespace only
        assertThatThrownBy(() -> authService.authenticate("   ", TEST_PASSWORD))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Please enter User ID ...");

        // Test with null
        assertThatThrownBy(() -> authService.authenticate(null, TEST_PASSWORD))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Please enter User ID ...");

        // Verify no repository interactions occurred
        verify(userRepository, never()).findById(anyString());
        verify(passwordEncoder, never()).matches(anyString(), anyString());
        verify(jwtTokenProvider, never()).generateToken(anyString(), anyString(), anyString(), anyString());
    }

    /**
     * Test empty password validation
     * Requirements: 1.6
     */
    @Test
    void testEmptyPasswordValidation() {
        // Test with empty string
        assertThatThrownBy(() -> authService.authenticate(TEST_USER_ID, ""))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Please enter Password ...");

        // Test with whitespace only
        assertThatThrownBy(() -> authService.authenticate(TEST_USER_ID, "   "))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Please enter Password ...");

        // Test with null
        assertThatThrownBy(() -> authService.authenticate(TEST_USER_ID, null))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Please enter Password ...");

        // Verify no repository interactions occurred
        verify(userRepository, never()).findById(anyString());
        verify(passwordEncoder, never()).matches(anyString(), anyString());
        verify(jwtTokenProvider, never()).generateToken(anyString(), anyString(), anyString(), anyString());
    }

    /**
     * Test user ID normalization (uppercase conversion)
     * This verifies that lowercase user IDs are converted to uppercase
     */
    @Test
    void testUserIdNormalization() {
        // Arrange
        String lowercaseUserId = "testuser";
        when(userRepository.findById(TEST_USER_ID)).thenReturn(Optional.of(testUser));
        when(passwordEncoder.matches(TEST_PASSWORD, TEST_HASHED_PASSWORD)).thenReturn(true);
        when(jwtTokenProvider.generateToken(
                TEST_USER_ID,
                "U",
                "Test",
                "User"
        )).thenReturn(TEST_TOKEN);

        // Act
        LoginResponseDto response = authService.authenticate(lowercaseUserId, TEST_PASSWORD);

        // Assert
        assertThat(response).isNotNull();
        assertThat(response.getUserId()).isEqualTo(TEST_USER_ID);

        // Verify that the repository was called with the normalized (uppercase) user ID
        verify(userRepository).findById(TEST_USER_ID);
    }

    /**
     * Test authentication with admin user type
     * Verifies that admin users are correctly identified
     */
    @Test
    void testAuthenticationWithAdminUser() {
        // Arrange
        User adminUser = new User();
        adminUser.setUserId("ADMIN");
        adminUser.setFirstName("Admin");
        adminUser.setLastName("User");
        adminUser.setPassword(TEST_HASHED_PASSWORD);
        adminUser.setUserType("A");

        when(userRepository.findById("ADMIN")).thenReturn(Optional.of(adminUser));
        when(passwordEncoder.matches(TEST_PASSWORD, TEST_HASHED_PASSWORD)).thenReturn(true);
        when(jwtTokenProvider.generateToken(
                "ADMIN",
                "A",
                "Admin",
                "User"
        )).thenReturn(TEST_TOKEN);

        // Act
        LoginResponseDto response = authService.authenticate("ADMIN", TEST_PASSWORD);

        // Assert
        assertThat(response).isNotNull();
        assertThat(response.getUserType()).isEqualTo("A");
        assertThat(response.getUserId()).isEqualTo("ADMIN");
    }
}
