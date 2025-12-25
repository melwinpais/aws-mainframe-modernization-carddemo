package com.carddemo.integration;

import com.carddemo.CardDemoApplication;
import com.carddemo.dto.LoginRequestDto;
import com.carddemo.dto.LoginResponseDto;
import com.carddemo.dto.MenuOptionDto;
import com.carddemo.entity.User;
import com.carddemo.repository.UserRepository;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.transaction.annotation.Transactional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

/**
 * Integration tests for authentication flow
 * Tests the complete flow: login → menu navigation → logout
 * Tests session validation across requests
 * Validates Requirements: 1.1, 1.2, 2.1, 13.3
 */
@SpringBootTest(
    classes = CardDemoApplication.class,
    webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT
)
@AutoConfigureMockMvc
@ActiveProfiles("test")
@Transactional
class AuthenticationFlowIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private PasswordEncoder passwordEncoder;

    private static final String REGULAR_USER_ID = "TESTUSER";
    private static final String ADMIN_USER_ID = "TESTADMIN";
    private static final String PASSWORD = "password123";

    @BeforeEach
    void setUp() {
        // Clean up any existing test users
        userRepository.deleteAll();

        // Create a regular user
        User regularUser = new User();
        regularUser.setUserId(REGULAR_USER_ID);
        regularUser.setFirstName("Test");
        regularUser.setLastName("User");
        regularUser.setPassword(passwordEncoder.encode(PASSWORD));
        regularUser.setUserType("U");
        userRepository.save(regularUser);

        // Create an admin user
        User adminUser = new User();
        adminUser.setUserId(ADMIN_USER_ID);
        adminUser.setFirstName("Test");
        adminUser.setLastName("Admin");
        adminUser.setPassword(passwordEncoder.encode(PASSWORD));
        adminUser.setUserType("A");
        userRepository.save(adminUser);
    }

    /**
     * Test complete authentication flow for regular user:
     * 1. Login with valid credentials
     * 2. Receive JWT token
     * 3. Access menu options with token
     * 4. Logout
     * Validates Requirements: 1.1, 1.2, 2.1, 13.3
     */
    @Test
    void testCompleteAuthenticationFlowForRegularUser() throws Exception {
        // Step 1: Login with valid credentials
        LoginRequestDto loginRequest = new LoginRequestDto();
        loginRequest.setUserId(REGULAR_USER_ID);
        loginRequest.setPassword(PASSWORD);

        MvcResult loginResult = mockMvc.perform(post("/api/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(loginRequest)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.token").exists())
                .andExpect(jsonPath("$.userId").value(REGULAR_USER_ID))
                .andExpect(jsonPath("$.userType").value("U"))
                .andExpect(jsonPath("$.firstName").value("Test"))
                .andExpect(jsonPath("$.lastName").value("User"))
                .andReturn();

        // Extract token from response
        String responseBody = loginResult.getResponse().getContentAsString();
        LoginResponseDto loginResponse = objectMapper.readValue(responseBody, LoginResponseDto.class);
        String token = loginResponse.getToken();

        assertThat(token).isNotNull();
        assertThat(token).isNotEmpty();

        // Step 2: Validate session with token
        mockMvc.perform(get("/api/auth/validate")
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.valid").value(true))
                .andExpect(jsonPath("$.userId").value(REGULAR_USER_ID))
                .andExpect(jsonPath("$.userType").value("U"));

        // Step 3: Access menu options with token (should get regular user menu)
        MvcResult menuResult = mockMvc.perform(get("/api/menu/options")
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").isArray())
                .andReturn();

        String menuResponseBody = menuResult.getResponse().getContentAsString();
        MenuOptionDto[] menuOptions = objectMapper.readValue(menuResponseBody, MenuOptionDto[].class);

        // Verify we got menu options and they don't include admin-only options
        assertThat(menuOptions).isNotEmpty();
        assertThat(menuOptions).noneMatch(option -> "A".equals(option.getUserType()));

        // Step 4: Logout
        mockMvc.perform(post("/api/auth/logout")
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message").exists());
    }

    /**
     * Test complete authentication flow for admin user:
     * 1. Login with valid credentials
     * 2. Receive JWT token
     * 3. Access menu options with token (should include admin options)
     * 4. Logout
     * Validates Requirements: 1.1, 1.2, 2.1, 13.3
     */
    @Test
    void testCompleteAuthenticationFlowForAdminUser() throws Exception {
        // Step 1: Login with valid credentials
        LoginRequestDto loginRequest = new LoginRequestDto();
        loginRequest.setUserId(ADMIN_USER_ID);
        loginRequest.setPassword(PASSWORD);

        MvcResult loginResult = mockMvc.perform(post("/api/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(loginRequest)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.token").exists())
                .andExpect(jsonPath("$.userId").value(ADMIN_USER_ID))
                .andExpect(jsonPath("$.userType").value("A"))
                .andExpect(jsonPath("$.firstName").value("Test"))
                .andExpect(jsonPath("$.lastName").value("Admin"))
                .andReturn();

        // Extract token from response
        String responseBody = loginResult.getResponse().getContentAsString();
        LoginResponseDto loginResponse = objectMapper.readValue(responseBody, LoginResponseDto.class);
        String token = loginResponse.getToken();

        assertThat(token).isNotNull();
        assertThat(token).isNotEmpty();

        // Step 2: Validate session with token
        mockMvc.perform(get("/api/auth/validate")
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.valid").value(true))
                .andExpect(jsonPath("$.userId").value(ADMIN_USER_ID))
                .andExpect(jsonPath("$.userType").value("A"));

        // Step 3: Access menu options with token (should get admin menu)
        MvcResult menuResult = mockMvc.perform(get("/api/menu/options")
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").isArray())
                .andReturn();

        String menuResponseBody = menuResult.getResponse().getContentAsString();
        MenuOptionDto[] menuOptions = objectMapper.readValue(menuResponseBody, MenuOptionDto[].class);

        // Verify we got menu options including admin options
        assertThat(menuOptions).isNotEmpty();
        assertThat(menuOptions).anyMatch(option -> "A".equals(option.getUserType()));

        // Step 4: Logout
        mockMvc.perform(post("/api/auth/logout")
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message").exists());
    }

    /**
     * Test session validation across multiple requests
     * Validates that the same token can be used for multiple API calls
     * Validates Requirement: 13.3
     */
    @Test
    void testSessionValidationAcrossMultipleRequests() throws Exception {
        // Step 1: Login
        LoginRequestDto loginRequest = new LoginRequestDto();
        loginRequest.setUserId(REGULAR_USER_ID);
        loginRequest.setPassword(PASSWORD);

        MvcResult loginResult = mockMvc.perform(post("/api/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(loginRequest)))
                .andExpect(status().isOk())
                .andReturn();

        String responseBody = loginResult.getResponse().getContentAsString();
        LoginResponseDto loginResponse = objectMapper.readValue(responseBody, LoginResponseDto.class);
        String token = loginResponse.getToken();

        // Step 2: Make multiple requests with the same token
        // Request 1: Validate session
        mockMvc.perform(get("/api/auth/validate")
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.valid").value(true));

        // Request 2: Get menu options
        mockMvc.perform(get("/api/menu/options")
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk());

        // Request 3: Validate session again
        mockMvc.perform(get("/api/auth/validate")
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.valid").value(true));

        // Request 4: Get menu options again
        mockMvc.perform(get("/api/menu/options")
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk());

        // All requests should succeed with the same token
    }

    /**
     * Test that requests without token are rejected
     * Validates Requirement: 13.3
     */
    @Test
    void testRequestsWithoutTokenAreRejected() throws Exception {
        // Attempt to access protected endpoint without token
        mockMvc.perform(get("/api/menu/options"))
                .andExpect(status().isUnauthorized());

        // Attempt to validate session without token
        mockMvc.perform(get("/api/auth/validate"))
                .andExpect(status().isUnauthorized());
    }

    /**
     * Test that requests with invalid token are rejected
     * Validates Requirement: 13.3
     */
    @Test
    void testRequestsWithInvalidTokenAreRejected() throws Exception {
        String invalidToken = "invalid.token.here";

        // Attempt to access protected endpoint with invalid token
        mockMvc.perform(get("/api/menu/options")
                        .header("Authorization", "Bearer " + invalidToken))
                .andExpect(status().isUnauthorized());

        // Attempt to validate session with invalid token
        mockMvc.perform(get("/api/auth/validate")
                        .header("Authorization", "Bearer " + invalidToken))
                .andExpect(status().isUnauthorized());
    }

    /**
     * Test login failure with invalid credentials
     * Validates Requirements: 1.1, 1.2
     */
    @Test
    void testLoginFailureWithInvalidCredentials() throws Exception {
        // Test with invalid user ID
        LoginRequestDto invalidUserRequest = new LoginRequestDto();
        invalidUserRequest.setUserId("INVALIDUSER");
        invalidUserRequest.setPassword(PASSWORD);

        mockMvc.perform(post("/api/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(invalidUserRequest)))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.message").value("User not found. Try again ..."));

        // Test with invalid password
        LoginRequestDto invalidPasswordRequest = new LoginRequestDto();
        invalidPasswordRequest.setUserId(REGULAR_USER_ID);
        invalidPasswordRequest.setPassword("wrongpassword");

        mockMvc.perform(post("/api/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(invalidPasswordRequest)))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.message").value("Wrong Password. Try again ..."));
    }

    /**
     * Test that user ID is normalized to uppercase during login
     * Validates Requirement: 1.1
     */
    @Test
    void testUserIdNormalizationDuringLogin() throws Exception {
        // Login with lowercase user ID
        LoginRequestDto loginRequest = new LoginRequestDto();
        loginRequest.setUserId("testuser"); // lowercase
        loginRequest.setPassword(PASSWORD);

        MvcResult loginResult = mockMvc.perform(post("/api/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(loginRequest)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.userId").value(REGULAR_USER_ID)) // Should be uppercase
                .andReturn();

        String responseBody = loginResult.getResponse().getContentAsString();
        LoginResponseDto loginResponse = objectMapper.readValue(responseBody, LoginResponseDto.class);

        // Verify the user ID in the response is uppercase
        assertThat(loginResponse.getUserId()).isEqualTo(REGULAR_USER_ID);
    }

    /**
     * Test complete flow with menu selection
     * Tests login → menu options → menu selection
     * Validates Requirements: 1.1, 1.2, 2.1
     */
    @Test
    void testLoginToMenuSelectionFlow() throws Exception {
        // Step 1: Login
        LoginRequestDto loginRequest = new LoginRequestDto();
        loginRequest.setUserId(REGULAR_USER_ID);
        loginRequest.setPassword(PASSWORD);

        MvcResult loginResult = mockMvc.perform(post("/api/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(loginRequest)))
                .andExpect(status().isOk())
                .andReturn();

        String responseBody = loginResult.getResponse().getContentAsString();
        LoginResponseDto loginResponse = objectMapper.readValue(responseBody, LoginResponseDto.class);
        String token = loginResponse.getToken();

        // Step 2: Get menu options
        MvcResult menuResult = mockMvc.perform(get("/api/menu/options")
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk())
                .andReturn();

        String menuResponseBody = menuResult.getResponse().getContentAsString();
        MenuOptionDto[] menuOptions = objectMapper.readValue(menuResponseBody, MenuOptionDto[].class);

        assertThat(menuOptions).isNotEmpty();

        // Step 3: Select a menu option (use the first available option)
        MenuOptionDto firstOption = menuOptions[0];
        String selectionRequest = String.format("{\"optionNumber\": %d}", firstOption.getOptionNumber());

        mockMvc.perform(post("/api/menu/select")
                        .header("Authorization", "Bearer " + token)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(selectionRequest))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.programName").exists())
                .andExpect(jsonPath("$.route").exists());
    }
}
